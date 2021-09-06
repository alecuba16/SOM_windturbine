create_turbines_groups<-function(wp_id=NULL,array_id_ot=NULL,distance_matrix=NULL,number_of_groups=c(3,4,5),fault=NULL,verbose=F,db_config=NULL){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  libraries<-c('dplyr')
  # Sources
  sources_common<-paste0("functions_common/formatter_get_tableinfo.R")
  dep<-dependencyLoader(c(libraries,sources_common))
  
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  
  rs<-formatter_get_tableinfo(wp_id = wp_id,db_config = db_config)
  if(rs$error) stop(rs$msg)
  data_table_name<-rs$data$data_table_name
  events_table_name<-rs$data$alarms_table_name
  ot_table_name<-rs$data$ot_table_name
  groups_final<-NULL
  
  general_warning<-F
  general_msg<-'ok'
  for(i in 1:length(number_of_groups)){
    objective_gid<-number_of_groups[i]
    cat(objective_gid,'\n')
    continue<-T
    error<-F
    tested_p<-NULL #To avoid infinite bucles
    p<-0.05#Try with 10% of range
    step<-0.01
    step_back<-step/2
    while(continue){
      #cat(p,' ')
      if(p<0){
        continue=F
        error=T
      }
      gid<-1
      threshold<-NA
      dmatrix_tmp<-as.matrix(distance_matrix)
      groups<-data.frame(error=logical(),n_groups=numeric(),gid=numeric(),lds=character(),stringsAsFactors = FALSE) 
      while(is.matrix(dmatrix_tmp)&&(nrow(dmatrix_tmp)>0)&&(gid<=objective_gid)){
        if(dim(dmatrix_tmp)[1]>1){
          d_min<-min(dmatrix_tmp,na.rm=TRUE)
          d_max<-max(dmatrix_tmp,na.rm=TRUE)
          if(is.na(threshold)) threshold<-(d_max-d_min)*p
          r<-which((dmatrix_tmp==d_min),arr.ind = T)[1,1]
          current_ld<-rownames(dmatrix_tmp)[r]
          column_exclude<-which(current_ld==colnames(dmatrix_tmp))
          to_check<-dmatrix_tmp[r,-column_exclude]
          lds_found<-names(to_check)[to_check<=(d_min+threshold)]
          lds<-c(current_ld,lds_found)
          lds_txt<-paste0(lds[order(lds)],collapse = ',')
          groups<-rbind(groups,data.frame(n_groups=objective_gid,gid=gid,lds=lds_txt,stringsAsFactors = FALSE))
          rownames<-rownames(dmatrix_tmp)[(!rownames(dmatrix_tmp) %in% lds)]
          colnames<-colnames(dmatrix_tmp)[(!colnames(dmatrix_tmp) %in% lds)]
          dmatrix_tmp<-as.matrix(dmatrix_tmp[!rownames(dmatrix_tmp) %in% lds,!colnames(dmatrix_tmp) %in% lds])
          rownames(dmatrix_tmp)<-rownames
          colnames(dmatrix_tmp)<-colnames
        }else{
          groups<-rbind(groups,data.frame(n_groups=objective_gid,gid=gid,lds=as.character(rownames(dmatrix_tmp)[1]),stringsAsFactors = FALSE))
          dmatrix_tmp<-NULL
        }
        if(!is.null(dmatrix_tmp))gid<-gid+1
      }
      if(verbose)cat(p,' ')
      if(gid==objective_gid&&is.null(dmatrix_tmp)){
        groups$error[groups$n_groups==objective_gid]<-F
        continue<-FALSE
        error<-F
      }else if(continue==T){
        tested_p<-c(tested_p,p)
        if(gid>objective_gid){
          while((p %in% tested_p)&&(p<1)){
            p<-p+step
          }
        }else{
          while((p %in% tested_p)&&(p>0)){
            p<-p-step
          }
          if(p<0){
            error<-T
            continue<-F
          }
        }
      }else{
        groups<-groups[groups$n_groups!=objective_gid,]
        general_msg<-paste0(general_msg,' cannot create for ',objective_gid,' groups, ')
        general_warning<-T
      }
    }
    groups$priori_avg<-NA
    groups$priori_median<-NA
    groups$priori_sdv<-NA
    groups$count_ots_group<-NA
    groups$count_ots_mean<-NA
    groups$count_ots_sdv<-NA
    groups$count_ots_median<-NA
    
    groups$error<-error
    
    if(!error){
    
      #Load probability
      for(j in 1:nrow(groups)){
        current_group<-groups$lds[j]
        current_group<-current_group[!is.na(current_group)]
        query<-paste0("SELECT ld_id,prob_fault_current from ",paste0(data_table_name,'_priori')," where fault='",fault,"' AND ld_id IN (",current_group,")")
        rs<-db_query(query,db_config);
        if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",rs$msg))
        df<-rs$data %>% group_by(ld_id) %>% summarise_all(funs(mean(., na.rm = T)))
        prob<-by(df,df$ld_id,function(x) sum(x$prob_fault_current,na.rm = T))
        prob<-cbind(prob)
        
        groups$priori_avg[j]<-NA
        groups$priori_median[j]<-NA
        groups$priori_sdv[j]<-NA
        if(nrow(prob)>0){
          groups$priori_avg[j]<-mean(prob,na.rm = T)
          groups$priori_median[j]<-median(prob,na.rm = T)
          groups$priori_sdv[j]<-sd(prob,na.rm = T)
        }
        
        groups$count_ots_group[j]<-NA
        groups$count_ots_mean[j]<-NA
        groups$count_ots_sdv[j]<-NA
        groups$count_ots_median[j]<-NA
        #Number of ots of the group
        if(!is.null(ot_table_name)&&!is.null(array_id_ot)&&any(!is.na(array_id_ot))&&nchar(array_id_ot)>1){
          query<-paste0("SELECT COUNT(distinct ot) as count,et.ld_id from ",ot_table_name," et where et.ld_id IN(",current_group,") AND et.id_ot IN(",array_id_ot,") group by et.ld_id,et.ot,et.date_time")
          rs<-db_query(query,db_config);
          if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",rs$msg))
          if(nrow(rs$data)>0){
            groups$count_ots_group[j]<-sum(rs$data$count,na.rm = T)
            groups$count_ots_mean[j]<-sum(rs$data$count,na.rm = T)/length(unlist(strsplit(current_group,split = ',')))
            t<-table(rs$data)
            groups$count_ots_sdv[j]<-ifelse(ncol(t)>1,sd(t[1,],na.rm = T),0)
            groups$count_ots_median[j]<-median(t[1,],na.rm = T)
          }
        }
      }
    }else{
      sel<-which(groups$n_groups!=objective_gid)
      sel<-c(sel,which(groups$n_groups==objective_gid)[1])
      groups<-groups[sel,]
      groups$lds<-NA
      groups$gid<-NA
    }
    groups_final<-rbind(groups_final,groups)
  }
  return(list(error=F,warning=general_warning,data=list(groups=groups_final),msg=general_msg))
}