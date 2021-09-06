#devtools::install_github("rwehrens/kohonen/kohonen")
#Example call:
#rs<-create_som_model(dim = seq(30,100,10),rlen = 10,alpha = c(0.05,0.01),wtdata = wtdata,parallel_mode = T,metrics=T,log_path='./a.log',verbose=T)
## Times for model
# wtdata 400.000x81 (escamb@10min@2years *_avg vars) times for 30,40,50,60,70,80,90,100 in minutes: 8,13,21,31,42,56,
## Memory
# wtdata 400.000x81 (escamb@10min@2years *_avg vars) and map 30x30 -> 1,5GB aprox
# wtdata 400.000x81 (escamb@10min@2years *_avg vars) and map 100x100 -> 2,25GB aprox
# wtdata 400.000x81 (escamb@10min@2years *_avg vars) 6 threads 30,40,50,60,70,80 -> 9,75GB
## Evaluation
# wtdata 400.000x81 (escamb@10min@2years *_avg vars) and map 30x30 -> 400MB /thread
# wtdata 400.000x81 (escamb@10min@2years *_avg vars) and map 100x100 -> 700MB /thread
helper_te<-function(rows,som_codes,som_pts){
  te<-apply(X = som_codes,MARGIN = 1,FUN = function(sc){
    te<-apply(X = rows,1,function(r){
      distances<-sapply(X = sc,function(sc){
        return(sqrt(sum(r-sc)^2))
      })
      #Get first distance
      #first_pos<-which(distances==max(distances,na.rm = T)) #Paper test
      first_pos<-which(distances==min(distances,na.rm = T))
      first<-som_pts[first_pos,]
      #second_pos<-which(distances==max(distances[-first_pos],na.rm = T)) #Paper test
      second_pos<-which(distances==min(distances[-first_pos],na.rm = T))
      second<-som_pts[second_pos,]
      #Get distance between first,second neuron must be 1 or 2 (diagonal)
      ind<-abs(second[1]-first[1]+second[2]-first[2])
      return(ifelse(((second[1]!=second[2])&&ind==1) || (second[1]==second[2]&&ind==2),1,0))
    })
    return(te)
  })
  return(te)
}

calculate_metrics<-function(som_codes=NULL,som_pts=NULL,som_distances=NULL,data=NULL,parallel=T,log_file=NULL,verbose=F){
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  libraries<-c('doSNOW','bigmemory','itertools','parallel')
  dep<-dependencyLoader(libraries)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if((is.null(data)||all(is.na(data)))) return(list(error=T,data=NULL,msg="missing data"))
  if((is.null(som_codes)||all(is.na(som_codes)))) return(list(error=T,data=NULL,msg="missing som_codes"))
  if((is.null(som_pts)||all(is.na(som_pts)))) return(list(error=T,data=NULL,msg="missing som_pts"))
  if((is.null(som_distances)||all(is.na(som_distances)))) return(list(error=T,data=NULL,msg="missing som_distances"))
  
  dim=max(som_pts,na.rm = T)#I assume is squared som
  
  #### Measure som quality
  # QE :The quantization error is computed by determining the average distance of
  # the sample vectors to the cluster centroids by which they are represented.
  # In case of the SOM, the cluster centroids are the prototype vectors.
  qe<-mean(som_distances,na.rm=T)
  
  #Topographic error, the proportion of all data vectors for which first and second BMUs are not adjacent units. Measures topology preservation.
  #http://www.cis.hut.fi/somtoolbox/package/docs2/som_quality.html
  mb_per_thread<-500
  if(Sys.info()[['sysname']]=="Linux"){
    free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  }else{#windows
    free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))/1024
  }
  if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1024
  num_threads<-floor(free/mb_per_thread)
  
  if((parallel&&(num_threads>1))){
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(nrow(data),floor(num_threads)) #Model in memory uses about 3% memory @600 @2years
    time_ini<-Sys.time()
    if(verbose)cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) INI parallel(',num_threads,' threads):',time_ini,'\n'))
    #Divide work
    chunk_length=floor(nrow(data)/num_threads)
    if(!is.null(log_file))
      cl <- parallel::makePSOCKcluster(num_threads,outfile=log_file)
    else
      cl <- parallel::makePSOCKcluster(num_threads)
    doSNOW::registerDoSNOW(cl)
    max_rows<-nrow(data)
    x<-bigmemory::as.big.matrix(as.matrix(data))
    rm(data)
    gc(verbose = F)
    mdesc <- describe(x)
    te <- foreach(i = seq(from = 1,to = max_rows,by = chunk_length),.export = 'helper_te',.verbose = F, .combine = 'c') %dopar% {
      wtdata<-bigmemory::attach.big.matrix(mdesc)
      helper_te(wtdata[i:min(max_rows,i+chunk_length-1),],som_codes,som_pts)
    }
    stopCluster(cl)
    rm(list=c('cl','num_threads','log_file','free','mb_per_thread'))
    time_ini
    if(verbose){
      cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) END parallel:',Sys.time(),', took '))
      Sys.time() - time_ini
      cat('\n')
    }
  }else{
    time_ini<-Sys.time()
    if(verbose)cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) INI sequential:',time_ini,'\n'))
    te<-apply(data,1,FUN = helper_te,som_codes,som_pts)
    if(verbose){
      cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) END sequential:',Sys.time(),', took '))
      Sys.time() - time_ini
      cat('\n')
    }
  }
  te<-sum(te)/max_rows
  return(list(error=F,data=list(qe=qe,te=te),msg='ok'))
}

som_vars<-function(model=NULL){
  if(is.null(model)) return(list(error=T,data=NULL,msg="model cannot be null"))
  if(!("unit.classif" %in% names(model))||(("unit.classif" %in% names(model))&&is.null(model$unit.classif))) return(list(error=T,data=NULL,msg="model is corrupt missing unit.classif"))
  if(!("distances" %in% names(model))||(("distances" %in% names(model))&&is.null(model$distances))) return(list(error=T,data=NULL,msg="model is corrupt missing distances"))
  if(!("codes" %in% names(model))||(("codes" %in% names(model))&&is.null(model$codes))) return(list(error=T,data=NULL,msg="model is corrupt missing codes"))
  if(!("grid" %in% names(model))||(("grid" %in% names(model))&&!("pts" %in% names(model$grid)))||(("grid" %in% names(model))&&("pts" %in% names(model$grid))&&is.null(model$grid$pts))) return(list(error=T,data=NULL,msg="model is corrupt missing unit.classif"))
  data<-ifelse("data" %in% names(model),model$data[1],NA)
  return(list(error=F,data=list(wtdata_bmu=model$unit.classif,som_distances=model$distances,som_codes=model$codes[[1]],som_pts=model$grid$pts,data=data),msg="ok"))
}

helper_model<-function(dim,wtdata,alpha,rlen,verbose){
  if(dim*dim>nrow(wtdata)) return(list(error=TRUE,msg=paste0('\ncannot create map bigger than:',floor(sqrt(nrow(wtdata))))))
  if(verbose) cat(paste0('\nSOM ',dim,' start:',Sys.time()))
  set.seed(1)
  dim<-min(dim*dim,nrow(wtdata))
  dim<-floor(sqrt(dim))
  som_grid <- kohonen::somgrid(xdim = dim, ydim=dim, topo="rectangular")
  som_model <- kohonen::som(as.matrix(wtdata), grid=som_grid, rlen=rlen,  alpha=alpha, keep.data = T)
  rm(som_grid)
  if(verbose) cat(paste0('\nSOM ',dim,' end:',Sys.time(),'\n'))
  som_model$model_columns
  return(list(error=F,data=list(dim=dim,model=som_model),msg='ok'))
}

create_som_model<-function(wtdata=NULL,dim=NULL,rlen=10,alpha=c(0.05,0.01),parallel_mode=F,log_file=NULL,verbose=F,normalize=T){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  libraries<-c('kohonen','doSNOW','bigmemory','itertools','parallel')
  dep<-dependencyLoader(libraries)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(!is.null(log_file)&&file.exists(log_file)) file.remove(log_file)
  
  if(normalize){
    #Scale
    sdv<-apply(wtdata,2,sd,na.rm=T)
    zero_sdv_columns<-(sdv==0)
    wtdata<-wtdata[,!zero_sdv_columns]
    sdv<-sdv[!zero_sdv_columns]
    mean<-sapply(1:ncol(wtdata), function(c) mean(wtdata[,c],na.rm=T))
    selected_columns<-colnames(wtdata)
    wtdata<-sapply(1:ncol(wtdata), function(c) {(wtdata[,c] - mean[c])/sdv[c]}) 
    stat<-data.frame(name=selected_columns,mean=mean,sdv=sdv)
  }else{
    #Selected columns
    selected_columns<-colnames(wtdata)
  }
  
  #complete cases only
  selected_rows<-complete.cases(wtdata)
  wtdata<-wtdata[selected_rows,]
  #To matrix
  wtdata<-as.matrix(wtdata)
  gc(verbose=F)
  
  ###############################################
  #Step 1 create models in parallel
  ###############################################
  mb_per_thread<-2100 #Worst case
  if(Sys.info()[['sysname']]=="Linux"){
    free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))
  }else{#windows
    free<-as.numeric(gsub(x=system("wmic OS get FreePhysicalMemory /Value"),pattern = '.*=([0-9]+)',replacement = '\\1'))
  }
  if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1048576
  num_threads<-floor((free/1024)/mb_per_thread)
  num_threads<-min(num_threads,length(dim))
  
  if(length(dim)>1&&parallel_mode&&num_threads>1){
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(length(dim),floor(num_threads))
    if(verbose) cat(paste0('SOM for:',paste0(dim,collapse = ','),' parallel (',num_threads,' threads)\n'))
    if(!is.null(log_file))
      cl <- parallel::makePSOCKcluster(num_threads,outfile=log_file)
    else
      cl <- parallel::makePSOCKcluster(num_threads)
    
    doSNOW::registerDoSNOW(cl)
    x<-as.big.matrix(wtdata)
    mdesc <- describe(x)
    model <- foreach(i = 1:length(dim),.export = c('helper_model','helper_te'),.packages = 'bigmemory',.verbose = F, .combine = 'rbind') %dopar% {
      wtdata<-bigmemory::attach.big.matrix(mdesc)
      helper_model(dim[i],wtdata,alpha,rlen,verbose)
    }
    stopCluster(cl)
    rm(x)
    gc(verbose = F)
  }else{
    if(verbose)cat(paste0('SOM for:',paste0(dim,collapse = ','),' sequential mode\n'))
    model<-lapply(dim,FUN = helper_model,wtdata,alpha,rlen,verbose)
    model<-do.call("rbind",model)
  }
  #model<-as.data.frame(model)
  #dim<-unlist(model$dim,use.names = F)
  wtdata_bmu<-NULL
  som_distances<-NULL
  som_codes<-NULL
  som_pts<-NULL
  som_model<-NULL
  outstat<-NULL
  stored_pos<-1
  for(m in 1:nrow(model)){
    if(!model[[m,'error']]){
      data<-model[[m,'data']]
      dim<-data[['dim']]
      tmp_data<-som_vars(data[['model']])
      if(tmp_data$error) return(list(error=T,data=NULL,msg=tmp_data$msg))
      wtdata_bmu<-rbind(wtdata_bmu,tmp_data$data$wtdata_bmu)
      som_distances<-rbind(som_distances,tmp_data$data$som_distances)
      som_codes<-c(som_codes,list(tmp_data$data$som_codes)) #matrix don't unlist
      rownames(som_codes[[stored_pos]])<-paste0(unlist(lapply(1:dim,function(j) rep(j,dim))),'_',seq(1,dim))  
      som_pts<-c(som_pts,list(tmp_data$data$som_pts)) #matrix don't unlist
      rownames(som_pts[[stored_pos]])<-paste0(unlist(lapply(1:dim,function(j) rep(j,dim))),'_',seq(1,dim))  
      som_model<-rbind(som_model,data['model'])
      outstat<-rbind(outstat,stat)
      stored_pos<-stored_pos+1
    }
  }
  
  rm(model)
  gc(verbose = F)
  return(list(error=FALSE,data=list(get_som_vars=som_vars,get_metrics=calculate_metrics,model=som_model,stat=stat,wtdata_bmu=wtdata_bmu,som_distances=som_distances,som_codes=som_codes,som_pts=som_pts,selected_rows=selected_rows,selected_columns=selected_columns),msg='ok'))
}
