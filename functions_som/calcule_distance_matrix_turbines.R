calcule_simplex<-function(centroids=NULL,nclusters=NULL){
  #TODO prepare compatible with dynamic ncluster size
  if(is.null(centroids))return(list(error=T,warning=F,data=NULL,msg="centroids is null"))

  lds_ids<-as.numeric(unique(centroids$ld_id))
  a<-NULL
  #columns<-((factorial(length(lds_ids))/ ( factorial(2)*factorial(length(lds_ids)-2)))*(nclusters^2))+1
  #rows<-((factorial(length(lds_ids))/ ( factorial(2)*factorial(length(lds_ids)-2)))*nclusters)+1
  columns<-((factorial(length(lds_ids))/ ( factorial(2)*factorial(length(lds_ids)-2)))*(nclusters^2))
  rows<-((factorial(length(lds_ids))/ ( factorial(2)*factorial(length(lds_ids)-2)))*nclusters)
  A<-matrix(0,ncol=columns,nrow=rows)
  for(f in 1:length(lds_ids)){
    first_xy<-centroids[centroids$ld_id == lds_ids[f],c('x','y','cluster')]
    if(f<length(lds_ids)){
      for(s in (f+1):length(lds_ids)){
        second_xy<-centroids[centroids$ld_id == lds_ids[s],c('x','y','cluster')]
        for(r in 1:nclusters){
          x1<-first_xy[first_xy$cluster==r,c('x','y')]
          for(c in 1:nclusters){
            x2<-second_xy[second_xy$cluster==c,c('x','y')]
            a<-c(a,dist(rbind(x1,x2)))
            names(a)[length(a)]<-paste0(lds_ids[f],'_',lds_ids[s],'_',r,'_',c)
          }
        }
      }
    }
  }
  row_indicator<-unique(gsub(replacement = '\\1',x = names(a),pattern ='(.*)_[0-9]+'))
  for(r in 1:length(row_indicator)){
    A[r,grepl(x = names(a),pattern =paste0(row_indicator[r],'_[0-9]+'))]<-1
  }
  rownames(A)<-row_indicator
  colnames(A)<-names(a)
  
  b<-rep(1,rows)
  res<-solveLP(a,b,A,maximum=FALSE, const.dir = rep("=",rows),lpSolve = T)
  if(!res$lpSolve||(res$status!=0))return(list(error=T,data=NULL,msg="problem with solveLP simplex"))
  solution<-res$solution[res$solution==1]
  
  pairs<-data.frame(first_ld_id=numeric(),first_cluster=numeric(),second_ld_id=numeric(),second_cluster=numeric(),stringsAsFactors = F)
  for(i in 1:length(solution)){
    splitted<-unlist(strsplit(names(solution)[i],split = '_'))
    pairs<-rbind(pairs,data.frame(first_ld_id=as.numeric(splitted[1]),first_cluster=as.numeric(splitted[3]),second_ld_id=as.numeric(splitted[2]),second_cluster=as.numeric(splitted[4]),stringsAsFactors = F))
  }
  
  distance_matrix<-matrix(, nrow = length(lds_ids), ncol = length(lds_ids))
  rownames(distance_matrix)<-lds_ids
  colnames(distance_matrix)<-lds_ids
  
  pairs$distance<-NA
  for(i in 1:nrow(pairs)){
    fc<-centroids[(centroids$ld_id==pairs$first_ld_id[i])&(centroids$cluster==pairs$first_cluster[i]),c('x','y')]
    sc<-centroids[(centroids$ld_id==pairs$second_ld_id[i])&(centroids$cluster==pairs$second_cluster[i]),c('x','y')]
    d<-dist(rbind(fc,sc))
    pairs$distance[i]<-d
  }
  for(first in lds_ids){
    for(second in lds_ids[lds_ids!=first]){
      if(sum((pairs$first_ld_id==first)&(pairs$second_ld_id==second))>0){
        value<-mean(pairs$distance[(pairs$first_ld_id==first)&(pairs$second_ld_id==second)],na.rm=T)
        distance_matrix[rownames(distance_matrix)==first,colnames(distance_matrix)==second]<-value
        distance_matrix[rownames(distance_matrix)==second,colnames(distance_matrix)==first]<-value
      }
    }
  }
  distance_matrix<-as.data.frame(distance_matrix)
  return(list(error=F,warning=F,data=list(distance_matrix=distance_matrix,clusters_pairs=pairs),msg='ok'))
}


calcule_exhaustive<-function(centroids=NULL,nclusters=NULL){
  if(is.null(centroids))return(list(error=T,data=NULL,msg="centroids is null"))
  
  recursive_tree<-function(tree,c,current_total,selected){
    if(c==nclusters){
      to_check<-1:nclusters
      to_check<-to_check[!to_check %in% selected]
      ret_current_total<-current_total+min(tree[to_check,c])
      ret_selected<-c(selected,to_check[which(min(tree[to_check,c],na.rm=TRUE)==tree[to_check,c])])
    } else {
      tmp_current_total<-NA
      tmp_selected<-NA
      to_check<-1:nclusters
      if(!is.null(selected)&&length(selected)>0) to_check<-to_check[!to_check %in% selected]
      for(r in to_check){
        tmp<-recursive_tree(tree=tree,c=c+1,current_total=current_total+tree[r,c],selected=c(selected,r))
        if(length(tmp$selected)==nclusters&&(is.na(tmp_current_total)||(!is.na(tmp_current_total)&&(tmp$current_total<tmp_current_total)))){
          tmp_current_total<-tmp$current_total
          tmp_selected<-tmp$selected
        }
      }
      ret_current_total<-tmp_current_total
      ret_selected<-tmp_selected
    }
    return(list(current_total=ret_current_total,selected=ret_selected))
  }
  
  
  # Construct pair matrix
  lds_ids<-unique(centroids$ld_id)
  distance_matrix<-matrix(, nrow = length(lds_ids), ncol = length(lds_ids))
  rownames(distance_matrix)<-lds_ids
  colnames(distance_matrix)<-lds_ids
  nclusters<-length(unique(centroids$cluster))
  
  clusters_pairs<-data.frame(first_ld_id=numeric(),second_ld_id=numeric(),first_cluster=numeric(),second_cluster=numeric())
  for(first in lds_ids){
    first_xy<-centroids[centroids$ld_id == first,c('x','y','cluster')]
    for(second in lds_ids[lds_ids != first]){
      second_xy<-centroids[centroids$ld_id == second,c('x','y','cluster')]
      totaldist<-0
      tmp_d_matrix<-matrix(, nrow = nclusters, ncol = nclusters)
      for(r in 1:nclusters){
        x1<-first_xy[first_xy$cluster==r,c('x','y')]
        for(c in 1:nclusters){
          x2<-second_xy[second_xy$cluster==c,c('x','y')]
          tmp_d_matrix[r,c]<-dist(rbind(x1,x2))
        }
      }
      
      #Best pairs selection
      selected<-c()
      result<-recursive_tree(tree=tmp_d_matrix,c=1,current_total=0,selected=NULL)
      totaldist<-result$current_total
      selected<-result$selected
      selected<-t(sapply(1:length(selected),function(i) c(selected[i],i)))
      selected<-as.data.frame(selected)
      rownames(selected)<-1:nclusters
      colnames(selected)<-c(second,first)
      clusters_pairs<-rbind(clusters_pairs,data.frame(first_ld_id=rep(first,nclusters),second_ld_id=rep(second,nclusters),first_cluster=selected[,2],second_cluster=selected[,1]))
      
      avgdist<-totaldist/nclusters
      distance_matrix[which(rownames(distance_matrix)==first),which(colnames(distance_matrix)==second)]<-avgdist
    }
  }
  distance_matrix<-as.data.frame(distance_matrix)
  return(list(error=F,warning=F,data=list(distance_matrix=distance_matrix,clusters_pairs=clusters_pairs),msg='ok'))
}

calcule_distance_matrix_turbines<-function(centroids=NULL,use_simplex=F,super_centroid_of_turbines=NULL,medioid=F,nclusters=NULL){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  dep<-dependencyLoader('linprog')
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(centroids))return(list(error=T,warning=F,data=NULL,msg="centroids is null"))
  
  #Check if same number of centroids
  if(is.factor(centroids$ld_id)) centroids$ld_id<-as.numeric(levels(centroids$ld_id)[centroids$ld_id])
  ncentroids<-centroids[,c('ld_id','cluster')]%>% group_by(ld_id) %>% summarise(count=length(cluster))
  if(length(unique(ncentroids$count))!=1) return(list(error=F,warning=T,data=NULL,msg="Calculate distance matrix is not implemented for different number of centroids"))
  nclusters<-unique(ncentroids$count)
  
  if(use_simplex){
    rs<-calcule_simplex(centroids=centroids,nclusters=nclusters)
  }else{
    rs<-calcule_exhaustive(centroids=centroids,nclusters=nclusters)
  }
  if(rs$error) return(list(error=T,warning=F,data=NULL,msg=rs$msg))
  clusters_pairs<-rs$data$clusters_pairs
  if(!is.null(super_centroid_of_turbines)){
    reference_ld_id<-super_centroid_of_turbines[1]
    nclusters<-length(unique(centroids$cluster))
    reference_machine<-(clusters_pairs$first_ld_id==reference_ld_id)&(clusters_pairs$second_ld_id %in% super_centroid_of_turbines)
    lds_ids<-super_centroid_of_turbines[2:length(super_centroid_of_turbines)]
    super_centroid<-data.frame(cluster=numeric(),x=numeric(),y=numeric())
    for(c in 1:nclusters){
      tmp<-clusters_pairs[reference_machine&(clusters_pairs$second_ld_id %in% lds_ids)&(clusters_pairs$first_cluster==c),]
      x<-centroids[(centroids$ld_id == reference_ld_id)&(as.numeric(rownames(centroids))==tmp$first_cluster[1]),'x']
      y<-centroids[(centroids$ld_id == reference_ld_id)&(as.numeric(rownames(centroids))==tmp$first_cluster[1]),'y']
      for(r in 1:nrow(tmp)){
        x<-c(x,centroids[(centroids$ld_id == tmp$second_ld_id[r])&(centroids$cluster==tmp$second_cluster[r]),'x'])
        y<-c(y,centroids[(centroids$ld_id == tmp$second_ld_id[r])&(centroids$cluster==tmp$second_cluster[r]),'y'])
      }
      if(medioid){
        x<-median(x,na.rm=T)
        y<-median(y,na.rm=T)
      }else{
        x<-mean(x,na.rm=T)
        y<-mean(y,na.rm=T)
      }
      super_centroid<-rbind(super_centroid,data.frame(cluster=c,x=x,y=y))
    }
  }else{
    super_centroid<-NULL
    reference_ld_id<-NULL
  }
  
  return(list(error=F,warning=F,data=list(distance_matrix=rs$data$distance_matrix,clusters_pairs=rs$data$clusters_pairs,super_centroid=super_centroid,reference_ld_id=reference_ld_id),msg='ok'))
}