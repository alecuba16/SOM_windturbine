calculate_mean_centroids = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,,drop=F])
}

calculate_median_centroids = function(i, dat, clusters) {
  ind = (clusters == i)
  dat[ind,]
  apply(dat[ind,,drop=F], 2, FUN = median) 
}

get_matrix_pos<-function(rows,cols,ncol){
  if(all(is.null(cols))) cols<-1:(ncol^2)
  if(is.logical(cols)) cols<-which(cols)
  if(rows>1){
    return((rows*(ncol^2))+cols)
  }else{
    return(cols)
  }
}

# calculate_si<-function(row){
#   current_cluster_points<-(clusters==clusters[row]) #current_cluster_of_row
#   if(length(current_cluster_points)==0) next()
#   current_cluster_points<-d[get_matrix_pos(row,current_cluster_points,ncol)]
#   #if(length(current_cluster_points)==0) return(0)
#   if(length(current_cluster_points)==0) next()
#   other_points<-(clusters!=clusters[row]) #current_cluster_of_row
#   other_points<-d[get_matrix_pos(row,other_points,ncol)]
#   ai<-mean(d[get_matrix_pos(row,NULL,ncol)]-current_cluster_points,na.rm=T)
#   bi<-min(d[get_matrix_pos(row,NULL,ncol)]-other_points,na.rm = T)
#   return((bi-ai)/max(ai,bi,na.rm = T))
# }

calculate_si<-function(row){
  current_cluster_points<-(clusters==clusters[row]) #current_cluster_of_row
  if(length(current_cluster_points)==0) next()
  current_cluster_points<-tmp_xy[current_cluster_points,]
  #if(length(current_cluster_points)==0) return(0)
  if(length(current_cluster_points)==0) next()
  other_points<-(clusters!=clusters[row]) #current_cluster_of_row
  other_points<-tmp_xy[other_points,]
  others_cluster<-unique(clusters[(clusters!=clusters[row])])
  ai<-mean(sqrt((tmp_xy[row,1]-current_cluster_points[,1])^2+(tmp_xy[row,2]-current_cluster_points[,2])^2),na.rm=T)
  bi<-NA
  for(cluster in others_cluster){
    if(is.na(bi)){
      bi<-mean(sqrt((tmp_xy[(clusters==cluster),1]-current_cluster_points[,1])^2+(tmp_xy[(clusters==cluster),1]-current_cluster_points[,2])^2),na.rm=T)
    }else{
      bi<-min(bi,mean(sqrt((tmp_xy[(clusters==cluster),1]-current_cluster_points[,1])^2+(tmp_xy[(clusters==cluster),1]-current_cluster_points[,2])^2),na.rm=T))
    }
  }
  return((bi-ai)/max(ai,bi,na.rm = T))
}

calcule_metrics<-function(clusters=NULL,matrix_xy=NULL){
  #Centroids
  tmp_centroids<-sapply(unique(clusters),calculate_mean_centroids,matrix_xy,clusters)
  tmp_centroids<-t(tmp_centroids)
  #Quality "Cluster Quality Based Performance Evaluation of Hierarchical Clustering Method"
  #### TO CHECK!
  #Measure validity index https://pdfs.semanticscholar.org/c4f9/df3c66105382d05e58ec35faa8d435f55c91.pdf
  clusters_list<-unique(clusters)
  ri<-NA
  for(i in clusters_list){
    card_c_i<-sum(matrix_xy[clusters==i,])        
    rij<-NA
    #Rij
    for(j in clusters_list[clusters_list!=i]){
      card_c_j<-sum(matrix_xy[clusters==j,])
      si<-(1/card_c_i)*sum((matrix_xy[clusters==i,]-tmp_centroids[i,1])^2)
      sj<-(1/card_c_j)*sum((matrix_xy[clusters==j,]-tmp_centroids[j,1])^2)
      dij<-sum((tmp_centroids[i,1]-tmp_centroids[j,1])^2)
      rij<-c(rij,((si+sj)/dij))
    }
    #Ri
    if(is.na(ri))
      ri<-max(rij,na.rm = T)
    else
      ri<-ri+max(rij,na.rm=T)
  }
  davbould<-(1/length(clusters_list))*ri
  
  #Measure Cohesion
  sse<-0
  for(c in unique(clusters)){
    sse<-sse+sum((matrix_xy[clusters==c,]-tmp_centroids[c,])^2)
  }
  rm(tmp_centroids)
  invisible(gc(verbose = F))
  
  ####------------
  
  
  #Measure Silhouette Index
  
  cores<-floor(4)
  log_file<-'cluster_best_nclusters.log'
  if(Sys.info()["sysname"]=="Linux"){
    si<-mclapply(1:nrow(matrix_xy),mc.cores = cores,FUN = calculate_si)
  }else{
    cl <- makePSOCKcluster(cores,outfile=log_file)
    setDefaultCluster(cl)
    #clusterExport(cl, c("ld_id","calculate_mean_centroids","wtdata_bmu",'som_pts'))
    si<-parLapply(cl,1:nrow(matrix_xy),fun = calculate_si,matrix_xy,clusters)
    stopCluster(cl)
  }
  si<-do.call("rbind",si)
  invisible(gc(verbose = F))
  n_si_bt_0<-sum(si>0.3,na.rm = T)
  n_si_lt_0<-sum(si<(-0.3),na.rm = T)
  n_si_eq_0<-sum((si>(-0.3))&(si<0.3),na.rm = T)
  si<-data.frame(bt0=n_si_bt_0,lt0=n_si_lt_0,eq0=n_si_eq_0)
  
  return(list(error=F,data=list(davbould=davbould,sse=sse,si=si),msg='ok'))
}

create_bmu_clustering<-function(nclusters=NULL,wtdata_bmu=NULL,ld_id=NULL,som_pts=NULL,clustering_method='hierarchical',use_mean=T,use_median=F,verbose=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  libraries<-c('wordspace')
  dep<-dependencyLoader(libraries)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(som_pts)) return(list(error=T,data=NULL,msg='Error som_pts is null'))
  if(is.null(nclusters)&&clustering_method!='hierarchical') return(list(error=T,data=NULL,msg='Error nclusters is null and clustering_method is not "hierarchical"'))
  if(is.null(wtdata_bmu)) return(list(error=T,data=NULL,msg='Error wtdata_bmu is null'))
  if(is.null(ld_id)) return(list(error=T,data=NULL,msg='Error ld_id is null'))
  set.seed(1)
  ncol<-max(som_pts)
  df<-NULL
  dynamicTree<-(is.null(nclusters)||!is.numeric(nclusters))
  
  total<-length(unique(ld_id))
  for(ld in unique(ld_id)){
    if(verbose){
      sink(stderr())
      cat(paste0('\n',(total-length(unique(ld_id))+1)," of ",total))
      sink()
    }
    #cat(paste0("Nclusters:",nclusters," ld_id:",ld,"  "))
    rows<-(ld_id==ld)
    bmus<-wtdata_bmu[rows&!is.na(wtdata_bmu)]
    wtdata_bmu<-wtdata_bmu[!rows]
    ld_id<-ld_id[!rows]
    invisible(gc(verbose = F))
    
    if(length(bmus)==0) next()
    num_rows<-sum(rows)
    tmp_xy<-matrix(c(som_pts[bmus,1],som_pts[bmus,2]),nrow=num_rows)
    tmp_xy<-unique(tmp_xy)
    #Free memory
    #rm(list=c('rows'))
    #Scale
    tmp_xy_scaled<-as.matrix(scale(tmp_xy,center = T,scale = T))
    invisible(gc(verbose = F))
    
    if(clustering_method=='hierarchical'){
      #Distances
      rs<-try(d<-dist(tmp_xy_scaled),silent = T)
      if(!inherits(rs, "try-error") ){
        #Hierarchical Clustering
        #determine num clust
        clusters<-hclust(d)
        if(dynamicTree){
          clusters<-cutreeDynamic(clusters, method = "hybrid", distM=as.matrix(d),deepSplit = FALSE,verbose = verbose)
          nclusters<-length(unique(clusters))
        }else{
          clusters<-cutree(clusters, nclusters) 
        }
        rm(list=c('d','rs'))
        invisible(gc(verbose = F))
      }else{
        rm(rs)
        sink(stderr())
        cat(paste0("\nWarning at ",iam,": cannot allocate space for ",clustering_method," using kmeans instead."))
        sink()
        clustering_method<-'kmeans'
        clusters<-kmeans(tmp_xy_scaled,nclusters)$cluster
      }
    }else if(clustering_method=='kmeans'){
      clusters<-kmeans(tmp_xy_scaled,nclusters)$cluster
    }else if(clustering_method=='optics'){
      return(list(error=T,data=NULL,msg="TODO IMPLEMENT OPTICS"))
    }else if(clustering_method=='dbscan'){
      return(list(error=T,data=NULL,msg="TODO IMPLEMENT DBSCAN"))
    }
    rm(tmp_xy_scaled)
    #tmp_xy_scaled<-tmp_xy
    #tmp_xy<-matrix(c(som_pts[bmus,1],som_pts[bmus,2]),nrow=num_rows)
    #Return to wtdata_bmu length
    tmpclust<-data.frame(x=tmp_xy[,1],y=tmp_xy[,2],cluster=clusters)
    tmp_xy<-matrix(c(som_pts[bmus,1],som_pts[bmus,2]),nrow=num_rows)
    clusters<-apply(tmp_xy,1,function(xy) tmpclust$cluster[(xy[1]==tmpclust$x)&(xy[2]==tmpclust$y)])
    rm(tmpclust)
    if(use_mean) tmp_centroids<-t(sapply(unique(clusters),calculate_mean_centroids, tmp_xy,clusters))
    if(use_median) tmp_centroids<-t(sapply(unique(clusters),calculate_mean_centroids, tmp_xy,clusters))
    colnames(tmp_centroids)<-c('x','y')
    rownames(tmp_centroids)<-1:nclusters
    #df<-rbind(df,list(ld_id=ld,clusters=clusters,cluster_centroids=tmp_centroids,active_bmus_xy_scaled=tmp_xy_scaled,active_bmus_xy=tmp_xy,calcule_metrics=calcule_metrics))
    df<-rbind(df,list(ld_id=ld,clusters=clusters,cluster_centroids=tmp_centroids,active_bmus_xy=tmp_xy,calcule_metrics=calcule_metrics))
    
    rm(list=c('tmp_xy','clusters'))
    invisible(gc(verbose = F))
  }
  
  clusters_xy<-list(ld_id=unlist(df[,'ld_id']),clusters_machines=df[,'clusters'])
  centroids<-NULL
  for(d in 1:nrow(df)){
    nclusters<-length(unique(df[[d,'clusters']]))
    ld_id<-rep(df[[d,'ld_id']],nclusters)
    x<-df[[d,'cluster_centroids']][,1]
    y<-df[[d,'cluster_centroids']][,2]
    cluster<-1:nclusters
    centroids<-rbind(centroids,data.frame(ld_id=ld_id,x=x,y=y,cluster=cluster))
  }
  centroids$ld_id<-as.factor(centroids$ld_id)
  
  if(verbose){
    sink(stderr())
    cat(paste0('\nFinish ',iam))
    sink()
  }
  
  return(list(error=F,data=list(clustering_method=clustering_method,clustering_result=df,clusters_xy=clusters_xy,centroids=centroids),msg='ok'))
}
