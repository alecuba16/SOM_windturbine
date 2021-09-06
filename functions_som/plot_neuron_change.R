plot_neuron_change<-function(wtdata_bmu=NULL,wtdata=NULL,som_pts=NULL,som_codes=NULL,model_params=NULL,seconds_to_aggregate=86400,plotly=F,use_median=F,interactive=F,nsdv=6){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('ggplot2','plotly','htmltools','functions_som/plot_umatrix.R','functions_som/create_umatrix.R')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata)) return(list(error=T,data=NULL,msg="wtdata is null"))
  if(is.null(wtdata_bmu)) return(list(error=T,data=NULL,msg="wtdata_bmu is null"))
  if(is.null(som_codes)) return(list(error=T,data=NULL,msg="som_codes is null"))
  
  lds_ids<-unique(wtdata$ld_id)
  neurons<-NULL
  values<-NULL
  
  if(model_params$hex){
    precision_x<-unique(som_pts$x-floor(som_pts$x))
    precision_x<-min(precision_x[precision_x>0])
    precision_y<-unique(som_pts$y-floor(som_pts$y))
    precision_y<-min(precision_y[precision_y>0])
  }else{
    precision_x<-1
    precision_y<-1
  }
  
  tmp<-wtdata[,c('date_time','ld_id')]
  if(!is.null(seconds_to_aggregate)) tmp$date_time<-as.POSIXct((as.numeric(wtdata$date_time)%/%seconds_to_aggregate)*seconds_to_aggregate,origin='1970-01-01',tz = "UTC")
  if(use_median){
    fn<-median
  }else{
    fn<-mean
  }
  
  for(ld in lds_ids){
    for(dt in unique(tmp$date_time[tmp$ld_id==ld])){
      selected<-som_pts[wtdata_bmu[(ld==tmp$ld_id)&(tmp$date_time==dt)],]
      x<-(fn(selected$x,na.rm = T)%/%precision_x)*precision_x
      y<-(fn(selected$y,na.rm = T)%/%precision_y)*precision_y
      neurons<-rbind(neurons,data.frame(ld_id=ld,date_time=dt,neuron=paste0('x',x,'_y',y),x=x,y=y))
    }
  }
  neurons$ld_id<-as.factor(neurons$ld_id)
  neurons$date_time<-as.POSIXct(neurons$date_time,tz='UTC',origin='1970-01-01')
  
  #p<-ggplot(neurons)+geom_point(aes(x=date_time,y=neuron,color=ld_id,text=paste0('date_time:',date_time,'<br>neuron:',neuron)))
  #if(plotly) p<-ggplotly(p,tooltip = c("text"))
  
  #Calculate median at date_time
  dist_ref<-NULL
  for(dt in unique(neurons$date_time)){
    xy<-neurons[neurons$date_time==dt,c('ld_id','x','y')]
    tmp_med_xy<-c(median(xy[,2]),median(xy[,3]))
    for(ld in lds_ids){
      if(any(xy$ld_id==ld)) dist_ref<-rbind(dist_ref,data.frame(ld_id=ld,date_time=dt,dist=as.numeric(dist(rbind(tmp_med_xy,xy[xy$ld_id==ld,c('x','y')])))))
    }
  }
  dist_ref$date_time<-as.POSIXct(dist_ref$date_time,tz='UTC',origin='1970-01-01')
  dist_ref$ld_id<-as.factor(dist_ref$ld_id)
  threshold_up<-mean(dist_ref$dist)+(nsdv*sd(dist_ref$dist))
  if(plotly){
    p<-ggplot(dist_ref)+geom_point(aes(x=date_time,y=dist,color=ld_id,text=paste0('ld_id:',ld_id,'<br>date_time:',date_time,'<br>distance:',dist)))
  }else{
    p<-ggplot(dist_ref)+geom_point(aes(x=date_time,y=dist,color=ld_id))
  }
  p<- p + geom_hline(yintercept = threshold_up,show.legend = T)
  p<- p + geom_text(aes(mean(dist_ref$date_time),threshold_up,label = paste0(nsdv,' sdv'), vjust = -2))+theme(legend.position = 'bottom')
  if(plotly) p<-ggplotly(p,tooltip = c("text"))
  
  #### Print umatrix with points over threshold
  dist_ref<-arrange(dist_ref,ld_id,date_time)
  neurons<-arrange(neurons,ld_id,date_time)
  neurons_out<-neurons[dist_ref$dist>=threshold_up,]
  rs<-create_umatrix(som_pts=som_pts,som_codes=som_codes,hex=model_params$hex)
  if(rs$error){
    output_msg <- paste0("\n Report rmd: on call create_umatrix\n",rs$msg)
    close_protocol(output_msg, "Report rmd", debug_mode)
    return(list(error=TRUE,data=NULL,msg=output_msg))
  }
  umatrix<-rs$data$umatrix
  rm(rs)
  invisible(gc(verbose=F))
  if(nrow(neurons_out)>0){
  rs<-plot_umatrix(umatrix=umatrix,som_pts=som_pts,som_cluster=NULL,wtdata_bmu=wtdata_bmu,model_params=model_params,plotly_umatrix=F,perps_umatrix=F,plotly_umatrix_clusters=F,perps_umatrix_clusters=F,legend=T,big_fonts=F,axis_tittle=T,title=F,add_points=neurons_out)
  if(rs$error){
    output_msg <- paste0("\n Report rmd: on call plot_umatrix\n",rs$msg)
    close_protocol(output_msg, "Report rmd", debug_mode)
    return(list(error=TRUE,data=NULL,msg=output_msg))
  }
    umatrix_plot<-rs$data$umatrix_with_clusters
  }else{
    umatrix_plot<-NA
  }
  
  if(interactive){
    neurons$ld_id<-as.numeric(levels(neurons$ld_id))[neurons$ld_id]
    neurons$ld_id[dist_ref$dist>=threshold]<-neurons$ld_id[dist_ref$dist>=threshold]+1000
    neurons$ld_id<-as.factor(neurons$ld_id)
    interactive<-plot_ly(data = arrange(neurons,date_time,ld_id),color = ~ld_id,x=~date_time,y=~x,z=~y,type="scatter3d", mode = "lines+markers") %>% layout(legend = list(orientation = 'h'))
  }else{
    interactive<-NULL
  }
  
  return(list(error=F,data=list(distance_plot=p,umatrix_projection=umatrix_plot,interactive_plot=interactive),msg='ok'))
}