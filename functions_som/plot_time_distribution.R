plot_time_distribution<-function(wtdata=NULL,som_pts=NULL,wtdata_bmu=NULL,model_params=NULL,legend=T,big_fonts=F,axis_tittle=T,title=T,date_time_name='date_time'){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  #options(rgl.useNULL=TRUE)
  #sources<-c('ggplot2','plotly','grid','gridGraphics','akima','rgl','RColorBrewer')
  #knit_hooks$set(webgl = hook_webgl)
  sources<-c('ggplot2','plotly','grid','gridGraphics','akima','plot3D','RColorBrewer')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(som_pts))return(list(error=T,data=NULL,msg="som_pts is null"))
  
  df<-data.frame(x=som_pts[wtdata_bmu,1],y=som_pts[wtdata_bmu,2])
  df<-unique(df)
  df<-na.omit(df)
  df$distance<-NA
  df<-df[order(df$x,df$y),]
  first_ld_dt<-wtdata[(wtdata$ld_id==unique(wtdata$ld_id)[1]),date_time_name]
  first_ld_dt<-sort(first_ld_dt)
  seconds_to_aggregate<-as.numeric(median(diff(first_ld_dt),na.rm=T), units = "secs")
  rm(first_ld_dt)
  invisible(gc(verbose = F))
  
  for(i in 1:nrow(df)){
    dists<-NULL
    selected_rows<-which((som_pts$x[wtdata_bmu]==df$x[i])&(som_pts$y[wtdata_bmu]==df$y[i]))
    if(length(selected_rows)>0){
      for(j in 1:length(selected_rows)){
        target_time<-wtdata[selected_rows[j],date_time_name]+as.difftime(seconds_to_aggregate, units="secs")
        target_ld<-wtdata[selected_rows[j],'ld_id']
        tmp_lds<-which(wtdata[,'ld_id']==target_ld)
        selected<-tmp_lds[(wtdata[tmp_lds,date_time_name]==target_time)]
        if(length(selected)>0){
          target_bmu<-wtdata_bmu[selected[1]]
          dists<-c(dists,sqrt(((som_pts$x[target_bmu]-df$x[i])^2)+((som_pts$y[target_bmu]-df$y[i])^2)))
        }
      }
    }
    if(!is.null(dists)) df$distance[i]<-median(dists,na.rm = T)
  }
  
  if(model_params$hex){
    dfpol<-data.frame(id=numeric(),x=numeric(),y=numeric(),distance=numeric())
    for(i in 1:nrow(df)){
      dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]-0.5,y=df$y[i]-0.25,distance=df$distance[i]))
      dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]-0.5,y=df$y[i]+0.25,distance=df$distance[i]))
      dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i],y=df$y[i]+0.5,distance=df$distance[i]))
      dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]+0.5,y=df$y[i]+0.25,distance=df$distance[i]))
      dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]+0.5,y=df$y[i]-0.25,distance=df$distance[i]))
      dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i],y=df$y[i]-0.5,distance=df$distance[i]))
    }
    p<-ggplot(dfpol,aes(x=x,y=y,fill=distance))+geom_polygon(aes(group=id), colour = "black")
  }else{
    p<-ggplot(df,aes(x=x,y=y,fill=distance))+geom_raster()  
  }
  
  p<-p+theme(plot.margin=unit(c(0,0,0,0),"mm"))+scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))
  if(title) p<-p+ggtitle(paste0(toupper(model_params$type)," time distribution"))
  if(!legend) p<-p+ theme(legend.position="none")
  if(big_fonts) p<-p+ theme(text = element_text(size=35))
  if(!axis_tittle) p<-p+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  p<-p+theme_bw()
  
  return(list(error=F,data=list(plot=p),msg='ok'))
}