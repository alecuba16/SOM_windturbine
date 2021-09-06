plot_distance_bmu<-function(wtdata=NULL,stat=NULL,wtdata_bmu=NULL,som_codes=NULL,som_selected_columns=NULL,som_selected_rows=NULL,plotly=F,legend=T,big_fonts=F,axis_tittle=T,title=F,separate_turbines=F,grid_format=T,moving_avg_days=NULL,seconds_to_aggregate=86400){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('ggplot2','plotly','htmltools','reshape2','ggpubr','TTR')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata)) return(list(error=T,data=NULL,msg="wtdata is null"))
  if(is.null(stat)) return(list(error=T,data=NULL,msg="stat is null"))
  if(is.null(wtdata_bmu)) return(list(error=T,data=NULL,msg="wtdata_bmu is null"))
  if(is.null(som_codes)) return(list(error=T,data=NULL,msg="som_codes is null"))
  
  if(plotly){
    plots <- htmltools::tagList()
  }else{
    plots <-list()
  }
  position<-1
  
  if(!is.null(moving_avg_days)) window<-((moving_avg_days*24*60*60)/seconds_to_aggregate)
  #remove rows where ld_id is NA
  lds_ids<-unique(wtdata$ld_id[som_selected_rows])
  data_ld_id<-wtdata$ld_id[som_selected_rows]
  data_date_time<-wtdata$date_time[som_selected_rows]
  data<-wtdata[som_selected_rows,colnames(wtdata) %in% som_selected_columns]
  data<-lapply(colnames(data),function(c) (data[,c]-stat$mean[stat$name==c])/stat$sdv[stat$name==c])
  data<-do.call("cbind",data)
  
  df<-NULL
  
  for(ld in lds_ids){
    selected_row<-(data_ld_id==ld)
    selected_data<-data[selected_row,]
    selected_bmu<-wtdata_bmu[selected_row]
    distance<-selected_data-som_codes[selected_bmu,]
    distance<-apply(distance,1,function(r) sqrt(sum(r^2,na.rm = T)))
    if(!is.null(moving_avg_days)) distance<-TTR::SMA(distance,min(window,length(distance),na.rm = T),na.rm=T)
    #if(!is.null(moving_avg_days)) distance<-TTR::SMA(distance,window,na.rm=T)
    df<-rbind(df,data.frame(ld_id=ld,date_time=data_date_time[selected_row],distance=distance))
  }
  min_d<-min(df$distance,na.rm = T)
  max_d<-max(df$distance,na.rm = T)
  
  for(ld in lds_ids){
    p<-ggplot(df[df$ld_id==ld,], aes(date_time,distance))+
      geom_point()+
      xlab('date_time')+ylab('distance')+
      theme_bw()+
      theme(plot.margin=unit(c(0,0,0,0),"mm"))+
      scale_y_continuous(limits = c(min_d, max_d))
    if(title) p<-p+ggtitle(ld)
    if(!legend) p<-p+ theme(legend.position="none")
    if(big_fonts) p<-p+ theme(text = element_text(size=35))
    if(!axis_tittle) p<-p+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    
    if(!plotly){
      plots[[position]]<-p
    }else{
      plots[[position]]<-ggplotly(p,width = 1280,height = 1280)
    }
    position<-position+1
  }
  
  
  if(!plotly&&grid_format){
    ncols<-floor(sqrt(length(plots)))
    nrows<-ceiling(length(plots)/ncols)
    m<-matrix(lds_ids,ncol=ncols,nrow=nrows,byrow = F)
    plots<-ggarrange(plotlist=plots,ncol=ncols,nrow = nrows,vjust = 1.8,labels = m,font.label =  list(size = 35, face = "bold", color ="black") )
  }
  return(list(error=F,data=list(lds_ids=lds_ids,plot=plots),msg='ok'))
}