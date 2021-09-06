plot_bmu_turbines<-function(num_registers_threshold=1,model_params=NULL,wtdata=NULL,wtdata_bmu=NULL,som_pts=NULL,clusters_xy=NULL,plotly=F,legend=T,big_fonts=F,axis_tittle=T,grid_format=T){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('ggplot2','plotly','htmltools','reshape2')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata))return(list(error=T,data=NULL,msg="wtdata is null"))
  if(is.null(wtdata_bmu))return(list(error=T,data=NULL,msg="wtdata_bmu is null"))
  if(is.null(clusters_xy))return(list(error=T,data=NULL,msg="clusters_xy is null"))
  if(is.null(som_pts))return(list(error=T,data=NULL,msg="som_pts is null"))
  
  ots<-NULL
  alarms<-NULL
  if('ot' %in% names(wtdata)) ots<-wtdata$ot
  if('alarm' %in% names(wtdata)) alarms<-wtdata$alarm
  
  lds_ids<-sort(unique(wtdata$ld_id))
  
  if(plotly&&!grid_format){
    plots <- htmltools::tagList()
  }else{
    plots <-list()
  }
  
  #count register per bmu
  bmu_count<-lapply(unique(wtdata_bmu),function(bmu) data.frame(neuron_id=bmu,total=sum(wtdata_bmu==bmu)))
  bmu_count<-do.call("rbind",bmu_count)
  
  position<-1
  for(ld in lds_ids){
    selected_rows <- (wtdata$ld_id==ld)
    active_neuron_id<-wtdata_bmu[selected_rows]
    active_neuron_id_filtered<-unlist(lapply(unique(active_neuron_id),function(nid) return(sum(active_neuron_id==nid,na.rm = T)>num_registers_threshold)))
    if(!is.null(active_neuron_id_filtered)&&sum(active_neuron_id_filtered)==0){
      num_registers_threshold<-median(unlist(lapply(unique(active_neuron_id),function(nid) return(sum(active_neuron_id==nid,na.rm = T)))))
    }
    active_neuron_id_filtered<-unlist(lapply(unique(active_neuron_id),function(nid) return(sum(active_neuron_id==nid,na.rm = T)>num_registers_threshold)))
    if(!is.null(active_neuron_id_filtered)&&sum(active_neuron_id_filtered)>0){
      clusters_xy_tmp<-unlist(clusters_xy$clusters_machines[clusters_xy$ld_id==ld])
      clusters_xy_tmp<-clusters_xy_tmp[active_neuron_id_filtered]
      active_neuron_xy<-as.data.frame(som_pts[active_neuron_id[active_neuron_id_filtered],])
      active_neuron_xy$neuron_id<-as.numeric(active_neuron_id[active_neuron_id_filtered])
      active_neuron_xy$cluster<-as.factor(clusters_xy_tmp)
      unique_active_neurons<-unique(active_neuron_xy$neuron_id)
      alpha<-sapply(unique_active_neurons,function(id){
        (sum(active_neuron_xy$neuron_id==id)/bmu_count$total[bmu_count$neuron_id==id])*length(lds_ids)
      })
      active_neuron_xy$alpha<-1
      for(i in 1:length(unique_active_neurons)){
        active_neuron_xy$alpha[active_neuron_xy$neuron_id==unique_active_neurons[i]]<-alpha[i]
      }
      
      #Alarms/ots
      bmus_with_alarms<-NULL
      tmp_alarm_ot<-rep(NA,length(selected_rows))
      if(!is.null(alarms)) tmp_alarm_ot <- alarms
      if(!is.null(ots)) tmp_alarm_ot <- (ots|tmp_alarm_ot)
      if(any(!is.na(tmp_alarm_ot))){
        tmp_alarm_ot<-which(tmp_alarm_ot==T)
        tmp_alarm_ot<-tmp_alarm_ot[!is.na(tmp_alarm_ot)]
        tmp_alarm_ot<-which(selected_rows==T)[which(selected_rows==T) %in% tmp_alarm_ot]
        bmus_with_alarms <- unique(wtdata_bmu[tmp_alarm_ot])
        bmus_with_alarms <- som_pts[bmus_with_alarms,]
      }
      #end alarms
      
      
      #For dynamic clustering
      nclusters<-as.numeric((levels(active_neuron_xy$cluster)))
      #Publication
      centroids<-as.data.frame(cbind(t(sapply(nclusters,function (c) round(colMeans(active_neuron_xy[active_neuron_xy$cluster==c,c('x','y')])))),nclusters))
      colnames(centroids)[3]<-'cluster'
      
      #End publication
      active_neuron_xy$alarm<-0
      if(!is.null(bmus_with_alarms)){
        for(j in 1:nrow(bmus_with_alarms)) active_neuron_xy$alarm[(active_neuron_xy$x==bmus_with_alarms$x[j])&(active_neuron_xy$y==bmus_with_alarms$y[j])]<-1
      } 
      
      if(model_params$hex){
        dfpol<-data.frame(id=numeric(),x=numeric(),y=numeric(),cluster=numeric())
        ids<-as.numeric(rownames(active_neuron_xy))
        dfpol<-rbind(dfpol,data.frame(id=ids,x=active_neuron_xy$x-0.5,y=active_neuron_xy$y-0.25,cluster=active_neuron_xy$cluster,alpha=active_neuron_xy$alpha,alarm=active_neuron_xy$alarm))
        dfpol<-rbind(dfpol,data.frame(id=ids,x=active_neuron_xy$x-0.5,y=active_neuron_xy$y+0.25,cluster=active_neuron_xy$cluster,alpha=active_neuron_xy$alpha,alarm=active_neuron_xy$alarm))
        dfpol<-rbind(dfpol,data.frame(id=ids,x=active_neuron_xy$x,y=active_neuron_xy$y+0.5,cluster=active_neuron_xy$cluster,alpha=active_neuron_xy$alpha,alarm=active_neuron_xy$alarm))
        dfpol<-rbind(dfpol,data.frame(id=ids,x=active_neuron_xy$x+0.5,y=active_neuron_xy$y+0.25,cluster=active_neuron_xy$cluster,alpha=active_neuron_xy$alpha,alarm=active_neuron_xy$alarm))
        dfpol<-rbind(dfpol,data.frame(id=ids,x=active_neuron_xy$x+0.5,y=active_neuron_xy$y-0.25,cluster=active_neuron_xy$cluster,alpha=active_neuron_xy$alpha,alarm=active_neuron_xy$alarm))
        dfpol<-rbind(dfpol,data.frame(id=ids,x=active_neuron_xy$x,y=active_neuron_xy$y-0.5,cluster=active_neuron_xy$cluster,alpha=active_neuron_xy$alpha,alarm=active_neuron_xy$alarm))
        
        # for(r in 1:nrow(active_neuron_xy)){
        #   dfpol<-rbind(dfpol,data.frame(id=r,x=active_neuron_xy$x[r]-0.5,y=active_neuron_xy$y[r]-0.25,cluster=active_neuron_xy$cluster[r]))
        #   dfpol<-rbind(dfpol,data.frame(id=r,x=active_neuron_xy$x[r]-0.5,y=active_neuron_xy$y[r]+0.25,cluster=active_neuron_xy$cluster[r]))
        #   dfpol<-rbind(dfpol,data.frame(id=r,x=active_neuron_xy$x[r],y=active_neuron_xy$y[r]+0.5,cluster=active_neuron_xy$cluster[r]))
        #   dfpol<-rbind(dfpol,data.frame(id=r,x=active_neuron_xy$x[r]+0.5,y=active_neuron_xy$y[r]+0.25,cluster=active_neuron_xy$cluster[r]))
        #   dfpol<-rbind(dfpol,data.frame(id=r,x=active_neuron_xy$x[r]+0.5,y=active_neuron_xy$y[r]-0.25,cluster=active_neuron_xy$cluster[r]))
        #   dfpol<-rbind(dfpol,data.frame(id=r,x=active_neuron_xy$x[r],y=active_neuron_xy$y[r]-0.5,cluster=active_neuron_xy$cluster[r]))
        # }
        #p<-ggplot(dfpol,aes(x=x,y=y,fill=cluster,alpha=alpha))+geom_polygon(aes(group=id), colour = "black")
        p<-ggplot()+geom_polygon(data=dfpol,aes(x=x,y=y,fill=cluster,alpha=alpha,group=id), colour = "black")+geom_point(data = subset(dfpol,alarm!=0),mapping = aes(x=x,y=y,group=id),colour = "#FF0000",size = 0.5,show.legend = F)
      }else{
        p<-ggplot(active_neuron_xy, aes(x=x,y=y,fill=cluster))+geom_raster()+geom_point(data = subset(active_neuron_xy,alarm!=0),mapping = aes(x=x,y=y),colour = "#FF0000",size = 0.5,show.legend = F)
      }
      
      p<- p + annotate("rect", xmin = centroids$x-1.5, xmax = centroids$x+1.5, ymin = centroids$y-1.5, ymax = centroids$y+1.5,alpha=0.75,fill="white")+
        annotate("text",x=centroids$x,y=centroids$y,label=centroids$cluster,size=25,color='#000000')
      
      #if(!is.null(bmus_with_alarms)) p <- p+geom_point(aes(x=bmus_with_alarms$x,y=bmus_with_alarms$y), colour = "#FF0000",size = 0.5)
      
      p<-p+theme_bw()+
        theme(plot.margin=unit(c(0,0,0,0),"mm"))+
        scale_y_continuous(expand = c(0,0))
      scale_x_continuous(expand = c(0,0))
      
      if(!legend) p<-p+ theme(legend.position="none")
      if(big_fonts) p<-p+ theme(text = element_text(size=35))
      if(!axis_tittle) p<-p+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
      
      plots[[position]]<- if(plotly&&!grid_format) ggplotly(p,width = 1280,height = 1280) else p
    }else{
      plots[[position]]<-NULL
    }
    position<-position+1
  }
  if(grid_format){
    ncols<-floor(sqrt(length(plots)))
    nrows<-ceiling(length(plots)/ncols)
    m<-matrix(lds_ids,ncol=ncols,nrow=nrows,byrow = F)
    plots<-ggarrange(plotlist=plots,ncol=ncols,nrow = nrows,vjust = 1.8,labels = m,font.label =  list(size = 35, face = "bold", color ="black") )
  }
  return(list(error=F,data=list(lds_ids=lds_ids,plots=plots),msg='ok'))
}