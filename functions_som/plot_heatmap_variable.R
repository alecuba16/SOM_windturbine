plot_heatmap_variable<-function(wtdata=NULL,wtdata_bmu=NULL,som_pts=NULL,som_selected_rows=NULL,som_selected_columns=NULL,model_params=NULL,plotly=F,legend=T,big_fonts=F,axis_tittle=T,title=F,separate_turbines=F,grid_format=T,plot_sdv=T){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('ggplot2','plotly','htmltools','reshape2','ggpubr')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata))return(list(error=T,data=NULL,msg="wtdata is null"))
  if(is.null(som_pts))return(list(error=T,data=NULL,msg="som_pts is null"))
  
  ots<-NULL
  alarms<-NULL
  if('ot' %in% names(wtdata)) ots<-wtdata$ot[som_selected_rows]
  if('alarm' %in% names(wtdata)) alarms<-wtdata$alarm[som_selected_rows]
  
  plots_mean <- if(plotly) htmltools::tagList() else list()
  plots_sdv<-NULL
  if(plot_sdv) plots_sdv <- if(plotly) htmltools::tagList() else list()
  
  position<-1
  lds_ids<-ifelse(separate_turbines,unique(wtdata$ld_id),0)
  
  #fun<-if(use_mean) mean else median
  
  selected_row<-som_selected_rows
  som_pts$row<-as.numeric(rownames(som_pts))
  for(col in som_selected_columns){
    current_column<-wtdata[som_selected_rows,colnames(wtdata)==col]
    current_column_range<-range(current_column,na.rm = T)
    for(ld in lds_ids){
      selected_row<-if(separate_turbines) which(wtdata$ld_id==ld)&som_selected_rows else which(som_selected_rows==T)
      var_unscaled <- wtdata[selected_row,colnames(wtdata)==col]
      current_wtdata_bmu<-wtdata_bmu[which(selected_row %in% which(som_selected_rows==T))]
      #Alarms/ots
      bmus_with_alarms<-NULL
      tmp_alarm_ot<-rep(NA,length(selected_row))
      if(!is.null(alarms)) tmp_alarm_ot <- (alarms[selected_row]|tmp_alarm_ot)
      if(!is.null(ots)) tmp_alarm_ot <- (ots[selected_row]|tmp_alarm_ot)
      if(any(!is.na(tmp_alarm_ot))){
        tmp_alarm_ot<-!is.na(tmp_alarm_ot)
        bmus_with_alarms<-unique(current_wtdata_bmu[tmp_alarm_ot])
        bmus_with_alarms<- som_pts[bmus_with_alarms,]
      }
      #end alarms
      current_som_pts<-unique(som_pts[current_wtdata_bmu,])
      if(model_params$hex){
        dfpol<-data.frame(id=numeric(),x=numeric(),y=numeric(),mean=numeric(),sdv=numeric())
        for(r in 1:nrow(current_som_pts)){
          mean<-mean(var_unscaled[current_wtdata_bmu %in% current_som_pts$row[r]],na.rm = T)
          sdv<-sd(var_unscaled[current_wtdata_bmu %in% current_som_pts$row[r]],na.rm = T)
          #To percentage
          sdv<-sdv*100/(current_column_range[2]-current_column_range[1])
          #End percentage
          dfpol<-rbind(dfpol,data.frame(id=r,x=current_som_pts$x[r]-0.5,y=current_som_pts$y[r]-0.25,mean=mean,sdv=sdv))
          dfpol<-rbind(dfpol,data.frame(id=r,x=current_som_pts$x[r]-0.5,y=current_som_pts$y[r]+0.25,mean=mean,sdv=sdv))
          dfpol<-rbind(dfpol,data.frame(id=r,x=current_som_pts$x[r],y=current_som_pts$y[r]+0.5,mean=mean,sdv=sdv))
          dfpol<-rbind(dfpol,data.frame(id=r,x=current_som_pts$x[r]+0.5,y=current_som_pts$y[r]+0.25,mean=mean,sdv=sdv))
          dfpol<-rbind(dfpol,data.frame(id=r,x=current_som_pts$x[r]+0.5,y=current_som_pts$y[r]-0.25,mean=mean,sdv=sdv))
          dfpol<-rbind(dfpol,data.frame(id=r,x=current_som_pts$x[r],y=current_som_pts$y[r]-0.5,mean=mean,sdv=sdv))
        }
        pmean<-ggplot(dfpol,aes(x=x,y=y,fill=mean))+geom_polygon(aes(group=id), colour = "black")
        if(plot_sdv) psdv<-ggplot(dfpol,aes(x=x,y=y,fill=sdv))+geom_polygon(aes(group=id), colour = "black")
      }else{
        dim<-sqrt(nrow(unique(som_pts)))
        #Mean
        m<-matrix(,ncol=dim,nrow = dim)
        #search same
        for(i in 1:(dim*dim)){
          pos_x<-som_pts[i,1]
          pos_y<-som_pts[i,2]
          m[pos_x,pos_y]<-mean(var_unscaled[wtdata_bmu[selected_row]==i],na.rm = T)
        }
        df<-melt(m)
        df<-df[!is.na(df$value),]
        #pmean<-ggplot(dfmean, aes(Var1,Var2))+geom_tile(aes(fill=value))
        pmean<-ggplot(df, aes(Var1,Var2))+geom_tile(aes(fill=value))
        #SDV
        if(plot_sdv){
          for(i in 1:(dim*dim)){
            m[pos_x,pos_y]<-sd(var_unscaled[wtdata_bmu[selected_row]==i],na.rm = T)
          }
          df<-melt(m)
          df<-df[!is.na(df$value),]
          psdv<-ggplot(df, aes(Var1,Var2))+geom_tile(aes(fill=value))
        }
      }
      
      if(!is.null(bmus_with_alarms)) pmean <- pmean+geom_point(data=bmus_with_alarms,aes(x=x,y=y), colour = "#FF0000",size = 0.1)
      if(!is.null(bmus_with_alarms)&&plot_sdv) psdv <- psdv+geom_point(data=bmus_with_alarms,aes(x=x,y=y), colour = "#FF0000",size = 0.1)
      
      pmean<-pmean+xlab('x')+ylab('y')+theme_bw()+ scale_fill_gradient(low = "black", high = "#93FFA0", limits=c(current_column_range[1],current_column_range[2]))+theme(plot.margin=unit(c(0,0,0,0),"mm"),aspect.ratio=1)+labs(aesthetic=c)+coord_fixed() #Force squared
      
      if(plot_sdv) psdv<-psdv+xlab('x')+ylab('y')+theme_bw()+ scale_fill_gradient(low = "black", high = "#93FFA0")+theme(plot.margin=unit(c(0,0,0,0),"mm"),aspect.ratio=1)+labs(aesthetic=c)+coord_fixed() #Force squared
      
      titletxt<-ifelse(separate_turbines,paste0(ld,'_',col),col)
      if(title) pmean<-pmean+ggtitle(titletxt)
      if(!legend) pmean<-pmean+ theme(legend.position="none")
      if(big_fonts) pmean<-pmean+ theme(text = element_text(size=35))
      if(!axis_tittle) pmean<-pmean+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
      
      plots_mean[[position]]<-if(!plotly) pmean else ggplotly(pmean,width = 1280,height = 1280)
      if(plot_sdv) plots_sdv[[position]]<- if(!plotly) psdv else ggplotly(psdv,width = 1280,height = 1280)
      
      position<-position+1
    }
  }
  if(!plotly&&grid_format){
    nrows<-floor(sqrt(length(som_selected_columns)))
    ncols<-ceiling(length(som_selected_columns)/nrows)
    m<-matrix(NA,ncol=ncols,nrow=nrows,byrow = F)
    for(i in 1:length(som_selected_columns)){
      m[((i-1)%/%ncols)+1,((i-1)%%ncols)+1]<-som_selected_columns[i]
    }
    plots_mean<-ggarrange(plotlist=plots_mean,ncol=ncols,nrow = nrows,vjust = 5,hjust = 0,labels = t(m),font.label =  list(size = 25, face = "bold", color ="black") )
    if(plot_sdv) plots_sdv<-ggarrange(plotlist=plots_sdv,ncol=ncols,nrow = nrows,vjust = 5,hjust = 0,labels = t(m),font.label =  list(size = 25, face = "bold", color ="black") )
  }
  return(list(error=F,data=list(lds_ids=lds_ids,plots_mean=plots_mean,plots_sdv=plots_sdv),msg='ok'))
}