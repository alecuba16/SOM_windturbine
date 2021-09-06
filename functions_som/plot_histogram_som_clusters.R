PCbiplot <- function(PC, x="PC1", y="PC2",alarms=NULL,ots=NULL,only_vars=F,alpha_good=0.04,alpha_bad=0.5) {
  status_tmp<-rep('healthy',length(PC$x[,x]))
  if(!is.null(alarms)){
    status_tmp[alarms==1]<-'alarm' #1 alarm
    if(!is.null(ots)){
      status_tmp[(alarms==1)&(ots==1)]<-'alarm&ot' #2 alarm&ot
    }
  }
  if(!is.null(ots)){
    status_tmp[ots==1&(status_tmp=='healthy')]<-'ot' #2 ot
  }
  
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x,status=status_tmp)
  
  colours = c('alarm' = "orange", 'healthy' = "green4", 'ot'="red",'alarm&ot' = "violet")
  plot <- ggplot(data, aes_string(x=x, y=y))
  if(!only_vars) plot<-plot+geom_point(data = subset(data, status == 'healthy'),alpha=alpha_good, size=3, aes(color=status))
  if(!only_vars) plot<-plot+geom_point(data = subset(data, status == 'alarm'),alpha=alpha_bad, size=3, aes(color=status))
  if(!only_vars) plot<-plot+geom_point(data = subset(data, status == 'ot'),alpha=alpha_bad, size=3, aes(color=status))
  if(!only_vars) plot<-plot+geom_point(data = subset(data, status == 'alarm&ot'),alpha=alpha_bad, size=3, aes(color=status))
  plot<-plot+scale_colour_manual(values=colours)
  #plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
  #plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y],na.rm=T) - min(data[,y],na.rm=T)/(max(datapc[,y],na.rm=T)-min(datapc[,y],na.rm=T))),
    (max(data[,x],na.rm=T) - min(data[,x],na.rm=T)/(max(datapc[,x],na.rm=T)-min(datapc[,x],na.rm=T)))
    ,na.rm=T)
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  return(plot)
}

plot_histogram_som_clusters<-function(wtdata=NULL,som_pts=NULL,som_cluster=NULL,wtdata_bmu=NULL,som_selected_rows=NULL,som_selected_columns=NULL,date_time_name='date_time',grid_format=T,density_plot=F,separate_biplot=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('ggplot2','ggpubr')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata))return(list(error=T,data=NULL,msg="wtdata is null"))
  if(is.null(som_cluster))return(list(error=T,data=NULL,msg="som_cluster is null"))
  if(is.null(wtdata_bmu))return(list(error=T,data=NULL,msg="wtdata_bmu is null"))
  if(is.null(som_selected_rows))return(list(error=T,data=NULL,msg="som_selected_rows is null"))
  if(is.null(som_selected_columns))return(list(error=T,data=NULL,msg="som_selected_columns is null"))
  
  ots<-NULL
  alarms<-NULL
  if('ot' %in% names(wtdata)) ots<-wtdata$ot[som_selected_rows]
  if('alarm' %in% names(wtdata)) alarms<-wtdata$alarm[som_selected_rows]
  
  som_selected_columns_orig<-som_selected_columns
  if(!('y' %in% som_selected_columns)) som_selected_columns<-c('y',som_selected_columns)
  if(!('x' %in% som_selected_columns)) som_selected_columns<-c('x',som_selected_columns)
  if(!(date_time_name %in% som_selected_columns)) som_selected_columns<-c(date_time_name,som_selected_columns)
  cols<-length(som_selected_columns)
  
  wtdata<-wtdata[som_selected_rows,colnames(wtdata) %in% som_selected_columns]
  wtdata$x<-som_pts[wtdata_bmu,1]
  wtdata$y<-som_pts[wtdata_bmu,2]
  rm(som_pts)
  invisible(gc(verbose = F))
  
  mins<-NULL
  maxs<-NULL
  range<-lapply(1:length(som_selected_columns),function(c){c(min(wtdata[,som_selected_columns[c]],na.rm = T),max(wtdata[,som_selected_columns[c]],na.rm = T))})
  range<-do.call("rbind",range)
  #for(c in 1:length(som_selected_columns)){
  #  mins<-c(mins,min(wtdata[,som_selected_columns[c]],na.rm = T))
  #  maxs<-c(maxs,max(wtdata[,som_selected_columns[c]],na.rm = T))
  #}
  
  p<-list()
  biplots<-list()
  count<-1
  for(cl in unique(som_cluster)){
    selected_rows<-wtdata_bmu %in% which(som_cluster==cl)
    for(c in 1:cols){
      tmpdata<-wtdata[selected_rows,som_selected_columns[c]]
      tmpdata<-sort(tmpdata)
      
      df<-data.frame(x=1:length(tmpdata),y=tmpdata)
      # diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
      bins<-(diff(range(as.numeric(tmpdata))) / (1 * IQR(as.numeric(tmpdata)) / length(tmpdata)^(1/3)))
      
      if(density_plot){
        t<-ggplot(df,aes(x=y))+geom_density(aes(y=..density..),color='black')  
      }else{
        t<-ggplot(df,aes(x=y))+geom_histogram(fill='black',bins=bins)  
      }
      #label
      t <- t+xlab(label = som_selected_columns[c])
      
      t <- if(som_selected_columns[c]==date_time_name){
          t<-t+xlim(as.POSIXct(range[c,1],tz = 'UTC',origin = '1970-01-01'),as.POSIXct(range[c,2],tz = 'UTC',origin = '1970-01-01'))
      }else{
          t<-t+xlim(range[c,1],range[c,2])
      }
      t <- t + theme_bw()
      t <- t + theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.margin = unit(c(1,0,0,0), "lines"))
      p[[length(p)+1]]<-t
      #count<-count+1
      rm(list=c('df','t'))
      invisible(gc(verbose = F))
    }
    #PCA
    #Dont select zero sd
    if(sum(selected_rows,na.rm = T)>1){
      sdv<-apply(wtdata[selected_rows,som_selected_columns_orig],2,function(col) sd(col,na.rm=T))
      som_selected_columns_orig<-som_selected_columns_orig[sdv!=0]
      pca<-prcomp(wtdata[selected_rows,som_selected_columns_orig],scale = T)
      biplot<-PCbiplot(pca,alarms=alarms[selected_rows],ots=ots[selected_rows])+theme(legend.direction = "horizontal", legend.position = "bottom")
    }else{
      biplot<-ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = "Cannot generate biplot with one entry") + 
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
    }
    if(!separate_biplot){
    p[[length(p)+1]]<-biplot
    #count<-count+1
    rm(list=c('pca','biplot'))
    }else{
      biplots[[length(biplots)+1]]<-biplot
    }
    invisible(gc(verbose = F))
  }
  
  clusters_info<-data.frame(cluster=numeric(),size=numeric())
  for(sc in unique(som_cluster)){
    size<-sum(wtdata_bmu %in% which(som_cluster==sc))
    clusters_info<-rbind(clusters_info,data.frame(cluster=sc,size=size,per=round(size*100/length(wtdata_bmu),digits = 1)))
  }
  
  if(grid_format){
    if(!separate_biplot) cols<-cols+1
    #p<-marrangeGrob(p, nrow=length(unique(som_cluster)), ncol=cols)
    m<-matrix(,ncol=cols,nrow=length(unique(som_cluster)))
    m[,1]<-paste0(clusters_info$cluster,'(',clusters_info$per,'%)')
    tm<-t(m)
    p<-ggarrange(plotlist = p, ncol =cols, nrow = length(unique(som_cluster)),labels = tm,hjust=-0.1,vjust = 1)
    #n <- length(p)
    #nCol <- length(som_selected_columns)+1
    #names<-matrix(,ncol=nCol,nrow=length(unique(clusters_info$cluster)))
    #names[,1]<-paste0(clusters_info$cluster,'(',clusters_info$per,'%)')
    #library(gridExtra)
    #p<-do.call("grid.arrange", c(p, ncol=nCol))
    #library(gridExtra)
    #p<-grid.arrange(grobs = p, ncol = nCol)
    #p<-cowplot::plot_grid(plotlist = p, labels = t(names), ncol = nCol, align = 'v')
  }
  return(list(error=F,data=list(plot=p,biplot=biplots),msg='ok'))
}
