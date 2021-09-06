plot_umatrix<-function(umatrix=NULL,som_pts=NULL,som_cluster=NULL,wtdata_bmu=NULL,model_params=NULL,plotly_umatrix=F,perps_umatrix=F,plotly_umatrix_clusters=F,perps_umatrix_clusters=F,legend=T,big_fonts=F,axis_tittle=T,title=T,add_points=NULL){
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
  
  if(is.null(umatrix))return(list(error=T,data=NULL,msg="umatrix is null"))
  if(is.null(som_pts))return(list(error=T,data=NULL,msg="som_pts is null"))
  if(is.null(som_cluster)) som_cluster<-rep(0,length(wtdata_bmu))
  df<-data.frame(x=som_pts[wtdata_bmu,1],y=som_pts[wtdata_bmu,2],cluster=as.factor(som_cluster[wtdata_bmu]))
  df<-unique(df)
  df<-na.omit(df)
  df$distance<-NA
  df<-df[order(df$x,df$y),]
  for(i in 1:nrow(df)){
    for(j in 1:nrow(umatrix)){
      selected<-which((df$x[i]==umatrix$x[j])&&(df$y[i]==umatrix$y[j]))
      if(length(selected)>0) df$distance[i]<-umatrix$distance[j]
    }
  }
  dic<-unique(c(som_pts$x),c(som_pts$y))
  dic<-dic[order(dic)]
  dic<-unique(dic)
  dim<-length(dic)
  
  dic_x<-unique(som_pts$x)
  dic_x<-dic_x[order(dic_x)]
  dim_x<-length(dic_x)
  dic_y<-unique(som_pts$y)
  dic_y<-dic_y[order(dic_y)]
  dim_y<-length(dic_y)
  
  #Umatrix
  if(plotly_umatrix){
    p<-plot_ly(data = df,x=~x,y=~y,z = ~distance,intensity=~distance,text =~paste0('x:',x,'\ny:',y,'\nz:',distance)) %>% add_trace(type='mesh3d')
  }else if(perps_umatrix){
    theta<-5 #52x52
    phi<-40
    df2<-akima::interp(df$x,df$y,df$distance,nx=dim,ny=dim,linear = T)
    plot3D::persp3D(x=df2$x,y=df2$y,z = df2$z,add = FALSE,bty ='b',colkey =FALSE,theta =theta, phi = phi)
    #p<-rgl::persp3d(x = df2$x, y = df2$y, df2$z)
    p <- recordPlot()
    plot.new() ## clean up device
  }else{
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
    if(title) p<-p+ggtitle("U-Matrix")
    if(!legend) p<-p+ theme(legend.position="none")
    if(big_fonts) p<-p+ theme(text = element_text(size=35))
    if(!axis_tittle) p<-p+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  }
  
  #Umatrix clustered
  if(!is.null(som_cluster)&&!is.null(wtdata_bmu)){
    if(plotly_umatrix_clusters){
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      #m<-matrix(,ncol=dim_y,nrow=dim_x,byrow = F)
      #col<-matrix(,ncol=dim_y,nrow=dim_x,byrow = F)
      colorscale = sample(col_vector, max(as.numeric(df$cluster),na.rm = T))
      
      #plot_ly(df) %>% add_trace(mode='markers',type='scatter3d', x=~x,y=~y,z=~distance) %>% 
      #  add_trace(mode='markers',type='mesh3d', x=~x,y=~y,z=~distance,color=~as.numeric(cluster),opacity=0.2)
      pc<- plot_ly(df) %>% add_trace(mode='markers',type='scatter3d', x=~x,y=~y,z=~distance,name=paste0('neurons'))
      for(c in unique(df$cluster)){
        current_cluster<-df[df$cluster==c,]
        pc<-pc %>% add_trace(data = current_cluster,name=paste0('cluster ',c),type='mesh3d', x=~x,y=~y,z=~distance,opacity=0.2)
      }
      pc <- plotly_build(pc)
      for(c in unique(df$cluster)){
        pc$data[[c]]$color<-toRGB(colorscale[c]) 
      }
      
    }else if(perps_umatrix_clusters){
      #Umatrix 3D clusterized
      df3<-akima::interp(df$x,df$y,df$cluster,nx=dim,ny=dim,linear = T)
      df3$z<-round(df2$z,0)
      pc <- plot3D::persp3D(x=df2$x,y=df2$y,z = df2$z,colvar=df3$z,add = F ,bty ='b',colkey =F,theta = theta, phi = phi)
      pc <- recordPlot()
      plot.new() ## clean up device
      
    }else{
      if(model_params$hex){
        dfpol<-data.frame(id=numeric(),x=numeric(),y=numeric(),cluster=numeric())
        for(i in 1:nrow(df)){
          dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]-0.5,y=df$y[i]-0.25,cluster=df$cluster[i]))
          dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]-0.5,y=df$y[i]+0.25,cluster=df$cluster[i]))
          dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i],y=df$y[i]+0.5,cluster=df$cluster[i]))
          dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]+0.5,y=df$y[i]+0.25,cluster=df$cluster[i]))
          dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i]+0.5,y=df$y[i]-0.25,cluster=df$cluster[i]))
          dfpol<-rbind(dfpol,data.frame(id=i,x=df$x[i],y=df$y[i]-0.5,cluster=df$cluster[i]))
        }
        if(!is.null(add_points)){
          pc<-ggplot(dfpol,aes(x=x,y=y))+geom_polygon(aes(group=id), colour = "black")+geom_point(data=add_points,aes(x=x,y=y,color=ld_id))
        }else{
          pc<-ggplot(dfpol,aes(x=x,y=y,fill=cluster))+geom_polygon(aes(group=id), colour = "black")
        }
        if(!is.null(add_points)) pc<-pc+geom_point(data=add_points,aes(x=x,y=y,color=ld_id))
      }else{
        if(!is.null(add_points)){
          pc<-ggplot(df,aes(x=x,y=y))+geom_raster(aes(fill=cluster))+geom_point(data=add_points,aes(x=x,y=y,color=ld_id))
        }else{
          pc<-ggplot(df,aes(x=x,y=y,fill=cluster))+geom_raster()  
        }
      }
      
      pc<-pc+theme_bw()+theme(plot.margin=unit(c(0,0,0,0),"mm"))+scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))
      if(title) pc<-pc+ggtitle(paste0(toupper(model_params$type)," codes clustering"))
      if(!legend) pc<-pc+ theme(legend.position="none")
      if(big_fonts) pc<-pc+ theme(text = element_text(size=35))
      if(!axis_tittle) pc<-pc+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    }
  }
  return(list(error=F,data=list(umatrix=p,umatrix_with_clusters=pc),msg='ok'))
}