get_optimal_som<-function(wtdata=NULL,dim=c(20,100,10),ruleofthethumb=T,rlen=10,alpha=c(0.05,0.01),parallel_mode=T,log_file=NULL,verbose=F,normalize=F,plot=F,plotly=F,plot_title=T,big_fonts=F,axis_tittle=T,legend=T){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('reshape2','ggplot2','plotly','functions_som/create_som_model.R')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata))return(list(error=T,data=NULL,msg="wtdata is null"))
  if(is.null(dim))return(list(error=T,data=NULL,msg="dim is null"))
  if(is.null(rlen))return(list(error=T,data=NULL,msg="rlen is null"))
  if(is.null(alpha))return(list(error=T,data=NULL,msg="alpha is null"))
  
  if(ruleofthethumb) dim<-round(sqrt(5*sqrt(nrow(wtdata))),0)
  rs<-create_som_model(dim=dim,wtdata=wtdata,rlen=rlen,alpha=alpha,parallel_mode=parallel_mode,log_file=log_file,verbose=verbose,normalize=normalize)
  if(rs$error) stop(rs$msg)
  som_result<-rs$data
  #Evaluate
  q<-NULL
  #Evaluate
  q<-NULL
  for(m in 1:length(som_result$model)){
    som_model<-som_result$model[[m]]
    rs<-som_result$get_som_vars(som_model)
    if(rs$error) stop(rs$msg)
    dim<-max(rs$data$som_pts,na.rm = T)
    rs<-som_result$get_metrics(som_codes = rs$data$som_codes,som_pts=rs$data$som_pts,som_distances = rs$data$som_distances,data = rs$data$data[[1]],verbose=T)
    if(rs$error) stop(rs$msg)
    te<-rs$data$te
    qe<-rs$data$qe
    q<-rbind(q,data.frame(dim=dim,qe=qe,te=te))
  }
  max_dim<-max(q$dim)
  min_dim<-min(q$dim)
  
  q$qe_norm<-(q$qe-min(q$qe))/(max(q$qe)-min(q$qe))
  q$te_norm<-(q$te-min(q$te))/(max(q$te)-min(q$te))
  #Get best som size intersection between two scores
  if(nrow(q)>2){
    qs<-data.frame(dim=seq(min(q$dim),max(q$dim),0.01))
    qs$qe_norm_s<-predict(loess(q$qe_norm~q$dim),newdata = qs$dim)
    qs$te_norm_s<-predict(loess(q$te_norm~q$dim),newdata = qs$dim)
    #qsm<-melt(qs,id='dim')
    #ggplot(qsm,aes(x=dim,y=value,group=variable,color=variable))+geom_line()
    intersection<-qs$dim[as.logical(abs(diff(qs$qe_norm_s < qs$te_norm_s)))]
    dim<-ceiling(intersection)
  }else{
    dim<-mean(c(max_dim,min_dim))
  }
  
  p<-NULL
  if(plot){
    #PLOT TE vs QE
    qp<-melt(q,id='dim')
    p<-ggplot(qp,aes(x=dim,y=value,group=variable,color=variable))+geom_line(data=function(x) x[x$variable %in% c('qe_norm','te_norm'),])+theme_bw()
    theme(plot.margin=unit(c(0,0,0,0),"mm"))
    if(!legend) p<-p+ theme(legend.position="none")
    if(!big_fonts) p<-p+ theme(text = element_text(size=35))
    if(!axis_tittle) p<-p+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    
    if(plotly) p<-ggplotly(p)
  }
  return(list(error=F,data=list(optimal_dim=dim,metrics=q,som_results=som_result,plot=p),msg='ok'))
}