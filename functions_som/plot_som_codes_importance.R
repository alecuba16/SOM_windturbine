#Expected format:
#df <- data.frame(
#variable = c("Male", "Female", "Child","Male", "Female", "Child","Male", "Female", "Child"),
#value = c(25,25,50,50,25,25,20,20,60),
#neuron_x = c(1,1,1,2,2,2,2,2,2),
#neuron_y = c(1,1,1,1,1,1,2,2,2) 
#)

plot_som_codes_importance<-function(som_codes=NULL,som_pts=NULL,som_selected_columns=NULL,plotly=F,legend=T,title=T){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('ggplot2','plotly')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(som_codes))return(list(error=T,data=NULL,msg="som_codes is null"))
  if(is.null(som_pts))return(list(error=T,data=NULL,msg="som_pts is null"))
  if(is.null(som_selected_columns))return(list(error=T,data=NULL,msg="som_selected_columns is null"))
  
  
  #normalize columns
  norm_som_codes<-apply(som_codes,2,function(c) (c-min(c,na.rm = T))/(max(c,na.rm = T)-min(c,na.rm = T)))
  df<-data.frame(neuron_x=as.numeric(),neuron_y=as.numeric(),variable=as.character(),value=as.numeric())
  for(r in 1:nrow(som_codes)){
    neuron_x<-som_pts[r,1]
    neuron_y<-som_pts[r,2]
    variable<-som_selected_columns
    max_val_row<-sum(norm_som_codes[r,])
    value<-norm_som_codes[r,]*100/max_val_row
    df<-rbind(df,data.frame(neuron_x=neuron_x,neuron_y=neuron_y,variable=variable,value=value))
  }
  
  df<-df[order(df$neuron_x,df$neuron_y),]
  
  p<- ggplot(df,aes(x="", y=value, fill=variable)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y",direction = -1) +
    facet_grid(facets = neuron_y~neuron_x ,shrink = T,as.table = F,switch='both') +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.spacing = unit(-0.5, "lines"),
          plot.margin=unit(c(0,0,0,0),"mm"),
          plot.background=element_blank(),
          legend.position = 'bottom') +
          scale_color_brewer(palette="Set1")+
          scale_fill_brewer(palette="Set1")+
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_y_discrete(expand = c(0,0)) + 
    scale_x_discrete(expand = c(0,0))
    if(title) p<-p+ggtitle('Variables importance for each neuron')
    if(!legend) p<-p+ theme(legend.position="none")
  return(list(error=F,data=list(plot=p),msg='ok'))
}