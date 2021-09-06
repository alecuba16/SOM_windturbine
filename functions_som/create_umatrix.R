create_umatrix<-function(som_pts=NULL,som_codes=NULL,hex=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  #sources<-c('ggplot2','plotly')
  #dep<-dependencyLoader(sources)
  #if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(som_pts)) return(list(error=T,data=NULL,msg="som_pts is null"))
  if(is.null(som_codes)) return(list(error=T,data=NULL,msg="som_codes is null"))
  #if(hex) return(list(error=T,data=NULL,msg="hex maps not implemented"))
  
  max_x<-max(som_pts[,1])
  max_y<-max(som_pts[,2])
  umatrix<-data.frame(x=as.numeric(),y=as.numeric(),distance=as.numeric())
  for(r in 1:nrow(som_pts)){
    d<-NULL
    x<-som_pts[r,1]
    y<-som_pts[r,2]
    #Left neuron
    if(x>1){
      left_id<-which(som_pts[,1]==(x-1)&som_pts[,2]==y)
      if(length(left_id)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[left_id,])^2,na.rm = T)))
    }
    #Right neuron
    if(x<max_x){
      right_id<-which(som_pts[,1]==(x+1)&som_pts[,2]==y)
      if(length(right_id)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[right_id,])^2,na.rm = T)))
    }
    #Bottom neuron
    if(y>1){
      bottom_id<-which(som_pts[,1]==x&som_pts[,2]==(y-1))
      if(length(bottom_id)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[bottom_id,])^2,na.rm = T)))
    }
    #Top neuron
    if(y<max_y){
      top_id<-which(som_pts[,1]==x&som_pts[,2]==(y+1))
      if(length(top_id)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[top_id,])^2,na.rm = T)))
    }
    
    if(hex){ #hex leftbottom
      left_bott<-which(som_pts[,1]==(x-0.25)&som_pts[,2]==(y-0.25))
      if(length(left_bott)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[left_bott,])^2,na.rm = T)))
    }
    if(hex){ #hex lefttop
      left_top<-which(som_pts[,1]==(x-0.25)&som_pts[,2]==(y+0.25))
      if(length(left_top)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[left_top,])^2,na.rm = T)))
    }
    if(hex){ #hex rightbottom
      right_bott<-which(som_pts[,1]==(x+0.25)&som_pts[,2]==(y-0.25))
      if(length(right_bott)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[right_bott,])^2,na.rm = T)))
    }
    if(hex){ #hex righttop
      right_top<-which(som_pts[,1]==(x+0.25)&som_pts[,2]==(y+0.25))
      if(length(right_top)>0) d<-c(d,sqrt(sum((som_codes[r,]-som_codes[right_top,])^2,na.rm = T)))
    }
    umatrix<-rbind(umatrix,data.frame(x=x,y=y,distance=sum(d)))
  }
  
  return(list(error=F,data=list(umatrix=umatrix),msg='ok'))
}