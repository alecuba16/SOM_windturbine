wtdata_bmu2<-lapply(unlist(clusters_list[,'ld_id']),function(ld){
  m<-matrix(c(som_pts[wtdata_bmu[data$ld_id==ld],1],som_pts[wtdata_bmu[data$ld_id==ld],2]),byrow = T,nrow =  length(wtdata_bmu[data$ld_id==ld]))
  colnames(m)<-c('x','y')
  return(m)
})

wtdata_filtered<-wtdata[som_selected_rows,c('ld_id','date_time',"Pot_avg","TempAceiteMultip_avg","TempAmb_avg","TempRodamMultip_avg","VelRotor_avg","VelViento_avg")]


##Prepare wtdata scaled by the reference.
final<-NULL
ref<-119
min_max_ref<-t(apply(wtdata[wtdata_filtered$ld_id==ref,!(colnames(wtdata_filtered) %in% c('date_time','ld_id'))],2,function(c) c(min(c,na.rm = T),max(c,na.rm = T))))
colnames(min_max_ref)<-c('min','max')
min_max_ref<-as.data.frame(min_max_ref)
min_max_ref_withoutoffset<-t(apply(min_max_ref,1,function(r) c(0,(r[2]-r[1]))))
lds_ids<-unique(wtdata_filtered$ld_id)
for(i in 1:length(wtdata_bmu)){
  if(lds_ids[i]!=ref){
    current_data<-wtdata_filtered[wtdata_filtered$ld_id==lds_ids[i],!(colnames(wtdata_filtered) %in% c('date_time','ld_id'))]
    current_data_scaled<-current_data
    min_max_new<-t(apply(current_data,2,function(c) c(min(c,na.rm = T),max(c,na.rm = T))))
    min_max_new<-t(apply(current_data,2,function(c) c(min(c,na.rm = T),max(c,na.rm = T))))
    min_max_new_withoutoffset<-apply(min_max_new,1,function(r) c(0,(r[2]-r[1])))
    for(c in 1:ncol(current_data)){
       new_per<-(((current_data[,c]-min_max_new[c,1])*100)/min_max_new[c,2])
       current_data_scaled[,c]<-((new_per*min_max_ref_withoutoffset[c,2])/100)+min_max_ref[c,1]
    }
    colnames(current_data_scaled)<-paste0(colnames(current_data_scaled),'_scaled')
  }
  tmp<-cbind(wtdata_bmu[[i]],wtdata_filtered[wtdata_filtered$ld_id==lds_ids[i],],current_data_scaled)
  final<-rbind(final,tmp)
}
final

tmp_som<-som_codes[0,!(colnames(som_codes) %in% c('x','y'))]
tmp_som<-matrix(NA,ncol=sum(!(colnames(som_codes) %in% c('x','y'))),nrow=nrow(final))
colnames(tmp_som)<-paste0('som_codes_',colnames(som_codes)[!(colnames(som_codes) %in% c('x','y'))])
final<-cbind(final,tmp_som)
unique_som_codes<- unique(som_codes[,c('x','y')])
for(i in 1:dim(unique_som_codes)[1]){
  x<-as.numeric(unique_som_codes[i,1])
  y<-as.numeric(unique_som_codes[i,2])
  tmp<-som_codes[(som_codes$x==x)&(som_codes$y==y),!(colnames(som_codes) %in% c('x','y'))]
  colnames(tmp)<-paste0('som_codes_',colnames(tmp))
  selected_rows<-((final$x==x)&(final$y==y))
  final[selected_rows, colnames(tmp_som)]<-tmp
}


for(i in 1:length(unique(wtdata_filtered$ld_id))){
  final$bmu_clustering[final$ld_id==unique(wtdata_filtered$ld_id)[i]]<-clustering_result[[i,'clusters']]
}

final$som_clustering<-NA
for(n in 1:nrow(som_pts)){
  x=as.numeric(som_pts[n,1])
  y=as.numeric(som_pts[n,2])
  final$som_clustering[(final$x==x)&(final$y==y)]<-som_codes_clustering[n]
}