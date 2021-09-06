df<-wf1_confidential[!colnames(wf1_confidential) %in% c('date_time','TurbineId')]
df$pot_vel<-df$Power/df$WindSpd
df$pot_vel[is.infinite(df$pot_vel)]<-NA
complete_cases<-complete.cases(df)
dfc<-df[complete_cases,]
d<-dist(dfc)
hc<-hclust(d)
hcd = as.dendrogram(hc)
ggdendrogram(cut(hcd, h = 40)$upper,theme_dendro = FALSE,leaf_labels = F,labels=F)+theme_bw()+theme(axis.text.x = element_blank(),text = element_text(size=35))+labs(x="Data entries",y="Height")


clusters<-cutree(hc,k=4)
tmp_df<-wf1_confidential[complete_cases,]
tmp_df$date_time[tmp_df$TurbineId==132]
dt<-tmp_df$date_time[tmp_df$TurbineId==132]
c<-clusters[tmp_df$TurbineId==132]
dfplot<-data.frame(dt=dt,cluster=c)
#ggplot(dfplot,aes(x=dt,y=cluster))+geom_line()+theme_bw()+scale_y_continuous(limits = c(1, 4))+geom_smooth(method = 'loess',size=4)+theme(text = element_text(size=30))+labs(x="Date",y="Cluster id")
ggplot(dfplot,aes(x=dt,y=cluster))+geom_line()+theme_bw()+scale_y_continuous(limits = c(1, 4))+theme(text = element_text(size=30))+labs(x="Date",y="Cluster id")

clusters<-data.frame(date_time=as.numeric(),ld_id=as.numeric(),cluster=as.numeric())
for(ld in unique(clusters_xy$ld_id)){
  current_cluster<-clusters_xy[['clusters_machines']][clusters_xy$ld_id==ld][[1]]
  #current_cluster<-((current_cluster*1000)+ld)
  clusters<-rbind(clusters,data.frame(date_time=wtdata$date_time[som_selected_rows&wtdata$ld_id==ld],ld_id=ld,cluster=current_cluster))
}
#ggplot(clusters[clusters$ld_id==132,],aes(x=date_time,y=cluster))+geom_line()+geom_smooth(size=4)+theme_bw()+scale_y_continuous(limits = c(1, 5))+theme(text = element_text(size=30))+labs(x="Date",y="Cluster id")
ggplot(clusters[clusters$ld_id==132,],aes(x=date_time,y=cluster))+geom_line()+theme_bw()+scale_y_continuous(limits = c(1, 5))+theme(text = element_text(size=30))+labs(x="Date",y="Cluster id")
