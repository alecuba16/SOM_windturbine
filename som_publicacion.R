#fuhrlander diario
#wt_query<-data.frame(ld_id=NA,ld_code=NA,wp_id=13,wp_code=NA,seconds_to_aggregate=86400,array_id_walm="1210,1272,1273,1280,1359,1360,1361,1362,1363,1364,1365,1366,1380,1381,1382,1392,1702,2142",array_ot="",freq_dat_med_min=10,fault="Mbear1",type="phealtdeep",filter="frange,f8sd,fclean,fnzv",power_condition="",include_variables="",exclude_variables="regex:model|fake_data|^SPCosPhi|^FrecRed|^Estado",target_name="alarm",creation_wtdata_date_ini=1325376000,creation_wtdata_date_end=1419984000,model='train',creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE)
#fuhrlander 10minutal
#wt_query<-data.frame(ld_id=NA,ld_code=NA,wp_id=13,wp_code=NA,seconds_to_aggregate=600,array_id_walm="1210,1272,1273,1280,1359,1360,1361,1362,1363,1364,1365,1366,1380,1381,1382,1392,1702,2142",array_ot="",freq_dat_med_min=10,fault="Mbear1",type="phealtdeep",filter="frange,f8sd,fclean,fnzv",power_condition="",include_variables="",exclude_variables="regex:model|fake_data|^SPCosPhi|^FrecRed|^Estado",target_name="alarm",creation_wtdata_date_ini=1325376000,creation_wtdata_date_end=1419984000,model='train',creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE)
before_vars<-ls()
tmpfolder<-'tmp'
### Multiplicadora ###
#Escambrons diario vars normalidad
#wt_query<-data.frame(wp_id=19,wp_code='escamb',seconds_to_aggregate=86400,fault="gbox1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,TempAceiteMultip_avg,TempAmb_avg,TempRodamMultip_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10001,10003,10014,10015,10045,10065,10068,10072,10073,10074,10075',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Escambrons horario vars normalidad
#wt_query<-data.frame(wp_id=19,wp_code='escamb',seconds_to_aggregate=3600,fault="gbox1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,TempAceiteMultip_avg,TempAmb_avg,TempRodamMultip_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10001,10003,10014,10015,10045,10065,10068,10072,10073,10074,10075',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Izco diario vars normalidad
#wt_query<-data.frame(wp_id=20,wp_code='izco',seconds_to_aggregate=86400,fault="gbox1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,TempAceiteMultip_avg,TempAmb_avg,TempMultip_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10001,10003,10014,10015,10045,10065,10068,10072,10073,10074,10075',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Izco horario vars normalidad
#wt_query<-data.frame(wp_id=20,wp_code='izco',seconds_to_aggregate=3600,fault="gbox1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,TempAceiteMultip_avg,TempAmb_avg,TempMultip_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10001,10003,10014,10015,10045,10065,10068,10072,10073,10074,10075',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Moncay diario vars normalidad
#wt_query<-data.frame(wp_id=21,wp_code='moncay',seconds_to_aggregate=86400,fault="gbox1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,TempAceiteMultip_avg,TempAmb_avg,TempRodamMultip_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10001,10003,10014,10015,10045,10065,10068,10072,10073,10074,10075',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Moncay horario vars normalidad
#wt_query<-data.frame(wp_id=20,wp_code='izco',seconds_to_aggregate=3600,fault="gbox1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,TempAceiteMultip_avg,TempAmb_avg,TempRodamMultip_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10001,10003,10014,10015,10045,10065,10068,10072,10073,10074,10075',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Bernabe diario vars normalidad
wt_query<-data.frame(wp_id=25,wp_code='bernabe',seconds_to_aggregate=86400,fault="gbox1",type="som",filter="frange",include_variables='wgen_w,wnac_wdspd,wrot_rotspd,wtrm_hyoiltmp,wnac_extmp,wtrm_tmpgbxoil',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='147,10008',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 

### Generador ###
#Escambrons diario vars normalidad
#wt_query<-data.frame(wp_id=19,wp_code='escamb',seconds_to_aggregate=86400,fault="gen1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,VelGen_avg,TempCojLOA_avg,TempAmb_avg,TempCojLA_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10080,10079,10064,10061,10057,10047,10042,10038,10006,10004',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Escambrons horario vars normalidad
#wt_query<-data.frame(wp_id=19,wp_code='escamb',seconds_to_aggregate=3600,fault="gen1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,VelGen_avg,TempCojLOA_avg,TempAmb_avg,TempCojLA_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10080,10079,10064,10061,10057,10047,10042,10038,10006,10004',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Izco diario vars normalidad
#wt_query<-data.frame(wp_id=20,wp_code='izco',seconds_to_aggregate=86400,fault="gen1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,VelGenTop_avg,TempCojLOA_avg,TempAmb_avg,TempCojLA_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10080,10061,10047,10042,10006,10004',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Izco horario vars normalidad
#wt_query<-data.frame(wp_id=20,wp_code='izco',seconds_to_aggregate=3600,fault="gen1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,VelGenTop_avg,TempCojLOA_avg,TempAmb_avg,TempCojLA_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10080,10061,10047,10042,10006,10004',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 

ifelse(!dir.exists(tmpfolder), dir.create(tmpfolder), FALSE)
#Dependencia basica
if(!exists("dependencyLoader")){
  if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
  source('functions_common/dependencyLoader.R')
}

# Sources
libraries<-c('plotly','ggplot2','xtable','kohonen')
sources_common<-paste0("functions_common/",c('load_wtdata.R','close_protocol.R','db_query.R','filter_custom.R'))
sources_som<-paste0("functions_som/",c('create_som_model.R','create_map_clustering.R'))

dep<-dependencyLoader(c(libraries,sources_common,sources_som))
if(dep$error)  stop(paste0("\n",iam,":on call dependencyLoader\n",dep$msg))
#debug_mode<-TRUE

db_config<-data.frame(user="user",password="password",dbname="yourHistoricalBD",host="127.0.0.1",port=3306)
if(!(Sys.info()["nodename"] %in% c("alexsmartive"))){
  db_config$host <- 'yourHost'
  db_config$port<-10003
}

date_time_name<-'date_time'

if(exists("wtdata_park",inherits = F)) rm(wtdata_park)

if(is.na(wt_query$ld_id)||is.null(wt_query$ld_id)){#Park prediction
  rs<-db_query(query=paste0('SELECT ld.ld_id,ld.ld_code,p.wp_code,ld.freq_dat_med_min from smartcast_DB.SC_LOGICALDEVICE ld INNER JOIN smartcast_DB.SC_WPLANT p ON ld.wp_id=p.wp_id where ld.wp_id=',wt_query$wp_id),db_config=db_config)
}else{#One turbine
  rs<-db_query(query=paste0('SELECT ld.ld_id,ld.ld_code,p.wp_code,ld.freq_dat_med_min from smartcast_DB.SC_LOGICALDEVICE ld INNER JOIN smartcast_DB.SC_WPLANT p ON ld.wp_id=p.wp_id where ld.wp_id=',wt_query$wp_id,' and ld.ld_id=',wt_query$ld_id),db_config=db_config)
}
if(rs$error)  stop(paste0("\n",iam,":on call query data_table_name \n",dep$msg))
turbines<-rs$data
for(t in 1:nrow(turbines)){
  if(t>1){
    tmp<-wt_query[1,,drop=F]
    tmp$ld_id<-turbines$ld_id[t]
    tmp$ld_code<-turbines$ld_code[t]
    wt_query<-rbind(wt_query,tmp)
  }else{
    wt_query$ld_id<-turbines$ld_id[t]
    wt_query$ld_code<-turbines$ld_code[t]
  }
}
rs  <-  load_wtdata(wt_query=wt_query,
                    date_time_name=date_time_name,
                    filter_exclude=paste(date_time_name,"ld_id",sep=","),
                    update_filter_ranges=F,
                    patch=F,
                    filter_verbose=F,
                    db_config=db_config)
if(rs$error) {
  output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
  close_protocol(output_msg, iam, debug_mode)
  stop(output_msg)
}
if(rs$warning) {
  output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
  close_protocol(output_msg, iam, debug_mode)
  stop(output_msg)
}
wtdata<-rs$data$wtdata
outliers<-rs$data$outliers

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
save(list=to_save,file=paste0(tmpfolder,'/wtdata_',wt_query$wp_code[1],'_',wt_query$seconds_to_aggregate[1],'_',wt_query$fault[1],'.RData'),compress = 'xz')
rm(list=to_save)
gc(verbose = F)

######################### SOM QE TE EVALUATION #######################
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
publication<-T
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
wp_code<-tmp_env$wt_query$wp_code[1]
fault<-tmp_env$wt_query$fault[1]
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
date_time_name<-tmp_env$date_time_name
rm(tmp_env)
gc(verbose = F)
if(!exists('get_optimal_som_size')) source('functions_som/get_optimal_som_size.R')
library(reshape2)
library(ggplot2)
library(htmlwidgets)

exclude_variables<-c('ld_id',date_time_name,'alarm','alarm_block_code','alarm_all_block_code','ot','ot_all','ot_block_code','ot_all_block_code')

#Publication eps
if(publication){
  plot<-T
  plotly<-F
  plot_title<-F
  big_fonts<-T
  axis_tittle<-F
  legend<-F
}else{
  plot<-T
  plotly<-F
  plot_title<-T
  big_fonts<-F
  axis_tittle<-T
  legend<-T
}
alpha<-c(0.05,0.01)
rlen<-10
rs<-get_optimal_som_size(wtdata=wtdata[,!(colnames(wtdata) %in% exclude_variables)],dim=c(20,100,10),rlen=rlen,alpha=alpha,parallel_mode=T,log_file=NULL,verbose=T,normalize=T,plot=T,plotly=F,plot_title=F,big_fonts=F,axis_tittle=T,legend=T)
if(rs$error)stop(rs$msg)
dim<-rs$data$optimal_dim
q<-rs$data$metrics

filename<-paste0(wp_code,'_',fault,'_',seconds_to_aggregate,'_som_qe_vs_te_dims_',min_dim,'-',max_dim)

if(publication) ggsave(paste0(getwd(),'/',tmpfolder,'/',filename,'.eps'),device='eps',plot=rs$data$plot, width = 20, height = 12)

if(!plotly){
  ggsave(paste0(getwd(),'/',tmpfolder,'/',filename,'.png'),plot=p, width = 20, height = 12)
}else{
  htmlwidgets::saveWidget(rs$data$plot,file = paste0(getwd(),'/',tmpfolder,'/',filename,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = T)
}

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
save(list=to_save,file=paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_dim_benchmark.RData'),compress = 'xz')
rm(list=to_save)
gc(verbose = F)

#################### OPTIMAL MODEL #################################
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
wp_code<-tmp_env$wt_query$wp_code[1]
fault<-tmp_env$wt_query$fault[1]
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
date_time_name<-tmp_env$date_time_name
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_dim_benchmark.RData') ,envir = tmp_env)
q<-tmp_env$q
exclude_variables<-tmp_env$exclude_variables
dim<-tmp_env$dim
rlen<-tmp_env$rlen
alpha<-tmp_env$alpha
rm(tmp_env)
gc(verbose = F)

if(!exists('create_som_model')) source('functions_som/create_som_model.R')
library(reshape2)
library(ggplot2)
library(plotly)
library(htmlwidgets)

#Create new mode
rs<-create_som_model(wtdata=wtdata[,!(colnames(wtdata) %in% exclude_variables)],dim=dim,rlen=rlen,alpha=alpha,parallel_mode=F,log_file=NULL,verbose=T,normalize=T)
if(rs$error) stop(rs$msg)

som_result<-rs$data
som_model<-som_result$model[[1]]
rs<-som_result$get_som_vars(som_model)
if(rs$error) stop(rs$msg)
som_pts<-rs$data$som_pts
som_codes<-rs$data$som_codes
som_distances<-rs$data$som_distances
dim<-max(rs$data$som_pts,na.rm = T)
som_selected_rows<-som_result$selected_rows
som_selected_columns<-som_result$selected_columns
wtdata_bmu<-rs$data$wtdata_bmu
#rs<-som_result$get_metrics(som_codes = rs$data$som_codes,som_pts=rs$data$som_pts,som_distances = rs$data$som_distances,data = rs$data$data[[1]],verbose=T)

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
save(list=to_save,file=paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_optimal_model.RData'),compress = 'xz')
rm(list=to_save)
gc(verbose = F)

################# Plots over som map, clustering and umatrix ##########################
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
#publicacion<-T
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
wp_code<-tmp_env$wt_query$wp_code[1]
fault<-tmp_env$wt_query$fault[1]
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
date_time_name<-tmp_env$date_time_name
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_optimal_model.RData') ,envir = tmp_env)
som_codes<-tmp_env$som_codes
wtdata_bmu<-tmp_env$wtdata_bmu
som_pts<-tmp_env$som_pts
som_selected_columns<-tmp_env$som_selected_columns
som_selected_rows<-tmp_env$som_selected_rows
dim<-tmp_env$dim
rm(tmp_env)
gc(verbose = F)

library(htmlwidgets)
library(grDevices)
library(ggpubr)

# use hierarchical clustering to cluster the codebook vectors
clusters<-hclust(dist(som_codes))
colnames(som_codes)<-som_selected_columns
#Idendro
#install.packages("idendr0")
#idendr0
#idendr0::idendro(clusters,som_codes)

som_cluster <- cutree(hclust(dist(som_codes)), 5)

if(!exists('create_umatrix')) source('functions_som/create_umatrix.R')
rs<-create_umatrix(som_pts=som_pts,som_codes=som_codes)
if(rs$error) stop(rs$msg)
umatrix<-rs$data$umatrix

plotly<-F
perps<-F
plot_with_clusters<-T
if(publicacion){
  plotly<-F
  perps<-T
  plot_with_clusters<-T
}
if(!exists('plot_umatrix')) source('functions_som/plot_umatrix.R')
rs<-plot_umatrix(umatrix=umatrix,som_pts=som_pts,som_cluster=som_cluster,wtdata_bmu=wtdata_bmu,plotly=plotly,perps=perps,plot_with_clusters=plot_with_clusters)
if(rs$error) stop(rs$msg)
plotted_umatrix<-rs$data$umatrix
plotted_umatrix_with_clusters<-rs$data$umatrix_with_clusters
file_name<-paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_umatrix')
if(publicacion){
  setEPS()
  postscript(paste0(file_name,'.eps'))
  plotted_umatrix
  dev.off()
  if(!is.null(plot_with_clusters)){
    setEPS()
    postscript(paste0(file_name,'_with_clustering.eps'))
    plotted_umatrix_with_clusters
    dev.off()
  }
}else if(plotly){
  htmlwidgets::saveWidget(plot,paste0(file_name,'.png'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = T)
  if(!is.null(plot_with_clusters)) htmlwidgets::saveWidget(plot_with_clusters,paste0(file_name,'_with_clustering.png'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = T)
}else{
  ggsave(paste0(file_name,'.png'),plot=plot, width = 20, height = 20)
  if(!is.null(plot_with_clusters)) ggsave(paste0(file_name,'_with_clustering.png'),plot=plot_with_clusters, width = 20, height = 20)
}

#---- histogram som cluster variables ------
tmp_som_selected_columns<-c(date_time_name,som_selected_columns)
cols<-length(tmp_som_selected_columns)

som_data<-wtdata[som_selected_rows,]

mins<-NULL
maxs<-NULL
for(c in 1:cols){
  mins<-c(mins,min(som_data[,tmp_som_selected_columns[c]],na.rm = T))
  maxs<-c(maxs,max(som_data[,tmp_som_selected_columns[c]],na.rm = T))
}

p<-list()
count<-1
for(cl in unique(som_cluster)){
  for(c in 1:cols){
    data<-som_data[som_cluster==cl,tmp_som_selected_columns[c]]
    df<-data.frame(x=1:length(data),y=data)
    t<-ggplot(df,aes(x=y))+geom_histogram(fill='black')+xlab(label = tmp_som_selected_columns[c])
    if(tmp_som_selected_columns[c]==date_time_name) 
      t<-t+xlim(as.POSIXct(mins[c],tz = 'UTC',origin = '1970-01-01'),as.POSIXct(maxs[c],tz = 'UTC',origin = '1970-01-01'))
    else
      t<-t+ xlim(mins[c],maxs[c])
    p[[count]]<-t
    count<-count+1
  }
}

#p<-marrangeGrob(p, nrow=length(unique(som_cluster)), ncol=cols)
m<-matrix(,ncol=cols,nrow=length(unique(som_cluster)))
m[,1]<-unique(som_cluster)
gp<-ggarrange(plotlist = p, ncol =cols, nrow = length(unique(som_cluster)),labels = t(m))
ggsave(filename = 'test.png',width = 40,height = 30,plot = gp)
#---------------------------------------- Heatmaps per variable ------------------------------
base_path<-paste0(getwd(),'/',tmpfolder,'/var_heatmaps')
ifelse(!dir.exists(base_path),dir.create(base_path), FALSE)
if(publicacion){
  plotly<-F
  legend<-F
  big_fonts<-T
  axis_tittle<-F
}
if(!exists('plot_heatmap_variable')) source('functions_som/plot_heatmap_variable.R')
rs<-plot_heatmap_variable(wtdata=wtdata,wtdata_bmu=wtdata_bmu,som_pts=som_pts,som_selected_columns=som_selected_columns,plotly=plotly,legend=legend,big_fonts=big_fonts,axis_tittle=axis_tittle)
if(rs$error) stop(rs$msg)
lds_ids<-rs$data$lds_ids
plots<-rs$data$plots

for(i in 1:length(lds_ids)){
  for(c in 1:length(som_selected_columns)){
    file_name<-paste0(wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_',lds_ids[i],'_',som_selected_columns[c])
    position<-(i*length(som_selected_columns))-length(som_selected_columns)+c
    if(plotly){
      htmlwidgets::saveWidget(plots[[position]],paste0(base_path,'/',file_name,'.html'),libdir = paste0(base_path,'/lib'),selfcontained = F)
    }else if(publicacion){
      ggsave(paste0(base_path,'/',file_name,'.eps'), device=cairo_ps,plot=plots[[position]], width = 12, height = 12)
    }else{
      ggsave(paste0(base_path,'/',file_name,'.png'),plot=plots[[position]], width = 20, height = 20)
    }
  }
}

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
rm(list=to_save)
gc(verbose = F)

####################### End Plots ###################


##############  Grouping multiple machines Clustering ############
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
wp_code<-tmp_env$wt_query$wp_code[1]
fault<-tmp_env$wt_query$fault[1]
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
date_time_name<-tmp_env$date_time_name
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_optimal_model.RData') ,envir = tmp_env)
som_codes<-tmp_env$som_codes
wtdata_bmu<-tmp_env$wtdata_bmu
som_pts<-tmp_env$som_pts
som_selected_columns<-tmp_env$som_selected_columns
som_selected_rows<-tmp_env$som_selected_rows
dim<-tmp_env$dim
rm(tmp_env)
gc(verbose = F)

library(ggplot2)
library(plotly)
library(htmlwidgets)
library(reshape2)



nclusters<-5
if(!exists("create_map_clustering")) source("functions_som/create_map_clustering.R")
rs<-create_map_clustering(nclusters,wtdata_bmu,wtdata$ld_id[som_selected_rows],som_pts)
if(rs$error) stop(rs$msg)
clustering_result<-rs$data
clusters_xy<-list(ld_id=unlist(clustering_result[,'ld_id']),clusters_machines=clustering_result[,'clusters'])

########################## TEST auto clustering ##############################
results_metrics<-data.frame(nclusters=as.numeric(),ld_id=as.numeric(),davbould=numeric(),sse=numeric(),si_bt0=numeric(),si_lt0=numeric(),si_eq0=numeric())
for(c in 2:10){
  rs<-create_map_clustering(c,wtdata_bmu,wtdata$ld_id[som_selected_rows],som_pts)
  if(rs$error) stop(rs$msg)
  clustering_result<-rs$data
  calcule_metrics<-clustering_result[[1,'calcule_metrics']]
  for(i in 1:nrow(clustering_result)){
    rs<-calcule_metrics(clusters = clustering_result[[i,'clusters']],matrix_xy = clustering_result[[i,'active_bmus_xy_scaled']])
    if(rs$error) stop(rs$msg)
    results_metrics<-rbind(results_metrics,data.frame(nclusters=c,ld_id=clustering_result[[i,'ld_id']],davbould=rs$data$davbould,sse=rs$data$sse,si_bt0=rs$data$si$bt0,si_lt0=rs$data$si$lt0,si_eq0=rs$data$si$eq0))
  }
}


results_metrics_avg<-results_metrics[,c('nclusters','davbould','sse','si_bt0','si_lt0','si_eq0')] %>% group_by(nclusters) %>% summarise_all(mean,na.rm=T)

results_metrics_avg$davbould_norm<-(results_metrics_avg$davbould-min(results_metrics_avg$davbould))/(max(results_metrics_avg$davbould)-min(results_metrics_avg$davbould))
results_metrics_avg$sse_norm<-(results_metrics_avg$sse-min(results_metrics_avg$sse))/(max(results_metrics_avg$sse)-min(results_metrics_avg$sse))

qp<-melt(results_metrics_avg,id='nclusters')
#p<-ggplot(qp,aes(x=nclusters,y=value,group=variable,color=variable))+geom_line(data=function(x) x[x$variable %in% c('davbould_norm','sse_norm'),],size=2)+theme_bw()+theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank(),text = element_text(size=35),plot.margin=unit(c(0,0,0,0),"mm"))
p<-ggplot(qp,aes(x=nclusters,y=value,group=variable,color=variable))+geom_line(data=function(x) x[x$variable %in% c('davbould_norm','sse_norm'),],size=2)+theme_bw()+theme(text = element_text(size=35),plot.margin=unit(c(0,0,0,0),"mm"))
ggsave(paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_clustering_metrics.png'),plot=p, width = 20, height = 12)

#Get best som size intersection between two scores
qs<-data.frame(nclusters=seq(min(results_metrics_avg$nclusters),max(results_metrics_avg$nclusters),0.5))
qs$davbould_norm_s<-predict(loess(results_metrics_avg$davbould_norm~results_metrics_avg$nclusters),newdata = qs$nclusters)
qs$sse_norm_s<-predict(loess(results_metrics_avg$sse_norm~results_metrics_avg$nclusters),newdata = qs$nclusters)
intersection<-qs$nclusters[as.logical(abs(diff(qs$davbould_norm_s < qs$sse_norm_s)))]
best_nclusters<-ceiling(intersection)[2]
################### TEST auto clustering #########################

#if(!is.data.frame(centroids)) centroids<-do.call("rbind",centroids)
clusters_xy<-list(ld_id=unlist(clustering_result[,'ld_id']),clusters_machines=clustering_result[,'clusters'])
tmp_x<-as.numeric(sapply(1:nrow(clustering_result),function(i) clustering_result[[i,'cluster_centroids']][,1]))
tmp_y<-as.numeric(sapply(1:nrow(clustering_result),function(i) clustering_result[[i,'cluster_centroids']][,2]))

centroids<-data.frame(ld_id=unlist(lapply(unlist(clustering_result[,'ld_id']),function(ld) rep(ld,nclusters))),x=tmp_x,y=tmp_y,cluster=rep(1:nclusters,nrow(clustering_result)))
centroids$ld_id<-as.factor(centroids$ld_id)

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
save(list=to_save,file=paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_clustering.RData'),compress = 'xz')
#rm(list=to_save)
#gc(verbose = F)

#------------ Plots centroids all turbines ---------------
p<-ggplot(centroids,aes(x=x,y=y,color=ld_id,label=ld_id))+geom_point()+geom_text(hjust = 0, nudge_x = 0.05,show.legend = F)
file_name<-paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_centroids')
ggsave(paste0(file_name,'.png'),plot=p, width = 20, height = 12)
gp<-ggplotly(p,width = 1280,height = 1024)
htmlwidgets::saveWidget(gp,file = paste0(file_name,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = T)

#-------------------- Plots active neurons per turbine --------------
if(publicacion){
  plotly<-F
  legend<-F
  big_fonts<-T
  axis_tittle<-F
}
if(!exists("plot_bmu_turbines")) source("functions_som/plot_bmu_turbines.R")
rs<-plot_bmu_turbines(num_registers_threshold=1,wtdata=wtdata[som_selected_rows,],wtdata_bmu=wtdata_bmu,clusters_xy=clusters_xy,plotly=plotly,legend=legend,big_fonts=big_fonts,axis_tittle=axis_tittle)
if(rs$error) stop(rs$msg)
lds_ids<-rs$data$lds_ids
plots<-rs$data$plots

for(i in 1:length(lds_ids)){
  file_name<-paste0(wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_',lds_ids[i],'_activeBMUs')
  if(plotly){
    htmlwidgets::saveWidget(plots[[i]],paste0(getwd(),'/',tmpfolder,'/',file_name,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)
  }else if(publicacion){
    ggsave(paste0(getwd(),'/',tmpfolder,'/',file_name,'.eps'), device=cairo_ps,plot=plots[[i]], width = 12, height = 12)
  }else{
    ggsave(paste0(getwd(),'/',tmpfolder,'/',file_name,'.png'),plot=plots[[i]], width = 20, height = 20)
  }
}

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
rm(list=to_save)
gc(verbose = F)

############################# TABLE OF PAIRS ######################################

if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
wp_code<-tmp_env$wt_query$wp_code[1]
wp_id<-tmp_env$wt_query$wp_id[1]
fault<-tmp_env$wt_query$fault[1]
array_id_ot<-tmp_env$wt_query$array_ot[1]
db_config<-tmp_env$db_config
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
date_time_name<-tmp_env$date_time_name
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_optimal_model.RData') ,envir = tmp_env)
som_codes<-tmp_env$som_codes
wtdata_bmu<-tmp_env$wtdata_bmu
som_pts<-tmp_env$som_pts
som_selected_columns<-tmp_env$som_selected_columns
som_selected_rows<-tmp_env$som_selected_rows
dim<-tmp_env$dim
rm(tmp_env)
gc(verbose = F)

tmp_env<-new.env()
load(file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_after_clustering.RData') ,envir = tmp_env)
centroids<-tmp_env$centroids
nclusters<-tmp_env$nclusters
rm(tmp_env)
gc(verbose = F)

library(xtable)
if(!exists("calcule_distance_matrix_turbines")) source("functions_som/calcule_distance_matrix_turbines.R")
rs<-calcule_distance_matrix_turbines(centroids=centroids,use_simplex=T)
if(rs$error)stop(rs$msg)
distance_matrix<-rs$data$distance_matrix
clusters_pairs<-rs$data$clusters_pairs

#mins<-apply(distance_matrix,2,function(x)return(array(which.min(x))))
#mins<-data.frame(col=names(mins),row=mins)
#distance_matrix$mins<-apply(mins,1,FUN=function(x)return(paste(x["col"],rownames(distance_matrix[as.numeric(x["row"]),]),distance_matrix[as.numeric(x["row"]),x["col"]],sep="/")))
print(xtable(distance_matrix), type = "html",file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_table_pairwise_distances.html'))

#----------------- Groups automatic 3-4 groups -------------------

if(!exists("formatter_get_tableinfo")) source("functions_common/formatter_get_tableinfo.R")

rs<-formatter_get_tableinfo(wp_id = wp_id,db_config = db_config)
if(rs$error) stop(rs$msg)
data_table_name<-rs$data$data_table_name
events_table_name<-rs$data$alarms_table_name
ot_table_name<-rs$data$ot_table_name

groups_final<-NULL
for(objective_gid in c(3,4,5)){
  cat(objective_gid,'\n')
  continue<-T
  tested_p<-NULL #To avoid infinite bucles
  p<-0.05#Try with 10% of range
  step<-0.01
  step_back<-step/2
  while(continue){
    #cat(p,' ')
    if(p<0||p>1){
      continue=T
      error=T
    }
    gid<-1
    threshold<-NA
    dmatrix_tmp<-distance_matrix
    groups<-data.frame(error=logical(),n_groups=numeric(),gid=numeric(),lds=character(),stringsAsFactors = FALSE) 
    while(is.matrix(dmatrix_tmp)&&(nrow(dmatrix_tmp)>0)&&(gid<=objective_gid)){
      if(dim(dmatrix_tmp)[1]>1){
        d_min<-min(dmatrix_tmp,na.rm=TRUE)
        d_max<-max(dmatrix_tmp,na.rm=TRUE)
        if(is.na(threshold)) threshold<-(d_max-d_min)*p
        r<-which((dmatrix_tmp==d_min),arr.ind = T)[1,1]
        current_ld<-rownames(dmatrix_tmp)[r]
        column_exclude<-which(current_ld==colnames(dmatrix_tmp))
        to_check<-dmatrix_tmp[r,-column_exclude]
        lds_found<-names(to_check)[to_check<=(d_min+threshold)]
        lds<-c(current_ld,lds_found)
        lds_txt<-paste0(lds[order(lds)],collapse = ',')
        groups<-rbind(groups,data.frame(n_groups=objective_gid,gid=gid,lds=lds_txt,stringsAsFactors = FALSE))
        rownames<-rownames(dmatrix_tmp)[(!rownames(dmatrix_tmp) %in% lds)]
        colnames<-colnames(dmatrix_tmp)[(!colnames(dmatrix_tmp) %in% lds)]
        dmatrix_tmp<-as.matrix(dmatrix_tmp[!rownames(dmatrix_tmp) %in% lds,!colnames(dmatrix_tmp) %in% lds])
        rownames(dmatrix_tmp)<-rownames
        colnames(dmatrix_tmp)<-colnames
      }else{
        groups<-rbind(groups,data.frame(n_groups=objective_gid,gid=gid,lds=as.character(rownames(dmatrix_tmp)[1]),stringsAsFactors = FALSE))
        dmatrix_tmp<-NULL
      }
      if(!is.null(dmatrix_tmp))gid<-gid+1
    }
    if(gid==objective_gid&&is.null(dmatrix_tmp)){
      groups$error[groups$n_groups==gid]<-F
      continue=FALSE
    }else{
      tested_p<-c(tested_p,p)
      if(gid>objective_gid){
        while((p %in% tested_p)&&(p<1)){
          p<-p+step
        }
      }else{
        while((p %in% tested_p)&&(p>0)){
          p<-p-step
        }
      }
    }
  }
  groups$priori_avg<-NA
  groups$priori_median<-NA
  groups$priori_sdv<-NA
  groups$count_ots_group<-NA
  groups$count_ots_mean<-NA
  groups$count_ots_sdv<-NA
  groups$count_ots_median<-NA
  #Load probability
  for(i in 1:nrow(groups)){
    current_group<-groups$lds[i]
    query<-paste0("SELECT ld_id,prob_fault_current from ",paste0(data_table_name,'_priori')," where fault='",fault,"' AND ld_id IN (",current_group,")")
    rs<-db_query(query,db_config);
    if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",rs$msg))
    df<-rs$data %>% group_by(ld_id) %>% summarise_all(funs(mean(., na.rm = T)))
    prob<-by(df,df$ld_id,function(x) sum(x$prob_fault_current,na.rm = T))
    prob<-cbind(prob)
    
    #avg
    groups$priori_avg[i]<-mean(prob,na.rm = T)
    #median
    groups$priori_median[i]<-median(prob,na.rm = T)
    #sdv
    groups$priori_sdv[i]<-sd(prob,na.rm = T)
    
    #Number of ots of the group
    if(is.null(array_id_ot)||is.na(array_id_ot)||nchar(array_id_ot)<1){
      query<-paste0("SELECT COUNT(distinct ot) as count,et.ld_id from ",ot_table_name," et where et.ld_id IN(",current_group,") AND (et.desc_substma='Generador' OR et.desc_substma='Multiplicadora') group by et.ld_id,et.ot,et.date_time")
    }else{
      query<-paste0("SELECT COUNT(distinct ot) as count,et.ld_id from ",ot_table_name," et where et.ld_id IN(",current_group,") AND et.id_ot IN(",array_id_ot,") group by et.ld_id,et.ot,et.date_time")
    }
    rs<-db_query(query,db_config);
    if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",rs$msg))
    if(nrow(rs$data)>0){
      groups$count_ots_group[i]<-sum(rs$data$count,na.rm = T)
      groups$count_ots_mean[i]<-sum(rs$data$count,na.rm = T)/length(unlist(strsplit(current_group,split = ',')))
      t<-table(rs$data)
      groups$count_ots_sdv[i]<-ifelse(ncol(t)>1,sd(t[1,],na.rm = T),0)
      groups$count_ots_median[i]<-median(t[1,],na.rm = T)
    }else{
      groups$count_ots_group[i]<-0
      groups$count_ots_mean[i]<-0
      groups$count_ots_sdv[i]<-0
      groups$count_ots_median[i]<-0
    }
  }
  groups_final<-rbind(groups_final,groups)
}

print(xtable(groups_final), type = "html",file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_table_groups.html'))
