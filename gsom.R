#fuhrlander diario
#wt_query<-data.frame(ld_id=NA,ld_code=NA,wp_id=13,wp_code=NA,seconds_to_aggregate=86400,array_id_walm="1210,1272,1273,1280,1359,1360,1361,1362,1363,1364,1365,1366,1380,1381,1382,1392,1702,2142",array_ot="",freq_dat_med_min=10,fault="Mbear1",type="phealtdeep",filter="frange,f8sd,fclean,fnzv",power_condition="",include_variables="",exclude_variables="regex:model|fake_data|^SPCosPhi|^FrecRed|^Estado",target_name="alarm",creation_wtdata_date_ini=1325376000,creation_wtdata_date_end=1419984000,model='train',creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE)
#fuhrlander 10minutal
#wt_query<-data.frame(ld_id=NA,ld_code=NA,wp_id=13,wp_code=NA,seconds_to_aggregate=600,array_id_walm="1210,1272,1273,1280,1359,1360,1361,1362,1363,1364,1365,1366,1380,1381,1382,1392,1702,2142",array_ot="",freq_dat_med_min=10,fault="Mbear1",type="phealtdeep",filter="frange,f8sd,fclean,fnzv",power_condition="",include_variables="",exclude_variables="regex:model|fake_data|^SPCosPhi|^FrecRed|^Estado",target_name="alarm",creation_wtdata_date_ini=1325376000,creation_wtdata_date_end=1419984000,model='train',creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE)
iam<-'gsom'
before_vars<-ls()
tmpfolder<-'tmp'
debug_mode<-T
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
#wt_query<-data.frame(wp_id=25,wp_code='bernabe',seconds_to_aggregate=86400,fault="gbox1",type="som",filter="frange,f8sd",include_variables='wgen_w,wnac_wdspd,wrot_rotspd,wtrm_hyoiltmp,wnac_extmp,wtrm_tmpgbxoil',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='147,10008',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 

#Bernabe 10minutal normalidad
#wt_query<-data.frame(wp_id=25,wp_code='bernabe',seconds_to_aggregate=600,fault="gbox1",type="som",filter="frange,f8sd,fclean,fnzv",include_variables='wgen_w,wnac_wdspd,wrot_rotspd,wtrm_hyoiltmp,wnac_extmp,wtrm_tmpgbxoil,alarm,ot',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='147,170,10008',array_ot='10003,10014,10001,10045,10065,10068,10075,10081,10082',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 


### Generador ###
#Escambrons diario vars normalidad
#wt_query<-data.frame(wp_id=19,wp_code='escamb',seconds_to_aggregate=86400,fault="gen1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,VelGen_avg,TempCojLOA_avg,TempAmb_avg,TempCojLA_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10080,10079,10064,10061,10057,10047,10042,10038,10006,10004',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Escambrons horario vars normalidad
#wt_query<-data.frame(wp_id=19,wp_code='escamb',seconds_to_aggregate=3600,fault="gen1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,VelGen_avg,TempCojLOA_avg,TempAmb_avg,TempCojLA_avg',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10080,10079,10064,10061,10057,10047,10042,10038,10006,10004',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
#Izco diario vars normalidad
wt_query<-data.frame(wp_id=20,wp_code='izco',seconds_to_aggregate=86400,fault="gen1",type="som",filter="frange",include_variables='VelViento_avg,Pot_avg,VelRotor_avg,VelGenTop_avg,TempCojLOA_avg,TempAmb_avg,TempCojLA_avg,ot,alarm',creation_wtdata_date_ini=1388534400,creation_wtdata_date_end=1514764800,power_condition='',array_id_walm='',array_ot='10006,10038,10042,10047,10061,10064',freq_dat_med_min=10,ld_id=NA,ld_code=NA,exclude_variables="",target_name=NA,creation_trn_percent=100,creation_model_path="windfarms/",creation_log_path="windfarms/",stringsAsFactors = FALSE) 
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
                    filter_exclude=paste(date_time_name,'alarm','ot',"ld_id",sep=","),
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

#################### SOM MODEL #################################
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

if(!exists('create_som_model')) source('functions_som/create_som_model.R')
library(reshape2)
library(ggplot2)
library(plotly)
library(htmlwidgets)

exclude_variables<-c('ld_id',date_time_name,'alarm','alarm_block_code','alarm_all_block_code','ot','ot_all','ot_block_code','ot_all_block_code')
#Create new model
model_params<-list(type='gsom',spreadFactor=0.8,hex=T) #Use gsom model
#model_params<-list(type='som',dim=10,rlen=10,alpha<-c(0.05,0.01),sizeRuleOfTheThumb=T,hex=F) #Use som model
model_params<-list(type='som',dim=NULL,rlen=10,alpha=c(0.05,0.01),sizeRuleOfTheThumb=T,hex=F) #Use som model

parallel_mode<-T
log_file<-NULL #For parallel mode logs
verbose<-T #Print msg
normalize<-T #Normalize data (necessary for faster converge)

rs<-create_som_model(wtdata=wtdata[,!(colnames(wtdata) %in% exclude_variables)],model_params=model_params,parallel_mode=parallel_mode,log_file=NULL,verbose=T,normalize=T)
if(rs$error) stop(rs$msg)
som_result<-rs$data
model_params<-rs$data$model_params
som_model<-som_result$model
som_pts<-rs$data$som_pts
som_codes<-rs$data$som_codes
som_distances<-rs$data$som_distances
som_center_neuron_position<-rs$data$center_neuron_position
dim<-rs$data$dim
som_selected_rows<-som_result$selected_rows
som_selected_columns<-som_result$selected_columns
wtdata_bmu<-rs$data$wtdata_bmu
hex<-rs$data$hex
stat<-rs$data$stat
#ATTENTION SLOW!!
rs<-som_result$get_metrics(som_codes = som_codes,som_pts=som_pts,som_distances =som_distances,wtdata = wtdata[som_selected_rows,colnames(wtdata) %in% som_selected_columns],verbose=T,stat=stat)
if(rs$error) stop(rs$msg)
qe<-rs$data$qe
te<-rs$data$te


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
model_params<-tmp_env$model_params
hex<-tmp_env$hex
rm(tmp_env)
gc(verbose = F)

library(htmlwidgets)
library(grDevices)

# use hierarchical clustering to cluster the codebook vectors
clusters<-hclust(dist(som_codes))
colnames(som_codes)<-som_selected_columns
#Idendro
#install.packages("idendr0")
#idendr0
#idendr0::idendro(clusters,som_codes)

som_cluster <- cutree(hclust(dist(som_codes)), 5)

if(!exists('create_umatrix')) source('functions_som/create_umatrix.R')
rs<-create_umatrix(som_pts=som_pts,som_codes=som_codes,hex=hex)
if(rs$error) stop(rs$msg)
umatrix<-rs$data$umatrix

plotly_umatrix<-T
perps_umatrix<-F
plotly_umatrix_clusters<-F
perps_umatrix_clusters<-F
if(exists('publicacion')&&publicacion){
  plotly_umatrix<-F
  perps_umatrix<-T
  plotly_umatrix_clusters<-F
  perps_umatrix_clusters<-T
}
if(!exists('plot_umatrix')) source('functions_som/plot_umatrix.R')
rs<-plot_umatrix(umatrix=umatrix,som_pts=som_pts,som_cluster=som_cluster,wtdata_bmu=wtdata_bmu,model_params=model_params,plotly_umatrix=plotly_umatrix,perps_umatrix=perps_umatrix,plotly_umatrix_clusters=plotly_umatrix_clusters,perps_umatrix_clusters=perps_umatrix_clusters)
if(rs$error) stop(rs$msg)
plotted_umatrix<-rs$data$umatrix
plotted_umatrix_with_clusters<-rs$data$umatrix_with_clusters
file_name<-paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_umatrix')
if(exists('publicacion')&&publicacion){
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
}else{
  if(plotly_umatrix_clusters){
    htmlwidgets::saveWidget(plotted_umatrix_with_clusters,paste0(file_name,'_with_clustering.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = T)
  }else{
    ggsave(paste0(file_name,'_with_clustering.png'),plot=plotted_umatrix_with_clusters, width = 20, height = 20)
  }
  if(plotly_umatrix){
    htmlwidgets::saveWidget(plotted_umatrix,paste0(file_name,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = T)
  }else{
    ggsave(paste0(file_name,'.png'),plot=plotted_umatrix, width = 20, height = 20)
  }
  
}

#---- histogram som cluster variables ------
if(!exists('plot_histogram_som_clusters')) source('functions_som/plot_histogram_som_clusters.R')
rs<-plot_histogram_som_clusters(wtdata=wtdata,som_cluster=som_cluster,wtdata_bmu=wtdata_bmu,som_selected_rows=som_selected_rows,som_selected_columns=som_selected_columns,date_time_name='date_time')
if(rs$error) stop(rs$msg)
hp<-rs$data$plot
ggsave(filename =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_histogram_clusters.png'),width = 40,height = 30,plot = hp)
#---------------------------------------- Heatmaps per variable ------------------------------
base_path<-paste0(getwd(),'/',tmpfolder,'/var_heatmaps')
ifelse(!dir.exists(base_path),dir.create(base_path), FALSE)
publicacion<-F
if(publicacion){
  plotly<-F
  legend<-F
  big_fonts<-T
  axis_tittle<-F
}else{
  plotly<-F
  legend<-T
  big_fonts<-F
  axis_tittle<-T
}
if(!exists('plot_heatmap_variable')) source('functions_som/plot_heatmap_variable.R')
rs<-plot_heatmap_variable(wtdata=wtdata,wtdata_bmu=wtdata_bmu,som_pts=som_pts,som_selected_columns=som_selected_columns,model_params=model_params,plotly=plotly,legend=legend,big_fonts=big_fonts,axis_tittle=axis_tittle)
if(rs$error) stop(rs$msg)
lds_ids<-rs$data$lds_ids
plots<-rs$data$plot

ggsave(paste0(base_path,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_heatmap_variables.png'),plot=plots, width = 20, height = 20)

# for(i in 1:length(lds_ids)){
#   for(c in 1:length(som_selected_columns)){
#     file_name<-paste0(wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_',lds_ids[i],'_',som_selected_columns[c])
#     position<-(i*length(som_selected_columns))-length(som_selected_columns)+c
#     if(plotly){
#       htmlwidgets::saveWidget(plots[[position]],paste0(base_path,'/',file_name,'.html'),libdir = paste0(base_path,'/lib'),selfcontained = F)
#     }else if(publicacion){
#       ggsave(paste0(base_path,'/',file_name,'.eps'), device=cairo_ps,plot=plots[[position]], width = 12, height = 12)
#     }else{
#       ggsave(paste0(base_path,'/',file_name,'.png'),plot=plots[[position]], width = 20, height = 20)
#     }
#   }
# }

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
model_params<-tmp_env$model_params
rm(tmp_env)
gc(verbose = F)

publicacion<-F

library(ggplot2)
library(plotly)
library(htmlwidgets)
library(reshape2)



nclusters<-5
if(!exists("create_bmu_clustering")) source("functions_som/create_bmu_clustering.R")
rs<-create_bmu_clustering(nclusters,wtdata_bmu,wtdata$ld_id[som_selected_rows],som_pts)
if(rs$error) stop(rs$msg)
clustering_result<-rs$data
clusters_xy<-rs$data$clusters_xy
centroids<-rs$data$centroids
########################## TEST auto clustering ##############################
# results_metrics<-data.frame(nclusters=as.numeric(),ld_id=as.numeric(),davbould=numeric(),sse=numeric(),si_bt0=numeric(),si_lt0=numeric(),si_eq0=numeric())
# for(c in 2:10){
#   rs<-create_map_clustering(c,wtdata_bmu,wtdata$ld_id[som_selected_rows],som_pts)
#   if(rs$error) stop(rs$msg)
#   clustering_result<-rs$data
#   calcule_metrics<-clustering_result[[1,'calcule_metrics']]
#   for(i in 1:nrow(clustering_result)){
#     rs<-calcule_metrics(clusters = clustering_result[[i,'clusters']],matrix_xy = clustering_result[[i,'active_bmus_xy_scaled']])
#     if(rs$error) stop(rs$msg)
#     results_metrics<-rbind(results_metrics,data.frame(nclusters=c,ld_id=clustering_result[[i,'ld_id']],davbould=rs$data$davbould,sse=rs$data$sse,si_bt0=rs$data$si$bt0,si_lt0=rs$data$si$lt0,si_eq0=rs$data$si$eq0))
#   }
# }
# 
# 
# results_metrics_avg<-results_metrics[,c('nclusters','davbould','sse','si_bt0','si_lt0','si_eq0')] %>% group_by(nclusters) %>% summarise_all(mean,na.rm=T)
# 
# results_metrics_avg$davbould_norm<-(results_metrics_avg$davbould-min(results_metrics_avg$davbould))/(max(results_metrics_avg$davbould)-min(results_metrics_avg$davbould))
# results_metrics_avg$sse_norm<-(results_metrics_avg$sse-min(results_metrics_avg$sse))/(max(results_metrics_avg$sse)-min(results_metrics_avg$sse))
# 
# qp<-melt(results_metrics_avg,id='nclusters')
# #p<-ggplot(qp,aes(x=nclusters,y=value,group=variable,color=variable))+geom_line(data=function(x) x[x$variable %in% c('davbould_norm','sse_norm'),],size=2)+theme_bw()+theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank(),text = element_text(size=35),plot.margin=unit(c(0,0,0,0),"mm"))
# p<-ggplot(qp,aes(x=nclusters,y=value,group=variable,color=variable))+geom_line(data=function(x) x[x$variable %in% c('davbould_norm','sse_norm'),],size=2)+theme_bw()+theme(text = element_text(size=35),plot.margin=unit(c(0,0,0,0),"mm"))
# ggsave(paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_clustering_metrics.png'),plot=p, width = 20, height = 12)
# 
# #Get best som size intersection between two scores
# qs<-data.frame(nclusters=seq(min(results_metrics_avg$nclusters),max(results_metrics_avg$nclusters),0.5))
# qs$davbould_norm_s<-predict(loess(results_metrics_avg$davbould_norm~results_metrics_avg$nclusters),newdata = qs$nclusters)
# qs$sse_norm_s<-predict(loess(results_metrics_avg$sse_norm~results_metrics_avg$nclusters),newdata = qs$nclusters)
# intersection<-qs$nclusters[as.logical(abs(diff(qs$davbould_norm_s < qs$sse_norm_s)))]
# best_nclusters<-ceiling(intersection)[2]
################### TEST auto clustering #########################

#if(!is.data.frame(centroids)) centroids<-do.call("rbind",centroids)
#clusters_xy<-list(ld_id=unlist(clustering_result[,'ld_id']),clusters_machines=clustering_result[,'clusters'])
#tmp_x<-as.numeric(sapply(1:nrow(clustering_result),function(i) clustering_result[[i,'cluster_centroids']][,1]))
#tmp_y<-as.numeric(sapply(1:nrow(clustering_result),function(i) clustering_result[[i,'cluster_centroids']][,2]))

#centroids<-data.frame(ld_id=unlist(lapply(unlist(clustering_result[,'ld_id']),function(ld) rep(ld,nclusters))),x=tmp_x,y=tmp_y,cluster=rep(1:nclusters,nrow(clustering_result)))
#centroids$ld_id<-as.factor(centroids$ld_id)

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
rs<-plot_bmu_turbines(num_registers_threshold=1,model_params=model_params,som_pts=som_pts,wtdata=wtdata[som_selected_rows,],wtdata_bmu=wtdata_bmu,clusters_xy=clusters_xy,plotly=F,legend=F,big_fonts=F,axis_tittle=F,grid_format=T)
if(rs$error) stop(rs$msg)
lds_ids<-rs$data$lds_ids
plots<-rs$data$plots

ggsave(paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_activeBMUs.png'),plot=plots, width = 20, height = 20) 
# for(i in 1:length(lds_ids)){
#     file_name<-paste0(wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_',lds_ids[i],'_activeBMUs')
#     if(plotly){
#       htmlwidgets::saveWidget(plots[[i]],paste0(getwd(),'/',tmpfolder,'/',file_name,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)
#     }else if(publicacion){
#       ggsave(paste0(getwd(),'/',tmpfolder,'/',file_name,'.eps'), device=cairo_ps,plot=plots[[i]], width = 12, height = 12)
#     }else{
#       ggsave(paste0(getwd(),'/',tmpfolder,'/',file_name,'.png'),plot=plots[[i]], width = 20, height = 20)
#     }
# }

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
if(!exists("create_turbines_groups")) source("functions_som/create_turbines_groups.R")
rs<-create_turbines_groups(wp_id=wp_id,array_id_ot=array_id_ot,distance_matrix=distance_matrix,fault=fault,number_of_groups=c(3,4,5),verbose=F,db_config=db_config)
if(rs$error) stop(rs$msg)
groups<-rs$data$groups
print(xtable(groups), type = "html",file =paste0(getwd(),'/',tmpfolder,'/',wp_code,'_',fault,'_',seconds_to_aggregate,'_dim',dim,'_table_groups.html'))
