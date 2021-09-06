#!/usr/bin/env Rscript
# Todo , el log debe decir rows en vez de register y 23 variables of total of 66 vars...
# Poner en el rmarkdown los puntos outliers en rojo.
# Para ejecutar desde linea de comandos dejando log de warnings y errores en
# models_main.Rout:
# R CMD BATCH --no-save --no-restore models_main.R
# with args:
# R CMD BATCH --no-save --no-restore '--args parallel=TRUE' models_main.R
# Para ejecutar igual per imprimiendo warnings y errores a pantalla:
# Rscript models_main.R

#Parsear argumentos si es por script tipo etc
args=(commandArgs(TRUE))
if(length(args)>0){
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
# Reset sink() (sink define the file output for messages. It should start at 0)
while(sink.number() > 0) sink()

Sys.setenv(TZ='UTC')
TZ='UTC'
iam='models_main'
setwd(".") #asumimos que debajo esta functions/ etc.
#If not set debug mode manually....
if(!exists("debug_mode")||(exists("debug_mode")&&!is.logical(debug_mode))) debug_mode <- FALSE # If true, messages will be displayed directly to console and not in log file.
if(!exists("force_regenerate")||(exists("force_regenerate")&&!is.logical(force_regenerate))) force_regenerate <- FALSE # If true, will regenerate the model event if it exists.
if(!exists("parallel")||(exists("parallel")&&!is.logical(parallel))) parallel <- FALSE # true if want parallelism
if(!exists("table_cast_config")||(exists("table_cast_config")&&!is.character(table_cast_config))) table_cast_config <- '1_cast_config_compatible'  # cast config table
if(!exists("table_filter_config")||(exists("table_filter_config")&&!is.character(table_filter_config))) table_filter_config <- '1_filter_config'  # cast config table
if(!exists("table_cast_park_dic")||(exists("table_cast_park_dic")&&!is.character(table_cast_park_dic))) table_cast_park_dic <- '1_cast_park_table_dic'  # cast config dic table
if(!exists("type")||(exists("type")&&!is.character(type))) type <- 'som'  # cast config table
if(!exists("date_time_name")||(exists("date_time_name")&&!is.character(date_time_name))) date_time_name <- 'date_time'
if(!exists("parallel_mode")||(exists("parallel_mode")&&!is.logical(parallel_mode))) parallel_mode <- T

model_params<-list(type='gsom',spreadFactor=0.8,hex=T) #Use gsom model
#model_params<-list(type='som',dim=10,rlen=10,alpha<-c(0.05,0.01),sizeRuleOfTheThumb=T,hex=F) #Use som model


# DB data
if(!exists("db_config_data")||(exists("db_config_data")&&!is.data.frame(db_config_data)))
  db_config_data<- data.frame(user='user',
                              password='password',
                              dbname='yourHistoricalBD',
                              host='yourHost',#'127.0.0.1', #
                              port=3306)

if(!exists("db_config_realtime")||(exists("db_config_realtime")&&!is.data.frame(db_config_realtime)))
  db_config_realtime<- data.frame(user='user',
                                  password='password',
                                  dbname='smartcast_DB',
                                  host='yourHost',
                                  port=3306)

if(Sys.info()["nodename"] != "smartbbdd"){
  db_config_data$host='127.0.0.1'
  db_config_realtime$host='127.0.0.1'
}

default_path <- "windfarms/" # Default directory to store engines made by brain0 or brain1. The "/" at the end is obligatory.



daily_alarms_threshold <- 0 # Define el umbral para decidir si necesita fusi?n de datos (<) o no (>=)

if(!debug_mode)
  sink("models_main.log", append = TRUE)
t_ini<-as.POSIXct(Sys.time(),tz=TZ,origin="1970-01-01");
t_ini_madrid<-t_ini
attr(t_ini_madrid, "tzone") <- "Europe/Madrid"
cat(paste0("\n\n>>------------------- INI models_main ",t_ini," UTC (",t_ini_madrid," Madrid ) ----------------------->>\n"))

#Basic dependency
if(!exists("dependencyLoader")){
  if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg="Missing dependency function: functions_common/dependencyLoader.R"));
  source('functions_common/dependencyLoader.R')
}
dep<-dependencyLoader(c('RMySQL','parallel','doParallel','brain4.R',paste0("functions_common/",c('getModelsToBeCreated.R','db_query.R','ranking_by_id_walm.R','need_fusion.R','get_most_unhealthy.R','close_protocol.R'))))

if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0(iam,":on call dependencyLoader\n",dep$msg)))
brain<-brain4

if(force_regenerate) #Force generate the model even there are created models.
  r2<-db_query(query=paste0("UPDATE ",table_cast_config," SET creation_date_ini=NULL,creation_date_end=NULL,creation_model_path=\"windfarms/\",creation_log_path=\"windfarms/\",creation_error=0 WHERE `type`='",type,"' AND creation_enable=1;"),db_config=db_config_data);

models<-getModelsToBeCreated(table_cast_config,force_regenerate,type=type,db_config=db_config_data)

if(models$error) {
  output_msg <- models$msg
  close_protocol(output_msg, iam, debug_mode, TZ = "UTC")
  stop(output_msg);
}

if(nrow(models$data)<=0) {
  output_msg <- "\nNo models to be generated, check DB.\n"
  close_protocol(output_msg, iam, debug_mode, TZ = "UTC")
  stop(output_msg);
}

n_models<-nrow(models$data)
if(n_models>0){
  for (model_id in unique(models$data$id)) {#Old for loop before parallel
    currentTimestampUTC<-floor(as.numeric(as.POSIXlt(Sys.time(),tz="UTC")))
    #Actualizamos la tabla para marcar creandolo
    query <- paste0("UPDATE ",table_cast_config," SET creation_date_ini=FROM_UNIXTIME(",currentTimestampUTC,"),creation_error=0 WHERE id=",model_id)
    r2<-db_query(query=query,db_config=db_config_data)
    
    r<-try(brain4(currentTimestampUTC=currentTimestampUTC,m=models$data[models$data$id==model_id,],model_params=model_params,parallel_mode=parallel_mode,verbose=T,table_filter_config=table_filter_config,table_cast_park_dic=table_cast_park_dic,table_artificial_config=table_artificial_config,date_time_name=date_time_name,db_config=db_config_data))
    if(inherits(r, "try-error")) {
      while(!debug_mode && sink.number() > 1) sink()
      cat(r)
      query <- paste0("UPDATE ",table_cast_config," SET creation_error=1 WHERE id=",model_id)
      r2<-db_query(query=query,db_config=db_config_data)
      next()
    }
    ##### NO ERROR: Update cast_config table with OK: ####
    currentTimestampUTC<-floor(as.numeric(as.POSIXlt(Sys.time(),tz="UTC")))
    query <- paste0("UPDATE ",table_cast_config," SET creation_date_end=FROM_UNIXTIME(",currentTimestampUTC,"),creation_model_path='",r$data$modelPath,"',creation_log_path='",r$data$logPath,"' WHERE id=",model_id,";")
    # r2<-db_query(query=query,db_config=db_config_data)
    r2 <- lapply(query, db_query, db_config=db_config_data)
    #### 
  }   
  
}else{
  cat("No models to be generated, check DB")
}

output_msg <- "\nDONE.\n"
close_protocol(output_msg, iam, debug_mode, TZ = "UTC")
