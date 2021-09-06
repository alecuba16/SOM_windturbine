# Brain4: Som
brain4<-function(currentTimestampUTC, m = NULL, m_un = NULL,parallel=FALSE,
                 table_filter_config="1_filter_config",
                 table_cast_park_dic="1_cast_park_table_dic",
                 table_artificial_config="1_artificial_config",
                 date_time_name="date_time",
                 horizon_list = 1, # In days
                 model_params=list(type='som',dim=50,rlen=10,alpha<-c(0.05,0.01),sizeRuleOfTheThumb=T,hex=F),
                 parallel_mode=T,
                 compute_metrics=F,
                 verbose=debug_mode,
                 db_config,
                 ld_id = m$ld_id,
                 ld_code = m$ld_code,
                 wp_code = m$wp_code,
                 wp_id = m$wp_id,
                 array_id_walm = m$array_id_walm,
                 array_ot = m$array_ot,
                 fault = m$fault,
                 type = m$type,
                 target_name=m$target_name,
                 filter = m$filter,
                 seconds_to_aggregate=m$seconds_to_aggregate,
                 freq_dat_med_min=m$freq_dat_med_min,
                 include_variables=m$include_variables,
                 exclude_variables=m$exclude_variables,
                 artificial_variables=m$artificial_variables,
                 power_condition = m$power_condition,
                 unix_timestamp_ini = m$creation_wtdata_date_ini,
                 unix_timestamp_end = m$creation_wtdata_date_end,
                 creation_trn_percent = m$creation_trn_percent,
                 creation_model_path = m$creation_model_path,
                 creation_log_path = m$creation_log_path,
                 additional_model_params = m$additional_model_params) {
  iam=match.call()[[1]]
  Sys.setenv(TZ='UTC')
  TZ='UTC'
  imagePath='images/'
  
  #Dependencia basica
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  if(!exists("createFile.R")){
    if(!file.exists('functions_common/createFile.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/createFile.R")));
    source('functions_common/createFile.R')
  }
  
  if(!exists("dirname2")){
    if(!file.exists('functions_common/dirname2.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dirname2.R")));
    source('functions_common/dirname2.R')
  }
  
  #Get pathname
  d<-dirname2(creation_log_path[1])
  if(d$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call createFile\n",d$msg)))
  d<-d$data
  
  
  # Sources
  libraries<-c('kohonen','RMySQL')
  if(parallel==TRUE&&.Platform$OS.type != "windows") { # domc only linux-unix
    libraries<-c(libraries,'doMC')
  }
  
  sources_common<-paste0("functions_common/",c('formatter_get_tableinfo.R','get_time_mark.R','load_wtdata.R','close_protocol.R','filter_custom.R','filter_custom.R','filter_check_data_range.R'))
  sources_som<-paste0("functions_som/",c('create_som_model.R'))
  
  dep<-dependencyLoader(c(libraries,sources_common,sources_som))
  if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  # Parallel setup
  if(parallel==TRUE&&.Platform$OS.type != "windows") { # domc only linux-unix
    registerDoMC(cores = detectCores())
  }
  
  #Add filter type to fault
  if(!is.null(filter)&&!is.na(filter)&&filter!="") fault<-paste0(fault,"_",gsub(",", "", filter))
  
  # Log taskes
  if(!debug_mode){
    log_path<-if(length(unique(ld_id))>1) paste0(d,paste0(unique(wp_code),collapse = '_'),"/park_model/logs/",fault[1],"_model.log") else paste0(d,paste0(unique(wp_code),collapse = '_'),"/",ld_id,"/logs/",fault[1],"_model.log")
    r<-createFile(log_path,append=TRUE)
    if(r$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call createFile\n",r$msg)))
    sink(log_path)
  }else{
    log_path <- NULL
  }
  
  if(parallel==TRUE&&.Platform$OS.type != "windows") { # prints here to be inserted on the log.
    cat("\n Parallel mode on\n")
  } 
  
  cat(paste("\n------------------- INI MODEL -----------------------\n",
            as.POSIXct(currentTimestampUTC,tz=TZ,origin="1970-01-01"),
            " ld_id(",ld_id,") wp_code(",wp_code,")\n",fault,
            " id_walm(",array_id_walm,")\n"))
  
  n_horizons <- length(horizon_list)
  
  ## Record elements
  results <- vector("list", 1)
  time_mark <- get_time_mark(currentTimestampUTC)
  war_rec <- character(1*n_horizons)
  count_war <- 0
  file_pattern<- if(length(ld_id)>0) paste(paste0(unique(wp_code),collapse = '_'),'park_model',fault[1],type[1],sep = "_") else paste(ld_code,ld_id[1],fault[1],type[1],sep = "_")
  imageZipFile<-paste0(imagePath,time_mark,"_",file_pattern,"_wsp.zip") #to store image files in zip
  names(results)[1] <- file_pattern
  cat("\n\nSTARTING MODELLING PROCESS FOR: ", file_pattern,"\n")
  
  # Record element
  result_h <- vector("list", n_horizons)
  # Number of models and list to record models and confusion matrix
  n_models <- 1
  fit_list <- vector("list", n_models)
  rmse_list <- vector("list", n_models)
  statistics_train_list <- vector("list", n_models)
  confidence_bands_train_list <- vector("list", n_models)
  cat("\nReading data...")
  
  ini <- as.POSIXct(unix_timestamp_ini, tz = "UTC", origin = "1970-01-01")
  end <- as.POSIXct(unix_timestamp_end, tz = "UTC", origin = "1970-01-01")
  cat("\nStart time:\t", as.character(ini), "\nEnd time:\t", as.character(end), "\n")
  
  #Initialize include variables
  tmp_include_variables<-''
  if(length(include_variables[1])>0 && nchar(include_variables[1])>2) tmp_include_variables<-include_variables[1]
  
  #Add selected alarms for image
  to_include<-c(target_name[1],"alarm","alarm_block_code","alarm_all","alarm_all_block_code","ot","ot_all_block_code","ot_all","ot_all_block_code")
  for(include in to_include){
    if(length(tmp_include_variables[1])>0 && nchar(tmp_include_variables[1])>2 && !any(grepl(pattern = paste0('^',include,'$|^',include,',|,',include,'$|,',include,','),x = tmp_include_variables)))
      tmp_include_variables<-paste0(tmp_include_variables[1],',',include)
  }
  
  #Additional params
  if(!is.null(additional_model_params)) eval(parse(text = additional_model_params[1]))
  if(length(tmp_include_variables[1])>0 && nchar(tmp_include_variables[1])>2 && exists('separate_power_levels',inherits = F) && !is.null(separate_power_levels)){
    #Get power var
    rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=m$wp_id[1],db_config=db_config)
    if(rs$error) {
      output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
      close_protocol(output_msg, iam, debug_mode)
      return(list(error=TRUE,data=NULL,msg=output_msg))
    }
    power_name<-rs$data$power_variable_name
    if(!any(grepl(pattern = paste0('^',power_name,'$|^',power_name,',|,',power_name,'$|,',power_name,','),x = tmp_include_variables)))  tmp_include_variables<-paste0(tmp_include_variables,',',power_name)
  }
  
  ## Load data
  cat("\nLoading data... ")
  # Current wt
  #Define aggregation 86400 -> aggregate per day
  seconds_offset <- 0
  power_condition <- '' #Always predict without filter power
  rs  <-  load_wtdata(wt_query=m,
                      date_time_name=date_time_name,
                      include_variables = rep(tmp_include_variables,nrow(m)),
                      table_cast_park_dic=table_cast_park_dic,
                      table_filter_config=table_filter_config,
                      table_artificial_config=table_artificial_config,
                      filter_exclude=paste(target_name[1],date_time_name,"ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                      update_filter_ranges=TRUE,
                      db_config=db_config)
  if(rs$error) {
    output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=TRUE,data=NULL,msg=output_msg))
  }
  wtdata <- rs$data$wtdata
  outliers<- rs$data$outliers
  wtdata0 <- rs$data$wtdata0
  rm(rs)
  
  #save filter range
  rs<-filter_check_data_range(wp_code=wp_code,table_cast_park_dic=table_cast_park_dic,db_config=db_config)
  if(rs$error){
    output_msg <- paste0("\n",iam,":on call filter_check_data_range\n\t",rs$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=TRUE,data=NULL,msg=output_msg))
  }
  
  if(rs$warning) cat(paste0("\n",iam,":on call filter_check_data_range\n\t",rs$msg))
  filter_range_data<-rs$data
  #End target_name filter
  
  # Unhealthy wt
  if(!is.null(m_un)) {
    rs_un  <- load_wtdata(wt_query=m_un,
                          include_variables=tmp_include_variables,
                          date_time_name=date_time_name,
                          target_name=target_name,
                          table_cast_park_dic=table_cast_park_dic,
                          table_filter_config=table_filter_config,
                          table_artificial_config=table_artificial_config,
                          filter_exclude=paste(target_name,date_time_name,"ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                          update_filter_ranges=TRUE,
                          db_config=db_config)
    if(rs_un$error) {
      cat("\n",iam,":on call load_wtdata\n",rs_un$msg)
      cat("\nThe model will be built without fusion options.")
      undata0 <- NULL
    } else {
      undata0 <- rs_un$data$wtdata
    }
    rm(rs_un)
  } else undata0 <- NULL
  
  cat(" Done.\n")
  
  
  #if(length(unique(wtdata[,target_name[1]]))<=1){
  #  cat(paste0("No ",target_name[1]," into data, unable to produce a model the target_name variable (",target_name[1],") on wtdata is a constant..."))
  #  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Unable to create a model: wtdata[,",target_name[1],"] is constant")));
  #}
  
  #Create empty model file
  #Get pathname
  d<-dirname2(creation_model_path[1])
  if(d$error) { 
    output_msg <- paste0("\n",iam,":on call dirname2\n\t",d$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=TRUE,data=NULL,msg=output_msg))
  }
  d<-d$data
  modelPath<- if(length(ld_id)>0) paste0(d,paste0(unique(wp_code),collapse = '_'),"/park_model/",time_mark,"_",fault[1],"_",type[1],".RData.xz") else paste0(d,wp_code,"/",ld_id,"/models/",time_mark,"_",fault,"_",type,".RData.xz")
  r<-createFile(modelPath,append=FALSE)
  if(r$error) {
    output_msg <- paste0("\n",iam,":on call createFile\n",r$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=TRUE,data=NULL,msg=output_msg))
  }
  
  #------------------------------ Horizon LOOP ------------------------------
  # Loop to iterate over horizon_list
  if(exists('separate_power_levels',inherits = F)&&!is.null(separate_power_levels)){
    min_pwr<-floor(min(wtdata[,power_name],na.rm = T))
    max_pwr<-ceiling(max(wtdata[,power_name],na.rm = T))
    horizon_list<-NULL
    separate_power_levels_df<-data.frame(min=as.numeric(),max=as.numeric())
    for(i in 1:length(separate_power_levels)){
      if(i==1){
        horizon_list<-paste0(min_pwr,'_',separate_power_levels[i],'kw')
        separate_power_levels_df<-rbind(separate_power_levels_df,data.frame(min=min_pwr,max=separate_power_levels[i]))
      }else{
        horizon_list<-c(horizon_list,paste0(separate_power_levels[i-1],'_',separate_power_levels[i],'kw'))
        separate_power_levels_df<-rbind(separate_power_levels_df,data.frame(min=separate_power_levels[i-1],max=separate_power_levels[i]))
      }
    }
    horizon_list<-c(horizon_list,paste0(separate_power_levels[i],'_',max_pwr,'kw'))
    separate_power_levels_df<-rbind(separate_power_levels_df,data.frame(min=separate_power_levels[i],max=max_pwr))
    n_horizons<-length(horizon_list)
    result_h <- vector("list", n_horizons)
  }
  
  for( k in 1:n_horizons ) {
    horizon <- horizon_list[k]
    names(result_h)[k] <- as.character(horizon)
    cat("\n\nHORIZON: ", horizon,"\n")
    numTimestamps<-sum(sapply(wtdata, inherits,"Date"))
    numTimestamps2<-sum(sapply(wtdata, inherits,"POSIXct"))
    if(numTimestamps>1||numTimestamps2>1){
      count_war <- count_war + 1
      if(numTimestamps>1)
        war_rec[count_war] <- paste("Multiple timestamp at ", file_pattern, "_", horizon, sep = "")
      else
        war_rec[count_war] <- paste("no timestamp at ", file_pattern, "_", horizon, sep = "")
      warning("WARNING: ", war_rec[count_war])
    }
    
    date_time0 <- wtdata[,date_time_name] # Date time backup
    assign(paste(date_time_name, 0, sep=''), date_time0) #Assign date_time
    
    # sapply(wtdata[wtdata$alarm==1,date_time_name],1,function(dt) )
    # end pre-alarm
    
    # Format wtdata and build unhealthy vector in alarma vector
    ###########
    
    if(!is.null(m_un) && !is.null(undata0)) {
      # Adding unhealthy data from undata
      wtdata <- rbind(wtdata, undata0) # Final order: original + health + unhealth
    } else {
      undata <- NULL
    }
    
    trainData<-wtdata
    #If defined
    if(exists('separate_power_levels_df',inherits = F)&&!is.null(separate_power_levels_df)){
      #Cut by power level
      trainData<-trainData[(trainData[,power_name]>=separate_power_levels_df$min[k])&(trainData[,power_name]<=separate_power_levels_df$max[k]),]
      #Remove Power variable if not in original include
      if(!any(grepl(pattern = paste0('^',power_name,'$|^',power_name,',|,',power_name,'$|,',power_name,','),x = include_variables[1])))  trainData[,power_name]<-NULL
    }
    
    #Save alarm,alarm_all,ot... and block code because can contain NA
    to_backup<-c('alarm','alarm_block_code','alarm_all','alarm_all_block_code','ot','ot_all','ot_block_code','ot_all_block_code')
    for(bk in to_backup){
      if(bk %in% names(wtdata)){
        assign(bk,wtdata[,c(date_time_name,bk)])  #backup bk in a variable with its name for futures analysis
        wtdata<-wtdata[ , !(names(wtdata) %in% bk)] # Drop bk column
      }else{
        assign(bk,NA)
      }
    }
    
    set.seed(1)
    #Train
    
    
    #TODO!!! CAMBIAR ESTA CHAPUZA necesito pasar parametros desde 1_cast_config!!!
    #if(!is.null(additional_model_params)) model_params<-eval(parse(text = 'list(type=\'gsom\',spreadFactor=0.8,hex=T)'))
    if(!is.null(additional_model_params)) eval(parse(text = additional_model_params[1]))
    
    exclude_variables<-c('ld_id',date_time_name,'alarm','alarm_block_code','alarm_all_block_code','ot','ot_all','ot_block_code','ot_all_block_code')
    #Create new model
    #model_params<-list(type='gsom',spreadFactor=0.8,hex=T) #Use gsom model
    #model_params<-list(type='som',dim=10,rlen=10,alpha<-c(0.05,0.01),sizeRuleOfTheThumb=T,hex=F) #Use som model
    parallel_mode<-T
    log_file<-NULL #For parallel mode logs
    verbose<-T #Print msg
    normalize<-T #Normalize data (necessary for faster converge)
    
    rs<-create_som_model(wtdata=trainData[,!(colnames(trainData) %in% exclude_variables)],model_params=model_params,parallel_mode=parallel_mode,log_file=NULL,verbose=T,standarize = T)
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
    
    if(compute_metrics){
      #ATTENTION SLOW!!
      rs<-som_result$get_metrics(som_codes = som_codes,som_pts=som_pts,som_distances =som_distances,wtdata = wtdata[som_selected_rows,colnames(wtdata) %in% som_selected_columns],verbose=T,stat=stat)
      if(rs$error) stop(rs$msg)
      qe<-rs$data$qe
      te<-rs$data$te
    }else{
      qe<-NULL
      te<-NULL
    }
    
    # Recording fit and cm lists

    #som_model<-rs$som_model
    result_h[[k]]$model_params <- model_params
    result_h[[k]]$model <- som_model
    result_h[[k]]$stat <- stat
    result_h[[k]]$metrics <-list(te=te,qe=qe)
    cat(" Done.\n")
    
    # Save image
    imagePathFinal<-paste0(imagePath,time_mark,"_",file_pattern,"_",horizon, "d_wsp.RData")
    r<-createFile(imagePathFinal,append=FALSE)
    if(r$error) {
      output_msg <- paste0("\n",iam,":on call createFile\n",r$msg)
      close_protocol(output_msg, iam, debug_mode)
      return(list(error=TRUE,data=NULL,msg=output_msg))
    }
    # save.image(file = imagePath) # NO sirve porque guarda una imagen del enviroment GlobalEnv
    # save(list = ls(all.names = TRUE), file = imagePath, envir = environment()) # Equivalente a salvar imagen pero especificando el enviroment

    savelist<-c("wtdata0", "wtdata", "undata0", "undata","trainData","m", "file_pattern","m_un","date_time_name")
    to_savelist<-c('wtdata_bmu','stat','dim','som_distances','som_center_neuron_position','som_codes','som_pts','som_selected_rows','som_selected_columns','filter_range_data','outliers','alarm','alarm_block_code','alarm_all','alarm_all_block_code','ot','ot_all','ot_block_code','ot_all_block_code')
    for(sv in to_savelist){
      if(exists(sv)) savelist<-c(savelist,sv); 
    }
    try(save(list = savelist,file = imagePathFinal, envir = environment() ) ) # No salva una imagen, solo las variables usadas en el reporte para salvar espacio
    try(myflags <- ifelse(file.exists(imageZipFile),"-u9Xjq","-9Xjq"))
    r<-try(zip(imageZipFile, files=imagePathFinal, flags = myflags, extras = "",zip = Sys.getenv("R_ZIPCMD", "zip")))
    if(inherits(r, "try-error")) {
      if(.Platform$OS.type=="windows")
        return(list(error=TRUE,data=NULL,msg=paste0(iam," please, install Rtools (zip) for windows or check there is zip command on console")))
      else
        return(list(error=TRUE,data=NULL,msg=paste0(iam," please, install Rtools (zip) for Unix/Mac check on console 'zip' command")))
      
    }
    try(if (file.exists(imagePathFinal)) file.remove(imagePathFinal))
  }
  results[[1]] <- result_h
  
  #Create empty model file
  #Get pathname
  if( any(is.na(creation_model_path)) || any(is.null(creation_model_path)) || any(creation_model_path == "") || any(creation_model_path == default_path) ) {
    d <- default_path
    if(length(ld_id)>1||(exists('park_model')&&park_model)){
      modelPath<- paste0(d,paste0(unique(wp_code),collapse = "_"),"/park_model/",time_mark,"_",unique(fault),"_",type[1],".RData.xz")
    }else{
      modelPath<- paste0(d,unique(wp_code),"/",ld_id,"/models/",time_mark,"_",fault,"_",type[1],".RData.xz")
    }
  } else {
    d<-dirname2(unique(creation_model_path))
    if(d$error) { 
      output_msg <- paste0("\n",iam,":on call dirname2\n\t",d$msg)
      close_protocol(output_msg, iam, debug_mode)
      return(list(error=TRUE,data=NULL,msg=output_msg))
    }
    d<-d$data
    modelPath<- paste0(d,time_mark,"_",fault,".RData.xz")
  } 
  # modelPath<- paste0(d,wp_code,"/",ld_id,"/models/",time_mark,"_",fault,".RData.xz")
  r<-createFile(modelPath,append=FALSE)
  if(r$error) {
    output_msg <- paste0("\n",iam,":on call createFile\n",r$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=TRUE,data=NULL,msg=output_msg))
  }
  
  save( "results", file = modelPath,compress = 'xz')
  
  output_msg <- paste0("\n",iam,":OK. END.\n")
  close_protocol(output_msg, iam, debug_mode)
  
  return(list(error=FALSE,data=list(modelPath=modelPath,logPath=log_path),msg=output_msg));
}
