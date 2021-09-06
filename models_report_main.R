#!/usr/bin/env Rscript
# This script is in charged of doing the report of the models of the wind turbines (wt) reported in the table 1_cast_config in
# the SmartCast DB:
#
# 1. Read the 1_cast_config table AND extract the rows that have a model 
# (creation_date_end is not empty and cast_enable is 1)
# 2. Make the wt_queries
# 3. Call report_template.Rmd given the results data by means of the enviroment
# 4. Write result in /var/rstudio/integrationv0/windfarms/park_code/ld_id/reports/YYYY-MM-DD-hh-mm_fault.html
# Run from command line writting logs and errors on *.Rout file:
# R CMD BATCH --no-save --no-restore models_report_main.R
# with args:
# R CMD BATCH --no-save --no-restore '--args park_report=TRUE' models_report_main.R  #For park report
# -------------------#

# Environment Cleanup
while(sink.number() > 0) sink()
to_remove<-ls()[(!ls() %in% c("debug_mode","table_cast_config","table_filter_config","type","template_file","db_config_data","db_config_realtime","hardcoded_cast_interval"))]
if(exists("debug_mode")&&debug_mode&&length(to_remove)>0){
    rm(list=to_remove)
    rm(to_remove)
}

# Initial definitions
Sys.setenv(TZ='UTC')
TZ='UTC'
#hardcoded_cast_interval <- NULL
#hardcoded_cast_interval <- list(ini=1448987925,end=1451579925)
if(!exists("test_mode")||(exists("test_mode")&&!is.logical(test_mode))) test_mode <- FALSE # If true, the cast will be done for all data since the next day to the last training date.
if(!exists("debug_mode")||(exists("debug_mode")&&!is.logical(debug_mode))) debug_mode <- FALSE # If true, messages will be displayed directly to console and not in log file.
if(!exists("table_cast_config")||(exists("table_cast_config")&&!is.character(table_cast_config))) table_cast_config <- '1_cast_config_compatible'  # cast config table
if(!exists("table_filter_config")||(exists("table_filter_config")&&!is.character(table_filter_config)))  table_filter_config <- '1_filter_config'
if(!exists("type")||(exists("type")&&!is.character(type))) type <- 'som'  # cast config table
if(!exists("template_file")||(exists("template_file")&&!is.character(template_file))) template_file <- 'models_report_template.Rmd'  # cast config table

# Data base configuration
if(!exists("db_config_data")||(exists("db_config_data")&&!is.data.frame(db_config_data)))
    db_config_data<- data.frame(user='user',
                                password='password',
                                dbname='yourHistoricalBD',
                                host='yourHost',
                                port=3306)

if(!exists("db_config_realtime")||(exists("db_config_realtime")&&!is.data.frame(db_config_realtime)))
    db_config_realtime<- data.frame(user='user',
                                    password='password',
                                    dbname='smartcast_DB',
                                    host='yourHost',
                                    port=3306)

if(!(Sys.info()["nodename"] %in% c("smartbbdd"))) db_config_data$host <- '127.0.0.1' #By default localhost.
if(!(Sys.info()["nodename"] %in% c("smartbbdd"))) db_config_realtime$host <- '127.0.0.1' #By default localhost.

setwd(".") #asumimos que debajo esta functions/ etc.
iam <- "models_report_main_park.R"

#Dependencia basica
if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) 
        return(list(error=TRUE,data=NULL,
                    msg=paste0("\n",iam,": Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
}

# Sources
libraries<-c('plyr','dplyr','rmarkdown')
sources<-paste0("functions_common/", 
                c('db_query.R',
                  'close_protocol.R',
                  'update_table_report.R'))
dep<-dependencyLoader(c(libraries,sources))

if(dep$error) stop(iam,":on call dependencyLoader\n",dep$msg)
rm(dep)

if(!debug_mode)
    sink("models_report_main.log", append = TRUE)

cat(paste("\n\n|------------------- INI",iam,"-----------------------|\n",
          as.POSIXct(Sys.time(),tz=TZ,origin="1970-01-01")),"UTC\n\n")

# 1. Read the 1_cast_config table AND extract the rows that have a model 
# (creation_date_end is not empty and cast_enable is 1)
query <- paste0("SELECT * FROM ",table_cast_config," WHERE creation_date_end IS NOT NULL AND creation_error = 0 AND `type`='",type,"' AND report_enable=1 ORDER BY id")

rs<-db_query(query=query,db_config=db_config_data)
if(rs$error)  {
    output_msg <- paste0(iam,": on call db_query\n",rs$msg)
    close_protocol(output_msg, iam, debug_mode)
    stop(output_msg)
}

# 2.Make the wt_queries
wt_queries <- dplyr::select(rs$data, id,wp_id,wp_code,ld_id,ld_code,freq_dat_med_min,seconds_to_aggregate,fault,array_id_walm,array_ot,type,filter,power_condition,include_variables,exclude_variables,artificial_variables,target_name,creation_wtdata_date_ini,creation_wtdata_date_end,path = creation_model_path)
nqueries <- nrow(wt_queries)
if(nqueries == 0) {
    output_msg <- paste0(iam, ": There is not an entry to report\n")
    close_protocol(output_msg, iam, debug_mode)
    stop(output_msg)
}

model_ids<-unique(wt_queries$id)
for(model_id in model_ids){
    report_env <- new.env()
    report_env$wt_query<-wt_queries[wt_queries$id==model_id,]
    sink(stderr())
    cat(paste0("\nModel id:",model_id))  
    sink()
    path_report <- gsub(".xz", "\\1", report_env$wt_query$path[1]) # remove xz
    path_report <- gsub(".RData", ".html", path_report)
    path_report <- gsub("models", "reports", path_report)
    path_report <- paste0(getwd(), "/",path_report)
    report_env$wp_code<-report_env$wt_query$wp_code[1]
    report_env$fault<-report_env$wt_query$fault[1]
    report_env$parent_directory<-getwd()
    report_ok <- try(render(input = template_file,
                            output_format = "html_document",
                            output_file = path_report,
                            encoding = "UTF-8",
                            envir = report_env))
    if( inherits(report_ok, "try-error") ) {
      return(list(error=TRUE,data=NULL,msg=paste0("\n\n",iam,":on Error rendering report for model_id (",model_id,") at ", path_report,"\n\t")))
      cat("\n\n",iam,":on Error rendering report for for model_id (",model_id,") at ", path_report,"\n\t")
      #update_table_report(table_cast_report, turbine, error = TRUE, db_config = db_config)
    }
    rm(report_env)
    invisible(gc(verbose = F))
}
rm(list=ls())
gc()
while(sink.number() > 0) sink()
