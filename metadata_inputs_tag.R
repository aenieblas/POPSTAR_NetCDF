## metadata_inputs_tags.R
## Anne-Elise Nieblas, Sylvain Bonhommeau, Julien Barde
## 9/8/2018
## DESCRIPTION: This script creates a data frame to inform the write_global_attributes.R function embedded in Tag2NetCDF.R. Metadata follow 
# the formatting of the POPSTAR NetCDF Format Reference Manual, doi http://dx.doi.org/10.13155/34980, which itself follows Climate and Forcasting (CF)
# standards.
# " All fields should be human-readable, and should be of character type, not numeric, even if the information content is a number. POPSTAR recommends
# that all of these attributes be used and contain meaningful information unless there are technical reasons rendering this impossible. However, files 
# that do not at least contain the attributes listed as “mandatory” will not be considered POSPSTAR-compliant.
# 
# Global attributes can be thought of as conveying five kinds of information:
# • What: what are the data in this data file;
# • Where: the spatial coverage of the data;
# • When: the temporal coverage of the data;
# • Who: who produced the data;
# • How: how were the data produced and made available.
# The global attributes specification follows the recommendations of Unidata NetCDF Attribute Convention for Dataset Discovery, at:
  # http://www.unidata.ucar.edu/software/netcdf-java/formats/DataDiscoveryAttConvention.html"

# In POPSTAR, global attribute names are in lower-case letters (except “Convention”).
# 
# Whenever time information is given in the global attributes, it ought to be a string of the format: "YYYY-MM-DDThh:mm:ssZ" (i.e. year - month - day T hour : minute : second Z) 
# If higher resolution than seconds is needed, any number of decimal digits (“.s”) for the seconds is acceptable: "YYYY-MM-DDThh:mm:ss.sZ"
# 
# In any case, the time must be in UTC. A capital “T” separates the date and the hour information. The string must end with a capital “Z”, an old indication of UTC. These formats are two (of many) described by ISO8601. 
# Examples: 
# • 2005-10-24T08:00:00Z 
# • 2008-01-01T22:50:02.031Z "


## INPUTS:  outputs_SS3.Rdata : r4ss output of the ss3 model
##          run_spec_ss3_standard.csv  : user-defined specifications of each model and run.



####################################### WRITE GLOBAL ATTRIBUTES ############################################
# CORE METADATA ELEMENTS
# text search

options(stringsAsFactors=FALSE)
metadata<-NULL
metadata_inputs_tag <- function(tag_spec_path,run=1,tag){
  
  ## LOAD MODEL RUN SPECIFICATIONS FILE
  # metadata$run_spec=read.csv(paste(tag_data_dir,'run_spec.csv',sep=''),sep=',',header=FALSE,row.names=1)
  
  # run_spec=read.table('https://docs.google.com/spreadsheets/d/1okuWL2Fmj4BsRDY_22dtclyF8RQC7gPyAF3YO2gwGic/edit?usp=sharing',header=FALSE,row.names=1)
  # csv file with all global attributes for tags. each diff tag is diff column, i.e., 'run'
  tag_spec1=read.csv(tag_spec_path,header=FALSE,row.names=1)
  tag_spec2 <- lapply(tag_spec1, function(x){
    x[x == ""] <- NA
    return(x)
  })
  
  tag_spec<-as.data.frame(tag_spec2,stringsAsFactors = FALSE)
  row.names(tag_spec)<-row.names(tag_spec1)
  #################################### 'WHAT' : DATA DESCRIPTION #######################################
  ########################################### WHAT: MANDATORY ################################################
  # data_type: Type of data contained in the file.
  metadata$data_type              <- tag_spec['data_type',run]
  
  # format_version: File format version
  metadata$format_version         <- tag_spec['format_version',run]

  # date_update: File update or creation date (UTC). See note on time format below.
  metadata$date_update            <-tag_spec['date_update',run]

  # data_mode: Indicates if the file contains real-time, or delayed-mode data. valid inputs: "R" (Real-time data) | "A" (Adjusted data. Real-time data 
  # with adjusted values) | "D" (Delayed-mode data)
  metadata$data_mode              <-tag_spec['data_mode',run]
  
  # naming_authority: The “id” and “naming_authority” attributes are intended to provide a globally unique identification for each data file.
  # For POPSTAR data, use:    naming_authority=”POPSTAR” and id=file name (without .nc suffix), which is designed to be unique. 
  # id: 
  metadata$naming_authority        <-tag_spec['naming_authority',run]
  metadata$id                      <- tag_spec['id',run]
  
  ########################################## WHAT: RECOMMENDED ###############################################
  # ices_platform_code: Platform code assigned by ICES (International Council for the Exploration of the Sea)
  metadata$ices_platform_code      <-tag_spec['ices_platform_code',run]#<-----------------------?
  
  # source: Method of production of the original data.
  metadata$source                  <-tag_spec['source',run]#<------------------------?
  
  # history: Audit trail for modifications to the original data. It should contain a separate line for each modification, with each line beginning with
  # a timestamp, and including user name, modification name, and modification arguments. The time stamp should follow the format outlined in the note 
  # on time formats below.
  metadata$history                  <-tag_spec['history',run]#<-----------------------?
  
  # quality_index: Code value valid for the whole data file. valid inputs: “unknown quality” |  “excellent” (no known problems, regular quality checking)| 
  # “probably good” (occasional problems, validation phase) |  “extremely suspect” (frequent problems) 
  metadata$quality_index            <-tag_spec['quality_index',run]#e.g.
  
  # references:  Published or web-based references that describe the data or methods used to produce it. Include a reference to POPSTAR and a 
  # project-specific reference if appropriate.
  metadata$references               <- tag_spec['references',run]#<-----------------------?
  
  # comment: Miscellaneous information about the data or methods used to produce it. Any free-format text is appropriate.
  metadata$comment                  <- tag_spec['comment',run]#<-----------------------?
  
  # Conventions:  Name of the conventions followed by the data file.
  metadata$Conventions              <- tag_spec['Conventions',run]#<-----------------------?
  
  # netcdf_version: Netcdf version used for the data file.
  metadata$netcdf_version           <-tag_spec['netcdf_version',run]
  
  # Free-format text describing the data file. The display of these two attributes together should allow data discovery for a human reader. 
  # title: title of the data file. Use the file name if in doubt.
  # summary: a longer description of the data file. A paragraph of up to 100 words is appropriate.
  metadata$title                    <- tag_spec['title',run]
  metadata$summary                  <- tag_spec['summary',run]

  # abstract: Paragraph describing the data file: type of data contained, how it was created, who collected it, what instruments were used, what data 
  # formatting convention was used,etc.
  metadata$abstract                 <-tag_spec['abstract',run]

  # keywords: Comma separated list of key words and phrases.
  metadata$keywords                 <- tag_spec['keywords',run]
  
  # cdm_data_type: The “cdm_data_type” attribute gives the Unidata CDM (common data model) data type used by THREDDS. E.g. “Point”, “Trajectory”, “Station”, “Radial”, “Grid”, “Swath”.
  # More: http://www.unidata.ucar.edu/projects/THREDDS/CDM/CDM-TDS.htm
  metadata$cdm_data_type            <-tag_spec['cdm_data_type',run]#<------------------ Obsolète.(????)

    
  #################################### 'WHERE' : DATA SPATIAL COVERAGE #######################################
  ########################################### WHERE: RECOMMENDED ################################################
  # area:  Geographical coverage. Use vocabulary from SeaDataNet sea areas (C16).  http://vocab.nerc.ac.uk/collection/C16/current/accepted/
  metadata$area                      <- tag_spec['area',run]
  
  # geospatial_lat_min: Southernmost valid latitude, a value between -90 and 90 degrees. This is calculated from the valid latitudes in the file. Decimal degrees
  if(!is.na(tag_spec['geospatial_lat_min',run])){
    metadata$geospatial_lat_min        <- tag_spec['geospatial_lat_min',run]
  }else{
    metadata$geospatial_lat_min        <- min(tag$lat,na.rm=T)}
  
  # geospatial_lat_max: Sorthernmost valid  latitude, a value between -90 and 90 decimal degrees. This is calculated from the valid latitudes in the file.
  if(!is.na(tag_spec['geospatial_lat_max',run])){
    metadata$geospatial_lat_max        <- tag_spec['geospatial_lat_max',run]
  }else{
    metadata$geospatial_lat_max        <- max(tag$lat,na.rm=T)}
  
  # geospatial_lon_min: The westernmost valid longitude, a value between -180 and 180 degrees. This is calculated from the valid longitudes in the file.
  if(!is.na(tag_spec['geospatial_lon_min',run])){
    metadata$geospatial_lon_min        <- tag_spec['geospatial_lon_min',run]
  }else{
    metadata$geospatial_lon_min        <- min(tag$lon,na.rm=T)}
  
  # geospatial_lon_max: The easternmost valid longitude, a value between -180 and 180 decimal degrees. This is calculated from the valid longitudes in the file.
  if(!is.na(tag_spec['geospatial_lon_max',run])){
    metadata$geospatial_lon_max        <- tag_spec['geospatial_lon_max',run]
  }else{
    metadata$geospatial_lon_max        <- max(tag$lon,na.rm=T)}
  
  # geospatial_vertical_min:  Minimum valid depth or pressure for measurements.  This is calculated from the valid depth or pressure in the file. 
  if(!is.na(tag_spec['geospatial_vertical_min',run])){
    metadata$geospatial_vertical_min   <- tag_spec['geospatial_vertical_min',run]
  }else{
    metadata$geospatial_vertical_min        <- abs(min(tag$depth_obs,na.rm=T))}
  
  # geospatial_vertical_max:  Maximum valid depth or pressure for measurements.  This is calculated from the valid depth or pressure in the file.
  if(!is.na(tag_spec['geospatial_vertical_max',run])){
    metadata$geospatial_vertical_max   <- tag_spec['geospatial_vertical_max',run]
  }else{
    metadata$geospatial_vertical_max        <- abs(max(tag$depth_obs,na.rm=T))}
  
  #################################### 'WHEN' : DATA TEMPORAL COVERAGE #######################################
  ########################################### WHEN: RECOMMENDED ################################################
  
  # time_coverage_start: Start date of the data in UTC. See note on time format below.
  if(!is.na(tag_spec['time_coverage_start',run])){
    metadata$time_coverage_start       <- tag_spec['time_coverage_start',run]
  }else{
    tt<-as.character(as.POSIXct(strptime(tag$time[1], "%d-%b-%Y %H:%M:%S"), tz="UTC"))
    rr<-gsub(' ','T',tt)
    rr<-paste0(rr,'Z')
    metadata$time_coverage_start        <- rr}
  
  # time_coverage_end: Final date of the data in UTC. See note on time format below.
  if(!is.na(tag_spec['time_coverage_end',run])){
    metadata$time_coverage_end         <- tag_spec['time_coverage_end',run]
  }else{
    tt<-as.character(as.POSIXct(strptime(tag$time[length(tag$time)], "%d-%b-%Y %H:%M:%S"), tz="UTC"))
    rr<-gsub(' ','T',tt)
    rr<-paste0(rr,'Z')
    metadata$time_coverage_end        <- rr}
  
  #################################### 'WHO' : DATA PRODUCERS #######################################
  ########################################### WHO: RECOMMENDED ################################################
  
  # institution: Preferably institution of the principal investigator.
  metadata$institution               <-tag_spec['institution',run]
  
  # institution_references: References to principal investigator institution, the place to find all information on the data file (web-based, i.e. give URLs).
  metadata$institution_references    <- tag_spec['institution_references',run]

  # sdn_edmo_code: SeaDataNet EDMO code of the institution. EDMO is the “European Directory of Marine Organisations”.http://seadatanet.maris2.nl/edmo/
  metadata$sdn_edmo_code             <- tag_spec['sdn_edmo_code',run]
  
  # contact: Contact person’s e-mail.
  metadata$contact                   <- tag_spec['contact',run]
  
  # author: List of relevant persons involved in the creation of the data file (comma separated).
  metadata$author                    <- tag_spec['author',run]
  
  # data_assembly_center: Data Assembly Center (DAC) in charge of this data file. The data_assembly_center are listed in reference table 4.
  metadata$data_assembly_center      <- tag_spec['data_assembly_center',run]#<---------------------------------------??
  
  # principal_investigator:  Name of the principal investigator in charge of the tag project.
  metadata$principal_investigator    <- tag_spec['principal_investigator',run]
 
  # principal_investigator_email: Principal investigator’s email address.
  metadata$principal_investigator_email<- tag_spec['principal_investigator_email',run]
  
  # project_name: Name of the project which operates the tag that performed the measurements.
  metadata$project_name                <- tag_spec['project_name',run]
  
  # deployment_code: Deployment code. It is unique among POPSTAR deployments. This code may be used as the local code in catalogues such as SeaDataNet Common Data Index (CDI).
  metadata$deployment_code             <- tag_spec['deployment_code',run]#e.g.<-----------------------???
  
  # deployment_label: The deployment label, a free text to describe the deployment. 
  metadata$deployment_label            <- tag_spec['deployment_label',run]#<------------------------???

  #################################### 'HOW' : DATA PRODUCTION METHODS #######################################
  ########################################### HOW: RECOMMENDED ################################################
  
  # distribution_statement: Statement describing data distribution policy. POPSTAR has adopted the CLIVAR data policy, which explicitly calls for free and unrestricted data exchange. Details at: 
  # http://www.clivar.org/data/data_policy.php 
  metadata$distribution_statement       <- tag_spec['distribution_statement',run]
  
  # doi: List of Data Object Identifiers (DOI) related to this data file (blank separated).
  metadata$doi                          <- tag_spec['doi',run]
  
  # citation: The citation to be used in publications using the data file.
  metadata$citation                     <- tag_spec['citation',run]
  
  # update_interval: Update interval for the file, valid options: “hourly” | “daily” | “yearly” | “void”.  Use “void” for delayed-mode or archive data that do not need continuous updating.
  metadata$update_interval              <- tag_spec['update_interval',run]
  
  # qc_manual: Contains the name of the manual that describes the quality control procedure. As of now, there is no separate QC manual, so the user’s manual is the appropriate reference.
  metadata$qc_manual                    <- tag_spec['qc_manual',run]
  
  
  #### RECOMMENDATIONS FOLLOWING WILDLIFE DATA ####
  metadata$tag_type                    <- tag_spec['tag_type',run]
  metadata$deploy_id                   <- tag_spec['deploy_id',run]
  metadata$ptt                         <- tag_spec['ptt',run]
  metadata$instrument                  <- tag_spec['instrument',run]
  metadata$sw                          <- tag_spec['sw',run]
  metadata$percent_decoded             <- tag_spec['percent_decoded',run]
  metadata$passes                      <- tag_spec['passes',run]
  metadata$percent_argos_loc           <- tag_spec['percent_argos_loc',run]
  metadata$msg_per_pass                <- tag_spec['msg_per_pass',run]
  metadata$ds                          <- tag_spec['ds',run]
  metadata$di                          <- tag_spec['di',run]
  metadata$min_power                   <- tag_spec['min_power',run]
  metadata$avg_power                   <- tag_spec['avg_power',run]
  metadata$max_power                   <- tag_spec['max_power',run]
  metadata$min_interval                <- tag_spec['qc_manual',run]
  metadata$earliest_xmit_time          <- tag_spec['earliest_xmit_time',run]
  metadata$latest_xmit_time            <- tag_spec['latest_xmit_time',run]
  metadata$xmit_days                   <- tag_spec['xmit_days',run]
  metadata$earliest_data_time          <- tag_spec['earliest_data_time',run]
  metadata$latest_data_time            <- tag_spec['latest_data_time',run]
  metadata$data_days                   <- tag_spec['data_days',run]
  metadata$release_date                <- tag_spec['release_date',run]
  metadata$release_type                <- tag_spec['release_type',run]
  metadata$deploy_date                 <- tag_spec['deploy_date',run]
  
  
  # sst_reference_data_set	NOAA OI SST V2 High Resolution
  # bathymetry_reference_data_set	ETOPO1-Bedrock
  
  
  
  # # ## MODEL AND SPECIES IDENTIFIERS
  # fao_sp      <- unlist(strsplit(model,'_'))[1]
  # run_num       <- unlist(strsplit(model,'_'))
  # # assessment_year <- unlist(strsplit(model,'_'))[2]
  # run_num       <- paste(run_num[3:length(run_num)],collapse='_')## ADJUSTED FOR ALWAYS HAVING ASSESSMENT YEAR AFTER SP CODE
  # # i_run       <- which(run_spec$FAO_code==fao_sp & run_spec$Run==run_num)
  # i_run         <- which(run_spec["FAO_code",]==fao_sp & run_spec["Run",]==run_num)
  # # sp_longname <- paste(run_spec$common_name[i_run],', ',run_spec$SpeciesName[i_run],sep='')
  # sp_longname   <- paste(run_spec["common_name",i_run],', ',run_spec["scientific_name",i_run],sep='')
  # assessment_id <- paste(run_spec["model_rfmo_id",i_run],'_', run_spec["FAO_code",i_run],'_FAOFishingRegion', paste(run_spec["FAO_region_code",i_run],collapse='_',sep=''),'_',run_spec["catch_start_year",i_run],"_",run_spec["catch_end_year",i_run],"_", run_spec["modeler_last_name",i_run],sep='')
  # contacts      <- strsplit(as.character(run_spec["contacts_and_roles",i_run]),';')
  # i_modeler     <- grep('originator',strsplit(as.character(run_spec["contacts_and_roles",i_run]),';'))
  # modeler       <- unlist(strsplit(unlist(contacts)[i_modeler],'='))[[2]]
  # ############################################ NETCDF DATA FILE METADATA #####################################################################
  # ############## HIGHLY RECOMMENDED BY NetCDF Attribute Convention for Dataset Discovery #########
  # metadata                           <-  NULL
  # # metadata$title                     <-  paste("SS3 model outputs for ",sp_longname," (",run_spec["FAO_code",i_run],") of ",run_spec["model_rfmo_id",i_run]," management authority in the FAO Fishing Region(s) ",paste(run_spec["FAO_region_code",i_run],collapse='_',sep='')," for the years ",run_spec["catch_start_year",i_run],"-",run_spec["catch_end_year",i_run],", modeled by ", modeler," for the run ",run_num," (",model,")",sep="")
  # metadata$title                     <-  paste("SS",outputs_SS3$SS_versionNumeric," outputs for the ",run_spec["model_rfmo_id",i_run]," ",run_spec["FAO_code",i_run]," ",run_spec["assess_purpose",i_run]," (",run_spec["catch_start_year",i_run],"-",run_spec["catch_end_year",i_run],") run id: ",model,sep="")
  # key_parameters                     <-  run_spec[49:dim(run_spec)[1],]
  # key_parameter_values               <-  key_parameters[which(!is.na(run_spec[49:dim(run_spec)[1],i_run])),i_run]
  # key_parameters_names               <-  row.names(key_parameters)
  # these_key_parameter_names          <-  key_parameters_names[which(!is.na(run_spec[49:dim(run_spec)[1],i_run]))]
  # metadata$summary                   <-  paste("This is a file of selected SS",outputs_SS3$SS_versionNumeric," outputs that have been read by the r4ss R package outputs (using the SS_output function) and then converted to NetCDF. This model was used originally developed by ",modeler," for the ",run_spec["catch_end_year",i_run],' ',run_spec["model_rfmo_id",i_run],' ',run_spec["assess_purpose",i_run],' of ',sp_longname,', with parameter(s): ',paste(these_key_parameter_names,'=',key_parameter_values,sep=" ",collapse=', '),'. It was later adapted to the IOTC_SS3 VRE (https://services.d4science.org/group/iotc_ss3/iotc_ss3) by running the ss324.R script (https://goo.gl/o4dtgR), with the inputs associated with the run specified in the run_spec_ss3_standard.csv file (https://goo.gl/QMNqd4).',sep="")
  # metadata$keywords                  <-  paste(paste(sp_longname,", FAO Region(s): ",paste(run_spec["FAO_region_code",i_run],collapse='_',sep=''),", modeler: ", modeler,", ",run_spec["keywords",i_run],", ",model,", ",sep=""),paste(variables,sep=" ",collapse = ', '),sep="")
  # 
  # ############## RECOMMENDED BY NetCDF Attribute Convention for Dataset Discovery #################
  # metadata$id                        <-  as.character(model)
  # metadata$naming_authority          <-  as.character(run_spec["naming_authority",i_run])#run_spec$naming_authority
  # # metadata$keywords_vocabulary <- "RDA FDIWG vocabulary : http://"                          #<--------------- AEN: valid here?
  # metadata$cdm_data_type             <-  as.character(run_spec["cdm_data_type",i_run] ) #run_spec$cdm_data_type  # "https://goo.gl/o4dtgR" #REPLACE WITH THE COMMAND LINE OR THE URL OF THE R Code which can be used to reproduce the data"<-outputs2netcdf or the ss324 write*() script?
  # metadata$history                   <-  as.character(run_spec["history",i_run]) #run_spec$history        #"This dataset has been produced on the BlueBridge-funded IOTC_VRE () by running the ss324.R script (https://goo.gl/o4dtgR), with the inputs associated with the run specified in the run_spec_ss3.csv file (https://goo.gl/KJjM4K). Inputs and parameterizations of the model have been provided by the IOTC and its consultants, unless otherwise specified."
  # metadata$comment                   <-  as.character(run_spec["comment",i_run])#run_spec$comment        #"Input data provided by the IOTC and its consultants." # TO BE ADAPTED
  # metadata$date_created              <-  as.character(Sys.time())
  # 
  # #### DATA CREATOR SEARCH ####
  # metadata$creator_name              <- as.character(run_spec["creator_name",i_run])#run_spec$creator_name    #"NIEBLAS.A.E. BARDE.J" # You can put whatever you want we want use this OGC metadata
  # metadata$creator_url               <- as.character(run_spec["creator_url",i_run])#run_spec$creator_url     #"https://i-marine.d4science.org" # You can put whatever you want we want use this OGC metadata
  # metadata$creator_email             <- as.character(run_spec["creator_email",i_run])#run_spec$creator_email   #"anne.elise.nieblas@gmail.com julien.barde@ird.fr"
  # metadata$institution               <- as.character(run_spec["institution",i_run])#run_spec$institution     #"iotc@iotc.org"
  # metadata$project                   <- as.character(run_spec["project",i_run])#run_spec$project         #"BlueBridge H2020 project"
  # # metadata$processing_level         <- "Level 4 since this is a FORECAST ?"
  # metadata$acknowledgement           <- as.character(run_spec["acknowledgement",i_run])#run_spec$acknowledgement #"This work has received funding from the European Union's Horizon 2020 research and innovation programme under the BlueBRIDGE project (Grant agreement No 675680)."
  # 
  # #### SPATIAL EXTENT OF RFMO ####   
  # sp_resolution =1 
  # # rmfo_poly=as.character(run_spec$rfmo_poly)#"POLYGON((30 -50, 140 -50, 140 30, 30 30,30 -50))"
  # rmfo_poly=as.character(run_spec["rfmo_poly",i_run])#"POLYGON((30 -50, 140 -50, 140 30, 30 30,30 -50))"
  # box <- readWKT(rmfo_poly)
  # class(box)
  # bbox <- box@bbox
  # longitudeVector = seq(from=bbox['x','min']+(sp_resolution/2), to=bbox['x','max']-(sp_resolution/2), by=sp_resolution)
  # latitudeVector = seq(from=bbox['y','min']+(sp_resolution/2), to=bbox['y','max']-(sp_resolution/2), by=sp_resolution)
  # 
  # metadata$geospatial_bounds         <- rmfo_poly  # IS SUFFICIENT TO FILL  geospatial_lat_min | geospatial_lat_max | geospatial_lon_min | geospatial_lon_max
  # metadata$geospatial_lat_min        <- bbox['y','min'] #
  # metadata$geospatial_lat_max        <- bbox['y','max']
  # metadata$geospatial_lon_min        <- bbox['x','min']
  # metadata$geospatial_lon_max        <- bbox['x','max']
  # # metadata$geospatial_vertical_min <-"0"                               
  # # metadata$geospatial_vertical_max <-"500"                              
  # 
  # #### TEMPORAL EXTENT OF INPUTS ####
  # metadata$time_coverage_start       <- as.character(outputs_SS3$startyr  )
  # metadata$time_coverage_end         <- as.character(outputs_SS3$endyr    )
  # metadata$time_coverage_duration    <- paste(outputs_SS3$endyr-outputs_SS3$startyr,sep='')
  # metadata$time_coverage_resolution  <- paste(outputs_SS3$seasdurations,' year(s)',sep='')
  # 
  # # metadata$standard_name_vocabulary <- "Stock Assessment Standards vocabulary : http://" #"RDA FDIWG vocabulary : http://"
  # metadata$licence                   <- as.character(run_spec["licence",i_run])#run_spec$licence #"CC licence to be clarified with IOTC / ICCAT / IRD / NOAA depending on the use case / VRE"
  # 
  # ############## SUGGESTED BY NetCDF Attribute Convention for Dataset Discovery #################
  # #### MODELER AND ADDITIONAL CONTRIBUTOR DETAILS ####
  # metadata$contacts_and_roles        <- as.character(run_spec["contacts_and_roles",i_run])
  # metadata$publisher_name            <- as.character(run_spec["publisher_name",i_run])#"ird@ird.fr"
  # metadata$publisher_url             <- as.character(run_spec["publisher_url",i_run])#"http://www.ird.fr"
  # metadata$publisher_email           <- as.character(run_spec["publisher_email",i_run])#"ird@ird.fr"
  # metadata$date_modified             <- as.character(Sys.time())
  # metadata$date_issued               <- as.character(Sys.time()) 
  # 
  # #### SPATIAL UNITS AND RESOLUTIONS ####
  # metadata$geospatial_lat_units      <- "degrees_north" 
  # metadata$geospatial_lat_resolution <- sp_resolution 
  # metadata$geospatial_lon_units      <- "degrees_east" 
  # metadata$geospatial_lon_resolution <- sp_resolution 
  # # metadata$geospatial_vertical_units      <- "use the same as the variable attribute"
  # # metadata$geospatial_vertical_resolution <- sp_resolution 
  # # metadata$geospatial_vertical_positive   <- "up or down ?"
  # 
  # ############## ADDITIONAL TO the NetCDF Attribute Convention for Dataset Discovery #################
  # metadata$source                    <-  as.character(run_spec["source",i_run]) #"IOTC database"
  # 
  # 
  # ########################################## STOCK ASSESSMENT MODEL METADATA ##################################################################
  # ##################### WRITE STANDARDIZED* SS3 MODEL METADATA ###################
  # ## *STANDARDIZATION IS BASED ON THE RECOMMENDED AND SUGGESTED METADATA NAMING CONVENTIONS DEVELOPED IN NIEBLAS ET AL. (LINK)
  # 
  # ################################################ MODEL RUN ################################################
  # ############## RECOMMENDED BY the Stock Assessment Model Convention for Dataset Discovery #################
  # metadata$model_files_url                          <- as.character(run_spec["model_files_url",i_run]) #run_spec$model_files_url  # "https://goo.gl/o4dtgR" #REPLACE WITH THE COMMAND LINE OR THE URL OF THE R Code which can be used to reproduce the data"<-outputs2netcdf or the ss324 write*() script?
  # metadata$model_history                            <- as.character(run_spec["model_history",i_run]) #run_spec$model_history        #"This dataset has been produced on the BlueBridge-funded IOTC_VRE () by running the ss324.R script (https://goo.gl/o4dtgR), with the inputs associated with the run specified in the run_spec_ss3.csv file (https://goo.gl/KJjM4K). Inputs and parameterizations of the model have been provided by the IOTC and its consultants, unless otherwise specified."
  # metadata$model_comment                            <- as.character(run_spec["model_comment",i_run] )#run_spec$model_comment        #"Input data provided by the IOTC and its consultants." # TO BE ADAPTED
  # 
  # 
  # metadata$management_authority_id                  <- as.character(run_spec["model_rfmo_id",i_run] )#run_spec$rfmo 
  # metadata$management_authority_full                <- as.character(run_spec["model_rfmo_full",i_run]) #run_spec$rfmo 
  # metadata$model_date_created                       <- as.character(outputs_SS3$repfiletime)
  # metadata$model_version_short                      <- as.character(outputs_SS3$SS_versionshort)
  # metadata$model_version_os                         <- paste(Sys.info()[['sysname']],Sys.info()[['release']],Sys.info()[['version']],sep=' ')
  # metadata$assessment_year                          <- as.character(run_spec["assessment_year",i_run]) #run_spec$assessment_year
  # metadata$n_estimated_parameters                   <- as.character(outputs_SS3$N_estimated_parameters)
  # 
  # #### DATA CREATOR SEARCH ####
  # # metadata$modeler_name                             <- as.character(run_spec["contributor_modeler",i_run]) #run_spec$contributor_modeler
  # metadata$modeler_url                              <- as.character(run_spec["modeler_url",i_run]) #run_spec$modeler_url     #"https://i-marine.d4science.org" # You can put whatever you want we want use this OGC metadata
  # metadata$modeler_email                            <- as.character(run_spec["modeler_email",i_run]) #run_spec$modeler_email   #"anne.elise.nieblas@gmail.com julien.barde@ird.fr"
  # 
  # metadata$geospatial_bounds_multiarea              <- as.character(run_spec["multiarea_poly",i_run]) #run_spec$multiarea_poly  # IS SUFFICIENT TO FILL  geospatial_lat_min | geospatial_lat_max | geospatial_lon_min | geospatial_lon_max
  # 
  # metadata$scientific_name                          <- as.character(run_spec["scientific_name",i_run]) #run_spec$scientific_name
  # metadata$common_name                              <- as.character(run_spec["common_name",i_run]) #run_spec$common_name
  # metadata$FAO_species_code                         <- as.character(run_spec["FAO_code",i_run])
  # metadata$FAO_region_code                         <- as.character(run_spec["FAO_region_code",i_run])
  # 
  # ############## SUGGESTED BY the Stock Assessment Model Convention for Dataset Discovery #################
  # metadata$input_files_used                         <- as.character(outputs_SS3$Files_used)
  # metadata$model_version_detailed                   <- as.character(outputs_SS3$SS_version)
  # metadata$model_version_numeric                    <- as.character(outputs_SS3$SS_versionNumeric)
  # metadata$assess_start_time                         <- as.character(outputs_SS3$StartTime)
  # metadata$assess_run_time                           <- as.character(outputs_SS3$RunTime)
  # metadata$n_assess_warnings                         <- as.character(outputs_SS3$Nwarnings)
  # metadata$assess_warnings                           <- paste(outputs_SS3$warnings,collapse=', ',sep='')
  # 
  # metadata$modeler_institution_id                   <- as.character(run_spec["modeler_institution_id",i_run]) #run_spec$modeler_institution_id       #IOTC
  # metadata$modeler_institution_full                 <- as.character(run_spec["modeler_institution_full",i_run]) #run_spec$modeler_institution       #Indian Ocean Tuna Commission
  # metadata$assess_purpose                        <- as.character(run_spec["assess_purpose",i_run]) #run_spec$modeler_project       #"BlueBridge H2020 project"
  # # metadata$model_processing_level                  <- "Level 4 since this is a FORECAST ?"
  # metadata$modeler_acknowledgement                 <- as.character(run_spec["modeler_acknowledgement",i_run]) #run_spec$modeler_acknowledgement #"This work has received funding from the European Union's Horizon 2020 research and innovation programme under the BlueBRIDGE project (Grant agreement No 675680)."
  # 
  # metadata$region_name                              <- as.character(run_spec["region_name",i_run]) #run_spec$region_name
  # metadata$area_name                                <- as.character(run_spec["area_name",i_run]) #run_spec$area_name
  # metadata$FAO_subregion_code                       <- as.character(run_spec["FAO_subregion_code",i_run]) #run_spec$area_id
  # metadata$assessment_id                            <- paste(run_spec["model_rfmo_id",i_run],'_', run_spec["FAO_code",i_run],'_FAOFishingRegion', paste(run_spec["FAO_region_code",i_run],collapse='_',sep=''),'_',run_spec["catch_start_year",i_run],"_",run_spec["catch_end_year",i_run],"_", run_spec["modeler_last_name",i_run],sep='')
  # metadata$kingdom                                  <- as.character(run_spec["kingdom",i_run]) #run_spec$kingdom
  # metadata$phylum                                   <- as.character(run_spec["phylum",i_run]) #run_spec$phylum
  # metadata$taxonomic_class                          <- as.character(run_spec["taxonomic_class",i_run]) #run_spec$taxonomic_class
  # metadata$taxonomic_order                          <- as.character(run_spec["taxonomic_order",i_run]) #run_spec$taxonomic_order
  # metadata$taxonomic_family                         <- as.character(run_spec["taxonomic_family",i_run]) #run_spec$taxonomic_family
  # metadata$genus                                    <- unlist(strsplit(as.character(run_spec["scientific_name",i_run])," "))[[1]]
  # metadata$species                                  <- unlist(strsplit(as.character(run_spec["scientific_name",i_run])," "))[[2]]
  # metadata$common_name2                             <- as.character(run_spec["common_name2",i_run])
  # metadata$n_areas                                  <- as.character(outputs_SS3$nareas)
  # metadata$n_seasons                                <- as.character(outputs_SS3$nseasons)
  # metadata$season_fraction                          <- as.character(outputs_SS3$seasfracs)
  # metadata$season_duration                          <- as.character(outputs_SS3$seasdurations)
  # metadata$n_forecast_years                         <-as.character( outputs_SS3$nforecastyears)
  # 
  # 
  # ####################### REFERENCE POINTS #############################
  # ############## RECOMMENDED BY the Stock Assessment Model Convention for Dataset Discovery #################
  # metadata$SPR_target                               <- as.character(outputs_SS3$sprtarg)
  # 
  # ############## SUGGESTED BY the Stock Assessment Model Convention for Dataset Discovery #################
  # metadata$B_target                                 <- as.character(outputs_SS3$btarg)
  # metadata$min_B_threshold                          <- as.character(outputs_SS3$minbthresh)
  # 
  # ####################### MODEL INPUTS/PARAMETERIZATIONS #############################
  # ############## RECOMMENDED BY the Stock Assessment Model Convention for Dataset Discovery #################
  # metadata$abundance_index_id                       <- paste(outputs_SS3$fleet_ID,collapse=', ',sep='')
  # metadata$abundance_index_names                    <- paste(outputs_SS3$FleetNames,collapse=', ',sep='')
  # metadata$abundance_index_area                     <- paste(outputs_SS3$fleet_area,collapse=', ',sep='')
  # metadata$catch_units                              <- paste(outputs_SS3$catch_units,collapse=', ',sep='')
  # metadata$abundance_index_error                    <- paste(outputs_SS3$survey_error,collapse=', ',sep='')
  # metadata$is_fisheries_dependent_abundance_index   <- paste(outputs_SS3$IsFishFleet,collapse=', ',sep='')
  # metadata$n_sexes                                  <- as.character(outputs_SS3$nsexes)
  # metadata$n_growth_patterns                        <- as.character(outputs_SS3$ngpatterns)
  # metadata$length_bins                              <-paste( outputs_SS3$lbins,collapse=', ',sep='')
  # metadata$size_bins                                <- as.character(outputs_SS3$sizebinlist)
  # metadata$age_bins                                 <- as.character(outputs_SS3$agebins)
  # metadata$max_age                                  <- as.character(outputs_SS3$accuage)
  # metadata$spawning_season                          <- as.character(outputs_SS3$spawnseas)
  # metadata$birth_season                             <- as.character(outputs_SS3$birthseas)
  # metadata$main_morphs                              <- as.character(outputs_SS3$mainmorphs   )                   ## WHAT DO THESE MEAN?
  # metadata$mean_weight                              <- as.character(outputs_SS3$mnwgt)
  # if(outputs_SS3$SRRtype==1){srrtype='1: null'}
  # if(outputs_SS3$SRRtype==2){srrtype='2: Ricker (2 parameters)'}
  # if(outputs_SS3$SRRtype==3){srrtype='3: standard Beverton-Holt (2 parameters)'}
  # if(outputs_SS3$SRRtype==4){srrtype='4: ignore steepness and no bias adjustment. Use this in conjunction with very low emphasis on recruitment deviations to get CAGEAN-like unconstrained recruitment estimates. (2 parameters, but only uses the first one.)'}
  # if(outputs_SS3$SRRtype==5){srrtype='5: Hockey stick (3 parameters) for ln(R0), fraction of virgin SSB at which inflection occurs, and the R level at SSB=0.0.'}
  # if(outputs_SS3$SRRtype==6){srrtype='6: Beverton-Holt with flat-top beyond Bzero (2 parameters)'}
  # if(outputs_SS3$SRRtype==7){srrtype='7: Survivorship function (3 parameters). Suitable for sharks and low fecundity stocks to assure recruits are <= pop production'}
  # metadata$SRR_type                                 <- as.character(srrtype       )
  # metadata$sigma_R                                  <- as.character(outputs_SS3$sigma_R_in    )                   ##std.dev. of log recruitment;
  # 
  # ############## SUGGESTED BY the Stock Assessment Model Convention for Dataset Discovery #################
  # metadata$catch_error                              <- paste(outputs_SS3$catch_error,collapse=', ',sep='')
  # metadata$abundance_index_units                    <- paste(outputs_SS3$survey_units,collapse=', ',sep='')
  # metadata$n_fisheries_dependent_abundance_index    <- as.character(outputs_SS3$nfishfleets)
  # metadata$n_abundance_indices                      <- as.character(outputs_SS3$nfleets)
  # metadata$length_bin_method                        <-as.character( outputs_SS3$Lbin_method)
  # metadata$n_length_bins                            <- as.character(outputs_SS3$nlbins)
  # metadata$length_bins_pop                          <- paste(outputs_SS3$lbinspop,collapse=', ',sep='')
  # metadata$n_length_bins_pop                        <- as.character(outputs_SS3$nlbinspop)
  # metadata$n_age_bins                               <- as.character(outputs_SS3$nagebins)
  # metadata$MG_param_dev_details                     <- as.character(outputs_SS3$MGParm_dev_details)
  # if(outputs_SS3$FecType==1){fectyp="1: eggs=Wt*(a+b*Wt)"}
  # if(outputs_SS3$FecType==2){fectyp="2: eggs=a*L^b"}
  # if(outputs_SS3$FecType==3){fectyp="3: eggs=a*Wt^b"}
  # if(outputs_SS3$FecType==4){fectyp="4: eggs=a+b*L"}
  # if(outputs_SS3$FecType==5){fectyp="5: eggs=a+b*W"}
  # metadata$fecundity_type                           <- fectyp                        
  # metadata$fecundity_param_1_name                   <- as.character(outputs_SS3$FecPar1name         )             
  # metadata$fecundity_param_2_name                   <- as.character(outputs_SS3$FecPar2name       )          
  # metadata$fecundity_param_1                        <- as.character(outputs_SS3$FecPar1      )                   
  # metadata$fecundity_param_2                        <- as.character(outputs_SS3$FecPar2      )                   
  # metadata$spawning_output_units                    <- as.character(outputs_SS3$SpawnOutputUnits)
  # metadata$seasonal_effects                         <- as.character(outputs_SS3$Seas_Effects)
  # metadata$growth_CV_type                           <- as.character(outputs_SS3$growthCVtype)
  # metadata$wt_at_age_switch                         <- as.character(outputs_SS3$wtatage_switch)
  # metadata$growth_varies                            <- as.character(outputs_SS3$growthvaries)
  # if(outputs_SS3$depletion_method==0){dep_meth="0: skip"}                                            
  # if(outputs_SS3$depletion_method==1){dep_meth="1: rel X*B0"}
  # if(outputs_SS3$depletion_method==2){dep_meth="2: rel X*Bmsy"}
  # if(outputs_SS3$depletion_method==3){dep_meth="3: rel X*B_styr"}
  # metadata$depletion_method                         <- as.character(dep_meth)                  
  # metadata$depletion_basis                          <- as.character(outputs_SS3$depletion_basis)
  # metadata$discard_type                             <- as.character(outputs_SS3$discard_type)
  # metadata$discard_DF                               <-as.character( outputs_SS3$DF_discard)
  # metadata$mean_weight_DF                           <- as.character(outputs_SS3$DF_mnwgt)
  # if(outputs_SS3$F_method==1){F_meth="1: Popeâ€™s"}
  # if(outputs_SS3$F_method==2){F_meth="2: Continuous F as parameters"}
  # if(outputs_SS3$F_method==3){F_meth="3: Hybrid"}
  # metadata$F_method                                 <- as.character(F_meth)
  # metadata$Kobe_MSY_basis                           <- as.character(outputs_SS3$Kobe_MSY_basis)
  # metadata$F_report_basis                           <- as.character(outputs_SS3$F_report_basis)
  # metadata$B_ratio_denominator                      <- as.character(outputs_SS3$B_ratio_denominator)
  # metadata$Kobe_warnings                            <-as.character( outputs_SS3$Kobe_warn)
  # metadata$tag_first_period                         <- as.character(outputs_SS3$tagfirstperiod)
  # metadata$tag_max_period                           <- as.character(outputs_SS3$tagaccumperiod)
  # if(!is.null(outputs_SS3$tagtorecap)){ metadata$tag_to_recapture                         <- as.character(outputs_SS3$tagtorecap)}
  # metadata$n_age_error_definitions                  <- as.character(outputs_SS3$N_ageerror_defs)
  # metadata$composition_data_exists                  <- as.character(outputs_SS3$comp_data_exists)
  # if(is.null(outputs_SS3$Pstar_sigma)==FALSE){
  #   metadata$Pstar_sigma                              <- as.character(outputs_SS3$Pstar_sigma )                   ## P* standard deviation
  # }
  # if(is.null(outputs_SS3$Pstart_signma)==FALSE){
  #   metadata$Pstar_sigma                              <- as.character(outputs_SS3$Pstart_signma )                   ## P* standard deviation
  # }
  # metadata$max_gradient_component                   <- as.character(outputs_SS3$maximum_gradient_component)
  # metadata$SB_zero                                  <- as.character(outputs_SS3$SBzero)
  # metadata$current_depletion                        <- as.character(outputs_SS3$current_depletion)
  # metadata$last_years_SPR                           <- as.character(outputs_SS3$last_years_SPR)
  # metadata$SPR_ratio_label                          <- as.character(outputs_SS3$SPRratioLabel  )                  ## WHAT DOES THIS MEAN?
  # metadata$last_years_SPR_ratio                     <- as.character(outputs_SS3$last_years_SPRratio)
  # 
  # ## KEY PARAMETERS
  # metadata$steepness                                <- as.character(run_spec["steepness",i_run]) #run_spec[iparam,grep('steep',colnames(run_spec))]
  # metadata$lambda                                   <- as.character(run_spec["lambda",i_run])#run_spec[iparam,grep('lambda',colnames(run_spec))]
  # metadata$natural_mortality                        <- as.character(run_spec["natural_mortality",i_run])
  # metadata$tagging_program                          <- as.character(run_spec["tagging_program",i_run])
  # metadata$tag_mixing_period                        <- as.character(run_spec["tag_mixing_period",i_run])
  # metadata$tag_release_mortality_rate               <- as.character(run_spec["tag_release_mortality_rate",i_run])
  # metadata$forecast_level                           <- as.character(run_spec["forecast_level",i_run])
  # metadata$point_estimate_parameters                <- as.character(run_spec["point_estimate_parameters",i_run])
  # 
  
  
  # ################### EXPLORATION OF THE METADATA OF THE OUTPUTS_SS3.RDATA FILE ###################
  # #### FUNCTION TO READ THE DIMNAMES OF A LARGE LIST
  # dimnames2 <- function(x) {
  #   if (is.vector(x)) {list(names(x))
  #   } else {d <- dimnames(x)
  #     if (is.null(d)){rep(list(NULL), length(dim(x)))
  #     } else {d
  #       }}}
  # 
  # #### READ THE "METADATA" EMBEDDED IN THE OUTPUTS_S33.RDATA FILE ####
  # for(i in 1:length(dimnames2(outputs_SS3)[[1]])){
  # dd=dimnames2(eval(parse(text=paste('outputs_SS3[',i,']',sep=''))))
  # DD=eval(parse(text=(paste('outputs_SS3$',dd[[1]],sep=''))))
  # Dd=dimnames2(eval(parse(text=(paste('outputs_SS3$',dd[[1]],sep='')))))
  # 
  # if(is.null(dim(DD))==TRUE & length(DD)>0 & is.null(Dd[[1]])==TRUE){
  #    print(dd)
  #   # eval(parse(text=paste('tt=paste(as.character(outputs_SS3$',dd[[1]],'),sep=" ",collapse = " ")',sep='')))
  #   # eval(parse(text=paste('metadata$',dd[[1]],'<- tt',sep='')))
  #   # eval(parse(text=paste('metadata$',dd[[1]],'<- outputs_SS3$',dd[[1]],sep='')))
  # }
  # }
  # ###################################################################################################
  
  return(metadata)
}

# ##### RELOADS UPDATED ss3.24.R to INFRASTRUCTURE ####
# overwrite<-T #SET TO "TRUE" IF THE FILES ALREADY ON THE WORKSPACE SHOULD BE OVERWRITTEN
# outputs_WS <- paste("/Home",username,"Workspace/VRE Folders/IOTC_SS3/ss3_public/Rscripts/",sep="/")
# listWS(outputs_WS) #GET THE LIST OF FILES AND FOLDERS IN ONE SUB-FOLDER
# metadata_outputs_SS3=paste(home_dir,'/Rscripts/metadata_inputs_ss324.R',sep='') # FILE WITH THE FUNCTION TO WRITE OGC 19115 metadata
# uploadWS(outputs_WS, metadata_outputs_SS3,overwrite)
# #####################################################