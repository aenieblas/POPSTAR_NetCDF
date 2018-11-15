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
  
  return(metadata)
}

