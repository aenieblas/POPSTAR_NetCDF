# Tag2NetCDF.R
# Anne-Elise Nieblas 
# 14/8/2018

# DESCRIPTION: converts tag data output to a standardized NetCDF-CF format, following the formatting of the POPSTAR NetCDF Format Reference Manual, doi http://dx.doi.org/10.13155/34980 
# tag_type options: "WILDLIFE"
# tag_data_format options: "csv","nc","kmz",...
# polygon

## REQUIRES:
# tag_specifications.csv: describes the global attributes of the tag
# tag_variable_specifications.csv: describes variable attributes of the tag


## SST from ocean model. with depth

## AE questions: no dimensions listed for deployment_latitude, deployment_longitude,deployment_qc, end_mission_status, start_date_qc
## AE: should i have included position_uncertainty_1,2,3,etc as a string and then employed th 

################################## CREATE NETCDF VERSION 4 : ENRICHED TAG DATA OUTPUT TO NETCDF ######################################
Tag2NetCDF <- function(script_dir,
                       tag_name,
                       tag_points=NULL, # data frame with individual point coordinates
                       tag_polygons_1=NULL, # data frame with polygon coordinates
                       tag_polygons_2=NULL,
                       tag_polygons_3=NULL,
                       tag_data=NULL,
                       n_values_1=NA,# dim_def: number of possible values for each time step for the same parameter. only use if n_values>1
                       n_values_2=NA,# dim_def: number of possible values for each time step for the same parameter. only use if n_values>1
                       n_values_3=NA,# dim_def: number of possible values for each time step for the same parameter. only use if n_values>1
                       n_param=4,# dim_def: temperature, depth, sunrise, sunset
                       n_mission=1,# dim_def
                       n_sensor=NA,# dim_def
                       n_config_param=NA,# dim_def
                       n_derivation=NA,# dim_def
                       n_param_tech=NA,# dim_def
                       n_history=NA,# dim_def
                       date_time=14,# dim_def
                       tag_spec_column
){
  ##################################### REQUIRE LIBRARIES, SOURCE FUNCTIONS ###########################################
  library(pacman)
  p_load('plyr','ncdf4','rgeos','chron')
  
  ## STANDARDIZE THE COLUMN NAMES BETWEEN THE DIFFERENT INPUT DATA FRAMES
  
  
  # source(paste(home_dir,script_dir,'afilldim.R',sep=''))
  # source(paste(home_dir,script_dir,'afilldim_var.R',sep=''))
  # source(paste(home_dir,script_dir,'VARdim_array.R',sep=''))
  # source(paste(home_dir,script_dir,'VAR_array_fast.R',sep=''))
  source(paste0(script_dir,'metadata_inputs_tag.R'))
  source(paste0(script_dir,'varid_list_create.R'))
  source(paste0(script_dir,'write_global_attributes_tag.R'))
  source(paste0(script_dir,'write_variable_attributes_tag.R'))
  source(paste0(script_dir,'var_att_inputs_tag.R'))
  
  
  
  ################################################# CREATE NETCDF DIMENSIONS ######################################################
  ############################################### DIMENSIONS OF DATA VARIABLES ####################################################
  # TIME: Number of environmental and physiological measurement timestamps.
  time=sort(unique(as.numeric(as.POSIXct(strptime(tag_points$time, "%d-%b-%Y %H:%M:%S"), tz="UTC"))))
  # to convert back to posixtct: as.POSIXct(time,origin='1970-01-01',tz='GMT')
  if(length(time)>0){TIME<-ncdim_def(name='TIME',longname='time',units='',vals=time,unlim=F)}
  
  time_uncertainty=sort(unique(unique(as.numeric(tag_polygons_1$time_poly)),unique(as.numeric(tag_polygons_2$time_poly)),unique(as.numeric(tag_polygons_3$time_poly))))
  if(time_uncertainty>0){TIME_UNCERTAINTY<-ncdim_def(name='TIME_UNCERTAINTY',longname='time_uncertainty',units='',vals=time_uncertainty,unlim=F)}
  
  # N_VALUES20: Maximum number of environmental and physiological parameter measurements sampled for a given timestamp. 
  # dimension is used only when it is necessary: if there is more than one value for each timestamp (N > 1) and the ## equals the number of values present for a given variable. You can add more than one of these, with each variable referencing a different N_VALUES## if required.
  # Example with N = 20: float <PARAM>_ADJUSTED(TIME, N_VALUES20)
  ## AE ADDITION: n_values_1 is the maximum number of values in the uncertainty polygon, level 1.
  if(!is.na(n_values_1)){
    n_values_1=seq(1:n_values_1)
    N_VALUES_1=ncdim_def(name='N_VALUES_1',longname='n_values for the position uncertainty level 1',units='',vals=n_values_1,unlim=F)}
  
  if(!is.na(n_values_2)){
    n_values_2=seq(1:n_values_2)
    N_VALUES_2=ncdim_def(name='N_VALUES_2',longname='n_values for the position uncertainty level 2',units='',vals=n_values_2,unlim=F)}
  
  if(!is.na(n_values_3)){
    n_values_3=seq(1:n_values_3)
    N_VALUES_3=ncdim_def(name='N_VALUES_3',longname='n_values for the position uncertainty level 3',units='',vals=n_values_3,unlim=F)}
  
  # TIME_TECH: Number of engineering data timestamps.
  # time_tech=sort(unique(unique(as.numeric(tag_points$time_tech)),unique(as.numeric(tag_polygons_1$time_tech)),unique(as.numeric(tag_polygons_2$time_tech)),unique(as.numeric(tag_polygons_3$time_tech))))
  time_tech=0
  if(time_tech>0){TIME_TECH=ncdim_def(name='TIME_TECH',longname='time_tech',units='',vals=time_tech,unlim=F)}
  
  # TIME_SATELLITE: Number of Iridium sessions. (AE CHANGED THIS FROM IRIDIUM TO BE MORE GENERIC)
  # time_satellite=sort(unique(as.numeric(tag_points$time_satellite),as.numeric(tag_polygons_1$time_satellite),as.numeric(tag_polygons_2$time_satellite),as.numeric(tag_polygons_3$time_satellite)))
  time_satellite=0
  if(time_satellite>0){TIME_SATELLITE=ncdim_def(name='TIME_SATELLITE',longname='time_satellite',units='',vals=time_satellite,unlim=F)}
  
  ############################################### DIMENSIONS OF META-DATA VARIABLES ####################################################
  # N_PARAM: Number of parameters measured or calculated for a timestamp. JUST TAG DATA (i.e., NOT ENRICHED)
  # Examples : (pressure, temperature) : N_PARAM = 2    OR
  # (pressure, temperature, luminosity) : N_PARAM = 3
  if(!is.na(n_param)){n_param=seq(1:n_param)
  N_PARAM=ncdim_def(name='N_PARAM',longname='n_param',units='',vals=n_param,unlim=F)}
  
  # N_SENSOR: Number of sensors mounted on the tag and used to measure the parameters.
  if(!is.na(n_sensor)){
    n_sensor=seq(1:n_sensor)
    N_SENSOR=ncdim_def(name='N_SENSOR',longname='n_sensor',units='',vals=n_sensor,unlim=F)}
  
  # N_CONFIG_PARAM: Number of configuration parameters.
  if(!is.na(n_config_param)){
    n_config_param=seq(1:n_config_param)
    N_CONFIG_PARAM=ncdim_def(name='N_CONFIG_PARAM',longname='n_config_param',units='',vals=n_config_param,unlim=F)}
  
  # N_MISSION: Number of missions.
  if(!is.na(n_mission)){
    n_mission=seq(1:n_mission)
    N_MISSION=ncdim_def(name='N_MISSION',longname='n_mission',units='',vals=n_mission,unlim=F)}
  
  # N_DERIVATION: Maximum number of calibrations for a parameter.
  if(!is.na(n_derivation)){ 
    n_derivation=seq(1:n_derivation)
    N_DERIVATION=ncdim_def(name='N_DERIVATION',longname='n_derivation',units='',vals=n_derivation,unlim=F)}
  
  # N_PARAM_TECH: Number of technical parameters measured or calculated for a timestamp.
  if(!is.na(n_param_tech)){
    n_param_tech=seq(1:n_param_tech)
    N_PARAM_TECH=ncdim_def(name='N_PARAM_TECH',longname='n_param_tech',units='',vals=n_param_tech,unlim=F)}
  
  ############################################### DIMENSIONS OF HISTORY VARIABLES ####################################################
  # N_HISTORY: Number of history records.
  if(!is.na(n_history)){
    n_history=seq(1:n_history)
    N_HISTORY=ncdim_def(name='N_HISTORY',longname='n_history',units='',vals=n_history,unlim=F)}
  
  ##################################################### COMMON DIMENSIONS ############################################################
  # DATE_TIME: This dimension is the length of an ASCII date and time value.  Date_time convention is: YYYYMMDDHHMISS. Date and time values are always
  # in universal time coordinates (UTC).  Examples: 20010105172834: January 5th 2001 17:28:34
  date_time=seq(1:date_time)
  if(!is.na(date_time)){
    date_time=seq(1:date_time)
    DATE_TIME=ncdim_def(name='DATE_TIME',longname='date_time',units='',vals=date_time,unlim=F)}
  
  # STRING DIMENSIONS:  String dimensions from 2 to 4096.
  string4096=seq(1:4096)
  STRING4096=ncdim_def(name='STRING4096',longname='string4096',units='',vals=string4096,unlim=F)
  string1024=seq(1:1024)
  STRING1024=ncdim_def(name='STRING1024',longname='string1024',units='',vals=string1024,unlim=F)
  string512=seq(1:512)
  STRING512=ncdim_def(name='STRING512',longname='string512',units='',vals=string512,unlim=F)
  string256=seq(1:256)
  STRING256=ncdim_def(name='STRING256',longname='string256',units='',vals=string256,unlim=F)
  string128=seq(1:128)
  STRING128=ncdim_def(name='STRING128',longname='string128',units='',vals=string128,unlim=F)
  string64=seq(1:64)
  STRING64=ncdim_def(name='STRING64',longname='string64',units='',vals=string64,unlim=F)
  string32=seq(1:32)
  STRING32=ncdim_def(name='STRING32',longname='string32',units='',vals=string32,unlim=F)
  string16=seq(1:16)
  STRING16=ncdim_def(name='STRING16',longname='string16',units='',vals=string16,unlim=F)
  string8=seq(1:8)
  STRING8=ncdim_def(name='STRING8',longname='string8',units='',vals=string8,unlim=F)
  string4=seq(1:4)
  STRING4=ncdim_def(name='STRING4',longname='string4',units='',vals=string4,unlim=F)
  string2=seq(1:2)
  STRING2=ncdim_def(name='STRING2',longname='string2',units='',vals=string2,unlim=F)
  
  
  ############################################### DEFINE DATA VARIABLES ##############################################
  # 2.3  Data variables
  # POPSTAR data variables include:
  # • Measurements: data sampled by the tag, parameters derived from the primary measurements and coordinate variables used to orient data in time and space;
  # • Engineering data: technical information reported by the tag;
  # • Iridium (satellite) data: information on Iridium fixes provided with each sessions that occurred during data transmission.
  # The variable names are written in CAPITALIZED letters. Each variable has a specific set of attributes, some of which are mandatory. 
  # The mandatory variables or attributes are in bold characters in the following tables.
  
  
  ##define variables of NetCDF - DEFINE EACH COLUMN AS VARIABLE
  # nonAvailable <- 999999.
  
  ## COORDINATE VARIABLES, all attributes but 'comments' are mandatory
  ## <------------------- what are the dimensions for each of these variables?
  JULD                 <- ncvar_def(name='juld',longname='Julian 1950 time',units='days since 1950-01-01T00:00:00Z',dim=TIME,missval=999999.,prec='double',compression = 1)  
  JULD_QC              <- ncvar_def(name='juld_qc',longname='Julian 1950 time Quality flag',units='',dim=TIME,missval=-128,prec='byte',compression = 1)
  LATITUDE             <- ncvar_def(name='latitude',longname='Measurement latitude',units='degree_north',dim=TIME,missval=99999.0,prec='double',compression = 1)  
  LONGITUDE            <- ncvar_def(name='longitude',longname='Measurement longitude',units='degree_east',dim=TIME,missval=99999.0,prec='double',compression = 1)  
  POSITION_QC          <- ncvar_def(name='position_qc',longname='Quality flag',units='',dim=TIME,missval=-128,prec='byte',compression = 1)
  
  # POSITION_UNCERTAINTY <- ncvar_def(name='position_uncertainty',longname='Uncertainty of measurement location',units='km',dim=TIME,missval=99999.0,prec='float',compression = 1)  
  ## AE CHANGED POSITION_UNCERTAINTY TO 3 LEVELS OF LAT AND LON UNCERTAINTY (LAT/LONS OF 50%, 75%, 95% UNCERTAINTY POLYGONS)
  # polygon lat/lons ## AE additions (lat/lon uncertainities for 3 different levels)
  # POSITION_UNCERTAINTY_1 <- ncvar_def(name='position_uncertainty_1',longname='Uncertainty level 1 of measurement latitude,longitude polygon',units='',dim=list(TIME,N_VALUES_1),missval=99999.0,prec='double',compression = 1)
  # POSITION_UNCERTAINTY_2 <- ncvar_def(name='position_uncertainty_2',longname='Uncertainty level 2 of measurement latitude,longitude polygon',units='',dim=list(TIME,N_VALUES_2),missval=99999.0,prec='double',compression = 1)
  # POSITION_UNCERTAINTY_3 <- ncvar_def(name='position_uncertainty_3',longname='Uncertainty level 3 of measurement latitude,longitude polygon',units='',dim=list(TIME,N_VALUES_3),missval=99999.0,prec='double',compression = 1)
  
  LATITUDE_UNCERTAINTY_1 <- ncvar_def(name='latitude_uncertainty_1',longname='Uncertainty level 1 of measurement latitude, polygon',units='degree_north',dim=list(N_VALUES_1,TIME_UNCERTAINTY),missval=99999.0,prec='float',compression = 1)
  LONGITUDE_UNCERTAINTY_1 <- ncvar_def(name='longitude_uncertainty_1',longname='Uncertainty level 1 of measurement longitude, polygon',units='degree_east',dim=list(N_VALUES_1,TIME_UNCERTAINTY),missval=99999.0,prec='double',compression = 1)
  
  # polygon lat/lons ## AE additions (lat/lon uncertainities)
  LATITUDE_UNCERTAINTY_2 <- ncvar_def(name='latitude_uncertainty_2',longname='Uncertainty level 2 of measurement latitude, polygon',units='degree_north',dim=list(N_VALUES_2,TIME_UNCERTAINTY),missval=99999.0,prec='double',compression = 1)
  LONGITUDE_UNCERTAINTY_2 <- ncvar_def(name='longitude_uncertainty_2',longname='Uncertainty level 2 of measurement longitude, polygon',units='degree_east',dim=list(N_VALUES_2,TIME_UNCERTAINTY),missval=99999.0,prec='double',compression = 1)
  
  # polygon lat/lons ## AE additions (lat/lon uncertainities)
  LATITUDE_UNCERTAINTY_3 <- ncvar_def(name='latitude_uncertainty_3',longname='Uncertainty level 3 of measurement latitude, polygon',units='degree_north',dim=list(N_VALUES_3,TIME_UNCERTAINTY),missval=99999.0,prec='double',compression = 1)
  LONGITUDE_UNCERTAINTY_3 <- ncvar_def(name='longitude_uncertainty_3',longname='Uncertainty level 3 of measurement longitude, polygon',units='degree_east',dim=list(N_VALUES_3,TIME_UNCERTAINTY),missval=99999.0,prec='double',compression = 1)
  
  
  ## MEASUREMENT VARIABLES, mandatory attributes listed in commented variable descriptions below
  # The N_VALUES## dimension is used only when it is necessary: if there is more than one value for each timestamp (N > 1) and the ## equals the number
  # of values present for a given variable. You can add more than one of these, with each variable referencing a different N_VALUES## if required
  ## <PARAM>: standard_name, units, _FillValue
  
  ## TAG MEASUREMENT VARIABLES (AE adjustment)
  # sea_water_temperature
  # PRES                 <- ncvar_def(name='sea_water_pressure',longname='Sea water pressure, equals 0 at sea-level',units='decibar',dim=TIME,missval=999999.,prec='double',compression = 1)  
  TEMP                 <- ncvar_def(name='sea_water_temperature',longname='Sea temperature in-situ ITS-90 scale',units='degree_Celsius',dim=TIME,missval=999999.,prec='float',compression = 1)  
  DEPTH                <- ncvar_def(name='depth',longname='Depth observed by tag',units='m',dim=TIME,missval=999999.,prec='float',compression = 1)
  ## AE ADDITION: SUNRISE AND SUNSET SHOULD BE A STANDARD OUTPUT OF TAGGING DATA ALGORITMS
  SUNRISE              <- ncvar_def(name='sunrise',longname='Time at sunrise',units='days since 1950-01-01T00:00:00Z',dim=TIME,missval=999999.,prec='float',compression = 1)  
  SUNSET               <- ncvar_def(name='sunset',longname='Time at sunset',units='days since 1950-01-01T00:00:00Z',dim=TIME,missval=999999.,prec='float',compression = 1)  
  
  # <PARAM>_QC: long_name, conventions, _FillValue, valid_min, valid_max, flag_values, flag_meanings
  TEMP_QC              <- ncvar_def(name='sea_water_temperature_qc',longname='Sea temperature in-situ ITS-90 scale quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1)  # var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9,
  DEPTH_QC             <- ncvar_def(name='depth_qc',longname='Depth observed by tag quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1) # var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9,
  SUNRISE_QC           <- ncvar_def(name='sunrise_qc',longname='Time at sunrise quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1)# var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9
  SUNSET_QC            <- ncvar_def(name='sunset_qc',longname='Time at sunset quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1)# var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9,
  
  # <PARAM>_UNCERTAINTY: long_name, _FillValue, units, 
  # If it is impossible to estimate the measurement uncertainty, it is required to define at least the attribute <PARAM>:accuracy with the nominal sensor accuracy.
  # The attributes <PARAM>:precision and <PARAM>:resolution are optional; they contain the sensor precision and resolution if known. 
  TEMP_UNCERTAINTY     <- ncvar_def(name='sea_water_temperature_uncertainty',longname='Sea temperature in-situ ITS-90 scale uncertainty',missval=999999.,dim=TIME,units='degree_Celsius',prec='float',compression = 1)
  DEPTH_UNCERTAINTY    <- ncvar_def(name='depth_uncertainty',longname='Depth observed by tag uncertainty',missval=999999.,dim=TIME,units='m',prec='float',compression = 1)
  SUNRISE_UNCERTAINTY  <- ncvar_def(name='sunrise_uncertainty',longname='Time at sunrise uncertainty',missval=999999.,dim=TIME,units='days since 1950-01-01T00:00:00Z',prec='float',compression = 1)
  SUNSET_UNCERTAINTY   <- ncvar_def(name='sunset_uncertainty',longname='Time at sunset uncertainty',missval=999999.,dim=TIME,units='days since 1950-01-01T00:00:00Z',prec='float',compression = 1)
  
  # <PARAM>_ADJUSTED: standard_name, units, _FillValue
  TEMP_ADJUSTED        <- ncvar_def(name='sea_water_temperature_adjusted',longname='Adjusted Sea temperature in-situ ITS-90 scale',units='degree_Celsius',dim=TIME,missval=999999.,prec='float',compression = 1)  
  DEPTH_ADJUSTED       <- ncvar_def(name='depth_adjusted',longname='Adjusted Depth observed by tag',units='m',dim=TIME,missval=999999.,prec='float',compression = 1)
  SUNRISE_ADJUSTED     <- ncvar_def(name='sunrise_adjusted',longname='Adjusted Time at sunrise',units='days since 1950-01-01T00:00:00Z',dim=TIME,missval=999999.,prec='float',compression = 1)  
  SUNSET_ADJUSTED      <- ncvar_def(name='sunset_adjusted',longname='Adjusted Time at sunset',units='days since 1950-01-01T00:00:00Z',dim=TIME,missval=999999.,prec='float',compression = 1)  
  
  # <PARAM>_ADJUSTED_QC: long_name, conventions, _FillValue, valid_min, valid_max, flag_values, flag_meanings
  TEMP_ADJUSTED_QC     <- ncvar_def(name='sea_water_temperature_adjusted_qc',longname='Adjusted Sea temperature in-situ ITS-90 scale quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1)  # var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9,
  DEPTH_ADJUSTED_QC    <- ncvar_def(name='depth_adjusted_qc',longname='Adjusted Depth observed by tag quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1)# var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9,
  SUNRISE_ADJUSTED_QC  <- ncvar_def(name='sunrise_adjusted_qc',longname='Adjusted Time at sunrise quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1)# var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9,
  SUNSET_ADJUSTED_QC   <- ncvar_def(name='sunset_adjusted_qc',longname='Adjusted Time at sunset quality flags',units='',dim=TIME,missval=-128,prec='byte',compression = 1)# var att: conventions='POPSTAR reference table 2',valid_min=0, valid_max=9
  
  # <PARAM>_ADJUSTED_UNCERTAINTY: long_name, _FillValue, units
  TEMP_ADJUSTED_UNCERTAINTY     <- ncvar_def(name='sea_water_temperature_adjusted_uncertainty',longname='Adjusted Sea temperature in-situ ITS-90 scale uncertainty',missval=999999.,dim=TIME,units='degree_Celsius',prec='float',compression = 1)  
  DEPTH_ADJUSTED_UNCERTAINTY    <- ncvar_def(name='depth_adjusted_uncertainty',longname='Adjusted Depth observed by tag uncertainty',missval=999999.,dim=TIME,units='m',prec='float',compression = 1)
  SUNRISE_ADJUSTED_UNCERTAINTY  <- ncvar_def(name='sunrise_adjusted_uncertainty',longname='Adjusted Time at sunrise uncertainty',missval=999999.,dim=TIME,units='days since 1950-01-01T00:00:00Z',prec='float',compression = 1)
  SUNSET_ADJUSTED_UNCERTAINTY   <- ncvar_def(name='sunset_adjusted_uncertainty',longname='Adjusted Time at sunset uncertainty',missval=999999.,dim=TIME,units='days since 1950-01-01T00:00:00Z',prec='float',compression = 1)
  
  ## ENGINEERING VARIABLES, all attributes but 'comments' are mandatory
  if(time_tech>0){
    JULD_TECH            <-ncvar_def(name='JULD_TECH',longname='Julian 1950 time',units='days since 1950-01-01T00:00:00Z',dim=TIME_TECH,missval=999999.,prec='double',compression = 1)  
    # <PARAM_TECH>
    # <PARAM_TECH>_QC
  }
  
  ## IRIDIUM/SATELLITE VARIABLES, all attributes but 'comments' are mandatory. 
  # Iridium data variables store information on Iridium sessions that occurred during tag data transmission.
  ## AE CHANGED 'IRIDIUM' TO 'SATELLITE' IN ALL CASES TO MAKE MORE GENERIC
  if(time_satellite>0){
    JULD_SATELLITE          <- ncvar_def(name='juld_satellite',longname='Julian 1950 time of satellite session',units='days since 1950-01-01T00:00:00Z',dim=TIME_SATELLITE,missval=999999.,prec='double',compression = 1)  
    LATITUDE_SATELLITE      <- ncvar_def(name='latitude_satellite',longname='Latitude of satellite session',units='degree_north',dim=TIME_SATELLITE,missval=99999.0,prec='double',compression = 1)  
    LONGITUDE_SATELLITE     <- ncvar_def(name='longitude_satellite',longname='longitude of satellite session',units='degree_east',dim=TIME_SATELLITE,missval=99999.0,prec='double',compression = 1)  
    CEP_RADIUS_SATELLITE    <- ncvar_def(name='cep_radius_satellite',longname='Circular error probability of satellite session location',units='km',dim=TIME_SATELLITE,missval=99999.0,prec='integer')
  }
  
  ## METADATA VARIABLES, mandatory variables and attributes TO BE DEFINED (as of August 2018)
  # POPSTAR meta-data variables include:
  # • Tag characteristics;
  # • Tag deployment and mission information;
  # • Tag configuration;
  # • Tag sensor and parameter information;
  # • Tag parameter calibration and derivation parameters.
  
  ## TAG CHARACTERISTICS
  TAG_NUMBER             <- ncvar_def(name='tag_number',longname='Tag unique identifier',units='',dim=STRING8,missval=' ',prec='char',compression = 1)  
  IMEI                   <- ncvar_def(name='imei',longname='Iridium transmission identifier',units='',dim=STRING256,missval=' ',prec='char',compression = 1)  
  TAG_MAKER              <- ncvar_def(name='tag_maker',longname='Name of the manufacturer',units='',dim=STRING256,missval=' ',prec='char',compression = 1)  
  FIRMWARE_VERSION       <- ncvar_def(name='firmware_version',longname='Firmware version for the tag',units='',dim=STRING32,missval=' ',prec='char',compression = 1)  
  MANUAL_VERSION         <- ncvar_def(name='manual_version',longname='Manual version for the tag',units='',dim=STRING16,missval=' ',prec='char',compression = 1)  
  TAG_SERIAL_NO          <- ncvar_def(name='tag_serial_no',longname='Serial number of the tag',units='',dim=STRING32,missval=' ',prec='char',compression = 1)  
  DAC_FORMAT_ID          <- ncvar_def(name='dac_format_id',longname='Format number used by the DAC to describe the data format type for each tag',units='',dim=STRING16,missval=' ',prec='char',compression = 1)  
  WMO_INST_TYPE          <- ncvar_def(name='wmo_inst_type',longname='Coded instrument type',units='',dim=STRING4,missval=' ',prec='char',compression = 1)  
  ANOMALY                <- ncvar_def(name='anomaly',longname='Describe any anomalies or problems the tag may have had',units='',dim=STRING256,missval=' ',prec='char',compression = 1)  
  BATTERY_TYPE           <- ncvar_def(name='battery_type',longname='Type of battery packs in the tag',units='',dim=STRING64,missval=' ',prec='char',compression = 1)  
  BATTERY_PACKS          <- ncvar_def(name='battery_packs',longname='Configuration of battery packs in the tag',units='',dim=STRING64,missval=' ',prec='char',compression = 1)  
  CONTROLLER_BOARD_TYPE  <- ncvar_def(name='controller_board_type',longname='Type of controller board',units='',dim=STRING32,missval=' ',prec='char',compression = 1)  
  CONTROLLER_BOARD_SERIAL_NO  <- ncvar_def(name='controller_board_serial_no',longname='Serial number of the controller board',units='',dim=STRING32,missval=' ',prec='char',compression = 1)  
  SPECIAL_FEATURES       <- ncvar_def(name='special_features',longname='Extra features of the tag (algorithms, compressee etc.)',units='',dim=STRING1024,missval=' ',prec='char',compression = 1)  
  TAG_OWNER              <- ncvar_def(name='tag_owner',longname='Tag owner',units='',dim=STRING64,missval=' ',prec='char',compression = 1)  
  OPERATING_INSTITUTION  <- ncvar_def(name='operating_institution',longname='Operating institution of the tag',units='',dim=STRING64,missval=' ',prec='char',compression = 1)  
  CUSTOMISATION          <- ncvar_def(name='customisation',longname='tag customisation, i.e. (institution and modifications',units='',dim=STRING1024,missval=' ',prec='char',compression = 1)  
  
  ## TAG DEPLOYMENT AND MISSION CHARACTERISTICS
  DEPLOYMENT_DATE        <- ncvar_def(name='deployment_date',longname='Date (UTC) of the deployment',units='',dim=DATE_TIME,missval=' ',prec='char',compression = 1)  
  DEPLOYMENT_LATITUDE    <- ncvar_def(name='deployment_latitude',longname='Latitude of the tag when deployed',units='degree_north',dim=list(),missval=99999,prec='double',compression = 1)  
  DEPLOYMENT_LONGITUDE   <- ncvar_def(name='deployment_longitude',longname='Longitude of the tag when deployed',units='degree_east',dim=list(),missval=99999,prec='double',compression = 1)  
  DEPLOYMENT_QC          <- ncvar_def(name='deployment_qc',longname='Quality on deployment date, time and location',units='',dim=list(),missval=' ',prec='char',compression = 1)  
  START_DATE             <- ncvar_def(name='start_date',longname='Date (UTC) of the first descent of the tag',units='',dim=DATE_TIME,missval=' ',prec='char',compression = 1)  
  START_DATE_QC          <- ncvar_def(name='start_date_qc',longname='Quality on start date',units='',dim=list(),missval=' ',prec='char',compression = 1)  
  STARTUP_DATE           <- ncvar_def(name='startup_date',longname='Date (UTC) of the activation of the tag',units='',dim=DATE_TIME,missval=' ',prec='char',compression = 1)  
  STARTUP_DATE_QC        <- ncvar_def(name='startup_date_qc',longname='Quality on startup date',units='',dim=list(),missval=' ',prec='char',compression = 1)  
  DEPLOYMENT_PLATFORM    <- ncvar_def(name='deployment_platform',longname='Identifier of the deployment platform',units='',dim=STRING32,missval=' ',prec='char',compression = 1)  
  DEPLOYMENT_CRUISE_ID   <- ncvar_def(name='deployment_cruise_id',longname='Identification number or reference number of the cruise used to deploy the tag',units='',dim=STRING32,missval=' ',prec='char',compression = 1)  
  END_MISSION_DATE       <- ncvar_def(name='end_mission_date',longname='Date (UTC) of the deployment',units='',dim=DATE_TIME,missval=' ',prec='char',compression = 1)  
  END_MISSION_STATUS     <- ncvar_def(name='end_mission_status',longname='Status of the end of mission of the tag',units='',dim=list(),missval=' ',prec='char',compression = 1)  
  
  ## TAG CONFIGURATION PARAMETERS
  # Configuration parameters are tag settings, not measurements reported by the tag.
  # Configuration parameters may or may not be reported by a tag.
  # Configuration parameter names are identified by the “CONFIG” prefix and provided in reference table 18.
  # Configuration parameters are gathered in missions (a dated set of configuration parameters).
  # For each configuration parameter of each mission, the name of the parameter and the value of the parameter are recorded.
  if(!is.na(n_config_param)){
    CONFIG_PARAMETER_NAME  <- ncvar_def(name='config_parameter_name',longname='Name of configuration parameter',units='',dim=list(N_CONFIG_PARAM, STRING128),missval=' ',prec='char',compression = 1)  
    CONFIG_PARAMETER_DATE  <- ncvar_def(name='config_parameter_name',longname='Julian 1950 time of the mission',units='days since 1950-01-01T00:00:00Z',dim=list(N_MISSION),missval=999999,prec='double',compression = 1)  
    CONFIG_PARAMETER_VALUE <- ncvar_def(name='config_parameter_value',longname='Value of configuration parameter',units='',dim=list(N_MISSION, N_CONFIG_PARAM),missval='99999.f',prec='double',compression = 1)  
  }
  
  ## TAG SENSOR INFORMATION
  if(!is.na(n_sensor)){
    SENSOR                 <- ncvar_def(name='sensor',longname='Name of the sensor mounted on the tag',units='',dim=list(N_SENSOR, STRING32),missval=' ',prec='char',compression = 1)  
    SENSOR_MAKER           <- ncvar_def(name='sensor_maker',longname='Name of the sensor manufacturer',units='',dim=list(N_SENSOR, STRING256),missval=' ',prec='char',compression = 1)  
    SENSOR_MODEL           <- ncvar_def(name='sensor_model',longname='Type of the sensor',units='',dim=list(N_SENSOR, STRING256),missval=' ',prec='char',compression = 1)  
    SENSOR_SERIAL_NO       <- ncvar_def(name='sensor_serial_no',longname='Serial number of the sensor',units='',dim=list(N_SENSOR, STRING16),missval=' ',prec='char',compression = 1)  
  }
  
  ## TAG PARAMETER INFORMATION
  ## ??? what's the difference between these and the measurement variables above? why not just add the extra variables "accuracy', 'resolution' above?
  if(!is.na(n_param)){
    PARAMETER              <- ncvar_def(name='parameter',longname='Name of parameter computed from tag measurements',units='',dim=list(N_PARAM, STRING64),missval=' ',prec='char',compression = 1)  
    PARAMETER_SENSOR       <- ncvar_def(name='parameter_sensor',longname='Name of the sensor that measures this parameter',units='',dim=list(N_PARAM, STRING128),missval=' ',prec='char',compression = 1)  
    PARAMETER_UNITS        <- ncvar_def(name='parameter_units',longname='Units of accuracy and resolution of the parameter',units='',dim=list(N_PARAM, STRING32),missval=' ',prec='char',compression = 1)  
    PARAMETER_ACCURACY     <- ncvar_def(name='parameter_accuracy',longname='Accuracy of the parameter',units='',dim=list(N_PARAM, STRING32),missval=' ',prec='char',compression = 1)  
    PARAMETER_RESOLUTION   <- ncvar_def(name='parameter_resolution',longname='Resolution of the parameter',units='',dim=list(N_PARAM, STRING32),missval=' ',prec='char',compression = 1)  
  }
  
  ## TAG PARAMETER DERIVATION AND CALIBRATION INFORMATION
  # A derived parameter is calculated from one or several parameters.
  # Example: salinity is derived from conductivity, temperature and pressure.
  # Calibrations are applied to parameters to create adjusted parameters. Different calibration methods will be used by groups processing tag data. When a method is applied, its description is stored in the following fields.
  # The equation used to calculate derived parameters is recorded as the first calibration of the parameter.
  # Its calibration date is set to the deployment date.
  # If no derivation or calibration is available, N_DERIVATION is set to 1, all values of the derivation section are set to fill values.
  
  # DERIVATION_PARAMETER
  if(!is.na(n_derivation)){
    DERIVATION_PARAMETER    <- ncvar_def(name='derivation_parameter',longname='List of parameters with derivation or calibration information',units='',dim=list(N_DERIVATION, STRING64),missval=' ',prec='char',compression = 1)  
    DERIVATION_EQUATION     <- ncvar_def(name='derivation_equation',longname='Derivation or calibration equation for this parameter',units='',dim=list(N_DERIVATION, STRING4096),missval=' ',prec='char',compression = 1)  
    DERIVATION_COEFFICIENT  <- ncvar_def(name='derivation_coefficient',longname='Derivation or calibration coefficients for this equation',units='',dim=list(N_DERIVATION, STRING4096),missval=' ',prec='char',compression = 1)  
    DERIVATION_COMMENT      <- ncvar_def(name='derivation_comment',longname='Comment applying to this parameter derivation or calibration',units='',dim=list(N_DERIVATION, STRING4096),missval=' ',prec='char',compression = 1)  
    DERIVATION_DATE         <- ncvar_def(name='derivation_date',longname='Date (UTC) of derivation or calibration',units='',dim=list(N_DERIVATION, DATE_TIME),missval=' ',prec='char',compression = 1)  
  }
  
  # TAG TECHNICAL PARAMETER INFORMATION
  if(!is.na(n_param_tech)){
    PARAMETER_TECH          <- ncvar_def(name='parameter_tech',longname='Name of technical parameter reported by the tag',units='',dim=list(N_PARAM_TECH, STRING64),missval=' ',prec='char',compression = 1)  
    PARAMETER_TECH_UNITS    <- ncvar_def(name='parameter_tech_units',longname='Units of accuracy and resolution of the technical parameter',units='',dim=list(N_PARAM_TECH, STRING32),missval=' ',prec='char',compression = 1)  
    PARAMETER_TECH_ACCURACY <- ncvar_def(name='parameter_tech_accuracy',longname='Accuracy of the technical parameter',units='',dim=list(N_PARAM_TECH, STRING32),missval=' ',prec='char',compression = 1)  
    PARAMETER_TECH_RESOLUTION<- ncvar_def(name='parameter_tech_resolution',longname='Resolution of the technical parameter',units='',dim=list(N_PARAM_TECH, STRING32),missval=' ',prec='char',compression = 1)  
  }
  
  ## HISTORY INFORMATION
  # A history record is created whenever an action is performed on a part of the time-series defined by history_start_time and history_stop_time.
  # The recorded actions are coded and described in the history code table from the reference table 7.
  if(!is.na(n_history)){
    HISTORY_INSTITUTION     <- ncvar_def(name='history_institution',longname='Institution which performed action',units='',dim=list(N_HISTORY, STRING2),missval=' ',prec='char',compression = 1)  
    HISTORY_STEP            <- ncvar_def(name='history_step',longname='Step in data processing',units='',dim=list(N_HISTORY, STRING4),missval=' ',prec='char',compression = 1)  
    HISTORY_SOFTWARE        <- ncvar_def(name='history_software',longname='Name of software which performed action',units='',dim=list(N_HISTORY, STRING8),missval=' ',prec='char',compression = 1)  
    HISTORY_SOFTWARE_RELEASE<- ncvar_def(name='history_software_release',longname='Version/release of software which performed action',units='',dim=list(N_HISTORY, STRING4),missval=' ',prec='char',compression = 1)  
    HISTORY_REFERENCE       <- ncvar_def(name='history_reference',longname='Reference of database',units='',dim=list(N_HISTORY, STRING64),missval=' ',prec='char',compression = 1)  
    HISTORY_DATE            <- ncvar_def(name='history_date',longname='Date the history record was created',units='',dim=list(N_HISTORY, DATE_TIME),missval=' ',prec='char',compression = 1)  
    HISTORY_ACTION          <- ncvar_def(name='history_action',longname='Action performed on data',units='',dim=list(N_HISTORY, STRING64),missval=' ',prec='char',compression = 1)  
    HISTORY_PARAMETER       <- ncvar_def(name='history_parameter',longname='Parameter action is performed on',units='',dim=list(N_HISTORY, STRING16),missval=' ',prec='char',compression = 1)  
    HISTORY_PREVIOUS_VALUE  <- ncvar_def(name='history_previous_value',longname='Parameter/Flag previous value before action',units='',dim=list(N_HISTORY),missval='99999.f',prec='float',compression = 1)  
    HISTORY_START_TIME_INDEX<- ncvar_def(name='history_start_time_index',longname='Start time index action applied on',units='',dim=list(N_HISTORY),missval=99999,prec='integer',compression = 1)  
    HISTORY_STOP_TIME_INDEX <- ncvar_def(name='history_stop_time_index',longname='Stop time index action applied on',units='',dim=list(N_HISTORY),missval=99999,prec='integer',compression = 1)  
    HISTORY_QCTEST          <- ncvar_def(name='history_qctest',longname='Documentation of tests performed, tests failed (in hex form)',units='',dim=list(N_HISTORY, STRING16),missval=' ',prec='char',compression = 1)  
  }
  
  
  ############################################# CREATE NETCDF FILE #####################################################
  ncName      <- paste0(nc_dir,tag_name,".nc") 
  ## TO DO: function to create list of mandatory variables and the optional variables that are available for each data file
  # create nc with mandatory variables
  nc <- nc_create(ncName, list(JULD, 
                               JULD_QC,
                               LATITUDE,
                               LONGITUDE,
                               POSITION_QC,
                               # POSITION_UNCERTAINTY_1,
                               # POSITION_UNCERTAINTY_2,
                               # POSITION_UNCERTAINTY_3,
                               LATITUDE_UNCERTAINTY_1,
                               LONGITUDE_UNCERTAINTY_1,
                               LATITUDE_UNCERTAINTY_2,
                               LONGITUDE_UNCERTAINTY_2,
                               LATITUDE_UNCERTAINTY_3,
                               LONGITUDE_UNCERTAINTY_3,
                               ## MEASUREMENT VARIABLES,
                               DEPTH,
                               DEPTH_QC,
                               DEPTH_UNCERTAINTY,
                               DEPTH_ADJUSTED,
                               DEPTH_ADJUSTED_QC,
                               DEPTH_ADJUSTED_UNCERTAINTY,
                               TEMP,
                               TEMP_QC,
                               TEMP_UNCERTAINTY,
                               TEMP_ADJUSTED,
                               TEMP_ADJUSTED_QC,
                               TEMP_ADJUSTED_UNCERTAINTY,
                               SUNRISE,
                               SUNRISE_QC,
                               SUNRISE_UNCERTAINTY,
                               SUNRISE_ADJUSTED,
                               SUNRISE_ADJUSTED_QC,
                               SUNRISE_ADJUSTED_UNCERTAINTY,
                               SUNSET,
                               SUNSET_QC,
                               SUNSET_UNCERTAINTY,
                               SUNSET_ADJUSTED,
                               SUNSET_ADJUSTED_QC,
                               SUNSET_ADJUSTED_UNCERTAINTY,TAG_NUMBER,
                               IMEI,
                               TAG_MAKER,
                               FIRMWARE_VERSION,
                               MANUAL_VERSION,
                               TAG_SERIAL_NO,
                               DAC_FORMAT_ID,
                               WMO_INST_TYPE,
                               ANOMALY,
                               BATTERY_TYPE,
                               BATTERY_PACKS,
                               CONTROLLER_BOARD_TYPE,
                               CONTROLLER_BOARD_SERIAL_NO,
                               TAG_OWNER,
                               OPERATING_INSTITUTION,
                               CUSTOMISATION,
                               SPECIAL_FEATURES,
                               ## TAG DEPLOYMENT AND MISSION CHARACTERISTICS
                               DEPLOYMENT_DATE,
                               DEPLOYMENT_LATITUDE,
                               DEPLOYMENT_LONGITUDE,
                               DEPLOYMENT_QC,
                               START_DATE,
                               START_DATE_QC,
                               STARTUP_DATE,
                               DEPLOYMENT_PLATFORM,
                               DEPLOYMENT_CRUISE_ID,
                               END_MISSION_DATE,
                               END_MISSION_STATUS),force_v4 = F)
  
  if(time_tech>0){
    nc<-ncvar_add(nc,JULD_TECH)
    # <PARAM_TECH>=,
    # <PARAM_TECH>_QC=,
  }
  
  if(time_satellite>0){
    nc<-ncvar_add(nc,JULD_SATELLITE)
    nc<-ncvar_add(nc,LATITUDE_SATELLITE)
    nc<-ncvar_add(nc,LONGITUDE_SATELLITE)
    nc<-ncvar_add(nc,CEP_RADIUS_SATELLITE)
  }
  
  if(!is.na(n_config_param)){
    nc<-ncvar_add(nc,CONFIG_PARAMETER_NAME)
    nc<-ncvar_add(nc,CONFIG_PARAMETER_DATE)
    nc<-ncvar_add(nc,CONFIG_PARAMETER_VALUE)
  }
  
  if(!is.na(n_sensor)){
    nc<-ncvar_add(nc,SENSOR)
    nc<-ncvar_add(nc,SENSOR_MAKER)
    nc<-ncvar_add(nc,SENSOR_MODEL)
    nc<-ncvar_add(nc,SENSOR_SERIAL_NO)
  }
  
  if(!is.na(n_param)){
    nc<-ncvar_add(nc,PARAMETER)
    nc<-ncvar_add(nc,PARAMETER_SENSOR)
    nc<-ncvar_add(nc,PARAMETER_UNITS)
    nc<-ncvar_add(nc,PARAMETER_ACCURACY)
    nc<-ncvar_add(nc,PARAMETER_RESOLUTION)
  }
  
  if(!is.na(n_derivation)){
    nc<-ncvar_add(nc,DERIVATION_PARAMETER)
    nc<-ncvar_add(nc,DERIVATION_EQUATION)
    nc<-ncvar_add(nc,DERIVATION_COEFFICIENT)
    nc<-ncvar_add(nc,DERIVATION_COMMENT)
    nc<-ncvar_add(nc,DERIVATION_DATE)
  }
  
  if(!is.na(n_param_tech)){
    nc<-ncvar_add(nc,PARAMETER_TECH)
    nc<-ncvar_add(nc,PARAMETER_TECH_UNITS)
    nc<-ncvar_add(nc,PARAMETER_TECH_ACCURACY)
    nc<-ncvar_add(nc,PARAMETER_TECH_RESOLUTION)
  }
  
  if(!is.na(n_history)){
    nc<-ncvar_add(nc,HISTORY_INSTITUTION)
    nc<-ncvar_add(nc,HISTORY_STEP)
    nc<-ncvar_add(nc,HISTORY_SOFTWARE)
    nc<-ncvar_add(nc,HISTORY_SOFTWARE_RELEASE)
    nc<-ncvar_add(nc,HISTORY_REFERENCE)
    nc<-ncvar_add(nc,HISTORY_DATE)
    nc<-ncvar_add(nc,HISTORY_ACTION)
    nc<-ncvar_add(nc,HISTORY_PARAMETER)
    nc<-ncvar_add(nc,HISTORY_PREVIOUS_VALUE)
    nc<-ncvar_add(nc,HISTORY_START_TIME_INDEX)
    nc<-ncvar_add(nc,HISTORY_STOP_TIME_INDEX)
    nc<-ncvar_add(nc,HISTORY_QCTEST)
  }
  
  ############################################# PUT VARIABLE IN FILE ###################################################
  # var_array(df='tag_points',variable='time',variable_dims=list('Time'),standard_dims=list(TIME),varlabel='time',varid='JULD',ncfile=nc)
  
  # standard_dims<-list(time)
  # dataDimF=NULL
  # for (ss in 1:length(standard_dims)){
  #   dataDimF=c(dataDimF,length(standard_dims[[ss]]))
  # }
  
  ## convert tag_DATA$time to UTC origin 1950-01-01 00:00:00!!
  ncvar_put(nc=nc, varid='juld', vals=time,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='juld_qc', vals=tag_data$juld_qc,start=c(1),count=c(length(time)))
  
  
  ncvar_put(nc=nc, varid='latitude', vals=tag_points$lat,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='longitude', vals=tag_points$lon,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='position_qc', vals=tag_data$position_qc,start=c(1),count=c(length(time)))
  
  ncvar_put(nc=nc, varid="latitude_uncertainty_1", vals=tag_polygons_1$lat,start=c(1,1),count=c(length(n_values_1),length(time_uncertainty)))
  ncvar_put(nc=nc, varid="latitude_uncertainty_2", vals=tag_polygons_2$lat,start=c(1,1),count=c(length(n_values_2),length(time_uncertainty)))
  ncvar_put(nc=nc, varid="latitude_uncertainty_3", vals=tag_polygons_3$lat,start=c(1,1),count=c(length(n_values_3),length(time_uncertainty)))
  
  ncvar_put(nc=nc, varid="longitude_uncertainty_1", vals=tag_polygons_1$lon,start=c(1,1),count=c(length(n_values_1),length(time_uncertainty)))
  ncvar_put(nc=nc, varid="longitude_uncertainty_2", vals=tag_polygons_2$lon,start=c(1,1),count=c(length(n_values_2),length(time_uncertainty)))
  ncvar_put(nc=nc, varid="longitude_uncertainty_3", vals=tag_polygons_3$lon,start=c(1,1),count=c(length(n_values_3),length(time_uncertainty)))
  
  ## MEASUREMENT VARIABLES, 
  ncvar_put(nc=nc, varid='depth', vals=tag_points$depth_obs,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='depth_qc', vals=tag_data$depth_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='depth_uncertainty', vals=tag_data$depth_uncertainty,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='depth_adjusted', vals=tag_data$depth_adjusted,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='depth_adjusted_qc', vals=tag_data$depth_adjusted_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='depth_adjusted_uncertainty', vals=tag_data$depth_adjusted_uncertainty,start=c(1),count=c(length(time)))
  
  ncvar_put(nc=nc, varid='sea_water_temperature', vals=tag_points$sst_obs,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sea_water_temperature_qc', vals=tag_data$sea_water_temperature_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sea_water_temperature_uncertainty', vals=tag_data$sea_water_temperature_uncertainty,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sea_water_temperature_adjusted', vals=tag_data$sea_water_temperature_adjusted,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sea_water_temperature_adjusted_qc', vals=tag_data$sea_water_temperature_adjusted_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sea_water_temperature_adjusted_uncertainty', vals=tag_data$sea_water_temperature_adjusted_uncertainty,start=c(1),count=c(length(time)))
  
  ## change to UTC, origin=1950-01-01 00:00:00
  ncvar_put(nc=nc, varid='sunrise', vals=as.numeric(as.POSIXct(strptime(tag_points$sunrise, "%d-%b-%Y %H:%M:%S"), tz="GMT")),start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunrise_qc', vals=tag_data$sunrise_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunrise_uncertainty', vals=tag_data$sunrise_uncertainty,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunrise_adjusted', vals=tag_data$sunrise_adjusted,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunrise_adjusted_qc', vals=tag_data$sunrise_adjusted_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunrise_adjusted_uncertainty', vals=tag_data$sunrise_adjusted_uncertainty,start=c(1),count=c(length(time)))
  
  ## change to UTC, origin=1950-01-01 00:00:00
  ncvar_put(nc=nc, varid='sunset', vals=as.numeric(as.POSIXct(strptime(tag_points$sunset, "%d-%b-%Y %H:%M:%S"), tz="GMT")),start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunset_qc', vals=tag_data$sunset_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunset_uncertainty', vals=tag_data$sunset_uncertainty,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunset_adjusted', vals=tag_data$sunset_adjusted,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunset_adjusted_qc', vals=tag_data$sunset_adjusted_qc,start=c(1),count=c(length(time)))
  ncvar_put(nc=nc, varid='sunset_adjusted_uncertainty', vals=tag_data$sunset_adjusted_uncertainty,start=c(1),count=c(length(time)))
  
  
  ###################s DOUBLE CHECK THE REST FOR CORRECT DIMENSIONS !! ################## <- AE START HERE
  ## THESE WILL BE MANDATORY WHEN INFORMATION IS AVAILABLE
  if(time_tech>0){
    ncvar_put(nc=nc, varid='juld_tech', vals=as.numeric(as.POSIXct(strptime(tag_data$juld_tech, "%d-%b-%Y %H:%M:%S"), tz="GMT")),start=c(1),count=c(length(time_tech)))
    # <PARAM_TECH>=,
    # <PARAM_TECH>_QC=,
  }
  
  
  ## IRIDIUM VARIABLES, all attributes but 'comments' are mandatory. 
  if(time_satellite>0){
    ncvar_put(nc=nc, varid='juld_satellite', vals=as.numeric(as.POSIXct(strptime(tag_data$juld_satellite, "%d-%b-%Y %H:%M:%S"), tz="GMT")),start=c(1),count=c(length(time_satellite)))
    ncvar_put(nc=nc, varid='latitude_satellite', vals=tag_data$latitude_satellite,start=c(1),count=c(length(time_satellite)))
    ncvar_put(nc=nc, varid='longitude_satellite', vals=tag_data$longitude_satellite,start=c(1),count=c(length(time_satellite)))
    ncvar_put(nc=nc, varid='cep_radius_satellite', vals=tag_data$cep_radius_satellite,start=c(1),count=c(length(time_satellite)))
  }
  
  ncvar_put(nc=nc, varid='tag_number', vals=tag_data$tag_number,start=c(1),count=c(length(string8)))
  ncvar_put(nc=nc, varid='imei', vals=tag_data$imei,start=c(1),count=c(length(string256)))
  ncvar_put(nc=nc, varid='tag_maker', vals=tag_data$tag_maker,start=c(1),count=c(length(string256)))
  ncvar_put(nc=nc, varid='firmware_version', vals=tag_data$firmware_version,start=c(1),count=c(length(string32)))
  ncvar_put(nc=nc, varid='manual_version', vals=tag_data$manual_version,start=c(1),count=c(length(string16)))
  ncvar_put(nc=nc, varid='tag_serial_no', vals=tag_data$tag_serial_no,start=c(1),count=c(length(string32)))
  ncvar_put(nc=nc, varid='dac_format_id', vals=tag_data$dac_format_id,start=c(1),count=c(length(string16)))
  ncvar_put(nc=nc, varid='wmo_inst_type', vals=tag_data$wmo_inst_type,start=c(1),count=c(length(string4)))
  
  ncvar_put(nc=nc, varid='anomaly', vals=tag_data$anomaly,start=c(1),count=c(length(string256)))
  ncvar_put(nc=nc, varid='battery_type', vals=tag_data$battery_type,start=c(1),count=c(length(string64)))
  ncvar_put(nc=nc, varid='battery_packs', vals=tag_data$battery_packs,start=c(1),count=c(length(string64)))
  ncvar_put(nc=nc, varid='controller_board_type', vals=tag_data$controller_board_type,start=c(1),count=c(length(string32)))
  ncvar_put(nc=nc, varid='controller_board_serial_no', vals=tag_data$controller_board_serial_no,start=c(1),count=c(length(string32)))
  ncvar_put(nc=nc, varid='special_features', vals=tag_data$special_features,start=c(1),count=c(length(string1024)))
  ncvar_put(nc=nc, varid='tag_owner', vals=tag_data$tag_owner,start=c(1),count=c(length(string64)))
  ncvar_put(nc=nc, varid='operating_institution', vals=tag_data$operating_institution,start=c(1),count=c(length(string64)))
  ncvar_put(nc=nc, varid='customisation', vals=tag_data$customisation,start=c(1),count=c(length(string1024)))
  
  ## TAG DEPLOYMENT AND MISSION CHARACTERISTICS
  ncvar_put(nc=nc, varid='deployment_date', vals=tag_data$deployment_date,start=c(1),count=c(length(date_time)))
  # HOW to put variables with no dimension? 
  # ncvar_put(nc=nc, varid='deployment_latitude', vals=tag_data$deployment_latitude,start=c(1),count=c(length(X)))## NO DIM
  # ncvar_put(nc=nc, varid='deployment_longitude', vals=tag_data$deployment_longitude,start=c(1),count=c(length(X)))## NO DIM
  # ncvar_put(nc=nc, varid='deployment_qc', vals=tag_data$deployment_qc,start=c(1),count=c(length(X))) ## NO DIM
  
  ncvar_put(nc=nc, varid='start_date', vals=tag_data$start_date,start=c(1),count=c(length(date_time)))
  # ncvar_put(nc=nc, varid='start_date_qc', vals=tag_data$start_date_qc,start=c(1),count=c(length(X)))## NO DIM
  ncvar_put(nc=nc, varid='startup_date', vals=tag_data$startup_date,start=c(1),count=c(length(date_time)))
  ncvar_put(nc=nc, varid='deployment_platform', vals=tag_data$deployment_platform,start=c(1),count=c(length(string32)))
  ncvar_put(nc=nc, varid='deployment_cruise_id', vals=tag_data$deployment_cruise_id,start=c(1),count=c(length(string32)))
  ncvar_put(nc=nc, varid='end_mission_date', vals=tag_data$end_mission_date,start=c(1),count=c(length(date_time)))
  # ncvar_put(nc=nc, varid='end_mission_status', vals=tag_data$end_mission_status,start=c(1),count=c(length(X))) ## NO DIM
  
  ## TAG CONFIGURATION PARAMETERS
  if(!is.na(n_config_param)){
    ncvar_put(nc=nc, varid='config_parameter_name', vals=tag_data$config_parameter_name,start=c(1),count=c(length(n_config_param),length(string128)))
    ncvar_put(nc=nc, varid='config_parameter_date', vals=tag_data$config_parameter_date,start=c(1),count=c(length(n_mission)))
    ncvar_put(nc=nc, varid='config_parameter_value', vals=tag_data$config_parameter_value,start=c(1),count=c(length(n_mission),length(n_config_param)))
  }
  
  ## TAG SENSOR INFORMATION
  if(!is.na(n_sensor)){
    ncvar_put(nc=nc, varid='sensor', vals=tag_data$sensor,start=c(1),count=c(length(n_sensor),length(string32)))
    ncvar_put(nc=nc, varid='sensor_maker', vals=tag_data$sensor_maker,start=c(1),count=c(length(n_sensor),length(string256)))
    ncvar_put(nc=nc, varid='sensor_model', vals=tag_data$sensor_model,start=c(1),count=c(length(n_sensor),length(string256)))
    ncvar_put(nc=nc, varid='sensor_serial_no', vals=tag_data$sensor_serial_no,start=c(1),count=c(length(n_sensor),length(string16)))
  }
  
  # ## TAG PARAMETER INFORMATION
  # if(!is.na(n_param)){
  #   ncvar_put(nc=nc, varid='parameter', vals=tag_data$parameter,start=c(1),count=c(length(n_param),length(string64)))
  #   ncvar_put(nc=nc, varid='parameter_sensor', vals=tag_data$parameter_sensor,start=c(1),count=c(length(n_param),length(string128)))
  #   ncvar_put(nc=nc, varid='parameter_units', vals=tag_data$parameter_units,start=c(1),count=c(length(n_param),length(string32)))
  #   ncvar_put(nc=nc, varid='parameter_accuracy', vals=tag_data$parameter_accuracy,start=c(1),count=c(length(n_param),length(string32)))
  #   ncvar_put(nc=nc, varid='parameter_resolution', vals=tag_data$parameter_resolution,start=c(1),count=c(length(n_param),length(string32)))
  # }
  
  ## TAG PARAMETER DERIVATION AND CALIBRATION INFORMATION
  # DERIVATION_PARAMETER
  if(!is.na(n_derivation)){
    ncvar_put(nc=nc, varid='derivation_parameter', vals=tag_data$derivation_parameter,start=c(1),count=c(length(n_derivation),length(string64)))
    ncvar_put(nc=nc, varid='derivation_equation', vals=tag_data$derivation_equation,start=c(1),count=c(length(n_derivation),length(string4096)))
    ncvar_put(nc=nc, varid='derivation_coefficient', vals=tag_data$derivation_coefficient,start=c(1),count=c(length(n_derivation),length(string4096)))
    ncvar_put(nc=nc, varid='derivation_comment', vals=tag_data$derivation_comment,start=c(1),count=c(length(n_derivation),length(string4096)))
    ncvar_put(nc=nc, varid='derivation_date', vals=tag_data$derivation_date,start=c(1),count=c(length(n_derivation),length(date_time)))
  }
  
  # TAG TECHNICAL PARAMETER INFORMATION
  if(!is.na(n_param_tech)){
    ncvar_put(nc=nc, varid='parameter_tech', vals=tag_data$parameter_tech,start=c(1),count=c(length(n_param_tech),length(string64)))
    ncvar_put(nc=nc, varid='parameter_tech_units', vals=tag_data$parameter_tech_units,start=c(1),count=c(length(n_param_tech),length(string32)))
    ncvar_put(nc=nc, varid='parameter_tech_accuracy', vals=tag_data$parameter_tech_accuracy,start=c(1),count=c(length(n_param_tech),length(string32)))
    ncvar_put(nc=nc, varid='parameter_tech_resolution', vals=tag_data$parameter_tech_resolution,start=c(1),count=c(length(n_param_tech),length(string32)))
  }
  
  ## HISTORY INFORMATION
  if(!is.na(n_history)){
    ncvar_put(nc=nc, varid='history_institution', vals=tag_data$history_institution,start=c(1),count=c(length(n_history),length(string2)))
    ncvar_put(nc=nc, varid='history_step', vals=tag_data$history_step,start=c(1),count=c(length(n_history),length(string4)))
    ncvar_put(nc=nc, varid='history_software', vals=tag_data$history_software,start=c(1),count=c(length(n_history),length(string8)))
    ncvar_put(nc=nc, varid='history_software_release', vals=tag_data$history_software_release,start=c(1),count=c(length(n_history),length(string4)))
    ncvar_put(nc=nc, varid='history_reference', vals=tag_data$history_reference,start=c(1),count=c(length(n_history),length(string64)))
    ncvar_put(nc=nc, varid='history_date', vals=tag_data$history_date,start=c(1),count=c(length(n_history),length(date_time)))
    ncvar_put(nc=nc, varid='history_action', vals=tag_data$history_action,start=c(1),count=c(length(n_history),length(string64)))
    ncvar_put(nc=nc, varid='history_parameter', vals=tag_data$history_parameter,start=c(1),count=c(length(n_history),length(string16)))
    ncvar_put(nc=nc, varid='history_previous_value', vals=tag_data$history_previous_value,start=c(1),count=c(length(n_history)))
    ncvar_put(nc=nc, varid='history_start_time_index', vals=tag_data$history_start_time_index,start=c(1),count=c(length(n_history)))
    ncvar_put(nc=nc, varid='history_stop_time_index', vals=tag_data$history_stop_time_index,start=c(1),count=c(length(n_history)))
    ncvar_put(nc=nc, varid='history_qctest', vals=tag_data$history_qctest,start=c(1),count=c(length(n_history),length(string16)))
  }
  
  
  ####################################### WRITE GLOBAL ATTRIBUTES ############################################
  metadata=metadata_inputs_tag(tag_spec_path=paste0(script_dir,'/tag_specifications.csv'),run=tag_spec_column,tag=tag_points)
  write_global_attributes_tag(metadata,nc,sp_resolution=1)
  
  ###################################### WRITE VARIABLE ATTRIBUTES ###########################################
  ## a vector of character strings
  variables=varid_list_create(time_tech,time_satellite, n_config_param, n_sensor,n_param,n_derivation,n_param_tech,n_history)
  
  # # read in the csv of the variable attributes
  # variable_att=read.csv(paste(script_dir,'tag_variable_specifications.csv',sep=''),sep=',',header=TRUE)
  
  for (v in 1:length(variables)){
    print(paste0('writing variable attributes for ',variables[v]))
    variable_metadata<-var_att_inputs_tag(paste0(script_dir,'tag_variable_specifications.csv'),varid=variables[v])
    write_variable_attributes_tag(variable_metadata,varid=variables[v],nc)
  }
  nc_close(nc)
}


# ##### RELOADS UPDATED ss3.24.R to INFRASTRUCTURE ####
# overwrite<-T #SET TO "TRUE" IF THE FILES ALREADY ON THE WORKSPACE SHOULD BE OVERWRITTEN
# outputs_WS <- paste("/Home",username,"Workspace/VRE Folders/IOTC_SS3/ss3_public/Rscripts/",sep="/")
# listWS(outputs_WS) #GET THE LIST OF FILES AND FOLDERS IN ONE SUB-FOLDER
# out2netcdf_outputs_SS3=paste(home_dir,'/Rscripts/OutputSS2NetCDF_standard.R',sep='') # FILE WITH THE FUNCTION TO WRITE OGC 19115 metadata
# uploadWS(outputs_WS,out2netcdf_outputs_SS3,overwrite)
# #####################################################