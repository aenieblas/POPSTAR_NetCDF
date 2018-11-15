varid_list_create<-function(
  time_tech=NA,
  time_satellite=NA,
  n_config_param=NA,
  n_sensor=NA,
  n_param=NA,
  n_derivation=NA,
  n_param_tech=NA,
  n_history=NA
){  
  ## a vector of character strings
  variables=as.character(c('juld', 
                           'juld_qc', 
                           'latitude',
                           'longitude',
                           'position_qc', 
                           # 'POSITION_UNCERTAINTY', 
                           ## MEASUREMENT VARIABLES, 
                           'depth', 
                           'depth_qc',
                           'depth_uncertainty',
                           'depth_adjusted',
                           'depth_adjusted_qc',
                           'depth_adjusted_uncertainty',
                           'sea_water_temperature', 
                           'sea_water_temperature_qc',
                           'sea_water_temperature_uncertainty',
                           'sea_water_temperature_adjusted',
                           'sea_water_temperature_adjusted_qc',
                           'sea_water_temperature_adjusted_uncertainty',
                           'sunrise', 
                           'sunrise_qc',
                           'sunrise_uncertainty',
                           'sunrise_adjusted',
                           'sunrise_adjusted_qc',
                           'sunrise_adjusted_uncertainty',
                           'sunset', 
                           'sunset_qc',
                           'sunset_uncertainty',
                           'sunset_adjusted',
                           'sunset_adjusted_qc',
                           'sunset_adjusted_uncertainty',
                           ## METADATA VARIABLES, mandatory variables and attributes TO BE DEFINED (as of August 2018)
                           ## TAG CHARACTERISTICS
                           'tag_number', 
                           'imei',
                           'tag_maker',
                           'firmware_version',
                           'manual_version',
                           'tag_serial_no',
                           'dac_format_id',
                           'wmo_inst_type',
                           'anomaly',
                           'battery_type',
                           'battery_packs',
                           'controller_board_type',
                           'controller_board_serial_no',
                           'special_features',
                           'tag_owner',
                           'operating_institution',
                           'customisation',
                           ## TAG DEPLOYMENT AND MISSION CHARACTERISTICS
                           'deployment_date',
                           'deployment_latitude',
                           'deployment_longitude',
                           'deployment_qc',
                           'start_date',
                           'start_date_qc',
                           'startup_date',
                           'deployment_platform',
                           'deployment_cruise_id',
                           'end_mission_date',
                           'end_mission_status'))
  
  if(time_tech>0){
    variables=c(variables,'juld_tech')
    # <PARAM_TECH>=,
    # <PARAM_TECH>_QC=,
  }
  ## IRIDIUM VARIABLES, all attributes but 'comments' are mandatory.
  if(time_satellite>0){
    variables=c(variables,'juld_satellite',
                'latitude_satellite',
                'longitude_satellite',
                'cep_radius_satellite')
  }
  ## TAG CONFIGURATION PARAMETERS
  if(!is.na(n_config_param)){
    variables=c(variables,'config_parameter_name',
                'config_parameter_date',
                'config_parameter_value')
  }
  
  ## TAG SENSOR INFORMATION
  if(!is.na(n_sensor)){
    variables=c(variables,'sensor',
                'sensor_maker',
                'sensor_model',
                'sensor_serial_no')
  }          
  
  ## TAG PARAMETER INFORMATION
  if(!is.na(n_param)){
    variables=c(variables,'parameter',
                'parameter_sensor',
                'parameter_units',
                'parameter_accuracy',
                'parameter_resolution')
  }
  
  ## TAG PARAMETER DERIVATION AND CALIBRATION INFORMATION
  # DERIVATION_PARAMETER
  if(!is.na(n_derivation)){
    variables=c(variables,'derivation_parameter',
                'derivation_equation',
                'derivation_coefficient',
                'derivation_comment',
                'derivation_date')
  }
  
  # TAG TECHNICAL PARAMETER INFORMATION
  if(!is.na(n_param_tech)){
    variables=c(variables,'parameter_tech',
                'parameter_tech_units',
                'parameter_tech_accuracy',
                'parameter_tech_resolution')
  }
  
  ## HISTORY INFORMATION
  if(!is.na(n_history)){
    variables=c(variables,'history_institution',
                'history_step',
                'history_software',
                'history_software_release',
                'history_reference',
                'history_date',
                'history_action',
                'history_parameter',
                'history_previous_value',
                'history_start_time_index',
                'history_stop_time_index',
                'history_qctest')
  }
  return(variables)
  
}