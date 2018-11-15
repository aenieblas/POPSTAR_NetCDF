write_global_attributes_tag <- function(metadata,ncfile,sp_resolution){
   
  #################################
  ##### GLOBAL ATTRIBUTES  ##########
  #################################
  
  
  #################################### 'WHAT' : DATA DESCRIPTION #######################################
  ########################################### WHAT: MANDATORY ##########################################
  
  ncatt_put(ncfile,0,"data_type",metadata$data_type)
  ncatt_put(ncfile,0,"format_version",metadata$format_version)
  ncatt_put(ncfile,0,"date_update",metadata$date_update)
  ncatt_put(ncfile,0,"data_mode",metadata$data_mode)
  ncatt_put(ncfile,0,"naming_authority",metadata$naming_authority)
  ncatt_put(ncfile,0,"id",metadata$id)
  
  ########################################## WHAT: RECOMMENDED #########################################
  
  ncatt_put(ncfile,0,"ices_platform_code",metadata$ices_platform_code)
  ncatt_put(ncfile,0,"source",metadata$source)
  ncatt_put(ncfile,0,"history",metadata$history)
  ncatt_put(ncfile,0,"data_mode",metadata$data_mode)
  ncatt_put(ncfile,0,"quality_index",metadata$quality_index)
  ncatt_put(ncfile,0,"references",metadata$references)
  ncatt_put(ncfile,0,"comment",metadata$comment)
  ncatt_put(ncfile,0,"Conventions",metadata$Conventions)
  ncatt_put(ncfile,0,"netcdf_version",metadata$netcdf_version)
  ncatt_put(ncfile,0,"title",metadata$title)
  ncatt_put(ncfile,0,"summary",metadata$summary)
  ncatt_put(ncfile,0,"abstract",metadata$abstract)
  ncatt_put(ncfile,0,"keywords",metadata$keywords)
  ncatt_put(ncfile,0,"naming_authority",metadata$naming_authority)
  ncatt_put(ncfile,0,"id",metadata$id)
  ncatt_put(ncfile,0,"cdm_data_type",metadata$cdm_data_type)
  
  #################################### 'WHERE' : DATA SPATIAL COVERAGE #################################
  ########################################### WHERE: RECOMMENDED #######################################
  
  ncatt_put(ncfile,0,"area",metadata$area)
  ncatt_put(ncfile,0,"geospatial_lat_min",metadata$geospatial_lat_min)
  ncatt_put(ncfile,0,"geospatial_lat_max",metadata$geospatial_lat_max)
  ncatt_put(ncfile,0,"geospatial_lon_min",metadata$geospatial_lon_min)
  ncatt_put(ncfile,0,"geospatial_lon_max",metadata$geospatial_lon_max)
  ncatt_put(ncfile,0,"geospatial_vertical_min",metadata$geospatial_vertical_min)
  ncatt_put(ncfile,0,"geospatial_vertical_max",metadata$geospatial_vertical_max)
  
  
  #################################### 'WHEN' : DATA TEMPORAL COVERAGE #################################
  ########################################### WHEN: RECOMMENDED ########################################
  
  ncatt_put(ncfile,0,"time_coverage_start",metadata$time_coverage_start)
  ncatt_put(ncfile,0,"time_coverage_end",metadata$time_coverage_end)
 
  #################################### 'WHO' : DATA PRODUCERS ##########################################
  ########################################### WHO: RECOMMENDED #########################################
  
  ncatt_put(ncfile,0,"institution",metadata$institution)
  ncatt_put(ncfile,0,"institution_references",metadata$institution_references)
  ncatt_put(ncfile,0,"sdn_edmo_code",metadata$sdn_edmo_code)
  ncatt_put(ncfile,0,"contact",metadata$contact)
  ncatt_put(ncfile,0,"author",metadata$author)
  ncatt_put(ncfile,0,"data_assembly_center",metadata$data_assembly_center)
  ncatt_put(ncfile,0,"principal_investigator",metadata$principal_investigator)
  ncatt_put(ncfile,0,"principal_investigator_email",metadata$principal_investigator_email)
  ncatt_put(ncfile,0,"project_name",metadata$project_name)
  ncatt_put(ncfile,0,"deployment_code",metadata$deployment_code)
  ncatt_put(ncfile,0,"deployment_label",metadata$deployment_label)
  
  #################################### 'HOW' : DATA PRODUCTION METHODS #################################
  ########################################### HOW: RECOMMENDED #########################################
  
  ncatt_put(ncfile,0,"distribution_statement",metadata$distribution_statement)
  ncatt_put(ncfile,0,"doi",metadata$doi)
  ncatt_put(ncfile,0,"citation",metadata$citation)
  ncatt_put(ncfile,0,"update_interval",metadata$update_interval)
  ncatt_put(ncfile,0,"qc_manual",metadata$qc_manual)
  
  ncatt_put(ncfile,0,"tag_type",metadata$tag_type)
  ncatt_put(ncfile,0,"deploy_id",metadata$deploy_id)
  ncatt_put(ncfile,0,"ptt",metadata$ptt)
  ncatt_put(ncfile,0,"instrument",metadata$instrument)
  ncatt_put(ncfile,0,"sw",metadata$sw)
  ncatt_put(ncfile,0,"percent_decoded",metadata$percent_decoded)
  ncatt_put(ncfile,0,"passes",metadata$passes)
  ncatt_put(ncfile,0,"percent_argos_loc",metadata$percent_argos_loc)
  ncatt_put(ncfile,0,"msg_per_pass",metadata$msg_per_pass)
  ncatt_put(ncfile,0,"ds",metadata$ds)
  ncatt_put(ncfile,0,"di",metadata$di)
  ncatt_put(ncfile,0,"min_power",metadata$min_power)
  ncatt_put(ncfile,0,"avg_power",metadata$avg_power)
  ncatt_put(ncfile,0,"max_power",metadata$max_power)
  ncatt_put(ncfile,0,"min_interval",metadata$min_interval)
  ncatt_put(ncfile,0,"earliest_xmit_time",metadata$earliest_xmit_time)
  ncatt_put(ncfile,0,"latest_xmit_time",metadata$latest_xmit_time)
  ncatt_put(ncfile,0,"xmit_days",metadata$xmit_days)
  ncatt_put(ncfile,0,"earliest_data_time",metadata$earliest_data_time)
  ncatt_put(ncfile,0,"latest_data_time",metadata$latest_data_time)
  ncatt_put(ncfile,0,"data_days",metadata$data_days)
  ncatt_put(ncfile,0,"release_date",metadata$release_date)
  ncatt_put(ncfile,0,"release_type",metadata$release_type)
  ncatt_put(ncfile,0,"deploy_date",metadata$deploy_date)
   
}

