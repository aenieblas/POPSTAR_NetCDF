write_variable_attributes_tag<-function(var_metadata,varid,ncfile){
 
  
  ncatt_put(ncfile,varid,'long_name',var_metadata$long_name)
  ncatt_put(ncfile,varid,'standard_name',var_metadata$standard_name)
  ncatt_put(ncfile,varid,'units',var_metadata$units)
  ncatt_put(ncfile,varid,'Conventions',var_metadata$Conventions)
  ncatt_put(ncfile,varid,'missingvalue',var_metadata$missingvalue)
  ncatt_put(ncfile,varid,'valid_min',var_metadata$valid_min)
  ncatt_put(ncfile,varid,'valid_max',var_metadata$valid_max)
  ncatt_put(ncfile,varid,'comment',var_metadata$comment)
  ncatt_put(ncfile,varid,'axis',var_metadata$axis)
  ncatt_put(ncfile,varid,'ancillary_variable',var_metadata$ancillary_variable)
  ncatt_put(ncfile,varid,'accuracy',var_metadata$accuracy)
  ncatt_put(ncfile,varid,'precision',var_metadata$precision)
  ncatt_put(ncfile,varid,'cell_methods',var_metadata$cell_methods)
  ncatt_put(ncfile,varid,'DM_indicator',var_metadata$DM_indicator)
  ncatt_put(ncfile,varid,'reference_scale',var_metadata$reference_scale)
  ncatt_put(ncfile,varid,'coordinate_reference',var_metadata$coordinate_reference)
  ncatt_put(ncfile,varid,'sdn_parameter_urn',var_metadata$sdn_parameter_urn)
  ncatt_put(ncfile,varid,'sdn_uom_urn',var_metadata$sdn_uom_urn)
  

}
# Note on vertical axis
# The PRES variable is usually a vertical axis. Its axis attribute is “Z” : PRES:axis=”Z”.
# It has a “positive” mandatory attribute set to “down”.

