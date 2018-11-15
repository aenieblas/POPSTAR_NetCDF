# var_att_inputs_tag.R

var_att_inputs_tag<-function(var_spec_path,varid){
  
  # load csv with variable attributes specified.
  var_spec1=read.csv(var_spec_path,header=TRUE,sep=',')
  ## replace empty cells with NAs
  var_spec2 <- lapply(var_spec1, function(x){
  x[x == ""] <- NA
  return(x)
})
  var_spec<-as.data.frame(var_spec2,stringsAsFactors = FALSE)
  
  n=which(var_spec$varid==varid)

  var_metadata<-NULL
  var_metadata$long_name                    <- var_spec[n,'long_name']
  var_metadata$standard_name                <- var_spec[n,'standard_name']
  
  if(!is.na(var_spec[n,'units'])){
    var_metadata$units                      <- var_spec[n,'units']}
  if(!is.na(var_spec[n,'conventions'])){
    var_metadata$Conventions                <- var_spec[n,'conventions']}
  if(!is.na(var_spec[n,'X_FillValue'])){
    var_metadata$missingvalue               <- var_spec[n,'X_FillValue']}
  if(!is.na(var_spec[n,'valid_min'])){
    var_metadata$valid_min                  <- var_spec[n,'valid_min']}
  if(!is.na(var_spec[n,'valid_max'])){
    var_metadata$valid_max                  <- var_spec[n,'valid_max']}
  if(!is.na(var_spec[n,'comment'])){
    var_metadata$comment                    <- var_spec[n,'comment']}
  if(!is.na(var_spec[n,'axis'])){ 
    var_metadata$axis                       <- var_spec[n,'axis']}
  if(!is.na(var_spec[n,'ancillary_variable'])){
    var_metadata$ancillary_variable         <- var_spec[n,'ancillary_variable']}
  if(!is.na(var_spec[n,'accuracy'])){
    var_metadata$accuracy                   <- var_spec[n,'accuracy']}
  if(!is.na(var_spec[n,'precision'])){
    var_metadata$precision                  <- var_spec[n,'precision']}
  if(!is.na(var_spec[n,'cell_methods'])){
    var_metadata$cell_methods               <- var_spec[n,'cell_methods']}
  if(!is.na(var_spec[n,'DM_indicator'])){
    var_metadata$DM_indicator               <- var_spec[n,'DM_indicator']}
  if(!is.na(var_spec[n,'reference_scale'])){
    var_metadata$reference_scale            <- var_spec[n,'reference_scale']}
  if(!is.na(var_spec[n,'coordinate_reference'])){
    var_metadata$coordinate_reference      <- var_spec[n,'coordinate_reference']}
  if(!is.na(var_spec[n,'coordinate_reference_frame'])){
    var_metadata$coordinate_reference_frame <- var_spec[n,'coordinate_reference_frame']}
  if(!is.na(var_spec[n,'sdn_parameter_urn'])){
    var_metadata$sdn_parameter_urn          <- var_spec[n,'sdn_parameter_urn']}
  if(!is.na(var_spec[n,'sdn_uom_urn'])){
    var_metadata$sdn_uom_urn                <- var_spec[n,'sdn_uom_urn']}  
  
  return(var_metadata)
  
}