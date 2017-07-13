factor_to_numeric = function(factor){
  return(as.numeric(levels(factor))[factor])
}