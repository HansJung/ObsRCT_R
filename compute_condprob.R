compute_condprob = function(given, val_name, sub_idx, 
                            adj_idx, adj_name, adj_val, 
                            interv_name, PP_ctrl_limit
                            model,Y_ind  ){
  # Input 
  ## given = list of all given list("PEEP"=PEEP_given)
  ## val_name = "PEEP"
  ## sub_idx = i
  ## adj_idx = j 
  ## adj_name = c("KG","NMBA",etc)
  ## adj_val = c(KG, NMBA, etc)
  ## interv_vallist c("VT","PEEP")
  
  data_sub = given[[val_name]][sub_idx,]
  adj_len = length(adj_val)
  for (i in 1:adj_len){
    data_sub[adj_name[i]] = adj_val[i][adj_idx]
  }
  
  if( is.element("VT", interv_name)){if (!is.na(data_sub['VT'])){ data_sub['VT'] = interv_val[['VT']] }}
  if (is.element("PP", interv_name)){
    if (!is.na(data_sub['PP'])){ 
      if (data_sub['PP'] >= PP_ctrl_limit){
        data_sub['PP'] = PP_ctrl_limit 
      }
      else{
        data_sub['PP'] = data_sub['PP']
      }
    }
  }
  
  
    if (!is.na(data_sub['PEEP'])){ 
      sub_FO2 = round(Y_given[i,]['FO2'],1)
      if (is.nan(sub_FO2) || is.na(sub_FO2)){
        next
      }
      if (sub_FO2 < 0.3){
        data_sub['PEEP'] = min(5, data_sub['PEEP'])
      }
      else if (sub_FO2 > 0.9){
        data_sub['PEEP'] = min(24, data_sub['PEEP'])
        data_sub['PEEP'] = max(18, data_sub['PEEP'])
      }
      else if (!is.na(sub_FO2)){
        data_sub['PEEP'] = PEEPs[which(FiO2s == sub_FO2)]
      }
      else{
        data_sub['PEEP'] = data_sub['PEEP']
      }
    }
  
    
}