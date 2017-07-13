compute_causal_effect = function(df_input, df_output, nround, obj_train, params){
  require(xgboost)
  library(xgboost)
  data_train = xgb.DMatrix(data=data.matrix(df_input), label=df_output,missing='NA')
  model_train <- xgboost(data = data_train, objective = obj_train, nrounds=nround,params=params, verbose=1)  
  return(model_train)
}