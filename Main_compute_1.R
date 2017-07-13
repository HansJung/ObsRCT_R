source("compute_causal_effect.R")
source("factor_to_numeric.R")

set.seed(123)
fitting = F 

df <- read.csv("Data/reduce_final_R.csv")
ID = df$SUBJECT_ID
Y = as.numeric(df$SURVIVAL)
MR = factor_to_numeric(df$ResRate)
Sex = as.numeric(df$GENDER)
Sev = factor_to_numeric(df$Sev)
Age = as.numeric(df$AGE)
PEEP = factor_to_numeric(df$PEEP)
VT = factor_to_numeric(df$VT_WEIGHT)
PP = factor_to_numeric(df$PP)
FO2 = factor_to_numeric(df$FiO2)

# 1. Build a model for f(Y | Y_given)
## Y_given = [ Sex, Sev, Age, FO2, PP, PEEP, VT, MR ]
eta_val = 1
depth_val = 10 
n_round = 10
Y_given = cbind(Sex,Sev,Age,FO2,PP,PEEP,VT,MR)
Y.cond = compute_causal_effect(Y_given,Y,"binary:logistic",eta_val,depth_val,n_round)


# 2. Build a model for f(MR | MR_given)
## where MR_given = [Sex, Sev, Age, FO2, PEEP, VT]
eta_val = 1
depth_val = 10
n_round = 2
MR_given  = cbind(Sex,Sev,Age,FO2,PP,VT)
MR.cond = compute_causal_effect(MR_given,MR,"reg:linear",eta_val,depth_val,n_round)

# 3. Build a model for f(PEEP | PEEP_given)
## where PEEP_given = [Sev, Age, FO2]
eta_val = 1
depth_val = 10 
n_round = 2
PEEP_given = cbind(Age,Sev,FO2)
PEEP.cond = compute_causal_effect(PEEP_given,PEEP,"reg:linear",eta_val,depth_val,n_round)

# 4. Build a model for f(FO2 | FO2_given)
## where FO2_given = [Age, Sev]
eta_val = 1
depth_val = 10 
n_round = 2
FO2_given = cbind(Age,Sev)
FO2.cond = compute_causal_effect(FO2_given,FO2,"reg:linear",eta_val,depth_val,n_round)

# 5. Find Fit (example: PEEP)
if (fitting == T){
  Model = PEEP.cond
  given = PEEP_given
  Data = PEEP
  
  Fit = predict(Model,data.matrix(given))
  Diff = Fit - Data
  Diff = Diff[which(!is.na(Diff))]
  
  fit_fun = fitdist(Diff,'norm')
}



# 6. Computation of statistics - VT, PP 
## Intervention? True! 
N = 8587
case_VT = T
case_PEEP = T
case_PP = T
case_MR = T 

## Variable fixation 
VT_val = 6 # (6,12)
PP_val = 30 # (30,50)
PP_ctrl_limit = 30 # (30,50)
FiO2s = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
PEEPs = c(5,8,10,10,12,14,16)

sample_N = 50
NMBA_N = sample(1:531,sample_N)
nonNMBA_N = sample(532:8587,sample_N)
samples = c(NMBA_N,nonNMBA_N)
Probs = rep(0,2*sample_N)

idx = 1
for (i in samples){
  # Added and divided for adjusting. 
  sum_numer = 0
  sum_denom = 0
  
  for (j in 1:N){
    # Adjusting setting for Y
    ## Adjusting = [Sex, Sev, Age]
    ## Y_given = [ Sex, Sev, Age, FO2, PP, PEEP, VT, MR ]
    data_sub = Y_given[i, ]
    data_sub['Sex'] = Sex[j]
    data_sub['Sev'] = Sev[j]
    data_sub['Age'] = Age[j]
    
    # If NA exists, go over to the next one, for avoiding NA output 
    ## (avoding missing variable) 
    if (any(is.na(data_sub))){
      next
    }
    
    # Intervention seteting 
    if (case_VT){ data_sub['VT'] = VT_val }
    if (case_PP){ 
      if (data_sub['PP'] >= PP_ctrl_limit){
        data_sub['PP'] = PP_val 
      }
      else{
        data_sub['PP'] = data_sub['PP']
      }
    }
    if (case_PEEP){ 
      sub_FO2 = round(Y_given[i,]['FO2'],1)
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
    if (case_MR){
      if (!is.na(data_sub['MR'] )){
        data_sub['MR'] = max(6,data_sub['MR'])
        data_sub['MR'] = min(35, data_sub['MR'])
      }
    }
    
    # Probability computation 
    Model = Y.cond
    prob_y = predict(Model, t(as.matrix(data_sub)))

    ##############################################################################
    # Adjusting setting for PEEP 
    ## Adjusting = [Sex, Sev, Age]
    ## PEEP_given = [Sev, Age, FO2]
    acc_mu = 5.424
    acc_sd = 2.6
    data_sub = PEEP_given[i,]
    data_sub['Sev'] = Sev[j]
    data_sub['Age'] = Age[j]
    
    # If NA exists, go over to next. 
    if (any(is.na(data_sub))){
      next
    }
    # Intervention setting to PEEP 
    if (case_PEEP){ 
      sub_FO2 = round(Y_given[i,]['FO2'],1)
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
    
    # Probability computation 
    Model = PEEP.cond
    Obs = PEEP[i]
    prob_PEEP = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub)))-acc_mu, sd= acc_sd )

    
    ##############################################################################
    # Adjusting setting for MR 
    ## Adjusting = [Sex, Sev, Age]
    ## MR_given = [Sex, Sev, Age, FO2, PEEP, VT]
    
    acc_mu = 20
    acc_sd = 5.87
    data_sub = MR_given[i,]
    data_sub['Sex'] = Sex[j]
    data_sub['Sev'] = Sev[j]
    data_sub['Age'] = Age[j]
    if (any(is.na(data_sub))){
      next
    }
    # Intervention setting 
    if (case_VT){ data_sub['VT'] = VT_val }
    if (case_PEEP){ 
      sub_FO2 = round(Y_given[i,]['FO2'],1)
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
    
    # Probability computation 
    Model = MR.cond
    Obs = MR[i]
    prob_MR = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )

    ##############################################################################
    # Adjusting setting for FO2 
    ## Adjusting = [Sex, Sev, Age]
    ## FO2_given = [Age, Sev]
    acc_mu = 0
    acc_sd = 0.15
    data_sub = FO2_given[i,]
    data_sub['Sev'] = Sev[j]
    data_sub['Age'] = Age[j]
    
    # Probability computation 
    Model = FO2.cond
    Obs = FO2[i]
    
    prob_FO2 = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )
    if (any(is.na(data_sub))){
      next
    }
    
    ##############################################################################
    sumval_numer = prob_y*prob_PEEP*prob_MR*prob_FO2
    sumval_denom = prob_PEEP*prob_MR*prob_FO2
    
    # print(c(as.integer(j),prob_PEEP, prob_MR, prob_FO2, sumval_denom))
    
    if (sumval_denom == 0){
      next
    }
    sum_numer = sum_numer + sumval_numer
    sum_denom = sum_denom + sumval_denom
  }
  prob = sum_numer/sum_denom
  print(c(idx,prob))
  Probs[idx] = prob
  idx = idx + 1
}
avg_survival = round(mean(Probs[which(!is.na(Probs))]),3)

  # print(c(ID[i],prob,Y[i]))
