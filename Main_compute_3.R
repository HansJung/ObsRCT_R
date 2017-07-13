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
NMBA = df$DOSAGE
KG = factor_to_numeric(df$WEIGHT)

# 1. Build a model for f(Y | Y_given)
## Y_given = [NMBA, FO2, PP, PEEP, VT, MR, Sev]
eta_val = 1
depth_val = 10 
n_round = 10
Y_given = cbind(NMBA,FO2, PP, PEEP, VT, MR, Sev)
Y.cond = compute_causal_effect(Y_given,Y,"binary:logistic",eta_val,depth_val,n_round)

# 2. Build a model for f(PEEP | PEEP_given )
## PEEP_given = [KG, Sex, Sev, PP, FO2]
eta_val = 1
depth_val = 10 
n_round = 10
PEEP_given = cbind(KG, Sex, Sev, PP, FO2)
PEEP.cond = compute_causal_effect(PEEP_given,PEEP,"reg:linear",eta_val,depth_val,n_round)

# 3. Build a model for f(PP | PP_given)
## PP_given = [Sev, KG, NMBA, FO2]
eta_val = 1
depth_val = 10
n_round = 10
PP_given  = cbind(Sev,KG,NMBA,FO2)
PP.cond = compute_causal_effect(PP_given,PP,"reg:linear",eta_val,depth_val,n_round)


# 4. Build a model for FO2 f(FO2 | FO2_given)
## FO2_given = [KG, Sex, Sev, NMBA]
eta_val = 1
depth_val = 10
n_round = 10
FO2_given  = cbind(KG,Sex,Sev,NMBA)
FO2.cond = compute_causal_effect(FO2_given,FO2,"reg:linear",eta_val,depth_val,n_round)

# 5. Build a model for f(VT | VT_given) 
## VT_given = [Sev,NMBA,FO2,PP, PEEP]
eta_val = 1
depth_val = 10 
n_round = 10
VT_given = cbind(Sev,NMBA,FO2,PP,PEEP)
VT.cond = compute_causal_effect(VT_given,VT,"reg:linear",eta_val,depth_val,n_round)


# 6. Find Fit
if (fitting == T){
  Model = Age.cond
  given = Age_given
  Data = Age
  
  Fit = predict(Model,data.matrix(given))
  Diff = Fit - Data
  Diff = Diff[which(!is.na(Diff))]
  
  fit_fun = fitdist(Diff,'norm')
}


# 6. Computation of statistics - PEEP case
N = 8587

case_VT = T
case_PEEP = T
case_PP = T
case_MR = T 
case_NMBA = T 

## Variable fixation 
VT_val = 6 
PP_val = 30 
PP_ctrl_limit = 30
FiO2s = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
NMBA_val = 150

### Control
Experiment = F # T means PEEP high

## If old 
PEEPs = c(5,8,10,10,12,14,16) # Conv
PEEP_min = 5 
PEEP_max_min = 18 
PEEP_max_max = 24   

## If new 
if (Experiment == T){
  PEEPs = c(12,14,16,20,20,22,22) # New 
  PEEP_min = 12 
  PEEP_max_min = 22 
  PEEP_max_max = 24   
}

# Sample for computing the average 
sample_N = 10
NMBA_N = sample(1:531,sample_N)
nonNMBA_N = sample(532:8587,sample_N)
samples = c(NMBA_N,nonNMBA_N)
Probs = rep(0,2*sample_N)
idx = 1

for(i in samples){
  # Added and divided for adjusting. 
  sum_numer = 0
  sum_denom = 0
  
  for (j in 1:N){
    # 1. Adjusting setting for Y
    ## Adjusting = [Sev]
    ## Y_given = [NMBA, FO2, PP, PEEP, VT, MR, Sev]
    data_sub = Y_given[i, ]
    data_sub['Sev'] = Sev[j]
    
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
        data_sub['PEEP'] = min(PEEP_min, data_sub['PEEP'])
      }
      else if (sub_FO2 > 0.9){
        data_sub['PEEP'] = min(PEEP_max_max, data_sub['PEEP'])
        data_sub['PEEP'] = max(PEEP_max_min, data_sub['PEEP'])
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
    if (case_NMBA){ data_sub['NMBA'] = NMBA_val }
    
    # Probability computation 
    Model = Y.cond
    Obs = Y[i]
    prob_y = predict(Model, t(as.matrix(data_sub)))
    
    ##############################################################################
    # 2. Adjusting setting for PEEP 
    ## Adjusting = [Sev]
    ## PEEP_given = [KG, Sex, Sev, PP, FO2]
    acc_mu = 5.2
    acc_sd = 2.62
    data_sub = PEEP_given[i,]
    data_sub['Sev'] = Sev[j]

    # If NA exists, go over to next. 
    if (any(is.na(data_sub))){
      next
    }
    
    # Intervention setting 
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
        data_sub['PEEP'] = min(PEEP_min, data_sub['PEEP'])
      }
      else if (sub_FO2 > 0.9){
        data_sub['PEEP'] = min(PEEP_max_max, data_sub['PEEP'])
        data_sub['PEEP'] = max(PEEP_max_min, data_sub['PEEP'])
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
    # 3. Adjusting setting for PP 
    ## Adjusting = [Sev]
    ## PP_given = [ Sev,Kg,NMBA,FO2 ]
    
    acc_mu = 20
    acc_sd = 5.87
    data_sub = PP_given[i,]
    data_sub['Sev'] = Sev[j]
    if (any(is.na(data_sub))){
      next
    }
    
    # Intervention setting 
    if (case_NMBA){ data_sub['NMBA'] = NMBA_val }
    
    # Probability computation 
    Model = PP.cond
    Obs = PP[i]
    prob_PP = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )
    
    
    ##############################################################################
    # 4. Adjusting setting for FO2 
    ## Adjusting = [Sev]
    ## FO2_given = [KG,Sex,Sev,NMBA]
    acc_mu = 0
    acc_sd = 0.11
    data_sub = FO2_given[i,]
    data_sub['Sev'] = Sev[j]
    
    if (any(is.na(data_sub))){
      next
    }

    # Intervention setting 
    if (case_NMBA){ data_sub['NMBA'] = NMBA_val }
    
    # Probability computation 
    Model = FO2.cond
    Obs = FO2[i]
    prob_FO2 = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )
    
    
    ##############################################################################
    # 5. Adjusting setting for VT 
    ## Adjusting = [Sev]
    ## VT_given = [Sev,NMBA,FO2,PP,PEEP]
    acc_mu = 1.13
    acc_sd = 3.54
    data_sub = VT_given[i,]
    data_sub['Sev'] = Sev[j]
    
    # Intervention setting 
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
        data_sub['PEEP'] = min(PEEP_min, data_sub['PEEP'])
      }
      else if (sub_FO2 > 0.9){
        data_sub['PEEP'] = min(PEEP_max_max, data_sub['PEEP'])
        data_sub['PEEP'] = max(PEEP_max_min, data_sub['PEEP'])
      }
      else if (!is.na(sub_FO2)){
        data_sub['PEEP'] = PEEPs[which(FiO2s == sub_FO2)]
      }
      else{
        data_sub['PEEP'] = data_sub['PEEP']
      }
    }
    if (case_NMBA){ data_sub['NMBA'] = NMBA_val }
    
    # Probability computation 
    Model = VT.cond
    Obs = VT[i]
    prob_VT = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )
    if (any(is.na(data_sub))){
      next
    }
    
    ##############################################################################
    sumval_numer = prob_y*prob_PEEP*prob_PP*prob_FO2*prob_VT
    sumval_denom = prob_PEEP*prob_PP*prob_FO2*prob_VT
    
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
