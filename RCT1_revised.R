setwd("~/Dropbox/Personal/Research/Causal_RCT/Code/ObsRCT")
source("compute_causal_effect.R")
source("factor_to_numeric.R")
library(mixtools)
library(fitdistrplus)

set.seed(100)
fitting = F
computing = T

train_Y = F
train_PIP = F
train_FO2 = F
train_FO2deno = F
train_NMBA = F


df <- read.csv("Data/reduce_final_R.csv")
ID = df$SUBJECT_ID
KG = factor_to_numeric(df$WEIGHT)
Age = as.numeric(df$AGE)
Sex = as.numeric(df$GENDER)
NMBA = df$DOSAGE
Sev = factor_to_numeric(df$Sev)
FO2 = factor_to_numeric(df$FiO2)
PEEP = factor_to_numeric(df$PEEP)
VT = factor_to_numeric(df$VT_WEIGHT)
PP = factor_to_numeric(df$PP)
RR = factor_to_numeric(df$ResRate)
PIP = factor_to_numeric(df$PIP)
MV = factor_to_numeric(df$Minute_Volume)
SO2 = factor_to_numeric(df$SpO2)
PO2 = factor_to_numeric(df$PaO2)
PCO2 = factor_to_numeric(df$PaCO2)
pH = factor_to_numeric(df$pH)
Y = as.numeric(df$SURVIVAL)




# 1. Build a model for f(Y | Y_given)
## Y_given = [ Kg,Age,Sex,NMBA,Sev,FO2,PEEP,V_T,PP,RR,PIP,MV,SO2,PO2,PCO2,pH ]
if (train_Y == T){
  param = list(booster="gbtree",eta = 0.05, gamma = 0.5, max_depth =15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  
  Y_given = cbind( RR,Sev,NMBA,PP,PEEP,VT,PIP,FO2 )
  print("Y Run!")
  Y.cond = compute_causal_effect(Y_given,Y,nround,"binary:logistic",param)
}

if (train_PIP == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 300
  PIP_given = cbind(PEEP,PP,NMBA,Sev)
  Given = PIP_given[which(!is.na(PIP))]
  Data = PIP[which(!is.na(PIP))]
  print("PIP Run!")
  PIP.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 

# 7. Build a model for f(FO2 | FO2_given), NUMERATOR 
## where FO2_given = [Age, Sev, NMBA]
if (train_FO2 == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0, max_depth = 15, 
               subsample = 0.8, lambda=0, alpha=0, verbose=0)
  nround = 1000
  FO2_given = cbind(PIP,PEEP,PP,NMBA,Sev)
  Given = FO2_given[which(!is.na(FO2))]
  Data = FO2[which(!is.na(FO2))]
  print("FO2 Run!")
  FO2.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 7. Build a model for f(FO2 | FO2_given), NUMERATOR 
## where FO2_given = [Age, Sev, NMBA]
if (train_FO2deno == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0, max_depth = 10, 
               subsample = 0.5, lambda=0, alpha=0, verbose=0)
  nround = 300
  FO2deno_given = cbind(PEEP,NMBA,Sev)
  Given = FO2deno_given[which(!is.na(FO2))]
  Data = FO2[which(!is.na(FO2))]
  print("FO2 deno Run!")
  FO2deno.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

## where FO2_given = [Age, Sev, NMBA]
if (train_NMBA == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0, max_depth = 10, 
               subsample = 0.5, lambda=0, alpha=0, verbose=0)
  nround = 300
  NMBA_given = cbind(Sev)
  Given = NMBA_given[which(!is.na(NMBA))]
  Data = NMBA[which(!is.na(NMBA))]
  print("NMBA deno Run!")
  NMBA.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}







if (fitting == T){
  Model = FO2deno.cond
  given = FO2deno_given
  Data = FO2
  
  Fit = predict(Model,data.matrix(given))
  Diff = Data - Fit
  Diff = Diff[which(!is.na(Diff))]
  
  fit_fun = fitdist(Diff,'norm')
  
  if(train_FO2 == T){
    FO2_mix_VT = normalmixEM(Diff)
  }else if(train_FO2deno == T){
    FO2deno_mix_VT = normalmixEM(Diff)
  }
  
}

# Simplified
## PIP: Diff ~ Logis, loc: 0, scale=3.7
## FO2: Diff ~ GMM 
## FO2_deno: Diff ~ Norm, mean=0, sd = 0.4



if (computing == T){  
  N = 8587
  
  ## Variable fixation 
  VT_val = 12 # (6,12)
  PP_val = 50 # (30,50)
  PP_ctrl_limit = PP_val # (30,50)
  FiO2s = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  PEEPs = c(5,8,10,10,12,14,16)
  
  sample_N = 100
  Nj = 200
  
  
  NMBA_N = sample(1:531,sample_N)
  nonNMBA_N = sample(532:8587,sample_N)
  samples = c(NMBA_N,nonNMBA_N)
  
  idx = 1
  
  
  sum_val = 0
  for (p in samples){
    data_sub = Y_given[p,]
    Y_p = Y[p]
    FO2_p = if(!is.na(data_sub['FO2'])){FO2_p = data_sub['FO2']}else{
      # print(c(p,"FO2 next"))
      next
    }
    PEEP_p = if(!is.na(data_sub['PEEP'])){
      sub_FO2 = round(Y_given[p,]['FO2'],1)
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
      PEEP_p = data_sub['PEEP']
      }else{next}
    RR_p = if(!is.na(data_sub['RR'])){
      data_sub['RR'] = max(6,data_sub['RR'])
      data_sub['RR'] = min(35, data_sub['RR'])
      RR_p = data_sub['RR']
      }else{
      next
    }
    PP_p = if(!is.na(data_sub['PP'])){
      if (data_sub['PP'] >= PP_ctrl_limit){
        data_sub['PP'] = PP_val 
      }
      else{
        data_sub['PP'] = data_sub['PP']
      }
      PP_p = data_sub['PP']
      }else{next}
    VT_p = if(!is.na(data_sub['VT'])){VT_p = data_sub['VT']}else{
      VT_p = VT_val
    }
    Sev_p = if(!is.na(data_sub['Sev'])){Sev_p = data_sub['Sev']}else{
      # print(c(p,"PP next"))
      next
    }
    NMBA_p = if(!is.na(data_sub['NMBA'])){NMBA_p = data_sub['NMBA']}else{
      # print(c(p,"PP next"))
      next
    }

    ####### Intervention to VT, PEEP, PP 
    # Intervention effect
    ## VT 
    VT_p = VT_val
    ## PP 
    if (PP_p >= PP_ctrl_limit){
      PP_p = PP_val 
    }
    else{
      PP_p = data_sub['PP']
    }
    ## PEEP 
    if (FO2_p < 0.3){
      PEEP_p = min(5, PEEP_p)
    }
    else if (FO2_p > 0.9){
      PEEP_p = min(24, PEEP_p)
      PEEP_p = max(18, PEEP_p)
    }
    else if (!is.na(FO2_p)){
      PEEP_p = PEEPs[which(FiO2s == FO2_p)]
    }
    else{
      PEEP_p = PEEP_p
    } 
    ####### END of Intervention 
    sum_fn_i = 0 
    sum_fd_i = 0
    
      
    # Compute denominator probability
    Obs_FO2deno = FO2_p -  predict( FO2deno.cond, (as.matrix(cbind(PEEP_p,NMBA_p,Sev_p)) ))
    prob_FO2deno = 0.7912333 * dnorm(Obs_FO2deno, mean= -0.09210143, sd= 0.08222925 ) + 
      0.2087667 * dnorm(Obs_FO2deno, mean = 0.36315736, sd = 0.12278200 ) 
    prob_FO2deno = prob_FO2deno[[1]]
      
    sum_fn_j = 0
    for (j in 1:Nj){
      mu_PIP = predict(PIP.cond, (as.matrix(cbind(PEEP_p,PP_p,NMBA_p,Sev_p))))
      PIP_j = rnorm(n=1,mean=mu_PIP, sd=6.8)
      Obs_FO2 = FO2_p - predict( FO2.cond, (as.matrix(cbind(PIP_j,PEEP_p,PP_p,NMBA_p,Sev_p))) )
      prob_FO2 = 0.8557091 * dnorm(Obs_FO2, mean = -0.07290717, sd = 0.10905512 ) + 
        0.1442909 * dnorm(Obs_FO2, mean=0.43332773, sd = 0.02717687 ) 
      prob_FO2 = prob_FO2[[1]]
      prob_y = predict( Y.cond, (as.matrix(cbind( RR_p,Sev_p,NMBA_p,PP_p,PEEP_p,VT_p,PIP_j,FO2_p ))) )
      f_n = log(prob_FO2) + log(prob_y)
      f_n = exp(f_n)
      sum_fn_j = sum_fn_j + f_n
    }
    prob_fn_j = sum_fn_j / Nj
    prob = exp( log(prob_fn_j) -  log(prob_FO2deno) )
    if (prob > 1){
      next
    }
      
    idx = idx + 1 
    # print(c(round(idx,3), round(p,3), round(prob,3), round(prob_fn_j,3), round(prob_FO2deno,3)))
    print(c(round(idx,3), round(p,3), round(prob,3)))
    sum_val = sum_val + prob
    
    #### END of p 
  }
  avg_survival = sum_val / idx 
  print(c("VT,PP",VT_val,PP_val, round(avg_survival,3)))
}
