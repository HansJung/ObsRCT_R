setwd("~/Dropbox/Personal/Research/Causal_RCT/Code/ObsRCT")
source("compute_causal_effect.R")
source("factor_to_numeric.R")
library(mixtools)
library(fitdistrplus)

set.seed(123)
fitting = F
computing =T

train_Y = F
train_FO2 = F
train_VT = F
train_PP = F
train_FO2deno = F
train_VTdeno = F
train_PPdeno = F


df <- read.csv("Data/reduce_final_R.csv")
df = subset(df, factor_to_numeric(df$Sev)<=300)
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
  param = list(booster="gbtree",eta = 0.05, gamma = 0.5, max_depth = 15, 
               subsample = 0.5, lambda=0.5, alpha=0.5, verbose=0)
  nround = 1000
  
  Y_given = cbind(RR,Sev,NMBA,KG,PP,PEEP,FO2,VT )
  print("Y Run!")
  Y.cond = compute_causal_effect(Y_given,Y,nround,"binary:logistic",param)
}

# 2. Build a model for f(FO2 | FO2_given), NUMERATOR 
## where FO2_given = [Age, Sev, NMBA]
if (train_FO2 == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0, max_depth = 5, 
               subsample = 0.5, lambda=0.3, alpha=0.1, verbose=0)
  nround = 500
  FO2_given = cbind(Age,Sev,NMBA,KG,PP,PEEP)
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


# 9. Build a model for f(VT | VT_given)
## where PEEP_given = [KG, Age, NMBA, Sev, FO2]
if (train_VT == T){
  param = list(booster="gbtree",eta = 0.02, gamma = 0, max_depth = 15, 
               subsample = 0.5, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  VT_given = cbind(RR,Sev,NMBA,KG,PP,PEEP,FO2)
  Given = VT_given[which(!is.na(VT))]
  Data = VT[which(!is.na(VT))]
  print("VT Run!")
  VT.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 9. Build a model for f(VT | VT_given)
## where PEEP_given = [KG, Age, NMBA, Sev, FO2]
if (train_VTdeno == T){
  param = list(booster="gbtree",eta = 0.02, gamma = 0, max_depth = 15, 
               subsample = 0.5, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  VTdeno_given = cbind(RR,Sev,NMBA,PP)
  Given = VTdeno_given[which(!is.na(VT))]
  Data = VT[which(!is.na(VT))]
  print("VT Run!")
  VTdeno.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 5. Build a model for f(PP | PP_given)
if (train_PP == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  PP_given = cbind(RR,Age,Sev,NMBA,KG)
  Given = PP_given[which(!is.na(PP))]
  Data = PP[which(!is.na(PP))]
  print("PP Run!")
  PP.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 

# 5. Build a model for f(PP | PP_given)
if (train_PPdeno == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  PPdeno_given = cbind( RR,Sev,NMBA)
  Given = PPdeno_given[which(!is.na(PP))]
  Data = PP[which(!is.na(PP))]
  print("PP Run!")
  PPdeno.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 




# 5. Find Fit (example: PEEP)
if (fitting == T){
  Model = FO2deno.cond
  given = FO2deno_given
  Data = FO2
  
  Fit = predict(Model,data.matrix(given))
  Diff = Data - Fit
  Diff = Diff[which(!is.na(Diff))]
  
  fit_fun = fitdist(Diff,'norm')
  if(train_FO2 == T){
    FO2_mix_PEEP = normalmixEM(Diff)
  }else if(train_FO2deno == T){
    FO2deno_mix_PEEP = normalmixEM(Diff)
  }
  # descdist(log(Fit), discrete=FALSE)
}

# FO2: Diff ~  GMM
# VT: Diff~ Norm(0,2)
# PP: Diff ~ Norm(0,6)
# FO2deno: Diff ~ GMM 
# PPdeno: Diff ~ Norm(0,6)
# VTdeno: Diff~ Norm(0,2)



# 6. Computation of statistics - VT, PP 
## Intervention? True! 
if (computing == T){  
  N = 8587
  
  
  VT_val = 7 # (6,12)
  PP_val = 30 # (30,50)
  PP_ctrl_limit = PP_val # (30,50)
  FiO2s = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)

    ## If old 
  PEEPs = c(5,8,10,10,12,14,16) # Conv
  PEEP_min = 5 
  PEEP_max_min = 18 
  PEEP_max_max = 24   
  
  ## If new 
  Experiment = T
  
  if (Experiment == T){
    PEEPs = c(12,14,16,20,20,22,22) # New 
    PEEP_min = 12 
    PEEP_max_min = 22 
    PEEP_max_max = 24   
  }
  
  sample_N = 2000
  samples = sample(1:dim(df)[1], sample_N)
  Nj = 200
  
  # NMBA_N = sample(1:531,sample_N)
  # nonNMBA_N = sample(532:8587,sample_N)
  # samples = c(NMBA_N,nonNMBA_N)
  # Probs = rep(0,2*sample_N)
  
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
      PEEP_p = data_sub['PEEP']
    }else{next}
    RR_p = if(!is.na(data_sub['RR'])){
      data_sub['RR'] = max(6,data_sub['RR'])
      data_sub['RR'] = min(35, data_sub['RR'])
      RR_p = data_sub['RR']
    }else{next}
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
    KG_p = if(!is.na(data_sub['KG'])){KG_p = data_sub['KG']}else{
      next
    }
    Age_p = if(!is.na(Age[p])){Age_p = Age[p]}else{
      next
    }
      
    # Compute the denominator 
    ## FO2_deno
    Obs_FO2deno = FO2_p -  predict( FO2deno.cond, (as.matrix(cbind(PEEP_p,NMBA_p,Sev_p)) ))
    prob_FO2deno = FO2deno_mix_PEEP$lambda[1] * dnorm(Obs_FO2deno, mean= FO2deno_mix_PEEP$mu[1], sd= FO2deno_mix_PEEP$sigma[1] ) + 
      FO2deno_mix_PEEP$lambda[2] * dnorm(Obs_FO2deno, mean= FO2deno_mix_PEEP$mu[2], sd= FO2deno_mix_PEEP$sigma[2] ) 
    prob_FO2deno = prob_FO2deno[[1]]
    
    ## FO2_numer RR,Sev,NMBA,KG,PP,PEEP,FO2
    Obs_FO2 = FO2_p - predict(FO2.cond, (as.matrix(cbind(Age_p,Sev_p,NMBA_p,KG_p,PP_p,PEEP_p))))
    prob_FO2 = FO2_mix_PEEP$lambda[1] * dnorm(Obs_FO2, mean= FO2_mix_PEEP$mu[1], sd= FO2_mix_PEEP$sigma[1] ) + 
      FO2_mix_PEEP$lambda[2] * dnorm(Obs_FO2, mean= FO2_mix_PEEP$mu[2], sd= FO2_mix_PEEP$sigma[2] ) 
    prob_FO2 = prob_FO2[[1]]
    
    ## VT_deno
    Obs_VTdeno = VT_p - predict(VTdeno.cond, as.matrix(cbind(RR_p,Sev_p,NMBA_p,PP_p)))
    prob_VTdeno = dnorm(Obs_VTdeno, mean=0, sd=2)
    
    ## VT_numer
    Obs_VT = VT_p - predict(VT.cond, as.matrix(cbind(RR_p,Sev_p,NMBA_p,KG_p,PP_p, PEEP_p, FO2_p)))
    prob_VT = dnorm(Obs_VT, mean=0, sd=2)
    
    ## PP_deno
    Obs_PPdeno = PP_p - predict(PPdeno.cond, as.matrix(cbind( RR_p,Sev_p,NMBA_p)))
    prob_PPdeno = dnorm(Obs_PPdeno, mean=0, sd=6)
    
    ## PP_numer
    Obs_PP = PP_p - predict(PP.cond, as.matrix(cbind(RR_p,Age_p,Sev_p,NMBA_p,KG_p)))
    prob_PP = dnorm(Obs_PP, mean=0, sd=6)
    
    ## Y 
    prob_y = predict(Y.cond, as.matrix(cbind(RR_p, Sev_p,NMBA_p,KG_p,PP_p,PEEP_p,FO2_p,VT_p)))
    
    
    # f_dp
    f_d =  log(prob_FO2deno) + log(prob_VTdeno) + log(prob_PPdeno) 
    f_n =  log(prob_y) + log(prob_FO2) +  log(prob_VT) + log(prob_PP)  
    
    prob = exp( f_n - f_d )[[1]]
    if (prob > 1){
      next
    }
    idx = idx + 1 
    print(c(round(idx,3), round(p,3), round(prob,3)))
    sum_val = sum_val + prob
    
    
    ##### END OF P    
  }
  avg_survival = sum_val / idx 
  print(c("PEEP",PEEP_min, round(avg_survival,3)))
}
    
