setwd("~/Dropbox/Personal/Research/Causal_RCT/Code/ObsRCT")
source("compute_causal_effect.R")
source("factor_to_numeric.R")

library(fitdistrplus)

set.seed(123)
fitting = F
computing = T

train_Y = F
train_PCO2 = F
train_RR = F
train_PO2 = F
train_PP = F
train_PIP = F
train_MV = F
train_SO2 = F 
train_VT = F 
train_FO2 = F
train_PEEP = F
train_pH = F 


df <- read.csv("Data/reduce_final_R.csv")
df = subset(df, factor_to_numeric(df$Sev)<150)
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
  param = list(booster="gbtree",eta = 0.05, gamma = 0.5, max_depth = 10, 
               subsample = 0.5, lambda=0.5, alpha=0.5, verbose=0)
  nround = 1000
  
  Y_given = cbind( KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2,PO2,PCO2,pH )
  print("Y Run!")
  Y.cond = compute_causal_effect(Y_given,Y,nround,"binary:logistic",param)
}

# 2. Build a model for f(PCO2 | PCO2_given)
if (train_PCO2 == T){
  param = list(booster="gbtree",eta = 0.05, gamma = 0.5, max_depth = 20, 
               subsample = 0.7, lambda=0.1, alpha=0.1, verbose=0)
  nround = 300
  PCO2_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2,PO2 )
  Given = PCO2_given[which(!is.na(PCO2))]
  Data = PCO2[which(!is.na(PCO2))]
  print("PCO2 Run!")
  PCO2.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 

# 3. Build a model for f(RR | RR_given)
if (train_RR == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0.5, max_depth = 15,
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  RR_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP)
  Given = RR_given[which(!is.na(RR))]
  Data = RR[which(!is.na(RR))]
  print("RR Run!")
  RR.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}


# 4. Build a model for f(PO2 | PO2_given)
if (train_PO2 == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 2000
  PO2_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2)
  Given = PO2_given[which(!is.na(PO2))]
  Data = PO2[which(!is.na(PO2))]
  print("PO2 Run!")
  PO2.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 


# 5. Build a model for f(PP | PP_given)
if (train_PP == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  PP_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT)
  Given = PP_given[which(!is.na(PP))]
  Data = PP[which(!is.na(PP))]
  print("PP Run!")
  PP.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 


# 6. Build a model for f(PIP | PIP_given)
if (train_PIP == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 300
  PIP_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR)
  Given = PIP_given[which(!is.na(PIP))]
  Data = PIP[which(!is.na(PIP))]
  print("PIP Run!")
  PIP.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 


# 7. Build a model for f(MV | MV_given)
if (train_MV == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 300
  MV_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR)
  Given = MV_given[which(!is.na(MV))]
  Data = MV[which(!is.na(MV))]
  print("MV Run!")
  MV.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 


# 8. Build a model for f(SO2 | SO2_given)
if (train_SO2 == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 300
  SO2_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,MV)
  Given = SO2_given[which(!is.na(SO2))]
  Data = SO2[which(!is.na(SO2))]
  print("SO2 Run!")
  SO2.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 


# 9. Build a model for f(VT | VT_given)
## where PEEP_given = [KG, Age, NMBA, Sev, FO2]
if (train_VT == T){
  param = list(booster="gbtree",eta = 0.02, gamma = 0, max_depth = 15, 
               subsample = 0.5, lambda=0.5, alpha=0.5, verbose=0)
  nround = 100
  VT_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,VT)
  Given = VT_given[which(!is.na(VT))]
  Data = VT[which(!is.na(VT))]
  print("VT Run!")
  VT.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}


# 10. Build a model for f(FO2 | FO2_given), NUMERATOR 
## where FO2_given = [Age, Sev, NMBA]
if (train_FO2 == T){
  param = list(booster="gbtree",eta = 1, gamma = 0, max_depth = 5, 
               subsample = 1, lambda=0, alpha=0, verbose=0)
  nround = 10
  FO2_given = cbind(Age, Sev, NMBA)
  Given = FO2_given[which(!is.na(FO2))]
  Data = FO2[which(!is.na(FO2))]
  print("FO2 Run!")
  FO2.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 11. Build a model for f(PEEP | PEEP_given)
## where PEEP_given = [KG, Age, NMBA, Sev, FO2]
if (train_PEEP == T){
  param = list(booster="gbtree",eta = 0.02, gamma = 0, max_depth = 15, 
               subsample = 0.5, lambda=0.5, alpha=0.5, verbose=0)
  nround = 100
  PEEP_given = cbind(KG, Age, NMBA, Sev, FO2)
  Given = PEEP_given[which(!is.na(PEEP))]
  Data = PEEP[which(!is.na(PEEP))]
  print("PEEP Run!")
  PEEP.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 12. Build a model for f(pH | pH_given)
## where pH_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2,PCO2]
if (train_pH == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0.5, max_depth = 15, 
               subsample = 0.7, lambda=0.5, alpha=0.5, verbose=0)
  nround = 500
  pH_given = cbind(KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2,PCO2)
  Given = pH_given[which(!is.na(pH))]
  Data = pH[which(!is.na(pH))]
  print("pH Run!")
  pH.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
} 


# PCO2: Diff ~ Norm, mean = 0, sd = 9
# RR: abs(log(abs(Diff))) ~ logis, loc=1.4, scale = 0.4
# PO2: log(abs(Diff)) ~ Norm, mean 3.5, sd=1.3
# PP: abs(Diff) ~ LN, meanlog=0.8, sdlog=1.21
# PIP: Diff ~ logis, loc = 0, scale=3.5
# MV: Diff ~ Norm, mean = 0, sd = 3
# SO2: abs(Diff) ~ gamma(1,0.4)
# VT: abs(log(abs(Diff))) ~ gamma, shape=1.2, rate=1.5
# FO2: log(abs(Diff)) ~ Norm, mean=-2.3, sd=1
# PEEP: abs(Diff) ~ gamma, shape = 0.25, rate = 0.4
# pH: abs(log(abs(Diff))),gamma // shape:9.4, rate = 3.3










# 5. Find Fit (example: PEEP)
if (fitting == T){
  Model = PEEP.cond
  given = PEEP_given
  Data = PEEP
  
  Fit = predict(Model,data.matrix(given))
  Diff = Data - Fit
  Diff = Diff[which(!is.na(Diff))]
  
  fit_fun = fitdist(Diff,'norm')
  d_FO2 = density(Diff)
  # descdist(log(Fit), discrete=FALSE)
}




# 6. Computation of statistics - VT, PP 
## Intervention? True! 
if (computing == T){  
  N = 8587
  
  ## Variable fixation 
  NMBA_val = 0
  
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
  Experiment = F
  
  if (Experiment == T){
    PEEPs = c(12,14,16,20,20,22,22) # New 
    PEEP_min = 12 
    PEEP_max_min = 22 
    PEEP_max_max = 24   
  }
  
  sample_N = 100
  NMBA_N = sample(1:531,sample_N)
  nonNMBA_N = sample(532:8587,sample_N)
  samples = c(NMBA_N,nonNMBA_N)
  Probs = rep(0,2*sample_N)
  
  idx = 1
  for (i in samples){
    # Added and divided for adjusting. 
    sum_numer = 0
    sum_denom = 0
    
    for (j in samples){
      # Adjusting setting for Y
      ## Adjusting = [Sex, KG, Sev, Age, NMBA, ]
      ## Y_given = [ KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2,PO2,PCO2,pH ]
      if (!is.na(Y[i])){
        data_sub = Y_given[i, ]
        data_sub['Sex'] = Sex[j]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sev'] = Sev[j]
        data_sub['NMBA'] = NMBA_val
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){ 
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val 
          }
          else{
            data_sub['PP'] = data_sub['PP']
          }
        }
        if (!is.na(data_sub['PEEP'])){ 
          sub_FO2 = round(Y_given[i,]['FO2'],1)
          if (is.nan(sub_FO2) || is.na(sub_FO2)){
            next
          }
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
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        if (!is.na(data_sub['NMBA'])){ data_sub['NMBA'] = NMBA_val }
        
        # Probability computation 
        Model = Y.cond
        prob_y = predict(Model, t(as.matrix(data_sub)))
      }else{
        next
      }
      
      
      ##############################################################################
      # 2. Adjusting setting for PCO2 
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## PCO2_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2,PO2 ]
      if (!is.nan(PCO2[i])){
        data_sub = PCO2_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){ 
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val 
          }
          else{
            data_sub['PP'] = data_sub['PP']
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
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        
        # ProbabilitPCO2 computation 
        acc_mu = 0
        acc_sd = 9
        Model = PCO2.cond
        Obs = PCO2[i]
        prob_PCO2 = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub)))-acc_mu, sd= acc_sd )
      }else{
        next
      }
      
      
      ##############################################################################
      # 3. Adjusting setting for RR
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## RR_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP]
      if (!is.na(RR[i])){
        data_sub = RR_given[i,]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        # Intervention seteting
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val
          }
          else{
            data_sub['PP'] = data_sub['PP']
          }
        }
        if (!is.na(data_sub['PEEP'])){
          sub_FO2 = round(Y_given[i,]['FO2'],1)
          if (is.nan(sub_FO2) || is.na(sub_FO2)){
            next
          }
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
        if (!is.na(RR[i])){
          RR[i] = max(6,RR[i])
          RR[i] = min(35, RR[i])
        }
        
        # ProbabilitpH computation
        acc_mu = 1.4
        acc_sd = 0.4
        Model = RR.cond
        Obs = abs( log( abs( RR[i]-predict(Model, t(as.matrix(data_sub))) ) ) )
        prob_RR = dlogis(Obs, location = acc_mu, scale = acc_sd )
      }else{
        next
      }
      
      
      ##############################################################################
      # 4. Adjusting setting for PO2 
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## PO2_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2]
      if (!is.nan(PO2[i])){
        data_sub = PO2_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){ 
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val 
          }
          else{
            data_sub['PP'] = data_sub['PP']
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
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        
        # ProbabilitPO2 computation 
        acc_mu = 3.5
        acc_sd = 1.3
        
        Model = PO2.cond
        Obs = log( abs( PO2[i] - predict(Model, t(as.matrix(data_sub))) ) )
        prob_PO2 = dnorm(Obs, mean=acc_mu, sd=acc_sd )
      }else{
        next
      }
      
      
      ##############################################################################
      # 5. Adjusting setting for PP
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## PP_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT]
      if (!is.na(PP[i]) && !is.nan(PP[i])){
        data_sub = PP_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(PP[i])){ 
          if (PP[i] >= PP_ctrl_limit){
            PP[i] = PP_val 
          }
          else{
            PP[i] = PP[i]
          }
        }
        if (!is.na(data_sub['PEEP'])){ 
          sub_FO2 = round(Y_given[i,]['FO2'],1)
          if (is.nan(sub_FO2) || is.na(sub_FO2)){
            next
          }
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
        
        
        
        # ProbabilitPO2 computation 
        acc_mu = 0.8
        acc_sd = 1.21
        Model = PP.cond
        Obs = abs( PP[i]-predict(Model, t(as.matrix(data_sub))) )
        prob_PP = dlnorm(Obs, meanlog = acc_mu, sdlog = acc_sd )
      }else{
        next
      }
      
      
      
      
      ##############################################################################
      # 6. Adjusting setting for PIP 
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## PIP_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR]
      if (!is.nan(PIP[i])){
        data_sub = PIP_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){ 
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val 
          }
          else{
            data_sub['PP'] = data_sub['PP']
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
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        
        # ProbabilitPIP computation 
        acc_mu = 0
        acc_sd = 3.5
        Model = PIP.cond
        Obs = PIP[i] - predict(Model, t(as.matrix(data_sub)))
        prob_PIP = dlogis(Obs,location=acc_mu,scale=acc_sd)
      }else{
        next
      }
      
      
      ##############################################################################
      # 7. Adjusting setting for MV 
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## MV_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR]
      if (!is.nan(MV[i])){
        data_sub = MV_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){ 
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val 
          }
          else{
            data_sub['PP'] = data_sub['PP']
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
        if (!is.na(data_sub['RR'])){
          if (!is.na(data_sub['RR'] )){
            data_sub['RR'] = max(6,data_sub['RR'])
            data_sub['RR'] = min(35, data_sub['RR'])
          }else{
            next
          }
        }
        
        # ProbabilitMV computation 
        acc_mu = 0
        acc_sd = 3
        Model = MV.cond
        Obs = MV[i]
        prob_MV = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub)))-acc_mu, sd= acc_sd )
      }else{
        next
      }
      
      
      
      ##############################################################################
      # 8. Adjusting setting for SO2
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## MV_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,MV]
      if (!is.nan(SO2[i])){
        data_sub = SO2_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){ 
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val 
          }
          else{
            data_sub['PP'] = data_sub['PP']
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
        if (!is.na(data_sub['RR'])){
          if (!is.na(data_sub['RR'] )){
            data_sub['RR'] = max(6,data_sub['RR'])
            data_sub['RR'] = min(35, data_sub['RR'])
          }else{
            next
          }
        }
        
        # ProbabilitSO2 computation 
        acc_mu = 1
        acc_sd = 0.4
        Model = SO2.cond
        Obs = abs(SO2[i] - predict(Model, t(as.matrix(data_sub))))
        prob_SO2 = dgamma(Obs, shape=acc_mu, rate= acc_sd )
      }else{
        next
      }
      
      
      
      
      ##############################################################################
      # 9. Adjusting setting for VT
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## VT_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP]
      
      if (!is.nan(VT[i])){
        data_sub = VT_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting
        if (!is.na(data_sub['PEEP'])){
          sub_FO2 = round(Y_given[i,]['FO2'],1)
          if (is.nan(sub_FO2) || is.na(sub_FO2)){
            next
          }
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
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        
        # ProbabilitSO2 computation
        acc_mu = 1.2
        acc_sd = 1.5
        Model = VT.cond
        Obs = abs( log( abs( VT_val - predict(Model, t(as.matrix(data_sub))) ) ) )
        prob_VT = dgamma(Obs, shape=acc_mu, rate=acc_sd)
        # prob_VT = 1 
      }else{
        next
      }
      
      ##############################################################################
      # 10. Adjusting setting for FO2
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## FO2_given = [Age, Sev, NMBA]
      
      if (!is.nan(FO2[i])){
        data_sub = FO2_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
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
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        
        # ProbabilitSO2 computation 
        acc_mu = -2.3
        acc_sd = 1
        Model = FO2.cond
        Obs = log( abs( FO2[i]- predict(Model, t(as.matrix(data_sub))) ) )
        prob_FO2 = dnorm(Obs, mean = acc_mu, sd = acc_sd )
      }else{
        next
      }
      
      ##############################################################################
      # 11. Adjusting setting for PEEP
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## PEEP_given = [KG, Age, NMBA, Sev, FO2]
      
      if (!is.nan(PEEP[i])){
        data_sub = PEEP_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
        if (!is.na(PEEP[i])){ 
          sub_FO2 = round(Y_given[i,]['FO2'],1)
          if (is.nan(sub_FO2) || is.na(sub_FO2)){
            next
          }
          if (sub_FO2 < 0.3){
            PEEP[i] = min(5, PEEP[i])
          }
          else if (sub_FO2 > 0.9){
            PEEP[i] = min(24, PEEP[i])
            PEEP[i] = max(18, PEEP[i])
          }
          else if (!is.na(sub_FO2)){
            PEEP[i] = PEEPs[which(FiO2s == sub_FO2)]
          }
          else{
            PEEP[i] = PEEP[i]
          }
        }
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        
        # ProbabilitSO2 computation 
        acc_mu = 0.52
        acc_sd = 0.4
        Model = PEEP.cond
        Obs = abs(PEEP[i] - predict(Model, t(as.matrix(data_sub))))
        prob_PEEP = dgamma(Obs, shape=acc_mu, rate = acc_sd)
      }else{
        next
      }
      
      
      ##############################################################################
      # 12. Adjusting setting for pH 
      ## Adjusting = [KG,Age,Sex,NMBA,Sev]
      ## pH_given = [KG,Age,Sex,NMBA,Sev,FO2,PEEP,VT,PP,RR,PIP,MV,SO2,PCO2]
      if (!is.nan(pH[i])){
        data_sub = pH_given[i, ]
        data_sub['KG'] = KG[j]
        data_sub['Age'] = Age[j]
        data_sub['Sex'] = Sex[j]
        data_sub['NMBA'] = NMBA_val
        data_sub['Sev'] = Sev[j]
        
        
        # Intervention seteting 
        if (!is.na(data_sub['VT'])){ data_sub['VT'] = VT_val }
        if (!is.na(data_sub['PP'])){ 
          if (data_sub['PP'] >= PP_ctrl_limit){
            data_sub['PP'] = PP_val 
          }
          else{
            data_sub['PP'] = data_sub['PP']
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
        if (!is.na(data_sub['RR'])){
          data_sub['RR'] = max(6,data_sub['RR'])
          data_sub['RR'] = min(35, data_sub['RR'])
        }
        
        # ProbabilitpH computation 
        acc_mu = 9.4
        acc_sd = 3.3
        Model = pH.cond
        Obs = abs( log( abs( pH[i] - predict(Model, t(as.matrix(data_sub))) ) ) )
        prob_pH = dgamma(Obs, shape=acc_mu, rate = acc_sd )
      }else{
        next
      }
      
      
      
      ##############################################################################
      # Adjusting over 
      prob_numer = log(prob_y) +  log(prob_PCO2) + log(prob_RR) + log(prob_PO2) 
      + log(prob_PP) + log(prob_PIP) + log(prob_MV) + log(prob_SO2) 
      + log(prob_VT) + log(prob_FO2) + log(prob_PEEP) + log(prob_pH)
      prob_denom = prob_numer - log(prob_y)
      # print(c(j, PP[i], RR[i], prob_PP, prob_RR))
      sum_numer = sum_numer + exp(prob_numer)
      sum_denom = sum_denom + exp(prob_denom)
      
      #### END OF ADJUSTING 
    }
    
    prob = sum_numer / sum_denom
    if (is.na(prob) || length(prob) <= 0){
      next
    }
    
    # if (prob < 1){
    #   round_idx = 3
    #   print(c(idx,prob))
    # }else{
    #   round_idx = 3
    #   print(c(idx,prob))
    # }
    # print(c(idx,prob))
    Probs[idx] = prob
    idx = idx + 1
  }
  avg_survival = round(mean(Probs[which(!is.na(Probs))]),3)
  print(c("NMBA",NMBA_val, avg_survival))
}
