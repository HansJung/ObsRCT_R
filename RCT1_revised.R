setwd("~/Dropbox/Personal/Research/Causal_RCT/Code/ObsRCT")
source("compute_causal_effect.R")
source("factor_to_numeric.R")

library(fitdistrplus)

set.seed(123)
fitting = T
computing = F

train_Y = F
train_PCO2 = F
train_pH = F
train_PO2 = F
train_PEEP = F 
train_FO2 = F
train_Sev = F
train_NMBA = F
train_FO2_deno = F
train_PEEP_deno = F 

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

# 3. Build a model for f(pH | pH_given)
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



# 2. Build a model for f(PEEP | PEEP_given)
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

# 3. Build a model for f(FO2 | FO2_given), NUMERATOR 
## where FO2_given = [Age, Sev, NMBA]
if (train_FO2 == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0, max_depth = 5, 
               subsample = 1, lambda=0, alpha=0, verbose=0)
  nround = 100
  FO2_given = cbind(Age, Sev, NMBA)
  Given = FO2_given[which(!is.na(FO2))]
  Data = FO2[which(!is.na(FO2))]
  print("FO2 Run!")
  FO2.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 4. Build a model for f(Sev | Sev_given)
## where Sev_given = [Age]
if (train_Sev == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0, max_depth = 15, 
               subsample = 0.8, lambda=0, alpha=0, verbose=0)
  nround = 1500
  Sev_given = cbind(Age)
  Given = Sev_given[which(!is.na(Sev))]
  Data = Sev[which(!is.na(Sev))]
  print("Sev Run!")
  Sev.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 5. Build a model for f(NMBA | NMBA_given)
## where NMBA_given = [Age]
if (train_NMBA == T){
  param = list(booster="gbtree",eta = 0.001, gamma = 0, max_depth = 30, 
               subsample = 1, lambda=0.5, alpha=0.5, verbose=0)
  nround = 1000
  NMBA_given = cbind(Age)
  Given = NMBA_given[which(!is.na(NMBA))]
  Data = NMBA[which(!is.na(NMBA))]
  print("NMBA Run!")
  NMBA.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 6. Build a model for f_deno(FO2 | FO2_deno_given)
## where FO2_deno_given = [Age]
if (train_FO2_deno == T){
  param = list(booster="gbtree",eta = 0.01, gamma = 0, max_depth = 15, 
               subsample = 0.5, lambda=0.5, alpha=0.5, verbose=0)
  nround = 1000
  FO2_deno_given = cbind(Age)
  Given = FO2_deno_given[which(!is.na(FO2))]
  Data = FO2[which(!is.na(FO2))]
  print("FO2_deno Run!")
  FO2_deno.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}

# 7. Build a model for f_deno(PEEP | PEEP_deno_given)
## where PEEP_deno_given = [Age]
if (train_PEEP_deno == T){
  param = list(booster="gbtree",eta = 0.1, gamma = 0.5, max_depth = 10, 
               subsample = 0.5, lambda=0.5, alpha=0.5,verbose=0)
  nround = 1000
  PEEP_deno_given = cbind(Age)
  Given = PEEP_deno_given[which(!is.na(PEEP))]
  Data = PEEP[which(!is.na(PEEP))]
  print("PEEP_deno Run!")
  PEEP_deno.cond = compute_causal_effect(Given,Data,nround,"reg:linear",param)
}


# 5. Find Fit (example: PEEP)
if (fitting == T){
  Model = PO2.cond
  given = PO2_given
  Data = PO2
  
  Fit = predict(Model,data.matrix(given))
  Diff = Fit - Data
  Diff = Diff[which(!is.na(Diff))]
  
  fit_fun = fitdist(Diff,'norm')
  # d_FO2 = density(Diff)
  # descdist(log(Fit), discrete=FALSE)
}



# 6. Computation of statistics - VT, PP 
## Intervention? True! 
if (computing == T){  
  N = 8587
  case_VT = T
  case_PEEP = T
  case_PP = T
  case_MR = T 
  
  ## Variable fixation 
  VT_val = 12 # (6,12)
  PP_val = 50 # (30,50)
  PP_ctrl_limit = 50 # (30,50)
  FiO2s = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  PEEPs = c(5,8,10,10,12,14,16)
  
  sample_N = 10
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
      ## Adjusting = [Sex, KG, Sev, Age, NMBA, ]
      ## Y_given = [ KG, Age, Sex, NMBA, Sev, FO2, PEEP, VT, PP, MR ]
      data_sub = Y_given[i, ]
      data_sub['Sex'] = Sex[j]
      data_sub['KG'] = KG[j]
      data_sub['Age'] = Age[j]
      data_sub['Sev'] = Sev[j]
      data_sub['NMBA'] = NMBA[j]
      
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
      if (case_MR){
        if (!is.na(data_sub['MR'] )){
          data_sub['MR'] = max(6,data_sub['MR'])
          data_sub['MR'] = min(35, data_sub['MR'])
        }else{
          next
        }
      }
      
      # Probability computation 
      Model = Y.cond
      prob_y = predict(Model, t(as.matrix(data_sub)))
      
      ##############################################################################
      # 2. Adjusting setting for PEEP 
      ## Adjusting = [Sex, KG, Sev, Age, NMBA]
      ## PEEP_given = [KG, Age, NMBA, Sev, FO2]
      acc_mu = 5.1
      acc_sd = 2.72
      data_sub = PEEP_given[i,]
      data_sub['KG'] = KG[j]
      data_sub['Age'] = Age[j]
      data_sub['Sev'] = Sev[j]
      data_sub['NMBA'] = NMBA[j]
      
      # Probability computation 
      Model = PEEP.cond
      Obs = PEEP[i]
      
      # Intervention setting to PEEP 
      if (!is.na(Obs)){ 
        sub_FO2 = round(Y_given[i,]['FO2'],1)
        if (is.nan(sub_FO2) || is.na(sub_FO2)){
          next
        }
        if (sub_FO2 < 0.3){
          Obs = min(5, Obs)
        }
        else if (sub_FO2 > 0.9){
          Obs = min(24, Obs)
          Obs = max(18, Obs)
        }
        else if (!is.na(sub_FO2)){
          Obs = PEEPs[which(FiO2s == sub_FO2)]
        }
        else{
          Obs = Obs
        }
      }
      
      # prob_PEEP = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub)))-acc_mu, sd= acc_sd )
      # prob_PEEP = d$y[which(abs(d$x-Obs)==min(abs(d$x-Obs)))]
      Obs = Obs - predict(Model, t(as.matrix(data_sub)))
      d = d_PEEP
      d_idx = which(abs(d$x-Obs)==min(abs(d$x-Obs)))
      # prob_PEEP = d$y[d_idx] * (d$x[d_idx] - d$x[d_idx-1])
      prob_PEEP = d$y[which(abs(d$x-Obs)==min(abs(d$x-Obs)))]
      
      ##############################################################################
      # 3. Adjusting setting for FO2 
      ## Adjusting = [Sex, KG, Age]
      ## FO2_given = [Age, NMBA,Sev]
      acc_mu = 0
      acc_sd = 0.7
      data_sub = FO2_given[i,]
      data_sub['Age'] = Age[j]
      data_sub['Sev'] = Sev[j]
      data_sub['NMBA'] = NMBA[j]
      
      # Probability computation 
      Model = FO2.cond
      Obs = FO2[i]
      
      # prob_FO2 = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )
      Obs = Obs - predict(Model, t(as.matrix(data_sub)))
      d = d_FO2
      d_idx = which(abs(d$x-Obs)==min(abs(d$x-Obs)))
      prob_FO2 = d$y[which(abs(d$x-Obs)==min(abs(d$x-Obs)))]
      # prob_FO2 = d$y[d_idx] * (d$x[d_idx] - d$x[d_idx-1])

      
      
      
      ##############################################################################
      # Adjusting over 
      prob_numer = prob_y * prob_PEEP * prob_FO2
      sum_numer = sum_numer + prob_numer
      
      prob_denom = prob_PEEP * prob_FO2
      sum_denom = sum_denom + prob_denom
    }
    ##############################################################################
    
    # # 6. Adjusting setting for FO2_deno 
    # ## Adjusting = [Sex, KG, Age]
    # ## FO2_deno_given = [KG]
    # 
    # acc_mu = 0
    # acc_sd = 0.2
    # data_sub = FO2_deno_given[i,]
    # 
    # # Probability computation
    # Model = FO2_deno.cond
    # Obs = FO2[i]
    # Obs = Obs - predict(Model, t(as.matrix(data_sub)))
    # d = d_FO2_deno
    # d_idx = which(abs(d$x-Obs)==min(abs(d$x-Obs)))
    # # prob_FO2_deno = d$y[d_idx] * (d$x[d_idx] - d$x[d_idx-1])
    # prob_FO2_deno = d$y[which(abs(d$x-Obs)==min(abs(d$x-Obs)))]
    # # prob_FO2_deno = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )
    # 
    # 
    # ##############################################################################
    # # 7. Adjusting setting for PEEP_deno
    # ## Adjusting = [Sex, KG, Age]
    # ## PEEP_deno_given = [FO2]
    # 
    # acc_mu = 0
    # acc_sd = 2.57
    # data_sub = PEEP_deno_given[i,]
    # 
    # # Probability computation
    # Model = PEEP_deno.cond
    # Obs = PEEP[i]
    # # Intervention setting to PEEP
    # if (!is.na(Obs)){
    #   sub_FO2 = round(Y_given[i,]['FO2'],1)
    #   if (is.nan(sub_FO2) || is.na(sub_FO2)){
    #     next
    #   }
    #   if (sub_FO2 < 0.3){
    #     Obs = min(5, Obs)
    #   }
    #   else if (sub_FO2 > 0.9){
    #     Obs = min(24, Obs)
    #     Obs = max(18, Obs)
    #   }
    #   else if (!is.na(sub_FO2)){
    #     Obs = PEEPs[which(FiO2s == sub_FO2)]
    #   }
    #   else{
    #     Obs = Obs
    #   }
    # }
    # Obs = Obs - predict(Model, t(as.matrix(data_sub)))
    # d = d_PEEP_deno
    # d_idx = which(abs(d$x-Obs)==min(abs(d$x-Obs)))
    # # prob_PEEP_deno = d$y[d_idx] * (d$x[d_idx] - d$x[d_idx-1])
    # prob_PEEP_deno = d$y[which(abs(d$x-Obs)==min(abs(d$x-Obs)))]
    # # prob_PEEP_deno = dnorm(Obs, mean=predict(Model, t(as.matrix(data_sub))) - acc_mu, sd= acc_sd )
    
    
    
    ##############################################################
    # sum_numer = sum_numer / length(samples)
    # sum_Y = sum_Y / length(samples)
    # sum_numer = sum_numer / length(samples)
    # sum_denom = prob_FO2_deno * prob_PEEP_deno
    # print(c("prob deno", prob_FO2_deno,prob_PEEP_deno))
    prob = sum_numer / sum_denom
    if (is.na(prob) || length(prob) <= 0){
      next
    }
    
    if (prob < 1){
      round_idx = 3
      # print(c(round(idx), round(prob_y,round_idx), round(prob_PEEP,round_idx), round(prob_FO2,round_idx), 
      #         round(prob_FO2_deno,round_idx), round(prob_PEEP_deno,round_idx) ))
      # print(c(round(idx), round(sum_numer,round_idx),round(sum_denom,round_idx)))
      print(c(round(idx,round_idx),round(i,round_idx),round(prob,round_idx), round(Y[i],round_idx)))
    }else{
      round_idx = 3
      # print(c(round(idx), round(sum_numer,round_idx),round(sum_denom,round_idx)))
      # print(c(round(idx), round(prob_y,round_idx), round(prob_PEEP,round_idx), round(prob_FO2,round_idx), 
      #         round(prob_FO2_deno,round_idx), round(prob_PEEP_deno,round_idx) ))
      print(c(round(idx,round_idx),round(i,round_idx),round(prob,round_idx), round(Y[i],round_idx)))
    }
    
    Probs[idx] = prob
    idx = idx + 1
  }
  avg_survival = round(mean(Probs[which(!is.na(Probs))]),3)
}
