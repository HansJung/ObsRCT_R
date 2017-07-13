source("compute_causal_effect.R")
source("factor_to_numeric.R")

df <- read.csv("Data/reduce_final_R.csv")
ID = df$SUBJECT_ID
Y = as.numeric(df$SURVIVAL)
MR = factor_to_numeric(df$ResRate)
MV = factor_to_numeric(df$Minute_Volume)
Sex = as.numeric(df$GENDER)
Sev = factor_to_numeric(df$Sev)
Age = as.numeric(df$AGE)
PEEP = factor_to_numeric(df$PEEP)
VT = factor_to_numeric(df$VT_WEIGHT)
PP = factor_to_numeric(df$PP)
PIP = factor_to_numeric(df$PIP)
FO2 = factor_to_numeric(df$FiO2)
NMBA = df$DOSAGE
KG = factor_to_numeric(df$WEIGHT)
SPO2 = factor_to_numeric(df$SpO2)
PaO2 = factor_to_numeric(df$PaO2)
PaCO2 = factor_to_numeric(df$PaCO2)
pH = factor_to_numeric(df$pH)

for (i in 1:length(NMBA)){
  if (NMBA[i] > 0){
    NMBA[i] = 1
  }
  else{
    NMBA[i] = 0
  }
}

X = NMBA
# Z = cbind(Sex,Age,FO2,PEEP,MV,VT,MR,PP,PIP,PaO2,SPO2,PaCO2,pH,Sev)
Z = Sev

eta_val = 0.05
depth_val = 50
n_round = 100
queit = 1
Y_given = cbind(X,Z)
Model = compute_causal_effect(Y_given,Y,"binary:logistic",eta_val,depth_val,n_round,queit)


N = 8587
prob_i = 0
Probs = rep(0,N)

# Sample for computing the average 
sample_N = 20
NMBA_N = sample(1:531,sample_N)
nonNMBA_N = sample(532:8587,sample_N)
samples = c(NMBA_N,nonNMBA_N)
Probs = rep(0,2*sample_N)
idx = 1

for (i in samples){
  prob_i = 0
  dos = NMBA[i]
  for (j in 1:N){
    data_sub = c(dos,Z[j])
    prob_y = predict(Model, t(as.matrix(data_sub)))
    prob_i = prob_i + prob_y 
  }
  survival_i = round(prob_i / N ,3) 
  Probs[idx] = survival_i
  print(c(idx,survival_i))
  idx = idx + 1
}
avg_survival = round(mean(Probs[which(!is.na(Probs))]),3)