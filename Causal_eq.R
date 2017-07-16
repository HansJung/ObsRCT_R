library(causaleffect)
library(igraph)

g_PC = graph.formula( VT -+ PIP, VT -+ PO2, VT -+ PCO2, VT -+ MV, VT -+ Y, 
                      PP -+ PIP, PP -+ PEEP, PP -+ RR, PP -+ SO2, PP -+ Y, 
                      PIP -+ VT, PIP -+ PCO2,
                      PEEP -+ PP, PEEP -+ PIP, PEEP -+ MV, PEEP -+ FO2, PEEP -+ SO2,
                      MV -+ PCO2, MV -+ Y, 
                      RR -+ PP, RR -+ MV, 
                      FO2 -+ SO2, 
                      NMBA -+ PP, NMBA -+ FO2, NMBA -+ PO2, NMBA -+ Y, 
                      KG -+ VT, KG -+ PP, KG -+ Sex,
                      Age -+ FO2, Age -+ NMBA, 
                      Sev -+ FO2, Sev -+ SO2, Sev -+ PO2, 
                      Sex -+ VT, Sev -+ PP, Sev -+ MV, Sev -+ KG,
                      SO2 -+ PCO2, 
                      PO2 -+ Y,
                      PCO2 -+ pH, 
                      pH -+ Y
)



g = g_PC
hidden = c(1,6,15,19,23)
# VT -> PIP
# PIP ->VT 
# PP  ->RR 
# PEEP->PP 
# RR  ->PP

g = set.edge.attribute(g,"description",hidden,"U")

causal_effect_VT = causal.effect(y="Y",x=c("VT","PP"),z =c("FO2","RR","PEEP"),G=g, simp=T, steps=F)
causal_effect_PEEP = causal.effect(y="Y",x="PEEP",z =c("FO2","RR","VT","PP"),G=g, simp=T)
causal_effect_NMBA = causal.effect(y="Y",x="NMBA",z =c("VT", "RR","FO2","PEEP"),G=g, simp=T)

print(V(g))
print(E(g))
# print(causal_effect)
print(plot.igraph(g,edge.arrow.size=0.5,edge.curved = T))
  

###### END OF THE CODE #######

# g_full = graph.formula(V_T -+ M_R, V_T -+ PIP, V_T -+ PCO2, 
#                        M_R -+ V_T, M_R -+ PO2, M_R -+ M_V,
#                        PIP -+ V_T, PIP -+ PCO2, 
#                        Sex -+ V_T, Sex -+ PP, Sex -+ M_V, 
#                        Kg -+ V_T, Kg -+ M_V,
#                        PP -+ M_R, PP -+ PIP, PP -+ Y, PP -+ SO2, 
#                        M_V -+ M_R, M_V -+ Y,
#                        PEEP -+ PIP, PEEP -+ PP, PEEP -+ SO2, PEEP -+ M_V, PEEP -+ FO2, 
#                        NMBA -+ PP, NMBA -+ FO2, NMBA -+ PO2, 
#                        Age -+ NMBA, Age -+ FO2, 
#                        Sev -+ FO2, Sev -+ PO2, Sev -+ SO2,
#                        FO2 -+ PEEP, FO2 -+ NMBA, FO2 -+ PO2, FO2 -+ SO2, 
#                        PCO2 -+ SO2, 
#                        PO2 -+ Y
# )



# g_full = graph.formula(V_T -+ M_R, V_T -+ PIP, V_T -+ PCO2, 
#                        M_R -+ V_T, M_R -+ PO2, M_R -+ M_V,
#                        PIP -+ V_T, PIP -+ PCO2, 
#                        Sex -+ V_T, Sex -+ PP, Sex -+ M_V, 
#                        Kg -+ V_T, Kg -+ M_V,
#                        PP -+ M_R, PP -+ PIP, PP -+ Y, PP -+ SO2, 
#                        M_V -+ M_R, M_V -+ Y,
#                        PEEP -+ PIP, PEEP -+ PP, PEEP -+ SO2, PEEP -+ M_V, PEEP -+ FO2, 
#                        NMBA -+ PP, NMBA -+ FO2, NMBA -+ PO2, 
#                        Age -+ NMBA, Age -+ FO2, 
#                        Sev -+ FO2, Sev -+ PO2, Sev -+ SO2,
#                        FO2 -+ PEEP, FO2 -+ NMBA, FO2 -+ PO2, FO2 -+ SO2, 
#                        PCO2 -+ SO2, 
#                        PO2 -+ Y
# )

# g_reduced =  graph.formula(V_T -+ T_V, V_T -+ T_O, V_T -+ T_A, V_T -+ Y,V_T -+ M_R,  #V_T to others  
#                            M_R -+ PO2, M_R -+ PCO2, M_R -+ pH,  # M_R to others  ) 
#                            FO2 -+ PP, FO2 -+ M_R, FO2 -+ V_T, FO2 -+ PEEP, FO2 -+ PO2, FO2 -+ PCO2, FO2 -+ pH, FO2 -+ Y,   # FO2 to others 
#                            PP -+ pH, PP -+ PO2, PP -+ PCO2, PP -+ T_V, PP -+ T_B, PP -+ T_A,  # PP to others 
#                            PEEP -+ V_T, PEEP -+ pH, PEEP -+ PO2, PEEP -+ T_V, PEEP -+ T_B, PEEP -+ T_A, # PEEP to others
#                            Kg -+ V_T, # Kg to others 
#                            pH -+ Y,
#                            Sex -+ M_R, Sex -+ V_T, # Sex to others 
#                            Sev -+ M_R, Sev -+ V_T, Sev -+ PP, Sev -+ PEEP, Sev -+ FO2, Sev -+ PO2, Sev -+ PCO2, Sev -+ pH, Sev -+ T_V, Sev -+ T_B, Sev -+ T_A, Sev -+ T_O, Sev -+ Y, Sev -+ NMBA, # severity to others 
#                            PO2 -+ Y,  #PaO2 to others 
#                            PCO2 -+ pH, PCO2 -+ Y, # PaCO2 to others 
#                            NMBA -+ V_T, NMBA -+ PP, NMBA -+ T_A, NMBA -+ T_V, 
#                            Age -+ V_T, Age -+ PP, Age -+ PEEP, Age -+ M_R, Age -+ FO2, Age -+ Y,
#                            T_V -+ T_O, T_V -+ Y, #T_V to others 
#                            T_B -+ T_O,  #T_B to others 
#                            T_A -+ Y, T_A -+ T_O, # T_A to others 
#                            T_O -+ Y # Basic--------------------------------// ARDSNet (2000) 
# )
# 
# g_reduced_simple =  graph.formula(V_T -+ T_V, V_T -+ T_O, V_T -+ T_A, V_T -+ Y,V_T -+ M_R,  #V_T to others  
#                                   M_R -+ PO2, M_R -+ PCO2, M_R -+ pH,  # M_R to others  ) 
#                                   FO2 -+ PP, FO2 -+ M_R, FO2 -+ V_T, FO2 -+ PEEP, FO2 -+ PO2, FO2 -+ PCO2, FO2 -+ pH, FO2 -+ Y,   # FO2 to others 
#                                   PP -+ pH, PP -+ PO2, PP -+ PCO2, PP -+ T_V, PP -+ T_B, PP -+ T_A,  # PP to others 
#                                   PEEP -+ V_T, PEEP -+ pH, PEEP -+ PO2, PEEP -+ T_V, PEEP -+ T_B, PEEP -+ T_A, # PEEP to others
#                                   Kg -+ V_T, # Kg to others 
#                                   pH -+ Y,
#                                   Sex -+ M_R, Sex -+ V_T, # Sex to others 
#                                   Sev -+ M_R, Sev -+ V_T, Sev -+ PP, Sev -+ PEEP, Sev -+ FO2, Sev -+ PO2, Sev -+ PCO2, Sev -+ pH, Sev -+ T_V, Sev -+ T_B, Sev -+ T_A, Sev -+ T_O, Sev -+ Y, Sev -+ NMBA, # severity to others 
#                                   PO2 -+ Y,  #PaO2 to others 
#                                   PCO2 -+ pH, PCO2 -+ Y, # PaCO2 to others 
#                                   NMBA -+ V_T, NMBA -+ PP, NMBA -+ T_A, NMBA -+ T_V, 
#                                   T_V -+ T_O, T_V -+ Y, #T_V to others 
#                                   T_B -+ T_O,  #T_B to others 
#                                   T_A -+ Y, T_A -+ T_O, # T_A to others 
#                                   T_O -+ Y # Basic--------------------------------// ARDSNet (2000) 
# )

# g_PC_reduced = graph.formula( V_T -+ Y, V_T -+ PO2, V_T -+ PCO2, V_T -+ RR, V_T -+ PP, V_T -+ PIP, V_T -+ MV,
#                               PP -+ PIP, PP -+ Y, PP -+ PO2, PP -+ RR,
#                               PIP -+ PCO2, 
#                               PEEP -+ PIP, PEEP -+ RR, PEEP -+ PO2, PEEP -+ SO2, PEEP -+ PP, PEEP -+ V_T,
#                               MV -+ Y, MV -+ PCO2, MV -+ SO2, 
#                               RR -+ PO2, RR -+ Y, RR -+ PIP, RR -+ MV, RR -+ SO2, 
#                               FO2 -+ PEEP, FO2 -+ SO2, FO2 -+ PO2, FO2 -+ PIP, 
#                               NMBA -+ PP, NMBA -+ FO2, NMBA -+ PIP, NMBA -+ Y, NMBA -+ PCO2, NMBA -+ PO2, 
#                               Kg -+ V_T, Kg -+ PP, Kg -+ PEEP, Kg -+ MV, Kg -+ PIP, 
#                               Age -+ Sev, Age -+ PO2, Age -+ PCO2, Age -+ NMBA, Age -+ FO2, Age -+ PEEP, Age -+ PP, 
#                               Sev -+ PO2, Sev -+ FO2, Sev -+ MV, Sev -+ PEEP, Sev -+ PP, 
#                               Sex -+ V_T, Sex -+ PP, Sex -+ MV, Sev -+ SO2, 
#                               SO2 -+ Y, SO2 -+ PCO2, SO2 -+ PO2, 
#                               PO2 -+ Y, PO2 -+ PCO2, 
#                               PCO2 -+ PO2, PCO2 -+ pH,
#                               pH -+ Y
# )