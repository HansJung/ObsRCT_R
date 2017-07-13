library(causaleffect)
library(igraph)

g_PC = graph.formula( V_T -+ Y, V_T -+ PO2, V_T -+ PCO2, V_T -+ RR, V_T -+ PP, V_T -+ PIP, V_T -+ MV,
                      PP -+ PIP, PP -+ Y, PP -+ PO2, PP -+ RR,
                      PIP -+ PCO2, 
                      PEEP -+ PIP, PEEP -+ MV, PEEP -+ RR, PEEP -+ PO2, PEEP -+ SO2, PEEP -+ PP, PEEP -+ V_T,
                      MV -+ Y, MV -+ PCO2, MV -+ PEEP, MV -+ SO2, 
                      RR -+ PO2, RR -+ Y, RR -+ PEEP, RR -+ PIP, RR -+ MV, RR -+ SO2, 
                      FO2 -+ PEEP, FO2 -+ SO2, FO2 -+ PO2, FO2 -+ PIP, 
                      NMBA -+ PP, NMBA -+ FO2, NMBA -+ PIP, NMBA -+ Y, NMBA -+ PCO2, NMBA -+ PO2, 
                      Kg -+ V_T, Kg -+ PP, Kg -+ PEEP, Kg -+ MV, Kg -+ PIP, 
                      Age -+ Sev, Age -+ PO2, Age -+ PCO2, Age -+ NMBA, Age -+ FO2, Age -+ PEEP, Age -+ PP, 
                      Sev -+ PO2, Sev -+ FO2, Sev -+ MV, Sev -+ PEEP, Sev -+ PP, 
                      Sex -+ V_T, Sex -+ PP, Sex -+ MV, Sev -+ SO2, 
                      SO2 -+ PP, SO2 -+ MV, SO2 -+ Y, SO2 -+ PCO2, SO2 -+ PO2, 
                      PO2 -+ Y, PO2 -+ PCO2, 
                      PCO2 -+ PO2, PCO2 -+ pH,
                      pH -+ Y
                    )

g_PC_reduced = graph.formula( V_T -+ Y, V_T -+ PO2, V_T -+ PCO2, V_T -+ RR, V_T -+ PP, V_T -+ PIP, V_T -+ MV,
                              PP -+ PIP, PP -+ Y, PP -+ PO2, PP -+ RR,
                              PIP -+ PCO2, 
                              PEEP -+ PIP, PEEP -+ MV, PEEP -+ RR, PEEP -+ PO2, PEEP -+ SO2, PEEP -+ PP, PEEP -+ V_T,
                              MV -+ Y, MV -+ PCO2, MV -+ SO2, 
                              RR -+ PO2, RR -+ Y, RR -+ PIP, RR -+ MV, RR -+ SO2, 
                              FO2 -+ PEEP, FO2 -+ SO2, FO2 -+ PO2, FO2 -+ PIP, 
                              NMBA -+ PP, NMBA -+ FO2, NMBA -+ PIP, NMBA -+ Y, NMBA -+ PCO2, NMBA -+ PO2, 
                              Kg -+ V_T, Kg -+ PP, Kg -+ PEEP, Kg -+ MV, Kg -+ PIP, 
                              Age -+ Sev, Age -+ PO2, Age -+ PCO2, Age -+ NMBA, Age -+ FO2, Age -+ PEEP, Age -+ PP, 
                              Sev -+ PO2, Sev -+ FO2, Sev -+ MV, Sev -+ PEEP, Sev -+ PP, 
                              Sex -+ V_T, Sex -+ PP, Sex -+ MV, Sev -+ SO2, 
                              SO2 -+ Y, SO2 -+ PCO2, SO2 -+ PO2, 
                              PO2 -+ Y, PO2 -+ PCO2, 
                              PCO2 -+ PO2, PCO2 -+ pH,
                              pH -+ Y
)

g = g_PC_reduced
hidden = c(9,10)
g = set.edge.attribute(g,"description",hidden,"U")

causal_effect = causal.effect(y="Y",x="PEEP",z =c("V_T","RR","FO2","RR"),G=g, simp=F, steps=F)
# causal_effect = causal.effect(y="Y",x="NMBA",z =c("V_T", "RR","FO2"),G=g, simp=T)
# causal_effect = causal.effect(y="Y",x="NMBA",z =NULL,G=g, simp=T)
print(V(g))
print(E(g))
print(causal_effect)
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