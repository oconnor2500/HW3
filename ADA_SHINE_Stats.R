#bivariate analysis----
#sofa prev MRSA vs participant MRSA Inguinal Fold Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$Sofa_MRSA_Prev)
#sofa prev MRSA vs participant MRSA Inguinal Fold Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$sex)
#White vs participant MRSA Inguinal Fold Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$race_white)
#Black vs participant MRSA Inguinal Fold Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$race_black)
#Sofa clean never vs Participant MRSA Inguinal Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$sofa_clean_never)
#Sofa clean 0 to 1 per month vs Participant MRSA Inguinal Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$sofa_clean_0_1)
#Sofa clean 1 to 3 month vs Participant MRSA Inguinal Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$sofa_clean_1_3)
#Sofa clean 4 per month Participant MRSA Inguinal Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$sofa_clean_4)
#Sofa clean higher than 4 per month vs Participant MRSA Nares Incidence
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$sofa_clean_higherthan4)

#having pet vs participant MRSA inguinal fold
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$Have_Pets)
#having pet vs Sofa MRSA
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$Have_Pets)
#White vs Sofa MRSA
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$race_white)
#Black vs Sofa MRSA
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$race_black)
#additional stuff----

#MRSA vs MSSP colonization
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$Sofa_MSSP_Prev)
#Participant Incidence MRSA vs clean sofa_never_9month
fisher.test(ada.household.wide$Participant_Inguinal_MRSA_Incid, ada.household.wide$cleansofanever)
#sofa prev MRSA vs clean sofa_0_1_9month
fisher.test(ada.household.wide$SParticipant_Nares_MRSA_Prev, ada.household.wide$Clean_Sofa_0_1_Follow_1month)
#sofa prev MRSA vs clean sofa_1_3_9month
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$Clean_Sofa_1_3_Follow_1month)
#sofa prev MRSA vs clean sofa_4_9month
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$Clean_Sofa_4_Follow_1month)
#sofa prev MRSA vs clean sofa_higherthan4_9month
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$Clean_Sofa_higherthan4_Follow_1month)
#sofa prev MSSA vs clean sofa_never_9month
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Clean_Sofa_Never_Follow_9month)
#sofa prev MSSA vs clean sofa_0_1_9month
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Clean_Sofa_0_1_Follow_9month)
#sofa prev MSSA vs clean sofa_1_3_9month
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Clean_Sofa_1_3_Follow_9month)
#sofa prev MSSA vs clean sofa_4_9month
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Clean_Sofa_4_Follow_9month)
#sofa prev MSSA vs clean sofa_higherthan4_9month
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Clean_Sofa_higherthan4_Follow_9month)
#sofa prev MSSP vs clean sofa_never_9month
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Clean_Sofa_Never_Follow_9month)
#sofa prev MSSP vs clean sofa_0_1_9month
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Clean_Sofa_0_1_Follow_9month)
#sofa prev MSSP vs clean sofa_1_3_9month
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Clean_Sofa_1_3_Follow_9month)
#sofa prev MSSP vs clean sofa_4_9month
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Clean_Sofa_4_Follow_9month)
#sofa prev MSSP vs clean sofa_higherthan4_9month
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Clean_Sofa_higherthan4_Follow_9month)

#sofa prev MRSA vs Have pets
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$Have_Pets)
#sofa prev MSSA vs Have pets
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Have_Pets)
#sofa prev MSSP vs Have pets
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Have_Pets)
#sofa prev MRSP vs Have pets
fisher.test(ada.household.wide$Sofa_MRSP_Prev, ada.household.wide$Have_Pets)

#sofa prev MRSA vs Sofa prev MSSA
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$Sofa_MSSA_Prev)
chisq.test(ada.household.wide$Sofa_MRSA_Prev, y = ada.household.wide$Sofa_MSSA_Prev)
#sofa prev MRSA vs Sofa Prev MSSP
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$Sofa_MSSP_Prev)
chisq.test(ada.household.wide$Sofa_MRSA_Prev, y = ada.household.wide$Sofa_MSSP_Prev)
#sofa prev MRSA vs Sofa Prev MRSP
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$Sofa_MRSP_Prev)
chisq.test(ada.household.wide$Sofa_MRSA_Prev, y = ada.household.wide$Sofa_MRSP_Prev)
#sofa prev MSSA vs Bath Shower less 1per week
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Bath_shower_less1perweek_Follow_9month)
chisq.test(ada.household.wide$Sofa_MSSA_Prev, y = ada.household.wide$Bath_shower_less1perweek_Follow_9month)
#sofa prev MSSA vs Bath Shower 1_2per week
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Bath_shower_1_2perweek_Follow_9month)
#sofa prev MSSA vs Bath Shower 3_4per week
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Bath_shower_3_4perweek_Follow_9month)
#sofa prev MSSA vs Bath Shower 5_6per week
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Bath_shower_5_6perweek_Follow_9month)
#sofa prev MSSA vs Bath Shower 1per day
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Bath_shower_1perday_Follow_9month)
#sofa prev MSSA vs Bath Shower 2ormoreper day
fisher.test(ada.household.wide$Sofa_MSSA_Prev, ada.household.wide$Bath_shower_2ormoreperday_Follow_9month)

#sofa prev MRSA vs Bath Shower less 1per week
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$bath_shower_less1perweek)
chisq.test(ada.household.wide$Sofa_MSSA_Prev, y = ada.household.wide$Bath_shower_less1perweek_Follow_9month)
#sofa prev MRSA vs Bath Shower 1_2per week
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$bath_shower_1_2perweek)
#sofa prev MRSA vs Bath Shower 3_4per week
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$bath_shower_3_4perweek)
#sofa prev MRSA vs Bath Shower 5_6per week
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$bath_shower_5_6perweek)
#sofa prev MRSA vs Bath Shower 1per day
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$bath_shower_1perday)
#sofa prev MRSA vs Bath Shower 2ormoreper day
fisher.test(ada.household.wide$Sofa_MRSA_Prev, ada.household.wide$bath_shower_2ormoreperday)

#Participant MRSA vs Bath Shower less 1per week
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$bath_shower_less1perweek)
chisq.test(ada.household.wide$Sofa_MSSA_Prev, y = ada.household.wide$Bath_shower_less1perweek_Follow_9month)
#Participantvs Bath Shower 1_2per week
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$bath_shower_1_2perweek)
#Participant vs Bath Shower 3_4per week
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$bath_shower_3_4perweek)
#Participant vs Bath Shower 5_6per week
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$bath_shower_5_6perweek)
#sParticipant vs Bath Shower 1per day
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$bath_shower_1perday)
#Participant vs Bath Shower 2ormoreper day
fisher.test(ada.household.wide$Participant_Nares_MRSA_Prev, ada.household.wide$bath_shower_2ormoreperday)

#sofa prev MSSP vs Bath Shower less 1per week
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Bath_shower_less1perweek_Follow_9month)
chisq.test(ada.household.wide$Sofa_MSSA_Prev, y = ada.household.wide$Bath_shower_less1perweek_Follow_9month)
#sofa prev MSSP vs Bath Shower 1_2per week
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Bath_shower_1_2perweek_Follow_9month)
#sofa prev MSSP vs Bath Shower 3_4per week
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Bath_shower_3_4perweek_Follow_9month)
#sofa prev MSSP vs Bath Shower 5_6per week
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Bath_shower_5_6perweek_Follow_9month)
#sofa prev MSSP vs Bath Shower 1per day
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Bath_shower_1perday_Follow_9month)
#sofa prev MSSP vs Bath Shower 2ormoreper day
fisher.test(ada.household.wide$Sofa_MSSP_Prev, ada.household.wide$Bath_shower_2ormoreperday_Follow_9month)

#box-tidwell

#multivariate modeling----
ada.sofaMRSA.Petownership.participant.model <- glm(Participant_Nares_MRSA_Prev ~ Sofa_MRSA_Prev + Have_Pets, data = ada.household.wide,
                                       family = 'binomial')
ada.sofaMRSA.participant.model <- glm(Participant_Nares_MRSA_Prev ~ Sofa_MRSA_Prev, data = ada.household.wide,
                                     family = 'binomial')
ada.sofaMRSA.sofaMRSA.model <- glm(Sofa_MRSA_Prev ~ Sofa_MSSA_Prev, data = ada.household.wide,
                                                  family = 'binomial')
ada.sofaMSSP.Petownership.0_1clean.model <- glm(Sofa_MSSP_Prev ~ Have_Pets + Clean_Sofa_0_1_Follow_9month, data = ada.household.wide,
                                                    family = 'binomial')
  
ada.sofaMSSP.Petownership.1_3clean.model <- glm(Sofa_MSSP_Prev ~ Have_Pets + Clean_Sofa_1_3_Follow_9month, data = ada.household.wide,
                                                    family = 'binomial')

ada.sofaMSSP.Petownership.4clean.model <- glm(Sofa_MSSP_Prev ~ Have_Pets + Clean_Sofa_4_Follow_9month, data = ada.household.wide,
                                                  family = 'binomial')
ada.sofaMSSP.Petownership.higherthan4clean.model <- glm(Sofa_MSSP_Prev ~ Have_Pets + Clean_Sofa_higherthan4_Follow_9month, data = ada.household.wide,
                                                  family = 'binomial')
summary(ada.sofaMSSP.Petownership.neverclean.model)
  
summary(ada.sofaMRSA.Petownership.participant.model)
SofaMRSA.Pet.ParticipantORmodel<-exp(cbind(OR = coef(ada.sofaMRSA.Petownership.participant.model), confint(ada.sofaMRSA.Petownership.participant.model)))
SofaMRSA.Pet.ParticipantORmodel

summary(ada.sofaMRSA.participant.model)
SofaMRSA.participant.ORmodel<-exp(cbind(OR = coef(ada.sofaMRSA.participant.model), confint(ada.sofaMRSA.participant.model)))
SofaMRSA.participant.ORmodel

lrtest(ada.sofaMRSA.Petownership.participant.model, ada.sofaMRSA.participant.model)

summary(ada.sofaMSSP.Petownership.0_1clean.model)
SofaMSSP.Pet.0_1.ORmodel<-exp(cbind(OR = coef(ada.sofaMSSP.Petownership.0_1clean.model), confint(ada.sofaMSSP.Petownership.0_1clean.model)))
SofaMSSP.Pet.0_1.ORmodel

summary(ada.sofaMSSP.Petownership.1_3clean.model)
SofaMSSP.Pet.1_3.ORmodel<-exp(cbind(OR = coef(ada.sofaMSSP.Petownership.1_3clean.model), confint(ada.sofaMSSP.Petownership.1_3clean.model)))
SofaMSSP.Pet.1_3.ORmodel

summary(ada.sofaMSSP.Petownership.4clean.model)
SofaMSSP.Pet.4.ORmodel<-exp(cbind(OR = coef(ada.sofaMSSP.Petownership.4clean.model), confint(ada.sofaMSSP.Petownership.4clean.model)))
SofaMSSP.Pet.4.ORmodel

summary(ada.sofaMSSP.Petownership.higherthan4clean.model)
SofaMSSP.Pet.higherthan4.ORmodel<-exp(cbind(OR = coef(ada.sofaMSSP.Petownership.higherthan4clean.model), confint(ada.sofaMSSP.Petownership.higherthan4clean.model)))
SofaMSSP.Pet.higherthan4.ORmodel
odds.n.ends(ada.sofaMSSP.Petownership.model)

