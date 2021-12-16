#table 1 

table1(~Participant_Nares_MRSA_Incid + Participant_Axilla_MRSA_Incid + Participant_Inguinal_MRSA_Incid + Sofa_MRSA_Prev|Have_Pets, data = ada.household.wide, render.missing = NULL,
       render.categorical = 'FREQ (PCTnoNA%)', caption = 'Participant MRSA Re-colonization Incidence Stratified by Pet Ownership')

table1(~Participant_Nares_MRSA_Incid + Participant_Axilla_MRSA_Incid + Participant_Inguinal_MRSA_Incid + Sofa_MRSA_Prev|race, data = ada.household.wide, render.missing = NULL,
       render.categorical = 'FREQ (PCTnoNA%)', caption = 'Participant MRSA Re-colonization Incidence Stratified by Race')

table1(~Participant_Nares_MRSA_Incid + Participant_Axilla_MRSA_Incid + Participant_Inguinal_MRSA_Incid + Sofa_MRSA_Prev|sex, data = ada.household.wide, render.missing = NULL,
       render.categorical = 'FREQ (PCTnoNA%)', caption = 'Participant MRSA Re-colonization Incidence Stratified by Sex')

table1(~Participant_Nares_MRSA_Incid + Participant_Axilla_MRSA_Incid + Participant_Inguinal_MRSA_Incid, data = ada.household.wide, render.missing = NULL,
       render.categorical = 'FREQ (PCTnoNA%)', caption = 'Participant MRSA Re-colonization Incidence')

table1(~Sofa_MRSA_Prev + sofa_clean_never + Clean_Sofa_1_3_Follow_9month|Have_Pets, data = ada.household.wide, render.missing = NULL,
       render.categorical = 'FREQ (PCTnoNA%)', caption = 'Environment MRSA Colonization Prevalence Stratified by Pet Ownership')

table1(~Sofa_MRSA_Prev + TV_Remote_MRSA_Prev + Bath_shower_1_2perweek_Follow_1month, data = ada.household.wide, render.missing = NULL,
       render.categorical = 'FREQ (PCTnoNA%)', caption = 'Environment MRSA Colonization Prevalence Stratified by Pet Ownership')

table1(~Sofa_MRSA_Prev + Sofa_MSSA_Prev + Sofa_MSSP_Prev|Have_Pets, data = ada.household.wide, 
       render.missing = NULL, render.categorical = 'FREQ (PCTnoNA%)', caption = 'Sofa Colonization Prevalence with Staph Strains (Stratified by Pet Ownership)')

table1(~Sofa_MRSA_Prev + Sofa_MSSA_Prev + Sofa_MSSP_Prev + Sofa_MRSP_Prev|Clean_Sofa_Never_Follow_9month, data = ada.household.wide, 
       render.missing = NULL, render.categorical = 'FREQ (PCTnoNA%)', caption = 'Sofa Colonization Prevalence with Staph Strains (Stratified by Pet Ownership)')
       
table1(~Sofa_MRSA_Prev + Sofa_MSSA_Prev + Sofa_MSSP_Prev + Sofa_MRSP_Prev|Have_Pets, data = ada.household.wide, 
       render.missing = NULL, render.categorical = 'FREQ (PCTnoNA%)', caption = 'Sofa Colonization Prevalence with Staph Strains (Stratified by Pet Ownership)')
