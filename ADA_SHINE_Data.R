#load raw data
ada.shine <- read_csv(file = '/Users/christopheroconnor/Dropbox/My things/School Stuff/Fall 2021/MPH 5245 Adv Data Analysis/Datasets/ADA_SHINE.csv')
#load data for PC
ada.shine <- read_csv(file = '/Users/oconn/Dropbox/My things/School Stuff/Fall 2021/MPH 5245 Adv Data Analysis/Datasets/ADA_SHINE.csv')
#cleaning data----
ada.shine.clean <- ada.shine %>% 
  mutate(visit = recode_factor(.x = redcap_event_name,
                               'enrollment_arm_1' = 'enroll',
                               '1month_followup_vi_arm_1' = '1month',
                               '3month_followup_vi_arm_1' = '3month',
                               '6month_followup_vi_arm_1' = '6month',
                               '9month_followup_vi_arm_1' = '9month')) %>% 
  mutate(sex = recode_factor(.x = ei_sex,
                             '0' = 'Male',
                             '1' = 'Female')) %>%
  mutate(pet_time_in_home = na_if(x = pet_time_in_home, y = '777')) %>% 
  mutate(race = recode_factor(.x = ei_race,
                              '0' = 'White',
                              '1' = 'Black',
                              '2' = 'Asian',
                              '3' = 'Pacific Islander',
                              '4' = 'American Indian',
                              '5' = 'Alaskan Native',
                              '6' = 'Biracial',
                              '55' = 'Other')) %>% 
  mutate(race_white = ifelse(race == 'White', 1, 0)) %>% 
  mutate(race_white = recode_factor(.x = race_white,
                                    '0' = 'No',
                                    '1' = 'Yes')) %>% 
  mutate(race_black = ifelse(race == 'Black', 1, 0)) %>% 
  mutate(race_black = recode_factor(.x = race_black,
                                    '0' = 'No',
                                    '1' = 'Yes')) %>% 
  mutate(race_asian = ifelse(race == 'Asian', 1, 0)) %>% 
  mutate(race_asian = recode_factor(.x = race_asian,
                                    '0' = 'No',
                                    '1' = 'Yes')) %>% 
  mutate(race_pacific = ifelse(race == 'Pacific Islander', 1, 0)) %>% 
  mutate(race_pacific = recode_factor(.x = race_pacific,
                                      '0' = 'No',
                                      '1' = 'Yes')) %>% 
  mutate(race_native = ifelse(race == 'American Indian', 1, 0)) %>% 
  mutate(race_native = recode_factor(.x = race_native,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>% 
  mutate(race_alaskan = ifelse(race == 'Alaskan Native', 1, 0)) %>% 
  mutate(race_alaskan = recode_factor(.x = race_alaskan,
                                      '0' = 'No',
                                      '1' = 'Yes')) %>% 
  mutate(race_biracial = ifelse(race == 'Biracial', 1, 0)) %>% 
  mutate(race_biracial = recode_factor(.x = race_biracial,
                                       '0' = 'No',
                                       '1' = 'Yes')) %>% 
  mutate(Skin_Disorder = recode_factor(.x = ei_skin_disorder,
                                       '1' = 'Yes',
                                       '0' = 'No')) %>% 
  mutate(Pet_Surgery = recode_factor(.x = pet_surgery,
                                     '1' = 'Yes',
                                     '0' = 'No')) %>% 
  mutate(Pet_skin_condition = recode_factor(.x = pet_skin_cond,
                                            '1' = 'Yes',
                                            '0' = 'No')) %>% 
  mutate(Participant_PetStaph_Axilla = recode_factor (.x = micro_par_petstaph_axilla,
                                                      '0' = "No Growth",
                                                      '4' = 'MRSP',
                                                      '5' = 'MSSP',
                                                      '6' = 'MSSS',
                                                      '7' = 'MSSD',
                                                      '3' = 'Swab not obtained')) %>% 
  mutate(Participant_Axilla_MSSP = ifelse(Participant_PetStaph_Axilla == 'MSSP', 1, 0)) %>% 
  mutate(Participant_Axilla_MSSP = recode_factor(.x = Participant_Axilla_MSSP,
                                                 '0' = 'No',
                                                 '1' = 'Yes')) %>%
  mutate(Participant_Axilla_MSSS = ifelse(Participant_PetStaph_Axilla == 'MSSS', 1, 0)) %>% 
  mutate(Participant_Axilla_MSSS = recode_factor(.x = Participant_Axilla_MSSS,
                                                 '0' = 'No',
                                                 '1' = 'Yes')) %>% 
  mutate(Participant_Axilla_MRSP = ifelse(Participant_PetStaph_Axilla == 'MRSP', 1, 0)) %>% 
  mutate(Participant_Axilla_MRSP = recode_factor(.x = Participant_Axilla_MRSP,
                                                 '0' = 'No',
                                                 '1' = 'Yes')) %>% 
  mutate(Participant_Axilla_MSSD = ifelse(Participant_PetStaph_Axilla == 'MSSD', 1, 0)) %>% 
  mutate(Participant_Axilla_MSSD = recode_factor(.x = Participant_Axilla_MSSD,
                                                 '0' = 'No',
                                                 '1' = 'Yes')) %>% 
  mutate(Participant_PetStaph_Nares = recode_factor (.x = micro_par_petstaph_nares,
                                                     '0' = "No Growth",
                                                     '4' = 'MRSP',
                                                     '5' = 'MSSP',
                                                     '6' = 'MSSS',
                                                     '7' = 'MSSD',
                                                     '3' = 'Swab not obtained')) %>%
  mutate(Participant_Nares_MSSP = ifelse(Participant_PetStaph_Nares == 'MSSP', 1, 0)) %>% 
  mutate(Participant_Nares_MSSP = recode_factor(.x = Participant_Nares_MSSP,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>% 
  mutate(Participant_Nares_MRSP = ifelse(Participant_PetStaph_Nares == 'MRSP', 1, 0)) %>% 
  mutate(Participant_Nares_MRSP = recode_factor(.x = Participant_Nares_MRSP,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>% 
  mutate(Participant_Nares_MSSS = ifelse(Participant_PetStaph_Nares == 'MSSS', 1, 0)) %>% 
  mutate(Participant_Nares_MSSS = recode_factor(.x = Participant_Nares_MSSS,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>% 
  mutate(Participant_Nares_MSSD = ifelse(Participant_PetStaph_Nares == 'MSSD', 1, 0)) %>% 
  mutate(Participant_Nares_MSSD = recode_factor(.x = Participant_Nares_MSSD,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>% 
  mutate(Participant_PetStaph_InguinalFold = recode_factor (.x = micro_par_petstaph_inguinalfold,
                                                            '0' = "No Growth",
                                                            '4' = 'MRSP',
                                                            '5' = 'MSSP',
                                                            '6' = 'MSSS',
                                                            '7' = 'MSSD',
                                                            '3' = 'Swab not obtained')) %>%
  mutate(Participant_InguinalFold_MSSP = ifelse(Participant_PetStaph_InguinalFold == 'MSSP', 1, 0)) %>% 
  mutate(Participant_InguinalFold_MSSP = recode_factor(.x = Participant_InguinalFold_MSSP,
                                                       '0' = 'No',
                                                       '1' = 'Yes')) %>% 
  mutate(Participant_InguinalFold_MRSP = ifelse(Participant_PetStaph_InguinalFold == 'MRSP', 1, 0)) %>% 
  mutate(Participant_InguinalFold_MRSP = recode_factor(.x = Participant_InguinalFold_MRSP,
                                                       '0' = 'No',
                                                       '1' = 'Yes')) %>% 
  mutate(Participant_InguinalFold_MSSS = ifelse(Participant_PetStaph_InguinalFold == 'MSSS', 1, 0)) %>% 
  mutate(Participant_InguinalFold_MSSS = recode_factor(.x = Participant_InguinalFold_MSSS,
                                                       '0' = 'No',
                                                       '1' = 'Yes')) %>% 
  mutate(Participant_InguinalFold_MSSD = ifelse(Participant_PetStaph_InguinalFold == 'MSSD', 1, 0)) %>% 
  mutate(Participant_InguinalFold_MSSD = recode_factor(.x = Participant_InguinalFold_MSSD,
                                                       '0' = 'No',
                                                       '1' = 'Yes')) %>% 
  mutate(Participant_Staph_Axilla = recode_factor (.x = micro_par_axilla,
                                                   '0' = 'No Growth',
                                                   '1' = 'MRSA',
                                                   '2' = 'MSSA',
                                                   '3' = 'Swab not obtained')) %>% 
  mutate(Participant_Axilla_MRSA = ifelse(Participant_Staph_Axilla == 'MRSA', 1, 0)) %>% 
  mutate(Participant_Axilla_MRSA = recode_factor(.x = Participant_Axilla_MRSA,
                                                 '0' = 'No',
                                                 '1' = 'Yes')) %>% 
  mutate(Participant_Axilla_MSSA = ifelse(Participant_Staph_Axilla == 'MSSA', 1, 0)) %>% 
  mutate(Participant_Axilla_MSSA = recode_factor(.x = Participant_Axilla_MSSA,
                                                 '0' = 'No',
                                                 '1' = 'Yes')) %>% 
  mutate(Participant_Staph_Nares = recode_factor (.x = micro_par_nares,
                                                  '0' = 'No Growth',
                                                  '1' = 'MRSA',
                                                  '2' = 'MSSA',
                                                  '3' = 'Swab not obtained')) %>% 
  mutate(Participant_Nares_MRSA = ifelse(Participant_Staph_Nares == 'MRSA', 1, 0)) %>% 
  mutate(Participant_Nares_MRSA = recode_factor(.x = Participant_Nares_MRSA,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>% 
  mutate(Participant_Nares_MSSA = ifelse(Participant_Staph_Nares == 'MSSA', 1, 0)) %>% 
  mutate(Participant_Nares_MSSA = recode_factor(.x = Participant_Nares_MSSA,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>% 
  mutate(Participant_Staph_InguinalFold = recode_factor (.x = micro_par_inguinalfold,
                                                         '0' = 'No Growth',
                                                         '1' = 'MRSA',
                                                         '2' = 'MSSA',
                                                         '3' = 'Swab not obtained')) %>% 
  mutate(Participant_InguinalFold_MRSA = ifelse(Participant_Staph_InguinalFold == 'MRSA', 1, 0)) %>% 
  mutate(Participant_InguinalFold_MRSA = recode_factor(.x = Participant_InguinalFold_MRSA,
                                                       '0' = 'No',
                                                       '1' = 'Yes')) %>% 
  mutate(Participant_InguinalFold_MSSA = ifelse(Participant_Staph_InguinalFold == 'MSSA', 1, 0)) %>% 
  mutate(Participant_InguinalFold_MSSA = recode_factor(.x = Participant_InguinalFold_MSSA,
                                                       '0' = 'No',
                                                       '1' = 'Yes')) %>% 
  mutate(Enroll_SSTI_last_12mo = recode_factor(.x = ei_ssti_12mo,
                                               '1' = 'Yes',
                                               '0' = 'No')) %>% 
  mutate(Followup_SSTI_last_12mo = recode_factor(.x = pf_ssti_lastvisit,
                                                 '1' = 'Yes',
                                                 '0' = 'No')) %>%
  mutate(Type_of_Pet = recode_factor(.x = pet_type,
                                     '1' = 'Dog',
                                     '2' = 'Cat')) %>% 
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '3')) %>% 
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '4')) %>%
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '5')) %>%
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '6')) %>%
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '7')) %>%
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '8')) %>%
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '9')) %>%
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '10')) %>%
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '11')) %>%
  mutate(Pet_Hair = recode_factor(.x = pet_hair,
                                  '1' = 'Long',
                                  '2' = 'Short')) %>% 
  mutate(Pet_In_or_Out = recode_factor(.x = pet_inout,
                                       '1' = 'Indoors',
                                       '0' = 'Outdoors')) %>% 
  mutate(Pet_Energy = recode_factor(.x = pet_energy,
                                    '1' = 'Yes',
                                    '0' = 'No')) %>% 
  mutate(Pet_Appetite = recode_factor(.x = pet_appetite,
                                      '1' = 'Yes',
                                      '0' = 'No')) %>% 
  mutate(Pet_in_Daycare_at_Enroll = recode_factor(.x = pet_daycare,
                                                  '1' = 'Yes',
                                                  '0' = 'No')) %>% 
  mutate(Pet_in_Daycare_followup = recode_factor(.x = pf_pet_daycare,
                                                 '1' = 'Yes',
                                                 '0' = 'No')) %>% 
  mutate(Pet_take_Antibiotics_6mo_enroll = recode_factor(.x = pet_rx_6mo,
                                                         '1' = 'Yes',
                                                         '0' = 'No',
                                                         '777' = 'Unsure')) %>% 
  mutate(Pet_Staph_Dorsal_Fur = recode_factor (.x = pet_dorsal_fur_growth,
                                               '0' = 'No Growth',
                                               '1' = 'MRSA',
                                               '2' = 'MSSA',
                                               '3' = 'Swab not obtained')) %>% 
  mutate(Dorsal_Fur_MRSA = ifelse(Pet_Staph_Dorsal_Fur == 'MRSA', 1, 0)) %>% 
  mutate(Dorsal_Fur_MRSA = recode_factor(.x = Dorsal_Fur_MRSA,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Dorsal_Fur_MSSA = ifelse(Pet_Staph_Dorsal_Fur == 'MSSA', 1, 0)) %>% 
  mutate(Dorsal_Fur_MSSA = recode_factor(.x = Dorsal_Fur_MSSA,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Pet_Staph_Mouth = recode_factor (.x = pet_mouth_growth,
                                          '0' = 'No Growth',
                                          '1' = 'MRSA',
                                          '2' = 'MSSA',
                                          '3' = 'Swab not obtained')) %>% 
  mutate(Pet_Mouth_MRSA = ifelse(Pet_Staph_Mouth== 'MRSA', 1, 0)) %>% 
  mutate(Pet_Mouth_MRSA = recode_factor(.x = Pet_Mouth_MRSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Pet_Mouth_MSSA = ifelse(Pet_Staph_Mouth== 'MSSA', 1, 0)) %>% 
  mutate(Pet_Mouth_MSSA = recode_factor(.x = Pet_Mouth_MSSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Pet_PetStaph_Dorsal_Fur = recode_factor (.x = pet_ps_dorsal_fur_growth,
                                                  '0' = "No Growth",
                                                  '4' = 'MRSP',
                                                  '5' = 'MSSP',
                                                  '6' = 'MSSS',
                                                  '7' = 'MSSD',
                                                  '3' = 'Swab not obtained')) %>%
  mutate(Dorsal_Fur_MSSP = ifelse(Pet_PetStaph_Dorsal_Fur == 'MSSP', 1, 0)) %>% 
  mutate(Dorsal_Fur_MSSP = recode_factor(.x = Dorsal_Fur_MSSP,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Dorsal_Fur_MRSP = ifelse(Pet_PetStaph_Dorsal_Fur == 'MRSP', 1, 0)) %>% 
  mutate(Dorsal_Fur_MRSP = recode_factor(.x = Dorsal_Fur_MRSP,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Dorsal_Fur_MSSS = ifelse(Pet_PetStaph_Dorsal_Fur == 'MSSS', 1, 0)) %>% 
  mutate(Dorsal_Fur_MSSS = recode_factor(.x = Dorsal_Fur_MSSS,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Dorsal_Fur_MSSD = ifelse(Pet_PetStaph_Dorsal_Fur == 'MSSD', 1, 0)) %>% 
  mutate(Dorsal_Fur_MSSD = recode_factor(.x = Dorsal_Fur_MSSD,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Pet_PetStaph_Mouth = recode_factor (.x = pet_ps_mouth_growth,
                                             '0' = "No Growth",
                                             '4' = 'MRSP',
                                             '5' = 'MSSP',
                                             '6' = 'MSSS',
                                             '7' = 'MSSD',
                                             '3' = 'Swab not obtained')) %>%
  mutate(Pet_Mouth_MSSP = ifelse(Pet_PetStaph_Mouth == 'MSSP', 1, 0)) %>% 
  mutate(Pet_Mouth_MSSP = recode_factor(.x = Pet_Mouth_MSSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Pet_Mouth_MRSP = ifelse(Pet_PetStaph_Mouth == 'MRSP', 1, 0)) %>% 
  mutate(Pet_Mouth_MRSP = recode_factor(.x = Pet_Mouth_MRSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Pet_Mouth_MSSS = ifelse(Pet_PetStaph_Mouth == 'MSSS', 1, 0)) %>% 
  mutate(Pet_Mouth_MSSS = recode_factor(.x = Pet_Mouth_MSSS,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Pet_Mouth_MSSD = ifelse(Pet_PetStaph_Mouth == 'MSSD', 1, 0)) %>% 
  mutate(Pet_Mouth_MSSD = recode_factor(.x = Pet_Mouth_MSSD,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Sofa_Staph_growth = recode_factor(.x = micro_env_9_growth,
                                           '0' = 'No Growth',
                                           '1' = 'MRSA',
                                           '2' = 'MSSA',
                                           '3' = 'Swab not obtained')) %>% 
  mutate(Sofa_PetStaph_growth = recode_factor(.x = micro_env_petstaph_9_growth,
                                              '0' = 'No Growth',
                                              '4' = 'MRSP',
                                              '5' = 'MSSP',
                                              '6' = 'MSSS',
                                              '7' = 'MSSD',
                                              '3' = 'Swab not obtained')) %>% 
  mutate(Sofa_MRSA = ifelse(Sofa_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Sofa_MRSA = recode_factor(.x = Sofa_MRSA,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>%
  mutate(Sofa_MSSA = ifelse(Sofa_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Sofa_MSSA = recode_factor(.x = Sofa_MSSA,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>%
  mutate(Sofa_MSSP = ifelse(Sofa_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Sofa_MSSP = recode_factor(.x = Sofa_MSSP,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>%
  mutate(Sofa_MRSP = ifelse(Sofa_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Sofa_MRSP = recode_factor(.x = Sofa_MRSP,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>% 
  mutate(Sofa_MSSS = ifelse(Sofa_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Sofa_MSSS = recode_factor(.x = Sofa_MSSS,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>% 
  mutate(Sofa_MSSD = ifelse(Sofa_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Sofa_MSSD = recode_factor(.x = Sofa_MSSD,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>% 
  mutate(Bedsheets1_Staph_growth = recode_factor(.x = micro_env_19a_growth,
                                                 '0' = 'No Growth',
                                                 '1' = 'MRSA',
                                                 '2' = 'MSSA',
                                                 '3' = 'Swab not obtained')) %>% 
  mutate(Bedsheets1_PetStaph_growth = recode_factor(.x = micro_env_petstaph_19a_growth,
                                                    '0' = 'No Growth',
                                                    '4' = 'MRSP',
                                                    '5' = 'MSSP',
                                                    '6' = 'MSSS',
                                                    '7' = 'MSSD',
                                                    '3' = 'Swab not obtained')) %>%
  mutate(Bedsheets1_MRSA = ifelse(Bedsheets1_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Bedsheets1_MRSA = recode_factor(.x = Bedsheets1_MRSA,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>%
  mutate(Bedsheets1_MSSA = ifelse(Bedsheets1_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Bedsheets1_MSSA = recode_factor(.x = Bedsheets1_MSSA,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>%
  mutate(Bedsheets1_MSSP = ifelse(Bedsheets1_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Bedsheets1_MSSP = recode_factor(.x = Bedsheets1_MSSP,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Bedsheets1_MRSP = ifelse(Bedsheets1_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Bedsheets1_MRSP = recode_factor(.x = Bedsheets1_MRSP,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Bedsheets1_MSSS = ifelse(Bedsheets1_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Bedsheets1_MSSS = recode_factor(.x = Bedsheets1_MSSP,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Bedsheets1_MSSD = ifelse(Bedsheets1_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Bedsheets1_MSSD = recode_factor(.x = Bedsheets1_MSSD,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Bedsheets2_Staph_growth = recode_factor(.x = micro_env_19b_growth,
                                                 '0' = 'No Growth',
                                                 '1' = 'MRSA',
                                                 '2' = 'MSSA',
                                                 '3' = 'Swab not obtained')) %>% 
  mutate(Bedsheets2_PetStaph_growth = recode_factor(.x = micro_env_petstaph_19b_growth,
                                                    '0' = 'No Growth',
                                                    '4' = 'MRSP',
                                                    '5' = 'MSSP',
                                                    '6' = 'MSSS',
                                                    '7' = 'MSSD',
                                                    '3' = 'Swab not obtained')) %>%
  mutate(Bedsheets2_MRSA = ifelse(Bedsheets2_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Bedsheets2_MRSA = recode_factor(.x = Bedsheets2_MRSA,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>%
  mutate(Bedsheets2_MSSA = ifelse(Bedsheets2_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Bedsheets2_MSSA = recode_factor(.x = Bedsheets2_MSSA,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>%
  mutate(Bedsheets2_MSSP = ifelse(Bedsheets2_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Bedsheets2_MSSP = recode_factor(.x = Bedsheets2_MSSP,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Bedsheets2_MRSP = ifelse(Bedsheets2_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Bedsheets2_MRSP = recode_factor(.x = Bedsheets2_MRSP,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Bedsheets2_MSSS = ifelse(Bedsheets2_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Bedsheets2_MSSS = recode_factor(.x = Bedsheets2_MSSS,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(Bedsheets2_MSSD = ifelse(Bedsheets2_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Bedsheets2_MSSD = recode_factor(.x = Bedsheets2_MSSD,
                                         '0' = 'No',
                                         '1' = 'Yes')) %>% 
  mutate(TV_Remote_Staph_growth = recode_factor(.x = micro_env_5_growth,
                                                '0' = 'No Growth',
                                                '1' = 'MRSA',
                                                '2' = 'MSSA',
                                                '3' = 'Swab not obtained')) %>% 
  mutate(TV_Remote_PetStaph_growth = recode_factor(.x = micro_env_petstaph_5_growth,
                                                   '0' = 'No Growth',
                                                   '4' = 'MRSP',
                                                   '5' = 'MSSP',
                                                   '6' = 'MSSS',
                                                   '7' = 'MSSD',
                                                   '3' = 'Swab not obtained')) %>%
  mutate(TV_Remote_MRSA = ifelse(TV_Remote_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(TV_Remote_MRSA = recode_factor(.x = TV_Remote_MRSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(TV_Remote_MSSA = ifelse(TV_Remote_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(TV_Remote_MSSA = recode_factor(.x = TV_Remote_MSSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(TV_Remote_MSSP = ifelse(TV_Remote_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(TV_Remote_MSSP = recode_factor(.x = TV_Remote_MSSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(TV_Remote_MRSP = ifelse(TV_Remote_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(TV_Remote_MRSP = recode_factor(.x = TV_Remote_MRSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(TV_Remote_MSSS = ifelse(TV_Remote_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(TV_Remote_MSSS = recode_factor(.x = TV_Remote_MSSS,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(TV_Remote_MSSD = ifelse(TV_Remote_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(TV_Remote_MSSD = recode_factor(.x = TV_Remote_MSSD,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(Main_telephone_Staph_growth = recode_factor(.x = micro_env_6_growth,
                                                     '0' = 'No Growth',
                                                     '1' = 'MRSA',
                                                     '2' = 'MSSA',
                                                     '3' = 'Swab not obtained')) %>% 
  mutate(Main_telephone_PetStaph_growth = recode_factor(.x = micro_env_petstaph_6_growth,
                                                        '0' = 'No Growth',
                                                        '4' = 'MRSP',
                                                        '5' = 'MSSP',
                                                        '6' = 'MSSS',
                                                        '7' = 'MSSD',
                                                        '3' = 'Swab not obtained')) %>%
  mutate(Telephone_MRSA = ifelse(Main_telephone_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Telephone_MRSA = recode_factor(.x =Telephone_MRSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(Telephone_MSSA = ifelse(Main_telephone_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Telephone_MSSA = recode_factor(.x = Telephone_MSSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(Telephone_MSSP = ifelse(Main_telephone_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Telephone_MSSP = recode_factor(.x = Telephone_MSSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Telephone_MRSP = ifelse(Main_telephone_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Telephone_MRSP = recode_factor(.x = Telephone_MRSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Telephone_MSSS = ifelse(Main_telephone_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Telephone_MSSS = recode_factor(.x = Telephone_MSSS,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Telephone_MSSD = ifelse(Main_telephone_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Telephone_MSSD = recode_factor(.x = Telephone_MSSD,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Computer_keyboard_Staph_growth = recode_factor(.x = micro_env_7_growth,
                                                        '0' = 'No Growth',
                                                        '1' = 'MRSA',
                                                        '2' = 'MSSA',
                                                        '3' = 'Swab not obtained')) %>% 
  mutate(Computer_keyboard_PetStaph_growth = recode_factor(.x = micro_env_petstaph_7_growth,
                                                           '0' = 'No Growth',
                                                           '4' = 'MRSP',
                                                           '5' = 'MSSP',
                                                           '6' = 'MSSS',
                                                           '7' = 'MSSD',
                                                           '3' = 'Swab not obtained')) %>%
  mutate(Comp_Keyboard_MRSA = ifelse(Computer_keyboard_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Comp_Keyboard_MRSA = recode_factor(.x = Comp_Keyboard_MRSA,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Comp_Keyboard_MSSA = ifelse(Computer_keyboard_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Comp_Keyboard_MSSA = recode_factor(.x = Comp_Keyboard_MSSA,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Comp_Keyboard_MSSP = ifelse(Computer_keyboard_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Comp_Keyboard_MSSP = recode_factor(.x = Comp_Keyboard_MSSP,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Comp_Keyboard_MRSP = ifelse(Computer_keyboard_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Comp_Keyboard_MRSP = recode_factor(.x = Comp_Keyboard_MRSP,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Comp_Keyboard_MSSS = ifelse(Computer_keyboard_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Comp_Keyboard_MSSS = recode_factor(.x = Comp_Keyboard_MSSS,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Comp_Keyboard_MSSD = ifelse(Computer_keyboard_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Comp_Keyboard_MSSD = recode_factor(.x = Comp_Keyboard_MSSD,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Tablet_Staph_growth = recode_factor(.x = micro_env_8_growth,
                                             '0' = 'No Growth',
                                             '1' = 'MRSA',
                                             '2' = 'MSSA',
                                             '3' = 'Swab not obtained')) %>% 
  mutate(Tablet_PetStaph_growth = recode_factor(.x = micro_env_petstaph_8_growth,
                                                '0' = 'No Growth',
                                                '4' = 'MRSP',
                                                '5' = 'MSSP',
                                                '6' = 'MSSS',
                                                '7' = 'MSSD',
                                                '3' = 'Swab not obtained')) %>%
  mutate(Tablet_MRSA = ifelse(Tablet_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Tablet_MRSA = recode_factor(.x = Tablet_MRSA,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>%
  mutate(Tablet_MSSA = ifelse(Tablet_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Tablet_MSSA = recode_factor(.x = Tablet_MSSA,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>%
  mutate(Tablet_MSSP = ifelse(Tablet_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Tablet_MSSP = recode_factor(.x = Tablet_MSSP,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>% 
  mutate(Tablet_MRSP = ifelse(Tablet_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Tablet_MRSP = recode_factor(.x = Tablet_MRSP,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>% 
  mutate(Tablet_MSSS = ifelse(Tablet_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Tablet_MSSS = recode_factor(.x = Tablet_MSSS,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>% 
  mutate(Tablet_MSSD = ifelse(Tablet_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Tablet_MSSD = recode_factor(.x = Tablet_MSSD,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>% 
  mutate(Door_handle_out_Staph_growth = recode_factor(.x = micro_env_10_growth,
                                                      '0' = 'No Growth',
                                                      '1' = 'MRSA',
                                                      '2' = 'MSSA',
                                                      '3' = 'Swab not obtained')) %>% 
  mutate(Door_handle_out_PetStaph_growth = recode_factor(.x = micro_env_petstaph_10_growth,
                                                         '0' = 'No Growth',
                                                         '4' = 'MRSP',
                                                         '5' = 'MSSP',
                                                         '6' = 'MSSS',
                                                         '7' = 'MSSD',
                                                         '3' = 'Swab not obtained')) %>%
  mutate(Door_Out_MRSA = ifelse(Door_handle_out_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Door_Out_MRSA = recode_factor(.x = Door_Out_MRSA,
                                       '0' = 'No',
                                       '1' = 'Yes')) %>%
  mutate(Door_Out_MSSA = ifelse(Door_handle_out_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Door_Out_MSSA = recode_factor(.x = Door_Out_MSSA,
                                       '0' = 'No',
                                       '1' = 'Yes')) %>%
  mutate(Door_Out_MSSP = ifelse(Door_handle_out_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Door_Out_MSSP = recode_factor(.x = Door_Out_MSSP,
                                       '0' = 'No',
                                       '1' = 'Yes')) %>% 
  mutate(Door_Out_MRSP = ifelse(Door_handle_out_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Door_Out_MRSP = recode_factor(.x = Door_Out_MRSP,
                                       '0' = 'No',
                                       '1' = 'Yes')) %>% 
  mutate(Door_Out_MSSS = ifelse(Door_handle_out_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Door_Out_MSSS = recode_factor(.x = Door_Out_MSSS,
                                       '0' = 'No',
                                       '1' = 'Yes')) %>% 
  mutate(Door_Out_MSSD = ifelse(Door_handle_out_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Door_Out_MSSD = recode_factor(.x = Door_Out_MSSD,
                                       '0' = 'No',
                                       '1' = 'Yes')) %>% 
  mutate(Bathroom_light_Staph_growth = recode_factor(.x = micro_env_11_growth,
                                                     '0' = 'No Growth',
                                                     '1' = 'MRSA',
                                                     '2' = 'MSSA',
                                                     '3' = 'Swab not obtained')) %>% 
  mutate(Bathroom_light_PetStaph_growth = recode_factor(.x = micro_env_petstaph_11_growth,
                                                        '0' = 'No Growth',
                                                        '4' = 'MRSP',
                                                        '5' = 'MSSP',
                                                        '6' = 'MSSS',
                                                        '7' = 'MSSD',
                                                        '3' = 'Swab not obtained')) %>%
  mutate(Bathroom_light_MRSA = ifelse(Bathroom_light_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Bathroom_light_MRSA = recode_factor(.x = Bathroom_light_MRSA,
                                             '0' = 'No',
                                             '1' = 'Yes')) %>%
  mutate(Bathroom_light_MSSA = ifelse(Bathroom_light_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Bathroom_light_MSSA = recode_factor(.x = Bathroom_light_MSSA,
                                             '0' = 'No',
                                             '1' = 'Yes')) %>%
  mutate(Bathroom_light_MSSP = ifelse(Bathroom_light_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Bathroom_light_MSSP = recode_factor(.x = Bathroom_light_MSSP,
                                             '0' = 'No',
                                             '1' = 'Yes')) %>% 
  mutate(Bathroom_light_MRSP = ifelse(Bathroom_light_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Bathroom_light_MRSP = recode_factor(.x = Bathroom_light_MRSP,
                                             '0' = 'No',
                                             '1' = 'Yes')) %>% 
  mutate(Bathroom_light_MSSS = ifelse(Bathroom_light_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Bathroom_light_MSSS = recode_factor(.x = Bathroom_light_MSSS,
                                             '0' = 'No',
                                             '1' = 'Yes')) %>% 
  mutate(Bathroom_light_MSSD = ifelse(Bathroom_light_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Bathroom_light_MSSD = recode_factor(.x = Bathroom_light_MSSD,
                                             '0' = 'No',
                                             '1' = 'Yes')) %>% 
  mutate(Bathroom_sink_Staph_growth = recode_factor(.x = micro_env_12_growth,
                                                    '0' = 'No Growth',
                                                    '1' = 'MRSA',
                                                    '2' = 'MSSA',
                                                    '3' = 'Swab not obtained')) %>% 
  mutate(Bathroom_sink_PetStaph_growth = recode_factor(.x = micro_env_petstaph_12_growth,
                                                       '0' = 'No Growth',
                                                       '4' = 'MRSP',
                                                       '5' = 'MSSP',
                                                       '6' = 'MSSS',
                                                       '7' = 'MSSD',
                                                       '3' = 'Swab not obtained')) %>%
  mutate(Bathroom_sink_MRSA = ifelse(Bathroom_sink_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Bathroom_sink_MRSA = recode_factor(.x = Bathroom_sink_MRSA,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Bathroom_sink_MSSA = ifelse(Bathroom_sink_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Bathroom_sink_MSSA = recode_factor(.x = Bathroom_sink_MSSA,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Bathroom_sink_MSSP = ifelse(Bathroom_sink_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Bathroom_sink_MSSP = recode_factor(.x = Bathroom_sink_MSSP,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Bathroom_sink_MRSP = ifelse(Bathroom_sink_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Bathroom_sink_MRSP = recode_factor(.x = Bathroom_sink_MRSP,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Bathroom_sink_MSSS = ifelse(Bathroom_sink_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Bathroom_sink_MSSS = recode_factor(.x = Bathroom_sink_MSSS,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Bathroom_sink_MSSD = ifelse(Bathroom_sink_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Bathroom_sink_MSSD = recode_factor(.x = Bathroom_sink_MSSD,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Toilet_flush_Staph_growth = recode_factor(.x = micro_env_13_growth,
                                                   '0' = 'No Growth',
                                                   '1' = 'MRSA',
                                                   '2' = 'MSSA',
                                                   '3' = 'Swab not obtained')) %>% 
  mutate(Toilet_flush_PetStaph_growth = recode_factor(.x = micro_env_petstaph_13_growth,
                                                      '0' = 'No Growth',
                                                      '4' = 'MRSP',
                                                      '5' = 'MSSP',
                                                      '6' = 'MSSS',
                                                      '7' = 'MSSD',
                                                      '3' = 'Swab not obtained')) %>%
  mutate(Toilet_MRSA = ifelse(Toilet_flush_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Toilet_MRSA = recode_factor(.x = Toilet_MRSA,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>%
  mutate(Toilet_MSSA = ifelse(Toilet_flush_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Toilet_MSSA = recode_factor(.x = Toilet_MSSA,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>%
  mutate(Toilet_MSSP = ifelse(Toilet_flush_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Toilet_MSSP = recode_factor(.x = Toilet_MSSP,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>% 
  mutate(Toilet_MRSP = ifelse(Toilet_flush_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Toilet_MRSP = recode_factor(.x = Toilet_MRSP,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>%
  mutate(Toilet_MSSS = ifelse(Toilet_flush_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Toilet_MSSS = recode_factor(.x = Toilet_MSSS,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>%
  mutate(Toilet_MSSD = ifelse(Toilet_flush_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Toilet_MSSD = recode_factor(.x = Toilet_MSSD,
                                     '0' = 'No',
                                     '1' = 'Yes')) %>%
  mutate(Kitchen_light_Staph_growth = recode_factor(.x = micro_env_14_growth,
                                                    '0' = 'No Growth',
                                                    '1' = 'MRSA',
                                                    '2' = 'MSSA',
                                                    '3' = 'Swab not obtained')) %>% 
  mutate(Kitchen_light_PetStaph_growth = recode_factor(.x = micro_env_petstaph_14_growth,
                                                       '0' = 'No Growth',
                                                       '4' = 'MRSP',
                                                       '5' = 'MSSP',
                                                       '6' = 'MSSS',
                                                       '7' = 'MSSD',
                                                       '3' = 'Swab not obtained')) %>%
  mutate(Kitchen_light_MRSA = ifelse(Kitchen_light_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Kitchen_light_MRSA = recode_factor(.x = Kitchen_light_MRSA,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Kitchen_light_MSSA = ifelse(Kitchen_light_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Kitchen_light_MSSA = recode_factor(.x = Kitchen_light_MSSA,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>%
  mutate(Kitchen_light_MSSP = ifelse(Kitchen_light_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Kitchen_light_MSSP = recode_factor(.x = Kitchen_light_MSSP,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Kitchen_light_MRSP = ifelse(Kitchen_light_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Kitchen_light_MRSP = recode_factor(.x = Kitchen_light_MRSP,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Kitchen_light_MSSS = ifelse(Kitchen_light_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Kitchen_light_MSSS = recode_factor(.x = Kitchen_light_MSSS,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Kitchen_light_MSSD = ifelse(Kitchen_light_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Kitchen_light_MSSD = recode_factor(.x = Kitchen_light_MSSD,
                                            '0' = 'No',
                                            '1' = 'Yes')) %>% 
  mutate(Kitchen_sink_Staph_growth = recode_factor(.x = micro_env_15_growth,
                                                   '0' = 'No Growth',
                                                   '1' = 'MRSA',
                                                   '2' = 'MSSA',
                                                   '3' = 'Swab not obtained')) %>% 
  mutate(Kitchen_sink_PetStaph_growth = recode_factor(.x = micro_env_petstaph_15_growth,
                                                      '0' = 'No Growth',
                                                      '4' = 'MRSP',
                                                      '5' = 'MSSP',
                                                      '6' = 'MSSS',
                                                      '7' = 'MSSD',
                                                      '3' = 'Swab not obtained')) %>%
  mutate(Kitchen_sink_MRSA = ifelse(Kitchen_sink_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Kitchen_sink_MRSA = recode_factor(.x = Kitchen_sink_MRSA,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Kitchen_sink_MSSA = ifelse(Kitchen_sink_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Kitchen_sink_MSSA = recode_factor(.x = Kitchen_sink_MSSA,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Kitchen_sink_MSSP = ifelse(Kitchen_sink_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Kitchen_sink_MSSP = recode_factor(.x = Kitchen_sink_MSSP,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>% 
  mutate(Kitchen_sink_MRSP = ifelse(Kitchen_sink_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Kitchen_sink_MRSP = recode_factor(.x = Kitchen_sink_MRSP,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>% 
  mutate(Kitchen_sink_MSSS = ifelse(Kitchen_sink_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Kitchen_sink_MSSS = recode_factor(.x = Kitchen_sink_MSSS,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>% 
  mutate(Kitchen_sink_MSSD = ifelse(Kitchen_sink_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Kitchen_sink_MSSD = recode_factor(.x = Kitchen_sink_MSSD,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>% 
  mutate(Refrigerator_Staph_growth = recode_factor(.x = micro_env_16_growth,
                                                   '0' = 'No Growth',
                                                   '1' = 'MRSA',
                                                   '2' = 'MSSA',
                                                   '3' = 'Swab not obtained')) %>% 
  mutate(Refrigerator_PetStaph_growth = recode_factor(.x = micro_env_petstaph_16_growth,
                                                      '0' = 'No Growth',
                                                      '4' = 'MRSP',
                                                      '5' = 'MSSP',
                                                      '6' = 'MSSS',
                                                      '7' = 'MSSD',
                                                      '3' = 'Swab not obtained')) %>%
  mutate(Refrigerator_MRSA = ifelse(Refrigerator_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Refrigerator_MRSA = recode_factor(.x = Refrigerator_MRSA,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Refrigerator_MSSA = ifelse(Refrigerator_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Refrigerator_MSSA = recode_factor(.x = Refrigerator_MSSA,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Refrigerator_MSSP = ifelse(Refrigerator_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Refrigerator_MSSP = recode_factor(.x = Refrigerator_MSSP,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>% 
  mutate(Refrigerator_MRSP = ifelse(Refrigerator_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Refrigerator_MRSP = recode_factor(.x = Refrigerator_MRSP,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Refrigerator_MSSS = ifelse(Refrigerator_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Refrigerator_MSSS = recode_factor(.x = Refrigerator_MSSS,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Refrigerator_MSSD = ifelse(Refrigerator_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Refrigerator_MSSD = recode_factor(.x = Refrigerator_MSSD,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Microwave_door_Staph_growth = recode_factor(.x = micro_env_17_growth,
                                                     '0' = 'No Growth',
                                                     '1' = 'MRSA',
                                                     '2' = 'MSSA',
                                                     '3' = 'Swab not obtained')) %>% 
  mutate(Microwave_door_PetStaph_growth = recode_factor(.x = micro_env_petstaph_17_growth,
                                                        '0' = 'No Growth',
                                                        '4' = 'MRSP',
                                                        '5' = 'MSSP',
                                                        '6' = 'MSSS',
                                                        '7' = 'MSSD',
                                                        '3' = 'Swab not obtained')) %>%
  mutate(Microwave_MRSA = ifelse(Microwave_door_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Microwave_MRSA = recode_factor(.x = Microwave_MRSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(Microwave_MSSA = ifelse(Microwave_door_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Microwave_MSSA = recode_factor(.x = Microwave_MSSA,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(Microwave_MSSP = ifelse(Microwave_door_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Microwave_MSSP = recode_factor(.x = Microwave_MSSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Microwave_MRSP = ifelse(Microwave_door_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Microwave_MRSP = recode_factor(.x = Microwave_MRSP,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Microwave_MSSS = ifelse(Microwave_door_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Microwave_MSSS = recode_factor(.x = Microwave_MSSS,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Microwave_MSSD = ifelse(Microwave_door_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Microwave_MSSD = recode_factor(.x = Microwave_MSSD,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>% 
  mutate(Oven_door_Staph_growth = recode_factor(.x = micro_env_18_growth,
                                                '0' = 'No Growth',
                                                '1' = 'MRSA',
                                                '2' = 'MSSA',
                                                '3' = 'Swab not obtained')) %>% 
  mutate(Oven_door_PetStaph_growth = recode_factor(.x = micro_env_petstaph_18_growth,
                                                   '0' = 'No Growth',
                                                   '4' = 'MRSP',
                                                   '5' = 'MSSP',
                                                   '6' = 'MSSS',
                                                   '7' = 'MSSD',
                                                   '3' = 'Swab not obtained')) %>%
  mutate(Oven_MRSA = ifelse(Oven_door_Staph_growth == 'MRSA', 1, 0)) %>% 
  mutate(Oven_MRSA = recode_factor(.x = Oven_MRSA,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>%
  mutate(Oven_MSSA = ifelse(Oven_door_Staph_growth == 'MSSA', 1, 0)) %>% 
  mutate(Oven_MSSA = recode_factor(.x = Oven_MSSA,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>%
  mutate(Oven_MSSP_Count = ifelse(Oven_door_PetStaph_growth == 'MSSP', 1, 0)) %>% 
  mutate(Oven_MSSP_Count = as.numeric(Oven_MSSP_Count)) %>% 
  mutate(Oven_MSSP = recode_factor(.x = Oven_MSSP_Count,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>% 
  mutate(Oven_MRSP = ifelse(Oven_door_PetStaph_growth == 'MRSP', 1, 0)) %>% 
  mutate(Oven_MRSP = recode_factor(.x = Oven_MRSP,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>% 
  mutate(Oven_MSSS = ifelse(Oven_door_PetStaph_growth == 'MSSS', 1, 0)) %>% 
  mutate(Oven_MSSS = recode_factor(.x = Oven_MSSS,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>% 
  mutate(Oven_MSSD = ifelse(Oven_door_PetStaph_growth == 'MSSD', 1, 0)) %>% 
  mutate(Oven_MSSD = recode_factor(.x = Oven_MSSD,
                                   '0' = 'No',
                                   '1' = 'Yes')) %>% 
  mutate(impression_cleanliness = as.numeric(impression_cleanliness)) %>% 
  mutate(Cleanliness_impression = recode_factor(.x = impression_cleanliness,
                                                '1' = 'Above Average',
                                                '2' = 'Average',
                                                '3' = 'Below Average',
                                                '4' = 'Very Dirty')) %>% 
  mutate(SHINE_assignment = recode_factor(.x = shine_randomization_assigment,
                                          '1' = 'Personal Decolonization',
                                          '2' = 'Environmental Hygiene',
                                          '3' = 'Integrated Approach')) %>% 
  mutate(Have_Pets = as.factor(hc_pets_in_home)) %>% 
  mutate(Have_Pets = recode_factor(.x = Have_Pets, 
                                   'FALSE' = 'No',
                                   'TRUE' = 'Yes')) %>% 
  mutate(pet_age = na_if(x = pet_age, y = '777')) %>% 
  mutate(Type_of_Pet = na_if(x = Type_of_Pet, y = '55')) %>%
  mutate(Enroll_SSTI_last_12mo = na_if(x = Enroll_SSTI_last_12mo, y = '777')) %>% 
  rename('pet_caretaker_enroll' = 'pet_caretaker',
         'pet_caretaker_followup' = 'pf_pet_caretaker') %>%
  mutate(Followup_SSTI_last_12mo = na_if(x = Followup_SSTI_last_12mo, y = '777')) %>%
  mutate(NaresMRSA_NaresMSSP = ifelse(Participant_Nares_MRSA == 'Yes' & Participant_Nares_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(NaresMRSA_NaresMSSP = as.factor(NaresMRSA_NaresMSSP)) %>% 
  mutate(NaresMRSA_AxillaMSSP = ifelse(Participant_Nares_MRSA == 'Yes' & Participant_Axilla_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(NaresMRSA_AxillaMSSP = as.factor(NaresMRSA_NaresMSSP)) %>% 
  mutate(NaresMRSA_InguinalMSSP = ifelse(Participant_Nares_MRSA == 'Yes' & Participant_InguinalFold_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(NaresMRSA_InguinalMSSP = as.factor(NaresMRSA_NaresMSSP)) %>% 
  mutate(AxillaMRSA_NaresMSSP = ifelse(Participant_Axilla_MRSA == 'Yes' & Participant_Nares_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(AxillaMRSA_NaresMSSP = as.factor(AxillaMRSA_NaresMSSP)) %>% 
  mutate(AxillaMRSA_AxillaMSSP = ifelse(Participant_Axilla_MRSA == 'Yes' & Participant_Axilla_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(AxillaMRSA_AxillaMSSP = as.factor(AxillaMRSA_NaresMSSP)) %>% 
  mutate(AxillaMRSA_InguinalMSSP = ifelse(Participant_Axilla_MRSA == 'Yes' & Participant_InguinalFold_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(AxillaMRSA_InguinalMSSP = as.factor(AxillaMRSA_NaresMSSP)) %>% 
  mutate(InguinalMRSA_NaresMSSP = ifelse(Participant_InguinalFold_MRSA == 'Yes' & Participant_Nares_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(InguinalMRSA_NaresMSSP = as.factor(InguinalMRSA_NaresMSSP)) %>% 
  mutate(InguinalMRSA_AxillaMSSP = ifelse(Participant_InguinalFold_MRSA == 'Yes' & Participant_Axilla_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(InguinalMRSA_AxillaMSSP = as.factor(InguinalMRSA_NaresMSSP)) %>% 
  mutate(InguinalMRSA_InguinalMSSP = ifelse(Participant_InguinalFold_MRSA == 'Yes' & Participant_InguinalFold_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(InguinalMRSA_InguinalMSSP = as.factor(InguinalMRSA_NaresMSSP)) %>%
  mutate(NaresMSSA_NaresMSSP = ifelse(Participant_Nares_MSSA == 'Yes' & Participant_Nares_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(NaresMSSA_NaresMSSP = as.factor(NaresMSSA_NaresMSSP)) %>% 
  mutate(NaresMSSA_AxillaMSSP = ifelse(Participant_Nares_MSSA == 'Yes' & Participant_Axilla_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(NaresMSSA_AxillaMSSP = as.factor(NaresMRSA_NaresMSSP)) %>% 
  mutate(NaresMSSA_InguinalMSSP = ifelse(Participant_Nares_MSSA == 'Yes' & Participant_InguinalFold_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(NaresMSSA_InguinalMSSP = as.factor(NaresMSSA_NaresMSSP)) %>% 
  mutate(AxillaMSSA_NaresMSSP = ifelse(Participant_Axilla_MSSA == 'Yes' & Participant_Nares_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(AxillaMSSA_NaresMSSP = as.factor(AxillaMSSA_NaresMSSP)) %>% 
  mutate(AxillaMSSA_AxillaMSSP = ifelse(Participant_Axilla_MSSA == 'Yes' & Participant_Axilla_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(AxillaMSSA_AxillaMSSP = as.factor(AxillaMSSA_NaresMSSP)) %>% 
  mutate(AxillaMSSA_InguinalMSSP = ifelse(Participant_Axilla_MSSA == 'Yes' & Participant_InguinalFold_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(AxillaMSSA_InguinalMSSP = as.factor(AxillaMSSA_NaresMSSP)) %>% 
  mutate(InguinalMSSA_NaresMSSP = ifelse(Participant_InguinalFold_MSSA == 'Yes' & Participant_Nares_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(InguinalMSSA_NaresMSSP = as.factor(InguinalMSSA_NaresMSSP)) %>% 
  mutate(InguinalMSSA_AxillaMSSP = ifelse(Participant_InguinalFold_MSSA == 'Yes' & Participant_Axilla_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(InguinalMSSA_AxillaMSSP = as.factor(InguinalMSSA_NaresMSSP)) %>% 
  mutate(InguinalMSSA_InguinalMSSP = ifelse(Participant_InguinalFold_MSSA == 'Yes' & Participant_InguinalFold_MSSP == 'Yes', 'Yes', 'No')) %>% 
  mutate(InguinalMSSA_InguinalMSSP = as.factor(InguinalMSSA_NaresMSSP)) %>%
  mutate(Clean_Sofa_Never = ifelse(hc_living_area_sofa == '0', 1, 0)) %>% 
  mutate(Clean_Sofa_Never = recode_factor(.x = Clean_Sofa_Never,
                                                 '0' = 'No',
                                                 '1' = 'Yes')) %>%
  mutate(Clean_Sofa_0_1 = ifelse(hc_living_area_sofa == '1', 1, 0)) %>% 
  mutate(Clean_Sofa_0_1 = recode_factor(.x = Clean_Sofa_0_1,
                                          '0' = 'No',
                                          '1' = 'Yes')) %>%
  mutate(Clean_Sofa_1_3 = ifelse(hc_living_area_sofa == '2', 1, 0)) %>% 
  mutate(Clean_Sofa_1_3 = recode_factor(.x = Clean_Sofa_1_3,
                                          '0' = 'No',
                                          '1' = 'Yes')) %>%
  mutate(Clean_Sofa_4 = ifelse(hc_living_area_sofa == '3', 1, 0)) %>% 
  mutate(Clean_Sofa_4 = recode_factor(.x = Clean_Sofa_4,
                                          '0' = 'No',
                                          '1' = 'Yes')) %>%
  mutate(Clean_Sofa_higherthan4 = ifelse(hc_living_area_sofa == '4', 1, 0)) %>% 
  mutate(Clean_Sofa_higherthan4 = recode_factor(.x = Clean_Sofa_higherthan4,
                                          '0' = 'No',
                                          '1' = 'Yes')) %>%
  mutate(Clean_Sofa_Unsure = ifelse(hc_living_area_sofa == '777', 1, 0)) %>% 
  mutate(Clean_Sofa_Unsure = recode_factor(.x = Clean_Sofa_Unsure,
                                          '0' = 'No',
                                          '1' = 'Yes')) %>%
  mutate(Bath_shower_less1perweek = ifelse(ei_take_bath_shower == '0', 1, 0)) %>% 
  mutate(Bath_shower_less1perweek = recode_factor(.x = Bath_shower_less1perweek,
                                          '0' = 'No',
                                          '1' = 'Yes')) %>%
  mutate(Bath_shower_1_2perweek = ifelse(ei_take_bath_shower == '1', 1, 0)) %>% 
  mutate(Bath_shower_1_2perweek = recode_factor(.x = Bath_shower_1_2perweek,
                                                  '0' = 'No',
                                                  '1' = 'Yes')) %>%
  mutate(Bath_shower_3_4perweek = ifelse(ei_take_bath_shower == '2', 1, 0)) %>% 
  mutate(Bath_shower_3_4perweek = recode_factor(.x = Bath_shower_3_4perweek,
                                                  '0' = 'No',
                                                  '1' = 'Yes')) %>%
  mutate(Bath_shower_5_6perweek = ifelse(ei_take_bath_shower == '3', 1, 0)) %>% 
  mutate(Bath_shower_5_6perweek = recode_factor(.x = Bath_shower_5_6perweek,
                                                  '0' = 'No',
                                                  '1' = 'Yes')) %>%
  mutate(Bath_shower_1perday = ifelse(ei_take_bath_shower == '4', 1, 0)) %>% 
  mutate(Bath_shower_1perday = recode_factor(.x = Bath_shower_1perday,
                                                  '0' = 'No',
                                                  '1' = 'Yes')) %>%
  mutate(Bath_shower_2ormoreperday = ifelse(ei_take_bath_shower == '5', 1, 0)) %>% 
  mutate(Bath_shower_2ormoreperday = recode_factor(.x = Bath_shower_2ormoreperday,
                                                  '0' = 'No',
                                                  '1' = 'Yes')) %>%
  mutate(Clean_Sofa_Never_Follow = ifelse(hf_living_area_sofa == '0', 1, 0)) %>% 
  mutate(Clean_Sofa_Never_Follow = recode_factor(.x = Clean_Sofa_Never_Follow,
                                          '0' = 'No',
                                          '1' = 'Yes')) %>%
  mutate(Clean_Sofa_0_1_Follow = ifelse(hf_living_area_sofa == '1', 1, 0)) %>% 
  mutate(Clean_Sofa_0_1_Follow = recode_factor(.x = Clean_Sofa_0_1_Follow,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(Clean_Sofa_1_3_Follow = ifelse(hf_living_area_sofa == '2', 1, 0)) %>% 
  mutate(Clean_Sofa_1_3_Follow = recode_factor(.x = Clean_Sofa_1_3_Follow,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(Clean_Sofa_4_Follow = ifelse(hf_living_area_sofa == '3', 1, 0)) %>% 
  mutate(Clean_Sofa_4_Follow = recode_factor(.x = Clean_Sofa_4_Follow,
                                      '0' = 'No',
                                      '1' = 'Yes')) %>%
  mutate(Clean_Sofa_higherthan4_Follow = ifelse(hf_living_area_sofa == '4', 1, 0)) %>% 
  mutate(Clean_Sofa_higherthan4_Follow = recode_factor(.x = Clean_Sofa_higherthan4_Follow,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>%
  mutate(Clean_Sofa_Unsure_Follow = ifelse(hf_living_area_sofa == '777', 1, 0)) %>% 
  mutate(Clean_Sofa_Unsure_Follow = recode_factor(.x = Clean_Sofa_Unsure_Follow,
                                           '0' = 'No',
                                           '1' = 'Yes')) %>%
  mutate(Bath_shower_less1perweek_Follow = ifelse(pf_shower_bath_how_often == '0', 1, 0)) %>% 
  mutate(Bath_shower_less1perweek_Follow = recode_factor(.x = Bath_shower_less1perweek_Follow,
                                                  '0' = 'No',
                                                  '1' = 'Yes')) %>%
  mutate(Bath_shower_1_2perweek_Follow = ifelse(pf_shower_bath_how_often == '1', 1, 0)) %>% 
  mutate(Bath_shower_1_2perweek_Follow = recode_factor(.x = Bath_shower_1_2perweek_Follow,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>%
  mutate(Bath_shower_3_4perweek_Follow = ifelse(pf_shower_bath_how_often == '2', 1, 0)) %>% 
  mutate(Bath_shower_3_4perweek_Follow = recode_factor(.x = Bath_shower_3_4perweek_Follow,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>%
  mutate(Bath_shower_5_6perweek_Follow = ifelse(pf_shower_bath_how_often == '3', 1, 0)) %>% 
  mutate(Bath_shower_5_6perweek_Follow = recode_factor(.x = Bath_shower_5_6perweek_Follow,
                                                '0' = 'No',
                                                '1' = 'Yes')) %>%
  mutate(Bath_shower_1perday_Follow = ifelse(pf_shower_bath_how_often == '4', 1, 0)) %>% 
  mutate(Bath_shower_1perday_Follow = recode_factor(.x = Bath_shower_1perday_Follow,
                                             '0' = 'No',
                                             '1' = 'Yes')) %>%
  mutate(Bath_shower_2ormoreperday_Follow = ifelse(pf_shower_bath_how_often == '5', 1, 0)) %>% 
  mutate(Bath_shower_2ormoreperday_Follow = recode_factor(.x = Bath_shower_2ormoreperday_Follow,
                                                   '0' = 'No',
                                                   '1' = 'Yes')) %>%
  mutate(attend_daycare = recode_factor(.x = ei_attend_daycare,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(attend_daycare_follow = recode_factor(.x = pf_attend_daycare,
                                        '0' = 'No',
                                        '1' = 'Yes')) %>%
  mutate(grade_school = recode_factor(.x = ei_grade_school,
                                      '0' = 'No',
                                      '1' = 'Yes')) %>% 
  select(record_id, 
         hc_household_id,
         pet_caretaker_enroll,
         pet_caretaker_followup,
         sex,
         race,
         race_white,
         race_black,
         race_asian,
         race_biracial,
         SHINE_assignment,
         Have_Pets,
         Type_of_Pet,
         visit,
         Cleanliness_impression,
         Clean_Sofa_Never,
         Clean_Sofa_Never_Follow,
         Clean_Sofa_0_1,
         Clean_Sofa_0_1_Follow,
         Clean_Sofa_1_3,
         Clean_Sofa_1_3_Follow,
         Clean_Sofa_4,
         Clean_Sofa_4_Follow,
         Clean_Sofa_higherthan4,
         Clean_Sofa_higherthan4_Follow,
         Clean_Sofa_Unsure,
         Clean_Sofa_Unsure_Follow,
         Bath_shower_less1perweek,
         Bath_shower_less1perweek_Follow,
         Bath_shower_1_2perweek,
         Bath_shower_1_2perweek_Follow,
         Bath_shower_3_4perweek,
         Bath_shower_3_4perweek_Follow,
         Bath_shower_5_6perweek,
         Bath_shower_5_6perweek_Follow,
         Bath_shower_1perday,
         Bath_shower_1perday_Follow,
         Bath_shower_2ormoreperday,
         Bath_shower_2ormoreperday_Follow,
         attend_daycare,
         attend_daycare_follow,
         grade_school,
         Participant_Staph_Axilla,
         Participant_Staph_Nares,
         Participant_Staph_InguinalFold,
         Participant_PetStaph_Axilla,
         Participant_PetStaph_Nares,
         Participant_PetStaph_InguinalFold,
         Pet_Staph_Dorsal_Fur,
         Pet_Staph_Mouth,
         Pet_PetStaph_Dorsal_Fur,
         Pet_PetStaph_Mouth,
         Enroll_SSTI_last_12mo,
         Followup_SSTI_last_12mo,
         Pet_Hair,
         pet_age,
         Pet_Surgery,
         Pet_skin_condition,
         Pet_In_or_Out,
         Pet_Appetite,
         Pet_Energy,
         Pet_in_Daycare_at_Enroll,
         Pet_in_Daycare_followup,
         Pet_take_Antibiotics_6mo_enroll,
         Sofa_PetStaph_growth,
         TV_Remote_PetStaph_growth,
         Main_telephone_PetStaph_growth,
         Computer_keyboard_PetStaph_growth,
         Tablet_PetStaph_growth,
         Door_handle_out_PetStaph_growth,
         Bathroom_light_PetStaph_growth,
         Bathroom_sink_PetStaph_growth,
         Toilet_flush_PetStaph_growth,
         Kitchen_light_PetStaph_growth,
         Kitchen_sink_PetStaph_growth,
         Refrigerator_PetStaph_growth,
         Microwave_door_PetStaph_growth,
         Oven_door_PetStaph_growth,
         Cleanliness_impression,
         Participant_Axilla_MRSA,
         Participant_Nares_MRSA,
         Participant_InguinalFold_MRSA,
         Participant_Axilla_MSSA,
         Participant_Nares_MSSA,
         Participant_InguinalFold_MSSA,
         Dorsal_Fur_MRSA,
         Dorsal_Fur_MSSA,
         Pet_Mouth_MRSA,
         Pet_Mouth_MSSA,
         Participant_Axilla_MSSP,
         Participant_Nares_MSSP,
         Participant_InguinalFold_MSSP,
         Dorsal_Fur_MSSP,
         Pet_Mouth_MSSP,
         TV_Remote_MRSA,
         Sofa_MRSA,
         Telephone_MRSA,
         Comp_Keyboard_MRSA,
         Tablet_MRSA,
         Door_Out_MRSA,
         Bathroom_light_MRSA,
         Bathroom_sink_MRSA,
         Toilet_MRSA,
         Kitchen_light_MRSA,
         Kitchen_sink_MRSA,
         Refrigerator_MRSA,
         Microwave_MRSA,
         Oven_MRSA,
         TV_Remote_MSSA,
         Sofa_MSSA,
         Telephone_MSSA,
         Comp_Keyboard_MSSA,
         Tablet_MSSA,
         Door_Out_MSSA,
         Bathroom_light_MSSA,
         Bathroom_sink_MSSA,
         Toilet_MSSA,
         Kitchen_light_MSSA,
         Kitchen_sink_MSSA,
         Refrigerator_MSSA,
         Microwave_MSSA,
         Oven_MSSA,
         TV_Remote_MSSP,
         Sofa_MSSP,
         Telephone_MSSP,
         Comp_Keyboard_MSSP,
         Tablet_MSSP,
         Door_Out_MSSP,
         Bathroom_light_MSSP,
         Bathroom_sink_MSSP,
         Toilet_MSSP,
         Kitchen_light_MSSP,
         Kitchen_sink_MSSP,
         Refrigerator_MSSP,
         Microwave_MSSP,
         Oven_MSSP,
         Participant_Axilla_MRSP,
         Participant_Nares_MRSP,
         Participant_InguinalFold_MRSP,
         Dorsal_Fur_MRSP,
         Pet_Mouth_MRSP,
         TV_Remote_MRSP,
         Sofa_MRSP,
         Telephone_MRSP,
         Comp_Keyboard_MRSP,
         Tablet_MRSP,
         Door_Out_MRSP,
         Bathroom_light_MRSP,
         Bathroom_sink_MRSP,
         Toilet_MRSP,
         Kitchen_light_MRSP,
         Kitchen_sink_MRSP,
         Refrigerator_MRSP,
         Microwave_MRSP,
         Oven_MRSP,
         Participant_Axilla_MSSS,
         Participant_Nares_MSSS,
         Participant_InguinalFold_MSSS,
         Dorsal_Fur_MSSS,
         Pet_Mouth_MSSS,
         TV_Remote_MSSS,
         Sofa_MSSS,
         Telephone_MSSS,
         Comp_Keyboard_MSSS,
         Tablet_MSSS,
         Door_Out_MSSS,
         Bathroom_light_MSSS,
         Bathroom_sink_MSSS,
         Toilet_MSSS,
         Kitchen_light_MSSS,
         Kitchen_sink_MSSS,
         Refrigerator_MSSS,
         Microwave_MSSS,
         Oven_MSSS,
         Participant_Axilla_MSSD,
         Participant_Nares_MSSD,
         Participant_InguinalFold_MSSD,
         Dorsal_Fur_MSSD,
         Pet_Mouth_MSSD,
         TV_Remote_MSSD,
         Sofa_MSSD,
         Telephone_MSSD,
         Comp_Keyboard_MSSD,
         Tablet_MSSD,
         Door_Out_MSSD,
         Bathroom_light_MSSD,
         Bathroom_sink_MSSD,
         Toilet_MSSD,
         Kitchen_light_MSSD,
         Kitchen_sink_MSSD,
         Refrigerator_MSSD,
         Microwave_MSSD,
         Oven_MSSD,
         NaresMRSA_NaresMSSP,
         NaresMRSA_AxillaMSSP,
         NaresMRSA_InguinalMSSP,
         AxillaMRSA_NaresMSSP,
         AxillaMRSA_AxillaMSSP,
         AxillaMRSA_InguinalMSSP,
         InguinalMRSA_NaresMSSP,
         InguinalMRSA_NaresMSSP,
         InguinalMRSA_InguinalMSSP,
         NaresMSSA_NaresMSSP,
         NaresMSSA_AxillaMSSP,
         NaresMSSA_InguinalMSSP,
         AxillaMSSA_NaresMSSP,
         AxillaMSSA_AxillaMSSP,
         AxillaMSSA_InguinalMSSP,
         InguinalMSSA_NaresMSSP,
         InguinalMSSA_AxillaMSSP,
         InguinalMSSA_InguinalMSSP) 

typeof(ada.shine.clean$Have_Pets)
#making wide table----
ada.shine.clean.wide <- ada.shine.clean %>% 
  select(record_id,
         hc_household_id,
         sex,
         race,
         race_white,
         race_black,
         race_asian,
         race_biracial,
         Cleanliness_impression,
         Clean_Sofa_Never,
         Clean_Sofa_Never_Follow,
         Clean_Sofa_0_1,
         Clean_Sofa_0_1_Follow,
         Clean_Sofa_1_3,
         Clean_Sofa_1_3_Follow,
         Clean_Sofa_4,
         Clean_Sofa_4_Follow,
         Clean_Sofa_higherthan4,
         Clean_Sofa_higherthan4_Follow,
         Clean_Sofa_Unsure,
         Clean_Sofa_Unsure_Follow,
         Bath_shower_less1perweek,
         Bath_shower_less1perweek_Follow,
         Bath_shower_1_2perweek,
         Bath_shower_1_2perweek_Follow,
         Bath_shower_3_4perweek,
         Bath_shower_3_4perweek_Follow,
         Bath_shower_5_6perweek,
         Bath_shower_5_6perweek_Follow,
         Bath_shower_1perday,
         Bath_shower_1perday_Follow,
         Bath_shower_2ormoreperday,
         Bath_shower_2ormoreperday_Follow,
         attend_daycare,
         attend_daycare_follow,
         grade_school,
         Have_Pets,
         pet_caretaker_enroll,
         pet_caretaker_followup,
         Type_of_Pet,
         pet_age,
         Pet_skin_condition,
         Pet_Surgery,
         Pet_Hair,
         Pet_Appetite,
         Pet_Energy,
         Pet_In_or_Out,
         Pet_in_Daycare_at_Enroll,
         Pet_in_Daycare_followup,
         Pet_take_Antibiotics_6mo_enroll,
         SHINE_assignment,
         visit,
         Enroll_SSTI_last_12mo,
         Followup_SSTI_last_12mo,
         Participant_Staph_Axilla,
         Participant_Staph_Nares,
         Participant_Staph_InguinalFold,
         Participant_PetStaph_Axilla,
         Participant_PetStaph_Nares,
         Participant_PetStaph_InguinalFold,
         Pet_Staph_Dorsal_Fur,
         Pet_Staph_Mouth,
         Pet_PetStaph_Dorsal_Fur,
         Pet_PetStaph_Mouth,
         Sofa_PetStaph_growth,
         TV_Remote_MSSP,
         TV_Remote_PetStaph_growth,
         Main_telephone_PetStaph_growth,
         Computer_keyboard_PetStaph_growth,
         Tablet_PetStaph_growth,
         Door_handle_out_PetStaph_growth,
         Bathroom_light_PetStaph_growth,
         Bathroom_sink_PetStaph_growth,
         Toilet_flush_PetStaph_growth,
         Kitchen_light_PetStaph_growth,
         Kitchen_sink_PetStaph_growth,
         Refrigerator_PetStaph_growth,
         Microwave_door_PetStaph_growth,
         Oven_door_PetStaph_growth,
         Participant_Axilla_MRSA,
         Participant_Nares_MRSA,
         Participant_InguinalFold_MRSA,
         Participant_Axilla_MSSA,
         Participant_Nares_MSSA,
         Participant_InguinalFold_MSSA,
         Dorsal_Fur_MRSA,
         Dorsal_Fur_MSSA,
         Pet_Mouth_MRSA,
         Pet_Mouth_MSSA,
         Participant_Axilla_MSSP,
         Participant_Nares_MSSP,
         Participant_InguinalFold_MSSP,
         Dorsal_Fur_MSSP,
         Pet_Mouth_MSSP,
         TV_Remote_MRSA,
         Sofa_MRSA,
         Telephone_MRSA,
         Comp_Keyboard_MRSA,
         Tablet_MRSA,
         Door_Out_MRSA,
         Bathroom_light_MRSA,
         Bathroom_sink_MRSA,
         Toilet_MRSA,
         Kitchen_light_MRSA,
         Kitchen_sink_MRSA,
         Refrigerator_MRSA,
         Microwave_MRSA,
         Oven_MRSA,
         TV_Remote_MSSA,
         Sofa_MSSA,
         Telephone_MSSA,
         Comp_Keyboard_MSSA,
         Tablet_MSSA,
         Door_Out_MSSA,
         Bathroom_light_MSSA,
         Bathroom_sink_MSSA,
         Toilet_MSSA,
         Kitchen_light_MSSA,
         Kitchen_sink_MSSA,
         Refrigerator_MSSA,
         Microwave_MSSA,
         Oven_MSSA,
         TV_Remote_MSSP,
         Sofa_MSSP,
         Telephone_MSSP,
         Comp_Keyboard_MSSP,
         Tablet_MSSP,
         Door_Out_MSSP,
         Bathroom_light_MSSP,
         Bathroom_sink_MSSP,
         Toilet_MSSP,
         Kitchen_light_MSSP,
         Kitchen_sink_MSSP,
         Refrigerator_MSSP,
         Microwave_MSSP,
         Oven_MSSP,
         Participant_Axilla_MRSP,
         Participant_Nares_MRSP,
         Participant_InguinalFold_MRSP,
         Dorsal_Fur_MRSP,
         Pet_Mouth_MRSP,
         TV_Remote_MRSP,
         Sofa_MRSP,
         Telephone_MRSP,
         Comp_Keyboard_MRSP,
         Tablet_MRSP,
         Door_Out_MRSP,
         Bathroom_light_MRSP,
         Bathroom_sink_MRSP,
         Toilet_MRSP,
         Kitchen_light_MRSP,
         Kitchen_sink_MRSP,
         Refrigerator_MRSP,
         Microwave_MRSP,
         Oven_MRSP,
         Participant_Axilla_MSSS,
         Participant_Nares_MSSS,
         Participant_InguinalFold_MSSS,
         Dorsal_Fur_MSSS,
         Pet_Mouth_MSSS,
         TV_Remote_MSSS,
         Sofa_MSSS,
         Telephone_MSSS,
         Comp_Keyboard_MSSS,
         Tablet_MSSS,
         Door_Out_MSSS,
         Bathroom_light_MSSS,
         Bathroom_sink_MSSS,
         Toilet_MSSS,
         Kitchen_light_MSSS,
         Kitchen_sink_MSSS,
         Refrigerator_MSSS,
         Microwave_MSSS,
         Oven_MSSS,
         Participant_Axilla_MSSD,
         Participant_Nares_MSSD,
         Participant_InguinalFold_MSSD,
         Dorsal_Fur_MSSD,
         Pet_Mouth_MSSD,
         TV_Remote_MSSD,
         Sofa_MSSD,
         Telephone_MSSD,
         Comp_Keyboard_MSSD,
         Tablet_MSSD,
         Door_Out_MSSD,
         Bathroom_light_MSSD,
         Bathroom_sink_MSSD,
         Toilet_MSSD,
         Kitchen_light_MSSD,
         Kitchen_sink_MSSD,
         Refrigerator_MSSD,
         Microwave_MSSD,
         Oven_MSSD,
         NaresMRSA_NaresMSSP,
         NaresMRSA_AxillaMSSP,
         NaresMRSA_InguinalMSSP,
         AxillaMRSA_NaresMSSP,
         AxillaMRSA_AxillaMSSP,
         AxillaMRSA_InguinalMSSP,
         InguinalMRSA_NaresMSSP,
         InguinalMRSA_NaresMSSP,
         InguinalMRSA_InguinalMSSP,
         NaresMSSA_NaresMSSP,
         NaresMSSA_AxillaMSSP,
         NaresMSSA_InguinalMSSP,
         AxillaMSSA_NaresMSSP,
         AxillaMSSA_AxillaMSSP,
         AxillaMSSA_InguinalMSSP,
         InguinalMSSA_NaresMSSP,
         InguinalMSSA_AxillaMSSP,
         InguinalMSSA_InguinalMSSP) %>% 
  setDT(ada.shine.clean) %>% 
  dcast(record_id ~ visit, value.var = c('hc_household_id',
                                         'sex',
                                         'race',
                                         'race_white',
                                         'race_black',
                                         'race_asian',
                                         'race_biracial',
                                         'SHINE_assignment',
                                         'Cleanliness_impression',
                                         'Clean_Sofa_Never',
                                         'Clean_Sofa_Never_Follow',
                                         'Clean_Sofa_0_1',
                                         'Clean_Sofa_0_1_Follow',
                                         'Clean_Sofa_1_3',
                                         'Clean_Sofa_1_3_Follow',
                                         'Clean_Sofa_4',
                                         'Clean_Sofa_4_Follow',
                                         'Clean_Sofa_higherthan4',
                                         'Clean_Sofa_higherthan4_Follow',
                                         'Clean_Sofa_Unsure',
                                         'Clean_Sofa_Unsure_Follow',
                                         'Bath_shower_less1perweek',
                                         'Bath_shower_less1perweek_Follow',
                                         'Bath_shower_1_2perweek',
                                         'Bath_shower_1_2perweek_Follow',
                                         'Bath_shower_3_4perweek',
                                         'Bath_shower_3_4perweek_Follow',
                                         'Bath_shower_5_6perweek',
                                         'Bath_shower_5_6perweek_Follow',
                                         'Bath_shower_1perday',
                                         'Bath_shower_1perday_Follow',
                                         'Bath_shower_2ormoreperday',
                                         'Bath_shower_2ormoreperday_Follow',
                                         'attend_daycare',
                                         'attend_daycare_follow',
                                         'grade_school',
                                         'Have_Pets',
                                         'Type_of_Pet',
                                         'pet_age',
                                         'Pet_Surgery',
                                         'Pet_skin_condition',
                                         'Pet_Hair',
                                         'Pet_In_or_Out',
                                         'Pet_Appetite',
                                         'Pet_Energy',
                                         'Pet_in_Daycare_at_Enroll',
                                         'Pet_in_Daycare_followup',
                                         'Pet_take_Antibiotics_6mo_enroll',
                                         'pet_caretaker_enroll',
                                         'pet_caretaker_followup',
                                         'Enroll_SSTI_last_12mo',
                                         'Followup_SSTI_last_12mo',
                                         'Participant_Staph_Axilla',
                                         'Participant_Staph_Nares',
                                         'Participant_Staph_InguinalFold',
                                         'Participant_PetStaph_Axilla',
                                         'Participant_PetStaph_Nares',
                                         'Participant_PetStaph_InguinalFold',
                                         'Pet_Staph_Dorsal_Fur',
                                         'Pet_Staph_Mouth',
                                         'Pet_PetStaph_Dorsal_Fur',
                                         'Pet_PetStaph_Mouth',
                                         'Sofa_PetStaph_growth',
                                         'TV_Remote_MSSP',
                                         'TV_Remote_PetStaph_growth',
                                         'Main_telephone_PetStaph_growth',
                                         'Computer_keyboard_PetStaph_growth',
                                         'Tablet_PetStaph_growth',
                                         'Door_handle_out_PetStaph_growth',
                                         'Bathroom_light_PetStaph_growth',
                                         'Bathroom_sink_PetStaph_growth',
                                         'Toilet_flush_PetStaph_growth',
                                         'Kitchen_light_PetStaph_growth',
                                         'Kitchen_sink_PetStaph_growth',
                                         'Refrigerator_PetStaph_growth',
                                         'Microwave_door_PetStaph_growth',
                                         'Participant_Axilla_MRSA',
                                         'Participant_Nares_MRSA',
                                         'Participant_InguinalFold_MRSA',
                                         'Participant_Axilla_MSSA',
                                         'Participant_Nares_MSSA',
                                         'Participant_InguinalFold_MSSA',
                                         'Dorsal_Fur_MRSA',
                                         'Dorsal_Fur_MSSA',
                                         'Pet_Mouth_MRSA',
                                         'Pet_Mouth_MSSA',
                                         'Participant_Axilla_MSSP',
                                         'Participant_Nares_MSSP',
                                         'Participant_InguinalFold_MSSP',
                                         'Dorsal_Fur_MSSP',
                                         'Pet_Mouth_MSSP',
                                         'Oven_door_PetStaph_growth',
                                         'TV_Remote_MRSA',
                                         'Sofa_MRSA',
                                         'Telephone_MRSA',
                                         'Comp_Keyboard_MRSA',
                                         'Tablet_MRSA',
                                         'Door_Out_MRSA',
                                         'Bathroom_light_MRSA',
                                         'Bathroom_sink_MRSA',
                                         'Toilet_MRSA',
                                         'Kitchen_light_MRSA',
                                         'Kitchen_sink_MRSA',
                                         'Refrigerator_MRSA',
                                         'Microwave_MRSA',
                                         'Oven_MRSA',
                                         'TV_Remote_MSSA',
                                         'Sofa_MSSA',
                                         'Telephone_MSSA',
                                         'Comp_Keyboard_MSSA',
                                         'Tablet_MSSA',
                                         'Door_Out_MSSA',
                                         'Bathroom_light_MSSA',
                                         'Bathroom_sink_MSSA',
                                         'Toilet_MSSA',
                                         'Kitchen_light_MSSA',
                                         'Kitchen_sink_MSSA',
                                         'Refrigerator_MSSA',
                                         'Microwave_MSSA',
                                         'Oven_MSSA',
                                         'TV_Remote_MSSP',
                                         'Sofa_MSSP',
                                         'Telephone_MSSP',
                                         'Comp_Keyboard_MSSP',
                                         'Tablet_MSSP',
                                         'Door_Out_MSSP',
                                         'Bathroom_light_MSSP',
                                         'Bathroom_sink_MSSP',
                                         'Toilet_MSSP',
                                         'Kitchen_light_MSSP',
                                         'Kitchen_sink_MSSP',
                                         'Refrigerator_MSSP',
                                         'Microwave_MSSP',
                                         'Oven_MSSP',
                                         'Participant_Axilla_MRSP',
                                         'Participant_Nares_MRSP',
                                         'Participant_InguinalFold_MRSP',
                                         'Dorsal_Fur_MRSP',
                                         'Pet_Mouth_MRSP',
                                         'TV_Remote_MRSP',
                                         'Sofa_MRSP',
                                         'Telephone_MRSP',
                                         'Comp_Keyboard_MRSP',
                                         'Tablet_MRSP',
                                         'Door_Out_MRSP',
                                         'Bathroom_light_MRSP',
                                         'Bathroom_sink_MRSP',
                                         'Toilet_MRSP',
                                         'Kitchen_light_MRSP',
                                         'Kitchen_sink_MRSP',
                                         'Refrigerator_MRSP',
                                         'Microwave_MRSP',
                                         'Oven_MRSP',
                                         'Participant_Axilla_MSSS',
                                         'Participant_Nares_MSSS',
                                         'Participant_InguinalFold_MSSS',
                                         'Dorsal_Fur_MSSS',
                                         'Pet_Mouth_MSSS',
                                         'TV_Remote_MSSS',
                                         'Sofa_MSSS',
                                         'Telephone_MSSS',
                                         'Comp_Keyboard_MSSS',
                                         'Tablet_MSSS',
                                         'Door_Out_MSSS',
                                         'Bathroom_light_MSSS',
                                         'Bathroom_sink_MSSS',
                                         'Toilet_MSSS',
                                         'Kitchen_light_MSSS',
                                         'Kitchen_sink_MSSS',
                                         'Refrigerator_MSSS',
                                         'Microwave_MSSS',
                                         'Oven_MSSS',
                                         'Participant_Axilla_MSSD',
                                         'Participant_Nares_MSSD',
                                         'Participant_InguinalFold_MSSD',
                                         'Dorsal_Fur_MSSD',
                                         'Pet_Mouth_MSSD',
                                         'TV_Remote_MSSD',
                                         'Sofa_MSSD',
                                         'Telephone_MSSD',
                                         'Comp_Keyboard_MSSD',
                                         'Tablet_MSSD',
                                         'Door_Out_MSSD',
                                         'Bathroom_light_MSSD',
                                         'Bathroom_sink_MSSD',
                                         'Toilet_MSSD',
                                         'Kitchen_light_MSSD',
                                         'Kitchen_sink_MSSD',
                                         'Refrigerator_MSSD',
                                         'Microwave_MSSD',
                                         'Oven_MSSD',
                                         'NaresMRSA_NaresMSSP',
                                         'NaresMRSA_AxillaMSSP',
                                         'NaresMRSA_InguinalMSSP',
                                         'AxillaMRSA_NaresMSSP',
                                         'AxillaMRSA_AxillaMSSP',
                                         'AxillaMRSA_InguinalMSSP',
                                         'InguinalMRSA_NaresMSSP',
                                         'InguinalMRSA_NaresMSSP',
                                         'InguinalMRSA_InguinalMSSP',
                                         'NaresMSSA_NaresMSSP',
                                         'NaresMSSA_AxillaMSSP',
                                         'NaresMSSA_InguinalMSSP',
                                         'AxillaMSSA_NaresMSSP',
                                         'AxillaMSSA_AxillaMSSP',
                                         'AxillaMSSA_InguinalMSSP',
                                         'InguinalMSSA_NaresMSSP',
                                         'InguinalMSSA_AxillaMSSP',
                                         'InguinalMSSA_InguinalMSSP')) 




summary(pet.shine.clean.wide)




wide.ada.clean <- ada.shine.clean.wide %>% 
  select(-hc_household_id_1month,
         -hc_household_id_3month,
         -hc_household_id_6month,
         -hc_household_id_9month,
         -Have_Pets_1month,
         -Have_Pets_3month,
         -Have_Pets_6month,
         -Have_Pets_9month,
         -Cleanliness_impression_1month,
         -Cleanliness_impression_3month,
         -Cleanliness_impression_6month,
         -Cleanliness_impression_9month,
         -Clean_Sofa_Never_1month,
         -Clean_Sofa_Never_3month,
         -Clean_Sofa_Never_6month,
         -Clean_Sofa_Never_9month,
         -Clean_Sofa_Never_Follow_enroll,
         -Clean_Sofa_0_1_1month,
         -Clean_Sofa_0_1_3month,
         -Clean_Sofa_0_1_6month,
         -Clean_Sofa_0_1_9month,
         -Clean_Sofa_1_3_1month,
         -Clean_Sofa_1_3_3month,
         -Clean_Sofa_1_3_6month,
         -Clean_Sofa_1_3_9month,
         -Clean_Sofa_4_1month,
         -Clean_Sofa_4_3month,
         -Clean_Sofa_4_6month,
         -Clean_Sofa_4_9month,
         -Clean_Sofa_higherthan4_1month,
         -Clean_Sofa_higherthan4_3month,
         -Clean_Sofa_higherthan4_6month,
         -Clean_Sofa_higherthan4_9month,
         -Clean_Sofa_Unsure_1month,
         -Clean_Sofa_Unsure_3month,
         -Clean_Sofa_Unsure_6month,
         -Clean_Sofa_Unsure_9month,
         -Clean_Sofa_0_1_Follow_enroll,
         -Clean_Sofa_1_3_Follow_enroll,
         -Clean_Sofa_4_Follow_enroll,
         -Clean_Sofa_higherthan4_Follow_enroll,
         -Clean_Sofa_Unsure_Follow_enroll,
         -Bath_shower_less1perweek_1month,
         -Bath_shower_less1perweek_3month,
         -Bath_shower_less1perweek_6month,
         -Bath_shower_less1perweek_9month,
         -Bath_shower_less1perweek_Follow_enroll,
         -Bath_shower_1_2perweek_1month,
         -Bath_shower_1_2perweek_3month,
         -Bath_shower_1_2perweek_6month,
         -Bath_shower_1_2perweek_9month,
         -Bath_shower_1_2perweek_Follow_enroll,
         -Bath_shower_3_4perweek_1month,
         -Bath_shower_3_4perweek_3month,
         -Bath_shower_3_4perweek_6month,
         -Bath_shower_3_4perweek_9month,
         -Bath_shower_3_4perweek_Follow_enroll,
         -Bath_shower_5_6perweek_1month,
         -Bath_shower_5_6perweek_3month,
         -Bath_shower_5_6perweek_6month,
         -Bath_shower_5_6perweek_9month,
         -Bath_shower_1perday_1month,
         -Bath_shower_1perday_3month,
         -Bath_shower_1perday_6month,
         -Bath_shower_1perday_9month,
         -Bath_shower_1perday_Follow_enroll,
         -Bath_shower_2ormoreperday_1month,
         -Bath_shower_2ormoreperday_3month,
         -Bath_shower_2ormoreperday_6month,
         -Bath_shower_2ormoreperday_9month,
         -Bath_shower_2ormoreperday_Follow_enroll,
         -attend_daycare_1month,
         -attend_daycare_3month,
         -attend_daycare_6month,
         -attend_daycare_9month,
         -attend_daycare_follow_enroll,
         -grade_school_1month,
         -grade_school_3month,
         -grade_school_6month,
         -grade_school_9month,
         -Pet_Hair_1month,
         -Pet_Hair_3month,
         -Pet_Hair_6month,
         -Pet_Hair_9month,
         -pet_age_1month,
         -pet_age_3month,
         -pet_age_6month,
         -pet_age_9month,
         -Pet_Appetite_1month,
         -Pet_Appetite_3month,
         -Pet_Appetite_6month,
         -Pet_Appetite_9month,
         -Pet_Energy_1month,
         -Pet_Energy_3month,
         -Pet_Energy_6month,
         -Pet_Energy_9month,
         -Pet_take_Antibiotics_6mo_enroll_1month,
         -Pet_take_Antibiotics_6mo_enroll_3month,
         -Pet_take_Antibiotics_6mo_enroll_6month,
         -Pet_take_Antibiotics_6mo_enroll_9month,
         -Pet_in_Daycare_at_Enroll_1month,
         -Pet_in_Daycare_at_Enroll_3month,
         -Pet_in_Daycare_at_Enroll_6month,
         -Pet_in_Daycare_at_Enroll_9month,
         -Pet_in_Daycare_followup_enroll,
         -Pet_In_or_Out_1month,
         -Pet_In_or_Out_3month,
         -Pet_In_or_Out_6month,
         -Pet_In_or_Out_9month,
         -Enroll_SSTI_last_12mo_1month,
         -Enroll_SSTI_last_12mo_3month,
         -Enroll_SSTI_last_12mo_6month,
         -Enroll_SSTI_last_12mo_9month,
         -Followup_SSTI_last_12mo_enroll,
         -pet_caretaker_enroll_1month,
         -pet_caretaker_enroll_3month,
         -pet_caretaker_enroll_6month,
         -pet_caretaker_enroll_9month,
         -pet_caretaker_followup_enroll,
         -sex_1month,
         -sex_3month,
         -sex_6month,
         -sex_9month,
         -race_1month,
         -race_3month,
         -race_6month,
         -race_9month,
         -race_white_1month,
         -race_white_3month,
         -race_white_6month,
         -race_white_9month,
         -race_black_1month,
         -race_black_3month,
         -race_black_6month,
         -race_black_9month,
         -race_asian_1month,
         -race_asian_3month,
         -race_asian_6month,
         -race_asian_9month,
         -race_biracial_1month,
         -race_biracial_3month,
         -race_biracial_6month,
         -race_biracial_9month,
         -Type_of_Pet_1month, 
         -Type_of_Pet_3month, 
         -Type_of_Pet_6month,
         -Type_of_Pet_9month,
         -Have_Pets_1month,
         -Have_Pets_3month,
         -Have_Pets_6month,
         -Have_Pets_9month,
         -SHINE_assignment_1month,
         -SHINE_assignment_3month,
         -SHINE_assignment_6month,
         -SHINE_assignment_9month,) %>% 
  rename('household_id' = 'hc_household_id_enroll',
         'sex' = 'sex_enroll',
         'race' = 'race_enroll',
         'race_white' = 'race_white_enroll',
         'race_black' = 'race_black_enroll',
         'race_asian' = 'race_asian_enroll',
         'race_biracial' = 'race_biracial_enroll',
         'Type_of_Pet' = 'Type_of_Pet_enroll',
         'Have_Pets' = 'Have_Pets_enroll',
         'Pet_Age' = 'pet_age_enroll',
         'SHINE_assignment' = 'SHINE_assignment_enroll',
         'Enroll_SSTI_last_12mo' = 'Enroll_SSTI_last_12mo_enroll',
         'pet_caretaker_enroll' = 'pet_caretaker_enroll_enroll',
         'Pet_Hair' = 'Pet_Hair_enroll',
         'Pet_In_or_Out' = 'Pet_In_or_Out_enroll',
         'Pet_Apppetite' = 'Pet_Appetite_enroll',
         'Pet_Energy' = 'Pet_Energy_enroll',
         'Pet_Antibiotic_6mo' = 'Pet_take_Antibiotics_6mo_enroll_enroll') %>% 
  rowid_to_column('ID') %>% 
  mutate(ID = as.numeric(ID)) %>% 
  mutate(Full_Study_Nares_MRSA = ifelse(Participant_Nares_MRSA_enroll == 'Yes' | 
                                          Participant_Nares_MRSA_1month == 'Yes' | 
                                          Participant_Nares_MRSA_3month == 'Yes' | 
                                          Participant_Nares_MRSA_6month == 'Yes' | 
                                          Participant_Nares_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Full_Study_Axilla_MRSA = ifelse(Participant_Axilla_MRSA_enroll == 'Yes' | 
                                           Participant_Axilla_MRSA_1month == 'Yes' | 
                                           Participant_Axilla_MRSA_3month == 'Yes' | 
                                           Participant_Axilla_MRSA_6month == 'Yes' | 
                                           Participant_Axilla_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Full_Study_Nares_MRSA = ifelse(Participant_Nares_MRSA_enroll == 'Yes' | 
                                          Participant_Nares_MRSA_1month == 'Yes' | 
                                          Participant_Nares_MRSA_3month == 'Yes' | 
                                          Participant_Nares_MRSA_6month == 'Yes' | 
                                          Participant_Nares_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Nares_MSSA_Prev = ifelse(Participant_Nares_MSSA_enroll == 'Yes' | 
                                                Participant_Nares_MSSA_1month == 'Yes' |
                                                Participant_Nares_MSSA_3month == 'Yes' |
                                                Participant_Nares_MSSA_6month == 'Yes' |
                                                Participant_Nares_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Nares_MSSA_Prev = as.factor(Participant_Nares_MSSA_Prev)) %>% 
  mutate(Participant_Axilla_MSSA_Prev = ifelse(Participant_Axilla_MSSA_enroll == 'Yes' | 
                                                 Participant_Axilla_MSSA_1month == 'Yes' |
                                                 Participant_Axilla_MSSA_3month == 'Yes' |
                                                 Participant_Axilla_MSSA_6month == 'Yes' |
                                                 Participant_Axilla_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MSSA_Prev = as.factor(Participant_Axilla_MSSA_Prev)) %>% 
  mutate(Participant_Inguinal_MSSA_Prev = ifelse(Participant_InguinalFold_MSSA_enroll == 'Yes' | 
                                                   Participant_InguinalFold_MSSA_1month == 'Yes' |
                                                   Participant_InguinalFold_MSSA_3month == 'Yes' |
                                                   Participant_InguinalFold_MSSA_6month == 'Yes' |
                                                   Participant_InguinalFold_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Inguinal_MSSA_Prev = as.factor(Participant_Inguinal_MSSA_Prev)) %>% 
  mutate(Participant_Nares_MRSP_Prev = ifelse(Participant_Nares_MRSP_enroll == 'Yes' |
                                                Participant_Nares_MRSP_1month == 'Yes' |
                                                Participant_Nares_MRSP_3month == 'Yes' |
                                                Participant_Nares_MRSP_6month == 'Yes' |
                                                Participant_Nares_MRSP_9month == 'Yes','Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Nares_MRSP_Prev = as.factor(Participant_Nares_MRSP_Prev)) %>% 
  mutate(Participant_Axilla_MRSP_Prev = ifelse(Participant_Axilla_MRSP_enroll == 'Yes' | 
                                                 Participant_Axilla_MRSP_1month == 'Yes' |
                                                 Participant_Axilla_MRSP_3month == 'Yes' |
                                                 Participant_Axilla_MRSP_6month == 'Yes' |
                                                 Participant_Axilla_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MRSP_Prev = as.factor(Participant_Axilla_MRSP_Prev)) %>% 
  mutate(Participant_Inguinal_MRSP_Prev = ifelse(Participant_InguinalFold_MRSP_enroll == 'Yes' | 
                                                   Participant_InguinalFold_MRSP_1month == 'Yes' |
                                                   Participant_InguinalFold_MRSP_3month == 'Yes' |
                                                   Participant_InguinalFold_MRSP_6month == 'Yes' |
                                                   Participant_InguinalFold_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Inguinal_MRSP_Prev = as.factor(Participant_Inguinal_MRSP_Prev)) %>% 
  mutate(Participant_Nares_MSSP_Prev = ifelse(Participant_Nares_MSSP_enroll == 'Yes' | 
                                                Participant_Nares_MSSP_1month == 'Yes' |
                                                Participant_Nares_MSSP_3month == 'Yes' |
                                                Participant_Nares_MSSP_6month == 'Yes' |
                                                Participant_Nares_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Nares_MSSP_Prev = as.factor(Participant_Nares_MSSP_Prev)) %>% 
  mutate(Participant_Axilla_MSSP_Prev = ifelse(Participant_Axilla_MSSP_enroll == 'Yes' | 
                                                 Participant_Axilla_MSSP_1month == 'Yes' |
                                                 Participant_Axilla_MSSP_3month == 'Yes' |
                                                 Participant_Axilla_MSSP_6month == 'Yes' |
                                                 Participant_Axilla_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MSSP_Prev = as.factor(Participant_Axilla_MSSP_Prev)) %>% 
  mutate(Participant_Inguinal_MSSP_Prev = ifelse(Participant_InguinalFold_MSSP_enroll == 'Yes' | 
                                                   Participant_InguinalFold_MSSP_1month == 'Yes' |
                                                   Participant_InguinalFold_MSSP_3month == 'Yes' |
                                                   Participant_InguinalFold_MSSP_6month == 'Yes' |
                                                   Participant_InguinalFold_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Inguinal_MSSP_Prev = as.factor(Participant_Inguinal_MSSP_Prev)) %>% 
  mutate(Participant_Nares_MSSS_Prev = ifelse(Participant_Nares_MSSS_enroll == 'Yes' | 
                                                Participant_Nares_MSSS_1month == 'Yes' |
                                                Participant_Nares_MSSS_3month == 'Yes' |
                                                Participant_Nares_MSSS_6month == 'Yes' |
                                                Participant_Nares_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Nares_MSSS_Prev = as.factor(Participant_Nares_MSSS_Prev)) %>% 
  mutate(Participant_Axilla_MSSS_Prev = ifelse(Participant_Axilla_MSSS_enroll == 'Yes' | 
                                                 Participant_Axilla_MSSS_1month == 'Yes' |
                                                 Participant_Axilla_MSSS_3month == 'Yes' |
                                                 Participant_Axilla_MSSS_6month == 'Yes' |
                                                 Participant_Axilla_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Axilla_MSSS_Prev = as.factor(Participant_Axilla_MSSS_Prev)) %>% 
  mutate(Participant_Inguinal_MSSS_Prev = ifelse(Participant_InguinalFold_MSSS_enroll == 'Yes' | 
                                                   Participant_InguinalFold_MSSS_1month == 'Yes' |
                                                   Participant_InguinalFold_MSSS_3month == 'Yes' |
                                                   Participant_InguinalFold_MSSS_6month == 'Yes' |
                                                   Participant_InguinalFold_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Inguinal_MSSS_Prev = as.factor(Participant_Inguinal_MSSS_Prev)) %>% 
  mutate(Participant_Nares_MSSD_Prev = ifelse(Participant_Nares_MSSD_enroll == 'Yes' | 
                                                Participant_Nares_MSSD_1month == 'Yes' |
                                                Participant_Nares_MSSD_3month == 'Yes' |
                                                Participant_Nares_MSSD_6month == 'Yes' |
                                                Participant_Nares_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Nares_MSSD_Prev = as.factor(Participant_Nares_MSSD_Prev)) %>% 
  mutate(Participant_Axilla_MSSD_Prev = ifelse(Participant_Axilla_MSSD_enroll == 'Yes' | 
                                                 Participant_Axilla_MSSD_1month == 'Yes' |
                                                 Participant_Axilla_MSSD_3month == 'Yes' |
                                                 Participant_Axilla_MSSD_6month == 'Yes' |
                                                 Participant_Axilla_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MSSD_Prev = as.factor(Participant_Axilla_MSSD_Prev)) %>% 
  mutate(Participant_Inguinal_MSSD_Prev = ifelse(Participant_InguinalFold_MSSD_enroll == 'Yes' | 
                                                   Participant_InguinalFold_MSSD_1month == 'Yes' |
                                                   Participant_InguinalFold_MSSD_3month == 'Yes' |
                                                   Participant_InguinalFold_MSSD_6month == 'Yes' |
                                                   Participant_InguinalFold_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Inguinal_MSSD_Prev = as.factor(Participant_Inguinal_MSSD_Prev)) %>% 
  mutate(Participant_Nares_MRSA_Prev = ifelse(Participant_Nares_MRSA_enroll == 'Yes' | 
                                                Participant_Nares_MRSA_1month == 'Yes' |
                                                Participant_Nares_MRSA_3month == 'Yes' |
                                                Participant_Nares_MRSA_6month == 'Yes' |
                                                Participant_Nares_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Nares_MRSA_Prev = as.factor(Participant_Nares_MRSA_Prev)) %>% 
  mutate(Participant_Axilla_MRSA_Prev = ifelse(Participant_Axilla_MRSA_enroll == 'Yes' | 
                                                 Participant_Axilla_MRSA_1month == 'Yes' |
                                                 Participant_Axilla_MRSA_3month == 'Yes' |
                                                 Participant_Axilla_MRSA_6month == 'Yes' |
                                                 Participant_Axilla_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MRSA_Prev = as.factor(Participant_Axilla_MRSA_Prev)) %>% 
  mutate(Participant_Inguinal_MRSA_Prev = ifelse(Participant_InguinalFold_MRSA_enroll == 'Yes' | 
                                                   Participant_InguinalFold_MRSA_1month == 'Yes' |
                                                   Participant_InguinalFold_MRSA_3month == 'Yes' |
                                                   Participant_InguinalFold_MRSA_6month == 'Yes' |
                                                   Participant_InguinalFold_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Inguinal_MRSA_Prev = as.factor(Participant_Inguinal_MRSA_Prev)) %>% 
  mutate(Pet_DorsalFur_MRSA_Prev = ifelse(Dorsal_Fur_MRSA_enroll == 'Yes' |
                                            Dorsal_Fur_MRSA_1month == 'Yes' |
                                            Dorsal_Fur_MRSA_3month == 'Yes' |
                                            Dorsal_Fur_MRSA_6month == 'Yes' |
                                            Dorsal_Fur_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MRSA_Prev = as.factor(Pet_DorsalFur_MRSA_Prev)) %>% 
  mutate(Pet_DorsalFur_MSSA_Prev = ifelse(Dorsal_Fur_MSSA_enroll == 'Yes' |
                                            Dorsal_Fur_MSSA_1month == 'Yes' |
                                            Dorsal_Fur_MSSA_3month == 'Yes' |
                                            Dorsal_Fur_MSSA_6month == 'Yes' |
                                            Dorsal_Fur_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MSSA_Prev = as.factor(Pet_DorsalFur_MSSA_Prev)) %>% 
  mutate(Pet_DorsalFur_MRSP_Prev = ifelse(Dorsal_Fur_MRSP_enroll == 'Yes' |
                                            Dorsal_Fur_MRSP_1month == 'Yes' |
                                            Dorsal_Fur_MRSP_3month == 'Yes' |
                                            Dorsal_Fur_MRSP_6month == 'Yes' |
                                            Dorsal_Fur_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MRSP_Prev = as.factor(Pet_DorsalFur_MRSP_Prev)) %>% 
  mutate(Pet_DorsalFur_MSSP_Prev = ifelse(Dorsal_Fur_MSSP_enroll == 'Yes' |
                                            Dorsal_Fur_MSSP_1month == 'Yes' |
                                            Dorsal_Fur_MSSP_3month == 'Yes' |
                                            Dorsal_Fur_MSSP_6month == 'Yes' |
                                            Dorsal_Fur_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MSSP_Prev = as.factor(Pet_DorsalFur_MSSP_Prev)) %>% 
  mutate(Pet_DorsalFur_MSSS_Prev = ifelse(Dorsal_Fur_MSSS_enroll == 'Yes' |
                                            Dorsal_Fur_MSSS_1month == 'Yes' |
                                            Dorsal_Fur_MSSS_3month == 'Yes' |
                                            Dorsal_Fur_MSSS_6month == 'Yes' |
                                            Dorsal_Fur_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MSSS_Prev = as.factor(Pet_DorsalFur_MSSS_Prev)) %>% 
  mutate(Pet_DorsalFur_MSSD_Prev = ifelse(Dorsal_Fur_MSSD_enroll == 'Yes' |
                                            Dorsal_Fur_MSSD_1month == 'Yes' |
                                            Dorsal_Fur_MSSD_3month == 'Yes' |
                                            Dorsal_Fur_MSSD_6month == 'Yes' |
                                            Dorsal_Fur_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Pet_DorsalFur_MSSD_Prev = as.factor(Pet_DorsalFur_MSSD_Prev)) %>% 
  mutate(Pet_Mouth_MRSA_Prev = ifelse(Pet_Mouth_MRSA_enroll == 'Yes' |
                                        Pet_Mouth_MRSA_1month == 'Yes' |
                                        Pet_Mouth_MRSA_3month == 'Yes' |
                                        Pet_Mouth_MRSA_6month == 'Yes' |
                                        Pet_Mouth_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MRSA_Prev = as.factor(Pet_Mouth_MRSA_Prev)) %>% 
  mutate(Pet_Mouth_MSSA_Prev = ifelse(Pet_Mouth_MSSA_enroll == 'Yes' |
                                        Pet_Mouth_MSSA_1month == 'Yes' |
                                        Pet_Mouth_MSSA_3month == 'Yes' |
                                        Pet_Mouth_MSSA_6month == 'Yes' |
                                        Pet_Mouth_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MSSA_Prev = as.factor(Pet_Mouth_MSSA_Prev)) %>% 
  mutate(Pet_Mouth_MRSP_Prev = ifelse(Pet_Mouth_MRSP_enroll == 'Yes' |
                                        Pet_Mouth_MRSP_1month == 'Yes' |
                                        Pet_Mouth_MRSP_3month == 'Yes' |
                                        Pet_Mouth_MRSP_6month == 'Yes' |
                                        Pet_Mouth_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MRSP_Prev = as.factor(Pet_Mouth_MRSP_Prev)) %>% 
  mutate(Pet_Mouth_MSSP_Prev = ifelse(Pet_Mouth_MSSP_enroll == 'Yes' |
                                        Pet_Mouth_MSSP_1month == 'Yes' |
                                        Pet_Mouth_MSSP_3month == 'Yes' |
                                        Pet_Mouth_MSSP_6month == 'Yes' |
                                        Pet_Mouth_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MSSP_Prev = as.factor(Pet_Mouth_MSSP_Prev)) %>% 
  mutate(Pet_Mouth_MSSS_Prev = ifelse(Pet_Mouth_MSSS_enroll == 'Yes' |
                                        Pet_Mouth_MSSS_1month == 'Yes' |
                                        Pet_Mouth_MSSS_3month == 'Yes' |
                                        Pet_Mouth_MSSS_6month == 'Yes' |
                                        Pet_Mouth_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MSSS_Prev = as.factor(Pet_Mouth_MSSS_Prev)) %>% 
  mutate(Pet_Mouth_MSSD_Prev = ifelse(Pet_Mouth_MSSD_enroll == 'Yes' |
                                        Pet_Mouth_MSSD_1month == 'Yes' |
                                        Pet_Mouth_MSSD_3month == 'Yes' |
                                        Pet_Mouth_MSSD_6month == 'Yes' |
                                        Pet_Mouth_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Pet_Mouth_MSSD_Prev = as.factor(Pet_Mouth_MSSD_Prev)) %>% 
  mutate(Participant_Nares_MSSA_Incid = ifelse(Participant_Nares_MSSA_enroll == 'No' & 
                                                 Participant_Nares_MSSA_1month == 'Yes' |
                                                 Participant_Nares_MSSA_3month == 'Yes' |
                                                 Participant_Nares_MSSA_6month == 'Yes' |
                                                 Participant_Nares_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MSSA_Incid = ifelse(Participant_Axilla_MSSA_enroll == 'No' & 
                                                  Participant_Axilla_MSSA_1month == 'Yes' |
                                                  Participant_Axilla_MSSA_3month == 'Yes' |
                                                  Participant_Axilla_MSSA_6month == 'Yes' |
                                                  Participant_Axilla_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Inguinal_MSSA_Incid = ifelse(Participant_InguinalFold_MSSA_enroll == 'No' & 
                                                    Participant_InguinalFold_MSSA_1month == 'Yes' |
                                                    Participant_InguinalFold_MSSA_3month == 'Yes' |
                                                    Participant_InguinalFold_MSSA_6month == 'Yes' |
                                                    Participant_InguinalFold_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Nares_MRSP_Incid = ifelse(Participant_Nares_MRSP_enroll == 'No' &
                                                 Participant_Nares_MRSP_1month == 'Yes' |
                                                 Participant_Nares_MRSP_3month == 'Yes' |
                                                 Participant_Nares_MRSP_6month == 'Yes' |
                                                 Participant_Nares_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MRSP_Incid = ifelse(Participant_Axilla_MRSP_enroll == 'No' & 
                                                  Participant_Axilla_MRSP_1month == 'Yes' |
                                                  Participant_Axilla_MRSP_3month == 'Yes' |
                                                  Participant_Axilla_MRSP_6month == 'Yes' |
                                                  Participant_Axilla_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Inguinal_MRSP_Incid = ifelse(Participant_InguinalFold_MRSP_enroll == 'No' & 
                                                    Participant_InguinalFold_MRSP_1month == 'Yes' |
                                                    Participant_InguinalFold_MRSP_3month == 'Yes' |
                                                    Participant_InguinalFold_MRSP_6month == 'Yes' |
                                                    Participant_InguinalFold_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Nares_MSSP_Incid = ifelse(Participant_Nares_MSSP_enroll == 'No' & 
                                                 Participant_Nares_MSSP_1month == 'Yes' |
                                                 Participant_Nares_MSSP_3month == 'Yes' |
                                                 Participant_Nares_MSSP_6month == 'Yes' |
                                                 Participant_Nares_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MSSP_Incid = ifelse(Participant_Axilla_MSSP_enroll == 'No' & 
                                                  Participant_Axilla_MSSP_1month == 'Yes' |
                                                  Participant_Axilla_MSSP_3month == 'Yes' |
                                                  Participant_Axilla_MSSP_6month == 'Yes' |
                                                  Participant_Axilla_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Inguinal_MSSP_Incid = ifelse(Participant_InguinalFold_MSSP_enroll == 'No' & 
                                                    Participant_InguinalFold_MSSP_1month == 'Yes' |
                                                    Participant_InguinalFold_MSSP_3month == 'Yes' |
                                                    Participant_InguinalFold_MSSP_6month == 'Yes' |
                                                    Participant_InguinalFold_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Nares_MSSS_Incid = ifelse(Participant_Nares_MSSS_enroll == 'No' & 
                                                 Participant_Nares_MSSS_1month == 'Yes' |
                                                 Participant_Nares_MSSS_3month == 'Yes' |
                                                 Participant_Nares_MSSS_6month == 'Yes' |
                                                 Participant_Nares_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MSSS_Incid = ifelse(Participant_Axilla_MSSS_enroll == 'No' & 
                                                  Participant_Axilla_MSSS_1month == 'Yes' |
                                                  Participant_Axilla_MSSS_3month == 'Yes' |
                                                  Participant_Axilla_MSSS_6month == 'Yes' |
                                                  Participant_Axilla_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Inguinal_MSSS_Incid = ifelse(Participant_InguinalFold_MSSS_enroll == 'No' & 
                                                    Participant_InguinalFold_MSSS_1month == 'Yes' |
                                                    Participant_InguinalFold_MSSS_3month == 'Yes' |
                                                    Participant_InguinalFold_MSSS_6month == 'Yes' |
                                                    Participant_InguinalFold_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Nares_MSSD_Incid = ifelse(Participant_Nares_MSSD_enroll == 'No' & 
                                                 Participant_Nares_MSSD_1month == 'Yes' |
                                                 Participant_Nares_MSSD_3month == 'Yes' |
                                                 Participant_Nares_MSSD_6month == 'Yes' |
                                                 Participant_Nares_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MSSD_Incid = ifelse(Participant_Axilla_MSSD_enroll == 'No' & 
                                                  Participant_Axilla_MSSD_1month == 'Yes' |
                                                  Participant_Axilla_MSSD_3month == 'Yes' |
                                                  Participant_Axilla_MSSD_6month == 'Yes' |
                                                  Participant_Axilla_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Inguinal_MSSD_Incid = ifelse(Participant_InguinalFold_MSSD_enroll == 'No' & 
                                                    Participant_InguinalFold_MSSD_1month == 'Yes' |
                                                    Participant_InguinalFold_MSSD_3month == 'Yes' |
                                                    Participant_InguinalFold_MSSD_6month == 'Yes' |
                                                    Participant_InguinalFold_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Participant_Nares_MRSA_Incid = ifelse(Participant_Nares_MRSA_enroll == 'No' & 
                                                 Participant_Nares_MRSA_1month == 'Yes' |
                                                 Participant_Nares_MRSA_3month == 'Yes' |
                                                 Participant_Nares_MRSA_6month == 'Yes' |
                                                 Participant_Nares_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Axilla_MRSA_Incid = ifelse(Participant_Axilla_MRSA_enroll == 'No' & 
                                                  Participant_Axilla_MRSA_1month == 'Yes' |
                                                  Participant_Axilla_MRSA_3month == 'Yes' |
                                                  Participant_Axilla_MRSA_6month == 'Yes' |
                                                  Participant_Axilla_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Inguinal_MRSA_Incid = ifelse(Participant_InguinalFold_MRSA_enroll == 'No' & 
                                                    Participant_InguinalFold_MRSA_1month == 'Yes' |
                                                    Participant_InguinalFold_MRSA_3month == 'Yes' |
                                                    Participant_InguinalFold_MRSA_6month == 'Yes' |
                                                    Participant_InguinalFold_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Pet_DorsalFur_MRSA_Incid = ifelse(Dorsal_Fur_MRSA_enroll == 'No' &
                                             Dorsal_Fur_MRSA_1month == 'Yes' |
                                             Dorsal_Fur_MRSA_3month == 'Yes' |
                                             Dorsal_Fur_MRSA_6month == 'Yes' |
                                             Dorsal_Fur_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MSSA_Incid = ifelse(Dorsal_Fur_MSSA_enroll == 'No' &
                                             Dorsal_Fur_MSSA_1month == 'Yes' |
                                             Dorsal_Fur_MSSA_3month == 'Yes' |
                                             Dorsal_Fur_MSSA_6month == 'Yes' |
                                             Dorsal_Fur_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MRSP_Incid = ifelse(Dorsal_Fur_MRSP_enroll == 'No' &
                                             Dorsal_Fur_MRSP_1month == 'Yes' |
                                             Dorsal_Fur_MRSP_3month == 'Yes' |
                                             Dorsal_Fur_MRSP_6month == 'Yes' |
                                             Dorsal_Fur_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MSSP_Incid = ifelse(Dorsal_Fur_MSSP_enroll == 'No' &
                                             Dorsal_Fur_MSSP_1month == 'Yes' |
                                             Dorsal_Fur_MSSP_3month == 'Yes' |
                                             Dorsal_Fur_MSSP_6month == 'Yes' |
                                             Dorsal_Fur_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MSSS_Incid = ifelse(Dorsal_Fur_MSSS_enroll == 'No' &
                                             Dorsal_Fur_MSSS_1month == 'Yes' |
                                             Dorsal_Fur_MSSS_3month == 'Yes' |
                                             Dorsal_Fur_MSSS_6month == 'Yes' |
                                             Dorsal_Fur_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_DorsalFur_MSSD_Incid = ifelse(Dorsal_Fur_MSSD_enroll == 'No' &
                                             Dorsal_Fur_MSSD_1month == 'Yes' |
                                             Dorsal_Fur_MSSD_3month == 'Yes' |
                                             Dorsal_Fur_MSSD_6month == 'Yes' |
                                             Dorsal_Fur_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Pet_Mouth_MRSA_Incid = ifelse(Pet_Mouth_MRSA_enroll == 'No' &
                                         Pet_Mouth_MRSA_1month == 'Yes' |
                                         Pet_Mouth_MRSA_3month == 'Yes' |
                                         Pet_Mouth_MRSA_6month == 'Yes' |
                                         Pet_Mouth_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MSSA_Incid = ifelse(Pet_Mouth_MSSA_enroll == 'No' &
                                         Pet_Mouth_MSSA_1month == 'Yes' |
                                         Pet_Mouth_MSSA_3month == 'Yes' |
                                         Pet_Mouth_MSSA_6month == 'Yes' |
                                         Pet_Mouth_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MRSP_Incid = ifelse(Pet_Mouth_MRSP_enroll == 'No' &
                                         Pet_Mouth_MRSP_1month == 'Yes' |
                                         Pet_Mouth_MRSP_3month == 'Yes' |
                                         Pet_Mouth_MRSP_6month == 'Yes' |
                                         Pet_Mouth_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MSSP_Incid = ifelse(Pet_Mouth_MSSP_enroll == 'No' &
                                         Pet_Mouth_MSSP_1month == 'Yes' |
                                         Pet_Mouth_MSSP_3month == 'Yes' |
                                         Pet_Mouth_MSSP_6month == 'Yes' |
                                         Pet_Mouth_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MSSS_Incid = ifelse(Pet_Mouth_MSSS_enroll == 'No' &
                                         Pet_Mouth_MSSS_1month == 'Yes' |
                                         Pet_Mouth_MSSS_3month == 'Yes' |
                                         Pet_Mouth_MSSS_6month == 'Yes' |
                                         Pet_Mouth_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Pet_Mouth_MSSD_Incid = ifelse(Pet_Mouth_MSSD_enroll == 'No' &
                                         Pet_Mouth_MSSD_1month == 'Yes' |
                                         Pet_Mouth_MSSD_3month == 'Yes' |
                                         Pet_Mouth_MSSD_6month == 'Yes' |
                                         Pet_Mouth_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Participant_Nares_MRSA_Prev = as.factor(Participant_Nares_MRSA_Prev)) %>% 
  mutate(TV_Remote_MRSA_Prev = ifelse(TV_Remote_MRSA_enroll == 'Yes' |
                                        TV_Remote_MRSA_1month == 'Yes' |
                                        TV_Remote_MRSA_3month == 'Yes' |
                                        TV_Remote_MRSA_6month == 'Yes' |
                                        TV_Remote_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(TV_Remote_MRSA_Prev = as.factor(TV_Remote_MRSA_Prev)) %>% 
  mutate(Sofa_MRSA_Prev = ifelse(Sofa_MRSA_enroll == 'Yes' |
                                   Sofa_MRSA_1month == 'Yes' |
                                   Sofa_MRSA_3month == 'Yes' |
                                   Sofa_MRSA_6month == 'Yes' |
                                   Sofa_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Sofa_MRSA_Prev = as.factor(Sofa_MRSA_Prev)) %>% 
  mutate(Telephone_MRSA_Prev = ifelse(Telephone_MRSA_enroll == 'Yes' |
                                        Telephone_MRSA_1month == 'Yes' |
                                        Telephone_MRSA_3month == 'Yes' |
                                        Telephone_MRSA_6month == 'Yes' |
                                        Telephone_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Telephone_MRSA_Prev = as.factor(Telephone_MRSA_Prev)) %>% 
  mutate(Comp_Keyboard_MRSA_Prev = ifelse(Comp_Keyboard_MRSA_enroll == 'Yes' |
                                            Comp_Keyboard_MRSA_1month == 'Yes' |
                                            Comp_Keyboard_MRSA_3month == 'Yes' |
                                            Comp_Keyboard_MRSA_6month == 'Yes' |
                                            Comp_Keyboard_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Comp_Keyboard_MRSA_Prev = as.factor(Comp_Keyboard_MRSA_Prev)) %>% 
  mutate(Tablet_MRSA_Prev = ifelse(Tablet_MRSA_enroll == 'Yes' |
                                     Tablet_MRSA_1month == 'Yes' |
                                     Tablet_MRSA_3month == 'Yes' |
                                     Tablet_MRSA_6month == 'Yes' |
                                     Tablet_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Tablet_MRSA_Prev = as.factor(Tablet_MRSA_Prev)) %>% 
  mutate(Door_Out_MRSA_Prev = ifelse(Door_Out_MRSA_enroll == 'Yes' |
                                       Door_Out_MRSA_1month == 'Yes' |
                                       Door_Out_MRSA_3month == 'Yes' |
                                       Door_Out_MRSA_6month == 'Yes' |
                                       Door_Out_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Door_Out_MRSA_Prev = as.factor(Door_Out_MRSA_Prev)) %>% 
  mutate(Bathroom_light_MRSA_Prev = ifelse(Bathroom_light_MRSA_enroll == 'Yes' |
                                             Bathroom_light_MRSA_1month == 'Yes' |
                                             Bathroom_light_MRSA_3month == 'Yes' |
                                             Bathroom_light_MRSA_6month == 'Yes' |
                                             Bathroom_light_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_light_MRSA_Prev = as.factor(Bathroom_light_MRSA_Prev)) %>% 
  mutate(Bathroom_sink_MRSA_Prev = ifelse(Bathroom_sink_MRSA_enroll == 'Yes' |
                                            Bathroom_sink_MRSA_1month == 'Yes' |
                                            Bathroom_sink_MRSA_3month == 'Yes' |
                                            Bathroom_sink_MRSA_6month == 'Yes' |
                                            Bathroom_sink_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_sink_MRSA_Prev = as.factor(Bathroom_sink_MRSA_Prev)) %>% 
  mutate(Toilet_MRSA_Prev = ifelse(Toilet_MRSA_enroll == 'Yes' |
                                     Toilet_MRSA_1month == 'Yes' |
                                     Toilet_MRSA_3month == 'Yes' |
                                     Toilet_MRSA_6month == 'Yes' |
                                     Toilet_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Toilet_MRSA_Prev = as.factor(Toilet_MRSA_Prev)) %>% 
  mutate(Kitchen_light_MRSA_Prev = ifelse(Kitchen_light_MRSA_enroll == 'Yes' |
                                            Kitchen_light_MRSA_1month == 'Yes' |
                                            Kitchen_light_MRSA_3month == 'Yes' |
                                            Kitchen_light_MRSA_6month == 'Yes' |
                                            Kitchen_light_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_light_MRSA_Prev = as.factor(Kitchen_light_MRSA_Prev)) %>% 
  mutate(Kitchen_sink_MRSA_Prev = ifelse(Kitchen_sink_MRSA_enroll == 'Yes' |
                                           Kitchen_sink_MRSA_1month == 'Yes' |
                                           Kitchen_sink_MRSA_3month == 'Yes' |
                                           Kitchen_sink_MRSA_6month == 'Yes' |
                                           Kitchen_sink_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_sink_MRSA_Prev = as.factor(Kitchen_sink_MRSA_Prev)) %>% 
  mutate(Refrigerator_MRSA_Prev = ifelse(Refrigerator_MRSA_enroll == 'Yes' |
                                           Refrigerator_MRSA_1month == 'Yes' |
                                           Refrigerator_MRSA_3month == 'Yes' |
                                           Refrigerator_MRSA_6month == 'Yes' |
                                           Refrigerator_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Refrigerator_MRSA_Prev = as.factor(Refrigerator_MRSA_Prev)) %>% 
  mutate(Microwave_MRSA_Prev = ifelse(Microwave_MRSA_enroll == 'Yes' |
                                        Microwave_MRSA_1month == 'Yes' |
                                        Microwave_MRSA_3month == 'Yes' |
                                        Microwave_MRSA_6month == 'Yes' |
                                        Microwave_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Microwave_MRSA_Prev = as.factor(Microwave_MRSA_Prev)) %>% 
  mutate(Oven_MRSA_Prev = ifelse(Oven_MRSA_enroll == 'Yes' |
                                   Oven_MRSA_1month == 'Yes' |
                                   Oven_MRSA_3month == 'Yes' |
                                   Oven_MRSA_6month == 'Yes' |
                                   Oven_MRSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Oven_MRSA_Prev = as.factor(Oven_MRSA_Prev)) %>% 
  mutate(TV_Remote_MSSA_Prev = ifelse(TV_Remote_MSSA_enroll == 'Yes' |
                                        TV_Remote_MSSA_1month == 'Yes' |
                                        TV_Remote_MSSA_3month == 'Yes' |
                                        TV_Remote_MSSA_6month == 'Yes' |
                                        TV_Remote_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(TV_Remote_MSSA_Prev = as.factor(TV_Remote_MSSA_Prev)) %>% 
  mutate(Sofa_MSSA_Prev = ifelse(Sofa_MSSA_enroll == 'Yes' |
                                   Sofa_MSSA_1month == 'Yes' |
                                   Sofa_MSSA_3month == 'Yes' |
                                   Sofa_MSSA_6month == 'Yes' |
                                   Sofa_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Sofa_MSSA_Prev = as.factor(Sofa_MSSA_Prev)) %>% 
  mutate(Telephone_MSSA_Prev = ifelse(Telephone_MSSA_enroll == 'Yes' |
                                        Telephone_MSSA_1month == 'Yes' |
                                        Telephone_MSSA_3month == 'Yes' |
                                        Telephone_MSSA_6month == 'Yes' |
                                        Telephone_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Telephone_MSSA_Prev = as.factor(Telephone_MSSA_Prev)) %>% 
  mutate(Comp_Keyboard_MSSA_Prev = ifelse(Comp_Keyboard_MSSA_enroll == 'Yes' |
                                            Comp_Keyboard_MSSA_1month == 'Yes' |
                                            Comp_Keyboard_MSSA_3month == 'Yes' |
                                            Comp_Keyboard_MSSA_6month == 'Yes' |
                                            Comp_Keyboard_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Comp_Keyboard_MSSA_Prev = as.factor(Comp_Keyboard_MSSA_Prev)) %>% 
  mutate(Tablet_MSSA_Prev = ifelse(Tablet_MSSA_enroll == 'Yes' |
                                     Tablet_MSSA_1month == 'Yes' |
                                     Tablet_MSSA_3month == 'Yes' |
                                     Tablet_MSSA_6month == 'Yes' |
                                     Tablet_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Tablet_MSSA_Prev = as.factor(Tablet_MSSA_Prev)) %>% 
  mutate(Door_Out_MSSA_Prev = ifelse(Door_Out_MSSA_enroll == 'Yes' |
                                       Door_Out_MSSA_1month == 'Yes' |
                                       Door_Out_MSSA_3month == 'Yes' |
                                       Door_Out_MSSA_6month == 'Yes' |
                                       Door_Out_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Door_Out_MSSA_Prev = as.factor(Door_Out_MSSA_Prev)) %>%   
  mutate(Bathroom_light_MSSA_Prev = ifelse(Bathroom_light_MSSA_enroll == 'Yes' |
                                             Bathroom_light_MSSA_1month == 'Yes' |
                                             Bathroom_light_MSSA_3month == 'Yes' |
                                             Bathroom_light_MSSA_6month == 'Yes' |
                                             Bathroom_light_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_light_MSSA_Prev = as.factor(Bathroom_light_MSSA_Prev)) %>% 
  mutate(Bathroom_sink_MSSA_Prev = ifelse(Bathroom_sink_MSSA_enroll == 'Yes' |
                                            Bathroom_sink_MSSA_1month == 'Yes' |
                                            Bathroom_sink_MSSA_3month == 'Yes' |
                                            Bathroom_sink_MSSA_6month == 'Yes' |
                                            Bathroom_sink_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_sink_MSSA_Prev = as.factor(Bathroom_sink_MSSA_Prev)) %>% 
  mutate(Toilet_MSSA_Prev = ifelse(Toilet_MSSA_enroll == 'Yes' |
                                     Toilet_MSSA_1month == 'Yes' |
                                     Toilet_MSSA_3month == 'Yes' |
                                     Toilet_MSSA_6month == 'Yes' |
                                     Toilet_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Toilet_MSSA_Prev = as.factor(Toilet_MSSA_Prev)) %>% 
  mutate(Kitchen_light_MSSA_Prev = ifelse(Kitchen_light_MSSA_enroll == 'Yes' |
                                            Kitchen_light_MSSA_1month == 'Yes' |
                                            Kitchen_light_MSSA_3month == 'Yes' |
                                            Kitchen_light_MSSA_6month == 'Yes' |
                                            Kitchen_light_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_light_MSSA_Prev = as.factor(Kitchen_light_MSSA_Prev)) %>% 
  mutate(Kitchen_sink_MSSA_Prev = ifelse(Kitchen_sink_MSSA_enroll == 'Yes' |
                                           Kitchen_sink_MSSA_1month == 'Yes' |
                                           Kitchen_sink_MSSA_3month == 'Yes' |
                                           Kitchen_sink_MSSA_6month == 'Yes' |
                                           Kitchen_sink_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_sink_MSSA_Prev = as.factor(Kitchen_sink_MSSA_Prev)) %>% 
  mutate(Refrigerator_MSSA_Prev = ifelse(Refrigerator_MSSA_enroll == 'Yes' |
                                           Refrigerator_MSSA_1month == 'Yes' |
                                           Refrigerator_MSSA_3month == 'Yes' |
                                           Refrigerator_MSSA_6month == 'Yes' |
                                           Refrigerator_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Refrigerator_MSSA_Prev = as.factor(Refrigerator_MSSA_Prev)) %>% 
  mutate(Microwave_MSSA_Prev = ifelse(Microwave_MSSA_enroll == 'Yes' |
                                        Microwave_MSSA_1month == 'Yes' |
                                        Microwave_MSSA_3month == 'Yes' |
                                        Microwave_MSSA_6month == 'Yes' |
                                        Microwave_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Microwave_MSSA_Prev = as.factor(Microwave_MSSA_Prev)) %>% 
  mutate(Oven_MSSA_Prev = ifelse(Oven_MSSA_enroll == 'Yes' |
                                   Oven_MSSA_1month == 'Yes' |
                                   Oven_MSSA_3month == 'Yes' |
                                   Oven_MSSA_6month == 'Yes' |
                                   Oven_MSSA_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Oven_MSSA_Prev = as.factor(Oven_MSSA_Prev)) %>% 
  mutate(TV_Remote_MRSP_Prev = ifelse(TV_Remote_MRSP_enroll == 'Yes' |
                                        TV_Remote_MRSP_1month == 'Yes' |
                                        TV_Remote_MRSP_3month == 'Yes' |
                                        TV_Remote_MRSP_6month == 'Yes' |
                                        TV_Remote_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(TV_Remote_MRSP_Prev = as.factor(TV_Remote_MRSP_Prev)) %>% 
  mutate(Sofa_MRSP_Prev = ifelse(Sofa_MRSP_enroll == 'Yes' |
                                   Sofa_MRSP_1month == 'Yes' |
                                   Sofa_MRSP_3month == 'Yes' |
                                   Sofa_MRSP_6month == 'Yes' |
                                   Sofa_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Sofa_MRSP_Prev = as.factor(Sofa_MRSP_Prev)) %>% 
  mutate(Telephone_MRSP_Prev = ifelse(Telephone_MRSP_enroll == 'Yes' |
                                        Telephone_MRSP_1month == 'Yes' |
                                        Telephone_MRSP_3month == 'Yes' |
                                        Telephone_MRSP_6month == 'Yes' |
                                        Telephone_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Telephone_MRSP_Prev = as.factor(Telephone_MRSP_Prev)) %>% 
  mutate(Comp_Keyboard_MRSP_Prev = ifelse(Comp_Keyboard_MRSP_enroll == 'Yes' |
                                            Comp_Keyboard_MRSP_1month == 'Yes' |
                                            Comp_Keyboard_MRSP_3month == 'Yes' |
                                            Comp_Keyboard_MRSP_6month == 'Yes' |
                                            Comp_Keyboard_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Comp_Keyboard_MRSP_Prev = as.factor(Comp_Keyboard_MRSP_Prev)) %>% 
  mutate(Tablet_MRSP_Prev = ifelse(Tablet_MRSP_enroll == 'Yes' |
                                     Tablet_MRSP_1month == 'Yes' |
                                     Tablet_MRSP_3month == 'Yes' |
                                     Tablet_MRSP_6month == 'Yes' |
                                     Tablet_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Tablet_MRSP_Prev = as.factor(Tablet_MRSP_Prev)) %>% 
  mutate(Door_Out_MRSP_Prev = ifelse(Door_Out_MRSP_enroll == 'Yes' |
                                       Door_Out_MRSP_1month == 'Yes' |
                                       Door_Out_MRSP_3month == 'Yes' |
                                       Door_Out_MRSP_6month == 'Yes' |
                                       Door_Out_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Door_Out_MRSP_Prev = as.factor(Door_Out_MRSP_Prev)) %>% 
  mutate(Bathroom_light_MRSP_Prev = ifelse(Bathroom_light_MRSP_enroll == 'Yes' |
                                             Bathroom_light_MRSP_1month == 'Yes' |
                                             Bathroom_light_MRSP_3month == 'Yes' |
                                             Bathroom_light_MRSP_6month == 'Yes' |
                                             Bathroom_light_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_light_MRSP_Prev = as.factor(Bathroom_light_MRSP_Prev)) %>% 
  mutate(Bathroom_sink_MRSP_Prev = ifelse(Bathroom_sink_MRSP_enroll == 'Yes' |
                                            Bathroom_sink_MRSP_1month == 'Yes' |
                                            Bathroom_sink_MRSP_3month == 'Yes' |
                                            Bathroom_sink_MRSP_6month == 'Yes' |
                                            Bathroom_sink_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_sink_MRSP_Prev = as.factor(Bathroom_sink_MRSP_Prev)) %>% 
  mutate(Toilet_MRSP_Prev = ifelse(Toilet_MRSP_enroll == 'Yes' |
                                     Toilet_MRSP_1month == 'Yes' |
                                     Toilet_MRSP_3month == 'Yes' |
                                     Toilet_MRSP_6month == 'Yes' |
                                     Toilet_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Toilet_MRSP_Prev = as.factor(Toilet_MRSP_Prev)) %>% 
  mutate(Kitchen_light_MRSP_Prev = ifelse(Kitchen_light_MRSP_enroll == 'Yes' |
                                            Kitchen_light_MRSP_1month == 'Yes' |
                                            Kitchen_light_MRSP_3month == 'Yes' |
                                            Kitchen_light_MRSP_6month == 'Yes' |
                                            Kitchen_light_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_light_MRSP_Prev = as.factor(Kitchen_light_MRSP_Prev)) %>% 
  mutate(Kitchen_sink_MRSP_Prev = ifelse(Kitchen_sink_MRSP_enroll == 'Yes' |
                                           Kitchen_sink_MRSP_1month == 'Yes' |
                                           Kitchen_sink_MRSP_3month == 'Yes' |
                                           Kitchen_sink_MRSP_6month == 'Yes' |
                                           Kitchen_sink_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_sink_MRSP_Prev = as.factor(Kitchen_sink_MRSP_Prev)) %>% 
  mutate(Refrigerator_MRSP_Prev = ifelse(Refrigerator_MRSP_enroll == 'Yes' |
                                           Refrigerator_MRSP_1month == 'Yes' |
                                           Refrigerator_MRSP_3month == 'Yes' |
                                           Refrigerator_MRSP_6month == 'Yes' |
                                           Refrigerator_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Refrigerator_MRSP_Prev = as.factor(Refrigerator_MRSP_Prev)) %>% 
  mutate(Microwave_MRSP_Prev = ifelse(Microwave_MRSP_enroll == 'Yes' |
                                        Microwave_MRSP_1month == 'Yes' |
                                        Microwave_MRSP_3month == 'Yes' |
                                        Microwave_MRSP_6month == 'Yes' |
                                        Microwave_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Microwave_MRSP_Prev = as.factor(Microwave_MRSP_Prev)) %>% 
  mutate(Oven_MRSP_Prev = ifelse(Oven_MRSP_enroll == 'Yes' |
                                   Oven_MRSP_1month == 'Yes' |
                                   Oven_MRSP_3month == 'Yes' |
                                   Oven_MRSP_6month == 'Yes' |
                                   Oven_MRSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Oven_MRSP_Prev = as.factor(Oven_MRSP_Prev)) %>% 
  mutate(TV_Remote_MSSP_Prev = ifelse(TV_Remote_MSSP_enroll == 'Yes' |
                                        TV_Remote_MSSP_1month == 'Yes' |
                                        TV_Remote_MSSP_3month == 'Yes' |
                                        TV_Remote_MSSP_6month == 'Yes' |
                                        TV_Remote_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(TV_Remote_MSSP_Prev = as.factor(TV_Remote_MSSP_Prev)) %>% 
  mutate(Sofa_MSSP_Prev = ifelse(Sofa_MSSP_enroll == 'Yes' |
                                   Sofa_MSSP_1month == 'Yes' |
                                   Sofa_MSSP_3month == 'Yes' |
                                   Sofa_MSSP_6month == 'Yes' |
                                   Sofa_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Sofa_MSSP_Prev = as.factor(Sofa_MSSP_Prev)) %>% 
  mutate(Telephone_MSSP_Prev = ifelse(Telephone_MSSP_enroll == 'Yes' |
                                        Telephone_MSSP_1month == 'Yes' |
                                        Telephone_MSSP_3month == 'Yes' |
                                        Telephone_MSSP_6month == 'Yes' |
                                        Telephone_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Telephone_MSSP_Prev = as.factor(Telephone_MSSP_Prev)) %>% 
  mutate(Comp_Keyboard_MSSP_Prev = ifelse(Comp_Keyboard_MSSP_enroll == 'Yes' |
                                            Comp_Keyboard_MSSP_1month == 'Yes' |
                                            Comp_Keyboard_MSSP_3month == 'Yes' |
                                            Comp_Keyboard_MSSP_6month == 'Yes' |
                                            Comp_Keyboard_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Comp_Keyboard_MSSP_Prev = as.factor(Comp_Keyboard_MSSP_Prev)) %>% 
  mutate(Tablet_MSSP_Prev = ifelse(Tablet_MSSP_enroll == 'Yes' |
                                     Tablet_MSSP_1month == 'Yes' |
                                     Tablet_MSSP_3month == 'Yes' |
                                     Tablet_MSSP_6month == 'Yes' |
                                     Tablet_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Tablet_MSSP_Prev = as.factor(Tablet_MSSP_Prev)) %>% 
  mutate(Door_Out_MSSP_Prev = ifelse(Door_Out_MSSP_enroll == 'Yes' |
                                       Door_Out_MSSP_1month == 'Yes' |
                                       Door_Out_MSSP_3month == 'Yes' |
                                       Door_Out_MSSP_6month == 'Yes' |
                                       Door_Out_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Door_Out_MSSP_Prev = as.factor(Door_Out_MSSP_Prev)) %>% 
  mutate(Bathroom_light_MSSP_Prev = ifelse(Bathroom_light_MSSP_enroll == 'Yes' |
                                             Bathroom_light_MSSP_1month == 'Yes' |
                                             Bathroom_light_MSSP_3month == 'Yes' |
                                             Bathroom_light_MSSP_6month == 'Yes' |
                                             Bathroom_light_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_light_MSSP_Prev = as.factor(Bathroom_light_MSSP_Prev)) %>% 
  mutate(Bathroom_sink_MSSP_Prev = ifelse(Bathroom_sink_MSSP_enroll == 'Yes' |
                                            Bathroom_sink_MSSP_1month == 'Yes' |
                                            Bathroom_sink_MSSP_3month == 'Yes' |
                                            Bathroom_sink_MSSP_6month == 'Yes' |
                                            Bathroom_sink_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_sink_MSSP_Prev = as.factor(Bathroom_sink_MSSP_Prev)) %>% 
  mutate(Toilet_MSSP_Prev = ifelse(Toilet_MSSP_enroll == 'Yes' |
                                     Toilet_MSSP_1month == 'Yes' |
                                     Toilet_MSSP_3month == 'Yes' |
                                     Toilet_MSSP_6month == 'Yes' |
                                     Toilet_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Toilet_MSSP_Prev = as.factor(Toilet_MSSP_Prev)) %>% 
  mutate(Kitchen_light_MSSP_Prev = ifelse(Kitchen_light_MSSP_enroll == 'Yes' |
                                            Kitchen_light_MSSP_1month == 'Yes' |
                                            Kitchen_light_MSSP_3month == 'Yes' |
                                            Kitchen_light_MSSP_6month == 'Yes' |
                                            Kitchen_light_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_light_MSSP_Prev = as.factor(Kitchen_light_MSSP_Prev)) %>% 
  mutate(Kitchen_sink_MSSP_Prev = ifelse(Kitchen_sink_MSSP_enroll == 'Yes' |
                                           Kitchen_sink_MSSP_1month == 'Yes' |
                                           Kitchen_sink_MSSP_3month == 'Yes' |
                                           Kitchen_sink_MSSP_6month == 'Yes' |
                                           Kitchen_sink_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_sink_MSSP_Prev = as.factor(Kitchen_sink_MSSP_Prev)) %>% 
  mutate(Refrigerator_MSSP_Prev = ifelse(Refrigerator_MSSP_enroll == 'Yes' |
                                           Refrigerator_MSSP_1month == 'Yes' |
                                           Refrigerator_MSSP_3month == 'Yes' |
                                           Refrigerator_MSSP_6month == 'Yes' |
                                           Refrigerator_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Refrigerator_MSSP_Prev = as.factor(Refrigerator_MSSP_Prev)) %>% 
  mutate(Microwave_MSSP_Prev = ifelse(Microwave_MSSP_enroll == 'Yes' |
                                        Microwave_MSSP_1month == 'Yes' |
                                        Microwave_MSSP_3month == 'Yes' |
                                        Microwave_MSSP_6month == 'Yes' |
                                        Microwave_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Microwave_MSSP_Prev = as.factor(Microwave_MSSP_Prev)) %>% 
  mutate(Oven_MSSP_Prev = ifelse(Oven_MSSP_enroll == 'Yes' |
                                   Oven_MSSP_1month == 'Yes' |
                                   Oven_MSSP_3month == 'Yes' |
                                   Oven_MSSP_6month == 'Yes' |
                                   Oven_MSSP_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Oven_MSSP_Prev = as.factor(Oven_MSSP_Prev)) %>% 
  mutate(TV_Remote_MSSS_Prev = ifelse(TV_Remote_MSSS_enroll == 'Yes' |
                                        TV_Remote_MSSS_1month == 'Yes' |
                                        TV_Remote_MSSS_3month == 'Yes' |
                                        TV_Remote_MSSS_6month == 'Yes' |
                                        TV_Remote_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(TV_Remote_MSSS_Prev = as.factor(TV_Remote_MSSS_Prev)) %>% 
  mutate(Sofa_MSSS_Prev = ifelse(Sofa_MSSS_enroll == 'Yes' |
                                   Sofa_MSSS_1month == 'Yes' |
                                   Sofa_MSSS_3month == 'Yes' |
                                   Sofa_MSSS_6month == 'Yes' |
                                   Sofa_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Sofa_MSSS_Prev = as.factor(Sofa_MSSS_Prev)) %>% 
  mutate(Telephone_MSSS_Prev = ifelse(Telephone_MSSS_enroll == 'Yes' |
                                        Telephone_MSSS_1month == 'Yes' |
                                        Telephone_MSSS_3month == 'Yes' |
                                        Telephone_MSSS_6month == 'Yes' |
                                        Telephone_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Telephone_MSSS_Prev = as.factor(Telephone_MSSS_Prev)) %>% 
  mutate(Comp_Keyboard_MSSS_Prev = ifelse(Comp_Keyboard_MSSS_enroll == 'Yes' |
                                            Comp_Keyboard_MSSS_1month == 'Yes' |
                                            Comp_Keyboard_MSSS_3month == 'Yes' |
                                            Comp_Keyboard_MSSS_6month == 'Yes' |
                                            Comp_Keyboard_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Comp_Keyboard_MSSS_Prev = as.factor(Comp_Keyboard_MSSS_Prev)) %>% 
  mutate(Tablet_MSSS_Prev = ifelse(Tablet_MSSS_enroll == 'Yes' |
                                     Tablet_MSSS_1month == 'Yes' |
                                     Tablet_MSSS_3month == 'Yes' |
                                     Tablet_MSSS_6month == 'Yes' |
                                     Tablet_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Tablet_MSSS_Prev = as.factor(Tablet_MSSS_Prev)) %>% 
  mutate(Door_Out_MSSS_Prev = ifelse(Door_Out_MSSS_enroll == 'Yes' |
                                       Door_Out_MSSS_1month == 'Yes' |
                                       Door_Out_MSSS_3month == 'Yes' |
                                       Door_Out_MSSS_6month == 'Yes' |
                                       Door_Out_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Door_Out_MSSS_Prev = as.factor(Door_Out_MSSS_Prev)) %>% 
  mutate(Bathroom_light_MSSS_Prev = ifelse(Bathroom_light_MSSS_enroll == 'Yes' |
                                             Bathroom_light_MSSS_1month == 'Yes' |
                                             Bathroom_light_MSSS_3month == 'Yes' |
                                             Bathroom_light_MSSS_6month == 'Yes' |
                                             Bathroom_light_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_light_MSSS_Prev = as.factor(Bathroom_light_MSSS_Prev)) %>% 
  mutate(Bathroom_sink_MSSS_Prev = ifelse(Bathroom_sink_MSSS_enroll == 'Yes' |
                                            Bathroom_sink_MSSS_1month == 'Yes' |
                                            Bathroom_sink_MSSS_3month == 'Yes' |
                                            Bathroom_sink_MSSS_6month == 'Yes' |
                                            Bathroom_sink_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_sink_MSSS_Prev = as.factor(Bathroom_sink_MSSS_Prev)) %>% 
  mutate(Toilet_MSSS_Prev = ifelse(Toilet_MSSS_enroll == 'Yes' |
                                     Toilet_MSSS_1month == 'Yes' |
                                     Toilet_MSSS_3month == 'Yes' |
                                     Toilet_MSSS_6month == 'Yes' |
                                     Toilet_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Toilet_MSSS_Prev = as.factor(Toilet_MSSS_Prev)) %>% 
  mutate(Kitchen_light_MSSS_Prev = ifelse(Kitchen_light_MSSS_enroll == 'Yes' |
                                            Kitchen_light_MSSS_1month == 'Yes' |
                                            Kitchen_light_MSSS_3month == 'Yes' |
                                            Kitchen_light_MSSS_6month == 'Yes' |
                                            Kitchen_light_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_light_MSSS_Prev = as.factor(Kitchen_light_MSSS_Prev)) %>% 
  mutate(Kitchen_sink_MSSS_Prev = ifelse(Kitchen_sink_MSSS_enroll == 'Yes' |
                                           Kitchen_sink_MSSS_1month == 'Yes' |
                                           Kitchen_sink_MSSS_3month == 'Yes' |
                                           Kitchen_sink_MSSS_6month == 'Yes' |
                                           Kitchen_sink_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_sink_MSSS_Prev = as.factor(Kitchen_sink_MSSS_Prev)) %>% 
  mutate(Refrigerator_MSSS_Prev = ifelse(Refrigerator_MSSS_enroll == 'Yes' |
                                           Refrigerator_MSSS_1month == 'Yes' |
                                           Refrigerator_MSSS_3month == 'Yes' |
                                           Refrigerator_MSSS_6month == 'Yes' |
                                           Refrigerator_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Refrigerator_MSSS_Prev = as.factor(Refrigerator_MSSS_Prev)) %>% 
  mutate(Microwave_MSSS_Prev = ifelse(Microwave_MSSS_enroll == 'Yes' |
                                        Microwave_MSSS_1month == 'Yes' |
                                        Microwave_MSSS_3month == 'Yes' |
                                        Microwave_MSSS_6month == 'Yes' |
                                        Microwave_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Microwave_MSSS_Prev = as.factor(Microwave_MSSS_Prev)) %>% 
  mutate(Oven_MSSS_Prev = ifelse(Oven_MSSS_enroll == 'Yes' |
                                   Oven_MSSS_1month == 'Yes' |
                                   Oven_MSSS_3month == 'Yes' |
                                   Oven_MSSS_6month == 'Yes' |
                                   Oven_MSSS_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Oven_MSSS_Prev = as.factor(Oven_MSSS_Prev)) %>% 
  mutate(TV_Remote_MSSD_Prev = ifelse(TV_Remote_MSSD_enroll == 'Yes' |
                                        TV_Remote_MSSD_1month == 'Yes' |
                                        TV_Remote_MSSD_3month == 'Yes' |
                                        TV_Remote_MSSD_6month == 'Yes' |
                                        TV_Remote_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(TV_Remote_MSSD_Prev = as.factor(TV_Remote_MSSD_Prev)) %>% 
  mutate(Sofa_MSSD_Prev = ifelse(Sofa_MSSD_enroll == 'Yes' |
                                   Sofa_MSSD_1month == 'Yes' |
                                   Sofa_MSSD_3month == 'Yes' |
                                   Sofa_MSSD_6month == 'Yes' |
                                   Sofa_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Sofa_MSSD_Prev = as.factor(Sofa_MSSD_Prev)) %>% 
  mutate(Telephone_MSSD_Prev = ifelse(Telephone_MSSD_enroll == 'Yes' |
                                        Telephone_MSSD_1month == 'Yes' |
                                        Telephone_MSSD_3month == 'Yes' |
                                        Telephone_MSSD_6month == 'Yes' |
                                        Telephone_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Telephone_MSSD_Prev = as.factor(Telephone_MSSD_Prev)) %>% 
  mutate(Comp_Keyboard_MSSD_Prev = ifelse(Comp_Keyboard_MSSD_enroll == 'Yes' |
                                            Comp_Keyboard_MSSD_1month == 'Yes' |
                                            Comp_Keyboard_MSSD_3month == 'Yes' |
                                            Comp_Keyboard_MSSD_6month == 'Yes' |
                                            Comp_Keyboard_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Comp_Keyboard_MSSD_Prev = as.factor(Comp_Keyboard_MSSD_Prev)) %>% 
  mutate(Tablet_MSSD_Prev = ifelse(Tablet_MSSD_enroll == 'Yes' |
                                     Tablet_MSSD_1month == 'Yes' |
                                     Tablet_MSSD_3month == 'Yes' |
                                     Tablet_MSSD_6month == 'Yes' |
                                     Tablet_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Tablet_MSSD_Prev = as.factor(Tablet_MSSD_Prev)) %>% 
  mutate(Door_Out_MSSD_Prev = ifelse(Door_Out_MSSD_enroll == 'Yes' |
                                       Door_Out_MSSD_1month == 'Yes' |
                                       Door_Out_MSSD_3month == 'Yes' |
                                       Door_Out_MSSD_6month == 'Yes' |
                                       Door_Out_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Door_Out_MSSD_Prev = as.factor(Door_Out_MSSD_Prev)) %>% 
  mutate(Bathroom_light_MSSD_Prev = ifelse(Bathroom_light_MSSD_enroll == 'Yes' |
                                             Bathroom_light_MSSD_1month == 'Yes' |
                                             Bathroom_light_MSSD_3month == 'Yes' |
                                             Bathroom_light_MSSD_6month == 'Yes' |
                                             Bathroom_light_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_light_MSSD_Prev = as.factor(Bathroom_light_MSSD_Prev)) %>% 
  mutate(Bathroom_sink_MSSD_Prev = ifelse(Bathroom_sink_MSSD_enroll == 'Yes' |
                                            Bathroom_sink_MSSD_1month == 'Yes' |
                                            Bathroom_sink_MSSD_3month == 'Yes' |
                                            Bathroom_sink_MSSD_6month == 'Yes' |
                                            Bathroom_sink_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Bathroom_sink_MSSD_Prev = as.factor(Bathroom_sink_MSSD_Prev)) %>% 
  mutate(Toilet_MSSD_Prev = ifelse(Toilet_MSSD_enroll == 'Yes' |
                                     Toilet_MSSD_1month == 'Yes' |
                                     Toilet_MSSD_3month == 'Yes' |
                                     Toilet_MSSD_6month == 'Yes' |
                                     Toilet_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Toilet_MSSD_Prev = as.factor(Toilet_MSSD_Prev)) %>% 
  mutate(Kitchen_light_MSSD_Prev = ifelse(Kitchen_light_MSSD_enroll == 'Yes' |
                                            Kitchen_light_MSSD_1month == 'Yes' |
                                            Kitchen_light_MSSD_3month == 'Yes' |
                                            Kitchen_light_MSSD_6month == 'Yes' |
                                            Kitchen_light_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_light_MSSD_Prev = as.factor(Kitchen_light_MSSD_Prev)) %>% 
  mutate(Kitchen_sink_MSSD_Prev = ifelse(Kitchen_sink_MSSD_enroll == 'Yes' |
                                           Kitchen_sink_MSSD_1month == 'Yes' |
                                           Kitchen_sink_MSSD_3month == 'Yes' |
                                           Kitchen_sink_MSSD_6month == 'Yes' |
                                           Kitchen_sink_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Kitchen_sink_MSSD_Prev = as.factor(Kitchen_sink_MSSD_Prev)) %>% 
  mutate(Refrigerator_MSSD_Prev = ifelse(Refrigerator_MSSD_enroll == 'Yes' |
                                           Refrigerator_MSSD_1month == 'Yes' |
                                           Refrigerator_MSSD_3month == 'Yes' |
                                           Refrigerator_MSSD_6month == 'Yes' |
                                           Refrigerator_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Refrigerator_MSSD_Prev = as.factor(Refrigerator_MSSD_Prev)) %>% 
  mutate(Microwave_MSSD_Prev = ifelse(Microwave_MSSD_enroll == 'Yes' |
                                        Microwave_MSSD_1month == 'Yes' |
                                        Microwave_MSSD_3month == 'Yes' |
                                        Microwave_MSSD_6month == 'Yes' |
                                        Microwave_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>%
  mutate(Microwave_MSSD_Prev = as.factor(Microwave_MSSD_Prev)) %>% 
  mutate(Oven_MSSD_Prev = ifelse(Oven_MSSD_enroll == 'Yes' |
                                   Oven_MSSD_1month == 'Yes' |
                                   Oven_MSSD_3month == 'Yes' |
                                   Oven_MSSD_6month == 'Yes' |
                                   Oven_MSSD_9month == 'Yes', 'Colonized', 'Not Colonized')) %>% 
  mutate(Oven_MSSD_Prev = as.factor(Oven_MSSD_Prev)) %>% 
  mutate(sofa_clean_never = ifelse(Clean_Sofa_Never_enroll == 'Yes' |
                                     Clean_Sofa_Never_Follow_1month == 'Yes' |
                                     Clean_Sofa_Never_Follow_3month == 'Yes' | 
                                     Clean_Sofa_Never_Follow_6month == 'Yes' |
                                     Clean_Sofa_Never_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(sofa_clean_never = as.factor(sofa_clean_never)) %>% 
  mutate(sofa_clean_0_1 = ifelse(Clean_Sofa_0_1_enroll == 'Yes' |
                                     Clean_Sofa_0_1_Follow_1month == 'Yes' |
                                     Clean_Sofa_0_1_Follow_3month == 'Yes' | 
                                     Clean_Sofa_0_1_Follow_6month == 'Yes' |
                                     Clean_Sofa_0_1_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(sofa_clean_0_1 = as.factor(sofa_clean_0_1)) %>% 
  mutate(sofa_clean_1_3 = ifelse(Clean_Sofa_1_3_enroll == 'Yes' |
                                   Clean_Sofa_1_3_Follow_1month == 'Yes' |
                                   Clean_Sofa_1_3_Follow_3month == 'Yes' | 
                                   Clean_Sofa_1_3_Follow_6month == 'Yes' |
                                   Clean_Sofa_1_3_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(sofa_clean_1_3 = as.factor(sofa_clean_1_3)) %>% 
  mutate(sofa_clean_4 = ifelse(Clean_Sofa_4_enroll == 'Yes' |
                                   Clean_Sofa_4_Follow_1month == 'Yes' |
                                   Clean_Sofa_4_Follow_3month == 'Yes' | 
                                   Clean_Sofa_4_Follow_6month == 'Yes' |
                                   Clean_Sofa_4_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(sofa_clean_4 = as.factor(sofa_clean_4)) %>% 
  mutate(sofa_clean_higherthan4 = ifelse(Clean_Sofa_higherthan4_enroll == 'Yes' |
                                   Clean_Sofa_higherthan4_Follow_1month == 'Yes' |
                                   Clean_Sofa_higherthan4_Follow_3month == 'Yes' | 
                                   Clean_Sofa_higherthan4_Follow_6month == 'Yes' |
                                   Clean_Sofa_higherthan4_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(sofa_clean_higherthan4 = as.factor(sofa_clean_higherthan4)) %>% 
  mutate(sofa_clean_unsure = ifelse(Clean_Sofa_Unsure_enroll == 'Yes' |
                                   Clean_Sofa_Unsure_Follow_1month == 'Yes' |
                                   Clean_Sofa_Unsure_Follow_3month == 'Yes' | 
                                   Clean_Sofa_Unsure_Follow_6month == 'Yes' |
                                   Clean_Sofa_Unsure_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(sofa_clean_unsure = as.factor(sofa_clean_unsure)) %>% 
  mutate(bath_shower_less1perweek = ifelse(Bath_shower_less1perweek_enroll == 'Yes' |
                                           Bath_shower_less1perweek_Follow_1month == 'Yes' |
                                           Bath_shower_less1perweek_Follow_3month == 'Yes' | 
                                           Bath_shower_less1perweek_Follow_6month == 'Yes' |
                                           Bath_shower_less1perweek_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(bath_shower_less1perweek = as.factor(bath_shower_less1perweek)) %>%
  mutate(bath_shower_1_2perweek = ifelse(Bath_shower_1_2perweek_enroll == 'Yes' |
                                           Bath_shower_1_2perweek_Follow_1month == 'Yes' |
                                           Bath_shower_1_2perweek_Follow_3month == 'Yes' | 
                                           Bath_shower_1_2perweek_Follow_6month == 'Yes' |
                                           Bath_shower_1_2perweek_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(bath_shower_1_2perweek = as.factor(bath_shower_1_2perweek)) %>%
  mutate(bath_shower_3_4perweek = ifelse(Bath_shower_3_4perweek_enroll == 'Yes' |
                                           Bath_shower_3_4perweek_Follow_1month == 'Yes' |
                                           Bath_shower_3_4perweek_Follow_3month == 'Yes' | 
                                           Bath_shower_3_4perweek_Follow_6month == 'Yes' |
                                           Bath_shower_3_4perweek_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(bath_shower_3_4perweek = as.factor(bath_shower_3_4perweek)) %>%
  mutate(bath_shower_5_6perweek = ifelse(Bath_shower_5_6perweek_enroll == 'Yes' |
                                           Bath_shower_5_6perweek_Follow_1month == 'Yes' |
                                           Bath_shower_5_6perweek_Follow_3month == 'Yes' | 
                                           Bath_shower_5_6perweek_Follow_6month == 'Yes' |
                                           Bath_shower_5_6perweek_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(bath_shower_5_6perweek = as.factor(bath_shower_5_6perweek)) %>%
  mutate(bath_shower_1perday = ifelse(Bath_shower_1perday_enroll == 'Yes' |
                                           Bath_shower_1perday_Follow_1month == 'Yes' |
                                           Bath_shower_1perday_Follow_3month == 'Yes' | 
                                           Bath_shower_1perday_Follow_6month == 'Yes' |
                                           Bath_shower_1perday_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(bath_shower_1perday = as.factor(bath_shower_1perday)) %>%
  mutate(bath_shower_2ormoreperday = ifelse(Bath_shower_2ormoreperday_enroll == 'Yes' |
                                           Bath_shower_2ormoreperday_Follow_1month == 'Yes' |
                                           Bath_shower_2ormoreperday_Follow_3month == 'Yes' | 
                                           Bath_shower_2ormoreperday_Follow_6month == 'Yes' |
                                           Bath_shower_2ormoreperday_Follow_9month == 'Yes', 'Yes', 'No')) %>%
  mutate(bath_shower_2ormoreperday = as.factor(bath_shower_2ormoreperday)) %>%
  mutate(Dog = ifelse(Type_of_Pet == 'Dog', 1, 0)) %>% 
  mutate(Dog = as.factor(Dog)) %>% 
  mutate(Cat = ifelse(Type_of_Pet == 'Cat', 1, 0)) %>% 
  mutate(Cat = as.factor(Cat)) %>% 
  mutate(long_fur = ifelse(Pet_Hair == 'Long', 1, 0)) %>% 
  mutate(long_fur = as.factor(long_fur)) %>% 
  mutate(short_fur = ifelse(Pet_Hair == 'Short', 1, 0)) %>% 
  mutate(short_fur = as.factor(short_fur)) %>% 
  mutate(indoor_pet = ifelse(Pet_In_or_Out == 'Indoors', 1, 0)) %>% 
  mutate(indoor_pet = as.factor(indoor_pet)) %>% 
  mutate(outdoor_pet = ifelse(Pet_In_or_Out == 'Outdoors', 1, 0)) %>% 
  mutate(outdoor_pet = as.factor(outdoor_pet))

ada.household.wide <- wide.ada.clean %>% 
  drop_na(Have_Pets)
ada.pets.wide <- wide.ada.clean %>% 
  drop_na(Type_of_Pet)
