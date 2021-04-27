### Preamble ###
# Purpose: Process and save data for use with shiny app
# Author: Amy Farrow
# Contact: amy.farrow@mail.utoronto.ca
# Date: 2021-04-25
# Pre-requisites: 01_import_data.R
# to-dos:

expdata <- isrdata %>%
  filter(t1_consent == 1 # Only include if instructor consented to first survey
         & t1_complete == 1 # Only included if instructor completed first survey
         & t1_timer_consent_3 > 1 # Only include if instructor read consent
         & s1_consent == 1 # Only include if student consented to first survey
         & s1_timer_consent_3 > 1 # Only include if student read consent
         & s1_fullsurvey == 1 # Only include if the student completed the whole first survey
         & t2_complete == 1 # Only include if the instructor completed the whole second survey
         # The rest of the lines make sure that the survey taker looked at each page for more than three seconds
         & t1_timer_gtky1_3 > 10
         & t1_timer_gtky2_3 > 10
         & t1_timer_gtky3_3 > 10
         & t1_timer_demos1_3 > 10
         & t1_timer_demos2_3 > 10
         & s1_timer_gtky1_3 > 10
         & s1_timer_gtky2_3 > 10
         & s1_timer_gtky3_3 > 10
         & s1_timerpre1_3 > 10
         & s1_timerpre2_3 > 10
         & (s1_timercontrolfeedback_3 > 10 | is.na(s1_timercontrolfeedback_3))
         & (s1_timercontrolresponse_3 > 10 | is.na(s1_timercontrolresponse_3))
         & (s1_timertreatmentfeedba_3 > 10 | is.na(s1_timertreatmentfeedba_3))
         & (s1_timertreatmentrespon_3 > 10 | is.na(s1_timertreatmentrespon_3))
         & s1_timerpost1_3 > 10
         & s1_timerpost2_3 > 10
         & s1_timerdemo1_3 > 10
         & (s2_timer1a_3 > 10 | is.na(s2_timer1a_3))
         & (s2_timer1b_3 > 10 | is.na(s2_timer1b_3))
         & (s2_timer2a_3 > 10 | is.na(s2_timer2a_3))
         & (s2_timer3a_3 > 10 | is.na(s2_timer3a_3))
         & (s2_timer3b_3 > 10 | is.na(s2_timer3b_3))
         & (s2_timer3c_3 > 10 | is.na(s2_timer3c_3))
         & (s2_timer4_3 > 10 | is.na(s2_timer4_3))
         & t2_timer > 10
         & t_drop_reasons != "Teacher administered GTKY survey mistakenly at end of semester"
         & t_drop_reasons != "Taught online course"
         & t_drop_reasons != "Administered to only graduate students"
  )

expdata <- expdata %>%
  mutate(s1_female = ifelse(s1_female == -99, NA, s1_female),
         t_female = ifelse(t_female == -99, NA, t_female)) %>% # Some NAs are shown as -99. Replace with NA.
  mutate(teacherid = as.factor(teacherid),
         f17_enrolled = as.factor(f17_enrolled),
         s1_female = as.factor(s1_female),
         t_female = as.factor(t_female))

expdata <- expdata %>%
  mutate(t1_race_1 = replace_na(as.numeric(t1_race_1), 0),
         t1_race_2 = replace_na(as.numeric(t1_race_2), 0),
         t1_race_3 = replace_na(as.numeric(t1_race_3), 0),
         t1_race_4 = replace_na(as.numeric(t1_race_4), 0),
         t1_race_5 = replace_na(as.numeric(t1_race_5), 0),
         t1_race_6 = replace_na(as.numeric(t1_race_6), 0),
         t1_race_7 = replace_na(as.numeric(t1_race_7), 0)) %>%
  mutate(t_race = ifelse(t1_race_1 + t1_race_2 + t1_race_3 + t1_race_4 + t1_race_5 + t1_race_6 + t1_race_7 > 1, 
                         "Mixed race/ethnicity", 
                         ifelse(t1_race_1 + t1_race_2 + t1_race_3 + t1_race_4 + t1_race_5 + t1_race_6 + t1_race_7 < 1, "Unknown",
                                ifelse(t1_race_1 == 1, "White/Caucasian",
                                       ifelse(t1_race_2 == 1, "Black or African American",
                                              ifelse(t1_race_3 == 1, "Hispanic American or Latino/a",
                                                     ifelse(t1_race_4, "Asian or Pacific Islander",
                                                            ifelse(t1_race_5, "American Indian or Alaskan",
                                                                   ifelse(t1_race_6, "Middle Eastern",
                                                                          "Other Race")))))))))

expdata <- expdata %>%
  mutate(s1_race_1 = replace_na(as.numeric(s1_race_1), 0),
         s1_race_2 = replace_na(as.numeric(s1_race_2), 0),
         s1_race_3 = replace_na(as.numeric(s1_race_3), 0),
         s1_race_4 = replace_na(as.numeric(s1_race_4), 0),
         s1_race_5 = replace_na(as.numeric(s1_race_5), 0),
         s1_race_6 = replace_na(as.numeric(s1_race_6), 0),
         s1_race_7 = replace_na(as.numeric(s1_race_7), 0)) %>%
  mutate(s_race = ifelse(s1_race_1 + s1_race_2 + s1_race_3 + s1_race_4 + s1_race_5 + s1_race_6 + s1_race_7 > 1,
                         "Mixed race/ethnicity", 
                         ifelse(s1_race_1 + s1_race_2 + s1_race_3 + s1_race_4 + s1_race_5 + s1_race_6 + s1_race_7 < 1, "Unknown",
                                ifelse(s1_race_1 == 1, "White/Caucasian",
                                       ifelse(s1_race_2 == 1, "Black or African American",
                                              ifelse(s1_race_3 == 1, "Hispanic American or Latino/a",
                                                     ifelse(s1_race_4, "Asian or Pacific Islander",
                                                            ifelse(s1_race_5, "American Indian or Alaskan",
                                                                   ifelse(s1_race_6, "Middle Eastern",
                                                                          "Other Race"))))))))) %>%
  mutate(s_hisp_black = ifelse((s1_race_2 == 1 | s1_race_3 == 1), 1, 0))

expdata <- expdata %>%
  mutate(t2_finalexamobj = ifelse(obj_exam == 1, t2_finalexam, NA))

ddata <- expdata %>%
  dplyr::select(treatment,
                s1_sim, s1_tsr, 
                s2_sim, s2_tsr, 
                t2_sim1, t2_tsr,
                grade, std_grade, 
                t2_finalexam, f17_enrolled,
                obj_exam,
                s_firstgen,
                s_hisp_black,
                year,
                s_firstgen) %>%
  mutate(all = 1)

write_csv(ddata, here("shiny/ddata.csv"))