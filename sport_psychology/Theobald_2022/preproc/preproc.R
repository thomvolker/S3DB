# preprocessing lines 3-275 are directly copied from the provided open code files

### load packages ###
{
  library(tidyverse)
  library(magrittr)
  library(psych)
  library(lme4)
  library(lmerTest)
  library(readxl)
  library(DataCombine)
  library(rmcorr)
  library(lmerTest)
  library(ICC)
  library(sjstats)
  library(sjPlot)
  library(apaTables)
  library(viridis)
}

{
  #### load data ####
  
  load("sport_psychology/Theobald_2022/preproc/data.RData")
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=TRUE)
  detach(data)
  
}



{
  
  #### outlier screening & exclusion ####
  
  ## exclude participants who did not answer questions
  
  inspect_participants <- data %>%
    filter(!is.na(day)) %>%
    group_by(record_id) %>%
    summarise(mean_unique_qs = mean(n_unique_qs_shifted, na.rm = T),
              sum_unique_qs = sum(n_unique_qs_shifted, na.rm=T),
              max_day = max(day, na.rm=T)
    )
  length(which(inspect_participants$mean_unique_qs < 2 | inspect_participants$mean_unique_qs==NA))
  #7
  
  data %<>%
    filter(!record_id %in% (inspect_participants %>% filter(mean_unique_qs < 2 | is.nan(mean_unique_qs)) %>% pull(record_id)))
  
  ## exclude participants who did not report exam grades or who did not participate in mock exams
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=TRUE)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
  #filter 1 person who indicates exam perf of 0 and 14 persons without mock exam
  length(which(aggdata$performance_exam ==0)) #1
  length(which(aggdata$performance_exam =="NaN")) #17
  data<-filter(data, performance_exam>0 & performance_exam!="NaN")
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=TRUE)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
  length(which(aggdata$performance_mock_exams =="NaN")) #14
  data<-filter(data, performance_mock_exams!="NaN")
  
  #outliers: did not use amboss for exam preparation; 
  #169 (only 2044 qs in 100 days phase), 
  #487 answered only around 500 qs overall
  #489 answered only around 600 qs overall 
  data<-filter(data, record_id!=169 & record_id!=487 &record_id!=489 )
  
  
  # #suspiciously high n of qs and large drop mock -> exam; probably shared account
  
  data<-filter(data, record_id!=207 & record_id!=498 & record_id!=365 & record_id!=363 
               & record_id!=273 & record_id!=150)
  
  rm(inspect_participants)
}  
{
  
  
  #### data exclusion daily level #### 
  
  outlier_goalsetting <- ggplot(data=data, aes(x = "", y = goal_qs)) + geom_boxplot() +
    stat_boxplot(geom = "errorbar",
                 width = 0.4) +
    stat_summary(fun=mean, geom="point", shape=5, size=4) +
    theme(text = element_text(size=15),
          axis.text.x = element_text(hjust=1)) +
    ylab("Goal setting (no of questions)")+
    xlab("")
  plot(outlier_goalsetting)
  
  #filter observations with unrealistically high goals (>= 600) and observations where student did not set a goal (goal_qs = 0)
  
  length(which(data$goal_qs==0)) #k =47 
  length(which(data$goal_qs>600)) #3
  
  # time between pre-learning (bs) questionnaire and post-learning (as) questionnaire
  
  data$after_survey_time_stamp<- as.POSIXct(strptime(data$after_survey_time_stamp, format = "%d.%m.%Y %H:%M"), tz = "Europe/London")
  data$goal_setting_time_stamp<- as.POSIXct(strptime(data$goal_setting_time_stamp, format = "%d.%m.%Y %H:%M"), tz = "Europe/London")
  
  
  data %<>%
    mutate(tdiff_bs_as = as.numeric(difftime(after_survey_time_stamp, goal_setting_time_stamp, units=("hours"),
    )))
  plot_tdiff_bs_as <- ggplot(data) + geom_histogram(aes(x = tdiff_bs_as), 
                                                    fill = "gray", color = "black", bins = 100) +
    scale_x_continuous(breaks = seq(0, 35, 4)) +
    scale_y_continuous(breaks = seq(0, 350, 50)) +
    geom_vline(xintercept = 24, size = 2.5) +
    xlab("time between pre-learning (bs) questionnaire and post-learning (as)") +
    theme_classic() 
  plot(plot_tdiff_bs_as)
  
  
  data_obs <- filter(data, day!= "days_participated_arm_4" & day!= "invitation_arm_1" & day!= "invitation_arm_2" & day!= "invitation_arm_3" & day!= "trait_measure_1_arm_1" & day!= "trait_measure_1_arm_2" & day!= "trait_measure_1_arm_3" 
                     & day!= "trait_measure_2_arm_1" & day!= "trait_measure_2_arm_2" & day!= "trait_measure_2_arm_3")
  sum(is.na(data_obs$tdiff_bs_as))
  
  length(which(data$tdiff_bs_as>=24)) #k=77 : larger gap 
  sum(is.na(data_obs$tdiff_bs_as)) #k = 495: either before or after learning questionnaire is missing
  
  # View(data %>% select(record_id, day,after_survey_time_stamp, goal_setting_time_stamp,tdiff_bs_as
  # ))
  
  data<-data[ order( data$record_id, data$day ),  ]
  
  data<- data %<>%
    slide(Var="goal_setting_time_stamp", GroupVar="record_id", NewVar="lead.goal_setting_time_stamp", slideBy = +1, keepInvalid=TRUE, reminder=F)
  # time between post-learning (as) questionnaire and pre-learning (bs) questionnaire
  data %<>%
    mutate(tdiff_as_bs = as.numeric(difftime(lead.goal_setting_time_stamp,after_survey_time_stamp), units=("hours")))
  describe(data$tdiff_as_bs)
  length(which(data$tdiff_as_bs>24)) #k = 807 (of 9355); 9% of data
  
  rm(outlier_goalsetting,plot_tdiff_bs_as,data_obs)
  
}

{
  # compute gap between evening questionnaire and morning questionnaire
  data<-data[ order( data$record_id, data$day ),  ]
  
  data<- data %<>%
    slide(Var="goal_setting_time_stamp", GroupVar="record_id", NewVar="lead.goal_setting_time_stamp", slideBy = +1, keepInvalid=TRUE, reminder=F)
  # time between post-learning (as) questionnaire and pre-learning (bs) questionnaire
  data %<>%
    mutate(tdiff_as_bs = as.numeric(difftime(lead.goal_setting_time_stamp,after_survey_time_stamp), units=("hours")))
  describe(data$tdiff_as_bs)
  length(which(data$tdiff_as_bs>24)) #k = 807 (of 8771); 21% of data
  length(which(data$tdiff_as_bs<1)) #k = 141 (of 8771) ; 1% of data
  
  #### these filters were used for daily analyses ####
  #filter observations with unrealistically high goals (>= 600) and observations where student did not set a goal (goal_qs = 0)
  #filter(goal_qs < 600)
  #filter(goal_qs > 0)
  
  #filter observations where time span between "before" and "after" questionnaire exceeded 24 hours or was below 1 hour 
  #this was done to make sure that before and after questionnaire refer to the same day
  #filter(tdiff_bs_as < 25)
  #filter(tdiff_bs_as > 0)
  
  #for analysis where anxiety is predicted by prior performance:
  #include only consecutive days, i.e., filter observations where time span between "after" and next "before" questionnaire exceeded 24 hours or was below 1 hour 
  #filter(tdiff_as_bs < 25)
  #filter(tdiff_as_bs >0)
  
}

{
  #### generate scales // compute variables // centering #### 
  
  #test anxiety
  
  data<- data %>% 
    mutate(test_anxiety=rowMeans(.[ , c("anx_t1","anx_t2","anx_t3","anx_t4", "anx_t5")] , na.rm=TRUE))
  
  # % correctly sloved
  
  data %<>% 
    mutate(perc_correct = 1-((n_unique_qs_shifted-n_unique_qs_correct_shifted)/n_unique_qs_shifted))         
  
  
  #create lead variable for anxiety before learning
  data<- data %<>%
    slide(Var="anxiety_b", GroupVar="record_id", NewVar="lead.anxiety_b", slideBy = +1, keepInvalid=TRUE, reminder=F)
  
  # centering
  data %<>%
    group_by(record_id) %>%
    mutate(
      # person mean
      pm_working_memory = mean(working_memory_state, na.rm=T),
    ) %>%
    ungroup() %>%
    mutate(
      # grand mean
      gm_working_memory = mean(working_memory_state, na.rm=T),
      
      # grand mean centering
      working_memory_grand_mean_c = pm_working_memory - gm_working_memory)
  
  #omega 
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=TRUE)
  detach(data)
  
  omega_anxiety_t1 <- aggdata[c("anx_t1","anx_t2","anx_t3","anx_t4", "anx_t5")]
  omega(omega_anxiety_t1)
  
  
  rm(omega_anxiety_t1)
  
  #reliability state anxiety 
  
  is.even <- function(x) x %% 2 == 0
  data<- mutate(data, days2=ifelse(is.even(day), 2,1))
  
  split_half_rel_anx <- aggregate(data[,"anxiety_b"], by=list(data$record_id, data$days2), FUN=mean, na.rm=TRUE)
  names(split_half_rel_anx) <- c("record_id", "days", "anxiety_b")
  split_half_rel_anx <- split_half_rel_anx %>%
    group_by(record_id) %>%
    filter(n()>1)
  cor.test(subset(split_half_rel_anx, days==1)$anxiety_b, subset(split_half_rel_anx, days==2)$anxiety_b)
  # .97
  rm(split_half_rel_anx)
  
}

{
  
  #### descriptives #####
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=T)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
  describe(aggdata$age)  #25.90 SD = 3.25
  tabulate(aggdata$gender) #71% female
  
}
{

}



{
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=T)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
}  
  
dataset <- aggdata

dataset <- dataset |>
  dplyr::select(performance_exam, test_anxiety, performance_mock_exams, working_memory_trait, performance_exam_prepare, cg)


write_csv(dataset, "sport_psychology/Theobald_2022/dataset.csv")

rm(aggdata, data, data_obs, inspect_participants, omega_anxiety_t1, outlier_goalsetting, plot_tdiff_bs_as)

view(dataset)

