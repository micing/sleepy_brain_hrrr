library(tidyverse)
library(magrittr)
library(tibbletime)
library(foreach)
library(doParallel)
#library(data.table)

path_hrd = "./data/data_SB2_pilots_forMicke_2019-04-25/PPG/"
path_rrd = "./data/data_SB2_pilots_forMicke_2019-04-25/Resp/"
randomization_file = "./data/design/randomizationlist_sleepybrain2pilots.csv"

#### Setup parallel computing ####

# cl=makeForkCluster(max(1, detectCores()-1), outfile="")
# registerDoParallel(cl)

#### Definitions ####

hr_sampling_rate = 100
rr_sampling_rate = 25
hr_summary_rate=seq(1,50,1)*hr_sampling_rate*30
rr_summary_rate=seq(1,50,1)*rr_sampling_rate*30

### Data quality check ###

# Quality control based on visual inspection by Eileen
# Reject the following recordings

remove_heartbeats=tribble(
  ~id, ~session,
  8001,    2,  # The whole file showed heart rate ~ 200-1400 bpm and periodic artifacts
  8052,    3,  # The whole file showed heart rate ~ 200-1400 bpm and periodic artifacts
  8016,    2)  # Most of the recording showed heart rate < 30 - deemed unrealistic

remove_respiration=tribble(
  ~id, ~session,
  8078,    3,  # Odd readings, likely due to missing data
  8064,    2,  # Missing data
  8064,    3)  # Missing data

#### NOTE: more screening is needed before further processing (see descriptives)

#### read data ####

r=read_csv2(randomization_file) %>% 
  rename(randomization="order_1fullsleepfirst_2sleepdeprivedfirst") 

heart=tibble()
for (f in list.files(path_hrd)) {
  d=read_tsv(paste0(path_hrd, f)) %>% 
    rename(epoch=cardiac) %>% 
    mutate(id=as.numeric(substr(f,5,8)), 
           session=as.numeric(substr(f,14,14)),
           time=epoch/hr_sampling_rate) %>%
    left_join(r, by="id") %>% 
    mutate(sleepy=(session==1 & randomization==2 | session==2 & randomization==1)*1) %>%
    arrange(time) %>%
    mutate(time_delta=time-lag(time),
           bpm=60/(time_delta),
           bpm_auc_epoch=time_delta*(bpm+lag(bpm))/2,  # trapezoidal rule
           bpm_auc_1sec = bpm_auc_epoch/time_delta)  %>%
    select(id, session, sleepy, time, epoch, starts_with("bpm"))
  heart = rbind(heart, d)  
}

resp=tibble()
for (f in list.files(path_rrd)) {
  d=read_tsv(paste0(path_rrd, f)) %>% 
    rename(epoch=respiratory) %>% 
    mutate(id=as.numeric(substr(f,5,8)), 
           session=as.numeric(substr(f,14,14)),
           time=epoch/rr_sampling_rate) %>%
    left_join(r, by="id") %>% 
    mutate(sleepy=(session==1 & randomization==2 | session==2 & randomization==1)*1) %>%
    arrange(time) %>%
    mutate(time_delta=time-lag(time),
           bpm=60/(time_delta),
           bpm_auc_epoch=time_delta*(bpm+lag(bpm))/2,  # trapezoidal rule
           bpm_auc_1sec = bpm_auc_epoch/time_delta)  %>%
    select(id, session, sleepy, time, epoch, starts_with("bpm"))
  resp = rbind(resp, d)  
}

### Functions ####

rolling_summary = function(data, window, summary_rate) {
  
  mean_fun = rollify(function(x) {mean(x, na.rm=T)}, window=window)
  sd_fun= rollify(function(x) {sd(x, na.rm=T)}, window=window)
  min_fun = rollify(function(x) {min(x, na.rm=T)}, window=window)
  max_fun = rollify(function(x) {max(x, na.rm=T)}, window=window)
  sum_fun = rollify(function(x) {sum(x, na.rm=T)}, window=window)
  sd_fun_auc = rollify(function(x) {sd(x, na.rm=F)}, window=window) # this is sd of means (auc) and should not remove NA
  
  iss = data  %>% select(id, session, sleepy) %>% distinct() 

  d2=foreach(i = 1:dim(iss)[1], .combine=rbind, .packages = c('tidyverse', 'data.table')) %dopar% {
    d = data %>% filter(id==iss$id[i], session==iss$session[i]) 
    print(paste("Currently working on: id =", iss$id[i], "session =", iss$session[i]))
    tibble(id=iss$id[i], session=iss$session[i], sleepy=iss$sleepy[i], epoch=seq(1, max(d$epoch))) %>%
      full_join(d, by = c("id", "session", "sleepy", "epoch")) %>%
      mutate(bpm_fill = bpm) %>%
      fill(bpm_fill) %>%
      mutate(mean_30s=mean_fun(bpm),
             sd_30s=sd_fun(bpm),
             mean_fill_30s=mean_fun(bpm_fill),
             sd_fill_30s=sd_fun(bpm_fill),
             auc_30s=sum_fun(bpm_auc_epoch)/(max_fun(time)-min_fun(time)),
             auc_sd_30s=sd_fun_auc(auc_30s)) %>%
      select(-bpm_fill)
  } 
  list(observed=data %>% left_join(d2) %>% arrange(id, session, epoch), 
       summary30s=d2 %>% filter(epoch %in% summary_rate) %>% 
         arrange(id, session, epoch) %>%
         select(id, session, sleepy, time, epoch, ends_with("30s")))
}

### Calculate heart rate and respiratory rates over 30s ####

data=list()

# NB: Something (bug?) prevents me from calling rolling_summary() with the rr_ and hr_sampling_rate variables
data$respiration = resp %>% rolling_summary(window=750, summary_rate=rr_summary_rate) 
data$respiration$observed_clean = data$respiration$observed %>% anti_join(remove_respiration)
data$respiration$summary30s %<>% mutate(time=epoch/rr_sampling_rate)
data$respiration$summary30s_clean = data$respiration$summary30s %>% 
  mutate(time=epoch/rr_sampling_rate) %>%
  anti_join(remove_respiration)

data$heartrate = heart %>% rolling_summary(window=3000, summary_rate=hr_summary_rate)
data$heartrate$observed_clean = data$heartrate$observed %>% anti_join(remove_heartbeats)
data$heartrate$summary30s %<>% mutate(time=epoch/hr_sampling_rate)
data$heartrate$summary30s_clean = data$heartrate$summary30s %>% 
  mutate(time=epoch/hr_sampling_rate) %>%
  anti_join(remove_heartbeats)

stopCluster(cl)
save(data, file="./data/hr_resp_data.RDta", compress=T)
