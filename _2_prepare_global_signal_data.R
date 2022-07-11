library("tidyverse")
#library("lme4")

path = "data/SB2_3_resting_state_regressors/"
path1 = "data/design/"

randomization_file = "randomizationlist_sleepybrain2pilots.csv"
randomization_file2 = "randomization_list.csv"

r=read_csv2(paste0(path1, randomization_file)) %>%
  rename(randomization="order_1fullsleepfirst_2sleepdeprivedfirst") %>%
  bind_rows(read_csv(paste0(path1, randomization_file2)) %>%
              separate(id, c("stub", "id"), sep="_", convert=TRUE) %>%
              mutate(randomization = case_when(first_condition == "fullsleep" ~ 1,
                                               first_condition == "sleepdeprived" ~ 2)) %>%
              select(id, randomization)) %>% arrange(id)

data=tibble()
for (f in list.files(path)) {
  d=read_tsv(paste0(path, f)) %>% rename_all(tolower) %>%
    mutate(id1=as.numeric(substr(f,5,6)),
           id2=as.numeric(substr(f,5,7)),
           session1=as.numeric(substr(f,12,12)),
           session2=as.numeric(substr(f,13,13))) %>%
    mutate(id = case_when(is.na(session2) ~ id1, !is.na(session2) ~id2),
           session = case_when(is.na(session2) ~ session1 - 1, !is.na(session2) ~ session2 - 1)) %>%
    group_by(id, session) %>% mutate(time=row_number()) %>% ungroup() %>%
    left_join(r, by=c("id")) %>%
    mutate(sleepy=(session==1 & randomization==2 | session==2 & randomization==1)*1) %>%
    select(id, -id1, -id2, session, sleepy, time, everything())

  data = data %>% bind_rows(d)
}

save(data, file="data/global_signal_data.RDta")



  
