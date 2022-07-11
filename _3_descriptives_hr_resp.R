library(tidyverse)


load("./data/hr_resp_data.RDta")

d=data$heartrate$observed_clean

baseplot = function(d, title="") {
  ggplot(d) +
    aes(x=time, y=bpm, color="red") +
    geom_line() +
    geom_line(aes(y=bpm_filter), color="black") +
    labs(title=title) +
    scale_y_continuous(trans="log") +
    theme_minimal()
}

is= d %>% group_by(id, session) %>% summarize()

for (i in 1:dim(is)[1]) {
  baseplot(d %>% filter(id==is$id[i], session==is$session[i]) %>% 
           mutate(bpm_filter=replace(bpm, bpm>100 | bpm<30, NA)),
           title=paste0("Heartrate: id = ", is$id[i], ", session = ", is$session[i])) %>%
    plot()

}

d=data$respiration$observed_clean

is= d %>% group_by(id, session) %>% summarize()

for (i in 1:dim(is)[1]) {
  baseplot(d %>% filter(id==is$id[i], session==is$session[i]) %>% 
             mutate(bpm_filter=replace(bpm, bpm>30 | bpm<7, NA)),
           title=paste0("Respiration: id = ", is$id[i], ", session = ", is$session[i])) %>%
    plot()
  
}

# TODO: filter must be applied before any mean, rolling means and variability is calculated

