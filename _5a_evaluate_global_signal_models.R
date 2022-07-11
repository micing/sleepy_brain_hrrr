library("tidyverse")
library("lme4")
#library(magrittr)
# library(tibbletime)
# library(foreach)
# library(doParallel)

#load("./data/hr_resp_data.RDta")

eyes_closed_time = 150
max_displacement = 0.3

load("data/global_signal_data.RDta")

d = data %>% select(id, session, sleepy, time, framewise_displacement, global_signal) %>%
  mutate(global_signal_log = log(global_signal)) %>%
  group_by(id, sleepy) %>% mutate(mean = mean(global_signal), mean_log = mean(global_signal_log)) %>% ungroup() %>%
  mutate(res = abs(global_signal - mean), res_log = abs(global_signal_log - mean_log)) %>%
  mutate(sleepy = factor(sleepy, c(0,1), c("full sleep", "sleep deprived"))) %>%
  mutate(eyes_closed = (time < eyes_closed_time)*1) %>%
  filter(time > 1) %>% mutate(fd = as.numeric(framewise_displacement)) %>%
  filter(fd < max_displacement)

# TODO
# Mixed model, kolla om "res" påverkas av "time"
# Lägg till designfaktor ögonen öppna (1:a 5 min) eller slutna (följande 15 min)
# Kontrollera om hjärt- och andningsfrekvens förklarar varians

# hist(d$res)
# hist(log(d$res))

m0 = lmer( global_signal ~ fd + ( 1 | id), data = d, REML = FALSE)
m1 = lmer( global_signal ~ time + fd + ( 1 | id), data = d, REML = FALSE)
m2 = lmer( global_signal ~ time + sleepy + fd + ( 1 + sleepy | id), data = d , REML = FALSE)
m3 = lmer( global_signal ~ time*sleepy + fd + ( 1 + sleepy | id), data = d, REML = FALSE)
m4 = lmer( global_signal ~ time*sleepy + fd + eyes_closed + ( 1 + sleepy | id), data = d , REML = FALSE)
m5 = lmer( global_signal ~ time*sleepy + fd + eyes_closed*time + ( 1 + sleepy | id), data = d , REML = FALSE)
m6 = lmer( global_signal ~ time*sleepy + fd + eyes_closed*time + eyes_closed*sleepy + ( 1 + sleepy | id), data = d , REML = FALSE)
m7 = lmer( global_signal ~ time*sleepy*eyes_closed + fd  + ( 1 + sleepy | id), data = d , REML = FALSE)
m8 = lmer( global_signal ~ time*sleepy + fd + I(time^2) + ( 1 + sleepy | id), data = d , REML = FALSE)
m9 = lmer( global_signal ~ time*sleepy + fd + I(time^2) + eyes_closed + ( 1 + sleepy | id), data = d , REML = FALSE)
m10 = lmer( global_signal ~ time*sleepy + fd + I(time^2) + eyes_closed*sleepy  + ( 1 + sleepy | id), data = d , REML = FALSE)
m11 = lmer( global_signal ~ time*sleepy + fd + I(time^2)*eyes_closed + eyes_closed*sleepy  + ( 1 + sleepy | id), data = d , REML = FALSE)

anova(m0, m1)
anova(m1, m2)
anova(m2, m3)
anova(m3, m4)
anova(m4, m5)
anova(m5, m6)
anova(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)

d$pred_fe = predict(m11, re.form=NA)
d$pred_re = predict(m11, re.form=NULL)
d$abs_res = abs(d$pred_re-d$global_signal)

p = d %>% ggplot(aes(x=time, y=pred_fe)) +
  geom_line(size=.1) +
  facet_wrap(vars(sleepy)) +
  theme_minimal() +
  ggtitle(paste0("Predicted global signal"))
plot(p)

mv1 = lmer( abs_res ~ time*sleepy + fd + I(time^2)*eyes_closed + eyes_closed*sleepy  + ( 1 + sleepy | id), data = d , REML = FALSE)
