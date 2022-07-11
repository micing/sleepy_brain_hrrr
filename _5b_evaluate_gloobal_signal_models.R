library("tidyverse")
library("lme4")
library("ggpubr")

re = "sleepy"
fe = "qube"

random_formulas = list(
  null = " + (1 | id)",
  sleepy = " + ( 1 + sleepy | id)"
)

fixed_formulas = list(
  null = " ~ 1",
  m1  = " ~ time*sleepy + fd + eyes_closed*time + eyes_closed*sleepy ",
  full_threeway = " ~ time*sleepy*eyes_closed + fd ",
  square = " ~ time*sleepy + fd + I(time^2) ",
  qube = " ~ time*sleepy + fd + I(time^2) + I(time^3) ",
  square2 = " ~ time*sleepy + fd + I(time^2)*eyes_closed + eyes_closed*sleepy "
)

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

m1 = lmer(paste0("global_signal", fixed_formulas[[fe]], random_formulas[[re]]), data = d, REML = FALSE)

d$fd=mean(d$fd)
d$pred_fe = predict(m1, re.form=NA, newdata=d)
d$pred_re = predict(m1, re.form=NULL, newdata=d)
d$abs_res = abs(d$pred_re-d$global_signal)
d$res = d$pred_re-d$global_signal

m2 = lmer(paste0("abs_res", fixed_formulas[[fe]], random_formulas[[re]]), data = d, REML = FALSE)

d$pred_var_fe = predict(m2, re.form=NA, newdata=d)
d$pred_var_re = predict(m2, re.form=NULL, newdata=d)

#d1 = d %>% select(sleepy, time, pred_fe, pred_var_fe) %>% pivot_longer(names_to)

p1 = d %>% ggplot(aes(x=time, y=pred_fe)) +
  geom_line(size=1) +
  facet_wrap(vars(sleepy)) +
  theme_minimal() +
  ggtitle(paste0("Predicted global signal, model: ", fe))


p2 = d %>% ggplot(aes(x=time, y=pred_var_fe)) +
  geom_line(size=1) +
  facet_wrap(vars(sleepy)) +
  theme_minimal() +
  ggtitle(paste0("Predicted global signal variability, model: ", fe))

p3 = d %>% ggplot(aes(x=time, y=res)) +
  geom_point(size=.1) +
  facet_wrap(vars(sleepy)) +
  theme_minimal() +
  ggtitle(paste0("Global signal residuals, model: ", fe))

p4 = ggarrange(p1, p2,  ncol = 1, nrow = 2)
p5 = ggarrange(p2, p3,  ncol = 1, nrow = 2) 

plot(p4)
plot(p5)
          