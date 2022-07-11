library("tidyverse")

load("./data/global_signal_data.RDta")

max_displacement = 0.2

#### prepare data ####
d = data %>% select(id, sleepy, time, global_signal, framewise_displacement) %>% 
  mutate(framewise_displacement = as.numeric(framewise_displacement)) %>% filter(!is.na(framewise_displacement)) %>%
  mutate(sleepy = factor(sleepy, c(0,1), c("full sleep", "sleep deprived"))) %>%
  mutate(filter = (framewise_displacement > max_displacement)*1) %>%
  mutate(global_signal_clean = case_when(filter == 0 ~ global_signal),
         global_signal_displacement = case_when(filter == 1 ~ global_signal))

#### plots
p = d %>% filter(framewise_displacement < 1) %>% ggplot(aes(x=framewise_displacement)) +
  geom_histogram() +
  theme_minimal()
plot(p)

p = d %>% ggplot(aes(x=time, y=global_signal, group=id)) +
  geom_line(size=0.1) +
  facet_wrap(vars(sleepy)) +
  theme_minimal() +
  ggtitle("Global signal (all data)")
plot(p)

p = d %>% ggplot(aes(x=time, y=global_signal_clean, group=id)) +
  geom_line(size=0.1) +
  geom_line(aes(y=global_signal_displacement, color="red")) +
  facet_wrap(vars(sleepy)) +
  theme_minimal() +
  ggtitle(paste0("Global signal cleaned, framewise displacement < ", max_displacement))
plot(p)

p = d %>% ggplot(aes(x=time, y=global_signal_clean, group=id)) +
  geom_line(size=0.1) +
  facet_wrap(vars(sleepy)) +
  theme_minimal() +
  ggtitle(paste0("Global signal cleaned, framewise displacement < ", max_displacement))
plot(p)



