## ---- pkgLoad ----

library(tidyverse)
library(lubridate)
library(gganimate)

#file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

## ---- readin ----
c0 <- read_rds("c0.rds")
c0
summary(c0)

wll <- read_rds("wll.rds")

# any non-missing turbs without l.level?

c0 %>% 
  filter(!is.na(turb.filtered) & is.na(l.level)) %>% 
  summary()

# how does that happen?

# not missing water.leve but recorded turbidity...

## ---- corTurbs ----

ggplot(c0, aes(turb.total, turb.filtered)) + geom_point(aes(colour = water.level)) + 
  geom_smooth(se = FALSE) + facet_wrap(~ lake, ncol = 1)

# that's odd, or does this mean than high total turb contains lotsa heavy stuff?

c0 %>% filter(turb.total > 250) %>% print(n = 30) # anything special about those dates?

## ---- dateSort ----

c1 <- 
  c0 %>% 
  group_by(lake) %>% 
  arrange(dt) %>%
  mutate(turb.part = turb.total - turb.filtered) %>% 
  ungroup()

c1

summary(c1)

ggplot(c1, aes(y = turb.filtered, x = turb.part)) + geom_point(aes(colour = water.level)) + 
  geom_smooth(se = FALSE) + facet_wrap(~ lake, ncol = 1)


## ---- tsPlot1 ----
c1 %>% 
  filter(yr > 1995) %>% 
  ggplot(., aes(dt, turb.filtered)) + geom_line() + geom_point() + facet_wrap(~ lake, ncol = 1)+
  scale_y_log10()

# standardise within year

c1 %>% 
  filter(yr > 1995) %>% 
  group_by(lake, w_yr) %>% 
  mutate(turb.filtered.mn = turb.filtered - median(turb.filtered, na.rm = TRUE)) %>% 
  #summary()
  ggplot(., aes(as.factor(w_yr), turb.filtered.mn)) + facet_wrap(~ lake, nrow = 2) + geom_boxplot() + 
  geom_jitter(width = 0.05, size = 2.5, aes(colour = mo_lab))

c1_std <- c1 %>% 
  filter(yr > 1995) %>% 
  group_by(lake, yr) %>% 
  summarise(turb.filtered.mn = mean(turb.filtered, na.rm = TRUE)) %>% 
  ungroup()

c1_std

sor1 <- 
c1 %>% 
  filter(yr > 1995) %>% 
  left_join(c1_std) %>% 
  mutate(turb.filtered.s = turb.filtered - turb.filtered.mn) %>% 
  filter(!is.na(turb.filtered) & lake == "Sorell") %>% 
  ungroup() %>% 
  arrange(dt)

 
ggplot(sor1, aes(dt, turb.filtered.s)) + geom_line() + geom_point(aes(size = water.level)) 

ggplot(sor1, aes(water.level, turb.filtered.s)) + geom_path(aes(colour = as.factor(w_yr))) + geom_point(aes(colour = as.factor(w_yr)))



## ---- phasePlots ----
c1 %>% 
  filter(!is.na(water.level) & yr == 2006 ) %>% 
  ggplot(., aes(water.level, turb.filtered)) + facet_wrap(~ lake, ncol = 1) + 
  geom_path() +
  geom_text(aes(label=mo)) + geom_point()

# year by year?

## ---- phasePlots ----
c1 %>% 
  filter(!is.na(water.level) & yr > 1994) %>% 
  ggplot(., aes(water.level, turb.filtered)) + facet_grid(lake ~ w_yr ) + 
  geom_path() +
  geom_text(aes(label=mo_lab)) 


## ---- colorLineSegs ----

  c1 %>% 
  filter(!is.na(water.level) & yr > 1995) %>% 
  mutate(yrf = factor(w_yr)) %>% 
  ggplot(., aes(water.level, turb.filtered, colour = yrf, group = 1)) + facet_wrap(~ lake, ncol = 1) + 
  geom_path() + geom_point() + 
  geom_vline(data = wll, aes(xintercept = crit.min), linetype = "dotted", colour = "red") +
  scale_y_log10()

last_plot() + aes(group = NA)  

## ---- gganim1 ----

library(gganimate)

p <- 
  c1 %>% 
  filter(!is.na(water.level) & yr > 1995 & lake == "Sorell" & !is.na(turb.filtered)) %>% 
  mutate(yrf = factor(w_yr)) %>% 
  ggplot(., aes(water.level, turb.filtered, group = 1)) +
  geom_vline(data = subset(wll, lake == "Sorell"), aes(xintercept = crit.min), linetype = "dotted", colour = "red") +
  scale_y_log10()
  
p +  geom_point(aes(colour = yrf)) 

ap1 <- p + geom_point(aes(colour = yrf), alpha = 0.8, size = 3.5) + 
  transition_states(yrf,
                    transition_length = 20,
                    state_length = 1,
                    wrap = FALSE) +
  ease_aes('cubic-in-out') +
  ggtitle('Water year {closest_state}')
ap1

animate(ap1, nframes = 50, renderer = gifski_renderer("gganim.gif"))

anim_save("sor.gif", anim = ap1, path = ".", renderer = gifski_renderer("gganim.gif"))


