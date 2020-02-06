## ---- pkgLoad ----

library(tidyverse)
library(lubridate)

## ---- fileRenderingTest ----

#file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

## ---- readin ----
c0 <- read_rds("c0.rds")
c0
summary(c0)

# critical water levels

wll <- read_rds("wll.rds")

# any non-missing turbs without l.level?

## ---- Check01 ----

c0 %>% 
  filter(!is.na(turb.filtered) & is.na(l.level)) %>% 
  summary()

# how does that happen?

# not missing water.leve but recorded turbidity...

## ---- corTurbs ----

ggplot(c0, aes(turb.total, turb.filtered)) + geom_point(aes(colour = water.level)) + 
  geom_smooth(se = FALSE) + facet_wrap(~ lake, ncol = 1)

# Sorrel and Crescent differ in abilities to winnow out colloidals

c0 %>% filter(turb.total > 250) %>% print(n = 30) # anything special about those dates?

c0 %>% print(n = 20)
tail(c0, 20)

## ---- tsPlot1 ----
c0 %>% 
  filter(yr > 1995) %>% 
  ggplot(., aes(dt, turb.filtered)) + geom_line() + geom_point() + facet_wrap(~ lake, ncol = 1)+
  scale_y_log10()

## ---- StandardisationTest ----

c0 %>% 
  filter(yr > 1995) %>% 
  group_by(lake, w_yr) %>% # note standardising by water year not calendar
  mutate(turb.filtered.md = turb.filtered - median(turb.filtered, na.rm = TRUE)) %>% 
  #summary()
  ggplot(., aes(as.factor(w_yr), turb.filtered.md)) + facet_wrap(~ lake, nrow = 2) + geom_boxplot() + 
  geom_jitter(width = 0.05, size = 2.5, aes(colour = mo_lab))

# some water years more variable than others


sor1 <- 
c0 %>% 
  filter(yr > 1995) %>% 
  ungroup() %>% 
  arrange(dt)

## ---- phasePlots1 ----
c0 %>% 
  filter(!is.na(water.level) & w_yr == 2006 ) %>% 
  arrange(lake,dt) %>% #print(n=21)
  ggplot(., aes(water.level, turb.total)) + facet_wrap(~ lake, ncol = 1) + 
  geom_path() +
  geom_text(aes(label=mo)) + geom_point()

# year by year?

## ---- phasePlots2 ----
c0 %>% 
  filter(!is.na(water.level) & yr > 1994) %>% 
  ggplot(., aes(water.level, turb.filtered)) + facet_grid(lake ~ w_yr ) + 
  geom_path() +
  geom_text(aes(label=mo_lab)) 

## ---- phasePlots3 ----

c1 <- 
  c0 %>% left_join(wll) %>% 
  arrange(lake, dt) %>% 
  mutate(wl = water.level - crit.min)
c1
summary(c1)

c1 %>% 
  filter(!is.na(water.level) & w_yr == 2006 ) %>% 
  arrange(lake,dt) %>% #print(n=21)
  ggplot(., aes(wl, turb.filtered)) + facet_wrap(~ lake, ncol = 1) + 
  geom_path() +
  geom_text(aes(label=mo)) + geom_point()


## ---- colorLineSegs ----

  c1 %>% 
  filter(!is.na(water.level) & yr > 1995) %>% 
  mutate(yrf = factor(w_yr)) %>% 
  ggplot(., aes(wl, turb.total, colour = yrf, group = 1)) + facet_wrap(~ lake, ncol = 1) + 
  geom_path() + geom_point() + 
  #geom_vline(data = wll, aes(xintercept = crit.min), linetype = "dotted", colour = "red") +
  scale_y_log10()

last_plot() + aes(group = NA)  

## ---- gganim1 ----

library(gganimate)

p <- 
  c0 %>% 
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


