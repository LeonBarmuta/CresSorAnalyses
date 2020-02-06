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


## ---- gganim1 ----

sor <- 
  c1 %>% 
  filter(!is.na(water.level) & yr > 1995 & lake == "Sorell" & !is.na(turb.filtered)) %>% 
  mutate(yrf = factor(w_yr),
         epoch = cut(yr, breaks = c(1990, 1999, 2005, 2017, max(yr)), 
         labels = c("1990s", "2000s", "2010s", "Recent")))

sor
summary(sor)

sor_p1 <- ggplot(sor, aes(x = water.level, y = turb.filtered)) +
  geom_point(aes(colour = epoch)) +
  geom_vline(data = subset(wll, lake == "Sorell"), aes(xintercept = crit.min), linetype = "dotted", colour = "red")


sor_p1

sor_p1 + geom_path()

# #%>% 
#   #ggplot(., aes(water.level, turb.filtered, group = 1)) +
#   ggplot(., aes(water.level, turb.filtered, group = yrf)) +
#   geom_vline(data = subset(wll, lake == "Sorell"), aes(xintercept = crit.min), linetype = "dotted", colour = "red") #+
#   #scale_y_log10()

  
# sor +  geom_point(aes(colour = yrf)) + theme(legend.position = "top")
# 
# sor + transition_reveal(as.integer(water.level)) # does not work

sor_a1 <- sor_p1 + 
  transition_time(yr) +
  labs(title = "Water year: {frame_time}") + 
  #view_follow(fixed_x = TRUE)
  shadow_wake(wake_length = 0.5, alpha = FALSE)
animate(sor_a1, nframes = 25)

# sor_a2 <- sor_p1 + transition_reveal(water.level) # not in year sequence; needs to be on x axis?
#   
# animate(sor_a2, nframes = 25)

sor_a3 <- sor_p1 +  
  transition_states(yrf,
                    transition_length = 20,
                    state_length = 1,
                    wrap = FALSE) +
  ease_aes('cubic-in-out') +
  ggtitle('Water year {closest_state}')

animate(sor_a3, nframes = 50)

animate(sor_a3)

# animate(ap1, nframes = 50, renderer = gifski_renderer("gganim.gif"))
# 
# anim_save("sor.gif", anim = ap1, path = ".", renderer = gifski_renderer("gganim.gif"))


