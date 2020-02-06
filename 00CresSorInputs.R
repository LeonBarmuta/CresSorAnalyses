## ---- pkgLoad ----

library(tidyverse)
library(readxl)
library(lubridate)

## ---- inputFiles ----

ll0 <- read_xlsx("../CresSorData/Crescent_Sorell_water_levels_1970_present.xlsx")
ll0
names(ll0) <- tolower(names(ll0))

tail(ll0)
ll0 <- 
  ll0 %>% 
  mutate(l.level = as.numeric(l.level),
         dt = as.Date(dt))
ll0


tb0 <- read_xlsx("../CresSorData/Crescent_Sorell_turbidity_1991_present.xlsx")
tb0
names(tb0) <- tolower(names(tb0))
tb0 <- 
  tb0 %>% 
  mutate(turb.total = as.numeric(turb.total),
         turb.filtered = as.numeric(turb.filtered),
         water.level = as.numeric(water.level),
         date = as.Date(date),
         turb.part = turb.total - turb.filtered # particulate turbidity
         )
tb0
summary(tb0)

tb0 %>% 
  filter(is.na(water.level) & !is.na(turb.filtered))

# just using turbidity data
tb0

tb0 %>%   group_by(lake) %>% 
  rename(dt = date) %>% 
  arrange(dt) %>%
  mutate(yr = year(dt),
         mo = month(dt, label = TRUE),
         ) %>% 
  ungroup() %>% 
  filter(yr > 1996) %>% 
  ggplot(., aes(dt, turb.filtered)) + geom_line() + geom_point() + facet_wrap(~ lake, ncol = 1)

## ---- merge ----

# any diff between l.level & water.level?

c0 <- full_join(ll0, tb0, by = c("lake"="lake", "dt"="date")) 
c0 <- 
  c0 %>% 
  mutate(lake = factor(lake, levels = c("Sorell", "Crescent"))) %>% 
  mutate(yr = year(dt),
            mo = month(dt),
            mo_lab = month(dt, label = TRUE, abbr = TRUE),
            dy = day(dt),
            #mo_shift = ifelse(mo > 8, mo - 8, mo + 4),
            w_yr = ifelse(mo < 9, yr -1, yr) # compute water year: Sept-August
  ) %>% 
  arrange(lake, dt)
             

c0

summary(c0)

c0 %>% 
  filter(is.na(l.level)& is.na(water.level)) %>% 
  summary()


c0 %>% 
  filter(!is.na(l.level)& !is.na(water.level) & !is.na(turb.total)) 
#hmm some issues here

c0 %>% 
  filter(!is.na(water.level)) %>% 
  summary()



# # date as water year
# 
# c0 %>% 
#   transmute(dt = dt, 
#             yr = year(dt),
#             mo = month(dt),
#             mo_lab = month(dt, label = TRUE, abbr = TRUE),
#             dy = day(dt),
#             mo_shift = ifelse(mo > 8, mo - 8, mo + 4),
#             w_yr = ifelse(mo < 9, yr -1, yr), 
#             w_dt = ymd(paste(w_yr, mo_shift, dy, sep = "-"))) %>% 
#   filter(is.na(w_dt))
# 
# # ah impossible days for shifted months!

saveRDS(c0, "c0.rds")


xx <- c0 %>% 
  filter(!is.na(turb.filtered)) #%>% 

summary(xx)
with(xx, all.equal(l.level, water.level))

summary(c0)
rm(xx)

## ---- WaterLevelLimts ----
wll <- 
tibble(lake = factor(c("Sorell", "Crescent"), levels = c("Sorell", "Crescent")), 
       crit.min = c(803.2, 802.2),
       pref.min = c(803.5, 802.7))
wll
saveRDS(wll, "wll.rds")




