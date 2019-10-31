# Plots for Sue
library(readxl)
library(tidyverse)

mwmt <- read_excel("Data/From Sue/MWMT Predictions vs 2019 actuals.xlsx")

mwmtLg <- mwmt %>% 
  select(-`...1`, -site) %>% 
  gather(key = variable, value = value, -fsite)

#original plot without legend and without 2019 data.
ggplot(data = mwmt) +
  geom_point(aes(x = mwmt_2009, y = fct_reorder(fsite, mwmt_2009)), shape = 1, size = 2) +
  geom_segment(aes(x = A1B_2039_delta, xend = A2_2039_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009))) +
  geom_segment(aes(x = A1B_2069_delta, xend = A2_2069_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009)), size = 1.2) +
  # geom_point(aes(x = mwmt_2019, y = fct_reorder(fsite, mwmt_2009)), color = "red", size = 3) +
  geom_vline(aes(xintercept = 13), linetype = 2) +
  geom_vline(aes(xintercept = 18), linetype = 2) +
  geom_vline(aes(xintercept = 20), linetype = 2) +
  labs(x = "Maximum Weekly Maximum Temperature (˚C)") +
  theme_bw() +
  theme(axis.title.y = element_blank(), legend.position = "bottom",
        text = element_text(size = 18))

ggsave("output/For Sue/all sites no 2019.jpeg", width = 8.5, height = 8.5, units = "in")



#original plot without legend and WITH 2019 data.
ggplot(data = mwmt) +
  geom_point(aes(x = mwmt_2009, y = fct_reorder(fsite, mwmt_2009)), shape = 1, size = 2) +
  geom_segment(aes(x = A1B_2039_delta, xend = A2_2039_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009))) +
  geom_segment(aes(x = A1B_2069_delta, xend = A2_2069_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009)), size = 1.2) +
  geom_point(aes(x = mwmt_2019, y = fct_reorder(fsite, mwmt_2009)), color = "red", size = 3) +
  geom_vline(aes(xintercept = 13), linetype = 2) +
  geom_vline(aes(xintercept = 18), linetype = 2) +
  geom_vline(aes(xintercept = 20), linetype = 2) +
  labs(x = "Maximum Weekly Maximum Temperature (˚C)") +
  theme_bw() +
  theme(axis.title.y = element_blank(), legend.position = "bottom",
        text = element_text(size = 18))

ggsave("output/For Sue/all sites with 2019.jpeg", width = 8.5, height = 8.5, units = "in")


#13 sites without legend and no 2019 data.
ggplot(data = mwmt %>% filter(!is.na(mwmt_2019))) +
  geom_point(aes(x = mwmt_2009, y = fct_reorder(fsite, mwmt_2009)), shape = 1, size = 2) +
  geom_segment(aes(x = A1B_2039_delta, xend = A2_2039_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009))) +
  geom_segment(aes(x = A1B_2069_delta, xend = A2_2069_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009)), size = 1.2) +
  # geom_point(aes(x = mwmt_2019, y = fct_reorder(fsite, mwmt_2009)), color = "red", size = 2) +
  geom_vline(aes(xintercept = 13), linetype = 2) +
  geom_vline(aes(xintercept = 18), linetype = 2) +
  geom_vline(aes(xintercept = 20), linetype = 2) +
  labs(x = "Maximum Weekly Maximum Temperature (˚C)") +
  theme_bw() +
  theme(axis.title.y = element_blank(), legend.position = "bottom",
        text = element_text(size = 18))

ggsave("output/For Sue/thirteen sites no 2019.jpeg", width = 8.5, height = 8.5, units = "in")


#13 sites without legend and WITH 2019 data.
ggplot(data = mwmt %>% filter(!is.na(mwmt_2019))) +
  geom_point(aes(x = mwmt_2009, y = fct_reorder(fsite, mwmt_2009)), shape = 1, size = 2) +
  geom_segment(aes(x = A1B_2039_delta, xend = A2_2039_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009))) +
  geom_segment(aes(x = A1B_2069_delta, xend = A2_2069_delta, 
                   y = fct_reorder(fsite, mwmt_2009), 
                   yend = fct_reorder(fsite, mwmt_2009)), size = 1.2) +
  geom_point(aes(x = mwmt_2019, y = fct_reorder(fsite, mwmt_2009)), color = "red", size = 3) +
  geom_vline(aes(xintercept = 13), linetype = 2) +
  geom_vline(aes(xintercept = 18), linetype = 2) +
  geom_vline(aes(xintercept = 20), linetype = 2) +
  labs(x = "Maximum Weekly Maximum Temperature (˚C)") +
  theme_bw() +
  theme(axis.title.y = element_blank(), legend.position = "bottom",
        text = element_text(size = 18))

ggsave("output/For Sue/thirteen sites with 2019.jpeg", width = 8.5, height = 8.5, units = "in")


