


library(readr)
library(dplyr)


df_FOI_raw <- read_csv(file = "data/dataset-2022-11-25.csv")

df_FOI_1 <- df_FOI_raw %>% 
  select(race, period, sex, age, foi_mean)

df_FOI_2 <- df_FOI_1 %>% 
  group_by(race, period, sex) %>% 
  mutate(
    # Cumulated hazard (H)
    H = cumsum(foi_mean),
    # Cumulated probability (P)
    P = 1 - exp(-H),
    # Instantaneous probability (p)
    p = P - lag(P)
    ) %>% 
  ungroup() %>% 
  mutate(p = if_else(condition = age == 0,
                     true = `P`,
                     false = `p`))
