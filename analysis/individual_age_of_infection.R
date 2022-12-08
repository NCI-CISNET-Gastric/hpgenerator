
#* This 

# 01 Initial Setup --------------------------------------------------------
#* Clean environment
remove(list = ls())

#* Refresh environment memory
gc()



# 02 Load packages and functions ------------------------------------------
library(readr)
library(dplyr)
library(data.table)

# 03 Load data ------------------------------------------------------------
# Force of infection data retrieved by Jorge in the shiny app
df_FOI_raw <- read_csv(file = "data/dataset-2022-11-25.csv")

# 04 Generate synthetic population ----------------------------------------
# Equal probabilities for demographic characteristics?
equal_probs <- TRUE

# Cohort size
n_pop <- 1e5

v_period    <- unique(df_FOI_raw$period)
v_race      <- unique(df_FOI_raw$race)
v_sex       <- unique(df_FOI_raw$sex)
v_age_event <- sort(unique(df_FOI_raw$age))

if (equal_probs) {
  v_prob_period <- rep((1/length(v_period)), length(v_period))
  v_prob_race   <- rep((1/length(v_race)), length(v_race))
  v_prob_sex    <- rep((1/length(v_sex)), length(v_sex))
}
if (!equal_probs) {
  v_prob_period <- c(0.3, 0.7)
  v_prob_race   <- c(0.1, 0.2, 0.1 , 0.3, 0.2)
  v_prob_sex    <- c(0.7, 0.3)
}


# Create synthetic cohort
set.seed(1234)
dt_pop <- data.table(id     = 1:n_pop,
                     period = sample(x = v_period, size = n_pop, replace = T, prob = v_prob_period),
                     sex    = sample(x = v_sex,    size = n_pop, replace = T, prob = v_prob_sex),
                     race   = sample(x = v_race,   size = n_pop, replace = T, prob = v_prob_race)
                     )
set.seed(NULL)


# 05 Wrangle FOI data -----------------------------------------------------
# Check number of unique sets of data
df_distinct <- df_FOI_raw %>% 
  distinct(race, period, sex)

df_FOI_1 <- df_FOI_raw %>% 
  select(race, period, sex, age, foi_mean)

# Obtain the instantaneous probability of infection
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

# Create a dataframe with the transposed probabilities
df_transp_probs_0 <- matrix(data = df_FOI_2$p,
                          ncol = length(unique(df_FOI_2$age)),
                          byrow = TRUE) %>%
  as.data.frame()

v_colnames <- paste0("age_", v_age_event)

colnames(df_transp_probs_0) <- v_colnames

# Create final long-wide dataframe to retrieve probabilities of infection
dt_transp_probs <- df_distinct %>% 
  cbind(df_transp_probs_0) %>% 
  as.data.table()


# 06 Merge wrangled data with synthetic data ------------------------------
dt_pop_probs <- data.table::merge.data.table(
  x = dt_pop, 
  y = dt_transp_probs, 
  by = c("race", "period", "sex"),
  all.x = TRUE,
  sort = FALSE)

# Set ID as the first column
setcolorder(dt_pop_probs, "id")


apply(X = dt_pop_probs[col %chin% "age_"], MARGIN = 1, FUN = )
