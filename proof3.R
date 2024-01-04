library(ggplot2)

# Simulate the data
set.seed(42)
n <- 10000
birth_year <- sample(1940:1998, n, replace = TRUE)
age <- 2023 - birth_year
infection_risk <- runif(n)

# Create the data frame
df <- data.frame(birth_year, age, infection_risk)

# Create the plot
ggplot(dt_ati_alt, aes(x = cohort, y = sum(has_event), color = age)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(limits = c(10, 90), option = "C") +
  labs(x = "Birth Cohort (Year)", y = "Proportion Infected", color = "Age") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_colorbar(ticks = FALSE))

# Print the plot
print(p)


dt_ati_alt <-  dt_ati[
  , age := ifelse(is.na(age_to_infection), 0, age_to_infection)
]

df_infected <- subset(dt_ati_alt, has_event == 1)

# Create the scatter plot
ggplot(df_infected, aes(x = cohort, y = age_to_infection, color = age)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(title = "Age at Infection by Birth Cohort",
       x = "Birth Cohort (Year)",
       y = "Age at Infection",
       color = "Age") +
  theme_minimal()


#save as csv

write.csv(dt_races_all_transformed, "dt_races_all_transformed.csv")

df_long <- melt(dt_races_all_transformed, id.vars = c("cohort", "race", "race_fac"), variable.name = "age", value.name = "proportion_infected")

library(ggridges)

ggplot(dt_races_all_transformed %>% filter(proportion_infected > 0, cohort==1940), 
       aes(x = cohort, y = proportion_infected, fill = age, group = race_fac, color=age))  +
  geom_line()+ facet_wrap(~race_fac, scales = "free_x")+
  scale_fill_viridis_c()


ggplot(dt_races_all_transformed %>% filter(proportion_infected > 0), 
       aes(x = cohort, y = proportion_infected, fill = age, group = interaction(race_fac, age))) +
  geom_density_ridges_gradient(
    aes(gradient = after_stat(density)), 
    scale = 3, 
    size = 0.3, 
    rel_min_height = 0.01
  ) +
  scale_fill_viridis_c()

ggplot(dt_races_all_transformed %>% filter(proportion_infected > 0),
       aes(x = cohort, y = factor(proportion_infected), fill = after_stat(density), group = interaction(race_fac, age))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c()
# Now create the heatmap with ggplot2
ggplot(dt_races_all_transformed %>% filter(proportion_infected > 0), aes(x = cohort, y = proportion_infected, fill = age, group = race_fac)) + 
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01)
  
  scale_fill_gradientn(name = "Age \n to \n infection", colours = rev(jet.colors(100))) +
  theme_minimal() + 
  labs(title = "Heatmap of Infection Proportion by Cohort and Age", 
       x = "Birth Cohort (Year)", 
       y = "Proportion Infected", 
       fill = "Average Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  
  
  library(ggplot2)
  library(ggridges)
  library(dplyr)
  
  # Filter for cohort 1940 and proportions greater than 0, then plot
  ggplot(dt_races_all_transformed %>% filter(proportion_infected > 0, cohort == 1940), 
         aes(x = proportion_infected, y = fct_reorder(race_fac, proportion_infected), fill = age)) +
    geom_density_ridges() +
    facet_wrap(~race_fac, scales = "free_y") +
    scale_fill_viridis_c() +
    theme_ridges() # Optional: Adds a suitable theme for ridgeline plots
  
  
  library(ggplot2)
  library(dplyr)
  
  # Filter for cohort 1940 and proportions greater than 0, then plot
  ggplot(dt_races_all_transformed %>% filter(proportion_infected > 0), 
         aes(x = cohort, y = proportion_infected, fill = as.factor(age))) +
    geom_area(position = 'fill') +
    facet_wrap(~race_fac, scales = "free_x")
  
  ggplot(dt_races_all_transformed %>% filter(proportion_infected > 0), aes(x = as.factor(cohort), y = proportion_infected, fill = as.factor(age), group = age)) +
    geom_line(aes(color = as.factor(age)), position = position_dodge(width = 0.2)) +
    geom_ribbon(aes(ymin = 0, ymax = proportion_infected), position = position_dodge(width = 0.2), alpha = 0.3) +
    facet_wrap(~race_fac, scales = "free_x") +
    labs(x = "Cohort", y = "Proportion Infected", fill = "Age") +
    theme_minimal()
  
