

dt_ati <- age_to_infection(df = df_p_hat_race_final_cohort, cohort = cohort, race = race, alpha0 = alpha0, alpha1 = alpha1, 
                           gamma0 = gamma0, gamma1 = gamma1, size = 1000)

dt_ati2 <- age_to_infection(df = df_p_hat_race_final_cohort, cohort = 1940:1943, race = race, alpha0 = alpha0, alpha1 = alpha1, 
                           gamma0 = gamma0, gamma1 = gamma1, size = 100)

jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                 "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                 "red", "#7F0000"))

color_map  <-  jet.colors(100)

library(ggiraph)

girafe(ggobj = gg_point)

gg_point <- ggplot(dt_ati, aes(x = cohort, y = age_to_infection, color = age_to_infection)) + 
  geom_point_interactive(aes(tooltip = paste0("Birth cohort:  ",cohort, "<br>Age at infection:  ",age_to_infection)), size = 1) +
  facet_wrap(~ race_fac) + 
  scale_color_gradientn(colors = color_map) + 
  labs(title = "Age at Infection Across Birth Cohorts by Race",
       x = "Birth Cohort", 
       y = "Age at Infection",
       color = "Age at Infection") + 
  theme_minimal() +
  theme(legend.position = "bottom")

library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming 'df' is your dataframe and it's already been read into R


dt_ati2 <- age_to_infection(df = df_p_hat_race_final_cohort, cohort = 1940:1948, race = race, alpha0 = alpha0, alpha1 = alpha1, 
                            gamma0 = gamma0, gamma1 = gamma1, size = 100)

# Preparing the data: counting the occurrences of age_to_infection for each cohort and race
dt_ati_hm <- dt_ati2 %>% 
  filter(!is.na(age_to_infection))

# Creating a new dataframe for the heatmap
# Counting the occurrences of age_to_infection for each cohort and race_fac combination
heatmap_data <- dt_ati_hm %>%
  group_by(cohort, race_fac) %>%
  count(age_to_infection) %>%
  ungroup() %>% 
  rename(people = n)

cohort = 1940:1948

ggplot(heatmap_data, aes(x = cohort, y = age_to_infection, fill = people)) + 
  geom_tile_interactive(aes(tooltip = paste0("Birth cohort:  ",cohort, "<br>Age at infection:  ",age_to_infection, "<br>People:  ",people)), size = 1) +
  #geom_bar() +
  scale_fill_gradientn(name = "People", colours = jet.colors(100)) +
  #scale_y_continuous(breaks = seq(0, .4, by = .1), 
  #                   labels = seq(0, .4, by = .1)) +
  scale_x_continuous(breaks = cohort, 
                     labels = cohort) +
  labs(title = "Population that eventually gets infected ",
       x = "Birth Cohort",
       y = "Age") + 
  theme_bw(base_size = 20)


# Using ggplot2 to create the heatmap
gg1 <- ggplot(heatmap_data, aes(x = cohort, y = age_to_infection, fill = people)) + 
  facet_wrap(~race_fac) +
  geom_tile_interactive(aes(tooltip = paste0("Birth cohort:  ",cohort, "<br>Age at infection:  ",age_to_infection, "<br>People:  ",people)), size = 1) +
  scale_fill_gradientn(colors = color_map)+ 
  theme_bw() +
  labs(title = "Heatmap of Age at Infection Frequency Across Birth Cohorts by Race",
       x = "Birth Cohort",
       y = "Race",
       fill = "Frequency") +
  theme(legend.position = c(.88, 0.2),
        #legend.box = "vertical",
        legend.title = element_text(vjust = .8),
        legend.key.size = unit(1, 'cm'), 
        legend.spacing.x = unit(.5, 'cm'),
        legend.text = element_text(size = 16),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        strip.text = element_text(size = 30, face = "bold")) +
  #edit legends
  guides(
    #reverse color order (higher value on top)
    color = guide_colorbar(reverse = TRUE))

girafe(ggobj = gg1) %>% 
  #add color to the tooltip according to the fill gradient
  girafe_options(opts_tooltip(use_fill = TRUE),
                 opts_zoom(min = .5, max = 4))

dt_ati_prueba <- dt_ati

# Assuming your data is in a dataframe named df
dt_ati_prueba$party <- as.factor(dt_ati_prueba$cohort)  # Convert 'cohort' to a factor for 'party'
dt_ati_prueba$seats <- 1  # Each row is one seat

dt_ati_prueba$age_group <- cut(dt_ati_prueba$age_to_infection, breaks = quantile(dt_ati_prueba$age_to_infection, probs = 0:5/5, na.rm = TRUE), include.lowest = TRUE)

#Filter has_event == 1

dt_ati_prueba <- dt_ati_prueba %>%
  filter(has_event == 1)

# Aggregate data to count the number of seats per cohort and race_fac
parliament_data_f <- dt_ati_prueba %>%
  group_by(race_fac, party) %>%
  summarise(seats = n(), .groups = 'drop') %>%
  ungroup() %>%
  mutate(age_group = dt_ati_prueba$age_group[match(party, dt_ati_prueba$cohort)]) %>%
  mutate(party = as.factor(party),
         group = party,
         order = row_number())



parliament_data <- dt_ati_prueba %>%
  group_by(race_fac, party) %>%
  summarise(seats = n(), .groups = 'drop') %>%
  ungroup() %>%
  mutate(age_group = dt_ati_prueba$age_group[match(party, dt_ati_prueba$cohort)])  # Get age_group for each party

# Add a unique identifier for each seat
parliament_data <- parliament_data %>%
  group_by(race_fac, party) %>%
  mutate(seat_id = row_number()) %>%
  ungroup()

# Create the Parliament Plot with Facets
plot <- ggplot(parliament_data, aes(x = seat_id, fill = age_group)) +
  geom_parliament_seats(aes(y = seats, group = party), 
                        data = parliament_data) +
  facet_wrap(~race_fac, scales = "free_y") +
  scale_fill_brewer(palette = "Spectral", name = "Age to Infection") +
  theme_minimal() +
  labs(title = "Parliament Diagram by Cohort and Age to Infection, Faceted by Race")

# Display the plot
print(plot)




# Reset row numbers (important for ggparliament plotting)
parliament_data_f <- parliament_data %>%
  mutate(mp_id = row_number())



colors <- scale_fill_brewer(palette = "Spectral", name = "Age to Infection")


ggplot(parliament_data_f, aes(x = mp_id, y = party, fill = age_group)) +
  geom_parliament_seats(data = parliament_data) +
  facet_wrap(~race_fac, scales = "free") +
  colors +
  theme_minimal() +
  labs(title = "Parliament Diagram by Cohort and Age to Infection, Faceted by Race")




# save as csv

write.csv(dt_ati, file = "dt_ati.csv")


library(ggparliament)


ggparliament::parliament_data()


us_rep <- election_data %>%
  filter(country == "USA" &
           year == 2016 &
           house == "Representatives")
head(us_rep)



us_house_semicircle <- parliament_data(election_data = us_rep,
                                       type = "semicircle",
                                       parl_rows = 10,
                                       party_seats = us_rep$seats)
head(us_house_semicircle)


ggplot(us_house_semicircle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(colour = NULL, 
       title = "United States Congress") +
  scale_colour_manual(values = us_house_semicircle$colour, 
                      limits = us_house_semicircle$party_short) 


russia_circle <- election_data %>%
  filter(country == "Russia" &
           house == "Duma" &
           year == 2016) %>% 
  parliament_data(election_data = .,
                  party_seats = .$seats,
                  parl_rows = 11,
                  type = "circle")


ggplot(russia_circle, aes(x, y, colour = party_short)) +
  geom_parliament_seats() +
  theme_ggparliament() +
  scale_colour_manual(
    values = russia_circle$colour,
    limits = russia_circle$party_short) +
  labs(colour = NULL) +
  theme(legend.position = "bottom")


ggplot(russia_circle, aes(x, y, colour = party_short)) +
  geom_parliament_seats() +
  theme_ggparliament() +
  scale_colour_manual(
    values = russia_circle$colour,
    limits = russia_circle$party_short) +
  labs(colour = NULL) +
  theme(legend.position = "bottom")

