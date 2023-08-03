#Three state example


#A well-designed model captures all the pertinent cause and effect relationships of a problem scenario and can calculate what might happen 
#accurately, in the sense that it has external validity because it accords reasonably well with reality.
#
#The goal of a DES model is often to understand how changes in the system's inputs affect its outputs, with an eye towards improving 
#the system's performance. DES is especially useful in complex systems where interactions and dependencies make it hard to predict outcomes. 
#It allows for experimentation with different scenarios, making it highly useful in decision-making processes.


#Health
#Sick
#Death

n_i       <- 100000               # number of individuals
n_t       <- 60                   # number of cycles #How we can determine the number of cycles. #in 60 years scale time 
v_n       <- c("H", "S", "D")     # state names
n_s       <- length(v_n)          # number of states

# Event probabilities (per cycle)
p_HS      <- 0.05               # probability to become Sick when Healthy
p_HD      <- 0.01
p_SD      <- 0.1                # probability to die when Sick

r_HS=-log(1-p_HS)               # rate to become sick when healthy #how we determine this?

HS # weibull tiene shape. Genero un vector de 0 a 100. poner un valor .o5 y ese es el valor exponencial. Generas una función que le metas a la función W function
#Toma shape y scale. Adentro genera una hazard de la weibull. pack: flexsurv. función hweib (poner shape y scale) y regresas hweibull
# 

r_SD=-log(1-p_SD)               # rate to die when sick #how we determine this?
r_HD=-log(1-p_HD)               # rate to die when healty  #how we determine this?

#en un año de 5 por ciento, cuál es la tasa a la que va a ocurrir ese evento.

#This example is because of the asumptions. 

#transform the probabilities into a "rate" space, allowing for continuous, non-negative values.
#more informative to express the likelihood of an event as a rate, rather than as a probability. ∫ee how quickly they happen.

age_max <- 110

# Population characteristics
p_female <- 0.50                # proportion population who are female
#


# load age distribution
age_dist = read.csv("data/MyPopulation_AgeDistribution.csv")  



#model functions

time_to_die_healty <- function(n = .N , rate_healthy_death)  {
  t_die_healthy <- rexp(n = n, rate = rate_healthy_death) ## Inverse of exponential distribution
  return(t_die_healthy) #Random sample from exponential distribution?
}

#The exponential distribution is often used to model the time between events in a process in which events 
#occur continuously and independently at a constant average rate. 
#generating random numbers (times until death, in this case) that are distributed according to an exponential distribution.

time_to_sick <- function(n = .N , rate_sick)  {
  t_to_sick <- rexp(n = n, rate = rate_sick)    ## Inverse of exponential distribution
  return(t_to_sick)
}



time_to_die_sick <- function(n = .N, rate_sick_death)  {
  t_sick_die <- rexp(n = n, rate = rate_sick_death)   ## Inverse of exponential distribution
  return(t_sick_die)
}






###### Create data sets ----
## data.frame ----
set.seed(1223)
dt_demog <- data.table(id = 1:n_i,
                       sex = sample(x = c(0, 1), 
                                    size = n_i,
                                    prob = c(0.5, 0.5),
                                    replace = TRUE),
                       age = sample(x = age_dist$age, 
                                    size = n_i,
                                    prob = age_dist$prop,
                                    replace = TRUE))

library(ggplot2)

ggplot(dt_demog, aes(x=age)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  labs(title="Histogram of Ages", x="Age", y="Count")

ggplot(dt_demog, aes(x=age)) + 
  geom_density(fill="steelblue") +
  labs(title="Density Plot of Ages", x="Age", y="Density")

#

###### time to die healthy
dt_demog[, t_to_die_healthy := time_to_die_healty(n = .N, rate_healthy_death = r_HD)] #time to sick

#We have years more than 100

###### Age of death healthy
dt_demog[ , age_death_healthy :=  round(age + t_to_die_healthy)]
dt_demog[ age_death_healthy > age_max, age_death_healthy := age_max]

###### Time to sick  ----
dt_demog[, t_to_sick := time_to_sick(n = .N, rate_sick = r_HS)] #time to sick

###### Age of sick ----
dt_demog[ , age_init_sick := round(age+t_to_sick)]
dt_demog[ age_init_sick > age_max, age_init_sick := NA]

###### indicator of sickness before death
dt_demog[, sick := ifelse(age_death_healthy>age_init_sick,1,0)]

###### Time to death when sick  ----
dt_demog[sick==1, t_to_death_sick := time_to_die_sick(n = .N, rate_sick_death = r_SD) ]  

###### Age of death when sick #Correct this line: death from deth
dt_demog[sick==1,age_death_sick := age_init_sick + round(t_to_death_sick)]
dt_demog[age_death_sick > age_max, age_death_sick := age_max]

###### Age of death ----
dt_demog[ , age_death := fifelse(test = age_death_healthy <= age_death_sick | is.na   (age_death_sick) ,
                                 yes =  age_death_healthy, 
                                 no = age_death_sick) ]

# Distribution of age of death
hist(dt_demog$age_death, main = "Age of death", xlab = "Age")



#Define the vector of age to trace, the population sex and the total population by sex.
v_age_full <- c(0:age_max)
v_sex <- unique(dt_demog$sex)
dt_n_pop <- dt_demog[ , .(n_pop = .N), by = .(sex)]


#Generate an empty datatable to be filled
dt_empty <- data.table(sex = rep(v_sex, length(v_age_full)),
                       age = rep(v_age_full, length(v_sex)),
                       deaths = 0,
                       sick_pop = 0)[order(sex, age)]

#Estimate the numner of deaths by age #Correct this: number
dt_deaths <- dt_demog[ , .(deaths = .N), keyby = c("age_death", "sex") ]
setnames(dt_deaths, "age_death", "age")

#Merge number of deaths with the empty data table to get the complete age range we want to evaluate
dt_living <- merge(dt_empty,dt_deaths, by=c("age","sex"), all.x = TRUE)
dt_living[ is.na(deaths.y), deaths.y:=0]
dt_living[ , deaths := deaths.x + deaths.y]

#Adding number of deaths in diagnosed
dt_deaths_ad <- dt_demog[ sick==1 , .(deaths_dx = .N), keyby = c("age_death", "sex") ]
setnames(dt_deaths_ad, "age_death", "age")
dt_living <- merge(dt_living, dt_deaths_ad, by=c("age","sex"), all.x = TRUE)
dt_living[ is.na(deaths_dx), deaths_dx := 0]

#Include population size by sex
dt_living <- merge(dt_living,dt_n_pop, by="sex")

#Estimate living pop
dt_living[ , cum_deaths := cumsum(deaths), by="sex"]
dt_living[ , living_pop := n_pop - cum_deaths]
dt_living[ , cum_deaths_dx := cumsum(deaths_dx), by = "sex"]

#Adding diagnoses
dt_diagnosis <- dt_demog[ age_init_sick <= age_max & sick==1, .(sick_pop = .N), by = .(age_init_sick, sex) ]
setnames(dt_diagnosis, "age_init_sick","age")
dt_living <- merge(dt_living,dt_diagnosis, by=c("sex", "age"), all.x = TRUE)
dt_living[ is.na(sick_pop.y), sick_pop.y := 0]
dt_living[ , sick_pop := sick_pop.x + sick_pop.y]

#Estimate diagnosed population
dt_living[ , cum_sick := cumsum(sick_pop), by = "sex"]
dt_living[ , sick_pop_living := cum_sick - cum_deaths_dx]

#Estimate healthy population
dt_living[ , healthy_pop_living := living_pop - sick_pop_living]

#Select the measure of interest to trace in a new table
dt_pop_trace <- dt_living[ , c("age", "sex", "cum_deaths","sick_pop_living", "living_pop", "healthy_pop_living")]
setnames(dt_pop_trace, c( "cum_deaths","sick_pop_living", "living_pop", "healthy_pop_living"),c("cumulative_deaths", "sick_population", "living_population", "healthy_population"))

