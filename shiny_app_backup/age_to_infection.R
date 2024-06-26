
#Load parameters

load(file = "hp_foi_us_races/11.01_df_parameters_linear_cauchy.RData")


library(data.table)
# generate population
n_pop <- 100000
dt_pop <- data.table(id = 1:n_pop,
                     cohort = 1940,
                     race = sample(x = c(4, 5), 
                                   size = n_pop, 
                                   prob = c(0.5, 0.5),
                                   replace = TRUE))
# F(a) <- par_alpha*(1-exp(-par_gamma*a))
# prob = par_alpha*(1-exp(-par_gamma*event_time))
#' Inverse constrained exponential function
#'
#' \code{inv_constrained_exponential} implements the inverse constrained 
#' exponential function.
#'
#' @param prob Probability of the event.
#' @param par_alpha Maximum proportion of individuals eventually getting the event.
#' @param par_gamma Rate of event for those evenatually getting the event. 
#' @return 
#' The time at which the event happens.
#' @export
inv_constrained_exponential <- function(prob, par_alpha, par_gamma){
  n_sample <- length(prob)
  has_event <- (prob <= par_alpha)
  event_time <- rep(0, n_sample)
  event_time[has_event] <- (-1/par_gamma[has_event])*log(1-prob[has_event]/par_alpha[has_event])
  return(event_time)
}

#' Get parameters for constrained exponential function
#'
#' \code{get_parameters_constrained_exponential} implements the inverse 
#' constrained exponential function.
#'
#' @param birth_cohort Year of birth cohort.
#' @param race Race in numeric categories.
#' @param alpha0 
#' @param alpha1 
#' @param gamma0 
#' @param gamma1 
#' @return 
#' The time at which the event happens.
#' @export
get_parameters_constrained_exponential <- function(birth_cohort, race,
                                                   alpha0, alpha1,
                                                   gamma0, gamma1){
  bc <- birth_cohort - 1908
  par_alpha <- ilogit(alpha0[race] + bc*alpha1[race])
  par_gamma <- exp(gamma0[race] + bc*gamma1[race])
  return(list(par_alpha = par_alpha, par_gamma = par_gamma))
}

prob <- runif(n = n_pop)
par_alpha <- 0.8
par_gamma <- 0.05

v_event_time <- inv_constrained_exponential(prob = prob, 
                                            par_alpha = par_alpha, 
                                            par_gamma = par_gamma)
hist(v_event_time)


df_parameters_linear_cauchy #df

alpha0 <- df_parameters_linear_cauchy[1:5,"Mean"]
alpha1 <- df_parameters_linear_cauchy[6:10,"Mean"]
gamma0 <- df_parameters_linear_cauchy[11:15,"Mean"]
gamma1 <- df_parameters_linear_cauchy[16:20,"Mean"]

get_parameters_constrained_exponential(birth_cohort = 1940, 
                                       race = c(4, 5), 
                                       alpha0 = alpha0, alpha1 = alpha1,
                                       gamma0 = gamma0, gamma1 = gamma1)
dt_pop[, c("par_alpha", "par_gamma") := get_parameters_constrained_exponential(birth_cohort = cohort, 
                                                race = race, 
                                                alpha0 = alpha0, alpha1 = alpha1,
                                                gamma0 = gamma0, gamma1 = gamma1)]


v_probs <- runif(n = n_pop)
dt_pop[, age_infection := inv_constrained_exponential(prob = v_probs, 
                                                      par_alpha = par_alpha,
                                                      par_gamma = par_gamma)]
hist(dt_pop$age_infection)
