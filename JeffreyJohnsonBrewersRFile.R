install.packages(dplyr)
library(dplyr)
# --------------------------------------------------
# ----------- Question 1 ---------------------------
# --------------------------------------------------


# probabilities for hitters a and b
p_hitter_a <- 0.07
p_hitter_b <- 0.10

#plate appearances
pa <- 100

#1,000 simulations for efficiency and precision
simulations <- 1000

#run simulations
walks_a <- rbinom(simulations, pa, p_hitter_a)
walks_b <- rbinom(simulations, pa, p_hitter_b)

#the probability of hitter A drawing more walks
event <- mean(walks_a > walks_b)
event

# --------------------------------------------------
# ----------------- Question 2 ---------------------
# --------------------------------------------------

# pitcher throwing a ball into the strike zone
event_a <- 0.48

# pitcher throwing a ball outside the strike zone
event_b <- 1 - event_a

# batter swinging at a strike
event_b_given_a <- 0.64

# batter swinging at a ball outside of the zone
event_b_given_b <- 0.29

# calculate probability of batter swinging (law of total probability)
p_swing <- (event_a * event_b_given_a) + (event_b * event_b_given_b)

# calulate probability of pitch in the zone given swing or A | swing (event a given p_swing) (bayes theorem)
p_a_given_swing <- (event_b_given_a * event_a) / p_swing

p_a_given_swing

# --------------------------------------------------
# ----------------- Question 3 (part a) ------------
# --------------------------------------------------

# pitcher a values:
a_velocity <- 92
a_vertical_break <- 18
a_release_height <- 6

#pitcher b values:
b_velocity <- 95
b_vertical_break <- 12
b_release_height <- 5.5

# model values:
intercept <- 138
velocity <- -1.77
induced_vertical_break <- -1.08
release_height <- 8.62
rse <- 9

# run expectancy change model:
run_expectancy_change_a <- intercept + (velocity * a_velocity) + (induced_vertical_break * a_vertical_break) + (release_height * a_release_height)
run_expectancy_change_b <- intercept + (velocity * b_velocity) + (induced_vertical_break * b_vertical_break) + (release_height * b_release_height)

# difference in a from b:
difference <- run_expectancy_change_a - run_expectancy_change_b

# find the standard deviation of the difference:
sd_difference <- sqrt(2) * rse

# z score:
z_score <- difference / sd_difference

# probability of b > a
prob_a_greater_than_b <- pnorm(z_score)

#answer to 3a
prob_a_greater_than_b

# --------------------------------------------------
# ----------------- Question 3 (part b) ------------
# --------------------------------------------------

# coefficients from induced vertical break
ivb_intercept <- -0.48
ivb_slope_velocity <- 0.0126
ivb_slope_release_height <- 2.60

# substitute the ivb into the first model
# using only velocity and release height

# intercept for adjusted model
adjusted_intercept <- intercept + (induced_vertical_break * ivb_intercept)

# velocity slope for adjusted model
adjusted_velocity_slope <- velocity + (induced_vertical_break * ivb_slope_velocity)

# release height slope for adjusted model
adjusted_release_height_slope <- release_height + (induced_vertical_break * ivb_slope_release_height)

adjusted_intercept
adjusted_velocity_slope
adjusted_release_height_slope

# --------------------------------------------------
# ----------------- Question 4 ---------------------
# --------------------------------------------------

set.seed(100)
# win totals
padres_wins <- 91
mets_wins <- 87
dbacks_wins <- 87
braves_wins <- 86

# matchups vs wild card opponent
mets_braves_games <- 2
padres_dbacks_games <- 3

# store a variable to count the number of times a simulation landed the mets in third place
num_simulations <- 100
mets_third_place_count <- 0

# simulations, iterate 1 to 100
for (i in 1:num_simulations) {
  # mets vs braves sim
  mets_wins_temp <- mets_wins + sample(0:2, 1)
  braves_wins_temp <- braves_wins + (mets_braves_games - (mets_wins_temp - mets_wins))
  
  # padres vs dbacks sim
  padres_wins_temp <- padres_wins + sample(0:3, 1)
  dbacks_wins_temp <- dbacks_wins + (padres_dbacks_games - (padres_wins_temp - padres_wins))
  
  # df with final standings
  final_standings <- data.frame(
    Team = c("San Diego Padres", "New York Mets", "Arizona Diamondbacks", "Atlanta Braves"),
    Wins = c(padres_wins_temp, mets_wins_temp, dbacks_wins_temp, braves_wins_temp)
  )
  
  # order by total wins
  final_standings <- final_standings[order(-final_standings$Wins), ]
  
  # if else statement to verify which place the mets landed
  # if condition: mets third place
  if (final_standings$Team[3] == "New York Mets") {
    mets_third_place_count <- mets_third_place_count + 1
  }
}

# calculate the probability
probability_mets_third <- mets_third_place_count / num_simulations

mets_third_place_count
probability_mets_third

# --------------------------------------------------
# ----------------- Question 5 ---------------------
# --------------------------------------------------


set.seed(100)

# road scoring probability values
road_team_probs <- c(0.35, 0.40, 0.15, 0.10)

# simulation
simulate_games <- function(p, num_simulations = 100) {
# store the amount of home wins 
  home_wins <- 0
  
  for (i in 1:num_simulations) {
# select runs scored by the road team
    road_runs <- sample(0:3, size = 1, prob = road_team_probs)
    
# determine home team runs based on road team runs
    if (road_runs == 3) {
      home_runs <- sample(0:3, size = 1, prob = c(0.35, 0.40, 0.15, 0.10))
    } else if (road_runs == 2) {
      home_runs <- sample(0:3, size = 1, prob = c(0.35, 0.40, 0.15, 0.10))
    } else if (road_runs == 1) {
      home_runs <- sample(0:2, size = 1, prob = c(0.35, 0.20, 0.45))
    } else {
      home_runs <- sample(0:1, size = 1, prob = c(1 - p, p))
    }
    
    # if else statement to determine if home team won
    if (home_runs > road_runs) {
      home_wins <- home_wins + 1
    }
  }
  
  # calculate winning probability
  return(home_wins / num_simulations)
}


# grid search for p values
target_win_probability <- 0.60
p_values <- seq(0, 1, by = 0.01)  # Test p values from 0 to 1 in increments of 0.01
winning_probabilities <- sapply(p_values, simulate_games)

#find optimal p value
closest_p_index <- which.min(abs(winning_probabilities - target_win_probability))
optimal_p <- p_values[closest_p_index]
optimal_win_probability <- winning_probabilities[closest_p_index]

optimal_p
optimal_win_probability

