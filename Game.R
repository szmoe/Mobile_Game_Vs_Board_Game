# Mobile Game Vs. Board Game for Nutrition Education

library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("Game.csv")))

Game_function <- function(x, varnames) {
  
  # Calculate year one cost for mobile game
  
  cost_mobile_year_one <- c (cost_development_mobile + cost_launch_mobile,
                             rep(0, n_years -1))
                            

  
  # Calculate annual cost for mobile game
  
  cost_mobile_annual <- vv(cost_maintenace_mobile,
                           var_CV, n_years,
                           relative_trend = inflation_rate)
  
  # Calculate total cost for mobile game
  
  cost_mobile_total <- cost_mobile_year_one + cost_mobile_annual
  
  # Calculate year one cost for board game
  
  cost_board_year_one <- c (cost_development_board + cost_teacher_training +
                         (cost_batch_production_board * Board_school_reserve_unit),
                         rep(0, n_years -1))
                 
  
  # Calculate annual cost for board game
  
  # If game is hosted in designated room 
  
  designated_room_yes_no <- chance_event(if_separate_space,
                                         value_if = 1,
                                         value_if_not = 0)
  
  board_room_cost <- if (designated_room_yes_no == 1) {
    cost_space
  } else { 0 }
  
  # If game is hosted in afterschool hours
  
  afterschool_hours_yes_no <- chance_event(if_value_time,
                                           value_if = 1,
                                           value_if_not = 0)
  
  board_time_cost <- if (afterschool_hours_yes_no == 1) {
    cost_time
  } else { 0 }
  
  # Calculate annual cost of board game 
  
  cost_board_annual <- vv (board_room_cost + board_time_cost + 
                                  (cost_batch_production_board * (Board_sale_unit +
                                                                    Board_backup_unit)),
                                  var_CV, n_years,
                                  relative_trend = inflation_rate)
  
  # Calculate total cost for board game
  
  cost_board_total <- cost_board_year_one + cost_board_annual
  
  # Add risks that can reduce benefits
  
  # Parents' acceptance of mobile game and board based on game hour
  
  optimal_game_duration_yes_no <- chance_event(if_optimal_game_duration,
                                               value_if = 1,
                                               value_if_not = 0)
  
  parents_like_mobile <- if (optimal_game_duration_yes_no == 1) {
    if_parents_like_mobile
  } else { if_parents_like_mobile_time}
  
  parents_like_board <- if (optimal_game_duration_yes_no == 1) {
    if_parents_like_board
  } else { if_parents_like_board_time }
  
  mobile_user_risk <- min(if_stable_internet,
                          if_access_internet,
                          if_own_mobile_phone,
                          if_student_like_mobile,
                          parents_like_mobile,
                          if_learning_good_mobile) # only improved knowledge may be considered by parents before game subscription
                                                   # most likely they won't remember to connect dots with actual behavioral change in a short time
                                                   # how long do they think before making purchase? What are the important factors?
  
  board_sale_risk <- min(if_learning_good_board,
                         if_student_like_board,
                         parents_like_board) # same risk for board user 
  # risk for board game user and board game sale are separated because number of sale doesn't equal to number of user
  # multiple players can play a game set
  
  good_reputation_mobile_risk <- min (if_student_like_mobile,
                                      if_parents_like_mobile,
                                      if_learning_good_mobile,
                                      if_good_action_mobile)
  
  good_reputation_board_risk <- min (if_student_like_board,
                                     if_parents_like_board,
                                     if_learning_good_board,
                                     if_good_action_board)
  
  value_nutrition_risk_mobile <- min (if_learning_good_mobile,
                                      if_good_action_mobile)
  
  value_nutrition_risk_board <- min (if_learning_good_board,
                                     if_good_action_board)
  
  # Calculate total benefits for mobile game
  
  # Calculate income from mobile game subscription
  
  income_mobile_subscription <- vv (Income_mobile_batch_sale * Mobile_user_unit,
                                    var_CV, n_years,
                                    relative_trend = inflation_rate)* mobile_user_risk
  
  # Calculate extra income from good reputation for mobile game intervention
  
  value_good_reputation_mobile <- vv (Value_good_reputation,
                                      var_CV, n_years,
                                      relative_trend = inflation_rate)* good_reputation_mobile_risk
  
  value_good_reputation_mobile[1] <- 0 # value starts at year 2
  
  # Calculate saving from not needing formal nutrition course for both mobile and board game intervention
  
  saving_nutrition_course <- vv (Saving_nutrition_course,
                                 var_CV, n_years,
                                 relative_trend = inflation_rate)
  
  # Calculate value of adopting good eating habits with mobile game intervention
  
  value_nutrition_mobile <- vv (Value_nutrition * (Mobile_user_unit * behavioural_change_mobile),
                                var_CV, n_years,
                                relative_trend = inflation_rate)* value_nutrition_risk_mobile
  
  # Sum all benefits for mobile game
  
  benefit_mobile_total <- income_mobile_subscription + value_good_reputation_mobile +
                          saving_nutrition_course + value_nutrition_mobile
  
  # Calculate total benefits for board game intervention
  
  # Calculate total savings for board game intervention
  
  saving_board_total <- vv (Saving_board_game + saving_nutrition_course,
                            var_CV, n_years,
                            relative_trend = inflation_rate)
  
  # Calculate income from board game sale
  
  income_board_sale <- vv (Income_board_batch_sale * Board_sale_unit,
                           var_CV, n_years,
                           relative_trend = inflation_rate)* board_sale_risk
  
  # Calculate extra income from good school reputation with board game intervention
  
  value_good_reputation_board <- vv (Value_good_reputation,
                                     var_CV, n_years,
                                     relative_trend = inflation_rate)* good_reputation_board_risk
  
  value_good_reputation_board[1] <- 0 # value starts at year 2
  
  # Calculate value of adopting good eating habits with board game intervention
  # Divided by 1000 to get per 1000 value
  value_nutrition_board <- vv (Value_nutrition * ((Board_user * behavioural_change_board)/1000),
                               var_CV, n_years,
                               relative_trend = inflation_rate)* value_nutrition_risk_board
  
  # Sum all benefits for board game intervention
  
  benefit_board_total <- saving_board_total + income_board_sale + 
                         value_good_reputation_board + value_nutrition_board
  
  # Calculate net profit for mobile game intervention
  
  mobile_intervention_result <- benefit_mobile_total - cost_mobile_total
  
  # Calculate net profit for board game intervention
  
  board_intervention_result <- benefit_board_total - cost_board_total
  
  # Calculate NPV with discount rate
  
  NPV_mobile <- discount (x = mobile_intervention_result,
                          discount_rate = discount_rate,
                          calculate_NPV = TRUE)
  
  NPV_board <- discount (x = board_intervention_result,
                         discount_rate = discount_rate,
                         calculate_NPV = TRUE)
  
  return(list(NPV_mobile_game = NPV_mobile,
              NPV_board_game = NPV_board,
              total_costs_mobile = sum(cost_mobile_total),
              total_costs_board = sum(cost_board_total),
              Cashflow_mobile_game = mobile_intervention_result,
              Cashflow_board_game = board_intervention_result))
  
}

input_game <- read.csv("Game.csv")

# Run the Monte Carlo Simulation

Game_mc_simulation <- mcSimulation(estimate = estimate_read_csv("Game.csv"),
                                   model_function = Game_function,
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames")


# Plot distributions histogram

plot_distributions(mcSimulation_object = Game_mc_simulation,
                   vars = c("NPV_mobile_game", "NPV_board_game"),
                   method = 'hist_simple_overlay',
                   base_size = 7)

# Plot distributions boxplot

plot_distributions(mcSimulation_object = Game_mc_simulation,
                   vars = c("NPV_mobile_game", "NPV_board_game"),
                   method = 'boxplot')

# Plot distributions smooth overlay

plot_distributions(mcSimulation_object = Game_mc_simulation,
                   vars = c("NPV_mobile_game", "NPV_board_game"),
                   method = 'smooth_simple_overlay')

# Plot cashflows

plot_cashflow(mcSimulation_object = Game_mc_simulation,
              cashflow_var_name = "Cashflow_mobile_game")

plot_cashflow(mcSimulation_object = Game_mc_simulation,
              cashflow_var_name = "Cashflow_board_game")

plot_cashflow(mcSimulation_object = Game_mc_simulation,
              cashflow_var_name = c("Cashflow_board_game", "Cashflow_mobile_game"),
              x_axis_name = "Year",
              y_axis_name = "Cashlow in million Myanmar Kyat") 


#Find EVPI 

mcSimulation_table <- data.frame(Game_mc_simulation$x, 
                                 Game_mc_simulation$y[1:3])

evpi_mobile <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_mobile_game")
evpi_board <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_board_game")



plot_evpi(evpi_mobile, decision_vars = "NPV_mobile_game")
plot_evpi(evpi_board, decision_vars = "NPV_board_game")


#Find PLS result
pls_result <- plsr.mcSimulation(object = Game_mc_simulation,
                                resultName = names
                                (Game_mc_simulation$y)[1], 
                                ncomp = 1)

plot_pls(pls_result, input_table = input_game, threshold = 0)

# Summary

install.packages("gtExtras")
install.packages("svglite")
library(gtExtras)
library(svglite)
mcSimulation_summary <- data.frame(Game_mc_simulation$x[2:38], 
                                   Game_mc_simulation$y[1:3])

gt_plt_summary(mcSimulation_summary) 

# summary of cashflow
summary(Game_mc_simulation$y$Cashflow_mobile_game1)
summary(Game_mc_simulation$y$Cashflow_board_game1)

summary(Game_mc_simulation$y$NPV_mobile_game)
summary(Game_mc_simulation$y$NPV_board_game)
