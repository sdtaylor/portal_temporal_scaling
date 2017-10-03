library(tidyverse)

# Note to future self. I'm currently using the seasonal average model with last year's
# obs heavily weighted. Just fixed a spline issue which gave weird results, and only
# tried a few things since. The current cost lost model is the best I've seen so far. 

temporal_scales = c(1,2,4,6)

upper_threshold = 1000
lower_threshold = 140

predictions = read_csv('results/portal_model_output.csv') 
temporal_model_sets = read.csv('temporal_model_sets.csv')


#x = predictions %>% gather(value_type, value, num_rodents, prediction)
#ggplot(x, aes(x=project_month, y=value, group=value_type, color=value_type)) + geom_line() + facet_wrap(~initial_month)

#ggplot(predictions, aes(x=project_month, y=prediction, group=initial_month, color=initial_month)) +
#  geom_line() +
#  #geom_hline(yintercept = upper_threshold, linetype=2) +
#  geom_hline(yintercept = lower_threshold, linetype=2) +
#  geom_line(aes(x=project_month, y=num_rodents), color='red', inherit.aes = FALSE)

#Use replicate_month to order the timeseries within each forecast replicate
predictions = predictions %>%
  group_by(initial_month, model) %>%
  arrange(project_month) %>%
  mutate(replicate_month = 1:12) %>%
  ungroup() %>%
  select(-month, -year)




predictions_binary = predictions %>%
    mutate(observed_binary = (num_rodents < upper_threshold & num_rodents > lower_threshold)*1,
          predicted_binary= (prediction < upper_threshold & prediction > lower_threshold)*1) %>%
  # mutate(observed_binary = ( num_rodents > lower_threshold)*1,
  #       predicted_binary= ( prediction > lower_threshold)*1) %>%
  select(initial_month, replicate_month, observed_binary, predicted_binary, model)

###########################################
scaled_predictions = temporal_model_sets %>%
  left_join(predictions_binary, by='replicate_month') %>%
  group_by(model, initial_month, temporal_scale, temporal_cell_id) %>%
  summarise(observed_binary = max(observed_binary),
            predicted_binary = max(predicted_binary)) %>%
  ungroup()

#The per month 
treatment_cost = 10
possible_loss_costs = 10 / seq(0.11, 0.99, 0.01)

#forecast outcomes given binary predictions
outcomes = data.frame(observed_binary=c(0,1,1,0),
                      predicted_binary=c(1,1,0,0),
                      type=c('fp','tp','fn','tn'))

#Gives cost per yr in terms of prescribed treatment vs actual losses
calculate_cost = function(df, treatment_cost, loss_cost, expense_type){
  temporal_scale = unique(df$temporal_scale)
  
  if(expense_type=='perfect'){
    df$predicted_binary = df$observed_binary
  } else if(expense_type=='always') {
    df$predicted_binary = 1
  } else if(expense_type=='never') {
    df$predicted_binary = 0
  } else if(expense_type=='forecast'){
    #No corrections done here
  } else {
    stop('No forecast type')
  }
  
  df = df %>%
    left_join(outcomes, by=c('observed_binary','predicted_binary')) %>%
    ungroup()
  
  total_tp_fp_months = sum(df$type %in% c('fp','tp')) * temporal_scale
  total_fn_months     = sum(df$type == 'fn') * temporal_scale
  total_cost = (treatment_cost * total_tp_fp_months) + (loss_cost * total_fn_months)
  
  total_replicates = df %>%
    select(initial_month, temporal_cell_id) %>%
    distinct() %>%
    nrow()
  
  total_monthly_replicates = total_replicates * temporal_scale
  print(paste(total_monthly_replicates))
  
  average_per_month_cost = total_cost / total_monthly_replicates
  return(average_per_month_cost)  
}


###############################################################################
#Calculate cost/loss model curve
cost_loss_values=data.frame()
#for(this_transect in unique(scaled_predictions_binary$Transect)){
for(this_model in unique(scaled_predictions$model)){
  for(this_loss_cost in possible_loss_costs){
    smallest_grain_data = scaled_predictions %>%
      filter(temporal_scale == min(temporal_scales), model==this_model)
    
    smallest_grain_perfect = calculate_cost(smallest_grain_data, treatment_cost = treatment_cost, 
                                            loss_cost = this_loss_cost, expense_type='perfect')
    smallest_grain_cost_never = calculate_cost(smallest_grain_data, treatment_cost = treatment_cost, 
                                               loss_cost = this_loss_cost, expense_type='never')
    smallest_grain_cost_maximimum = min(treatment_cost, smallest_grain_cost_never)
    print(smallest_grain_perfect)
    print(smallest_grain_cost_maximimum)
    for(this_temporal_scale in temporal_scales){
      this_scale_data = scaled_predictions %>%
        filter(temporal_scale == this_temporal_scale, model==this_model)
      
      this_scale_expense = calculate_cost(this_scale_data, treatment_cost = treatment_cost, 
                                          loss_cost = this_loss_cost, expense_type='forecast')
      
      cost_loss_values = cost_loss_values %>%
        bind_rows(data.frame('a' = treatment_cost / this_loss_cost,
                             'expense_max' = smallest_grain_cost_maximimum,
                             'expense_perfect' = smallest_grain_perfect, 
                             'expense_forecast' = this_scale_expense,
                             'temporal_scale' = this_temporal_scale,
                             'model' = this_model))
    }
      
  }
}
cost_loss_values$value = with(cost_loss_values, (expense_max - expense_forecast)/(expense_max - expense_perfect))


ggplot(cost_loss_values, aes(x=a, y=value, group=as.factor(temporal_scale), color=as.factor(temporal_scale))) +
  geom_line() + 
  ylim(0,1) +
  facet_grid(~model)
