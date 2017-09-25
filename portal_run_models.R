#Uncomment these two lines to install the required packages
#install.packages(c('tidyverse','devtools'))
#devtools::install_github('weecology/portalr')
library(portalr)
library(tidyverse)
library(tscount)


model_output_file = 'results/portal_model_output.csv'

########################################################################
########################################################################
#The portalr package is made to deal specifically with this dataset.
#This function will download the most up to date data into the 
#current project working directory
portalr::download_observations(base_folder = '.')

########################################################################
########################################################################

rodent_counts = portalr::abundance(path='.', level='Site', unknowns = TRUE, incomplete = FALSE,
                                 shape='flat') %>%
  group_by(period) %>%
  summarize(num_rodents = sum(abundance))

#There are ~24 species or rodents seen at the site. In the raw data above
#species are only identified by a 2 letter code. This data.frame will associate
#the 2 letter code with the  full names as well as other info
species_info = read.csv('PortalData/Rodents/Portal_rodent_species.csv')

# This data is organized by periods. Period 1 is the first trapping session
# in 1977. The period is incrimented by 1 every month trapping is done. 
# Information about all the periods, such as dates and which of 24 plots were
# sampled, is in this data.frame
trapping_info = read.csv('PortalData/Rodents/Portal_rodent_trapping.csv')


# Some trapping periods start on the last day of the month, so they'll span
# two months if you summarize naively. This assigned each period to a single 
# month,year
period_months = trapping_info %>%
  group_by(period) %>%
  summarize(month = min(month), year=min(year))

# Add in the non-naive year,month 
rodent_counts = rodent_counts %>%
  left_join(period_months, by='period')

# 2 trappings in a single calendar month? Average the two into one
rodent_counts = rodent_counts %>%
  group_by(year, month) %>%
  summarise(num_rodents = as.integer(mean(num_rodents))) %>%
  ungroup()
  
# Interpolate missing sampling periods with a spline smoother

all_months = expand.grid(year=1978:2016, month=1:12)
rodent_counts = rodent_counts %>%
  right_join(all_months, by=c('year','month'))

rodent_counts$project_month = with(rodent_counts, (year-1977)*12 + month-1)

smoother = with(filter(rodent_counts, !is.na(num_rodents)), smooth.spline(x=project_month, y=num_rodents))

rodent_counts$interpolated_num_rodents = predict(smoother, min(rodent_counts$project_month):max(rodent_counts$project_month))$y

rodent_counts = rodent_counts %>%
  mutate(num_rodents = ifelse(is.na(num_rodents), interpolated_num_rodents, num_rodents)) %>%
  mutate(num_rodents = as.integer(num_rodents)) %>%
  select(-interpolated_num_rodents)

rm(all_months, smoother, period_months)
########################################################################
testing_years = c(2000:2016)

predictions = data.frame()

for(this_testing_year in testing_years){
  this_subset_training_data = rodent_counts %>%
    filter(year<this_testing_year)
  this_subset_testing_data = rodent_counts %>%
    filter(year==this_testing_year)
  model = tsglm(this_subset_training_data$num_rodents, model=list(past_obs=1, past_mean=12), distr = 'nbinom')
  
  this_subset_testing_data$prediction = predict(model, 12)$pred
  this_subset_testing_data$initial_year = this_testing_year-1
  
  predictions = predictions %>%
    bind_rows(this_subset_testing_data)
}

write_csv(predictions, model_output_file)

