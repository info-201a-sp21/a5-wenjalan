library('dplyr')

# load data
shootings <- read.csv('data/shootings-2018.csv', stringsAsFactors = FALSE)

# returns a list of data used for the summary
get_summary_info <- function(data) {
  # testing: data = shootings
  data = shootings
  
  # list to return
  info <- list()
  
  # n shootings
  info$n_shootings <- nrow(data)
  
  # n lives lost
  info$n_lives_lost <- data %>%
    summarize(
      n_lives_lost = sum(num_killed)
    ) %>%
    pull(
      n_lives_lost
    )
  
  # max city impact: max number of deaths + injuries (casualites)
  info$most_impacted_city <- data %>%
    group_by(
      city
    ) %>%
    summarize(
      num_casualties = sum(num_killed) + sum(num_injured) 
    ) %>%
    filter(
      num_casualties == max(num_casualties)
    ) %>%
    pull(
      city
    )
  
  # average deaths per shooting
  info$avg_deaths <- data %>%
    summarize(
      avg_deaths = mean(num_killed)
    ) %>%
    pull(
      avg_deaths
    )
    
  
  # average injuries
  info$avg_injuries <- data %>%
    summarize(
      avg_injuries = mean(num_injured)
    ) %>%
    pull(
      avg_injuries
    )
  
  # return the list
  return (info)
}