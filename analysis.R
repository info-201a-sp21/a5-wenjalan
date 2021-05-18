library("dplyr")
library("plotly")
library("leaflet")
library("ggplot2")
library("lubridate")

# load data
data <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# returns a list of data used for the summary
get_summary_info <- function(data) {
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
  return(info)
}

# returns a table with some summarizing information
get_summary_table <- function(data) {
  # testing: data = shootings
  # data = shootings

  # create a casualties column
  # choose the date, city, state, and casualties columns
  # sort table by state, then city
  table_data <- data %>%
    group_by(
      city
    ) %>%
    summarize(
      state = state,
      city = city,
      casualties = sum(num_killed) + sum(num_injured)
    ) %>%
    distinct(
      city,
      state,
      casualties,
    ) %>%
    arrange(
      -casualties
    )

  # create the table itself
  table <- plot_ly(
    type = "table",
    header = list(
      values = c("City", "State", "Casualties")
    ),
    cells = list(
      values = rbind(table_data$city, table_data$state, table_data$casualties)
    )
  )

  # return the table
  return(table)
}

# returns information about the shooting with the most casualties
get_most_casualties_info <- function(data) {
  # list to return
  info <- list()

  # get the one with the most casualties
  most_casualties_shooting <- data %>%
    mutate(
      casualties = num_killed + num_injured
    ) %>%
    filter(
      casualties == max(casualties)
    )

  # save the date, location, num injured, and num killed
  info$date <- most_casualties_shooting$date
  info$state <- most_casualties_shooting$state
  info$city <- most_casualties_shooting$city
  info$lat <- most_casualties_shooting$lat
  info$long <- most_casualties_shooting$long
  info$n_killed <- most_casualties_shooting$num_killed
  info$n_injured <- most_casualties_shooting$num_injured

  # return
  return(info)
}

# returns an interactive map of the incidents
get_interactive_map <- function(data) {
  # create the map
  map <- leaflet(data = data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      ~long,
      ~lat,
      radius = ~ (num_killed + num_injured),
      popup = ~ paste(
        "<p>Killed:",
        num_killed,
        "<br/>",
        "Injured: ",
        num_injured,
        "<br/>",
        "Date: ",
        date,
        "</p>"
      )
    )

  # return the map
  return(map)
}

# returns a bar plot of number of casualties in each state
get_bar_plot <- function(data) {
  # create a summary frame for easy graphing
  graph_data <- data %>%
    group_by(
      state
    ) %>%
    summarize(
      state = state.abb[match(state, state.name)],
      casualties = sum(num_killed) + sum(num_injured)
    ) %>%
    distinct(
      state,
      casualties
    )

  # create the plot
  plot <- ggplot(
    data = graph_data,
    aes(
      x = reorder(state, -casualties),
      y = casualties
    )
  ) +
    ggtitle(
      "Casualties by State"
    ) +
    xlab(
      "State"
    ) +
    ylab(
      "Casualties"
    ) +
    geom_bar(
      stat = "identity",
      width = 0.5,
    )

  # wrap it in plotly?
  plot <- ggplotly(plot)

  # return the plot
  return(plot)
}
