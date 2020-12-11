
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(plotly)
library("leaflet")


shootings <- read.csv("shootings-2018.csv", stringsAsFactors = FALSE)


# SUMMARY INFORMATION

# How many shooting events occurred?
shooting_number <- nrow(shootings)

# How many lives were lost?
total_killed <- sum(shootings$num_killed)

# How many people were injured?
total_injured <- sum(shootings$num_injured)

# Which city was most impacted by shootings (make sure to clarify
# how you are measuring "impact")?
# The city that was impacted the most(had the most shootings) in 2018 was
most_impacted_city <- shootings %>%
  group_by(city) %>%
  summarize(count = n()) %>%
  filter(count == max(count)) %>%
  pull(city)

# most impacted state
most_impacted_states <- shootings %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  filter(count == max(count)) %>%
  pull(state)


# What percentage of all victims were killed?
percentage_killed <- shootings %>%
  summarize(
    total = total_killed + total_injured,
    percentage = 100 - (round(total_injured / total, 3) * 100)
  ) %>%
  pull(percentage)




# PARTICULAR INCIDENT
most_deadly_incident <- shootings %>%
  top_n(1, wt = num_killed)

date <- most_deadly_incident$date
location <- paste(most_deadly_incident$address,
  most_deadly_incident$city,
  most_deadly_incident$state,
  sep = ", "
)
injured <- most_deadly_incident$num_injured
killed <- most_deadly_incident$num_killed
victims <- injured + killed





# SUMMARY TABLE
# of the top 10 states impacted by shootings.
# Columns: state, # of shootings, # total killed, # total injured
summary_table_2 <- shootings %>%
  group_by(state) %>%
  rename(State = state) %>%
  summarize(
    Shooting_Number = n(),
    People_Killed = sum(num_killed),
    People_Injured = sum(num_injured),
    Total_Victims = People_Killed + People_Injured
  ) %>%
  arrange(desc(Shooting_Number)) %>%
  top_n(10, wt = Shooting_Number)



# CHOROPLETH MAP OF SHOOTING OCCURENCES PER STATE

# Replace state with lowercase for joining two data sets
# and summarize the shooting occurences count per state
shootings_2 <- shootings %>%
  mutate(state = tolower(state)) %>%
  group_by(state) %>%
  summarize(count = n())

# join map data and shootings data summary together
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(shootings_2, by = "state") %>%
  rename(
    Longitude = long,
    Latitude = lat
  )

# Function that creates a choropleth map, with the color intensity
# of each state depending on the number of shootings that occured there.
make_shootings_map <- function(state_shape) {
  ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = Longitude, y = Latitude, group = group, fill = count),
      color = "gray", # show state outlines
      size = .1 # thinly stroked
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_gradient(low = "white", high = "darkred", na.value = "white") +
    labs(
      fill = "Number of Shootings",
      title = "Map of Shooting Occurences Per State in 2018",
      subtitle = "NA count indicate zero shootings"
    )
}

# convert the static map into a simple, interactive one
interactive_shootings_map <- ggplotly(make_shootings_map(state_shape))





# INTERACTIVE LEAFLET MAP FOR INDIVIDUAL SHOOTINGS

# make a new data frame with a new "label" column,
# which will be the label displayed when a state is clicked on
shootings_3 <- shootings %>%
  mutate(label = paste(
    "State: ", state, "<br>",
    "City: ", city, "<br>",
    "People killed:", num_killed
  ))

# create the interactive map
# set circle radius to exponentioanlly depend on the # of deaths at the shooting
# set color to red to thematically match the concept
leaflet_map <- leaflet(data = shootings_3) %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
  addCircles(
    lat = ~lat,
    lng = ~long,
    popup = ~label,
    radius = ~ num_killed^4.5,
    color = "red",
    fill = TRUE,
    fillOpacity = .8
  )


# COLUMN CHART
# Columns: # of shootings, # killed, # victims

# make the data frame of only CA, IL, FL
three_states <- shootings %>%
  group_by(state) %>%
  summarize(
    shooting_number = n(),
    killed_total = sum(num_killed),
    injured_total = sum(num_injured),
    victim_total = killed_total + injured_total
  ) %>%
  filter(state == "California" | state == "Illinois" | state == "Florida") %>%
  mutate(State = state)

# rearrange the dataframe to serve my graphing purposes
gathered <- three_states %>%
  gather(
    key = category,
    value = counts,
    -State
  ) %>%
  slice(4:15) %>%
  filter(category != "injured_total")


# This is code that the professor sent to me in order to fix my y axis problem.
# if I do not do this, and insert (gathered) into the function below this code,
# the column chart will have a y-axis that jumps around values
get_data <- function() {
  d <- read.csv(textConnection("
id,State,category,counts
1,California,shooting_number,35
2,Florida,shooting_number,30
3,Illinois,shooting_number,35
4,California,killed_total,50
5,Florida,killed_total,48
6,Illinois,killed_total,21
7,California,victim_total,186
8,Florida,victim_total,180
9,Illinois,victim_total,171
"))
  return(d)
}

# function to make the comparison column plot
make_comparison_plot <- function(df) {
  graph1 <- ggplot(df) +
    geom_col(
      mapping = aes(x = category, y = counts, fill = State),
      position = "dodge",
      color = "black"
    ) +
    scale_fill_manual(values = c(rep(c("orchid2",
                                       "purple4",
                                       "lightskyblue1")))) +
    labs(
      title = "California, Florida & Illinois Shooting Victim Comparison"
    )

  graph2 <- ggplotly(graph1)
  return(graph2)
}
