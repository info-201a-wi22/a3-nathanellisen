library(readr)
library("tidyverse")
library("tidyr")
library(ggplot2)
library("plyr")
library(usdata)
library("dplyr")


urlfile = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"

data <- read.csv(url(urlfile))





# Value 1: What is the average county-wide percentage of males that were in prison in 2016

value_1_df <- data %>%
  select(year, county_name, male_pop_15to64, male_adult_jail_pop) %>%
  filter (year == 2016)

value_1_df <- mutate(
  value_1_df,
  men_jail_percent = male_adult_jail_pop / male_pop_15to64
)

value_1_df <- mutate_all(value_1_df, ~replace(., is.na(.), 0))
value_1_df <- mutate_all(value_1_df, ~replace(., is.infinite(.), 0))

value_1 <- mean(value_1_df$men_jail_percent, na.rm = TRUE)





# Value 2: White percentage of total population 15 to 64 in 2016

value_2_df <- data %>%
  select(year, county_name, total_pop_15to64, white_pop_15to64) %>%
  filter (year == 2016)

value_2_df <- mutate_all(value_2_df, ~replace(., is.na(.), 0))
value_2_df <- mutate_all(value_2_df, ~replace(., is.infinite(.), 0))

total_adult_pop_2016 <- sum(value_2_df$total_pop_15to64) 

white_adult_pop_2016 <- sum(value_2_df$white_pop_15to64)

value_2 <- white_adult_pop_2016 / total_adult_pop_2016





# Value 3: White percentage of jail population in 2016

value_3_df <- data %>%
  select(year, county_name, total_jail_pop, white_jail_pop) %>%
  filter (year == 2016)


value_3_df <- mutate_all(value_3_df, ~replace(., is.na(.), 0))
value_3_df <- mutate_all(value_3_df, ~replace(., is.infinite(.), 0))

total_jail_pop_2016 <- sum(value_3_df$total_jail_pop) 

white_jail_pop_2016 <- sum(value_3_df$white_jail_pop)

value_3 <- white_jail_pop_2016 / total_jail_pop_2016





# Value 4: King County Percentage of Population in Jail in 2016

value_4_df <- data %>%
  select(year, county_name, total_pop_15to64, total_jail_pop) %>%
  filter(county_name == "King County") %>%
  filter(year == 2016)

assumed_king_total_pop_2016 <- sum(value_4_df$total_pop_15to64)

assumed_king_jail_pop_2016 <- sum(value_4_df$total_jail_pop)

value_4 <- assumed_king_jail_pop_2016 / assumed_king_total_pop_2016





# Value 5: Santa Clara County Percentage of Population in Jail in 2016

value_5_df <- data %>%
  select(year, county_name, total_pop_15to64, total_jail_pop) %>%
  filter(county_name == "Santa Clara County") %>%
  filter(year == 2016)

santa_clara_total_pop_2016 <- sum(value_5_df$total_pop_15to64)

santa_clara_jail_pop_2016 <- sum(value_5_df$total_jail_pop)

value_5 <- santa_clara_jail_pop_2016 / santa_clara_total_pop_2016




## Trends Over Time chart


# Showing Percent of Black Population in Jail over 1990-2016

tot_df <- data %>%
  select(year, county_name, black_pop_15to64, black_jail_pop)

tot_df <- mutate_all(tot_df, ~replace(., is.na(.), 0))
tot_df <- mutate_all(tot_df, ~replace(., is.infinite(.), 0))

tot_df <- arrange(tot_df, year)

tot_df_sums <- ddply(tot_df, "year", numcolwise(sum))

tot_df_summary <- tot_df_sums <- filter(tot_df_sums, year >= 1990)

tot_df_summary <- mutate(
  tot_df_summary,
  black_percent_in_jail = black_jail_pop / black_pop_15to64
)

ggplot(tot_df_summary, mapping = aes(x = year, y = black_percent_in_jail)) +
  geom_point(size=5) +
  ggtitle("Percentage of Black Adult Population in Jail from 1990 to 2016") +
  xlab("Year") +
  ylab("Percentage") + 
  geom_line() +
  geom_smooth() 
  



## Variable Comparison Chart

# Black vs. White population in each state, largest to smallest (by total pop) in 2016

vcc_df <- data %>%
  select(year, state, total_pop, black_pop_15to64, white_pop_15to64) %>%
  filter(year == 2016)

vcc_df <- mutate_all(vcc_df, ~replace(., is.na(.), 0))
vcc_df <- mutate_all(vcc_df, ~replace(., is.infinite(.), 0))

vcc_df_sums <- ddply(vcc_df, "state", numcolwise(sum))

vcc_df_sums <- arrange(vcc_df_sums, -total_pop)

vcc_df_sums <- mutate(
  vcc_df_sums,
  black_percent_of_pop = black_pop_15to64 / total_pop,
  white_percent_of_pop = white_pop_15to64 / total_pop
)

library("tidyr")

vcc_long <- vcc_df_sums %>%
  gather("Stat", "Value", -state)

vcc_for_plot <- filter(vcc_long, Value < 1)

level_order <- as.vector(vcc_for_plot$state)

ggplot(vcc_for_plot, aes(x = level_order, y=Value, fill=Stat)) + 
  geom_col(position= "dodge") +
  ggtitle("Percentage of Black and White Residents in all 50 States Ranked Largest to Smallest") +
  xlab("State") +
  ylab("Percentage of Residents")





## Map

# Difference Between Black and White Encarceration Rates in 2016

map_df <- data %>%
  select(year, state, total_pop, black_pop_15to64, white_pop_15to64, black_jail_pop, white_jail_pop) %>%
  filter(year == 2016)

map_df <- mutate_all(map_df, ~replace(., is.na(.), 0))
map_df <- mutate_all(map_df, ~replace(., is.infinite(.), 0))

map_df <- mutate(
  map_df,
  percent_black_map = (black_jail_pop / black_pop_15to64),
  percent_white_map = (white_jail_pop / white_pop_15to64)
)

map_df <- mutate(
  map_df,
  difference = percent_black_map - percent_white_map
)

map_df_sums <- ddply(map_df, "state", numcolwise(sum))

map_df_2 <- select(map_df_sums, state, difference)

map_df_2 <- mutate_all(map_df_2, ~replace(., is.na(.), .01))
map_df_2 <- mutate_all(map_df_2, ~replace(., is.infinite(.), .01))

map_df_2 <- mutate(map_df_2, state = abbr2state(state))

map_df_for_plot <- mutate(
  map_df_2, 
  state = tolower(state),
  difference = difference * 1
  )

state_shape <- map_data("state") %>%
  dplyr::rename(state = region) %>% 
  left_join(map_df_for_plot, by="state") 

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )


ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = difference),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Percent Difference (Black minus White)") +
  ggtitle("The Clear Difference in Incarceration between Blacks and Whites") +
  blank_theme





