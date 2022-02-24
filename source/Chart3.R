library(readr)
library("tidyverse")
library("tidyr")
library(ggplot2)
library("plyr")
library(usdata)
library("dplyr")


urlfile = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"

data <- read.csv(url(urlfile))

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