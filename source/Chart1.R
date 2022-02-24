library(readr)
library("tidyverse")
library("tidyr")
library(ggplot2)
library("plyr")
library(usdata)
library("dplyr")


urlfile = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"

data <- read.csv(url(urlfile))

tot_df <- data %>%
  select(year, county_name, black_pop_15to64, black_jail_pop, white_pop_15to64, white_jail_pop)

tot_df <- mutate_all(tot_df, ~replace(., is.na(.), 0))
tot_df <- mutate_all(tot_df, ~replace(., is.infinite(.), 0))

tot_df <- arrange(tot_df, year)

tot_df_sums <- ddply(tot_df, "year", numcolwise(sum))

tot_df_summary <- tot_df_sums <- filter(tot_df_sums, year >= 1990)

tot_df_summary <- mutate(
  tot_df_summary,
  black_percent_in_jail = black_jail_pop / black_pop_15to64,
  white_percent_in_jail = white_jail_pop / white_pop_15to64
)

ggplot(tot_df_summary, mapping = aes(x = year)) + 
  geom_line(aes(y=black_percent_in_jail, color = "black"), size = 1.5) +
  geom_line(aes(y=white_percent_in_jail, color = "white"), size = 1.5) +
  ggtitle("Percentage of Adult Population in Jail from 1990 to 2016") +
  xlab("Year") +
  ylab("Percentage") +
  labs(color = "Demographic")

