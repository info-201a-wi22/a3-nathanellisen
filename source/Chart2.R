library(readr)
library("tidyverse")
library("tidyr")
library(ggplot2)
library("plyr")
library(usdata)
library("dplyr")


urlfile = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"

data <- read.csv(url(urlfile))

vcc_df <- data %>%
  select(year, state, total_pop, black_pop_15to64, white_pop_15to64) %>%
  filter(year == 2016)

vcc_df <- mutate_all(vcc_df, ~replace(., is.na(.), 0))
vcc_df <- mutate_all(vcc_df, ~replace(., is.infinite(.), 0))

vcc_df_sums <- ddply(vcc_df, "state", numcolwise(sum))

vcc_df_sums <- arrange(vcc_df_sums, -total_pop)

vcc_df_sums <- filter(vcc_df_sums, total_pop > 7000000)

vcc_df_sums <- mutate(
  vcc_df_sums,
  black = black_pop_15to64 / total_pop,
  white = white_pop_15to64 / total_pop
)

vcc_long <- vcc_df_sums %>%
  gather("Stat", "Value", -state)

vcc_for_plot <- filter(vcc_long, Value < 1)

level_order <- as.vector(vcc_for_plot$state)

ggplot(vcc_for_plot, aes(x = level_order, y=Value, fill=Stat)) + 
  geom_col(position= "dodge") +
  ggtitle("Percentage of Black and White Residents in 13 Largest States") +
  xlab("State") +
  ylab("Percentage of Residents") +
  scale_color_hue(labels = c("Black", "White")) +
  labs(fill = "Demographic")

