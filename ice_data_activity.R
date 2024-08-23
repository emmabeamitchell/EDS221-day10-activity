library(tidyverse)
library(here)
temp_data <- read_csv(here("data", "ntl20_v6.csv"), na = "-999")
ice_data <-read_csv(here("data", "ntl33_v7.csv"), na = "-999")



ice_duration_hist <- ggplot(data = ice_data, aes(fill = lakeid, x = ice_duration)) +
  geom_histogram() +
  labs(title = "Ice Duration per Lake",
       x = "Ice Duration",
       y = "number of days of ice")

ice_duration_hist

Madison_ice_data <- ice_data |>
  group_by(year4) |>
  summarize(mean_ice_madison = mean(ice_duration))

ggplot(data = Madison_ice_data, aes(x = year4, y = mean_ice_madison)) +
  geom_line()
#ice duration trend going down

mean_air_temp <- temp_data |>
  filter(month %in% c(1,2,12)) |>
  group_by(year4) |>
  summarize(mean_temp = mean(ave_air_temp_adjusted))

mean_air_plot <- ggplot(mean_air_temp, aes(x = year4, y = mean_temp)) + geom_line()

mean_air_plot

mean_join <- left_join(mean_air_temp, Madison_ice_data)

join_graph <- ggplot(mean_join, aes(x = mean_ice_madison, y = mean_temp)) + geom_point()

join_graph

thaw_date_subset <- ice_data |>
  filter(year4 >= 1970) |>
  mutate(ice_off_date = ymd(ice_off)) |>
  mutate(ice_off_date_day = lubridate::yday(ice_off_date))


thaw_plot <- ggplot(thaw_date_subset, aes(x = year4, y = ice_off_date_day)) + geom_line()
