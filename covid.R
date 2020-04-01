#### Death time line

library(tidyverse)
library(lubridate)

# input data
time_series_deaths <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  rename(Province_State = "Province/State", Country_Region = "Country/Region")

# wide to long format
time_series_deaths_long <- time_series_deaths %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long),
               names_to = "Date", values_to = "Deaths") %>% 
  group_by(Country_Region,Date) %>% 
  summarise(Deaths = sum(Deaths))

# convert date to data format
time_series_deaths_long$Date <- mdy(time_series_deaths_long$Date)

# plot data
time_series_deaths_long %>% 
  filter (Country_Region %in% c("China","France","Italy", "Spain", 
                                "Korea, South", "US")) %>% 
  ggplot(aes(x = Date,  y = Deaths, color = Country_Region)) + 
  geom_point() +
  geom_line() +
  ggtitle("COVID-19 Deaths")


# input confirmed time series
time_series_confirmed <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  rename(Province_State = "Province/State", Country_Region = "Country/Region")

# wide to long format
time_series_deaths_long <- time_series_deaths %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long),
               names_to = "Date", values_to = "Deaths")
head(time_series_deaths_long)

# wide to long
time_series_confirmed_long <- time_series_confirmed %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long),
               names_to = "Date", values_to = "Confirmed")
head(time_series_confirmed_long)

# convert date to data format
time_series_confirmed_long$Date <- 

time_series_confirmed_long <- time_series_confirmed_long %>% 
  unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)

time_series_deaths_long <- time_series_deaths_long %>% 
  unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
  select(Key, Deaths)

time_series_long_joined <- left_join(time_series_confirmed_long,
              time_series_deaths_long, by = c("Key")) %>% 
    select(-Key)
head(time_series_long_joined)

# Also clean up / separate wide to long from group.