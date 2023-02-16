incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(tidyverse)
library(maps)
library(mapproj)


# Intro and Summary Information -------------------------------------------

summary(incarceration_data)  

# What is the average total jail pop across all states (in the 2018 year)
avg_total <- incarceration_data %>%
  filter(year == max(year)) %>% 
  summarise(avg_total_jail = mean(total_jail_pop, na.rm = T))

# Average black incarceration in 2018
avg_black_total <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  summarise(avg_black_jail = mean(black_jail_pop, na.rm = T))

# Average white incarceration in 2018
avg_white_total <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  summarise(avg_white_jail = mean(white_jail_pop, na.rm = T))
# Average latinx incarceration in 2018
avg_latinx_total <- incarceration_data %>%
  filter(year == max(year)) %>% 
  summarise(avg_latinx_jail = mean(latinx_jail_pop, na.rm = T))

# what is the sum of the total black, white and latino population in the most current year?
total_white <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  summarise(total_white = sum(white_pop_15to64)) %>% 
  pull(total_white)

total_latinx <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  summarise(total_latinx = sum(latinx_pop_15to64)) %>% 
  pull(total_latinx)

total_black <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  summarise(total_black = sum(black_pop_15to64)) %>% 
  pull(total_black)

total_blacklatinx <- total_black + total_latinx

# What is the difference in the avg. number of blacks jailed to whites jailed
# in 2018
black_latinx_jail <- incarceration_data %>%
  filter(year == max(year)) %>% 
  select(black_jail_pop, latinx_jail_pop) %>% 
  summarise(total_black_latinx_jail = sum(black_jail_pop, na.rm = T) +
              sum(latinx_jail_pop, na.rm = T)) %>% 
  pull(total_black_latinx_jail)

white_jail <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  select(white_jail_pop) %>% 
  summarise(avg_white_jail = mean(sum(white_jail_pop, na.rm = T))) %>% 
  pull(avg_white_jail)

blacklatinx_white_jailed_diff <- black_latinx_jail - white_jail

# What is the total jail population in 2018
total_jail <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = T)) %>% 
  pull(total_jail_pop)


# What is the prop of whites jailed to total white population? 
prop_white <- white_jail / total_white


# What state had the highest number of black_jail_incarcerations & in what year
highest_num_black <- incarceration_data %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>% 
  pull(state, year)
# What is the average black_jail_pop from 1986 - 2018?
black_jail_avg <- incarceration_data %>% 
  filter(year >= "1986" & year <= "2018") %>% 
  summarise(avg_black_jail = mean(black_jail_pop, na.rm = T)) %>% 
  pull(avg_black_jail)

white_jail_avg <- incarceration_data %>% 
  filter(year >= "1986" & year <= "2018") %>% 
  summarise(avg_white_jail = mean(white_jail_pop, na.rm = T)) %>% 
  pull(avg_white_jail)


# Chart 1: Trend over time ------------------------------------------------

# Trend in black jail population over time in 4 most populated states in each region
populated <- incarceration_data %>% 
  select(state, year, black_jail_pop) %>% 
  filter(state == "CA" | state == "FL" | state == "NY" |state == "TX") %>% 
  filter(year >= "1986" & year <= "2017")

populated_state_trend <- ggplot(data = populated)+
  geom_col(mapping = aes(x = year, y = black_jail_pop, fill = state))+
  labs(title = "Black inceration trends over time in 4 most populated states",
       x = "Year",
       y = "# of Blacks incarcerated")+
  facet_wrap(~state)

# Chart 2: 2+ continous variables ------------------------------------------

# jail incarceration by black, latinx and white race

distribution <- incarceration_data %>%
  select(year, total_jail_pop, black_jail_pop, white_jail_pop, latinx_jail_pop) %>% 
  group_by(year) %>% 
  filter(year > 1985) %>% 
  summarise(total_black_jail = sum(black_jail_pop, na.rm = T),
            total_white_jail = sum(white_jail_pop, na.rm = T),
            total_latinx_jail = sum(latinx_jail_pop, na.rm = T)) 


cont_variable_chart <- ggplot(data = distribution, aes(x = year, y = total_jail_pop)) +
  geom_line(mapping = aes(y = total_black_jail, color = "darkred"), size = 1)+
  geom_line(mapping = aes(y = total_white_jail, color = "steelblue"), size = 1)+
  geom_line(mapping = aes(y = total_latinx_jail, color = "green"), size = 1)+
  labs(title = "Trend of Total Black, Latinx, and White Jail Population",
       subtitle = "From 1985 - 2018",
       x = "Year",
       y = "Total Jail Population")+
  scale_color_discrete(name = "Race", labels = c("Black", "Latinx" , "White"))



# Chart 3: Map ------------------------------------------------------------

#  Map black_jail_pop by state
incarceration_mod <- incarceration_data %>%
  select(fips, year, county_name, black_jail_pop) %>% 
  filter(year == max(year, na.rm = T)) %>% 
  filter(! is.na(black_jail_pop))
# group_by(fips, county_name) %>% 
#summarise(total_black_pop = sum(black_jail_pop)) 

state_shape <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")
# merge map data and filtered incarceration data
map_data <- state_shape %>% 
  left_join(incarceration_mod, by = "fips") %>% 
  filter(county_name != "Unknown") 

blank_theme <- blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank())    # remove border around plot

# Map it 
incarceraton_map <- ggplot(data = map_data)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
               color = "gray", size = 0.2) +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)),
                        na.value = "white", low = "lightgray", high = "red")+
  blank_theme+
  coord_map()+
  ggtitle("Black Incarcerations in counties in the U.S")+
  labs(fill = "# of Blacks Incarcerated")
