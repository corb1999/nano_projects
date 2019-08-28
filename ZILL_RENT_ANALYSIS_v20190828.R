#
#### INTRO ####
# EXPLORE ZILLOW RENTAL PRICE DATA TO ANALYZE WHERE TO LIVE
# DATA FROM HERE = https://www.kaggle.com/zillow/rent-index
# v2019.08.28
#### INTRO ^^^ ####
#

#### LOAD LIBRARIES -------------------------------------------------- ####
library(tidyverse)
library(lubridate)
library(reshape2)
library(data.table)
library(tidylog)
library(ggmap)
library(ggridges)
library(DataExplorer)
library(psych)
clock_a <- now()
#### LOAD LIBRARIES ^^^ ####

#### DATA LOADS ===================================================== ####

# Load the price dataset which was downloaded from kaggle
clock_1 <- now()
xx <- read.csv("price.csv")
clock_2 <- now()
time_load_data_price <- clock_2 - clock_1

# Load the price per sqrft dataset which was downloaded from kaggle
clock_1 <- now()
yy <- read.csv("pricepersqft.csv")
clock_2 <- now()
time_load_data_ppsf <- clock_2 - clock_1

c(time_load_data_price, time_load_data_ppsf)

dim(xx)
dim(yy)

# Sampling some rows to get a flavor of the data
# xxxx <- xx[sample(1:nrow(xx), 250, replace = FALSE), 1:10]
# yyyy <- yy[sample(1:nrow(yy), 250, replace = FALSE), 1:10]
# View(xxxx)
# View(yyyy)

# The folowing column selections will be used in the following melt manipulation
xx1 <- xx[, 1:6]
yy1 <- yy[, 1:6]
xx2 <- xx[, 7:81]
yy2 <- yy[, 7:81]


#### DATA LOADS ^^^ #####

#### WRANGLE 1 -------------------------------------------------------- ####

# My manual workaround for manipulating the dates (temporary object)
leto <- as.character(c("January", "February", "March", "April", "May", "June",
          "July", "August", 
          "September", "October", "November", "December"))
ghanima <- c(paste0("0", 1:9), "10", "11", "12")
muad_dib <- data.frame(mon = leto, 
                       month_num = ghanima) %>% 
  mutate(mon = as.character(mon))
rm(leto, ghanima)

price <- xx %>%
  filter(Population.Rank <= 1000) %>% 
  melt(id.vars = colnames(xx1), 
       variable.vars = colnames(xx2), 
       variable.name = "yrMonth", 
       value.name = "medianPrice"
       ) %>% 
  rename(city_code = City.Code, 
         city = City, 
         metro = Metro, 
         pop_rank = Population.Rank
         ) %>% 
  tbl_df() %>% 
  mutate(charTIME = as.character(yrMonth), 
         yr = str_sub(charTIME, 
                      start = str_length(charTIME)-3, 
                      end = str_length(charTIME)), 
         mon = str_sub(charTIME, 
                         start = 0, 
                         end = str_length(charTIME)-5)
         ) %>% 
  select(city_code:pop_rank, 
         yr, mon, 
         medianPrice)
price
# Checks to make sure that the melt worked correctly
# price[995:1005, ]
# price[1995:2005, ]
# price[9995:10005, ]

# Similar to wrangle above, melt the price per square foot down
ppsqr <- yy %>% 
  filter(Population.Rank <= 1000) %>% 
  melt(id.vars = colnames(yy1), 
       variable.vars = colnames(yy2), 
       variable.name = "yrMonth", 
       value.name = "ppsqrft"
        ) %>% 
  rename(city_code = City.Code, 
         city = City, 
         metro = Metro, 
         pop_rank = Population.Rank
          ) %>% 
  tbl_df() %>% 
  mutate(charTIME = as.character(yrMonth), 
         yr = str_sub(charTIME, 
                      start = str_length(charTIME)-3, 
                      end = str_length(charTIME)), 
         mon = str_sub(charTIME, 
                         start = 0, 
                         end = str_length(charTIME)-5)
        ) %>% 
  select(city_code, yr, mon, ppsqrft) 
ppsqr

# Perform a join on the two tables to make one single DF
p0 <- left_join(price, ppsqr, by = c("city_code", "yr", "mon"))
p1 <- p0 %>% 
  mutate(sqrft = medianPrice / ppsqrft, 
         mon = as.character(mon), 
         yr = as.character(yr)
         ) %>% 
  left_join(., muad_dib, by = "mon") %>% 
  mutate(yrmonth = paste0(yr, month_num, "01"), 
         yr = as.factor(yr), 
         mon = as.factor(mon), 
         yrmonth = ymd(yrmonth))
p1

# Clean up
rm(xx, yy, xx1, xx2, yy1, yy2, p0, price, ppsqr, muad_dib)
ls()
gc(verbose = TRUE)


#### WRANGLE1 ^^^ ####

#### WRANGLE 2 -------------------------------------------------------------- ####

# My personal selection of states that I wanted to examine more closely
fave_states <- c("CT", "CA", "FL", "OR", "WA", 
         "VA", "MA", "RI", "TX", "IL", 
         "CO", "RI", "VT", "ME", "NH", 
         "PA", "AZ", "NJ", "NY", "NC")

# Aggregation by city_code (imporant because some cities share same name)
psmry1 <- p1 %>% 
  filter(!is.na(medianPrice)) %>% 
  group_by(city_code) %>% 
  summarise(city = first(city), 
            metro = first(metro), 
            county = first(County), 
            state = first(State), 
            pop_rank = first(pop_rank), 
            records = n(), 
            meanPRICE = mean(medianPrice), 
            meanPPSQRFT = mean(ppsqrft, na.rm = TRUE), 
            meanSQRFT = mean(sqrft, na.rm = TRUE)
            ) %>% 
  arrange(pop_rank)
psmry1

# Aggregation by state
psmry2 <- p1 %>% 
  filter(!is.na(medianPrice)) %>% 
  group_by(State) %>% 
  summarise(cities = n_distinct(city), 
            metros = n_distinct(metro), 
            counties = n_distinct(County), 
            pop_rank = min(pop_rank), 
            records = n(), 
            meanPRICE = mean(medianPrice), 
            meanPPSQRFT = mean(ppsqrft, na.rm = TRUE), 
            meanSQRFT = mean(sqrft, na.rm = TRUE), 
            price25 = quantile(medianPrice, probs = 0.25),
            price75 = quantile(medianPrice, probs = 0.75)
            ) %>% 
  arrange(pop_rank)
psmry2

# Clean up
ls()
gc(verbose = TRUE)


#### WRANGLE 2 ^^^ ####

#### VIZ =========================================================== ####

plt1 <- p1 %>% filter(yr %in% c(2012:2016)) %>% 
  group_by(yrmonth) %>% 
  summarise(medPrice = median(medianPrice), 
            meanPrice = mean(medianPrice)
            ) %>% 
  ggplot(aes(x = yrmonth, y = medPrice)) + 
  geom_line(size = 2.5, color = "#ffad4a") + 
  geom_line(aes(y = meanPrice), size = 2.5, 
            color = "#69b3e7") + 
  annotate("text", x = ymd(20130101), y = 1220, 
           label = "Median Rent Price", 
           color = "#ffad4a", size = 6) + 
  annotate("text", x = ymd(20130101), y = 1550, 
           label = "Mean Rent Price", 
           color = "#69b3e7", size = 6) + 
  scale_x_date(name = "Observed Month", 
               date_breaks = "years") + 
  scale_y_continuous(name = "Rent Prices Aggregated", 
                     breaks = c(seq(1200, 2000, 50)), 
                     limits = c(1200, 1800), 
                     labels = scales::dollar) + 
  theme_bw() + 
  labs(title = "Mean and Median Rent Prices in USA Cities")
plt1
rm(plt1)
# NOTES:
# As a whole, American rent costs have been steadily increasing over time

plt2 <- p1 %>%
  filter(pop_rank <= 20) %>% 
  ggplot(aes(x = medianPrice, 
             y = reorder(city, desc(pop_rank)), 
             color = yr)) + 
  geom_jitter(size = 2, 
              width = 0.01, 
              height = 0.2) + 
  scale_x_continuous(labels = scales::dollar, 
                     breaks = c(seq(750, 5000, by = 250))) + 
  theme_bw() + 
  labs(title = "City-Level Rent Trends, Top 20", 
       color = "Observed Year") + 
  xlab("Median Rent Price") + 
  ylab("City, ordered by population rank")
plt2
rm(plt2)
# NOTES:
# This exhibit clearly shows that some cities show dramatic median
#   rent cost increases over time, while others are relatively stable
# CA cities are showing huge increases in rent costs over time

plt3 <- p1 %>% 
  filter(State %in% psmry2$State[1:20], 
         pop_rank <= 200
         ) %>% 
  ggplot(aes(x = medianPrice, 
             y = State,  
             color = yr)) + 
  geom_jitter(size = 1.2, 
              width = 0.1, 
              height = 0.1) + 
  scale_x_continuous(name = "Median Rent Price", 
                     labels = scales::dollar, 
                     breaks = c(seq(750, 5000, by = 250))) +
  theme_bw() + 
  labs(title = "Top 200 Cities in 20 Largest States Rent Trends") + 
  ylab("State")
plt3
rm(plt3)
# NOTES:
# When aggregating multiple cities on the state level, the time trend 
#   becomes less pronounced as local markets are mixed together
#   but new state dynamics appear
# States like NY, MA, IL, AZ have clearly different rental markets as
#   evidenced by the large gaps.  This makes sense if you think about NYC 
#   versus upstate NY, but this exhibit can show how state-level rent
#   figures can tell an incomplete story

plt4 <- p1 %>% 
  filter(State == "CT", pop_rank <= 800) %>% 
  ggplot(aes(x = medianPrice, 
             y = reorder(city, medianPrice), 
             color = yr)) + 
  geom_jitter(size = 2, 
              width = 0.01, 
              height = 0.3) + 
  geom_point(aes(x = mean(medianPrice), y = city), 
             size = 2, color = "black") + 
  scale_x_continuous(labels = scales::dollar, 
                     breaks = c(seq(750, 5000, by = 250))) + 
  theme_bw() + 
  labs(tag = "CT", 
       title = "State Summary", color = "Observed Year") + 
  xlab("Median Rent Price") + 
  ylab("City")
plt4
rm(plt4)
# NOTES:
# Alter the state abbreviation in the filter clause to isolate 
#   a particular state and see how cities compare on rent prices

plt5 <- psmry2 %>% 
  filter(pop_rank <= 25) %>% 
  ggplot(aes(x = reorder(State, desc(pop_rank)), 
             y = meanPRICE)) + 
  geom_point(aes(y = price25), size = 2.5, color = "#f0b323") + 
  geom_point(aes(y = price75), size = 2.5, color = "#f0b323") + 
  geom_segment(aes(y = price25, xend = State, yend = price75), 
               color = "#f0b323") + 
  geom_point(color = "#1d4f91", size = 5.5) + 
  geom_label(aes(label = paste("Cities:", cities, sep = " ")), 
             hjust ="bottom", 
             size = 4) + 
  scale_y_continuous(labels = scales::dollar, 
                     breaks = c(seq(500, 3000, by = 250))) + 
  theme_bw() + 
  coord_flip() + 
  labs(title = "Observed Rent Prices Grouped by 25 Large States", 
       caption = "Blue point is the mean of all city yr/month median prices. 
                    Yellow points indicate 25th and 75th quantile prices.") + 
  xlab("State") + 
  ylab("Rent Price")
plt5
rm(plt5)

plt6 <- p1 %>% 
  filter(!is.na(medianPrice), 
         State == fave_states) %>% 
  ggplot(aes(x = medianPrice, 
             y = reorder(State, medianPrice), 
             fill = ..x..)
         ) + 
  geom_density_ridges_gradient(rel_min_height = 0.01) + 
  scale_fill_viridis_c(name = "Rent Scale", option = "C") +
  scale_x_continuous(labels = scales::dollar) + 
  theme_bw() + 
  labs(title = "Select State's Rent Price Distributions") + 
  xlab("Median Rent Prices $") + 
  ylab("States, ordered by Median Price")
plt6
rm(plt6)


#### VIZ ^^^ ####

#### END ####
clock_b <- now()
time_script_run <- clock_b - clock_a
time_load_data_price
time_load_data_ppsf
time_script_run
ls()
gc(verbose = TRUE)

# rm(list = ls())

# ps ________________________________________________________________ ####
# Next steps:
# Try forecasting/projecting prices out
# Try getting the data into map vizualizations