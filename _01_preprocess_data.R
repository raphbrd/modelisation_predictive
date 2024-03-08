rm(list = objects())
graphics.off()
library(tidyverse)
library(lubridate)
library(quantreg)
library(mgcv)
library(readxl)
library(qgam)
library(mgcv)

# custom utilities scripts
source('utils/score.R')
source('utils/utils.R')

setwd("/Users/raphaelbordas/Desktop/Master_2023_24/S2 - Modélisation prédictive/challenge_net_load")

### Data preprocessing ####

# Data0 are train and validation data
# Data1 are test data
Data0 <- read_delim("data/net-load-forecasting/train.csv",
                    delim = ",")
Data1 <- read_delim("data/net-load-forecasting/test.csv",
                    delim = ",")

# Load, Net_demand, Solar_power and Wind_power are not in Data1
setdiff(colnames(Data0), colnames(Data1))

#' variable transformations : 
#' - Set a numeric Date named Time
#' - Transforms WeekDays in various factors
transforms <- function(df) {
  return(
    df %>% mutate(
      Time = as.numeric(Date),
      WeekDays = as.factor(WeekDays),
      WeekDays2 = weekdays(Date),
      WeekDays3 = forcats::fct_recode(
        WeekDays2,
        'WorkDay' = 'Thursday',
        'WorkDay' = 'Tuesday',
        'WorkDay' = 'Wednesday'
      )
    )
  )
}

Data0 <- transforms(Data0)
Data1 <- transforms(Data1)

#' Stringency index
#' 
#' https://ourworldindata.org/coronavirus/country/france
stringency <- read.csv("data/misc_data/stringency_idx_france.csv")
stringency_idx <- stringency %>% 
  select(date, stringency_index) %>%
  rename(Date = date) %>%
  mutate(Date = date(Date))
Data0 <- Data0 %>% 
  left_join(stringency_idx, by = "Date")
Data1 <- Data1 %>% 
  left_join(stringency_idx, by = "Date")
# by default, stringency index is 0 before Covid
Data0$stringency_index[is.na(Data0$stringency_index)] <- 0
Data1$stringency_index[is.na(Data1$stringency_index)] <- 0

# binary variables for lockdowns

Data0 <- mutate(Data0,confinement1 = ifelse(Data0$Date >= as.Date("2020-03-17") & Data0$Date 
                                            <= as.Date("2020-05-11"),1,0),
                confinement2 = ifelse(Data0$Date >= as.Date("2020-10-30") & Data0$Date <= as.Date("2020-12-15"),1,0),
                confinement3 = ifelse(Data0$Date >= as.Date("2021-04-03") & Data0$Date <= as.Date("2021-05-03"),1,0))

Data0$confinement1 <- factor(Data0$confinement1)
Data0$confinement2 <- factor(Data0$confinement2)
Data0$confinement3 <- factor(Data0$confinement3)

Data1 <- mutate(Data1, confinement1 = 0, confinement2 = 0, confinement3 = 0)

Data1$confinement1 <- factor(Data1$confinement1)
Data1$confinement2 <- factor(Data1$confinement2)
Data1$confinement3 <- factor(Data1$confinement3)

# restrict the range of traning data
Data0 <- Data0[Data0$stringency_index < 80, ]
sel_a <- which(Data0$Year >= 2018 & Data0$Year <= 2021) # training
sel_b <- which(Data0$Year > 2021)

# Test data : this is the main target of this challenge
range(Data1$Date) # from 2022-09-02 to 2023-10-01

train_data <- Data0[sel_a, ]
val_data <- Data0[sel_b, ]
test_data <- Data1

# second version of the training set : randomely selecting
# validation points over the last 2 years of the training data
set.seed(42)
tmp <- Data0[Data0$Year >= 2018, ]
sel_c <- sample(which(tmp$Year >= 2021), 365, replace = FALSE)
train_data2 <- tmp[-sel_c, ]
val_data2 <- tmp[sel_c, ]

# cross-validation indexes 
# train_data and val_data should be merged in that case
block_list <- get_cv_blocks(nrow(train_data) + nrow(val_data), K = 8)
