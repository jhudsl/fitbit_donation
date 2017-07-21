library(tidyverse)
library(wavelets)
fitbit <- read_csv("~/Downloads/my_fitbit_data.csv") %>% 
  select(-X1)

fitbit %>% 
  ggplot(aes(x = time, y = value, color = type)) +
  geom_line() + 
  facet_wrap(~date)


#tag workout times in a super inexact way
time_to_secs <- function(hour, min) hour*3600 + min*60

working_out_start <- time_to_secs(7,05)
working_out_end <- time_to_secs(9,25)
weekend <- c("2017-06-24", "2017-06-25")

fitbit_workout <- fitbit %>% 
  mutate(working_out = ifelse(time > working_out_start & time < working_out_end, 1, 0)) %>% 
  mutate(workout_out = ifelse(date %in% weekend, 0, workout_out))

fitbit_workout %>% 
  ggplot(aes(x = time, y = value, group = type, color = working_out)) +
  geom_line() + 
  facet_wrap(~date)

#grab one day
one_day <- fitbit %>% 
  filter(date == "2017-06-27" & type == "heart rate") %>% 
  mutate(value = as.numeric(value))

#plot the raw data for the day
one_day %>% 
  ggplot(aes(x = time, y = value, color = type)) +
  geom_line() 


#do a windowed discrete wavelet transform 
wt <- modwt(one_day$value, n.levels=8, filter = "la8") %>% align()

#plot results to see how well it worked


wtData <- do.call(cbind, wt@W) %>% as_data_frame()

wtData %>% 
  mutate(index = 1:length(V1)) %>% 
  gather(layer, value, -index) %>% 
  ggplot(aes(x = index, y = value)) + 
    geom_line() + 
    facet_wrap(~layer)

#extract the data. This needs work as it's a single vector now. 
 unlist(
  c(
    wt@W,
    wt@V[[wt@level]]
   )
  ) %>% as_data_frame()

library(purrr)

lapply(wt@W, FUN = function(layer) layer )

names(wt@W)


