# A test of tag prediction
# devtools::install_github("krlmlr/here")
library(here)
library(tidyverse)
library(lubridate)

rawData <- here::here("tagPrediction/raw_data.csv") %>% read_csv() %>% select(-X1)
tagData <- here::here("tagPrediction/activity_tags.csv") %>% read_csv() %>% select(-X1)

#takes data in the standard report format and adds the column totalMins which starts on the first day and counts up. 
makeLong <- function(data){
  minsInDay <- 24*60
  
  data %>% 
    group_by(type) %>% 
    arrange(date) %>% 
    mutate(day = as.integer(difftime(date, min(date), units = "days"))) %>% 
    mutate(minute = time/60,
           totalMins = (day*minsInDay) + minute) 
}

long_data <- rawData %>% 
  makeLong() %>% 
  select(value, type, totalMins)


# Create a dataframe with every minute in it
maxMinute <- max(long_data$totalMins)
fullData <- data_frame(
  totalMins = rep(seq(0, maxMinute), each = 2),
  type = rep(c("heart rate", "steps"), times = maxMinute + 1)
) %>% 
  left_join(long_data, by = c("totalMins", "type")) 

# subset data into heartrate and steps for now.
hrData <- fullData %>% filter(type == "heart rate") %>% fill(value)
stepsData <- fullData %>% filter(type == "steps") %>% fill(value)
combinedData <- hrData %>% bind_cols(stepsData) %>% select(hr = value, steps = value1) %>% as.matrix()

# Make into time series objects
tsHr<- ts(hrData$value,frequency=1)
tsSteps<- ts(stepsData$value,frequency=1)

plot(decompose(tsHr))

# install.packages("changepoint")
library(changepoint)

# change in mean and variance
changePoints <- cpt.meanvar(tsHr, method = "BinSeg", Q=100, test.stat = "Poisson")
changePointsMv <- cpt.meanvar(combinedData, method = "BinSeg", Q=100, test.stat = "Poisson")

# Extract the predicted change points times in minutes and insert into 
changeTimes <- data_frame(
    totalMins = c(0,changePoints@cpts),
    segment = 1:(length(changePoints@cpts) + 1)
  ) %>% 
  right_join(
    hrData,
    by = "totalMins"
  ) %>% fill(segment)

# Check what we have. 
ggplot(changeTimes, aes(x = totalMins, y = value)) +
  geom_line() + 
  facet_wrap(~segment, scales = "free_x") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) + labs(title = "Detected unique periods of heart rate over last week")


# Multivariate attemps using BCP
# install.packages('bcp')
library('bcp')

smallData <- combinedData[1:(24*60),]

bChangePoints <- bcp(smallData, w0 = 0.7, p0 = 0.001)
plot(bChangePoints)

# combine data with posterior probs of change
bChangeTimes <- data_frame(
  changeProb = c(0,bChangePoints$posterior.prob), #first timepoint isn't included so we add it. 
  totalMins = seq(0, length(bChangePoints$posterior.prob) )
) %>% 
  filter(changeProb == 1 | totalMins == 0) %>%
  mutate(segment = 1:n()) %>% 
  right_join(
    hrData,
    by = "totalMins"
  ) %>% fill(segment)

ggplot(bChangeTimes, aes(x = totalMins, y = value)) +
  geom_line() + 
  facet_wrap(~segment, scales = "free_x") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) + labs(title = "Detected unique periods of heart rate over last week")

summary(bChangePoints)

# Markov Change Model attempt
library(msm)
statetable.msm(state, PTNUM, data=cav)
Q <- rbind ( c(0, 0.25, 0, 0.25),
             c(0.166, 0, 0.166, 0.166),
             c(0, 0.25, 0, 0.25),
             c(0, 0, 0, 0) )

cav.msm <- msm( state ~ years, subject=PTNUM, data = cav,
                qmatrix = Q, deathexact = 4, covariates = ~sex)


