library(plotly)
library(httr)
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(jsonlite)


fitbit_endpoint <- oauth_endpoint(  authorize = "https://www.fitbit.com/oauth2/authorize",  
                                    access = "https://api.fitbit.com/oauth2/token")

# Personal App info
myapp <- oauth_app( appname = "data_access",
                    key = "2287QM",
                    secret = "6a99b80ecf85cbe33d59c174e9c74748")

#Jeff's app info. These values aren't truly secret so they can be exposed. 
# myapp <- oauth_app( appname = "data_access",
#                     key = "228RXV",
#                     secret = "ff4ae11f55374f9688f319320c3cb4e1")

#What the heck, why do I need this?! Pull request will be sent
Sys.setenv("HTTR_SERVER_PORT" = "1410")

token <- oauth2.0_token(fitbit_endpoint, 
                        myapp, 
                        use_basic_auth = TRUE,
                        scope=c("activity", "heartrate", "sleep"))

conf <- config(token = token)




#Heart Rate at second level
heart_sec <- GET("https://api.fitbit.com/1/user/activities/heart/date/2017-06-11/1d.json", config=conf) %>% 
  content(as="text") 

heart_sec

#grab data for heart rate at a minute granularity
heart_min <- GET("https://api.fitbit.com/1/user/-/activities/heart/date/2017-06-10/1d/1sec/time/00:00/23:59.json", config=conf) %>% 
  content(as="text") %>% 
  fromJSON()

heart_min %>% 
  .$`activities-heart-intraday` %>% 
  .$dataset %>% 
  mutate(time = as.numeric(hms(time))) %>% 
  rename(heart_rate = value) %>% 
  ggplot(aes(x = time, y = heart_rate)) + 
  geom_line(color = "#a6cee3", alpha = 0.5) +
  geom_smooth(se = F, span = 0.4, color = "#1f78b4") + 
  theme_minimal

#Plotting heartrate with rolling average
heartPlot <- heart_sec %>% 
  mutate(heart_rate_rolling = rollapply(heart_rate,60,mean,align='right',fill=NA)) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = heart_rate), color = "steelblue", alpha = 0.1, size = 1) + 
  geom_line(aes(y = heart_rate_rolling)) + 
  theme_minimal()

# ggplotly(heartPlot)



#grab data for steps at a minute level as well
steps_min <- GET("https://api.fitbit.com/1/user/-/activities/steps/date/today/1d.json", config=conf) %>% 
  content(as="text") %>% 
  fromJSON() %>% 
  .$`activities-steps-intraday` %>% 
  .$dataset %>% 
  mutate(time = as.numeric(hms(time))) %>% 
  rename(step_num = value)


#Merge the two dataframes of heart and steps
steps_heart <- heart_min %>% 
  inner_join(steps_min, by = "time") %>% 
  mutate(step_to_heart_ratio = ifelse(step_num != 0, step_num/heart_rate, NA)) %>%
  gather(statistic, value, -time)


#Annotations
# ann_text <- data.frame(time = c(27500, 34000, 37500), 
#                        step_to_heart_ratio = c(0.75,0.1, 0.67),
#                        lab = c("Jogging", "Weight Lifting", "Brisk Walk"))

ggplot(steps_heart, aes(x = time, y = value)) + 
  facet_grid(statistic~., scales = "free_y") +
  geom_line(color = "#a6cee3", alpha = 0.5) +
  geom_smooth(se = F, span = 0.4, color = "#1f78b4") + 
  # geom_text(data = ann_text, aes(label = lab), family = "Times New Roman") + 
  theme_minimal() + 
  xlim(21000,50000) +
  labs(title = "Steps to Heart Rate by Minute", y = "ratio") + 
  theme(text=element_text(family="Times New Roman"))