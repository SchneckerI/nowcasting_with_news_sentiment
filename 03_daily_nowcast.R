
# Initializing ------------------------------------------------------------

# install.packages("pacman")
library(pacman)

p_load(
  readr,
  readxl,
  data.table,
  lubridate
)

# growth rate
na_diff <- function(x){
  c(NA, diff(x)*100)
}

# Load data ------------------------------------------------------------

data_path <- "C:/Users/yanni/OneDrive - Wirtschaftsuniversität Wien/Macrometrics/Project/data/"

daily_data <- as.data.table(
  read_csv(file = paste0(data_path,"daily_data.csv"))
)

load(file = paste0(data_path,"real_time_gdp.RData"))


## loop try

curr_day <- as.character(daily_data[1035, day]) # 1035 = 2000-01-01

curr_doq <- daily_data[1035, doq]

curr_pmi_release <- daily_data[1035, pmi_available]


current_nowcast_dt <- daily_data[1:1035]

current_nowcast_dt <- current_nowcast_dt[doq %in% curr_doq,] # filter for obs with same doq
current_nowcast_dt <- current_nowcast_dt[pmi_available %in% curr_pmi_release,] # filter for obs with same pmi release cycle stage

curr_gdp_vintage <- real_time_gdp[[curr_day]]

curr_gdp_vintage[, gdp_vintage_gr:= na_diff(log(value))]


curr_day

real_time_gdp[[curr_day]]

nowcast_dt <- current_nowcast_dt[curr_gdp_vintage, , on = .(quarter = date)]

nowcast_dt <- curr_gdp_vintage[current_nowcast_dt, , on = .(date = quarter)]
nowcast_dt <- nowcast_dt[,.(day, date, doq, pmi, pmi_available,`News Sentiment`, gdp_vintage_gr, gdp_gr)]

# split into in and out of sample

nowcast_in_dt <- na.omit(nowcast_dt)

nowcast_out_dt <- nowcast_dt[which(is.na(nowcast_dt[,.(gdp_vintage_gr)])),] # filters obs where no gdp data was available

# nowcast model estimation

nowcast_model_pmi_bench <- lm(gdp_vintage_gr ~ pmi, nowcast_in_dt) # pmi benchmark
summary(nowcast_model_pmi_bench)

nowcast_model_news <- lm(gdp_vintage_gr ~ pmi + `News Sentiment`, nowcast_in_dt) # news 
summary(nowcast_model_news)

# prediction
nowcast_pmi_bench_pred <- predict(nowcast_model_pmi_bench, nowcast_out_dt)

nowcast_news_pred <- predict(nowcast_model_news, nowcast_out_dt)

# MSE

nowcast_pmi_bench_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2)

nowcast_news_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_news_pred)^2)
