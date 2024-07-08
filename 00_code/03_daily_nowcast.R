
# Initializing ------------------------------------------------------------

# install.packages("pacman")
library(pacman)

p_load(
  readr,
  readxl,
  data.table,
  lubridate
)

# diff function for growth rate
na_diff <- function(x){
  c(NA, diff(x)*100)
}

# Load data ------------------------------------------------------------

raw_data_path <- "01_data_raw/"

tidy_data_path <- "02_data_tidy/"

results_path <- "03_results/"

daily_data <- as.data.table(
  read_csv(file = paste0(tidy_data_path,"daily_data.csv"))
)

load(file = paste0(tidy_data_path,"real_time_gdp.RData"))

## control centre

nowcast_start <- which(daily_data[, day] %in% "2000-01-01") # change date of nowcast start
nowcast_end <- nrow(daily_data)

## loop prep

# create placeholder list with one element for each day
nowcast_results <- vector("list", length = length(nowcast_start:nowcast_end))


## loop start

for(i in nowcast_start:nowcast_end){
print(i) # for observing progress
  
curr_day <- as.character(daily_data[i, day]) # i_start = 2000-01-01

curr_doq <- daily_data[i, doq]

curr_pmi_release <- daily_data[i, pmi_available]


current_nowcast_dt <- daily_data[1:i]

current_nowcast_dt <- current_nowcast_dt[doq %in% curr_doq,] # filter for obs with same doq
current_nowcast_dt <- current_nowcast_dt[pmi_available %in% curr_pmi_release,] # filter for obs with same pmi release cycle stage

curr_gdp_vintage <- real_time_gdp[[curr_day]]

curr_gdp_vintage[, gdp_vintage_gr:= na_diff(log(value))]

nowcast_dt <- current_nowcast_dt[curr_gdp_vintage, , on = .(quarter = quarter)]

nowcast_dt <- curr_gdp_vintage[current_nowcast_dt, , on = .(quarter = quarter)]
nowcast_dt <- nowcast_dt[,.(day, quarter, doq, pmi, pmi_available,news, gdp_vintage_gr, gdp_gr)]

# split into in and out of sample

nowcast_in_dt <- na.omit(nowcast_dt)

nowcast_out_dt <- nowcast_dt[which(is.na(nowcast_dt[,.(gdp_vintage_gr)])),] # filters obs where no gdp data was available

# nowcast model estimation

nowcast_model_pmi_bench <- lm(gdp_vintage_gr ~ pmi, nowcast_in_dt) # pmi benchmark
summary(nowcast_model_pmi_bench)

nowcast_model_news <- lm(gdp_vintage_gr ~ pmi + news, nowcast_in_dt) # news 
summary(nowcast_model_news)

# prediction
nowcast_pmi_bench_pred <- predict(nowcast_model_pmi_bench, nowcast_out_dt)

nowcast_news_pred <- predict(nowcast_model_news, nowcast_out_dt)

# SE

nowcast_pmi_bench_se <- (nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2

nowcast_news_se <- (nowcast_out_dt$gdp_gr - nowcast_news_pred)^2

# MSE

nowcast_pmi_bench_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2)

nowcast_news_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_news_pred)^2)

# summary dt for nowcast

nowcast_results[[i]] <- data.table(
  day = curr_day,
  quarter = nowcast_out_dt[, quarter],
  doq = curr_doq,
  gdp_gr = nowcast_out_dt[, gdp_gr],
  prediction_pmi_bench = nowcast_pmi_bench_pred,
  prediction_news = nowcast_news_pred,
  se_pmi_bench = nowcast_pmi_bench_se,
  se_news = nowcast_news_se
)

nowcast_results[[i]][, nowcast_number:= 1:.N]

}

#### loop end
nowcast_results_dt <- NULL

for (j in nowcast_start:nowcast_end) {
  print(j)
  if(j == nowcast_start){
    nowcast_results_dt <- nowcast_results[[j]]
  } else{
    nowcast_results_dt <- rbind(nowcast_results_dt, nowcast_results[[j]])
  }
}

## calculate mse for each doq and separately for different nowcast numbers

nowcast_results_dt[,mse_pmi_bench:= mean(se_pmi_bench), by = .(doq, nowcast_number)]
nowcast_results_dt[,mse_news:= mean(se_news), by = .(doq, nowcast_number)]

nowcast_mse <- unique(nowcast_results_dt[,.(doq, nowcast_number, mse_pmi_bench, mse_news)])
nowcast_mse[, mse_reduction:= mse_pmi_bench - mse_news]

nowcast_mse_next_release <- nowcast_mse[nowcast_number %in% 1, ]
nowcast_mse_nextnext_release <- nowcast_mse[nowcast_number %in% 2, ]

## calculate mse for each doq

nowcast_results_dt[,mse_pmi_bench:= mean(se_pmi_bench), by = .(doq)]
nowcast_results_dt[,mse_news:= mean(se_news), by = .(doq)]
nowcast_mse <- unique(nowcast_results_dt[,.(doq, nowcast_number, mse_pmi_bench, mse_news)])
nowcast_mse[, mse_reduction:= mse_pmi_bench - mse_news]

## simple plot

plot(nowcast_mse_next_release$doq, nowcast_mse_next_release$mse_reduction, type = "l")
plot(nowcast_mse_nextnext_release$doq, nowcast_mse_nextnext_release$mse_reduction, type = "p")


plot(nowcast_mse$doq, nowcast_mse$mse_reduction)

### save results

write.csv(nowcast_results_dt, file = paste0(results_path, "nowcast_results_01.csv"))


# single nowcast

# curr_day <- as.character(daily_data[1035, day]) # 1035 = 2000-01-01
# 
# curr_doq <- daily_data[1035, doq]
# 
# curr_pmi_release <- daily_data[1035, pmi_available]
# 
# 
# current_nowcast_dt <- daily_data[1:1035]
# 
# current_nowcast_dt <- current_nowcast_dt[doq %in% curr_doq,] # filter for obs with same doq
# current_nowcast_dt <- current_nowcast_dt[pmi_available %in% curr_pmi_release,] # filter for obs with same pmi release cycle stage
# 
# curr_gdp_vintage <- real_time_gdp[[curr_day]]
# 
# curr_gdp_vintage[, gdp_vintage_gr:= na_diff(log(value))]
# 
# 
# curr_day
# 
# real_time_gdp[[curr_day]]
# 
# nowcast_dt <- current_nowcast_dt[curr_gdp_vintage, , on = .(quarter = quarter)]
# 
# nowcast_dt <- curr_gdp_vintage[current_nowcast_dt, , on = .(quarter = quarter)]
# nowcast_dt <- nowcast_dt[,.(day, quarter, doq, pmi, pmi_available,news, gdp_vintage_gr, gdp_gr)]
# 
# # split into in and out of sample
# 
# nowcast_in_dt <- na.omit(nowcast_dt)
# 
# nowcast_out_dt <- nowcast_dt[which(is.na(nowcast_dt[,.(gdp_vintage_gr)])),] # filters obs where no gdp data was available
# 
# # nowcast model estimation
# 
# nowcast_model_pmi_bench <- lm(gdp_vintage_gr ~ pmi, nowcast_in_dt) # pmi benchmark
# summary(nowcast_model_pmi_bench)
# 
# nowcast_model_news <- lm(gdp_vintage_gr ~ pmi + news, nowcast_in_dt) # news 
# summary(nowcast_model_news)
# 
# # prediction
# nowcast_pmi_bench_pred <- predict(nowcast_model_pmi_bench, nowcast_out_dt)
# 
# nowcast_news_pred <- predict(nowcast_model_news, nowcast_out_dt)
# 
# # SE
# 
# nowcast_pmi_bench_se <- (nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2
# 
# nowcast_news_se <- (nowcast_out_dt$gdp_gr - nowcast_news_pred)^2
# 
# # MSE
# 
# nowcast_pmi_bench_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2)
# 
# nowcast_news_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_news_pred)^2)
# 
# # summary dt for nowcast
# 
# nowcast_results <- data.table(
#   day = curr_day,
#   quarter = nowcast_out_dt[, quarter],
#   doq = curr_doq,
#   gdp_gr = nowcast_out_dt[, gdp_gr],
#   prediction_pmi_bench = nowcast_pmi_bench_pred,
#   prediction_news = nowcast_news_pred,
#   se_pmi_bench = nowcast_pmi_bench_se,
#   se_news = nowcast_news_se,
# )
# 
# nowcast_results[, nowcast_number:= 1:.N]
# 
# 
