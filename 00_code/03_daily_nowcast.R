
# Initializing ------------------------------------------------------------

rm(list = ls())

# install.packages("pacman")
library(pacman)

p_load(
  readr,
  readxl,
  data.table,
  lubridate,
  ggplot2
)

# diff function for growth rate
na_diff <- function(x){
  c(NA, diff(x)*100)
}

# normal growth rate function

gr_fun <- function(x){
  c(NA,(diff(x)/(x[1:length(x)-1]))*100)
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

nowcast_start <- which(daily_data[, day] %in% "2007-01-01") # change date of nowcast start
nowcast_end <- nrow(daily_data) # full sample
# nowcast_end <- which(daily_data[, day] %in% "2019-12-31") # truncate: change date of nowcast end

window <- "fixed"

## loop prep

# create placeholder list with one element for each day
nowcast_results <- vector("list", length = length(nowcast_start:nowcast_end))

# # test
# 
# i <- which(daily_data$day %in% "2001-05-01")
# nowcast_results[[i]]
## loop start

for(i in nowcast_start:nowcast_end){
  
  k <- (i-nowcast_start + 1)
print(k) # for observing progress
  
curr_day <- as.character(daily_data[i, day]) # i_start = 2000-01-01

curr_doq <- daily_data[i, doq]

curr_pmi_release <- daily_data[i, pmi_available]


current_daily_dt <- daily_data[1:i]

current_nowcast_dt <- current_daily_dt

current_nowcast_dt <- current_nowcast_dt[doq %in% curr_doq,] # filter for obs with same doq

current_nowcast_obs <- nrow(current_nowcast_dt)
current_discard <- current_nowcast_dt[!(pmi_available %in% curr_pmi_release),]

current_add_index_ahead <- current_discard$day + days(1)
current_add_index_back <- current_discard$day - days(1)


current_add_data <- current_daily_dt[day %in% current_add_index_ahead | day %in% current_add_index_back,]
current_add_data <- current_add_data[pmi_available %in% curr_pmi_release,]

current_nowcast_dt <- current_nowcast_dt[pmi_available %in% curr_pmi_release,] # filter for obs with same pmi release cycle stage
# current_nowcast_dt <- current_nowcast_dt[pmi_available == pmi_usual,] # filter for only observations with normal pmi release cycle

current_nowcast_dt <- rbind(current_add_data, current_nowcast_dt)

while(!(nrow(current_nowcast_dt) == current_nowcast_obs)){
  
  current_add_index_ahead <- current_add_index_ahead[which(!(current_add_index_ahead %in% current_nowcast_dt$day))]
  current_add_index_back <- current_add_index_back[which(!(current_add_index_back %in% current_nowcast_dt$day))]
  
  current_add_index_ahead <- current_add_index_ahead + days(1)
  current_add_index_back <- current_add_index_back - days(1)  
  
  current_add_data <- current_daily_dt[day %in% current_add_index_ahead | day %in% current_add_index_back,]
  current_add_data <- current_add_data[pmi_available %in% curr_pmi_release,]
  current_nowcast_dt <- rbind(current_add_data, current_nowcast_dt)
}


curr_gdp_vintage <- real_time_gdp[[curr_day]]

# curr_gdp_vintage[, gdp_vintage_gr:= na_diff(log(value))] # log diff growth
curr_gdp_vintage[, gdp_vintage_gr:= gr_fun(value)] # normal growth

nowcast_dt <- current_nowcast_dt[curr_gdp_vintage, , on = .(quarter = quarter)]

nowcast_dt <- curr_gdp_vintage[current_nowcast_dt, , on = .(quarter = quarter)]
nowcast_dt <- nowcast_dt[,.(day, quarter, doq, pmi, pmi_available, cci, cci_available, news, news_aoq, news_diff_aoq, gdp_vintage_gr, gdp_gr)]

## split into in and out of sample

nowcast_in_dt <- na.omit(nowcast_dt)

current_in_obs <- nrow(nowcast_in_dt)

# fixed or expanding window

if(window == "fixed"){

if(k == 1){
  fixed_window_obs <- nrow(nowcast_in_dt)
} else{
  nowcast_in_dt <- nowcast_in_dt[(current_in_obs-fixed_window+1):current_in_obs,]
  
}

}


if(nrow(nowcast_in_dt) > 0){

nowcast_out_dt <- nowcast_dt[which(is.na(nowcast_dt[,.(gdp_vintage_gr)])),] # filters obs where no gdp data was available

# nowcast model estimation

nowcast_model_pmi_bench <- lm(gdp_vintage_gr ~ pmi, nowcast_in_dt) # pmi benchmark
summary(nowcast_model_pmi_bench)

nowcast_model_news <- lm(gdp_vintage_gr ~ pmi + news_aoq, nowcast_in_dt) # news aoq
summary(nowcast_model_news)

nowcast_model_news_cci <- lm(gdp_vintage_gr ~ pmi + news_aoq + cci, nowcast_in_dt) # news aoq + cci

# prediction
nowcast_pmi_bench_pred <- predict(nowcast_model_pmi_bench, nowcast_out_dt)

nowcast_news_pred <- predict(nowcast_model_news, nowcast_out_dt)

nowcast_news_cci_pred <- predict(nowcast_model_news_cci, nowcast_out_dt)

# SE

nowcast_pmi_bench_se <- (nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2

nowcast_news_se <- (nowcast_out_dt$gdp_gr - nowcast_news_pred)^2

nowcast_news_cci_se <- (nowcast_out_dt$gdp_gr - nowcast_news_cci_pred)^2

# # MSE
# 
# nowcast_pmi_bench_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2)
# 
# nowcast_news_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_news_pred)^2)

# summary dt for nowcast

nowcast_results[[k]] <- data.table(
  day = curr_day,
  quarter = nowcast_out_dt[, quarter],
  doq = curr_doq,
  gdp_gr = nowcast_out_dt[, gdp_gr],
  prediction_pmi_bench = nowcast_pmi_bench_pred,
  prediction_news = nowcast_news_pred,
  prediction_news_cci = nowcast_news_cci_pred,
  se_pmi_bench = nowcast_pmi_bench_se,
  se_news = nowcast_news_se,
  se_news_cci = nowcast_news_cci_se
)

nowcast_results[[k]][, nowcast_number:= 1:.N]


} else{
  nowcast_results[[k]] <- data.table(
    day = curr_day,
    quarter = nowcast_out_dt[, quarter],
    doq = curr_doq,
    gdp_gr = nowcast_out_dt[, gdp_gr],
    prediction_pmi_bench = NA,
    prediction_news = NA,
    prediction_news_cci = NA,
    se_pmi_bench = NA,
    se_news = NA,
    se_news_cci = NA,
    nowcast_number= NA
  )
}


}

#### loop end
nowcast_results_dt <- NULL

for (j in 1:length(nowcast_results)) {
  print(j)
  if(j == 1){
    nowcast_results_dt <- nowcast_results[[j]]
  } else{
    nowcast_results_dt <- rbind(nowcast_results_dt, nowcast_results[[j]])
  }
}

# ## calculate mse for each doq and separately for different nowcast numbers
# 
# nowcast_results_dt[,mse_pmi_bench:= mean(se_pmi_bench), by = .(doq, nowcast_number)]
# nowcast_results_dt[,mse_news:= mean(se_news), by = .(doq, nowcast_number)]
# 
# nowcast_mse <- unique(nowcast_results_dt[,.(doq, nowcast_number, mse_pmi_bench, mse_news)])
# nowcast_mse[, mse_reduction:= mse_pmi_bench - mse_news]
# 
# nowcast_mse_next_release <- nowcast_mse[nowcast_number %in% 1, ]
# nowcast_mse_nextnext_release <- nowcast_mse[nowcast_number %in% 2, ]
# 
# ## calculate mse for each doq
# 
# nowcast_results_dt[,mse_pmi_bench:= mean(se_pmi_bench), by = .(doq)]
# nowcast_results_dt[,mse_news:= mean(se_news), by = .(doq)]
# nowcast_mse <- unique(nowcast_results_dt[,.(doq, nowcast_number, mse_pmi_bench, mse_news)])
# nowcast_mse[, mse_reduction:= mse_pmi_bench - mse_news]

## only real "nowcasts" --> quarter of date of nowcast = quarter for which nowcast is for

nowcast_results_dt[, quarter_of_nc:= floor_date(as.Date(day), unit = "quarter")]
nowcast_results_dt_rn <- nowcast_results_dt[quarter == quarter_of_nc,]

# for loop over samples
# samp <- 2
sample_periods <- c("covid", "great_recession", "2010s", "post_covid")

for (samp in 1:length(sample_periods)) {
  

sample_period <- sample_periods[samp]


if(sample_period == "covid"){
nowcast_results_dt_rn <- nowcast_results_dt_rn[year(quarter) < 2023 & year(quarter) > 2019,]

nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]

} else if(sample_period == "great_recession"){
  nowcast_results_dt_rn <- nowcast_results_dt_rn[year(quarter) < 2010,]
  
  nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
  nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
  nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]
  
} else if(sample_period == "2010s"){
  nowcast_results_dt_rn <- nowcast_results_dt_rn[year(quarter) > 2009 & year(quarter) < 2020,]
  
  nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
  nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
  nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]
  
} else if(sample_period == "post_covid"){
  nowcast_results_dt_rn <- nowcast_results_dt_rn[year(quarter) > 2022,]
  
  nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
  nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
  nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]
  
}


nowcast_rn_mse <- unique(nowcast_results_dt_rn[,.(doq, mse_pmi_bench, mse_news, mse_news_cci)])
nowcast_rn_mse <- nowcast_rn_mse[doq < 91,] # discard days of quarter that do not exist in every quarter

if (samp == 1){
nowcast_sample_mse <- data.table(
  doq = nowcast_rn_mse[, doq],
  sample = sample_period,
  pmi_bench = nowcast_rn_mse[, mse_pmi_bench],
  news = nowcast_rn_mse[, mse_news],
  news_cci = nowcast_rn_mse[, mse_news_cci]
)

} else{
  nowcast_sample_mse_current <- data.table(
    doq = nowcast_rn_mse[, doq],
    sample = sample_period,
    pmi_bench = nowcast_rn_mse[, mse_pmi_bench],
    news = nowcast_rn_mse[, mse_news],
    news_cci = nowcast_rn_mse[, mse_news_cci]
  )
  nowcast_sample_mse <- rbind(nowcast_sample_mse_current, nowcast_sample_mse)
}

nowcast_results_dt_rn <- nowcast_results_dt[quarter == quarter_of_nc,]

}


## simple plot

plot(nowcast_mse_next_release$doq, nowcast_mse_next_release$mse_reduction, type = "l")
plot(nowcast_mse_nextnext_release$doq, nowcast_mse_nextnext_release$mse_reduction, type = "p")


plot(nowcast_mse$doq, nowcast_mse$mse_reduction)
plot(nowcast_mse$doq, nowcast_mse$mse_pmi_bench, type = "p")
plot(nowcast_mse$doq, nowcast_mse$mse_news)

plot(nowcast_rn_mse$doq, nowcast_rn_mse$mse_pmi_bench)
plot(nowcast_rn_mse$doq, nowcast_rn_mse$mse_news)
plot(nowcast_rn_mse$doq, nowcast_rn_mse$mse_news_cci)

ggplot(data = nowcast_rn_mse, aes())

# Create the ggplot
ggplot(nowcast_sample_mse, aes(x = doq, group = sample)) +
  # Add the first time series
  geom_line(aes(y = pmi_bench, color = 'pmi')) +
  # Add the second time series
  geom_line(aes(y = news, color = 'news')) +
  # Add the area between the two lines
  geom_ribbon(aes(ymin = pmin(pmi_bench, news), ymax = pmax(pmi_bench, news)), fill = 'grey', alpha = 0.4) +
  facet_wrap(~ sample, ncol = 2, scales = "free_y") +
  # Add labels and title
  labs(title = 'Forecast Error throughout the Quarter',
       x = 'Day of Quarter',
       y = 'Mean Squared Error') +
  # Customize the theme
  theme_minimal()+
  # Customize the colors and legend
  scale_color_manual(name = 'OLS Model', values = c('pmi' = 'blue', 'news' = 'red'))
  # Move the legend to the bottom
  # Move the legend inside the plot frame at the top right
  #theme(legend.position.inside = c(1, 1), legend.justification = c(1, 1))


ggplot(nowcast_sample_mse, aes(x = doq, y = mse, color = model)) +
  geom_line() +
  facet_wrap(~ sample, ncol = 2) +
  labs(title = "Mean Squared Error of Forecast Over Time",
       x = "Time",
       y = "Mean Squared Error",
       color = "Model") +
  theme_minimal()



ggsave(filename = paste0(results_path, "ols_mse.png"),
       bg = "white")
?ggsave
### save results

write.csv(nowcast_results_dt, file = paste0(results_path, "nowcast_results_01.csv"))

### diagnostics

nowcast_results_dt[se_pmi_bench > se_news,]

nowcast_results_dt[se_news > se_pmi_bench,]

max(na.omit(nowcast_results_dt[, mse_news]))

nowcast_results_dt[, mse_news]

mse_max <- which(nowcast_results_dt[, mse_news] %in% max(na.omit(nowcast_results_dt[, mse_news])))

nowcast_results_dt[mse_max,]

nowcast_results_dt[mse_news %in% 0.5321892, ]

test_dt <- nowcast_results_dt[doq %in% 33 , ]

daily_data[day %in% "2001-05-01", ]
daily_data[]

news_diff

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
