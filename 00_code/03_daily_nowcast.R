
# Initializing ------------------------------------------------------------

rm(list = ls())

# nowcasting package from github

# devtools::install_github('nmecsys/nowcasting')

# install.packages("pacman")
library(pacman)

p_load(
  readr,
  readxl,
  data.table,
  lubridate,
  ggplot2,
  devtools,
  nowcasting,
  xts,
  TSstudio
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

# OLS  ------------------------------------------------------------

## control centre

nowcast_start <- which(daily_data[, day] %in% "2007-01-01") # change date of nowcast start
nowcast_end <- nrow(daily_data) # full sample
# nowcast_end <- which(daily_data[, day] %in% "2019-12-31") # truncate: change date of nowcast end

window <- "expanding" # fixed or expanding

same_pmi_rls <- FALSE

discard_new_gdp_release <- TRUE # should the observation with the gdp value released during the quarter 
# (nowcast_quarter - 1 quarter) be discarded before performing the nowcast? 
# first release gdp might be very inaccurate --> bad training observation --> might cause spikes?


## loop prep

# create placeholder list with one element for each day
nowcast_results <- vector("list", length = length(nowcast_start:nowcast_end))

# # test
# 
# i <- which(daily_data$day %in% "2007-03-01")
# nowcast_results[[i]]
## loop start

for(i in nowcast_start:nowcast_end){
  
  k <- (i-nowcast_start + 1)
print(k) # for observing progress
  
curr_day <- as.character(daily_data[i, day]) # i_start = 2000-01-01

curr_doq <- daily_data[i, doq]

if(curr_doq > 90){
  next
}

curr_pmi_release <- daily_data[i, pmi_available]


current_daily_dt <- daily_data[1:i]

current_nowcast_dt <- current_daily_dt

current_nowcast_dt <- current_nowcast_dt[doq %in% curr_doq,] # filter for obs with same doq

if(same_pmi_rls == TRUE){

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

}

curr_gdp_vintage <- real_time_gdp[[curr_day]]

# curr_gdp_vintage[, gdp_vintage_gr:= na_diff(log(value))] # log diff growth
curr_gdp_vintage[, gdp_vintage_gr:= gr_fun(value)] # normal growth

# nowcast_dt <- current_nowcast_dt[curr_gdp_vintage, , on = .(quarter = quarter)]

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
  nowcast_in_dt <- nowcast_in_dt[(current_in_obs-fixed_window_obs+1):current_in_obs,]
  
}

}


if(nrow(nowcast_in_dt) > 0){

nowcast_out_dt <- nowcast_dt[which(is.na(nowcast_dt[,.(gdp_vintage_gr)])),] # filters obs where no gdp data was available

if(discard_new_gdp_release == TRUE){
  
  if(nrow(nowcast_out_dt) == 1){
    
    nowcast_in_dt <- nowcast_in_dt[1:(nrow(nowcast_in_dt) - 1 ),] # discard last observation
  }
  
}

  
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

nowcast_results_dt_rn <- nowcast_results_dt_rn[!(quarter %in% "2020-04-01" | quarter %in% "2020-04-01"),] # exclude extreme growth rates 

nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]

nowcast_results_covid <- nowcast_results_dt_rn

} else if(sample_period == "great_recession"){
  nowcast_results_dt_rn <- nowcast_results_dt_rn[year(quarter) < 2010,]
  
  nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
  nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
  nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]
  
  nowcast_results_great_rec <- nowcast_results_dt_rn
  
} else if(sample_period == "2010s"){
  nowcast_results_dt_rn <- nowcast_results_dt_rn[year(quarter) > 2009 & year(quarter) < 2020,]
  
  nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
  nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
  nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]
  
  nowcast_results_2010s <- nowcast_results_dt_rn
  
} else if(sample_period == "post_covid"){
  nowcast_results_dt_rn <- nowcast_results_dt_rn[year(quarter) > 2022,]
  
  nowcast_results_dt_rn[, mse_pmi_bench:= mean(se_pmi_bench), by = doq]
  nowcast_results_dt_rn[, mse_news:= mean(se_news), by = doq]
  nowcast_results_dt_rn[, mse_news_cci:= mean(se_news_cci), by = doq]
  
  nowcast_results_post_covid <- nowcast_results_dt_rn
  
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

# plot(nowcast_mse_next_release$doq, nowcast_mse_next_release$mse_reduction, type = "l")
# plot(nowcast_mse_nextnext_release$doq, nowcast_mse_nextnext_release$mse_reduction, type = "p")
# 
# 
# plot(nowcast_mse$doq, nowcast_mse$mse_reduction)
# plot(nowcast_mse$doq, nowcast_mse$mse_pmi_bench, type = "p")
# plot(nowcast_mse$doq, nowcast_mse$mse_news)
# 
# plot(nowcast_rn_mse$doq, nowcast_rn_mse$mse_pmi_bench)
# plot(nowcast_rn_mse$doq, nowcast_rn_mse$mse_news)
# plot(nowcast_rn_mse$doq, nowcast_rn_mse$mse_news_cci)
# 
# ggplot(data = nowcast_rn_mse, aes())

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


# ggplot(nowcast_sample_mse, aes(x = doq, y = mse, color = model)) +
#   geom_line() +
#   facet_wrap(~ sample, ncol = 2) +
#   labs(title = "Mean Squared Error of Forecast Over Time",
#        x = "Time",
#        y = "Mean Squared Error",
#        color = "Model") +
#   theme_minimal()



ggsave(filename = paste0(results_path, "ols_mse.png"),
       bg = "white")
?ggsave
### save results

write.csv(nowcast_results_dt, file = paste0(results_path, "nowcast_results_ols.csv"),
          row.names = FALSE)

### diagnostics

# nowcast_results_dt[se_pmi_bench > se_news,]
# 
# nowcast_results_dt[se_news > se_pmi_bench,]
# 
# max(na.omit(nowcast_results_dt[, mse_news]))
# 
# nowcast_results_dt[, mse_news]
# 
# mse_max <- which(nowcast_results_dt[, mse_news] %in% max(na.omit(nowcast_results_dt[, mse_news])))
# 
# nowcast_results_dt[mse_max,]
# 
# nowcast_results_dt[mse_news %in% 0.5321892, ]
# 
# test_dt <- nowcast_results_dt[doq %in% 33 , ]
# 
# daily_data[day %in% "2001-05-01", ]
# daily_data[]
# 
# news_diff

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

# DFM ------------------------------------------------------------

# load extra quarterly and monthly data

load(file = paste0(tidy_data_path,"real_time_macro_data_monthly.RData"))
load(file = paste0(tidy_data_path,"real_time_macro_data_quarterly.RData"))


## control centre

first_data_point <- names(real_time_macro_data_monthly)[1]

nowcast_start <- which(daily_data[, day] %in% first_data_point) # change date of nowcast start
nowcast_end <- nrow(daily_data) # full sample
# nowcast_end <- which(daily_data[, day] %in% "2019-12-31") # truncate: change date of nowcast end

window <- "expanding" # fixed or expanding

same_pmi_rls <- FALSE

dfm_lags <- 2 # number of lags in dfm

# dfm factors and shocks
factors_shocks <- "constant" # variable or constant

# in case constant:
cons_factors <- 2
cons_shocks <- 2


## loop prep

# create placeholder list with one element for each day
nowcast_results_mac <- vector("list", length = length(nowcast_start:nowcast_end))

# # test
# 
# i <- 6000
# k <- 45
# nowcast_results_mac[[i]]
## loop start

for(i in nowcast_start:nowcast_end){
  
  k <- (i-nowcast_start + 1)
  print(k) # for observing progress
  
  curr_day <- as.character(daily_data[i, day]) # i_start = 2000-01-01
  
  curr_doq <- daily_data[i, doq]
  curr_dom <- daily_data[i, dom]
  
  curr_quarter <- daily_data[i, quarter]
  curr_year <- daily_data[i, year]
  curr_qoy <- daily_data[i, qoy]
  curr_nowcast_index <- curr_year + 1/4*curr_qoy - 1/4
  
  
  if(curr_doq > 90){
    next
  }
  
  curr_pmi_release <- daily_data[i, pmi_available]
  
  
  current_daily_dt <- daily_data[1:i]
  
  current_nowcast_dt <- current_daily_dt
  
  current_nowcast_dt <- current_nowcast_dt[dom %in% curr_dom,] # filter for obs with same dom
  
  if(same_pmi_rls == TRUE){
    
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
    
  }
  
  
  curr_vintage_q <- real_time_macro_data_quarterly[[curr_day]]
  
  # curr_gdp_vintage[, gdp_vintage_gr:= na_diff(log(value))] # log diff growth
  curr_vintage_q[, gdp_vintage_gr:= gr_fun(vintage_gdp)] # normal growth
  
  # nowcast_dt <- curr_vintage_q[current_nowcast_dt, , on = .(quarter = quarter)] # merge with daily data
  # nowcast_dt <- nowcast_dt[,.(day, quarter, doq, pmi, pmi_available, cci, cci_available, news, news_aoq, 
  #                             nasdaq, export,
  #                             gdp_vintage_gr, gdp_gr)]
  
  ## merge with monthly macro data
  curr_vintage_m <- real_time_macro_data_monthly[[curr_day]]
  
  # # aggregate monthly data to quarterly frequency
  # aggr_cols <- colnames(curr_vintage_m)[-1]
  # curr_vintage_m[, quarter:= floor_date(month, unit = "quarter")]
  # 
  # curr_vintage_m_quarterly <- curr_vintage_m [, lapply(.SD, mean), .SDcols = aggr_cols, by = .(quarter)]
  # 
  # nowcast_dt <- curr_vintage_m_quarterly[nowcast_dt, on= .(quarter = quarter)]
  
  # prepare quarterly data for monthly nowcasting data
  
  curr_vintage_q <- na.omit(curr_vintage_q)
  
  # curr_vintage_q_xts <- as.xts(curr_vintage_q)
  # curr_vintage_q_ts <- xts_to_ts(curr_vintage_q_xts$export)
  curr_vintage_q[, quarter_lm:=quarter + months(2)]
  
  monthly_helper <- data.table(
    month = seq.Date(from = curr_vintage_q[1, quarter], 
                             to = curr_vintage_q[nrow(curr_vintage_q), quarter_lm],
                             by = "months")
  )
  
  # curr_vintage_q_monthly <- monthly_helper[curr_vintage_q, , on = .(month = quarter_lm)]
  
  curr_vintage_q_monthly <- curr_vintage_q[monthly_helper, , on = .(quarter_lm = month)]
  setnames(curr_vintage_q_monthly, "quarter_lm", "month")
  curr_vintage_q_monthly <- curr_vintage_q_monthly[, .(month, export, gdp_vintage_gr)]
  curr_vintage_q_monthly <- as.data.table(nafill(curr_vintage_q_monthly, type = "nocb"))
  
  # curr_vintage_m[curr_vintage_q_monthly, , on= .(month = quarter_lm)]
  
  curr_vintage_full <- curr_vintage_q_monthly[curr_vintage_m, .(month,
                                                         ind_pro, construct,  car_reg ,   ret_sales,  ind_orders, unemp ,cons_loans, mon_sup, spread,
                                                         export, gdp_vintage_gr),
                                       on= .(month = month)]
  
  nowcast_dt <- curr_vintage_full[current_nowcast_dt, .(month, pmi, cci, news_aom, nasdaq_aom,
                                                        ind_pro, construct,  car_reg ,   ret_sales,  ind_orders, unemp ,cons_loans, mon_sup, spread,
                                                        export, gdp_vintage_gr), 
                                  on = .(month = month)]
  
  ## Nowcast
  
  nowcast_dfm_cols <- which(!(colnames(nowcast_dt) %in% "month"))
  
  nowcast_dfm_dt <- nowcast_dt[, ..nowcast_dfm_cols]
  
  # na_data <- matrix(data = NA, nrow = 12, ncol = ncol(nowcast_dfm_dt))
  # colnames(na_data) <- colnames(nowcast_dfm_dt)
  nowcast_dfm_ts_unbalanced <- ts(nowcast_dfm_dt, start = c(year(nowcast_dt$month[1]), month(nowcast_dt$month[1])), frequency = 12)
  nowcast_dfm_ts_balanced <- Bpanel(nowcast_dfm_ts_unbalanced, trans = c(rep(0, 3), rep(1, ncol(nowcast_dfm_ts_unbalanced)-5), 7, 0), NA.replace = TRUE)
  nowcast_dfm_ts <- ts(nowcast_dfm_ts_balanced, start = c(year(nowcast_dt$month[1]), month(nowcast_dt$month[1])), frequency = 12)
  
  
  if(factors_shocks == "variable"){
  ## Factors
  
  dfm_x_cols <- which(!(colnames(nowcast_dfm_ts) == "gdp_vintage_gr"))
  nowcast_dfm_x <- nowcast_dfm_ts[, dfm_x_cols]
  
  curr_factors <- ICfactors(nowcast_dfm_x, rmax = 7)$r_star
  
  ## Shocks
  
  curr_shocks <- ICshocks(nowcast_dfm_x, r= curr_factors, p = 2)$q_star
  
  } else{
    curr_factors <- cons_factors
    curr_shocks <- cons_shocks
  }
  
  frequency <- c(rep(12,ncol(nowcast_dfm_dt)-2), 4, 4)
  
  now_bench <- nowcast(formula = gdp_vintage_gr ~ pmi + cci + nasdaq_aom + ind_pro
                       + construct + car_reg + ret_sales + ind_orders + unemp 
                       + cons_loans + mon_sup + spread + export, data = nowcast_dfm_ts, r = curr_factors, q = curr_shocks, p = dfm_lags, method = "2s_agg",
               frequency = frequency)
  
  now_news <- nowcast(formula = gdp_vintage_gr ~., data = nowcast_dfm_ts, r = curr_factors, q = curr_shocks, p = dfm_lags, method = "2s_agg",
                       frequency = frequency)
  
  # take nowcast prediction out of results 
  prediction_bench_dfm <- now_bench$yfcst[which(index(now_bench$yfcst) == curr_nowcast_index), "out"]
  prediction_news_dfm <- now_news$yfcst[which(index(now_news$yfcst) == curr_nowcast_index), "out"]
  
  
  # nowcast.plot(now_bench, type = "eigenvectors")
  # nowcast.plot(now_news, type = "eigenvectors")
  # 
  # ## split into in and out of sample
  # 
  # nowcast_in_index <- which(is.na(nowcast_dt$gdp_vintage_gr))
  # nowcast_in_dt <- nowcast_dt[!(nowcast_in_index),]
  # 
  # current_in_obs <- nrow(nowcast_in_dt)
  # 
  # # fixed or expanding window
  # 
  # if(window == "fixed"){
  #   
  #   if(k == 1){
  #     fixed_window_obs <- nrow(nowcast_in_dt)
  #   } else{
  #     nowcast_in_dt <- nowcast_in_dt[(current_in_obs-fixed_window_obs+1):current_in_obs,]
  #     
  #   }
  #   
  # }
  # 
  # 
  # if(nrow(nowcast_in_dt) > 0){
  #   
  #   nowcast_out_dt <- nowcast_dt[nowcast_in_index,] # filters obs where no gdp data was available
  #   
  #   # nowcast model estimation
  #   
  #   nowcast_model_pmi_bench <- lm(gdp_vintage_gr ~ pmi, nowcast_in_dt) # pmi benchmark
  #   summary(nowcast_model_pmi_bench)
  #   
  #   nowcast_model_news <- lm(gdp_vintage_gr ~ pmi + news_aoq, nowcast_in_dt) # news aoq
  #   summary(nowcast_model_news)
  #   
  #   nowcast_model_news_cci <- lm(gdp_vintage_gr ~ pmi + news_aoq + cci, nowcast_in_dt) # news aoq + cci
  #   
  #   # prediction
  #   nowcast_pmi_bench_pred <- predict(nowcast_model_pmi_bench, nowcast_out_dt)
  #   
  #   nowcast_news_pred <- predict(nowcast_model_news, nowcast_out_dt)
  #   
  #   nowcast_news_cci_pred <- predict(nowcast_model_news_cci, nowcast_out_dt)
    
    # SE
  
    curr_gdp_gr <- daily_data[i, gdp_gr]
    
    nowcast_dfm_bench_se <- (curr_gdp_gr - prediction_bench_dfm)^2
    
    nowcast_dfm_news_se <- (curr_gdp_gr - prediction_news_dfm)^2
    
    # nowcast_news_cci_se <- (nowcast_out_dt$gdp_gr - nowcast_news_cci_pred)^2
    
    # # MSE
    # 
    # nowcast_pmi_bench_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_pmi_bench_pred)^2)
    # 
    # nowcast_news_mse <- mean((nowcast_out_dt$gdp_gr - nowcast_news_pred)^2)
    
    # summary dt for nowcast
    
    nowcast_results_mac[[k]] <- data.table(
      day = curr_day,
      quarter = curr_quarter,
      doq = curr_doq,
      gdp_gr = curr_gdp_gr,
      prediction_bench_dfm = prediction_bench_dfm,
      prediction_news_dfm = prediction_news_dfm,
     # prediction_news_cci = nowcast_news_cci_pred,
      se_dfm_bench = nowcast_dfm_bench_se,
      se_dfm_news = nowcast_dfm_news_se #,
      #se_news_cci = nowcast_news_cci_se
    )
    
    # nowcast_results_mac[[k]][, nowcast_number:= 1]
    
    
  # } else{
  #   nowcast_results_mac[[k]] <- data.table(
  #     day = curr_day,
  #     quarter = nowcast_out_dt[, quarter],
  #     doq = curr_doq,
  #     gdp_gr = nowcast_out_dt[, gdp_gr],
  #     prediction_pmi_bench = NA,
  #     prediction_news = NA,
  #     prediction_news_cci = NA,
  #     se_pmi_bench = NA,
  #     se_news = NA,
  #     se_news_cci = NA,
  #     nowcast_number= NA
  #   )
}
  
  


#### loop end
nowcast_results_mac_dt <- NULL

for (j in 1:length(nowcast_results_mac)) {
  print(j)
  if(j == 1){
    nowcast_results_mac_dt <- nowcast_results_mac[[j]]
  } else{
    nowcast_results_mac_dt <- rbind(nowcast_results_mac_dt, nowcast_results_mac[[j]])
  }
}

# ## calculate mse for each doq and separately for different nowcast numbers
# 
# nowcast_results_mac_dt[,mse_pmi_bench:= mean(se_pmi_bench), by = .(doq, nowcast_number)]
# nowcast_results_mac_dt[,mse_news:= mean(se_news), by = .(doq, nowcast_number)]
# 
# nowcast_mse <- unique(nowcast_results_mac_dt[,.(doq, nowcast_number, mse_pmi_bench, mse_news)])
# nowcast_mse[, mse_reduction:= mse_pmi_bench - mse_news]
# 
# nowcast_mse_next_release <- nowcast_mse[nowcast_number %in% 1, ]
# nowcast_mse_nextnext_release <- nowcast_mse[nowcast_number %in% 2, ]
# 
# ## calculate mse for each doq
# 
# nowcast_results_mac_dt[,mse_dfm_bench:= mean(se_dfm_bench), by = .(doq)]
# nowcast_results_mac_dt[,mse_dfm_news:= mean(se_dfm_news), by = .(doq)]
# nowcast_mse_dfm <- unique(nowcast_results_mac_dt[,.(doq, mse_dfm_bench, mse_dfm_news)])
# nowcast_mse[, mse_reduction:= mse_pmi_bench - mse_news]


nowcast_results_dfm_dt <- nowcast_results_mac_dt

# for loop over samples
# samp <- 2
sample_periods <- c("14-16", "17-19", "covid", "post_covid")

for (samp in 1:length(sample_periods)) {
  
  sample_period <- sample_periods[samp]
  
  
  if(sample_period == "covid"){
    nowcast_results_dfm_dt <- nowcast_results_dfm_dt[year(quarter) < 2023 & year(quarter) > 2019,]
    
    nowcast_results_dfm_dt <- nowcast_results_dfm_dt[!(quarter %in% "2020-04-01" | quarter %in% "2020-04-01"),] # exclude extreme growth rates 
    
    nowcast_results_dfm_dt[, mse_dfm_bench:= mean(se_dfm_bench), by = doq]
    nowcast_results_dfm_dt[, mse_dfm_news:= mean(se_dfm_news), by = doq]
    # nowcast_results_dfm_dt[, mse_dfm_news_cci:= mean(se_dfm_news_cci), by = doq]
    
    nowcast_results_dfm_covid <- nowcast_results_dfm_dt
    
  } else if(sample_period == "14-16"){
    nowcast_results_dfm_dt <- nowcast_results_dfm_dt[year(quarter) < 2017,]
    
    nowcast_results_dfm_dt[, mse_dfm_bench:= mean(se_dfm_bench), by = doq]
    nowcast_results_dfm_dt[, mse_dfm_news:= mean(se_dfm_news), by = doq]
    # nowcast_results_dfm_dt[, mse_dfm_news_cci:= mean(se_dfm_news_cci), by = doq]
    
    nowcast_results_dfm_14_16 <- nowcast_results_dfm_dt
    
  } else if(sample_period == "17-19"){
    nowcast_results_dfm_dt <- nowcast_results_dfm_dt[year(quarter) > 2016 & year(quarter) < 2020,]
    
    nowcast_results_dfm_dt[, mse_dfm_bench:= mean(se_dfm_bench), by = doq]
    nowcast_results_dfm_dt[, mse_dfm_news:= mean(se_dfm_news), by = doq]
    # nowcast_results_dfm_dt[, mse_dfm_news_cci:= mean(se_dfm_news_cci), by = doq]
    
    nowcast_results_dfm_17_19 <- nowcast_results_dfm_dt
    
  } else if(sample_period == "post_covid"){
    nowcast_results_dfm_dt <- nowcast_results_dfm_dt[year(quarter) > 2022,]
    
    nowcast_results_dfm_dt[, mse_dfm_bench:= mean(se_dfm_bench), by = doq]
    nowcast_results_dfm_dt[, mse_dfm_news:= mean(se_dfm_news), by = doq]
    # nowcast_results_dfm_dt[, mse_dfm_news_cci:= mean(se_dfm_news_cci), by = doq]
    
    nowcast_results_dfm_post_covid <- nowcast_results_dfm_dt
    
  }
  
  
  nowcast_mse_dfm <- unique(nowcast_results_dfm_dt[,.(doq, mse_dfm_bench, mse_dfm_news #, mse_dfm_news_cci
                                                  )])
  nowcast_mse_dfm <- nowcast_mse_dfm[doq < 91,] # discard days of quarter that do not exist in every quarter
  
  if (samp == 1){
    nowcast_dfm_sample_mse <- data.table(
      doq = nowcast_mse_dfm[, doq],
      sample = sample_period,
      dfm_bench = nowcast_mse_dfm[, mse_dfm_bench],
      dfm_news = nowcast_mse_dfm[, mse_dfm_news] #,
      # news_cci = nowcast_mse_dfm[, mse_dfm_news_cci]
    )
    
  } else{
    nowcast_dfm_sample_mse_current <- data.table(
      doq = nowcast_mse_dfm[, doq],
      sample = sample_period,
      dfm_bench = nowcast_mse_dfm[, mse_dfm_bench],
      dfm_news = nowcast_mse_dfm[, mse_dfm_news] #,
      # news_cci = nowcast_mse_dfm[, mse_dfm_news_cci]
    )
    nowcast_dfm_sample_mse <- rbind(nowcast_dfm_sample_mse_current, nowcast_dfm_sample_mse)
  }
  
  nowcast_results_dfm_dt <- nowcast_results_mac_dt
  
}


# Create the ggplot
ggplot(nowcast_dfm_sample_mse, aes(x = doq, group = sample)) +
  # Add the first time series
  geom_line(aes(y = dfm_bench, color = 'bench')) +
  # Add the second time series
  geom_line(aes(y = dfm_news, color = 'news')) +
  # Add the area between the two lines
  geom_ribbon(aes(ymin = pmin(dfm_bench, dfm_news), ymax = pmax(dfm_bench, dfm_news)), fill = 'grey', alpha = 0.4) +
  facet_wrap(~ sample, ncol = 2, scales = "free_y") +
  # Add labels and title
  labs(title = 'Forecast Error throughout the Quarter',
       x = 'Day of Quarter',
       y = 'Mean Squared Error') +
  # Customize the theme
  theme_minimal()+
  # Customize the colors and legend
  scale_color_manual(name = 'DFM Model', values = c('bench' = 'blue', 'news' = 'red'))
# Move the legend to the bottom
# Move the legend inside the plot frame at the top right
#theme(legend.position.inside = c(1, 1), legend.justification = c(1, 1))


# ggplot(nowcast_sample_mse, aes(x = doq, y = mse, color = model)) +
#   geom_line() +
#   facet_wrap(~ sample, ncol = 2) +
#   labs(title = "Mean Squared Error of Forecast Over Time",
#        x = "Time",
#        y = "Mean Squared Error",
#        color = "Model") +
#   theme_minimal()


# save plot
ggsave(filename = paste0(results_path, "dfm_mse.png"),
       bg = "white")

### save results

write.csv(nowcast_results_mac_dt, file = paste0(results_path, "nowcast_results_dfm.csv"),
          row.names = FALSE)
