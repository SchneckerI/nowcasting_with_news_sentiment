# Initializing ------------------------------------------------------------

rm(list = ls())

# install.packages("pacman")
library(pacman)

p_load(
  readr,
  readxl,
  data.table,
  lubridate
)

# Set locale to English for better date handling
Sys.setlocale("LC_TIME", "C")

# growth rate
na_diff <- function(x){
  c(NA, diff(x)*100)
}

# Loading data ------------------------------------------------------------

raw_data_path <- "01_data_raw/"

tidy_data_path <- "02_data_tidy/"

plot_path <- "20_plots/"

# load gdp gr
gdp_gr <- as.data.table(read.csv(paste0(tidy_data_path, "gdp_latest_vintage.csv")))
gdp_gr[, quarter:= as.Date(quarter)]

## News data ------------------------------------------------------------

news_sentiment_raw <- as.data.table(
  read_excel(paste0(raw_data_path,"news_sentiment_data.xlsx"),
                                  sheet = "Data")
)

news_sentiment_raw[, date:= as.Date(date)]

## Stock data ------------------------------------------------------------

nasdaq_raw <- as.data.table(
  read_csv(paste0(tidy_data_path,"nasdaq_daily.csv"))
)

nasdaq_raw[, day:= as.Date(day)]

setnames(nasdaq_raw, "value", "nasdaq")

daily_helper <- data.table(
  date = seq.Date(from = nasdaq_raw$day[1], to = nasdaq_raw$day[length(nasdaq_raw$day)], by ="days")
  )

nasdaq_fill <- nasdaq_raw[daily_helper, , on = .(day = date)]
nasdaq_fill <-  as.data.table(nafill(nasdaq_fill, type = "locf"))

## PMI data ------------------------------------------------------------

pmi_raw <- as.data.table(
  read_excel(paste0(raw_data_path,"pmi.xlsx"), 
                  sheet = "First Release Data", skip = 3)
)

pmi_raw <- na.omit(pmi_raw)

pmi_raw[,Period:= paste("1",Period, sep =" ")]
pmi_raw[,month:= as.Date(Period, format = "%d %b %Y")]

pmi_raw[,release_date:= as.Date(`Original Release Date`, format = "%d %b %Y")]

pmi_raw[,quarter:=floor_date(month, unit = "quarter")]
setorder(pmi_raw, "release_date") # reorder from earliest to latest to avoid confusion

pmi_raw[, pmi_monthly:=as.numeric(`First Release`)]

## transform into daily series

daily_index_pmi <- seq.Date(from = as.Date(min(pmi_raw$release_date)),
                            to = today(),
                            by = "days")

pmi_daily <- data.table(day = daily_index_pmi,
                        pmi= NA)

pmi_daily[,quarter:= floor_date(day, unit = "quarter")]


# daily pmi loop

pmi_daily_value <- numeric(length(daily_index_pmi))
pmi_values_available <- numeric(length(daily_index_pmi))

for (i in 1:length(daily_index_pmi)) {
  current_quarter_pmi_values <- which(pmi_raw[,quarter] %in% pmi_daily[i, quarter] & pmi_daily[i, day] >= pmi_raw[,release_date])
  if(is.na(current_quarter_pmi_values[1])){
    last_month <- pmi_daily[i, quarter] - months(1)
    pmi_daily_value[i] <- pmi_raw[month %in% last_month, pmi_monthly]
    pmi_values_available[i] <- 0
  } else{
    pmi_daily_value[i] <- mean(pmi_raw[current_quarter_pmi_values, pmi_monthly])
    pmi_values_available[i] <- length(current_quarter_pmi_values)
  }
}

pmi_daily[,pmi:=pmi_daily_value]
pmi_daily[,pmi_available:=pmi_values_available]

# save daily pmi data
write.csv(pmi_daily, file = paste0(tidy_data_path,"pmi_daily.csv"),
          row.names = FALSE)

# simple plot
plot(pmi_daily$day, pmi_daily$pmi, type = "l",
     xlab = "time",
     ylab = "pmi")


## save plot
# 1. Open jpeg file
jpeg(paste0(plot_path, "plot_pmi.jpg"), width = 600, height = 350)
# 2. Create the plot
plot(x = pmi_daily$day, y = pmi_daily$pmi,
     pch = 20, frame = TRUE,
     xlab = "Time", ylab = "Daily PMI", col = "#2E9FDF",
     type = "l")
# 3. Close the file
dev.off()

## cci data ------------------------------------------------------------

cci_raw <- as.data.table(
  read_excel(paste0(raw_data_path,"cci.xlsx"), 
             sheet = "First Release Data", skip = 3)
)

cci_raw <- na.omit(cci_raw)

cci_raw[,Period:= paste("1",Period, sep =" ")]
cci_raw[,month:= as.Date(Period, format = "%d %b %Y")]

cci_raw[,release_date:= as.Date(`Original Release Date`, format = "%d %b %Y")]

cci_raw[,quarter:=floor_date(month, unit = "quarter")]
setorder(cci_raw, "release_date") # reorder from earliest to latest to avoid confusion

cci_raw[, cci_monthly:=as.numeric(`First Release`)]

## transform into daily series

daily_index_cci <- seq.Date(from = as.Date(min(cci_raw$release_date)),
                            to = today(),
                            by = "days")

cci_daily <- data.table(day = daily_index_cci,
                        cci= NA)

cci_daily[,quarter:= floor_date(day, unit = "quarter")]


# daily cci loop

cci_daily_value <- numeric(length(daily_index_cci))
cci_values_available <- numeric(length(daily_index_cci))

for (i in 1:length(daily_index_cci)) {
  current_quarter_cci_values <- which(cci_raw[,quarter] %in% cci_daily[i, quarter] & cci_daily[i, day] >= cci_raw[,release_date])
  if(is.na(current_quarter_cci_values[1])){
    last_month <- cci_daily[i, quarter] - months(1)
    cci_daily_value[i] <- cci_raw[month %in% last_month, cci_monthly]
    cci_values_available[i] <- 0
  } else{
    cci_daily_value[i] <- mean(cci_raw[current_quarter_cci_values, cci_monthly])
    cci_values_available[i] <- length(current_quarter_cci_values)
  }
}

cci_daily[,cci:=cci_daily_value]
cci_daily[,cci_available:=cci_values_available]

# save daily cci data
write.csv(cci_daily, file = paste0(tidy_data_path,"cci_daily.csv"),
          row.names = FALSE)

# simple plot
plot(cci_daily$day, cci_daily$cci, type = "l")

## save plot
# 1. Open jpeg file
jpeg(paste0(plot_path, "plot_cci.jpg"), width = 600, height = 350)
# 2. Create the plot
plot(x = cci_daily$day, y = cci_daily$cci,
     pch = 20, frame = TRUE,
     xlab = "Time", ylab = "Daily CCI", col = "#2E9FDF",
     type = "l")
# 3. Close the file
dev.off()



# delete quarter again for merge
cci_daily[,quarter:= NULL]


## Merge daily data ------------------------------------------------------------

daily_data <- pmi_daily[news_sentiment_raw, , on = .(day = date)][cci_daily, ,on= .(day = day)][nasdaq_fill, , on= .(day = day)]


daily_data[, quarter:= floor_date(day, unit = "quarter")]
daily_data[, doq:= 1:.N, by = quarter]

daily_data <- daily_data[gdp_gr, , on = .(quarter = quarter)]

daily_data <- na.omit(daily_data)

setnames(daily_data, "News Sentiment", "news") # for easier handling later

## Adapt news daily data to cumulative mean of quarter and mean of month ------------------------------------------------------------

daily_data[, news_cumsum:= cumsum(news), by = quarter]
daily_data[, news_aoq:= news_cumsum/doq, by = quarter]
daily_data[1:29, news_aoq:= news_cumsum/(1:29)] # correct first quarter


daily_data[, month:=floor_date(day, unit = "month")]
daily_data[, dom:= 1:.N, by = month]
daily_data[, news_cumsum_monthly:= cumsum(news), by = month]
daily_data[, news_aom:= news_cumsum_monthly/dom, by = month]


daily_data[, news_diff:= c(NA,diff(news))*100]
daily_data[, news_diff_cumsum:= cumsum(news_diff), by = quarter]
daily_data[, news_diff_aoq:= news_diff_cumsum/ doq, by = quarter]

daily_data[,news_cumsum:= NULL] # remove cumsum column again
daily_data[,news_cumsum_monthly:= NULL] # remove cumsum column again
daily_data[,news_diff_cumsum:= NULL] # remove diff cumsum column again


# daily_data[,news_gr:= na_diff(log(news)), by = doq] # qoq news gr (log)
# daily_data[,news_aoq_gr:= na_diff(log(news_aoq)), by = doq] # qoq news aoq gr (log)
# daily_data[,.(quarter,news_aoq,news_aoq_gr), by = doq] # check functionality of gr by doq

## Adapt stocks daily data to cumulative mean of month ------------------------------------------------------------

daily_data[, nasdaq_cumsum_monthly:=cumsum(nasdaq), by = month]
daily_data[, nasdaq_aom:=nasdaq_cumsum_monthly/dom, by = month]
daily_data[, nasdaq_cumsum_monthly:= NULL] # remove cumsum column again


## Final steps ------------------------------------------------------------
daily_data <- na.omit(daily_data)
daily_data[, year:= year(day)]
daily_data[, qoy:= quarter(day)]
# daily_data[, pmi_usual:= median(pmi_available), by = .(doq, qoy)] # usual release pmi cycle per quarter 
# daily_data[, cci_usual:= median(cci_available), by = .(doq, qoy)]# usual release cci cycle per quarter 

# quick news plot
plot(daily_data$day, daily_data$news_aoq, type = "l")

## save news plot
## save plot
# 1. Open jpeg file
jpeg(paste0(plot_path, "plot_news.jpg"), width = 600, height = 350)
# 2. Create the plot
plot(x = daily_data$day, y = daily_data$news_aoq,
     pch = 20, frame = TRUE,
     xlab = "Time", ylab = "News Sentiment", col = "#2E9FDF",
     type = "l")
# 3. Close the file
dev.off()



plot(daily_data$day, daily_data$pmi, type = "l")

# save daily pmi data
write.csv(daily_data, file = paste0(tidy_data_path,"daily_data.csv"),
          row.names = FALSE)
