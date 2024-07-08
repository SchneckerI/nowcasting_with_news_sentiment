# Initializing ------------------------------------------------------------

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


# Loading data ------------------------------------------------------------

raw_data_path <- "01_data_raw/"

tidy_data_path <- "02_data_tidy/"

## News data ------------------------------------------------------------

news_sentiment_raw <- as.data.table(
  read_excel(paste0(raw_data_path,"news_sentiment_data.xlsx"),
                                  sheet = "Data")
)

news_sentiment_raw[, date:= as.Date(date)]


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
plot(pmi_daily$day, pmi_daily$pmi, type = "l")

## Merge daily data ------------------------------------------------------------

daily_data <- pmi_daily[news_sentiment_raw, , on = .(day = date)]

daily_data[, quarter:= floor_date(day, unit = "quarter")]
daily_data[, doq:= 1:.N, by = quarter]

daily_data <- daily_data[gdp_gr, , on = .(quarter = quarter)]

daily_data <- na.omit(daily_data)

setnames(daily_data, "News Sentiment", "news") # for easier handling later

# save daily pmi data
write.csv(daily_data, file = paste0(tidy_data_path,"daily_data.csv"),
          row.names = FALSE)
