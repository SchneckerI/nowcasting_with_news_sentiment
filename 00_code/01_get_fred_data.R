################# FRED-DATA ########################

######Intro#####

#Guide: https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html

#Packages

library(usethis)
library(fredr)
library(data.table)
library(lubridate)
library(xts)

# Function to check if a data table is empty
is_empty_dt <- function(dt) {
  nrow(dt) == 0
}

##FRED API

#request API key from fred website
#getting it into R

# usethis::edit_r_environ()

# Type FRED_API_KEY="keynumber" into opened renviron file and save
# Restart R

###### DATA Paths #####

raw_data_path <- "01_data_raw/"

tidy_data_path <- "02_data_tidy/"

plot_path <- "20_plots/"

###### GDP VINTAGE DATA #####

gdp_raw <- data.table(
  fredr(
  series_id = "GDPC1", # real gdp in levels: Billions of Chained 2017 Dollars, Seasonally Adjusted Annual Rate
  observation_start = as.Date("1979-10-01"),
  observation_end = as.Date("2024-01-01"),
  frequency = "q", # quarterly
  realtime_start = as.Date("1776-07-04") # get all vintages for observations
)
)

setnames(gdp_raw, "date", "quarter")

###### Current VINTAGE #####

gdp_current <- gdp_raw[realtime_end %in% "9999-12-31",]

# variables to transform
transform_vars <- "value"

# log variables
gdp_current[, paste0(transform_vars, "_log"):=lapply(.SD, log), .SDcols = transform_vars]

# growth rate
na_diff <- function(x){
  c(NA, diff(x)*100)
}

# normal growth rate function

gr_fun <- function(x){
  c(NA,(diff(x)/(x[1:length(x)-1]))*100)
}

# gdp_current[, paste0(transform_vars, "_gr"):=lapply(.SD, na_diff), .SDcols = paste0(transform_vars, "_log")]
gdp_current[, paste0(transform_vars, "_gr"):=lapply(.SD, gr_fun), .SDcols = paste0(transform_vars)]


gdp_gr <- na.omit(data.table(
  quarter = gdp_current[, quarter],
  gdp_gr = gdp_current[, value_gr]
))

# quick plot
plot(gdp_gr$quarter, gdp_gr$gdp_gr, type = "l")

## save plot
# 1. Open jpeg file
jpeg(paste0(plot_path, "plot_gdp_gr.jpg"), width = 600, height = 350)
# 2. Create the plot
plot(x = gdp_gr$quarter, y = gdp_gr$gdp_gr,
     pch = 20, frame = TRUE,
     xlab = "Quarter", ylab = "GDP Growth Rate", col = "#2E9FDF",
     type = "l")
# 3. Close the file
dev.off()

# save data
write.csv(gdp_gr, paste0(tidy_data_path, "gdp_latest_vintage.csv"),
          row.names = FALSE)


###### Real time newest VINTAGE #####

daily_index <- seq.Date(from = as.Date("1991-12-04"), # first vintage
                        to = as.Date(today()), # today's vintage
                        by = "days"
                        )


# create placeholder list with one element for each day
real_time_gdp <- vector("list", length = length(daily_index))

# fill with datatables containing specific up-to-date information
for (i in 1:length(daily_index)) {
  real_time_gdp[[i]] <- gdp_raw[realtime_start <= daily_index[i] & daily_index[i] <= realtime_end,]
}

names(real_time_gdp) <- daily_index

save(real_time_gdp, file = paste0(tidy_data_path, "real_time_gdp.RData"))


# test_index <- which(gdp_raw[, realtime_start] <= daily_index[1] & daily_index[1] <= gdp_raw[, realtime_end])
# 
# gdp_raw[test_index]


###### Monthly MACRO VINTAGE DATA #####

# Monthly Macro Data from JAE Paper:
# industrial production, construction production, new passenger
# car registrations, retail trade, external trade, industrial orders, unemployment rate, surveys of the European Commission
# and the Purchasing Managers' surveys for services, manufacturing and construction, consumer confidence, loans to the
# private sector (deflated by HICP), monetary aggregate M1 (deflated by HICP) and two financial indicators: Eurostoxx and
# corporate spreads (i.e., difference between BBB non-financial corporate bond yields and AAA government bond yields).

# daily: nasdaq composite (stocks)

# quarterly: EXPGSC1 (exports)

# loop setup
macro_series <- c("INDPRO", "TTLCONS", "USASACRMISMEI", "RSXFS", "AMTMNO",
                  "UNRATE", "CONSUMER", "M1REAL", "BAA10YM")

new_names <- c("ind_pro", "construct", "car_reg", "ret_sales", "ind_orders",
               "unemp", "cons_loans", "mon_sup", "spread")

# create placeholder list to store data
real_time_macro_m <- vector("list", length = length(daily_index))


# loop
for(j in 1:length(macro_series)){
  print(j)
  curr_series <- macro_series[j]
  
  curr_raw <- data.table(
    fredr(
      series_id = curr_series, # current series
      observation_start = as.Date("1993-01-01"),
      observation_end = as.Date("2024-01-01"),
      frequency = "m", # monthly
      realtime_start = as.Date("1776-07-04") # get all vintages for observations
    )
  )
  
  curr_raw <- curr_raw[, c(-2)]
  
  setnames(curr_raw, c("date"), c("month"))
  
  daily_index <- seq.Date(from = as.Date("1991-12-04"), # first vintage
                          to = as.Date(today()), # today's vintage
                          by = "days"
  )
  
  
  # create placeholder list with one element for each day
  real_time_current <- vector("list", length = length(daily_index))
  
  # fill with datatables containing specific up-to-date information
  for (i in 1:length(daily_index)) {
    real_time_current[[i]] <- curr_raw[realtime_start <= daily_index[i] & daily_index[i] <= realtime_end, .(month, value)]
    
    setnames(real_time_current[[i]], "value" , new_names[j])
  }
  
 names(real_time_current) <- daily_index
 
 if(j == 1){
  real_time_macro_m  <- real_time_current
 } else{
   
   for(k in 1:length(real_time_macro_m)){
   real_time_macro_m[[k]] <- real_time_macro_m[[k]][real_time_current[[k]], , on = .(month = month)]
   }
 }
 
}

names(real_time_macro_m) <- daily_index

real_time_macro_m[[1]]


nrow(real_time_current[[1]])

# Filter out empty data tables
real_time_macro_data_monthly <- Filter(Negate(is_empty_dt), real_time_macro_m)

# save monthly dt
save(real_time_macro_data_monthly, file = paste0(tidy_data_path, "real_time_macro_data_monthly.RData"))

###### Quarterly MACRO VINTAGE DATA #####

# loop setup
macro_series <- c("GDPC1", "EXPGSC1")

new_names <- c("vintage_gdp", "export")

# create placeholder list to store data
real_time_macro_q <- vector("list", length = length(daily_index))


# loop
for(j in 1:length(macro_series)){
  print(j)
  curr_series <- macro_series[j]
  
  curr_raw <- data.table(
    fredr(
      series_id = curr_series, # current series
      observation_start = as.Date("1979-10-01"),
      observation_end = as.Date("2024-01-01"),
      frequency = "q", # quarterly
      realtime_start = as.Date("1776-07-04") # get all vintages for observations
    )
  )
  
  curr_raw <- curr_raw[, c(-2)]
  
  setnames(curr_raw, c("date"), c("quarter"))
  
  daily_index <- seq.Date(from = as.Date("1991-12-04"), # first vintage
                          to = as.Date(today()), # today's vintage
                          by = "days"
  )
  
  
  # create placeholder list with one element for each day
  real_time_current <- vector("list", length = length(daily_index))
  
  # fill with datatables containing specific up-to-date information
  for (i in 1:length(daily_index)) {
    real_time_current[[i]] <- curr_raw[realtime_start <= daily_index[i] & daily_index[i] <= realtime_end, .(quarter, value)]
    
    setnames(real_time_current[[i]], "value" , new_names[j])
  }
  
  names(real_time_current) <- daily_index
  
  if(j == 1){
    real_time_macro_q  <- real_time_current
  } else{
    
    for(k in 1:length(real_time_macro_q)){
      real_time_macro_q[[k]] <- real_time_macro_q[[k]][real_time_current[[k]], , on = .(quarter = quarter)]
    }
  }
  
}

names(real_time_macro_q) <- daily_index

# Filter out empty data tables
real_time_macro_data_quarterly <- Filter(Negate(is_empty_dt), real_time_macro_q)

# save quarterly dt
save(real_time_macro_data_quarterly, file = paste0(tidy_data_path, "real_time_macro_data_quarterly.RData"))

###### DAILY MACRO VINTAGE DATA #####

# NASDAQCOM
curr_raw <- data.table(
  fredr(
    series_id = "NASDAQCOM", # current series
    observation_start = as.Date("1979-10-01"),
    observation_end = as.Date("2024-01-01"),
    frequency = "d" #daily # get all vintages for observations
  )
)

curr_cut <- curr_raw[,.(date, value)]

nasdaq_daily <- as.data.table(nafill(curr_cut, type = "locf"))

setnames(nasdaq_daily, "date", "day")

# save
write.csv(nasdaq_daily, paste0(tidy_data_path, "nasdaq_daily.csv"),
          row.names = FALSE)
