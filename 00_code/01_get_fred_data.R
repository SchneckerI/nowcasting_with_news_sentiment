################# FRED-DATA ########################

######Intro#####

#Guide: https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html

#Packages

library(usethis)
library(fredr)
library(data.table)
library(lubridate)

##FRED API

#request API key from fred website
#getting it into R

# usethis::edit_r_environ()

# Type FRED_API_KEY="keynumber" into opened renviron file and save
# Restart R

###### DATA Paths #####

raw_data_path <- "01_data_raw/"

tidy_data_path <- "02_data_tidy/"

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

gdp_current[, paste0(transform_vars, "_gr"):=lapply(.SD, na_diff), .SDcols = paste0(transform_vars, "_log")]

gdp_gr <- na.omit(data.table(
  quarter = gdp_current[, date],
  gdp_gr = gdp_current[, value_gr]
))

# quick plot
plot(gdp_gr$quarter, gdp_gr$gdp_gr, type = "l")

# save
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
