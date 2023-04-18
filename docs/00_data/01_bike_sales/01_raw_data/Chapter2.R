library(tidyverse)
library(readr)

bikes_tbl <- read.xlsx(path='00_data/01_bike_sales/01_raw_data/bikes.xlsx')
orderlines_tbl <- read_excel(path='00_data/01_bike_sales/01_raw_data/orderlines.xlsx')
bikeshops_tbl <-read_excel(path='00_data/01_bike_sales/01_raw_data/bikeshops.xlsx')




