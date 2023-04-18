library(tidyverse)
library(readr)
library(lubridate)

bikes_tbl <- read_excel(path='00_data/01_bike_sales/01_raw_data/bikes.xlsx')
orderlines_tbl <- read_excel(path='00_data/01_bike_sales/01_raw_data/orderlines.xlsx')
bikeshops_tbl <-read_excel(path='00_data/01_bike_sales/01_raw_data/bikeshops.xlsx')


bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_wrangled_tbl<- bike_orderlines_joined_tbl %>%
  #Separate State and city
  separate(col= location, into=c ("city", "state"), sep = ',') %>%
  # Selecction of necessary data
  select (...1, order.id, order.date, quantity, price, state)%>%
  #Total price
  mutate (total_price = price*quantity)%>%
  #Correcting the point
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# First Point: Sales by State
#Step 1: Preparing the represented dat<
sales_by_state<- bike_orderlines_wrangled_tbl %>%
  #Selecting the necessary data
  select (state, total_price)%>%
  group_by(state)%>%
  summarize(sales= sum(total_price))%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))


#Step 2: Graphic
graph_sales_state <- sales_by_state%>%
  ggplot(aes(x=state, y=sales ))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) +
  # Adding labels to the bars

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +

  labs(
    title    = "SALES BY STATE",
    subtitle = "",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# Point 2: Sales by location and year 

sales_state_year <- bike_orderlines_wrangled_tbl %>%
  
  select(order_date, total_price, state) %>%
  mutate (year = year(order_date))%>%
  group_by(year, state)%>%
  summarise(sales=sum(total_price))%>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

graph_sales_state_year<- sales_state_year%>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "REVENUE BY YEAR AND STATE",
    subtitle = "",
    fill = "States" # Changes the legend name
  )


  
  
  
  




  
  


  

