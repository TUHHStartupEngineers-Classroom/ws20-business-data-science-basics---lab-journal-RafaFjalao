library(tidyverse)
library(lubridate)
library(scales)
library(maps)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


# Graphic 1
#Data Wrangling 
# I had a problem with the dates, I dont know what happen, but I tried everything
# and it kept giving me the false results.
#So for this point, I made it the best I could 

covid_data_spain_tbl <- covid_data_tbl%>%
  select("dateRep","day", "month", "year", "cases", "countriesAndTerritories", "continentExp")%>%
  rename("country"="countriesAndTerritories", "continent"= "continentExp", "date"="dateRep")%>%
  filter(year==2020)%>%
  filter(country%>% str_detect("Spain"))%>%
  arrange(day)%>%
  arrange(month)%>%
  mutate(acum = cumsum(cases))%>%
  select("acum","country")

covid_data_spain_tbl<-covid_data_spain_tbl%>%mutate(orden= 1:nrow(covid_data_spain_tbl)) 
  

covid_data_germany_tbl <- covid_data_tbl%>%
  select("dateRep","day", "month", "year", "cases", "countriesAndTerritories", "continentExp")%>%
  rename("country"="countriesAndTerritories", "continent"= "continentExp", "date"="dateRep")%>%
  filter(year==2020)%>%
  filter(country%>% str_detect("Germany"))%>%
    arrange(day)%>%
    arrange(month)%>%
    mutate(acum = cumsum(cases))%>%
    select("acum","country")

covid_data_germany_tbl<- covid_data_germany_tbl%>%
  mutate(orden= 1:nrow(covid_data_germany_tbl))
covid_data_germany_tbl<- covid_data_germany_tbl[-c(339),]

covid_data_france_tbl <- covid_data_tbl%>%
  select("dateRep","day", "month", "year", "cases", "countriesAndTerritories", "continentExp")%>%
  rename("country"="countriesAndTerritories", "continent"= "continentExp", "date"="dateRep")%>%
  filter(year==2020)%>%
  filter(country%>% str_detect("France"))%>%
  arrange(day)%>%
  arrange(month)%>%
  mutate(acum = cumsum(cases))%>%
  select("acum","country")

covid_data_france_tbl<- covid_data_france_tbl%>%
  mutate(orden= 1:nrow(covid_data_france_tbl))
covid_data_france_tbl<- covid_data_france_tbl[-c(339),]

covid_data_UK_tbl <- covid_data_tbl%>%
  select("dateRep","day", "month", "year", "cases", "countriesAndTerritories", "continentExp")%>%
  rename("country"="countriesAndTerritories", "continent"= "continentExp", "date"="dateRep")%>%
  filter(year==2020)%>%
  filter(country%>% str_detect("United_Kingdom"))%>%
  arrange(day)%>%
  arrange(month)%>%
  mutate(acum = cumsum(cases))%>%
  select("acum", "country")
  
covid_data_UK_tbl<-covid_data_UK_tbl%>%
  mutate(orden= 1:nrow(covid_data_UK_tbl))
covid_data_UK_tbl<- covid_data_UK_tbl[-c(339),]



covid_data_USA_tbl <- covid_data_tbl%>%
  select("dateRep","day", "month", "year", "cases", "countriesAndTerritories", "continentExp")%>%
  rename("country"="countriesAndTerritories", "continent"= "continentExp", "date"="dateRep")%>%
  filter(year==2020)%>%
  filter(country%>% str_detect("United_States_of_America"))%>%
  arrange(day)%>%
  arrange(month)%>%
  mutate(acum = cumsum(cases))%>%
  select("acum","country")

covid_data_USA_tbl<-covid_data_USA_tbl%>%
  mutate(orden= 1:nrow(covid_data_USA_tbl))

covid_data_USA_tbl<- covid_data_USA_tbl[-c(339),]


for_graph <- bind_rows( covid_data_UK_tbl, covid_data_USA_tbl, covid_data_france_tbl,covid_data_germany_tbl, covid_data_spain_tbl)



for_graph%>%
  ggplot(aes(x=orden, y= acum, color = country))+
  geom_line(size = 1, linetype = 1)+
  scale_color_manual(values=c("yellow2", "slateblue","salmon2", "purple", "red3"))+
  labs(
    title = "Coronavirus in Spain, UK, Germany, UK and USA 2020",
    subtitle = "",
    caption = "04.12.2020",
    x = "Days",
    y = "Cumulative cases",
    color = "")+
  theme(legend.position  = "bottom", 
        legend.direction = "horizontal",
        axis.text.x = element_text(size=5),
        axis.text.y = element_text(size=5))


#Graphic 2

#Data Wrangling
world <- map_data("world")

covid_data_mod <- covid_data_tbl%>%
  select("deaths", "countriesAndTerritories", "popData2019")%>%
  rename("region"="countriesAndTerritories", "deaths"="deaths", "population"="popData2019")%>%
  mutate(across(region, str_replace_all, "_", " "))%>%
  mutate(region = case_when(
    region == "United Kingdom" ~ "UK",
    region == "United States of America" ~ "USA",
    region == "Czechia" ~ "Czech Republic",
    TRUE ~ region ))%>%
  group_by(region)%>%
  summarise(total_deaths= sum(deaths/population))%>%
  ungroup()

plot_data <- left_join(covid_data_mod, world, by = "region")


ggplot(plot_data, aes(map_id = region, fill =total_deaths*100 ))+
  geom_map(map = plot_data,  color = "red")+
  expand_limits(x = plot_data$long, y = plot_data$lat)+
  scale_fill_viridis_c(
    alpha = 1,
    begin = 0,
    end = 1,
    direction = 1,
    option = "C",
    values = NULL,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill")+labs(
    title = "Confirmed COVID-19 deaths to the size of the population",
    subtitle = "",
    caption = "04.12.2020",
    x = "",
    y = "",
    fill= ("Mortality rate in %"))+
  theme(legend.position  = "top", 
        legend.direction = "horizontal",
        axis.text.x = element_text(size=0),
        axis.text.y = element_text(size=0))
  
  

  
           




  


  


  
