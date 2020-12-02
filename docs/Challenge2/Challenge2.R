library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi) 
library(dplyr)


# Second Challenge

url_home <- "https://www.rosebikes.com/bikes"

html_home <- url_home %>%
  read_html()

family_tbl <- html_home%>%
  html_nodes(css=".main-navigation-category-with-tiles__title") %>%
  html_text()%>%
  discard(.p = ~stringr::str_detect(.x,"Urban|Sale|Kids|Bikes"))%>% #deleting
  as_tibble()

ivector <- as_vector(1:nrow(family_tbl)) # create a vector for the interation 

family_name_tbl <- as_tibble()

family_name_price_tbl<- as_tibble()
bikes_data_tbl <- as_tibble()
for (i in ivector){
family_name_tbl<- as_tibble() 
name <- family_tbl[i,1]%>%
  str_replace_all("\n", "")
  

url_home <- glue("https://www.rosebikes.com/bikes/{name}")


html<- url_home %>%
  read_html

model_name_tbl<- html%>%
  html_nodes(css=".catalog-category-bikes__title-text") %>%
  html_text()%>%
  str_replace_all(pattern="\n",replacement = "")%>%
  as_tibble()%>%
  rename(model_name=value)

price_tbl <- html%>%
  html_nodes(css=".catalog-category-bikes__price-title") %>%
  html_text()%>%
  str_replace_all(pattern=",",replacement = "")%>%  # Delete the comma
  str_replace_all(pattern="\n",replacement = "")%>% # delete the \n
  as_tibble()%>%
  separate(col=value, into =c("from","price"), sep= "€")%>% #eliminating from
  select("price")
  


ivector_family<- as_vector(1:nrow(price_tbl))

for (i in ivector_family){
  
  family_name_tbl[i,1]<-name
}

family_name_tbl <-family_name_tbl%>%
  rename (family_name=...1)

family_name_price_tbl<- bind_cols(family_name_tbl, model_name_tbl, price_tbl)

bikes_data_tbl<- bind_rows(bikes_data_tbl, family_name_price_tbl)


}                                         

print(bikes_data_tbl)

  



