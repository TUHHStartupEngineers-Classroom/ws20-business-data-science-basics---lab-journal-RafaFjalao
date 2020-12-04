library(tidyverse)
library(vroom)
library(data.table)
library(vroom)

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

#Import all the necessary data to answer the question of the challenge

#patent

patent_tbl <- vroom(
  file       = "challenge3/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

# Transform the tbl into a dt

patent_dt <- setDT(patent_tbl)

rm(patent_tbl)

#patent assignee

patent_assignee_tbl <- vroom(
  file       = "challenge3/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

# Transform the tbl into a dt

patent_assignee_dt <- setDT(patent_assignee_tbl)


rm(patent_assignee_tbl)

#assignee

assignee_tbl <- vroom(
  file       = "challenge3/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

# Transform the tbl into a dt

assignee_dt <- setDT(assignee_tbl)


rm(assignee_tbl)

#uspc

uspc_tbl <- vroom(
  file       = "challenge3/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

# Transform the tbl into a dt

uspc_dt <- setDT(uspc_tbl)


rm(uspc_tbl)

#QUESTION 1: PATENT DOMINANCE 

# Analysing the excel with the dictonary, we can see that in the data element
#type, we can select the kind of patent( 2 for US company or coorporation)
#So I will start doing that to reduce the unnecessary data 

assignee_US_dt <- assignee_dt[type==2, list(id, organization)][
  , .(assignee_id = id, organization = organization)]

#Combination both tables

assignee_patent_assignee_US_dt <-merge(x = assignee_US_dt, y = patent_assignee_dt, 
                                        by    = "assignee_id", 
                                        all.x = FALSE,  # we just keep the ones
                                        all.y = FALSE)  # which appears on both
                                                        #sides


#First I create a new column with all 1, to count the number of patents

totalnumber_patent_US_dt <- assignee_patent_assignee_US_dt[,cal:=1]

# Now I make the total of patents and ordering descending

totalnumber_patent_US_dt <-totalnumber_patent_US_dt[
  , sum(cal), by = organization][
    order(V1, decreasing = TRUE)][,
       .(organization = organization, number_patents = V1)]

top_10 <- totalnumber_patent_US_dt[1:10,.(organization, number_patents)] 

write_rds(top_10,"top_10.rds")

top_10

#QUESTION 2: RECENT PATENT ACTIVITY


# first, I will select the necessary data from patent 

patent_mod_dt <- patent_dt[, .(patent_id=id, date=date)]

# Now I combine the US assignee patent with the date through the patent id
# Just keeping the commom elements will just give us the companies in the USA

patent_date_US_dt <- merge(x = assignee_patent_assignee_US_dt, y = patent_mod_dt, 
                           by    = "patent_id", 
                           all.x = FALSE,  # we just keep the ones
                           all.y = FALSE)  # which appears on both


#Now I will just keep the id of the patents, the ones granted in 2019,the 
#name of the company and the operator cal (I will use it later)
# I use the library lubricate to select the year 
patent_US_2019_dt <- patent_date_US_dt[lubridate::year(date) == "2019", .(organization, date, patent_id)][
  , cal:= 1]


# Now I make the total of patents and ordering descending

totalnumber_patent_US_2019_dt <-patent_US_2019_dt[
  , sum(cal), by = organization][
    order(V1, decreasing = TRUE)][,
  .(organization = organization, number_patents = V1)]

top_10_US_2019 <- totalnumber_patent_US_2019_dt[1:10,.(organization, number_patents)] 

write_rds(top_10_US_2019, "top_10_US_2019.rds")

top_10_US_2019


#QUESTION 3: INNOVATION IN TECH 

#question 3.1: What is the most innovative sector?

# for that I will analyse the uspc_dt
# for that I only need the mainclass variable, and I will add the cal(like before)

uspc_mainclass_dt <- uspc_dt[,.(mainclass_id)][,cal:=1]

# Now I will sum and get the most innovative sector

uspc_innovative_sector <- uspc_mainclass_dt[,sum(cal), by= mainclass_id][
  order(V1, decreasing = TRUE)][,.(mainclass_id=mainclass_id, number_patents = V1)]

top_innovative_sector <- uspc_innovative_sector[1,.(mainclass_id, number_patents)]

write_rds(top_innovative_sector, "top_innovative_sector.rds")

top_innovative_sector

#correspond with ACTIVE SOLID-STATE DEVICES


#Question 3.2: 

#First the top 10 WW 
# Similar process like question 1

assignee_WW_dt <- assignee_dt[,.(assignee_id= id, organization = organization)]

assignee_patent_assignee_WW_dt <-merge(x = assignee_WW_dt, y = patent_assignee_dt, 
                                       by    = "assignee_id", 
                                       all.x = FALSE,  # we just keep the ones
                                       all.y = FALSE) 


#Now let's get the top 10 companies worldwide
#adding first the cal 

assignee_patent_assignee_WW_dt <- assignee_patent_assignee_WW_dt[,cal:=1]

# Now I make the total of patents and ordering descending

totalnumber_patent_WW_dt <-assignee_patent_assignee_WW_dt[
  , sum(cal), by = organization][
    order(V1, decreasing = TRUE)][,
                                  .(organization = organization, number_patents = V1)]

top_10_WW <- totalnumber_patent_WW_dt[1:10,.(organization, number_patents)] 

write_rds(top_10_WW, "top_10_WW.rds")

top_10_WW

# Now the USPC comes into play. We merge all data together

uspc_assignee_patent_assignee_WW_dt <-merge(x = assignee_patent_assignee_WW_dt, y = uspc_dt, 
                                                        by    = "patent_id", 
                                                        all.x = FALSE,  # we just keep the ones
                                                        all.y = FALSE)  # which appears on both



#Cleaning up the data

uspc_assignee_patent_assignee_WW_mod_dt <- uspc_assignee_patent_assignee_WW_dt[,
            .(organization, mainclass_id)][,cal:=1]


# Now we used the top 10 WW obtained before “skipping the one with no name 
#to prevent from errors and introduce it here

uspc_assignee_patent_assignee_top_10_WW_dt<- uspc_assignee_patent_assignee_WW_mod_dt[organization %in%
top_10_WW[!4, organization], sum(cal),by=mainclass_id][
  order(V1, decreasing = TRUE)][,
                                .(mainclass_id = mainclass_id, number_patents = V1)]

#Now I get the top 5 Sector

top_5_sector <- uspc_assignee_patent_assignee_top_10_WW_dt[1:5, .(mainclass_id, number_patents)]

write_rds(top_5_sector, "top_5_sector.rds")
top_5_sector


#257: ACTIVE SOLID-STATE DEVICES
#438: SEMICONDUCTOR DEVICE MANUFACTURING: PROCESS
#365: STATIC INFORMATION STORAGE AND RETRIEVAL
#370: MULTIPLEX COMMUNICATIONS
#358: FACSIMILE AND STATIC PRESENTATION PROCESSING



