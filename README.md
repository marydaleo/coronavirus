# coronavirus

getwd()
setwd("C:/Users/maryd/Documents/R datasets/Coronavirus2")
library(readr)
library(knitr)
library(dplyr)
library(tidyr)
#get data for current date: 08 March 2020
corona <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-08-2020.csv"

corona_csv <- read_csv(corona)
corona_tbl <- tbl_df(corona_csv)

rm(corona)
rm(corona_csv)

#look at data
names(corona_tbl)
summary(corona_tbl)
glimpse(corona_tbl)
print(ls(corona_tbl))

### SELECT COUNTRIES AND THE CURRENT DATE ###

variable_list <- select(corona_tbl, "Country/Region", "Confirmed", "Deaths")
names(variable_list) = c("country", "cases", "deaths")
variable_list$cases<-as.numeric(variable_list$cases)
variable_list$deaths<-as.numeric(variable_list$deaths)
variable_list
names(variable_list)

#combine country/regions for a cumulative case count for each country
todaycases<-variable_list %>% 
  group_by(country) %>% 
  summarise(totalcases = sum(cases, na.rm = TRUE))
todaycases$totalcases<-as.numeric(todaycases$totalcases)

todaydeaths<-variable_list %>%
  group_by(country) %>%
  summarise(totaldeaths = sum(deaths, na.rm = TRUE))
todaydeaths$totaldeaths<-as.numeric(todaydeaths$totaldeaths)

rm(corona_tbl)
rm(variable_list)

#join cases and deaths
today_case_death <- left_join(todaycases, todaydeaths, by = "country", copy = TRUE)

rm(todaycases)
rm(todaydeaths)
#save(todaycases, file = "C:/Users/maryd/Documents/R datasets/Coronavirus/todaycases.csv")

#save as excel file
#install.packages("writexl")
#library(writexl)
#write_xlsx(x = todaycases, path = "C:/Users/maryd/Documents/R datasets/Coronavirus/todaycases.xlsx", col_names = TRUE)


#bring in ghsi data

ghsi <- "C:/Users/maryd/Documents/R datasets/Coronavirus/GHSI.csv"
ghsi_csv <- read_csv(ghsi)
ghsi_tbl <- ghsi_csv

glimpse(ghsi_tbl)

rm(ghsi)
rm(ghsi_csv)


#join ghsi data w/ today cases
cases_ghsi <- left_join(today_case_death, ghsi_tbl, by = "country", copy = TRUE)
rm(ghsi_tbl)
rm(todaycases)

#bring in regions + countries w/ IRC presence#

ircregions <- "C:/Users/maryd/Documents/R datasets/Coronavirus/country_regions.csv"
ircregions_csv <- read_csv(ircregions)
ircregions_tbl <- tbl_df(ircregions_csv)

#summary(ircregions_tbl)
#glimpse(ircregions_tbl)

rm(ircregions)
rm(ircregions_csv)

#join cases_ghsi data w/ ircregions - TBC
cases_ghsi_irc <- left_join(cases_ghsi, ircregions_tbl, by = "country", copy = TRUE)

rm(cases_ghsi)


#save as excel file
install.packages("writexl")
library(writexl)
write_xlsx(x = cases_ghsi_irc, path = "C:/Users/maryd/Documents/R datasets/Coronavirus2/cases_ghsi_irc.xlsx", col_names = TRUE)

