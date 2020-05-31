library(tidyverse) 

date <- "05-30-2020"

case_dat <-  read.csv((paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", date,".csv"))) %>%  
  select(Country_Region, Confirmed, Deaths) %>%
  group_by(Country_Region) %>% 
  summarise_all(tibble::lst(sum)) %>% 
  transmute(country= Country_Region,
            cases = Confirmed_sum,
            deaths = Deaths_sum)

final_dat <- left_join(case_dat, read.csv("data/IRC3.csv",na.strings="n/a")) %>% 
  select(region,
         country,
         IRC_site,
         GHSI,
         cases,
         deaths,
         stance,
         phase,
         risk,
         stance_phase) %>% 
  arrange(desc(cases))

#save as excel file
#install.packages("writexl")
library(writexl)
write_xlsx(x = final_dat, path = "C:/Users/maryd/Documents/R datasets/coronavirus3/final_xl.xlsx", col_names = TRUE)

#save as csv file
write.csv (final_dat, file = "final.csv")
