library(tidyverse) 
library(scales)

#date <- "05-10-2020"

dat <- read.csv((paste0("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")))

#all countries cases, deaths, testing - still need to summarize
case_dat <- dat %>%
    select(iso_code, location, date, total_cases, new_cases, total_deaths, new_deaths, total_tests, new_tests, 
         total_tests_per_thousand, tests_units, population, cvd_death_rate) %>%
  mutate(date = as.Date(date), country = location) %>%
  group_by(country,date) %>%
  transmute(cases = total_cases,
            new_cases = new_cases,
            deaths = total_deaths,
            new_deaths = new_deaths,
            total_tests = total_tests,
            testposrate = sum(cases/total_tests),
            tests_per_thousand = total_tests_per_thousand,
            tests_per_million = total_tests_per_thousand*1000,
            fatality_ratio = deaths/cases,
            population = population,
            cvd_death_rate = cvd_death_rate)

#get just testing and look at Test Positivity Rate
tests_dat <- case_dat %>%
  group_by(country, date) %>%
  filter(total_tests >= 0)

#visualize test positivity over time
ggplot(tests_dat %>% filter(country == "Germany")) +
  geom_col(mapping = aes(x = date, y = testposrate))


#look at most recent testing data - NEEDS WORK!!!!
tests_sum <- case_dat %>% 
  group_by(country) %>% 
  filter(total_tests == max(total_tests, na.rm = TRUE)) %>%
  arrange(desc(total_tests)) 
  

#all countries growth rates  
dat_growth <- dat %>%
  select(location, date, total_cases) %>%
  group_by(location,date) %>%
  summarise(total_cases = sum(total_cases)) %>%
  group_by(location) %>%
  mutate(growth_rate = round(total_cases/lag(total_cases, 1) - 1,2),
         doubling_time = round(log(2)/growth_rate,2),
         growth_rate = growth_rate,
         date = as.Date(date))  

#filter by growth rate AND see growth rate weekly avg
filter_growth <- dat_growth %>%
  filter(date >= max(date)-7) %>%
  mutate(week_avg = mean(growth_rate)) %>%
  mutate(week_avg_dbl = mean(doubling_time)) %>%
  arrange(desc(week_avg))

#bar chart of new_cases by country and date
ggplot(case_dat %>% filter(country == "Sierra Leone")) +
  geom_col(mapping = aes(x = date, y = new_cases))

#dataframe for last 6 weeks or so to create epi curve
curve_dat <- case_dat %>%
  select(country, date, new_cases, new_deaths) %>%
  filter(date >= "2020-04-01")

#IRC countries by region
irc_asia <- c("Pakistan",
             "Malaysia",
             "Bangladesh",
             "Thailand",
             "Afghanistan",
             "Burma")

irc_eaf <- c("Kenya",
                "Somalia",
                "Ethiopia",
                "Sudan",
                "Uganda",
                "Zimbabwe",
                "South Sudan",
                "Yemen")

irc_euro <- c("Italy",
              "Germany",
              "United Kingdom",
              "Belgium",
              "Switzerland",
              "Sweden",
              "Serbia",
              "Greece",
              "Bosnia and Herzegovina")

irc_lakes <- c("Democratic Republic of Congo",
               "Tanzania",
               "Central African Republic",
               "Burundi")

irc_latin <- c("Mexico",
               "Colombia",
               "Honduras",
               "Guatemala",
               "Venezuela",
               "El Salvador")

irc_me <- c("Iraq",
            "Tunisia",
            "Lebanon",
            "Jordan",
            "Libya",
            "Syria")

irc_usa <- c("United States")

irc_waf <- c("Cameroon",
             "Cote d'Ivoire",
             "Nigeria",
             "Niger",
             "Burkina Faso",
             "Senegal",
             "Mali",
             "Liberia",
             "Sierra Leone",
             "Chad")
 
### bar chart for each region ###
#Asia 
ggplot(curve_dat %>%  filter(country %in% irc_asia)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y") 

#test positivity for asia#
ggplot(tests_dat %>% filter(country %in% irc_asia)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#East Africa
ggplot(curve_dat %>%  filter(country %in% irc_eaf)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#Europe
ggplot(curve_dat %>%  filter(country %in% irc_euro)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#Great Lakes
ggplot(curve_dat %>%  filter(country %in% irc_lakes)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#Latin America
ggplot(curve_dat %>%  filter(country %in% irc_latin)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#Middle East
ggplot(curve_dat %>%  filter(country %in% irc_me)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#United States
ggplot(curve_dat %>%  filter(country %in% irc_usa)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#West Africa
ggplot(curve_dat %>%  filter(country %in% irc_waf)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")



#bar chart of IRC cases by country and date - NEEDS WORK!!!
irc_countries <- c("Afghanistan",
                   "Bangladesh",
                   "Bosnia and Herzegovina",
                   "Burkina Faso",
                   "Burundi",
                   "Cameroon",
                   "Central African Republic",
                   "Chad",
                   "Colombia",
                   "Congo (Kinshasa)",
                   "Cote d'Ivoire",
                   "El Salvador",
                   "Ethiopia",
                   "Greece",
                   "Guatemala",
                   "Honduras",
                   "Iraq",
                   "Jordan",
                   "Kenya",
                   "Lebanon",
                   "Liberia",
                   "Libya",
                   "Malaysia",
                   "Mali",
                   "Mexico",
                   "Burma",
                   "Niger",
                   "Nigeria",
                   "Pakistan",
                   "Senegal",
                   "Serbia",
                   "Sierra Leone",
                   "Somalia",
                   "South Sudan",
                   "Sudan",
                   "Syria",
                   "Tanzania",
                   "Thailand",
                   "Tunisia",
                   "Uganda",
                   "Venezuela",
                   "Yemen",
                   "Zimbabwe")




#join IRC data to this data #
final_dat <- left_join(case_dat, read.csv("data/IRC3.csv")) %>% 
#  left_join(read.csv("data/IRC3.csv",na.strings="n/a")) %>% 
  select(region,
         country,
         IRC_site,
         GHSI,
         cases,
         deaths,
         tests,
         testposrate,
         tests_per_million,
         stance,
         phase,
         risk) %>% 
  arrange(desc(cases))

#save as excel file
#install.packages("writexl")
library(writexl)
write_xlsx(x = final_dat, path = "C:/Users/maryd/Documents/R datasets/Coronavirus3/oxford_xl.xlsx", col_names = TRUE)
