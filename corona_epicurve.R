library(tidyverse) 
library(scales)

#get test data from our world in data
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

#Countries and dates reporting testing
tests_dat <- case_dat %>%
  group_by(country, date) %>%
  filter(total_tests >= 0)

#dataframe for last 6 weeks or so to create epi curve
curve_dat <- case_dat %>%
  select(country, date, new_cases, new_deaths) %>%
  filter(date >= "2020-04-01")

#bar chart of new_cases by country and date
ggplot(curve_dat %>% filter(country == "Nigeria")) +
  geom_col(mapping = aes(x = date, y = new_cases))

ggplot(tests_dat %>% filter(country == "Nigeria")) +
  geom_col(mapping = aes(x = date, y = testposrate)) 


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

### bar chart and test positivity for each region ###
#Asia 
ggplot(curve_dat %>%  filter(country %in% irc_asia)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y") 

ggplot(tests_dat %>% filter(country %in% irc_asia)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#East Africa
ggplot(curve_dat %>%  filter(country %in% irc_eaf)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

ggplot(tests_dat %>% filter(country %in% irc_eaf)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#Europe
ggplot(curve_dat %>%  filter(country %in% irc_euro)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

ggplot(tests_dat %>% filter(country %in% irc_euro)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#Great Lakes
ggplot(curve_dat %>%  filter(country %in% irc_lakes)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

ggplot(tests_dat %>% filter(country %in% irc_lakes)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")     
  #Note: as of 20 may no great lakes country reporting testing

#Latin America
ggplot(curve_dat %>%  filter(country %in% irc_latin)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

ggplot(tests_dat %>% filter(country %in% irc_latin)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#Middle East
ggplot(curve_dat %>%  filter(country %in% irc_me)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

ggplot(tests_dat %>% filter(country %in% irc_me)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#United States
ggplot(curve_dat %>%  filter(country %in% irc_usa)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

ggplot(tests_dat %>% filter(country %in% irc_usa)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#West Africa
ggplot(curve_dat %>%  filter(country %in% irc_waf)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = country)) +
  facet_wrap(~country, scales = "free_y")

ggplot(tests_dat %>% filter(country %in% irc_waf)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/maryd/Documents/R datasets/coronavirus3/plots")

######### need to figure out better way to save all ###########

#middle east
all_me <- c("Turkey",
            "Saudi Arabia",
            "Qatar",
            "United Arab Emirates",
            "Kuwait",
            "Israel",
            "Egypt",
            "Bahrain",
            "Algeria",
            "Morocco",
            "Oman",
            "Armenia",
            "Iraq",
            "Azerbaijan",
            "Lebanon",
            "Tunisia",
            "Cyprus",
            "Jordan",
            "Libya",
            "Syria")

ggplot(curve_dat %>%  filter(country %in% all_me)) +
  geom_col(mapping = aes(x = date, y = new_cases, fill = "red")) +
  facet_wrap(~country, scales = "free_y") 

#plot cumulative cases
ggplot(case_dat %>%  filter(country %in% all_me)) +
  geom_point(mapping = aes(x = date, y = cases, color = country))

ggplot(tests_dat %>% filter(country %in% all_me)) +
  geom_col(mapping = aes(x = date, y = testposrate, fill = country)) +
  facet_wrap(~country, scales = "free_y")

#ALL IRC country programs - currently excludes offices
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



#save as excel file
#install.packages("writexl")
library(writexl)
write_xlsx(x = final_dat, path = "C:/Users/maryd/Documents/R datasets/Coronavirus3/oxford_xl.xlsx", col_names = TRUE)
