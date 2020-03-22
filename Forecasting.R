library(data.table)
library(tidyverse)
library(ggthemes)
library(fable)
library(feasts)
library(tsibble)
library(lubridate)

# save and import coronavirus data from Johns Hopkins University, with system date in filenames for posterity
corona_url = "https://github.com/CSSEGISandData/COVID-19/"
corona_confirmed = curl_download("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                                 paste0("data\\",Sys.Date(),"_confirmedcases.csv"))
corona_deaths = curl_download("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
                              paste0("data\\",Sys.Date(),"_deaths.csv"))
corona_recovered = curl_download("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",
                                 paste0("data\\",Sys.Date(),"_recovered.csv"))
# read in data on confirmed cases, deaths and recoveries
covid_confirmed = fread(corona_confirmed)[,data:="confirmed"]
covid_deaths = fread(corona_deaths)[,data:="deaths"]
covid_recovered = fread(corona_recovered)[,data:="recovered"]

# aggregate data by country
covid_data = covid_data[,lapply(.SD, sum), keyby=.(`Country/Region`, data)]
# melt to long form data, and convert dates to actual Date object
covid_data = melt(covid_data, c("Country/Region", "data"), variable.name = "date")[,date:=mdy(date)] %>% 
  janitor::clean_names() %>% 
  as.data.table()
setkey(covid_data, data)
# join to population data and drop NA
covid_data = na.omit(population[covid_data, on="country_region"])
covid_data[,value_per_cap := value/population] # get per cap values

vic_covid_data = rbindlist(list(covid_confirmed, covid_deaths, covid_recovered))[`Province/State`=="Victoria" & `Country/Region`=="Australia"][,`:=`(Lat=NULL, Long=NULL, `Country/Region`=`Province/State`, `Province/State`=NULL)]
vic_covid_data = melt(vic_covid_data, c("Country/Region", "data"), variable.name = "date")[,date:=mdy(date)] %>% 
  janitor::clean_names() %>% 
  as.data.table()
vic_covid_data[,population:=6629870][,value_per_cap:=value/population] # hardcode vic population from ERP sep-2019
covid_data = rbind(vic_covid_data, covid_data)

# Time series modelling ----
covid_data = covid_data %>% 
  as_tsibble(key=c(country_region, data), index=date)
cases_fit <- covid_data %>% 
  filter(data=="confirmed") %>% 
  model(
    cases_arima = ARIMA(value),
  )
percap_fit <- covid_data %>% 
  filter(data=="confirmed") %>% 
  model(
    percap_arima = ARIMA(value_per_cap),
  )

cases_fit %>% 
  filter(country_region=="Victoria") %>% 
  forecast() %>% 
  autoplot(filter(covid_data, country_region=="Victoria", data=="confirmed")) +
  theme_tufte() +
  scale_x_date(name="Date", breaks=scales::breaks_width("2 weeks"), labels=scales::label_date_short()) +
  scale_y_continuous(name="Cases", breaks=scales::breaks_extended(), labels=scales::label_comma())

percap_fit %>% 
  filter(country_region=="Victoria") %>% 
  forecast() %>% 
  autoplot(filter(covid_data, country_region=="Victoria", data=="confirmed")) +
  theme_tufte() +
  scale_x_date(name="Date", breaks=scales::breaks_width("2 weeks"), labels=scales::label_date_short()) +
  scale_y_continuous(name="Share of Population", breaks=scales::breaks_extended(), labels=scales::label_percent())
