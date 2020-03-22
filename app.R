library(shiny)
library(shinyWidgets)
library(curl)
library(zip)
library(tidyverse)
library(magrittr)
library(data.table)
library(ggthemes)
library(lubridate)

# source("T:/TIGR_EFP/REVENUE AND TAXATION/Tools/DTF Style Guides/DTF Tax and Gambling ggtheme.R")

# save and import population data from world bank (2018)
wb_population_url = "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv"
# check if data file already exists, download if not
if (length(list.files("resources", pattern="^API_SP.POP.TOTL"))==0){
    file = curl_download(wb_population_url, tempfile())
    data_files = zip_list(file) %>% .$filename  %>% str_subset("^API_SP.POP.TOTL")
    unzip(file, data_files, exdir = "resources")
}
# read to data.table and keep only country and 2018 population
population = fread(list.files("resources", pattern="^API_SP.POP.TOTL", full.names=TRUE), skip = 4, header = TRUE)[,.(country_region=`Country Name`,population=`2018`)][,country_region:=ifelse(country_region=="United States", "US", country_region)]

countries_investigated = c("Victoria",
                           "Australia")

# save and import coronavirus data from Johns Hopkins University
corona_url = "https://github.com/CSSEGISandData/COVID-19/"
corona_confirmed = curl_download("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                                 paste0("resources\\",Sys.Date(),"_confirmedcases.csv"))
corona_deaths = curl_download("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
                              paste0("resources\\",Sys.Date(),"_deaths.csv"))
corona_recovered = curl_download("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",
                                 paste0("resources\\",Sys.Date(),"_recovered.csv"))


# read in data on confirmed cases, deaths and recoveries
covid_confirmed = fread(corona_confirmed)[,data:="confirmed"]
covid_deaths = fread(corona_deaths)[,data:="deaths"]
covid_recovered = fread(corona_recovered)[,data:="recovered"]
# join datasets and drop latitude, longitude, and province/state data (only interested in nations, but can re-add Victoria)
covid_data = rbindlist(list(covid_confirmed, covid_deaths, covid_recovered))[,`:=`(Lat=NULL, Long=NULL, `Province/State`=NULL)]
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
covid_data[,date:=date]
vic_covid_data[,date:=date]

most_recent_vic = vic_covid_data[,max(date)]
first_forecast = most_recent_vic+1
first_case_vic = vic_covid_data[data=="confirmed" & value>0, min(date)]

cov_growth = copy(covid_data)[data=="confirmed"]
cov_growth[,end_dates := (value!=0|date==most_recent_vic)*1, by=country_region]
cov_growth[end_dates==1, end_dates := ifelse(date==min(date)|date==most_recent_vic, 1, 0), by=country_region]
cov_growth = cov_growth[end_dates==1,
                         {
                             cagr=(max(value)/min(value))^(1/as.numeric(difftime(max(date), min(date), "days")))-1
                             .(cagr)
                         },
                         by=country_region]
growth_quartiles = quantile(cov_growth$cagr)



# ui = fluidPage(
#   title="COVID-19 Dashboard",
#   sidebarLayout(position = "right",
#     sidebarPanel(
#     # mainPanel(
#       h4("Choose plotting options"),
#       radioButtons("log", "Scale for y-axis:", choices = list("Linear Scale"=FALSE, "Log Scale"=TRUE)),
#       materialSwitch("smooth", "Use smoothed curve:", value=TRUE),
#       
#       fluidRow(column(6, selectInput("plotvariable",
#                                      "Variable:",
#                                      list("Confirmed Cases"="confirmed", "Deaths"="deaths", "Recovered"="recovered"))),
#                column(6, selectInput("percap",
#                                      "Values:",
#                                      list("Per Capita (2018)"="value_per_cap", "Total"="value")))),
#       dateRangeInput("date_range",
#                      "Date Range:",
#                      min = min(covid_data$date), 
#                      max = max(covid_data$date),
#                      start = min(covid_data$date), 
#                      end = max(covid_data$date)),
#       pickerInput("countries",
#                   "Countries/Regions:",
#                   choices=unique(covid_data$country_region),
#                   selected=countries_investigated,
#                   multiple=TRUE,
#                   options = pickerOptions(actionsBox=TRUE))
#     ),
#     mainPanel(
#       h2("Coronavirus (COVID-19) in Victoria"),
#       p("As at ",
#         format(most_recent_vic, "%d %B %Y"),
#         "Victoria had",
#         vic_covid_data[date==most_recent_vic & data=="confirmed", value],
#         "confirmed cases, or",
#         scales::percent(vic_covid_data[date==most_recent_vic & data=="confirmed", value_per_cap], accuracy=0.001),
#         "per cent of the population."),
#       p("The first case in Victoria was recognised on ",
#         format(first_case_vic, "%d %B %Y"),
#         ", reflecting an average growth rate of",
#         scales::percent(cov_growth[country_region=="Victoria",cagr], accuracy = 0.1),
#         "per day in comparison to an average rate of",
#         scales::percent(cov_growth[,mean(cagr)], accuracy = 0.1),
#         "among observed countries/regions."),
#       plotOutput("chooseplot", height = 850),
#       plotOutput("growthplot")
#     )  
#   )
# )

ui = fluidPage(
    title="COVID-19 Dashboard",
    mainPanel(width = 12,
              h2("Coronavirus (COVID-19) in Victoria"),
              p("As at ",
                format(most_recent_vic, "%d %B %Y"),
                "Victoria had",
                strong(vic_covid_data[date==most_recent_vic & data=="confirmed", value]),
                "confirmed cases, or",
                strong(scales::percent(vic_covid_data[date==most_recent_vic & data=="confirmed", value_per_cap], accuracy=0.001)),
                "per cent of the population."),
              p("The first case in Victoria was recognised on ",
                format(first_case_vic, "%d %B %Y"),
                ", reflecting an average growth rate of",
                strong(scales::percent(cov_growth[country_region=="Victoria",cagr], accuracy = 0.1)),
                "per day in comparison to an average rate of",
                strong(scales::percent(cov_growth[,mean(cagr, na.rm=TRUE)], accuracy = 0.1)),
                "among observed countries/regions."),
              fluidRow(
                  column(3,
                         h4("Choose plotting options"),
                         radioButtons("log", "Scale for y-axis:", choices = list("Linear Scale"=FALSE, "Log Scale"=TRUE)),
                         materialSwitch("smooth", "Use smoothed curve:", value=TRUE),
                         numericInput("smoothingfactor", "Smoothing factor:", value=15, min=1, max=100),
                         selectInput("plotvariable",
                                     "Variable:",
                                     list("Confirmed Cases"="confirmed", "Deaths"="deaths", "Recovered"="recovered")),
                         selectInput("percap",
                                     "Values:",
                                     list("Per Capita (2018)"="value_per_cap", "Total"="value")),
                         dateRangeInput("date_range",
                                        "Date Range:",
                                        min = min(covid_data$date), 
                                        max = max(covid_data$date),
                                        start = min(covid_data$date), 
                                        end = max(covid_data$date)),
                         pickerInput("countries",
                                     "Countries/Regions:",
                                     choices=unique(covid_data$country_region),
                                     selected=countries_investigated,
                                     multiple=TRUE,
                                     options = pickerOptions(actionsBox=TRUE))
                  ),
                  column(9, plotOutput("chooseplot", height = 600))
              ),
              h4("COVID-19 confirmed case CAGR distribution across nations from first case"),
              plotOutput("growthplot"),
              h4("Forecast of cases in Victoria"),
              numericInput("forecastlength", "Forecast window (days):", min=1, value=10),
              p("Dashed line reflects a 1st quartile CAGR relative to other nations, with the upper and lower bands reflecting a median CAGR and continuing current Victorian trends respectively."),
              plotOutput("forecast")
    )  
)

server = function(input, output){
    
    y_scale = reactive({
        ifelse(input$log,
               "log10",
               "identity")
    })
    
    y_labels = reactive({
        ifelse(input$percap=="value_per_cap",
               scales::label_percent(),
               scales::label_comma())
    })
    
    smoothing_factor = reactive({input$smoothingfactor/100})
    
    plot_type = reactive({
        ifelse(input$smooth,
               expression(stat_smooth(se=FALSE, span = smoothing_factor())),
               expression(geom_line(size=1)))
    })
    
    palette_used = reactive({ifelse(length(input$countries)<=8, "primary2", "secondary")})
    
    output$chooseplot = renderPlot({
        covid_data[country_region %in% input$countries & date %between% input$date_range & data == input$plotvariable] %>% 
            ggplot(aes(date, get(input$percap), colour = country_region)) +
            eval(plot_type()) +
            theme_tufte(base_size = 18) +
            geom_rangeframe(colour="grey20") +
            scale_y_continuous(breaks = scales::breaks_extended(), labels = y_labels(), trans = y_scale()) +
            scale_x_date(breaks = scales::breaks_width("1 week", 2),
                         labels = scales::label_date_short()) +
            xlab("Date") +
            ylab("Value") +
            # scale_colour_dtf(palette_used(), name="Country", labels = waiver()) +
            scale_colour_hue(name="Country") +
            theme(legend.position = c(0,1),
                  legend.direction = "vertical",
                  legend.justification = c(0,1))
        
    })
    
    output$growthplot = renderPlot({
        cov_growth %>% 
            ggplot(aes(cagr)) +
            geom_density(fill='grey20', alpha = 0.5) +
            geom_vline(xintercept = cov_growth[country_region=="Victoria",cagr]) +
            theme_tufte(base_size = 18) +
            geom_rangeframe() +
            scale_x_continuous(name = "CAGR", breaks = scales::breaks_extended(), labels = scales::label_percent(0.1)) +
            ylab("Density")
    })
    
    forecast_dt = reactive({tibble(country_region="Victoria",
                                   data="confirmed",
                                   date=seq(from=first_forecast, by="day", length.out=input$forecastlength),
                                   low_growth = 1+growth_quartiles[2],
                                   med_growth = 1+growth_quartiles[3],
                                   high_growth = 1+growth_quartiles[4],
                                   const_growth = 1+cov_growth[country_region=="Victoria",cagr]) %>% 
            mutate(low_growth=cumprod(low_growth),
                   med_growth=cumprod(med_growth),
                   high_growth=cumprod(high_growth),
                   const_growth=cumprod(const_growth))})
    
    vic_cov_forecast = reactive({rbindlist(list(vic_covid_data[data=="confirmed"], forecast_dt()), idcol=TRUE, fill=TRUE) %>% 
            fill(value, population) %>% 
            mutate(low_forecast = ifelse(.id==2, value*low_growth, NA),
                   med_forecast = ifelse(.id==2, value*med_growth, NA),
                   high_forecast = ifelse(.id==2, value*high_growth, NA),
                   const_forecast = ifelse(.id==2, value*const_growth, NA),
                   low_forecast_per_cap = low_forecast/population,
                   med_forecast_per_cap = med_forecast/population,
                   high_forecast_per_cap = high_forecast/population,
                   const_forecast_per_cap = const_forecast/population)})
    
    output$forecast = renderPlot({
        vic_cov_forecast() %>% 
            filter(date>=(max(date)-days(30))) %>% 
            ggplot() +
            theme_tufte(base_size = 18) +
            geom_rangeframe(aes(date, fcoalesce(value_per_cap,med_forecast_per_cap)), colour="grey20") +
            geom_line(aes(date, value_per_cap)) +
            geom_line(aes(date, low_forecast_per_cap), linetype="dashed") +
            geom_ribbon(aes(date, ymin=const_forecast_per_cap, ymax=med_forecast_per_cap),
                        alpha=0.4, fill="royalblue") +
            scale_y_continuous(breaks = scales::breaks_extended(), labels = scales::label_percent()) +
            scale_x_date(breaks = scales::breaks_width("1 week", 2),
                         labels = scales::label_date_short()) +
            xlab("Date") +
            ylab("Percent of population")
    })
    
}

shinyApp(ui, server)