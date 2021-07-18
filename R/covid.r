library(tidyverse)
library(rvest)
library(Dict)
library(collections)
library(padr)

reload <- FALSE

if (reload){
  if (file.exists("current_state.txt")){
    current_state <- read_file("current_state.txt")
    #apparently saving and loading the file adds an extra line break:
    current_state <- substr(current_state,1,nchar(current_state)-1)
  }else{
    current_state <- ""
  }


  website <- read_html("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0")%>%html_node(".restBody")%>%html_text
  if (current_state != website){
    print("data set has changed and will be reloaded")
    covid19 <- read.csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data")
    write.csv(covid19, file = "RKI_COVID19.csv")
    current_state <- website
    write(current_state, file="current_state.txt")
  }

}else{
  print("loading local dataset")
  covid19 <- read.csv("RKI_COVID19.csv")
}


#' Returns a time series for a given grouping(Regency,Bundesland,Altersgruppe) in a given category.
#' @param pRegency A string
#' @param category A string
#' @example
#' mav(7,time_series)
get_timeseries <- function(grouping,category){
  group = unique(covid19 %>% grouping)
  result <- dict()
  function_name = paste("get_",grouping,sep="")
  for (item in group){
    result[[item]] = get(function_name)(item,category)
  }
  print(result)
  return(result)
}

#' Returns the values over time for a given Bundesland in a given category.
#' @param pRegency A string
#' @param category A string
#' @example
#' mav(7,time_series)
get_Bundesland <- function(pLand,category="cases"){
  result <- arrange(covid19 %>%
                             group_by(Refdatum,Bundesland) %>%
                             mutate(date=as.Date(Refdatum),cases=sum(AnzahlFall),deaths=sum(AnzahlTodesfall))%>%
                             filter(Bundesland == pLand),date)
  result <- unique(result %>% ungroup() %>%
    select(date,as.name(category)))
  return(pad(as_tibble(result)))
}

#' Returns the values over time for a given age group in a given category.
#' @param pRegency A string
#' @param category A string
#' @example
#' mav(7,time_series)
get_Altersgruppe <- function(group,category="cases"){
  result <- arrange(covid19 %>%
                             group_by(Refdatum,Altersgruppe) %>%
                             mutate(date=as.Date(Refdatum),cases=sum(AnzahlFall),deaths=sum(AnzahlTodesfall))%>%
                             filter(Altersgruppe == group),date)
  result <- unique(result %>% ungroup() %>%
    select(date,as.name(category)))
  return(pad(as_tibble(result)))
}

#' Returns the values over time for a given regency in a given category.
#' @param pRegency A string
#' @param category A string
#' @example
#' mav(7,time_series)
get_Landkreis <- function(pRegency,category="cases"){
  result <- arrange(covid19 %>%
                             group_by(Refdatum,Landkreis) %>%
                             mutate(date=as.Date(Refdatum),cases=sum(AnzahlFall),deaths=sum(AnzahlTodesfall))%>%
                             filter(Landkreis == pRegency),date)
  result <- unique(result %>% ungroup() %>%
    select(date,as.name(category)))
  return(pad(as_tibble(result)))
}


#' Calculates the moving average of data x only using the n previous values.
#' @param n A number
#' @param x A time series
#' @example
#' mav(7,time_series)
mav <- function(n,x){stats::filter(x,rep(1/n,n), sides = 1)}

#' Plots the moving average of the cases in a given Bundesland.
#' @param pLand A String
#' @param window_size A number
#' @example
#' plot_cases_mav_land("Berlin",window_size=10)
plot_cases_mav_land <- function(pLand,window_size=7){
  mav_cases <- tibble(cases = mav(window_size,land_cases[[pLand]]$cases),date=as.Date(unlist(land_cases[[pLand]]$date)))
  plot1 <- ggplot() +
    geom_point(data = (land_cases[[pLand]]), aes(x=date, y=cases,colour="orange")) +
    geom_line(data = tail(mav_cases,-window_size-1) ,aes(x=date, y=cases,colour="light_blue"))+
    ggtitle("gleitender Durchschnitt der Fälle im Bundesland", pLand)
  plot1
}

#' Plots the moving average of the deaths in a given Bundesland.
#' @param pLand A String
#' @param window_size A number
#' @example
#' plot_deaths_mav_land("Berlin",window_size=10)
plot_deaths_mav_land <- function(pLand,window_size=7){
  mav_deaths <- tibble(deaths = mav(window_size,land_deaths[[pLand]]$deaths),date=as.Date(unlist(land_deaths[[pLand]]$date)))
  plot1 <- ggplot() +
    geom_point(data = (land_deaths[[pLand]]), aes(x=date, y=deaths,colour="orange")) +
    geom_line(data = tail(mav_deaths,-window_size-1) ,aes(x=date, y=deaths,colour="light_blue"))+
    ggtitle("gleitender Durchschnitt der Todesfälle im Bundesland", pLand)
  plot1
}

#' Plots the moving average of the cases in a given age group.
#' @param pLand A String
#' @param window_size A number
#' @example
#' plot_cases_mav_land("Berlin",window_size=10)
plot_cases_mav_agegroup <- function(pLand,window_size=7){
  mav_cases <- tibble(cases = mav(window_size,group_cases[[pLand]]$cases),date=as.Date(unlist(group_cases[[pLand]]$date)))
  plot1 <- ggplot() +
    geom_point(data = (group_cases[[pLand]]), aes(x=date, y=cases,colour="orange")) +
    geom_line(data = tail(mav_cases,-window_size-1) ,aes(x=date, y=cases,colour="light_blue"))+
    ggtitle("gleitender Durchschnitt der Todesfälle im Bundesland", pLand)
  plot1
}

#' Plots the moving average of the deaths in a given age group.
#' @param pLand A String
#' @param window_size A number
#' @example
#' plot_deaths_mav_agegroup("Berlin",window_size=10)
plot_deaths_mav_agegroup <- function(pLand,window_size=7){
  mav_deaths <- tibble(deaths = mav(window_size,group_deaths[[pLand]]$deaths),date=as.Date(unlist(group_deaths[[pLand]]$date)))
  plot1 <- ggplot() +
    geom_point(data = (group_deaths[[pLand]]), aes(x=date, y=deaths,colour="orange")) +
    geom_line(data = tail(mav_cases,-window_size-1) ,aes(x=date, y=deaths,colour="light_blue"))+
    ggtitle("gleitender Durchschnitt der Todesfälle im Bundesland", pLand)
  plot1
}


#' Plots the moving average of the cases in a given regency
#' @param pLand A String
#' @param window_size A number
#' @example
#' plot_cases_mav_land("Berlin",window_size=10)
plot_cases_mav_regency <- function(pLand,window_size=7){
  mav_cases <- tibble(cases = mav(window_size,regency_cases[[pLand]]$cases),date=as.Date(unlist(regency_cases[[pLand]]$date)))
  plot1 <- ggplot() +
    geom_point(data = (regency_cases[[pLand]]), aes(x=date, y=cases,colour="orange")) +
    geom_line(data = tail(mav_cases,-window_size-1) ,aes(x=date, y=cases,colour="light_blue"))+
    ggtitle("gleitender Durchschnitt der Todesfälle im Bundesland", pLand)
  plot1
}

#' Plots the moving average of the deaths in a given regency.
#' @param pLand A String
#' @param window_size A number
#' @example
#' plot_deaths_mav_agegroup("Berlin",window_size=10)
plot_deaths_mav_regency <- function(pLand,window_size=7){
  mav_deaths <- tibble(deaths = mav(window_size,regency_deaths[[pLand]]$deaths),date=as.Date(unlist(regency_deaths[[pLand]]$date)))
  plot1 <- ggplot() +
    geom_point(data = (regency_deaths[[pLand]]), aes(x=date, y=deaths,colour="orange")) +
    geom_line(data = tail(mav_cases,-window_size-1) ,aes(x=date, y=deaths,colour="light_blue"))+
    ggtitle("gleitender Durchschnitt der Todesfälle im Bundesland", pLand)
  plot1
}

#' Returns maximal correlation between two time series and the lag at which it is reached.
#' @param time_series_0 A list
#' @param time_series_1 A list
#' @example
#' get_ccf(get_Bundesland("Berlin"),get_Bundesland("Mecklenburg-Vorpommern"))
get_ccf <- function(time_series_0,time_series_1){
  results <- ccf(time_series_0$cases,time_series_1$cases,type="correlation",pl=TRUE)
  print(results)
  max_cor <- results[which.max(head(unlist(results,use.names=TRUE),49))-25]
  return(c(max_cor$acf,max_cor$lag))
}

#' Returns the fallsterblichkeit (deaths/cases) for a given item and grouping.
#' @param item A String
#' @param grouping A string
#' @example
#' get_fallsterblichkeit("Hamburg","Bundesland")
get_fallsterblickeit <- function(item,grouping){
  function_name <- paste("get_",grouping,sep="")
  deaths <- pad(as_tibble(get(function_name)(item,"deaths")))
  cases <- pad(as_tibble(get(function_name)(item,"cases")))
  result <- data.frame(date=cases[1],test=c(deaths[-1]/cases[-1]))
  return(result)
}

get_ccf(get_Bundesland("Berlin"),get_Bundesland("Mecklenburg-Vorpommern"))

get_fallsterblickeit("Hamburg","Bundesland")

if (reload && file.exists("land_cases.rdata") && file.exists("land_deaths.rdata") && file.exists("group_cases.rdata") && file.exists("group_deaths.rdata") && file.exists("regency_cases.rdata") && file.exists("regency_deaths.rdata")){
  land_cases = load("land_cases.rdata")
  land_deaths = load("land_deaths.rdata")
  group_cases = load("group_cases.rdata")
  group_deaths = load("group_deaths.rdata")
  regency_cases = load("regency_cases.rdata")
  regency_deaths = load("regency_deaths.rdata")
}else{
  land_cases = get_timeseries("Bundesland","cases")
  save(land_cases,"land_cases.rdata")
  land_deaths = get_timeseries("Bundesland","deaths")
  save(land_deaths,"land_deaths.rdata")
  group_cases = get_timeseries("Altersgruppe","cases")
  save(group_cases,"group_cases.rdata")
  group_deaths = get_timeseries("Altersgruppe","deaths")
  save(group_deaths,"group_deaths.rdata")
  regency_cases = get_timeseries("Landkreis","cases")
  save(regency_cases,"regency_cases.rdata")
  regency_deaths =get_timeseries("Landkreis","deaths")
  save(regency_deaths,"regency_deaths.rdata")
}

plot_cases_mav_land("Hamburg")
plot_deaths_mav_land("Hamburg")
plot_cases_mav_agegroup("Hamburg")
plot_deaths_mav_agegroup("Hamburg")
plot_cases_mav_regency("Hamburg")
plot_deaths_mav_regency("Hamburg")


get_covid_per_month <- function(){
  sum_anzahlFaelle <- summarize(covid19,sum = sum(AnzahlFall))$sum
  sum_anzahlEDB <- summarize(covid19,sum = sum(AnzahlFall[IstErkrankungsbeginn==1]))$sum
  AnteilEDB <- sum_anzahlEDB / sum_anzahlFaelle * 100
  covid19$MeldedatumPerMonth <- format(as.Date(covid19$Meldedatum), "%Y-%m")
  covid19PerMonth <- covid19 %>% select(MeldedatumPerMonth,IstErkrankungsbeginn, AnzahlFall)  %>%
    group_by(MeldedatumPerMonth) %>%
    summarize(sumPerMonthTotal = sum(AnzahlFall),
              sumPerMonthBekannt = sum(AnzahlFall[IstErkrankungsbeginn==1]))
  covid19PerMonth <- covid19PerMonth %>% mutate(AnteilPerMonth = sumPerMonthBekannt / sumPerMonthTotal * 100) %>%
    mutate(TotalAnteilPerMonth = sumPerMonthTotal / sum_anzahlFaelle * 100) %>%
    mutate(TotalBekanntAnteilPerMonth = sumPerMonthBekannt / sum_anzahlEDB * 100)
  return(covid19PerMonth)
}

print(get_covid_per_month())
