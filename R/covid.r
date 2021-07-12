library(tidyverse)
library(rvest)
library(dict)
library(collections)

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
  return(result)
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
  return(result)
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
  return(result)
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


land_cases = get_timeseries("Bundesland","cases")
land_deaths = get_timeseries("Bundesland","deaths")
group_cases = get_timeseries("Altersgruppe","cases")
group_deaths = get_timeseries("Altersgruppe","deaths")
regency_cases = get_timeseries("Landkreis","cases")
regency_deaths =get_timeseries("Landkreis","deaths")


plot_cases_mav_land("Hamburg")
plot_deaths_mav_land("Hamburg")
plot_cases_mav_agegroup("Hamburg")
plot_deaths_mav_agegroup("Hamburg")
plot_cases_mav_regency("Hamburg")
plot_deaths_mav_regency("Hamburg")
