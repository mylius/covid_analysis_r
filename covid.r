library(dplyr)
library(ggplot2)
library(zoo)
covid19 <- read.csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data")

cases_Bundesland <- covid19 %>% 
  group_by(Meldedatum,Bundesland) %>% 
  mutate(date=as.Date(Meldedatum),cases=sum(AnzahlFall),deaths=sum(AnzahlTodesfall))

cases_age <- covid19 %>% 
  group_by(Meldedatum,Altersgruppe) %>% 
  mutate(date=as.Date(Meldedatum),cases=sum(AnzahlFall),deaths=sum(AnzahlTodesfall))

cases_Landkreis <- covid19 %>% 
  group_by(Meldedatum,Landkreis) %>% 
  mutate(date=as.Date(Meldedatum),cases=sum(AnzahlFall),deaths=sum(AnzahlTodesfall))

plot_new_cases_land <- function(pLand){
  cases_Bundesland %>% filter(Bundesland == pLand) %>%
    ggplot(aes(x=date,y=cases,main="cases")) +
    ggtitle("Fälle im Bundesland", pLand) +
    geom_line()
}

plot_new_deaths_land <- function(pLand){
  cases_Bundesland %>% filter(Bundesland == pLand) %>%
    ggplot(aes(x=date,y=deaths)) +
    ggtitle("Todesfälle im Bundesland", pLand) +
    geom_line()
}

plot_new_cases_age_group <- function(group){
  cases_age %>% filter(Altersgruppe == group) %>%
    ggplot(aes(x=date,y=cases)) +
    ggtitle("Fälle in der Altersgruppe", group) +
    geom_line()
}

plot_new_deaths_age_group <- function(group){
  cases_age %>% filter(Altersgruppe == group) %>%
    ggplot(aes(x=date,y=deaths)) +
    ggtitle("Todesfälle in der Altersgruppe", group) +
    geom_line()
}

plot_new_cases_regency <- function(pRegency){
  cases_age %>% filter(Landkreis == pRegency) %>%
    ggplot(aes(x=date,y=cases)) +
    ggtitle("Fälle im Landkreis", pRegency) +
    geom_line()
}

plot_new_deaths_regency <- function(pRegency){
  cases_age %>% filter(Landkreis == pRegency) %>%
    ggplot(aes(x=date,y=deaths)) +
    ggtitle("Todesfälle im Landkreis", pRegency) +
    geom_line()
}

mav <- function(n,x){stats::filter(x,rep(1/n,n), sides = 1)}

plot_cases_mav_land <- function(pLand,window_size=7){
  cases_temp <- unique(arrange((cases_Bundesland %>%filter(Bundesland==pLand) %>% select(date,cases)),date))%>%ungroup()
  mav_cases <- tibble(cases = mav(window_size,cases_temp%>%select(cases)),date=as.Date(unlist(cases_temp%>%select(date))))
  plot1 <- ggplot() + 
    geom_point(data = (cases_Bundesland %>% filter(Bundesland == pLand)), aes(x=date, y=cases,colour="orange")) +
    geom_line(data = tail(mav_cases,-window_size-1) ,aes(x=date, y=cases,colour="light_blue"))+
    ggtitle("gleitender Durchschnitt der Fälle im Bundesland", pLand)
  plot1
}

plot_new_cases_land("Hamburg")
#plot_new_cases_mav("Hamburg")
plot_new_deaths_land("Hamburg")
plot_new_cases_age_group("A60-A79")
plot_new_deaths_age_group("A60-A79")
plot_new_cases_regency("LK Heilbronn")
plot_new_deaths_regency("LK Heilbronn")
plot_cases_mav_land("Hamburg")
