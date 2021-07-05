library(dplyr)
library(ggplot2)
covid19 <- read.csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data")
cases <- covid19 %>% 
  group_by(Meldedatum,Bundesland) %>% 
  mutate(date=as.Date(Meldedatum),cases=sum(AnzahlFall),deaths=sum(AnzahlTodesfall))

plot_new_cases <- function(land){
  cases %>% filter(Bundesland == land) %>%
    ggplot(aes(x=date,y=cases)) +
    geom_line()
}

plot_new_deaths <- function(land){
  cases %>% filter(Bundesland == land) %>%
    ggplot(aes(x=date,y=deaths)) +
    geom_line()
}

mav <- function(x){stats::filter(x,rep(1/7,7), sides = 1)}


plot_new_cases("Hamburg")
#plot_new_cases_mav("Hamburg")
plot_new_deaths("Hamburg")
