library(dplyr)
library(ggplot2)
library(ggmap)
library(XML)
library(rvest)

gdp_table <- read_html('https://en.wikipedia.org/wiki/List_of_IMF_ranked_countries_by_GDP')
gdp <- html_table(html_nodes(gdp_table, "table")[[3]], fill = T)

suicide_table <- read_html('https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate')
suicide <-html_table(html_nodes(suicide_table, "table")[[3]], fill = T)

data <- read.csv('C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/1주차/data1.csv')

ggplot(data=data, aes(x=log.GDP, y=Life.Expectancy )) +
  geom_point(aes(size=Suicide.rate, col=Region)) +
  theme(legend.position = "none") 


map <- map_data(map='world')
data$Country <- as.character(data$Country)
m <- left_join(data,map,by=c("Country"="region"))


ggplot(data=m,aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=as.numeric(Suicide.rate))) +
  scale_fill_gradient(low="red", high="black")