readme
================
Moustapha GNING && Mouhamad TOP
24/04/2020

## Projets portant sur l’évolution des campagnes de financement participatif du site Ulule

Dans un premier temps nous allons proceder au chargement des packages et
des données. Toutes les manipulations de données ont ete faite au sens
tydiverse

``` r
knitr::opts_chunk$set(echo = T, message=F, eval=T, warning = F)
```

``` r
setwd("~/projet expertise/GNING/GNING_TOP/Gning-topp")
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
data<-read_csv2("data_ulule_2019.csv")
```

``` r
data<-data[data$is_cancelled==FALSE,]
data <- data[data$country %in% c("FR","BE","IT","CA","ES","CH","DE","GB"),]


data_3<-data %>% 
  select(c(3,7,14))
data_4 <-data[,-c(3,7,14)]

taux_change <- 0.9

n<-nrow(data_3)

for (i in 1:n) {

  if(data_3$currency[i] != "EUR")
  {
    data_3$amount_raised[i] <- data_3$amount_raised[i] * taux_change
    data_3$goal[i] <- data_3$goal[i] * taux_change
    data_3$currency[i] <- "EUR"
  }
  
}

tmp <- cbind(data_4,data_3)

#changer le format des dates
tmp$date_end<-as.POSIXct(tmp$date_end)
tmp$date_start<-as.POSIXct(tmp$date_start)
tmp<- tmp %>%filter(tmp$date_end < ymd("2018-12-31"))
tmp["annee"] = year(tmp$date_start)
tmp['count'] = 1 

count = tmp %>% 
  filter(!is.na(tmp$category))%>% 
group_by(`annee`, `category`) %>% 
  summarise(count=  sum(count))

prop = tmp %>% 
  filter(!is.na(tmp$category))%>% 
group_by(`annee`, `category`) %>% 
  summarise(prop=  sum(`goal_raised`)/sum(count))

moyenne = tmp %>% 
  filter(tmp$goal_raised=="TRUE" & !is.na(tmp$category))%>% 
group_by(`annee`, `category`) %>% 
  summarise(moyenne=mean(`amount_raised`, na.rm = TRUE))

data_final<-merge(prop,moyenne, by=c("annee","category"), all=TRUE)
data_final <- merge(data_final,count,by=c("annee","category"), all=TRUE)
```

## L’évolution du nombre total de campagne financée par categorie de BD (bande dessinee)

``` r
    plot(data_final[data_final$category=="BD", "annee"], data_final[data_final$category=="BD", "count"],type = "l")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Evolution par annee de la moyenne des campagnes financées

``` r
  plot(data_final[data_final$category=="Musique", "annee"], data_final[data_final$category=="Musique", "moyenne"],type = "l")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Proportion des campagnes financées par annee

``` r
  plot(data_final[data_final$category=="BD", "annee"], data_final[data_final$category=="BD", "prop"],type = "l")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

\#Voici le lien pour notre application

MOUSTAPHA et TOP

<https://gningfata.shinyapps.io/GNING_TOP/>
