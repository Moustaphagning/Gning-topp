


##Extraction des donnees

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#setwd("C:/Users/gning/Documents/projet expertise/GNING/GNING_TOP")
data<-read_csv2("data_ulule_2019.csv")



#Exclure du périmétre les campagnes annulées
##Se restreindre au périmétre des 8 pays ayant le plus de campagnes au total



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



library(shiny)
library(plotly)
library(shinydashboard)

ui <- fluidPage(
  titlePanel("Projet shiny"),
  sidebarPanel(
    radioButtons("suivi", "Suivie", choices = c("Nombre-total-de-campagnes-cr?es"="count", "Proportion-de-campagnes-financ?es"="prop", "Montant-moyen-des-campagnes-financ?es"="moyenne")),
    selectInput("categorie", "categorie:", unique(data_final$category)),
  ),
  mainPanel(plotlyOutput("plot"))
)

server <- function(input, output) {
  
  
  output$plot <- renderPlotly({
    suivi<-input$suivi
    categorie<-input$categorie
    data = data_final[data_final$category==input$categorie, ]
    plot_ly(data, x = ~annee, y = data[,suivi], type = 'scatter',  mode = 'lines')
  })
  
}


shinyApp(ui, server)
