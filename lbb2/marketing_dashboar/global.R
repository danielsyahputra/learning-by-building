library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(sass)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(DT)
library(ggthemes)
library(readr)
library(glue)

# Data Preparation
marketing <- read.csv("marketing_data.csv")

marketing$Income <- parse_number(marketing$Income)
marketing$Dt_Customer <- mdy(marketing$Dt_Customer)

marketing$Education[marketing$Education == "Graduation"] <- "Undergraduate"
marketing$Education[marketing$Education == "2n Cycle"] <- "Master"

status <- c("YOLO", "Alone", "Absurd")
marketing$Marital_Status[marketing$Marital_Status %in% status] <- "Single"

marketing <- marketing %>% 
  mutate_if(is.character, as.factor)

average_income <- aggregate(Income ~ Education + Marital_Status, data=marketing, FUN=mean)

imputed_data <- left_join(marketing[is.na(marketing$Income),], 
                          average_income, 
                          by = c("Education", "Marital_Status"))

imputed_data$Income.x <- imputed_data$Income.y
imputed_data <- imputed_data %>% select(-Income.y)
colnames(imputed_data)[5] <- "Income"

marketing <- full_join(marketing,                             
                       imputed_data, 
                       by = c('ID',
                              'Year_Birth',
                              'Education',
                              'Marital_Status',
                              'Kidhome',
                              'Teenhome',
                              'Dt_Customer',
                              'Recency',
                              'MntWines',
                              'MntFruits',
                              'MntMeatProducts',
                              'MntFishProducts',
                              'MntSweetProducts',
                              'MntGoldProds',
                              'NumDealsPurchases',
                              'NumWebPurchases',
                              'NumCatalogPurchases',
                              'NumStorePurchases',
                              'NumWebVisitsMonth',
                              'AcceptedCmp3',
                              'AcceptedCmp4',
                              'AcceptedCmp5',
                              'AcceptedCmp1',
                              'AcceptedCmp2',
                              'Response',
                              'Complain',
                              'Country'))
marketing <- marketing  %>%                                  
  mutate(Income = coalesce(Income.x, Income.y)) %>%
  select(-c("Income.x", "Income.y"))