### Nigeria's Inflation Project

##Installing packages needed for project

#tidyverse for data importation & wrangling
install.packages("tidyverse")
library(tidyverse)

#ggplot for data visualisation
install.packages("ggplot2")
library(ggplot2)

#lubridate for date manipulation
install.packages("lubridate")
library(lubridate)

#dplyr for grammar of data manipulation
install.packages("dplyr")
library(dplyr)

#lmtest for linear regression
install.packages("lmtest")
library(lmtest)

#car for influence analysis and regression model valuation
install.packages("car")
library(car)

#Displaying working directory
getwd()

#Setting working directory
setwd("/Users/Wale/Downloads/Projects/Own Path/Nigeria Inflation")

#Importing data (csv files)
Inflation_rate <- read.csv("Inflation.csv")
Interest_rate <- read.csv("Interest_Rate.csv")
Fx_supply <- read.csv("Fx_Supply.csv")
Food_inflation <- read.csv("Food_Inflation.csv")
Nigeria_inflation_data <- read.csv("Nigeria_inflation_data.csv")

#Inspecting imported data
View(Inflation_rate)
View(Interest_rate)
View(Fx_supply)
View(Food_inflation)
View(Nigeria_inflation_data)

str(Inflation_rate)
str(Interest_rate)
str(Fx_supply)
str(Food_inflation)

##Removing irrelevant columns
#Inflation rate table
Inflation_rate2 <-  Inflation_rate %>% 
  select(-c(Date))

#Interest Rate
Interest_rate2 <- Interest_rate %>% 
  select(-c(InterBankCallRate, MRR))

#Fx Supply
Fx_supply2 <- Fx_supply %>% 
  select(-c(Date, Level.1.Category, Unit))

#Food Inflation
Food_inflation2 <- Food_inflation %>% 
  select(-c(Date))

View(Inflation_rate2)
View(Interest_rate2)
View(Fx_supply2)
View(Food_inflation2)

#Renaming month description from numeric to month names
month_mapping <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

Interest_rate2$Month <- month_mapping[Interest_rate2$Month]
View(Interest_rate2)

#Converting data type for month comlumn 
Interest_rate2 <- mutate(Interest_rate2, Month = as.character(Month))
str(Interest_rate2)

#Renaming MPR Column
Interest_rate2 <- rename(Interest_rate2,
                         Interest_rate = "MPR")


#Merging the dataframes 
View(Inflation_rate2)
View(Interest_rate2)
View(Fx_supply2)
View(Food_inflation2)

str(Inflation_rate2)
str(Interest_rate2)
str(Fx_supply2)
str(Food_inflation2)

#Left join. Left join keeps all rows from original data frames and fills in missing values
Nigeria_data <- Inflation_rate2 %>% 
  left_join(Interest_rate2, by = c("Year", "Month")) %>% 
  left_join(Fx_supply2, by = c("Year", "Month")) %>% 
  left_join(Food_inflation2, by = c("Year", "Month"))


View(Nigeria_data)
summary(Nigeria_data)
str(Nigeria_data)
rm(Nigeria_data2)
View(Nigeria_inflation_data)

#Replacing NA with a value (0)

Nigeria_inflation_data$Fx_Supply..Billions. <- replace(Nigeria_inflation_data$Fx_Supply..Billions., is.na(Nigeria_inflation_data$Fx_Supply..Billions.), 0.00)

Nigeria_inflation_data$Interest_Rate <- replace(Nigeria_inflation_data$Interest_Rate, is.na(Nigeria_inflation_data$Interest_Rate), 0.00)

#Getting average for all indicators
Average_Nigeria_Data <- Nigeria_inflation_data %>% 
  group_by(Year) %>% 
  summarize(Average_inflation = mean(Inflation_Rate),
            Average_interest = mean(Interest_Rate),
            Average_fxsupply = mean(Fx_Supply..Billions.),
            Average_foodinflation = mean(Food_Inflation))

View(Average_Nigeria_Data)


#Visualization

# Time Series Plot of Inflation Rate
ggplot(Nigeria_inflation_data, aes(x = Year, y = Inflation_Rate)) +
  geom_line() +
  labs(title = "Inflation Rate Over the Years",
       x = "Year",
       y = "Inflation Rate")

# Scatter Plot between Inflation Rate and Interest Rate
ggplot(Nigeria_inflation_data, aes(x = Inflation_Rate, y = Interest_Rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Inflation Rate vs. Interest Rate",
       x = "Inflation Rate",
       y = "Interest Rate")

ggplot(Nigeria_inflation_data, aes(x = Year)) +
  geom_line(aes(y = Inflation_Rate, color = "Inflation Rate")) +
  geom_line(aes(y = Interest_Rate, color = "Interest Rate")) +
  scale_color_manual(values = c("Inflation Rate" = "red", "Interest Rate" = "blue")) +
  labs(title = "Comparison of Inflation Rate and Interest Rate Over the Years",
       x = "Year",
       y = "Rate",
       color = "Variable") +
  theme_minimal()

ggplot(data = Average_Nigeria_Data, aes(x = Year)) + 
  geom_line(aes(y = Average_inflation, color = "Inflation Rate"), size = 1.5) +
  geom_line(aes(y = Average_interest, color = "Interest Rate"), size = 1.5) +
  labs(title = "Inflation and Interest Rate Trend",
       x = "Year",
       y = "Value") + 
  scale_color_manual(values = c("Inflation Rate" = "red", "Interest Rate" = "blue")) +
  theme_minimal()

