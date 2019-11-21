library(data.table) #load data
library(tidyverse) #to manipulate data
library(psych) #summarizing data
library(stringr) #manipulate strings
library(ggplot2) #plot data points


#set working directory
setwd("C:/Users/admin/Documents/Elder_Research")

#upload data
autism_data_or <- fread("Autism-Adult-Data (2).csv", header = TRUE)

#remove questions A1 through A10
autism_data <- autism_data_or[, -c(1:10)]

#inspect data
str(autism_data)
summary(autism_data)

unique(autism_data$Ethnicity)
unique(autism_data$Relation)
unique(autism_data$Country_of_res)
unique(autism_data$Age_desc)

#remove single quotes
autism_data$Ethnicity <- gsub("'", "", autism_data$Ethnicity)
autism_data$Relation <- gsub("'", "", autism_data$Relation)
autism_data$Country_of_res <- gsub("'", "", autism_data$Country_of_res)
autism_data$Age_desc <- gsub("'", "", autism_data$Age_desc)

#change ? to unknowns
autism_data$Ethnicity[autism_data$Ethnicity == '?'] <- "Unknown"
autism_data$Ethnicity[autism_data$Ethnicity == 'others'] <- "Others"
autism_data$Relation[autism_data$Relation == '?'] <- "Unknown"
autism_data$Age[autism_data$Age == '?'] <- NA


#added turkish to middle eastern category
#combined latino to hispanic
#after further inspection noticed that middle eastern label had an extra space
autism_data$Ethnicity[autism_data$Ethnicity == 'Latino'] <- "Hispanic"
autism_data$Ethnicity[autism_data$Ethnicity == 'Turkish'] <- "Middle Eastern"
autism_data$Ethnicity[autism_data$Ethnicity == 'Middle Eastern '] <- "Middle Eastern" #removed extra space


#age category
#breaking down to under 18, 18 to 35, 36 to 55, 55+
autism_data[Age <= 18, Age_Group := "Under 18"]
autism_data[Age >18 & Age <36, Age_Group := "18 to 35"]
autism_data[Age >35 & Age <56, Age_Group := "36 to 55"]
autism_data[Age >55, Age_Group := "55+"]

autism_data$Age_Group <- factor(autism_data$Age_Group, 
                                levels = c("Under 18", "18 to 35", "36 to 55", "55+"))

#Remove Age desc colum
colnames(autism_data)
autism_data <- autism_data[, -9]

#summarize data by ASD
describe.by(autism_data, group = 'ASD')

#cross tabs
table(autism_data$ASD)
prop.table(table(autism_data$ASD))

race_table <- xtabs(~ Ethnicity + ASD, data = autism_data )#ASD by Ethnicity
prop.table(race_table,2)

gender_table <-xtabs(~ Gender+ ASD, data = autism_data ) #ASD by Gender
prop.table(gender_table, 2)

age_table <- xtabs( ~ Age_Group + ASD, data = autism_data) #ASD by Age Group
prop.table(age_table, 2)


#Visualize data
ggplot(autism_data, aes(ASD)) +
  geom_bar()

ggplot(autism_data, aes(ASD, fill = Gender)) +
  geom_bar()

ggplot(autism_data, aes(Age_Group, fill = ASD)) +
  geom_bar()

ggplot(autism_data, aes(Ethnicity)) +
  geom_bar()

ggplot(autism_data, aes(Relation)) +
  geom_bar()




