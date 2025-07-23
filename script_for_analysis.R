setwd("C:/Users/Nikhil Gaba/Documents/Internship/GitHub Repositories/labour-market-earnings-determinants")

#Read in the required dataset
mydata = read.csv("wages.csv")

#Load necessary libaries
library(stargazer)
library(ggplot2)
library(AER)

#Understand the nature of the data (columns, a few data rows, etc.)
names(mydata)
dim(mydata)
