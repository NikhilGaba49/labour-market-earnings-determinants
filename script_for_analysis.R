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

# Visualising the relationships between Earnings with Years of Schooling and Work Experience
# Construct the variable "Earnings"
mydata$earnings = mydata$wage * mydata$hours/1000

# Construct the scatter plots appropriately for each relationship titled and axes labelled, output in jpeg (image format)
pdf("earnings_education_qm.pdf")
ggplot(data=mydata, mapping = aes(x=school, y=earnings) #scatter plot with earnings as the dependent variable and school as the independent variable
) + geom_point() + geom_smooth(formula = y ~ poly(x, 2)) + labs(
  title="Relationship between Earnings ($1000s) and Education (in Years)", #title for scatter plot
  x="Education (In Years)", #label for x-axis
  y="Earnings (in $1000s)") #label for y-axis
dev.off()

pdf("earnings_experience_qm.pdf")
ggplot(data=mydata, mapping = aes(x=exper, y=earnings) #scatter plot with earnings as the dependent variable and exper as the independent variable
) + geom_point() + geom_smooth(formula = y ~ poly(x, 2)) + labs(
  title="Relationship between Earnings (in $1000s) and Workforce Experience (in Years)", #title for scatter plot
  x="Workforce Experience (in Years)", #label for x-axis
  y="Earnings (in $1000s)") #label for y-axis
dev.off()