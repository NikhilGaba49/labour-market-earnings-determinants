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

# Sequential Hypothesis Testing for Earnings-School
# Create non-linear terms for polynomial regression
mydata$schoolcubic = mydata$school * mydata$school * mydata$school
mydata$schoolquad = mydata$school * mydata$school

# Cubic earnings-school relationship
reg1 = lm(earnings ~ schoolcubic + schoolquad + school, data=mydata)
cov1=vcovHC(reg1, type = "HC1")  #Provides us with the heteroskedastic variance  
se1=sqrt(diag(cov1)) #Provides us with the heteroskedastic standard errors

# Quadratic earnings-school relationship
reg2 = lm(earnings ~ schoolquad + school, data=mydata)
cov2=vcovHC(reg2, type = "HC1")    
se2=sqrt(diag(cov2))

# Linear earnings-school relationship
reg3 = lm(earnings ~ school, data=mydata)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))

# Regression output table for reg1, reg2 and reg3, with standard errors included 
stargazer(reg1,reg2,reg3, type="text",
          se=list(se1,se2,se3), #with all their corresponding pre-defined standard errors which account for heteroskedastic errors.
          dep.var.labels=c("Annual Earnings"), # dependent variable name (i.e. Annual Earnings)
          covariate.labels=
            c("Years of Schooling Cubed",
              "Years of Schooling Squared",
              "Years of Schooling",
              "Constant"), #Names/labels of the coefficients
          out="reg_output_school.txt")   # Output results to your director in a text file

# Obtain t-statistic on schoolcubic, schoolquad, and school respectively 
coeftest(reg1, vcov = vcovHC(reg1, "HC1")) #Assuming heteroskedasticity
coeftest(reg2, vcov = vcovHC(reg2, "HC1")) 
coeftest(reg3, vcov = vcovHC(reg3, "HC1")) 