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

# Sequential Hypothesis Testing for Earnings-Exper
# Create non-linear terms for polynomial regression
mydata$expercubic = mydata$exper * mydata$exper * mydata$exper
mydata$experquad = mydata$exper * mydata$exper  

# Cubic earnings-exper relationship
reg4 = lm(earnings ~ expercubic + experquad + exper, data=mydata)
cov4=vcovHC(reg4, type = "HC1")    
se4=sqrt(diag(cov4))

# Quadratic earnings-exper relationship
reg5 = lm(earnings ~ experquad + exper, data=mydata)
cov5=vcovHC(reg5, type = "HC1")    
se5=sqrt(diag(cov5))

# Linear earnings-exper relationship
reg6 = lm(earnings ~ exper, data=mydata)
cov6=vcovHC(reg6, type = "HC1")    
se6=sqrt(diag(cov6))

# Regression output table for reg4, reg5 and reg6, with standard errors included 
stargazer(reg4,reg5,reg6, type="text",
          se=list(se4,se5,se6), #with all their corresponding pre-defined standard errors which account for heteroskedastic errors.
          dep.var.labels=c("Annual Earnings"), #Dependent variable name
          covariate.labels=
            c("Years of Experience Cubed",
              "Years of Experience Squared",
              "Years of Experience",
              "Constant"), #Names/labels of coefficients
          out="reg_output_exper.txt")   # Output results to your director in a text file

# Obtain t-statistic on expercubic, experquad, and exper respectively 
coeftest(reg4, vcov = vcovHC(reg4, "HC1")) #Assuming heteroskedasticity
coeftest(reg5, vcov = vcovHC(reg5, "HC1")) 
coeftest(reg6, vcov = vcovHC(reg6, "HC1")) 

# Running quadratic regression with controlled variables
#Quadratic earnings-school and earnings-exper relationship, holding rural, year, and industry dummies fixed
reg7 = lm(earnings ~ schoolquad + school + experquad + exper + rural + year + trad + con + bus + fin, data = mydata)
cov7=vcovHC(reg7, type = "HC1")    
se7=sqrt(diag(cov7))

#Regression output table for reg7, inclusive of standard errors
stargazer(reg7, type = "text", 
          se = list(se7), 
          dep.var.labels = c("Annual Earnings"), 
          covariate.labels = 
            c("Years of Schooling Squared", "Years of Schooling",
              "Years of Experience Squared", "Years of Experience",
              "Rural", "Year", "Tradesperson", "Construction", "Business", "Finance"),
          out = "reg_output_nonlinear.txt")