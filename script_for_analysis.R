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

# Testing of non-linear relationship between Earnings and School and/or Exper
# Overall regression F-statistic for the quadratic regression
linearHypothesis(reg7, 
                 c("schoolquad = 0", "experquad = 0"),
                 vcov = vcovHC(reg7, "HC1")) #Assuming heteroskedasticity

# Computing the Partial Effects on Earnings
# Construct dataframe for predicting earnings for school=12 and exper=5
newdata1=data.frame(schoolquad=144,school=12,experquad=25,exper=5,rural=1,year=2025, trad=0, con=0, bus=0, fin=0)
# Construct dataframe for predicting earnings for school=13 and exper=5
newdata2=data.frame(schoolquad=169,school=13,experquad=25,exper=5,rural=1,year=2025, trad=0, con=0, bus=0, fin=0)
# Construct dataframe for predicting earnings for school=12 and exper=6
newdata3=data.frame(schoolquad=144,school=12,experquad=36,exper=6,rural=1,year=2025, trad=0, con=0, bus=0, fin=0)

# Compute the predicted value for earnings for school=12 and exper=5 
ahe1=predict(reg7, newdata=newdata1)
# Compute the predicted value for earnings for school=13 and exper=5 
ahe2=predict(reg7, newdata=newdata2)
# Compute the predicted value for earnings for school=12 and exper=6 
ahe3=predict(reg7, newdata=newdata3)

# Compute partial effect on earnings changing from school=12 to school=13
dahe_school = ahe2-ahe1
# Compute partial effect on earnings changing from exper=5 to exper=6
dahe_exper = ahe3-ahe1

# F-statistic for joint-test that dahe_school=0
Ftest_school=linearHypothesis(reg7,c("school+25*schoolquad=0"),vcov = vcovHC(reg7, "HC1"))
Fstat_school=Ftest_school[2,3]

# F-statistic for joint-test that dahe_exper=0
Ftest_exper=linearHypothesis(reg7,c("exper+11*experquad=0"),vcov = vcovHC(reg7, "HC1"))
Fstat_exper=Ftest_exper[2,3]

# Compute standard errors of partial effects dahe_school and dahe_exper respectively
se_dahe_school=abs(dahe_school)/sqrt(Fstat_school)
se_dahe_exper=abs(dahe_exper)/sqrt(Fstat_exper)

# 95% CI for partial effect dahe_school
dahe_school_ci95L=dahe_school-se_dahe_school*1.96
dahe_school_ci95H=dahe_school+se_dahe_school*1.96

# 95% CI for partial effect dahe_exper
dahe_exper_ci95L=dahe_exper-se_dahe_exper*1.96
dahe_exper_ci95H=dahe_exper+se_dahe_exper*1.96

# Output results for 1 more year of schooling
sprintf("partial effect of an additional year of schooling: %f", dahe_school)
sprintf("SE of partial effect of an additional year of schooling: %f", se_dahe_school)
sprintf("95 CI lower bound for partial effect of additional year of schooling: %f", dahe_school_ci95L)
sprintf("95 CI upper bound for partial effect of additional year of schooling: %f", dahe_school_ci95H)

# Output results for 1 more year of work experience
sprintf("partial effect of additional year of work experience: %f", dahe_exper)
sprintf("SE of partial effect of additional year of work experience: %f", se_dahe_exper)
sprintf("95 CI lower bound for partial effect of additional year of work experience: %f", dahe_exper_ci95L)
sprintf("95 CI upper bound for partial effect of additional year of work experience: %f", dahe_exper_ci95H)

# Estimating a linear regression (not a non-linear one)
# Run a linear regression for earnings-school relationship and earnings-exper relationship
reg8 = lm(earnings ~ school + exper + rural + year + trad + con + bus + fin, data = mydata)
cov8=vcovHC(reg8, type = "HC1")    
se8=sqrt(diag(cov8))

# Regression output table for reg8, inclusive of standard errors
stargazer(reg8, type = "text", 
          se = list(se8), 
          dep.var.labels = c("Annual Earnings"), #Dependent variable name
          covariate.labels = 
            c("Years of Schooling", "Years of Experience",
              "Rural", "Year", "Tradesperson", "Construction",
              "Business", "Finance"), #Names/labels of coefficients
          out = "reg_output_linear.txt")

# Estimating a log-linear relationships
# Create Logarithmic variable for earnings 
mydata$log_earn = log(mydata$earnings)

# Regression of log_earn on school and exper, holding rural, year and industry dummies fixed
reg9 = lm(log_earn ~ schoolquad + school + experquad + exper + rural + year + trad + con + bus + fin, data = mydata)
cov9=vcovHC(reg9, type = "HC1")    
se9=sqrt(diag(cov9))
#Obtain overall summary of reg9
summary(reg9)

#Regression output for reg9, inclusive of standard errors
stargazer(reg9, type = "text", 
          se = list(se9), 
          dep.var.labels = c("Logarithm of Earnings"), #Dependent variable name
          covariate.labels = 
            c("Years of Schooling Squared", "Years of Schooling",
              "Years of Experience Squared", "Years of Experience",
              "Rural", "Year", "Tradesperson", "Construction", "Business", "Finance"), #Names/labels of coefficients
          out = "reg_output_log_log.txt")

#Construct dataframe for predicting log_earn for school=12
newdata4=data.frame(schoolquad=144,school=12,experquad=25,exper=5,rural=1,year=2025, trad=0, con=0, bus=0, fin=0)
#Construct dataframe for predicting log_earn for school=13
newdata5=data.frame(schoolquad=13^2,school=13,experquad=25,exper=5,rural=1,year=2025, trad=0, con=0, bus=0, fin=0)

#Compute predicted value of log_earn for school=12
earn1=predict(reg9, newdata=newdata4)
#Compute predicted value of log_earn for school=13
earn2=predict(reg9, newdata=newdata5)
# Compute partial effect on ahe from changing from school=12 to school=13
d_earn=earn2-earn1
percentage_d_earn=d_earn*100
percentage_d_earn # i.e. an increase in one year of schooling (from years 12 to 13) results in a 9.92% increase in earnings. 
# Where we hold everything else constant.

# Effect of policy on earnings
# Create dummy variable where 1 = 2022 onwards and 0 = otherwise
mydata$post=as.numeric(mydata$year>=2022)
# Regression of earnings on post from 2022 onwards
reg10=lm(earnings ~ post, data=mydata[mydata$boom_twn==1,])
cov10=vcovHC(reg10, type = "HC1")    
se10=sqrt(diag(cov10))

# Regression output for reg10, inclusive of standard errors
stargazer(reg10, type = "text", 
          se = list(se10), 
          dep.var.labels = c("Earnings in $1000s"), #Dependent variable name
          covariate.labels = 
            c("Post", "Constant"), #Names/labels of coefficients
          out = "reg_output_post_bmtn.txt")

# Computing Difference-In-Differences
# Create interaction term boom_twn_post 
mydata$boom_twn_post=mydata$boom_twn * mydata$post
# Regression of earnings on post and boom_twn inclusive of interaction term boom_twn_post
reg11=lm(earnings ~ post + boom_twn + boom_twn_post, data=mydata)
cov11=vcovHC(reg11, type = "HC1")    
se11=sqrt(diag(cov11))

# Regression output for reg10 and reg11 for comparison, inclusive of standard errors
stargazer(reg10, reg11, type = "text", 
          se = list(se10,se11), 
          dep.var.labels = c("Earnings in $1000s"), #Dependent variable name
          covariate.labels = 
            c("Post","Boom Town", "Boom Town Times Post", "Constant"), #Names/labels of coefficients
          out = "reg_output_post_bmtn_interaction.txt")