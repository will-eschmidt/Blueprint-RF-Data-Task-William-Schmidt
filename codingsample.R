install.packages("car")
setwd("C:/Users/willi/Documents/Pols 310")
library(car)
mydata <- read.table("panel80.txt")
# attach(mydata) 
# In case you want to work with the variables directly
names(mydata) 
# This shows us all the variable names.
 options(scipen=20) 
 # suppress "scientific" notation
 #options(scipen=NULL) 
 # Brings things back to normal
car.model <- lm(CARFEEL3 ~ INC + AGE + EDUC  + PDEMCONT + PARTYID + DEMPART3 + EDUC:INC + PARTYID*PDEMCONT*DEMPART3, data=mydata) 
summary(car.model)
layout(matrix(c(1,2,3,4),2,2)) 
# optional 4 graphs/page plot(reagan.model) 
# These are diagnostic plots.
vif(lm(CARFEEL3 ~ INC + AGE + EDUC + PDEMCONT + PARTYID + DEMPART3 + EDUC:INC + PARTYID*PDEMCONT*DEMPART3, data=mydata))
tol <- 1/vif(lm(CARFEEL3 ~ INC + AGE + EDUC + PDEMCONT + PARTYID + DEMPART3 + EDUC:INC + PARTYID*PDEMCONT*DEMPART3, data=mydata))
tol 
mysubsetdata<-subset(mydata, select=c(CARFEEL3, DEMPART3, INC, AGE, PARTYID, EDUC, PDEMCONT))
#This keeps only the variables that we are using.
cor(mysubsetdata, use = "pairwise.complete.obs") 
# A correlation matrix for the variables in the regression

library(MASS)


# This prints out the values of the ridge estimates as lambda increases.
model2 <- lm(CARFEEL3 ~ INC + AGE + PDEMCONT + PARTYID + DEMPART3 + EDUC, data=mydata)
summary(model2)
tol2 <- 1/vif(lm(CARFEEL3 ~ INC + AGE + EDUC + PDEMCONT + PARTYID + DEMPART3, data=mydata))
tol2

sdvariables <- sapply(mysubsetdata, sd, na.rm = TRUE)
# This gets the standard deviations of all the variables.
sdvariables 
# This prints out the standard deviations, which is not very useful but nice to see.
mystandardizeddata <- as.data.frame(scale(mysubsetdata, center=FALSE, scale=sdvariables) )
# standardize variables 
var(mystandardizeddata, use = "pairwise.complete.obs") 
# Note that the variance-covariance matrix = correlation matrix
carter.model2 <- lm(CARFEEL3 ~ INC + AGE + PARTYID + PDEMCONT + DEMPART3 + EDUC, data=mystandardizeddata)
summary(carter.model2)

cartermodel3 <- lm(CARFEEL3 ~ INC + AGE + EDUC  + PDEMCONT + PARTYID + DEMPART3 + EDUC:INC + PARTYID*PDEMCONT*DEMPART3, data=mystandardizeddata) 
summary(cartermodel3)
install.packages("fields")
library(fields)
increment <- .5
x <- -5; y <- -5; z <- NULL; w <- NULL; iterations <- 30
thing1 <- NULL; thing2 <- NULL;xcounter<-0;ycounter<-0
for (i in 1:iterations) { 
  xcounter <- xcounter+1
  ycounter <- 0
  for (j in 1:iterations) {
    ycounter <- ycounter+1
    thing1[j] <- 3*x[i] + 4*y[j] + 3*x[i]*y[j] # This is your prediction equation.
    if (ycounter < iterations) y[j+1] <- y[j] + increment}
  z <- cbind(z,thing1)
  if (xcounter < iterations) x[i+1] <- x[i] + increment}
t <- matrix(z, iterations,iterations)
x<-matrix(x)
y<-matrix(y)
drape.plot(x,y,t, col=FALSE)


