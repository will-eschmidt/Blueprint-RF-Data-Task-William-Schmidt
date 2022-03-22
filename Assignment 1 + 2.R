#Us Party, Assignment One for Pols 310 
# First we get our data.
setwd("C:/Users/willi/Documents/Pols 310")
mydata <- read.table("usparty.txt")
names(mydata) 
# Lets us see all the variable names.
#attach(mydata) 
# This puts the variable names in memory. We will not be using this.
mysubsetdata<-subset(mydata, select=c(YEAR, MTOTCONG)) 
#This keeps only the two variables that we need.
summary(mysubsetdata) 
# Since no variables are listed, a summary for all variables in the data frame is printed.
mysubsetdata 
#This prints out all the variable values.

# Now let us work with just the years 1950 through 1970.
my3270data <- subset(mysubsetdata, YEAR >= 1932 & YEAR <= 1970)
my3270data
# attach(my5070data) # Let us avoid variable confusion with these data sets by not doing this.
plot(my3270data$YEAR, my3270data$MTOTCONG, xlab="", ylab="", ylim=c(0.2,0.8), pch=19, type="o", axes=FALSE)
axis(1, at=c(1932, 1936, 1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968)) 
# This defines the X axis tick marks.a
axis(2, yaxs="r") 
# This defines the Y axis.
box()
title(xlab="Year", ylab="Congressional Mobilization", main="Figure 1: Plot of U.S. Congressional Mobilization, 1932-70", cex=1.5, col="black", font=2)
mean(my3270data$MTOTCONG)



# Now let us work with just the years 1972 through 1988.
my7288data <- subset(mysubsetdata, YEAR >= 1972 & YEAR <= 1988)
my7288data
# attach(my7288data) # Again, let us avoid variable confusion with these data sets by not doing this.
plot(my7288data$YEAR, my7288data$MTOTCONG, xlab="", ylab="", ylim=c(0.2,0.8), pch=19, type="o", axes=FALSE)
axis(1, at=c(1972, 1976, 1980, 1984, 1988))
# This defines the X axis tick marks.
axis(2, yaxs="r") 
# This defines the Y axis.
box()
title(xlab="Year", ylab="Congressional Mobilization", main="Figure 2: Plot of U.S. Congressional Mobilization, 1972-88", cex=1.5, col="black", font=2)
mean(my7288data$MTOTCONG)

windows()

# Now let us work with on-year and off-year separately to get the means
myON3270data <- subset(mysubsetdata, YEAR == 1932 | YEAR == 1936 | YEAR == 1940 | YEAR == 1944 | YEAR == 1948 | YEAR == 1952 | YEAR == 1956 | YEAR 
== 1960 | YEAR == 1964 | YEAR == 1968)
mean(myON3270data$MTOTCONG)
myOFF3270data <- subset(mysubsetdata, YEAR == 1934 | YEAR == 1938 | YEAR == 1942 | YEAR == 1946 | YEAR == 1950 | YEAR == 1954 | YEAR == 1958 | YEAR == 1962 | YEAR == 1966 | YEAR == 1970)
mean(myOFF3270data$MTOTCONG)

#Let's find the means for On and Off years for elections between 1972 and 1988 means
myON7288data <- subset(mysubsetdata, YEAR == 1972 | YEAR == 1976 | YEAR 
                            == 1980 | YEAR == 1984 | YEAR == 1988)
mean(myON7288data$MTOTCONG)
myOFF7288data <- subset(mysubsetdata, YEAR == 1974 | YEAR == 1978 | YEAR == 1982 | YEAR == 1986)
mean(myOFF7288data$MTOTCONG)

# First we get our data.
mydata <- read.table("usparty.txt")
names(mydata) # Lets us see all the variable names.
mysubsetdata<-subset(mydata, select=c(YEAR, MTOTCONG, ON)) #This keeps only the three variables that we need.
summary(mysubsetdata) # Since no variables are listed, a summary for all variables in the data frame is printed.
mysubsetdata #This prints out all the variable values.
# Now let us work with just the years 1932 through 1970, on-year data only.
my3270onyeardata <- subset(mysubsetdata, YEAR >= 1932 & YEAR <= 1970 & ON == 1)
my3270onyeardata
# Now let us work with just the years 1972 through 1988, on-year data only.
my7288onyeardata <- subset(mysubsetdata, YEAR >= 1972 & YEAR <= 1988 & ON == 1)
my7288onyeardata
# Here is the t-test between the earlier and later periods for on-year elections.
t.test(my3270onyeardata$MTOTCONG, my7288onyeardata$MTOTCONG)
# If you were assuming equal variances across time periods, then you would write this test as below.
t.test(my3270onyeardata$MTOTCONG, my7288onyeardata$MTOTCONG, var.equal=TRUE)
# You could also conduct an F-test to see if the variances really are equal, to be safe.
var.test(my3270onyeardata$MTOTCONG, my7288onyeardata$MTOTCONG)


# Now let us work with just the years 1932 through 1970, off-year data only.
my3270offyeardata <- subset(mysubsetdata, YEAR >= 1932 & YEAR <= 1970 & ON == 0)
my3270offyeardata
# Now let us work with just the years 1972 through 1988, off-year data only.
my7288offyeardata <- subset(mysubsetdata, YEAR >= 1972 & YEAR <= 1988 & ON == 0)
my7288offyeardata
# Here is the t-test between the earlier and later periods for off-year elections.
t.test(my3270offyeardata$MTOTCONG, my7288offyeardata$MTOTCONG)
# If you were assuming equal variances across time periods, then you would write this test as below.
t.test(my3270offyeardata$MTOTCONG, my7288offyeardata$MTOTCONG, var.equal=TRUE)
# You could also conduct an F-test to see if the variances really are equal, to be safe.
var.test(my3270offyeardata$MTOTCONG, my7288offyeardata$MTOTCONG)

