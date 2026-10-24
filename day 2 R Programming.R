# 1.Covariance and correlation
# Children of three ages are asked to indicate their preference for three
# photographs of adults. Do the data suggest that there is a significant
# relationship between age and photograph preference? What is wrong
# with this study?
#
# Photograph:
# Age of child A B C
# 5-6 years: 18 22 20
#
# 7-8 years: 2 28 40
#
# 9-10 years: 20 10 40
#
# 1. Use cov() to calculate the sample covariance between B and C.
cov(c(22, 28, 10), c(20, 40, 40))

# 2. Use another call to cov() to calculate the sample covariance matrix for
# the preferences.
cov(data.frame(A=c(18,2,20), B=c(22,28,10), C=c(20,40,40)))

# 3. Use cor() to calculate the sample correlation between B and C.
cor(c(22, 28, 10), c(20, 40, 40))

# 4. Use another call to cor() to calculate the sample correlation matrix for
# the preferences.
cor(data.frame(A=c(18,2,20), B=c(22,28,10), C=c(20,40,40)))



## QUESTION 2
# 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 25, 25, 25, 25, 25, 28, 28, 30,
# 30, 30.
# (i) Partition the dataset using an equal-frequency partitioning method with bin equal to 3
# (ii) apply data smoothing using bin means and bin boundary.
# (iii) Plot Histogram for the above frequency division

# (i) Partition the dataset using an equal-frequency partitioning method with bin equal to 3
split(c(18,18,18,20,20,20,20,20,20,20,21,21,21,21,25,25,25,25,25,28,28,30,30,30),
      rep(1:8, each=3, length.out=24))

# (ii) apply data smoothing using bin means and bin boundary.
lapply(split(c(18,18,18,20,20,20,20,20,20,20,21,21,21,21,25,25,25,25,25,28,28,30,30,30),
             rep(1:8, each=3, length.out=24)),
       function(x) list(Bin_Mean=rep(mean(x),3),
                        Bin_Boundary=ifelse(x-min(x)<=max(x)-x,min(x),max(x))))

# (iii) Plot Histogram for the above frequency division
hist(c(18,18,18,20,20,20,20,20,20,20,21,21,21,21,25,25,25,25,25,28,28,30,30,30),
     main="Histogram for the above frequency division",
     xlab="Values")



# QUESTION: 3.Two Maths teachers are comparing how their Year 9 classes performed in the end of year exams. Their results are as follows:
# Class A: 76, 35, 47, 64, 95, 66, 89, 36, 8476,35,47,64,95,66,89,36,84
# Class B: 51, 56, 84, 60, 59, 70, 63, 66, 5051,56,84,60,59,70,63,66,50
# (i) Find which class had scored higher mean, median and range.
# (ii) Plot above in boxplot and give the inferences
# Class B: 51, 56, 84, 60, 59, 70, 63, 66, 5051,56,84,60,59,70,63,66,50

# (i) Find which class had scored higher mean, median and range.
c(mean(c(76,35,47,64,95,66,89,36,84)), mean(c(51,56,84,60,59,70,63,66,50)),
  median(c(76,35,47,64,95,66,89,36,84)), median(c(51,56,84,60,59,70,63,66,50)),
  diff(range(c(76,35,47,64,95,66,89,36,84))), diff(range(c(51,56,84,60,59,70,63,66,50))))

# (ii) Plot above in boxplot and give the inferences
boxplot(list(Class_A=c(76,35,47,64,95,66,89,36,84), Class_B=c(51,56,84,60,59,70,63,66,50)),
        main="Class A and Class B Boxplot", ylab="Marks")





# QUESTION 4.Let us consider one example to make the calculation method clear. Assume that the
# minimum and maximum values for the feature F are $50,000 and $100,000 correspondingly.
# It needs to range F from 0 to 1. In accordance with min-max normalization, v = $80,
# b) Use the two methods below to normalize the following group of data: 200, 300, 400, 600, 1000
# (a) min-max normalization by setting min = 0 and max = 1
# (b) z-score normalization

# (a) min-max normalization by setting min = 0 and max = 1
((c(200,300,400,600,1000)-min(c(200,300,400,600,1000)))/
    (max(c(200,300,400,600,1000))-min(c(200,300,400,600,1000))))

# (b) z-score normalization
(c(200,300,400,600,1000)-mean(c(200,300,400,600,1000)))/sd(c(200,300,400,600,1000))





# QUESTION 5.Make a histogram for the “AirPassengers “dataset, start at 100 on the x-axis, and from
# values 200 to 700, make the bins 150 wide

hist(AirPassengers,
     breaks=seq(100,700,by=150),
     main="Histogram of AirPassengers",
     xlab="AirPassengers",
     xlim=c(100,700))





# QUESTION 6.Obtain Multiple Lines in Line Chart using a single Plot Function in R.Use
# attributes“mpg”and“qsec”of the dataset “mtcars”

plot(mtcars$mpg, type="o", xlab="Cars", ylab="Values", main="Multiple Lines: mpg and qsec")
lines(mtcars$qsec, type="o")




# QUESTION 7.Download the Dataset "water" From R dataset Link.Find out whether there is a linear
# relation between attributes"mortality" and"hardness" by plot function.Fit the Data into the
# Linear Regression model.Predict the mortality for the hardness=88.

plot(water$hardness, water$mortality,
     xlab="Hardness",
     ylab="Mortality",
     main="Mortality vs Hardness")

model <- lm(mortality ~ hardness, data=water)

predict(model, data.frame(hardness=88))





# QUESTION 8.Create a Boxplot graph for the relation between "mpg"(miles per galloon) and "cyl"(number
# of Cylinders) for the dataset "mtcars" available in R Environment.

boxplot(mpg ~ cyl,
        data=mtcars,
        main="MPG vs Number of Cylinders",
        xlab="Number of Cylinders",
        ylab="Miles per Gallon (mpg)")





#  QUESTION 9. Assume the Tennis coach wants to determine if any of his team players are scoring outliers. To visualize the distribution of points scored by his players, then how can he decide to develop the box plot? Give suitable example using Boxplot visualization technique.

boxplot(c(12,15,18,20,22,24,25,27,30,32,35,60),
        main="Tennis Players' Scores",
        ylab="Points Scored",
        col="lightblue")




# QUESTION 10.Implement using R language in which age group of people are affected byblood pressure based on the diabetes dataset show it using scatterplot and bar chart (that is BloodPressure vs Age using dataset “diabetes.csv”)

diabetes <- data.frame(
  Age = c(21,25,29,31,35,39,42,45,48,52,55,58,62,65,68),
  BloodPressure = c(72,75,78,80,82,85,88,90,92,95,98,100,102,105,108)
)

# Scatterplot
plot(diabetes$Age, diabetes$BloodPressure,
     main="Blood Pressure vs Age",
     xlab="Age",
     ylab="Blood Pressure",
     pch=19)

# Bar chart
barplot(tapply(diabetes$BloodPressure,
               cut(diabetes$Age,
                   breaks=c(20,30,40,50,60,70)),
               mean),
        main="Blood Pressure by Age Group",
        xlab="Age Group",
        ylab="Average Blood Pressure")

