##equal width
data<- c(5,10,11,13,15,35,50,55,72,92,204,215)
bins<-3
bin_indices<-cut(data,bins)
mean_smooth<-tapply(data,bin_indices,mean)
print(mean_smooth)
median_smooth<-tapply(data,bin_indices,median)
print(median_smooth)
min_max_smooth<-tapply(data,bin_indices,function(x)c(min(x), max(x)))
print(min_max_smooth)

#equal frequency (split)

data<- c(5,10,11,13,15,35,50,55,72,92,204,215)
bins<-3
bin_indices<-cut(seq_along(data),bins,lables=FALSE)
bin_data<-split(data,bin_indices)
mean_smooth<-sapply(bin_data,mean)
print(mean_smooth)
median_smooth<-sapply(bin_data,median)
print(median_smooth)
min_max_smooth<-sapply(bin_data,function(x)c(min(x), max(x)))
print(min_max_smooth)

## highest mean and median 
class_a <- c(76,35,47,64,95,66,89,36,84,76,35,47,64,95,66,89,36,84)

class_b <- c(51,56,84,60,59,70,73,66,50,51,56,84,60,59,70,63,66,50)

mean(class_a)
mean(class_b)

median(class_a)
median(class_b)

if(mean(class_a) > mean(class_b))
{
  cat("class a has highest mean value")
}else
{
  cat("class b has highest mean value")
}

if(median(class_a) > median(class_b))
{
  cat("the highest median is class a")
}else
{
  cat("the highest median is class b")
}


## graph plot 

class_a <- c(76,35,47,64,95,66,89,36,84,76,35,47,64,95,66,89,36,84)

class_b <- c(51,56,84,60,59,70,73,66,50,51,56,84,60,59,70,63,66,50)

boxplot(class_a, class_b,
        names = c("Class A", "Class B"),
        main = "Comparison of Class A and Class B",
        ylab = "Marks",
        col = c("lightblue", "red"))

## 
age1 <- c(23,23,27,27,39,41,47,49,50)
age2 <- c(52,54,56,57,58,58,60,61)

age <- c(age1, age2)

x <- 35

min_age <- min(age)
max_age <- max(age)

min_max <- (x - min_age) / (max_age - min_age)

cat("Minimum age =", min_age, "\n")
cat("Maximum age =", max_age, "\n")
cat("Min-Max normalized value of 35 =", min_max)


age1 <- c(23,23,27,27,39,41,47,49,50)
age2 <- c(52,54,56,57,58,58,60,61)

age <- c(age1, age2)

x <- 35

mean_age <- mean(age)
sd_age <- 12.94

z_score <- (x - mean_age) / sd_age

cat("Mean age =", mean_age, "\n")
cat("Standard deviation =", sd_age, "\n")
cat("Z-score normalized value of 35 =", z_score)