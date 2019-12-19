####
#Title: "Homework11"
#author: "Logan King"
#fontsize: 12
#geometry: margin=1
#output: pdf_document
####

library(MASS)
library(ggplot2)
library(BSDA)
library(UsingR)
library(OpenMx)
library(ggplot2)
library(pwr)
set.seed(12071997)

#Problem 1
##a
K <- 10000 #set K for replicate function
m <- 62.9 #set mean/median
sd <- 13.3 #set sd
sign1a <- 0 #create null vector
j <- 1 #use j for place holder
for(i in c(7,28,49)){ #run simulation for sample sizes of i
  pvals <- replicate(K, SIGN.test(rnorm(i, m, sd), alternative="two.sided", md=m)$p.value)
  #replicate test K times;
  #test for rnorm values with set parameters, two.sided, median set as m
  #save p.value for each replication in vector named pvals
  sign1a[j] <- sum(pvals<.05)/K
  #find proportion of pvals less than .05, save as j place in vector
  j <- j+1
}#repeat for each i
sign1a
#Operations found at <https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/SIGN.test>
#Operations also found in various class material

##b
K <- 10000 #set K for replicate function
m <- 62.9 #set mean/median
sd <- 13.3 #set sd
signrank1b <- 0 #create null vector
j <- 1 #use j for place holder
for(i in c(7,28,49)){ #run simulation for sample sizes of i
  pvals <- replicate(K, wilcox.test(rnorm(i, m, sd), alternative="two.sided", mu=m)$p.value)
  #replicate test K times;
  #test for rnorm values with set parameters, two.sided, median set as m
  #save p.value for each replication in vector named pvals
  signrank1b[j] <- sum(pvals<.05)/K
  #find proportion of pvals less than .05, save as j place in vector
  j <- j+1
}#repeat for each i
signrank1b
#difference here is test used, nothing else changes
#Operations fount in Hypothesis Testing in R from class

#Problem2
hw7.2 <- c(0.0477, 0.0489, 0.0473)
data.frame(row.names=c("Sample Size = 7","Sample Size = 28","Sample Size = 49"), 
           "sign"=sign1a, "sign-rank"=signrank1b, "t-test"=hw7.2)
#From this, we can draw that the sign test will produce the lowest propotion of a type 1 error. 
#The error is significantly lower in the instances of 7 and 28 sample sizes, but approaches 
#the other test's errors when the sample size is 49. It is to be noted, however that the probability
#of a type 1 error is less than 5% in all instances. 

#Problem 3
##a
K <- 10000
m <- 62.9
sd <- 13.3
med <- 59 #create value for new suspected median
sign3a <- 0
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, SIGN.test(rnorm(i, m, sd), alternative="two.sided", md=med)$p.value)
  #be sure to change median value in test arguments
  sign3a[j] <- 1-sum(pvals>.05)/K
  #need this new equation to find power
  j <- j+1
}
sign3a
#operations found in problem 1a

##b
K <- 10000
m <- 62.9
sd <- 13.3
med <- 59 #create value for new suspected median
signrank3b <- 0
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, wilcox.test(rnorm(i, m, sd), alternative="two.sided", mu=med)$p.value)
  #be sure to change median value in test arguments
  signrank3b[j] <- 1-sum(pvals>.05)/K
  j <- j+1
}
signrank3b
#difference here is test used, nothing else changes
#operations found in problem 1b

##c
K <- 10000
m <- 62.9
sd <- 13.3
med <- 59 #create value for new suspected median
ttest3c <- 0
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, t.test(rnorm(i, m, sd), alternative="two.sided", mu=med)$p.value)
  #be sure to change median value in test arguments
  ttest3c[j] <- 1-sum(pvals>.05)/K
  j <- j+1
}
ttest3c
#difference here is test used, nothing else changes
#operations found in hw 7.2

#Problem 4
data.frame(row.names=c("Sample Size = 7","Sample Size = 28","Sample Size = 49"), 
           "Sign"=sign3a, "Sign-rank"=signrank3b, "t-test"=ttest3c)
#The power for each test increases with a bigger sample size. Sign rank and t-test both have a higher
#power than the sign test. T-test performs just slightly better than the sign rank test in terms of 
#power, but at best the power is only slightly above 50%. A larger sample size would remedy this
#issue. 

#Problem 5
##a
K <- 10000
df <- 2
med <- 1.386294
sign5a <- 0
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, SIGN.test(rchisq(i, df), alternative="two.sided", md=med)$p.value)
  #be sure to change rnorm to rchisq
  sign5a[j] <- sum(pvals<.05)/K
  j <- j+1
}
sign5a
#operations found in earlier problems

##b
K <- 10000
df <- 2
med <- 1.386294
signrank5b <- 0
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, wilcox.test(rchisq(i, df), alternative="two.sided", mu=med)$p.value)
  #be sure to change rnorm to rchisq
  signrank5b[j] <- sum(pvals<.05)/K
  j <- j+1
}
signrank5b
#operations found in earlier problems

#Problem 6
data.frame(row.names=c("Sample Size = 7","Sample Size = 28","Sample Size = 49"), 
           "Sign"=sign5a, "Sign-rank"=signrank5b)
#The results of tests using the chi-squared distribution show that the sign-rank test has a
#significantly higher type 1 error than the sign test at each sample size. At every sample size
#tested, the sign test is below 5%, while the sign-rank test is never below that value. 

#Problem 7
##a
K <- 10000
df <- 2
med <- .5
sign7a <- 0
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, SIGN.test(rchisq(i, df), alternative="two.sided", md=med)$p.value)
  sign7a[j] <- 1-sum(pvals>.05)/K
  #note equation change for power
  j <- j+1
}
sign7a
#operations found in previous problems

##b
K <- 10000
df <- 2
med <- .5
signrank7b <- 0
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, wilcox.test(rchisq(i, df), alternative="two.sided", mu=med)$p.value)
  signrank7b[j] <- 1-sum(pvals>.05)/K
  #note equation change for power
  j <- j+1
}
signrank7b
#operations found in previous problems

#Porblem 8
data.frame(row.names=c("Sample Size = 7","Sample Size = 28","Sample Size = 49"), 
           "Sign"=sign7a, "Sign-rank"=signrank7b)
#The power for each test increases significantly when jumping from a sample size of 7 to 28. At a 
#sample size of 49, the power for the sign and sign-rank tests are 98% and 100%, respectively. 
#We can be confident that we are not making type two errors at this sample size (and at size 28
#of the sign-rank test).
