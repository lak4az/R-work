####
#Title: "Homework7"
#author: "Logan King"
#fontsize: 12
#geometry: margin=1
#output: pdf_document
####

library("MASS")
library("BSDA")
library("ggplot2")
set.seed(12071997)

#Problem1
K <- 10000 #set K as 10000 for number of samples
m <- 62.9 #set m as mean of population
s <- 13.3 #set s as stdev of population
t1e1 <- NULL #set t1e1 as empty vector for loop values to be stored
j <- 1 #set j as 1, will be used to store each value in a position in t1e1
for(i in c(7,28,49)){ #repeat loop for sample sizes 7, 28, 49
  pvals <- replicate(K, z.test(rnorm(i,m,s), y=NULL, alternative="two.sided", mu=m, sigma.x=s,
                               conf.level=.95)$p.value)
  #replicate ztest for normal distribution set by parameters for each sample size K times; 
  #one-sample test (y=Null); two tailed hypothesis (alternative="two.sided"), set parameters
  #mu and sigma.x; confidence level is .95; finding p-value; save as pvals
  t1e1[j] <- sum(pvals<.05)/K #sum the number of pvals less than the significance level and divide by
                             #total K
  j <- j+1 #add 1 to j to save each i in new position j of t1e1
}
t1e1 #print t1e1

#The Type I error of the z-test that the practicioner will conduct by determining
#the proportion of 10,000 random samples that would lead the practitioner to reject the null
#hypothesis is .0519 for a sample size of 7, .0506 for a sample size of 28, and .0484 for 
#sample size of 49.

#Operations found in Graphics in R and Simulations in R from class
#Operations also found at 
#<https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/z.test>


#Problem2
K <- 10000 #set K as 10000 for number of samples
m <- 62.9 #set m as mean of population
s <- 13.3 #set s as stdev of population
t1e2 <- NULL #set t1e2 as empty vector for loop values to be stored
j <- 1 #set j as 1, will be used to store each value in a position in t1e2
for(i in c(7,28,49)){ #repeat loop for sample sizes 7, 28, 49
  pvals <- replicate(K, t.test(rnorm(i,m,s), y=NULL, alternative="two.sided", mu=m, conf.level=.95)
                     $p.value)
  #replicate ttest for normal distribution set by parameters for each sample size K times; 
  #one-sample test (y=Null); two tailed hypothesis (alternative="two.sided"), set parameters mu; 
  #confidence level is .95; finding p-value; save as pvals
  t1e2[j] <- sum(pvals<.05)/K #sum the number of pvals less than the significance level and divide by
                             #total K
  j <- j+1 #add 1 to j to save each i in new position j of t1e2
}
t1e2 #print t1e2

#The Type I error of the t-test that the practicioner will conduct by determining
#the proportion of 10,000 random samples that would lead the practitioner to reject the null
#hypothesis is .0477 for a sample size of 7, .0489 for a sample size of 28, and .0473 for a sample 
#size of 49. 

#Operations found in Graphics in R and Simulations in R from class
#Operations also found at 
#<https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/t.test>

#Problem3
K <- 10000 #set K as 10000 for number of samples
m <- 62.9 #set m as mean of population
s <- 13.3 #set s as stdev of population
t1e3 <- NULL #set t1e3 as empty vector for loop values to be stored
j <- 1 #set j as 1, will be used to store each value in a position in t1e3
for(i in c(7,28,49)){ #repeat loop for sample sizes 7, 28, 49
  pvals <- replicate(K, z.test(rnorm(i,m,s), y=NULL, alternative="two.sided", mu=m, 
                               sigma.x=s/sqrt(i),conf.level=.95)$p.value)
  #replicate ztest for normal distribution set by parameters for each sample size K times; 
  #one-sample test (y=Null); two tailed hypothesis (alternative="two.sided"), set parameters
  #mu and sigma.x; confidence level is .95; finding p-value; save as pvals
  t1e3[j] <- sum(pvals<.05)/K #sum the number of pvals less than the significance level and divide by
                             #total K
  j <- j+1 #add 1 to j to save each i in new position j of t1e3
}
t1e3 #print t1e3

#The Type I error of the z-test that the practicioner will conduct by determining
#the proportion of 10,000 random samples that would lead the practitioner to reject the 
#null hypothesis is .4638 for a sample size of 7, .7079 for a sample size of 28 and .7815 for a 
#sample size of 49.

#Operations found in Graphics in R and Simulations in R from class
#Operations also found at 
#<https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/z.test>

#Problem 4
summarydata <- data.frame(row.names=c("Sample Size = 7","Sample Size = 28","Sample Size = 49"),
                          "Problem1"=t1e1, "Problem2"=t1e2, "Problem3"=t1e3)
#create dataframe with row names as sample sizes and column names as problem numbers
summarydata #print dataframe

#From this summary, we can conclude that a ztest in which we know the population mean and standard
#deviation and a ttest in which we only know the population mean will lead to drastically lower 
#proportions of the type 1 error for each sample size in 10000 samples than a ztest for which a 
#random sample is taken from the population for which the standard deviation is not known. 

#Operations found in Introduction to R from class

#Problem 5
##a
K <- 10000 #set K as 10000 for number of samples
df <- 2 #set df as 2
prob5 <- rchisq(K, df) #define the distribution and save as prob5
hist(prob5, prob=T) #create histogram
curve(dchisq(x, df), add=T) #add density curve on histogram

#The shape of the density curve of the population distribution fits the chi squared distribution.

#Operations found in Graphics in R from class.


##b
K <- 10000
m5 <- 2
s5 <- 2
df <- 2
t1e5b <- NULL
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, z.test(rchisq(i,df), y=NULL, alternative="greater", mu=m5, sigma.x=s5,
                               conf.level=.95)$p.value)
  #same as 1 except note the rchisq and different parameter values for ztest
  t1e5b[j] <- sum(pvals<.05)/K
  j <- j+1
}
t1e5b

#The Type I error of the z-test that the practicioner will conduct by determining
#the proportion of 10,000 random samples that would lead the practitioner to reject the null
#hypothesis is .0626 for a sample size of 7, .0576 for a sample size of 28, and .0559 for 
#sample size of 49.

#Operations found in Graphics in R and Simulations in R from class
#Operations also found at 
#<https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/z.test>


##c
K <- 10000
m5 <- 2
s5 <- 2
df <- 2
t1e5c <- NULL
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, t.test(rchisq(i,df), y=NULL, alternative="greater", mu=m5, conf.level=.95)
                     $p.value)
  #same as 2 except note the rchisq and different parameter values for ttest
  t1e5c[j] <- sum(pvals<.05)/K
  j <- j+1
}
t1e5c

#The Type I error of the t-test that the practicioner will conduct by determining
#the proportion of 10,000 random samples that would lead the practitioner to reject the null
#hypothesis is .0131 for a sample size of 7, .0196 for a sample size of 28, and .0265 for a sample 
#size of 49.

#Operations found in Graphics in R and Simulations in R from class
#Operations also found at 
#<https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/t.test>


##d
K <- 10000
m5 <- 2
s5 <- 2
df <- 2
t1e5d <- NULL
j <- 1
for(i in c(7,28,49)){
  pvals <- replicate(K, z.test(rchisq(i,df), y=NULL, alternative="greater", mu=m5, 
                               sigma.x=s5/sqrt(i), conf.level=.95)$p.value)
  #same as 3 except note the rchisq and different parameter values for ztest
  t1e5d[j] <- sum(pvals<.05)/K
  j <- j+1
}
t1e5d

#The Type I error of the z-test that the practicioner will conduct by determining
#the proportion of 10,000 random samples that would lead the practitioner to reject the 
#null hypothesis is .2526 for a sample size of 7, .3650 for a sample size of 28 and .3928 for a 
#sample size of 49.

#Operations found in Graphics in R and Simulations in R from class
#Operations also found at 
#<https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/z.test>


#Problem 6
summarydata6 <- data.frame(row.names=c("Sample Size = 7","Sample Size = 28","Sample Size = 49"),
                          "Problem5b"=t1e5b, "Problem5c"=t1e5c, "Problem5d"=t1e5d)
summarydata6

#From this summary, we can conclude that a ttest in which we only know the population mean will lead
#to lower proportions of the type 1 error for each sample size in 10000 samples than a ztest in 
#which we know the population mean and standard deviation. Both of these will lead to drastically
#lower proportions of the type 1 error for each sample size in 10000 samples than a ztest for which
#a random sample is taken from the population for which the standard deviation is not known.

#Operations found in Introduction to R from class

#Problem 7
summarydata
summarydata6

#Overall, we can conclude that a ztest for which a random sample is taken from the population
#for which the standard deviation is not known consistently generates drastically high proportions
#of a Type 1 error over our simulations. The best option for each distribution seems to be the
#ttest for which we only know the population mean. 