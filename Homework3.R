####
#Title: "Homework3"
#author: "Logan King"
#fontsize: 12
#geometry: margin=1
#output: pdf_document
####

library("MASS")

#Problem 1
##a
nym2002 <- data.frame(read.table("C:/Users/Student/Downloads/nym2002.txt", header=T))
nym2002[1:5,]
#operations found in Introduction to R from class

##b
nrow(nym2002) #since every row has a value for time, we can include every row
#operations found in Introduction to R from class

##c
oldestage <- max(nym2002[,3])
youngestage <- min(nym2002[,3])
oldestage
youngestage
#operations found in Introduction to R from class

##d
ageslowest <- nym2002[which.max(nym2002[,5]),3]
agefastest <- nym2002[which.min(nym2002[,5]),3]
ageslowest
agefastest
#operations found in Introduction to R from class

##e
length(which(nchar(as.character(nym2002[,4]), type="chars", allowNA = T, keepNA =2)==2))
#the nchar function counts the number of characters (2 for US and territories)
#the which(,==2) function alows me to specify which elements have 2 characters 
#   (which ones are from the US and territories)
#the length function tells me the number of elements given by the which operation
#   (the actual number of people from the US and territories)
#operations found in Introduction to R from class
#nchar operation found at <http://www.endmemo.com/program/R/nchar.php>

##f
min(nym2002[which(nym2002[,3] > ageslowest),1])
#subset the nym to the third column and find which runners were older than the age 
#   of the slowest person
#then subset nym to those values and the first column and take min to find best finisher
#   of that subset
##operations found in Introduction to R from class

##g
min(nym2002[,1])
#operations found in Introduction to R from class

##h
foreign <- nym2002[which(nchar(as.character(nym2002[,4]), type="chars", 
                               allowNA = T, keepNA =3)==3),4]
#use similar code from e to find foreign country runners
#subset nym2002 to get actual values instead of positions
#save as a vector
us <- nym2002[which(nchar(as.character(nym2002[,4]), type="chars", 
                          allowNA = T, keepNA =2)==2),4]
#repeat above to find us runners
us <- c("US")
#change us vector because us only needs one value
foreignvect <- unique(as.character(foreign))
#change to character vector and find number of unique values
usvect <- unique(as.character(us))
#same as above
answer1h <- length(foreignvect) + length(usvect)
#find length of vectors and add together, save as new variable
answer1h
#operations found in Introduction to R from class
#unique() operation found at 
#<https://chemicalstatistician.wordpress.com/2018/03/10/use-unique-instead-of-levels-to-find-the-possible-values-of-a-character-variable-in-r/>

#Problem 2
statecrashes <- read.csv("C:/Users/Student/Downloads/state crashes.csv", header=T)
library(ggplot2)
library(datasets)
library(Hmisc)
##a
statecrashesplot <- ggplot(statecrashes, aes(x=Licensed.drivers, y=Fatal.crashes))
statecrashesplot + geom_point(shape=5)
#operations found in Introduction to R from class

##b
colors2 <- c("1" = "green", "2"= "orange", "3"="brown")
#save vector for colors
plot2b <- ggplot(statecrashes, aes(x=Licensed.drivers, y=Fatal.crashes, 
                                   colour=as.factor(Hand.held.ban))) + 
  geom_point(shape=5) + scale_colour_manual(values=colors2)
#include handheldban as for color, but be sure to use as.factor so it will 
#   treat colors as discrete
plot2b
#operations found in Graphics in R from class
#as.factor found on piazza

##c
plot2b + geom_smooth(se=F)
#operations found at 
#<https://stackoverflow.com/questions/46584425/removing-the-confidence-interval-on-ggplot2-on-plot>

##d
plot2b + geom_smooth(method=lm, se=F)
#operations found in Graphics in R from class

##e
plot2e <- plot2b + geom_smooth(method=lm, alpha=.2, 
                               aes(fill=as.factor(Hand.held.ban))) + 
  scale_fill_manual(values=colors2)
plot2e
#operations found on piazza

##f
plot2b + geom_smooth(method=lm, se=F) + geom_smooth(method=lm, colour="blue", se=F)
#operations found in Graphics in R from class

##g
#From this set of graphics, we can draw multiple conclusions:
##1. States with more licensed drivers tend to have more fatal crashes.
##2. In regards to handheld ban, the states with no ban have the highest rate of
##    fatal crashes, followed by some ban, and total ban.
##3. It should be noted that this data could be influenced by outliers.
