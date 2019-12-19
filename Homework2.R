#Title: "Homework2"
#author: "Logan King"
#fontsize: 12
#geometry: margin=1
#output: pdf_document

library("MASS")

#Problem 1
##a
name <- "Gretchen Martinet"
department <- "Statistics"
courses <- c(2559, 3080)
activeteach2559 <- F
activeteach3080 <- T
enr2559 <- 10
enr3080 <- c(90,90,88)
days2559 <- c("Tuesday", "Thursday")
days3080 <- matrix(c("Monday", "Wednesday", "Tuesday", "Thursday", "Tuesday", "Thursday"),
                   nrow=3, byrow=T)
problem_1list <- list(Name=name, Department=department, Courses=courses, 
                      ActiveTeach2559=activeteach2559, ActiveTeach3080=activeteach3080,
                      Enr2559=enr2559, Enr3080=enr3080, Days2559=days2559, 
                      Days3080=days3080)
problem_1list
#operations found in Introduction to R from class

##b
answer1b <- which(problem_1list$Enr3080==90)
answer1b
#operations found in Introduction to R from class

##c
finalreports <- 0
for(n in 1:3) {
  if(problem_1list$ActiveTeach2559 == T) {
    finalreports <- finalreports + problem_1list$Enr2559[n]
  }
  if(problem_1list$ActiveTeach3080 == T) {
    finalreports <- finalreports + problem_1list$Enr3080[n]
  }
}
print(finalreports)
#operations found at 
#<https://www.dataquest.io/blog/control-structures-in-r-using-loops-and-if-else-statements/>

#Problem2
##a
planetname <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", 
                "Neptune")
planetname
planetdistance <- c(.39, .72, 1, 1.52, 5.2, 9.54, 19.18, 30.06)
planetdistance
planettype <- c(rep("terrestrial",4), rep("gas",4))
planettype
planetdiameter <- c(.382, .949, 1, .532, 11.209, 9.449, 4.007, 3.883)
planetdiameter
planetrotation <- c(58.64, -243.02, 1, 1.03, .41, .43, -.72, .67)
planetrotation
planetrings <- c(rep("no",4), rep("yes",4))
planetrings
planetmoons <- c(0, 0, 1, rep("1+", 5))
planetmoons
planet_data <- data.frame(planetname, planetdistance, planettype, planetdiameter, 
                          planetrotation, planetrings, planetmoons)
planet_data
names(planet_data) <- c("name", "distance", "type", "diameter", "rotation", "rings", 
                        "moons")
planet_data
#operations found in Introduction to R from class

##b
farawayplanets <- subset(planet_data, distance>=2)
farawayplanets
#operations found at <http://rprogramming.net/subset-data-in-r/>

##c
answer2c <- which(planet_data$rotation < 0)
answer2c
#operations found in Introduction to R from class

##d
largediameter <- subset(planet_data, diameter > 1)
largediameter[,c(1,3)]
#operations found at <http://rprogramming.net/subset-data-in-r/>

##e
manymoons <- subset(planet_data, moons == "1+")
manymoons[,2:3]
#operations found at <http://rprogramming.net/subset-data-in-r/>

#Problem3
USArrests

##a
USArrests2 <- USArrests[,-3]
USArrests2[1:5,]
#operations found in Introduction to R from class

##b
arrests_tot <- c(sum(USArrests2$Murder), sum(USArrests2$Assault), sum(USArrests2$Rape))
arrests_tot
#operations found at 
#<https://stackoverflow.com/questions/9676212/how-to-sum-all-values-of-a-column-of-in-a-data-frame>

##c
USArrests2$PropRape <- USArrests2$Rape / USArrests2$Assault
USArrests2[1:5,]
#operations found at 
#<https://stackoverflow.com/questions/13013231/how-can-i-divide-one-column-of-a-data-frame-through-another>

##d
answer3d <- subset(USArrests2, PropRape>.2)
row.names(answer3d)
#operations found at 
#<https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/row.names>

##e
answer3e <- USArrests2[order(-(USArrests2$Murder)),]
answer3e
#operations found at 
#<https://www.r-bloggers.com/r-sorting-a-data-frame-by-the-contents-of-a-column/>

##f
sum(USArrests2$Murder > USArrests2$Rape)
#operations found in Introduction to R from class