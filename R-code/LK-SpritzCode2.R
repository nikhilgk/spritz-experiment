# Analyzing the Spritz Experiment results

#setwd("/Users/lkirch/W241-SpritzAnalysis")
#SpritzAll <- read.csv("Spritz Survey (Responses) - Form Responses 1.csv")

# run SpritzCode.R first to get the correct dataset

################  Spritz Data Analysis ##################

# summary stats all fields
summary(Spritz)

# which reading style first and which article first
table(Spritz$firstNormal, Spritz$firstFun)

# age density plot
agehist <- hist(Spritz$Age, xlab="Age", main="Distribution of Age in the Spritz Experiment", freq=FALSE)
curve(dnorm(x, mean=mean(Spritz$Age), sd=sd(Spritz$Age)), add=TRUE, col="blue", lwd=2)

# total correct answers
Spritz$score <- Spritz$FUN_ANS1 + Spritz$FUN_ANS2 + Spritz$FUN_ANS3 + Spritz$FUN_ANS4 + Spritz$FUN_ANS5 + Spritz$SAD_ANS1 + Spritz$SAD_ANS2 + Spritz$SAD_ANS3 + Spritz$SAD_ANS4 + Spritz$SAD_ANS5
table(Spritz$score)

scorehist <- hist(Spritz$score, xlab="Number of Correct Answers", main="Overall Reading Comprehension")
summary(Spritz$score)

# total per question answered correctly
sum(Spritz$FUN_ANS1)
sum(Spritz$FUN_ANS2)
sum(Spritz$FUN_ANS3)
sum(Spritz$FUN_ANS4)
sum(Spritz$FUN_ANS5)
sum(Spritz$SAD_ANS1)
sum(Spritz$SAD_ANS2)
sum(Spritz$SAD_ANS3)
sum(Spritz$SAD_ANS4)
sum(Spritz$SAD_ANS5)

# by reading type
table(Spritz$firstNormal, Spritz$FUN_ANS1)
table(Spritz$firstNormal, Spritz$FUN_ANS2)
table(Spritz$firstNormal, Spritz$FUN_ANS3)
table(Spritz$firstNormal, Spritz$FUN_ANS4)
table(Spritz$firstNormal, Spritz$FUN_ANS5)

table(Spritz$firstNormal, Spritz$SAD_ANS1)
table(Spritz$firstNormal, Spritz$SAD_ANS2)
table(Spritz$firstNormal, Spritz$SAD_ANS3)
table(Spritz$firstNormal, Spritz$SAD_ANS4)
table(Spritz$firstNormal, Spritz$SAD_ANS5)

# article lengths
funWords <- 345
sadWords <- 458

table(Spritz$Meta_firstStyle, Spritz$Meta_firstEssay)


# break out into groups
normFUN <- subset(Spritz, firstNormal == 1 & firstFun == 1 & t3 > 0)
normSAD <- subset(Spritz, firstNormal == 1 & firstFun == 0 & t3 > 0)
spritzFUN <- subset(Spritz, firstNormal == 0 & firstFun == 1 & t4 > 0)
spritzSAD <- subset(Spritz, firstNormal == 0 & firstFun == 0 & t4 > 0)

# save these subsets
write.table(normFUN, "normFUN.csv", sep="\t")
write.table(normSAD, "normSAD.csv", sep="\t")
write.table(spritzFUN, "spritzFUN.csv", sep="\t")
write.table(spritzSAD, "spritzSAD.csv", sep="\t")

# wpm normal style
normFUNwpm <- mean(normFUN$Meta_03/funWords)
normSADwpm <- mean(normSAD$Meta_03/sadWords)

normFUNwpm
normSADwpm

# just checking spritz wpm 
spritzFUNwpm <- mean(spritzFUN$Meta_04/funWords)
spritzSADwpm <- mean(spritzSAD$Meta_04/sadWords)

spritzFUNwpm
spritzSADwpm

summary(Spritz$t3)
summary(Spritz$t4)
summary(Spritz$t7)



# 2-Way Frequency Table 
#attach(mydata)
mytable <- table(Spritz$Meta_firstStyle,Spritz$Meta_firstEssay) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages


# Spritz experiment data analysis
# Uses SpritzMain table
options(scipen=100)
options(digits=4)
library("pastecs")
Spritz$FUN <- Spritz$FUN_ANS1 + Spritz$FUN_ANS2 + Spritz$FUN_ANS3 + Spritz$FUN_ANS4 + Spritz$FUN_ANS5
Spritz$SAD <- Spritz$SAD_ANS1 + Spritz$SAD_ANS2 + Spritz$SAD_ANS3 + Spritz$SAD_ANS4 + Spritz$SAD_ANS5
stat.desc(Spritz$FUN)
stat.desc(Spritz$SAD)
t.test(Spritz$FUN, conf.level=0.99)
t.test(Spritz$SAD, conf.level=0.99)
