# Analyzing the Spritz Experiment results

setwd("/Users/lkirch/spritz-experiment/R-code")

library("ggplot2")
library("pastecs")
require("foreign")
require("MASS")


SpritzAll <- read.csv("Responses2.csv")

# SpritzAll <- read.csv("~/Desktop/Spritz Survey (Responses) - Form Responses 1.csv")
# SpritzAll <- read.csv("C:/Users/Murat Aydogdu/Desktop/Spritz Survey (Responses) - Form Responses 1.csv")
# table(SpritzAll$Timestamp)
Spritz <- subset(SpritzAll, grepl("12/2",as.character(SpritzAll$Timestamp)))
# take out iffy HITs, if needed: 1nlcj2, l02n0t, 8ed4bq, cgo7zp, xw5gdf, rofvta
Spritz <- subset(Spritz, grepl("1nlcj2",as.character(Spritz$Code)) == FALSE)
Spritz <- subset(Spritz, grepl("l02n0t",as.character(Spritz$Code)) == FALSE)
Spritz <- subset(Spritz, grepl("8ed4bq",as.character(Spritz$Code)) == FALSE)
Spritz <- subset(Spritz, grepl("cgo7zp",as.character(Spritz$Code)) == FALSE)
Spritz <- subset(Spritz, grepl("xw5gdf",as.character(Spritz$Code)) == FALSE)
Spritz <- subset(Spritz, grepl("rofvta",as.character(Spritz$Code)) == FALSE)

names(Spritz)

# Recode answers to facilitate analysis
# First: which article came first, and which article had Spritz?
#"firstStyle":"Normal","firstEssay":"Fun"
Spritz$firstNormal <- ifelse(Spritz$FirstType=="Normal",1,0)
table(Spritz$firstNormal)
Spritz$firstFun <- ifelse(Spritz$FirstEssay=="Fun",1, 0)
table(Spritz$firstFun)

# ANS variables: 1 for True, = 0 for False
# A   variables: 1, 2, 3, 4 (A, B, C, D)
table(as.character(Spritz$FUN_Q1))
Spritz$FUN_ANS1 <- ifelse(grepl("4",as.character(Spritz$FUN_Q1)) & grepl("40",as.character(Spritz$FUN_Q1))==FALSE, 1, 0)
Spritz$FUN_A1 <- 0
Spritz$FUN_A1 <- ifelse(grepl("20 m",as.character(Spritz$FUN_Q1)), 1, Spritz$FUN_A1)
Spritz$FUN_A1 <- ifelse(grepl("4",as.character(Spritz$FUN_Q1)), 2, Spritz$FUN_A1)
Spritz$FUN_A1 <- ifelse(grepl("40 ",as.character(Spritz$FUN_Q1)), 3, Spritz$FUN_A1)
Spritz$FUN_A1 <- ifelse(grepl("10 ",as.character(Spritz$FUN_Q1)), 4, Spritz$FUN_A1)
table(Spritz$FUN_ANS1)
table(Spritz$FUN_A1)

table(as.character(Spritz$FUN_Q2))
Spritz$FUN_ANS2 <- ifelse(grepl("An active break where",Spritz$FUN_Q2), 1, 0)
Spritz$FUN_A2 <- 0
Spritz$FUN_A2 <- ifelse(grepl("A time to be",Spritz$FUN_Q2), 1, Spritz$FUN_A2)
Spritz$FUN_A2 <- ifelse(grepl("A Fitness Un",Spritz$FUN_Q2), 2, Spritz$FUN_A2)
Spritz$FUN_A2 <- ifelse(grepl("An active br",Spritz$FUN_Q2), 3, Spritz$FUN_A2)
Spritz$FUN_A2 <- ifelse(grepl("A Fifteen Up",Spritz$FUN_Q2), 4, Spritz$FUN_A2)
table(Spritz$FUN_ANS2)
table(Spritz$FUN_A2)

table(as.character(Spritz$FUN_Q3))
Spritz$FUN_ANS3 <- ifelse(grepl("Dr. Brendo",Spritz$FUN_Q3), 1, 0)
Spritz$FUN_A3 <- 0
Spritz$FUN_A3 <- ifelse(grepl("Rosie Hale",Spritz$FUN_Q3), 1, Spritz$FUN_A3)
Spritz$FUN_A3 <- ifelse(grepl("Dr. Charle",Spritz$FUN_Q3), 2, Spritz$FUN_A3)
Spritz$FUN_A3 <- ifelse(grepl("Dr Bayard ",Spritz$FUN_Q3), 3, Spritz$FUN_A3)
Spritz$FUN_A3 <- ifelse(grepl("Dr. Brendo",Spritz$FUN_Q3), 4, Spritz$FUN_A3)
table(Spritz$FUN_ANS3)
table(Spritz$FUN_A3)

table(as.character(Spritz$FUN_Q4))
Spritz$FUN_ANS4 <- ifelse(grepl("Less off-task",Spritz$FUN_Q4), 1, 0)
Spritz$FUN_A4 <- 0
Spritz$FUN_A4 <- ifelse(grepl("Less off-ta",Spritz$FUN_Q4), 1, Spritz$FUN_A4)
Spritz$FUN_A4 <- ifelse(grepl("More rowdy ",Spritz$FUN_Q4), 2, Spritz$FUN_A4)
Spritz$FUN_A4 <- ifelse(grepl("Happier stu",Spritz$FUN_Q4), 3, Spritz$FUN_A4)
Spritz$FUN_A4 <- ifelse(grepl("Students ea",Spritz$FUN_Q4), 4, Spritz$FUN_A4)
table(Spritz$FUN_ANS4)
table(Spritz$FUN_A4)

table(as.character(Spritz$FUN_Q5))
Spritz$FUN_ANS5 <- ifelse(grepl("Ontario",Spritz$FUN_Q5), 1, 0)
Spritz$FUN_A5 <- 0
Spritz$FUN_A5 <- ifelse(grepl("Quebec",Spritz$FUN_Q5), 1, Spritz$FUN_A5)
Spritz$FUN_A5 <- ifelse(grepl("Maine ",Spritz$FUN_Q5), 2, Spritz$FUN_A5)
Spritz$FUN_A5 <- ifelse(grepl("Boston",Spritz$FUN_Q5), 3, Spritz$FUN_A5)
Spritz$FUN_A5 <- ifelse(grepl("Ontari",Spritz$FUN_Q5), 4, Spritz$FUN_A5)
table(Spritz$FUN_ANS5)
table(Spritz$FUN_A5)

table(Spritz$SAD_Q1)
Spritz$SAD_ANS1 <- ifelse(grepl("240",Spritz$SAD_Q1), 1, 0)
Spritz$SAD_A1 <- 0
Spritz$SAD_A1 <- ifelse(grepl("5",Spritz$SAD_Q1),   1, Spritz$SAD_A1)
Spritz$SAD_A1 <- ifelse(grepl("40",Spritz$SAD_Q1),  2, Spritz$SAD_A1)
Spritz$SAD_A1 <- ifelse(grepl("100",Spritz$SAD_Q1), 3, Spritz$SAD_A1)
Spritz$SAD_A1 <- ifelse(grepl("240",Spritz$SAD_Q1), 4, Spritz$SAD_A1)
table(Spritz$SAD_ANS1)
table(Spritz$SAD_A1)

table(Spritz$SAD_Q2)
Spritz$SAD_ANS2 <- ifelse(grepl("27",Spritz$SAD_Q2), 1, 0)
Spritz$SAD_A2 <- 0
Spritz$SAD_A2 <- ifelse(grepl("7",Spritz$SAD_Q2), 1, Spritz$SAD_A2)
Spritz$SAD_A2 <- ifelse(grepl("17",Spritz$SAD_Q2), 2, Spritz$SAD_A2)
Spritz$SAD_A2 <- ifelse(grepl("27",Spritz$SAD_Q2), 3, Spritz$SAD_A2)
Spritz$SAD_A2 <- ifelse(grepl("37",Spritz$SAD_Q2), 4, Spritz$SAD_A2)
table(Spritz$SAD_ANS2)
table(Spritz$SAD_A2)

table(Spritz$SAD_Q3)
Spritz$SAD_ANS3 <- ifelse(grepl("Guilt longer",Spritz$SAD_Q3), 1, 0)
Spritz$SAD_A3 <- 0
Spritz$SAD_A3 <- ifelse(grepl("Shame long",Spritz$SAD_Q3), 1, Spritz$SAD_A3)
Spritz$SAD_A3 <- ifelse(grepl("Guilt long",Spritz$SAD_Q3), 2, Spritz$SAD_A3)
Spritz$SAD_A3 <- ifelse(grepl("Fear longe",Spritz$SAD_Q3), 3, Spritz$SAD_A3)
Spritz$SAD_A3 <- ifelse(grepl("Surprise l",Spritz$SAD_Q3), 4, Spritz$SAD_A3)
table(Spritz$SAD_ANS3)
table(Spritz$SAD_A3)

table(Spritz$SAD_Q4)
Spritz$SAD_ANS4 <- ifelse(grepl("You need more time",Spritz$SAD_Q4), 1, 0)
Spritz$SAD_A4 <- 0
Spritz$SAD_A4 <- ifelse(grepl("You need more time",Spritz$SAD_Q4), 1, Spritz$SAD_A4)
Spritz$SAD_A4 <- ifelse(grepl("The events that ca",Spritz$SAD_Q4), 2, Spritz$SAD_A4)
Spritz$SAD_A4 <- ifelse(grepl("Happiness is fleet",Spritz$SAD_Q4), 3, Spritz$SAD_A4)
Spritz$SAD_A4 <- ifelse(grepl("Sadness has less i",Spritz$SAD_Q4), 4, Spritz$SAD_A4)
table(Spritz$SAD_ANS4)
table(Spritz$SAD_A4)

table(Spritz$SAD_Q5)
Spritz$SAD_ANS5 <- ifelse(grepl("Leuven",Spritz$SAD_Q5), 1, 0)
Spritz$SAD_A5 <- 0
Spritz$SAD_A5 <- ifelse(grepl("Ghent",Spritz$SAD_Q5), 1, Spritz$SAD_A5)
Spritz$SAD_A5 <- ifelse(grepl("Leuve",Spritz$SAD_Q5), 2, Spritz$SAD_A5)
Spritz$SAD_A5 <- ifelse(grepl("Marbu",Spritz$SAD_Q5), 3, Spritz$SAD_A5)
Spritz$SAD_A5 <- ifelse(grepl("Amste",Spritz$SAD_Q5), 4, Spritz$SAD_A5)
table(Spritz$SAD_ANS5)
table(Spritz$SAD_A5)

# There should be no missing values in this section.
# Each variable is set to -1 initially.
# But there should remain no -1s after recoding
table(Spritz$English)
# Is English your primary language?
# PrimEng: 1 = Yes, 0 = No
Spritz$PrimEng <- -1
Spritz$PrimEng <- ifelse(grepl("Yes",Spritz$English), 1, Spritz$PrimEng)
Spritz$PrimEng <- ifelse(grepl("No", Spritz$English), 0, Spritz$PrimEng)
table(Spritz$PrimEng)

# What is your gender?
table(Spritz$Gender)
# Are you a female?
# Female: 1 = Yes, 0 = No
Spritz$Female <- -1
Spritz$Female <- ifelse(grepl("Fema",Spritz$Gender), 1, Spritz$Female)
Spritz$Female <- ifelse(grepl("Male", Spritz$Gender), 0, Spritz$Female)
table(Spritz$Female)

#Ethnicity origin (or Race): Please specify your ethnicity.
table(Spritz$Race)
# 1: Asian / Pacific Islander 
# 2: Black or African American 
# 3: Hispanic or Latino 
# 4: Native American or American Indian                              
# 5: Other 
# 6: White

Spritz$RaceCode <- -1
Spritz$RaceCode <- ifelse(grepl("Asian",Spritz$Race), 1, Spritz$RaceCode)
Spritz$RaceCode <- ifelse(grepl("Black",Spritz$Race), 2, Spritz$RaceCode)
Spritz$RaceCode <- ifelse(grepl("Hispa",Spritz$Race), 3, Spritz$RaceCode)
Spritz$RaceCode <- ifelse(grepl("Nativ",Spritz$Race), 4, Spritz$RaceCode)
Spritz$RaceCode <- ifelse(grepl("Other",Spritz$Race), 5, Spritz$RaceCode)
Spritz$RaceCode <- ifelse(grepl("White",Spritz$Race), 6, Spritz$RaceCode)
table(Spritz$RaceCode)

# What is the highest degree or level of education you have completed?
table(Spritz$Degree)
# 1: Some primary / secondary school (K-12), no diploma 
# 2: High school diploma 
# 3: Some college, no degree 
# 4: Associate or technical degree 
# 5: Bachelor'??s degree       
# 6: Graduate degree / PhD or Professional Degree 

Spritz$DegreeCode <- -1
Spritz$DegreeCode <- ifelse(grepl("Some pr",Spritz$Degree), 1, Spritz$DegreeCode)
Spritz$DegreeCode <- ifelse(grepl("High sc",Spritz$Degree), 2, Spritz$DegreeCode)
Spritz$DegreeCode <- ifelse(grepl("Some co",Spritz$Degree), 3, Spritz$DegreeCode)
Spritz$DegreeCode <- ifelse(grepl("Associa",Spritz$Degree), 4, Spritz$DegreeCode)
Spritz$DegreeCode <- ifelse(grepl("Bachelo",Spritz$Degree), 5, Spritz$DegreeCode)
Spritz$DegreeCode <- ifelse(grepl("Graduat",Spritz$Degree), 6, Spritz$DegreeCode)
table(Spritz$DegreeCode)

# What types of materials do you read at least once a week? Check all that apply.
Spritz$readTM <- ifelse(grepl("Text messages and ",Spritz$Reading), 1, 0)
Spritz$readBook <- ifelse(grepl("Books or e-books",Spritz$Reading), 1, 0) 
Spritz$readSci <- ifelse(grepl("Science and techn",Spritz$Reading), 1, 0)
Spritz$readMag <- ifelse(grepl("Magazine articles",Spritz$Reading), 1, 0)
Spritz$readProf <- ifelse(grepl("Professional doc",Spritz$Reading), 1, 0)
Spritz$readNone <- ifelse(grepl("I don't read any",Spritz$Reading), 1, 0)
table(Spritz$Reading)

table(Spritz$readTM)
table(Spritz$readBook)
table(Spritz$readSci)
table(Spritz$readMag)
table(Spritz$readProf)
table(Spritz$readNone)

# Do you typically wear glasses or contact lenses while you read?
table(Spritz$Glasses)
# UsesGlasses 1 = Yes, 0 = No
Spritz$UsesGlasses <- -1
Spritz$UsesGlasses <- ifelse(grepl("Yes",Spritz$Glasses), 1, Spritz$UsesGlasses)
Spritz$UsesGlasses <- ifelse(grepl("No", Spritz$Glasses), 0, Spritz$UsesGlasses)
table(Spritz$UsesGlasses)

# Which of the following best describes your previous experience with a reading technology like the one you used today (e.g., Spritz)?
table(Spritz$SpritzAns)
Spritz$SpritzExp <- -1
Spritz$SpritzExp <- ifelse(grepl("I have never used a",Spritz$SpritzAns), 0, Spritz$SpritzExp) 
Spritz$SpritzExp <- ifelse(grepl("I have tried using ",Spritz$SpritzAns), 1, Spritz$SpritzExp)
Spritz$SpritzExp <- ifelse(grepl("I have used a readi",Spritz$SpritzAns), 2, Spritz$SpritzExp)
Spritz$SpritzExp <- ifelse(grepl("I am familiar with ",Spritz$SpritzAns), 3, Spritz$SpritzExp)
table(Spritz$SpritzExp)


################  Spritz Data Analysis ##################

# Coding for time outcome variable
# firstNormal = 0  =>  Spritz time is Meta_04, Normal time is Meta_07
# firstNormal = 1  =>  Spritz time is Meta_07, Normal time is Meta_03
# trt = 0  =>  I want Normal time
# trt = 1  =>  I want Spritz time
# trt = 0 & firstNormal = 0  => Meta_07
# trt = 0 & firstNormal = 1  => Meta_03
# trt = 1 & firstNormal = 0  => Meta_04
# trt = 1 & firstNormal = 1  => Meta_07
SpritzMain2$Y_time <- (1-SpritzMain2$trt) * (1-SpritzMain2$firstNormal) * SpritzMain2$t7 +
  (1-SpritzMain2$trt) * SpritzMain2$firstNormal * SpritzMain2$t3 +
  SpritzMain2$trt * (1-SpritzMain2$firstNormal) * SpritzMain2$t4 +
  SpritzMain2$trt * SpritzMain2$firstNormal * SpritzMain2$t7

g <-ggplot(SpritzMain2, aes(FS, Y, colour = factor(trt))) + geom_point(position = position_jitter(w = 0.2, h = 0.2))+
  labs( title = "Number of correct answers by article type and treatment (trt=1 is Spritz)", x="Article", y="Correct answers")+
  theme(panel.grid.major.x  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        panel.grid.major.y  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=8), 
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(colour="blue", size=16, face="bold"))+
  scale_shape_discrete(name  ="Type",
                       breaks=c(0,1),
                       labels=c("Normal", "Spritz"))  
g

g <-ggplot(SpritzMain2, aes(FS, Y, colour = factor(trt))) + geom_boxplot()+
  labs( title = "Number of correct answers by article type and treatment (trt=1 is Spritz)", x="Article", y="Correct answers")+
  theme(panel.grid.major.x  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        panel.grid.major.y  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=8), 
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(colour="blue", size=16, face="bold"))+
  scale_shape_discrete(name  ="Type",
                       breaks=c(0,1),
                       labels=c("Normal", "Spritz"))  

g


#Univariate statistics
stat.desc(SpritzMain2$Y)
stat.desc(SpritzMain2$trt)
t.test(Y ~ FS, data = SpritzMain2)
t.test(Y ~ firstNormal, data = SpritzMain2)
t.test(Y ~ firstNormal, data = SpritzMain2)
t.test(Y ~ trt, data = SpritzMain2)

sqldf("select FS, TRT, avg(Y) from SpritzMain2 group by FS, TRT")

SpritzMain2$isSAD <- ifelse(SpritzMain2$FS == 'SAD', 1, 0)

# There doesn't seem to be an effect of whether the first article was normal or Spritz
summary(lm(SpritzMain2$Y ~ SpritzMain2$firstNormal))

# People have done worse with SAD article overall
summary(lm(SpritzMain2$Y ~ SpritzMain2$isSAD))

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt))
ols(Y ~ trt,SpritzMain2,cluster="Code")

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt * SpritzMain2$isSAD))
ols(Y ~ trt*isSAD,SpritzMain2,cluster="Code")

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt * SpritzMain2$isSAD + SpritzMain2$firstNormal))

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt + SpritzMain2$isSAD+ SpritzMain2$firstNormal 
           + SpritzMain2$SpritzExp + SpritzMain2$UsesGlasses + SpritzMain2$DegreeCode
           + SpritzMain2$PrimEng + SpritzMain2$Female + SpritzMain2$RaceCode 
           + SpritzMain2$readTM + SpritzMain2$readBook + SpritzMain2$readSci 
           + SpritzMain2$readMag + SpritzMain2$readProf))

# Murat code from: http://www.r-bloggers.com/standard-robust-and-clustered-standard-errors-computed-in-r/
ols <- function(form, data, robust=FALSE, cluster=NULL,digits=3){
  r1 <- lm(form, data)
  if(length(cluster)!=0){
    data <- na.omit(data[,c(colnames(r1$model),cluster)])
    r1 <- lm(form, data)
  }
  X <- model.matrix(r1)
  n <- dim(X)[1]
  k <- dim(X)[2]
  if(robust==FALSE & length(cluster)==0){
    se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(r1))/(n-k))))
    res <- cbind(coef(r1),se)
  }
  if(robust==TRUE){
    u <- matrix(resid(r1))
    meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X
    dfc <- n/(n-k)    
    se <- sqrt(dfc*diag(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X))))
    res <- cbind(coef(r1),se)
  }
  if(length(cluster)!=0){
    clus <- cbind(X,data[,cluster],resid(r1))
    colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster,"resid")
    m <- dim(table(clus[,cluster]))
    dfc <- (m/(m-1))*((n-1)/(n-k))
    uclust  <- apply(resid(r1)*X,2, function(x) tapply(x, clus[,cluster], sum))
    se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc)   
    res <- cbind(coef(r1),se)
  }
  res <- cbind(res,res[,1]/res[,2],(1-pnorm(res[,1]/res[,2]))*2)
  res1 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res1) <- rownames(res)
  colnames(res1) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
  return(res1)
}

# ==================================
# Analysis for Time outcome variable
# ==================================

#Univariate statistics
stat.desc(SpritzMain2$Y_time)
stat.desc(SpritzMain2$trt)
t.test(Y_time ~ FS, data = SpritzMain2)
t.test(Y_time ~ firstNormal, data = SpritzMain2)
t.test(Y_time ~ trt, data = SpritzMain2)

sqldf("select FS, TRT, avg(Y_time) from SpritzMain2 group by FS, TRT")

# As with comprehension, no effect of whether the first article was normal or Spritz
summary(lm(SpritzMain2$Y_time ~ SpritzMain2$firstNormal))

# People took around 16s longer to read the SAD article
summary(lm(SpritzMain2$Y_time ~ SpritzMain2$isSAD))

# Using Spritz @ 260wpm shaved ~15s off reading time
summary(lm(SpritzMain2$Y_time ~ SpritzMain2$trt))
ols(Y_time ~ trt,SpritzMain2,cluster="Code")

# Accounting for the article differences, Spritz shaved off 20s. No interaction effects.
summary(lm(SpritzMain2$Y_time ~ SpritzMain2$trt * SpritzMain2$isSAD))
ols(Y_time ~ trt*isSAD,SpritzMain2,cluster="Code")

summary(lm(SpritzMain2$Y_time ~ SpritzMain2$trt * SpritzMain2$isSAD + SpritzMain2$firstNormal))

summary(lm(SpritzMain2$Y_time ~ SpritzMain2$trt + SpritzMain2$isSAD+ SpritzMain2$firstNormal 
           + SpritzMain2$SpritzExp + SpritzMain2$UsesGlasses + SpritzMain2$DegreeCode
           + SpritzMain2$PrimEng + SpritzMain2$Female + SpritzMain2$RaceCode 
           + SpritzMain2$readTM + SpritzMain2$readBook + SpritzMain2$readSci 
           + SpritzMain2$readMag + SpritzMain2$readProf))


# total correct answers
SpritzMain2$score <- SpritzMain2$FUN_ANS1 + SpritzMain2$FUN_ANS2 + SpritzMain2$FUN_ANS3 + SpritzMain2$FUN_ANS4 + SpritzMain2$FUN_ANS5 + SpritzMain2$SAD_ANS1 + SpritzMain2$SAD_ANS2 + SpritzMain2$SAD_ANS3 + SpritzMain2$SAD_ANS4 + SpritzMain2$SAD_ANS5

# % total correct answers
SpritzMain2$scorePct <- ((SpritzMain2$FUN_ANS1 + SpritzMain2$FUN_ANS2 + SpritzMain2$FUN_ANS3 + SpritzMain2$FUN_ANS4 + SpritzMain2$FUN_ANS5 + SpritzMain2$SAD_ANS1 + SpritzMain2$SAD_ANS2 + SpritzMain2$SAD_ANS3 + SpritzMain2$SAD_ANS4 + SpritzMain2$SAD_ANS5)/10)*100

# article lengths
funWords <- 345
sadWords <- 458

table(SpritzMain2$FirstType, SpritzMain2$FirstEssay)

SpritzMain2$Y_timeInMins <- SpritzMain2$Y_time/(1000*60)
SpritzMain2$Y_wpm <- (SpritzMain2$isSAD * (sadWords) + (1 - SpritzMain2$isSAD) * (funWords)) / SpritzMain2$Y_timeInMins

# break out into groups
normFUN <- subset(SpritzMain2, firstNormal == 1 & firstFun == 1)
normSAD <- subset(SpritzMain2, firstNormal == 1 & firstFun == 0)
spritzFUN <- subset(SpritzMain2, firstNormal == 0 & firstFun == 1)
spritzSAD <- subset(SpritzMain2, firstNormal == 0 & firstFun == 0)

# save these subsets - in case we need them later
write.table(normFUN, "normFUN.csv", sep="\t")
write.table(normSAD, "normSAD.csv", sep="\t")
write.table(spritzFUN, "spritzFUN.csv", sep="\t")
write.table(spritzSAD, "spritzSAD.csv", sep="\t")

# normal percent correct answers
normFUNpctCor <- mean(normFUN$scorePct)
normSADpctCor <- mean(normSAD$scorePct)

normFUNpctCor
normSADpctCor

# spritz percent correct answers
spritzFUNpctCor <- mean(spritzFUN$scorePct)
spritzSADpctCor <- mean(spritzSAD$scorePct)

spritzFUNpctCor
spritzSADpctCor

scorePcthistNormFUN <- hist(normFUN$scorePct, xlab="Number of Correct Answers", main="Overall Reading Comprehension Normal Reading FUN Article")
summary(scorePcthistNormFUN)
scorePcthistNormSAD <- hist(normSAD$scorePct, xlab="Number of Correct Answers", main="Overall Reading Comprehension Normal Reading SAD Article")
summary(scorePcthistNormSAD)

scorePcthistSpritzFUN <- hist(spritzFUN$scorePct, xlab="Percent Correct Answers", main="Overall Reading Comprehension Spritz Reading FUN Article")
summary(scorePcthistSpritzFUN)
scorePcthistSpritzSAD <- hist(spritzSAD$scorePct, xlab="Percent Correct Answers", main="Overall Reading Comprehension Spritz Reading SAD Article")
summary(scorePcthistSpritzSAD)


# wpm normal style
normFUNwpm <- mean(normFUN$Y_wpm)
normSADwpm <- mean(normSAD$Y_wpm)

normFUNwpm
normSADwpm

# spritz wpm 
spritzFUNwpm <- mean(spritzFUN$Y_wpm)
spritzSADwpm <- mean(spritzSAD$Y_wpm)

spritzFUNwpm
spritzSADwpm

# converting PSB rates into wpm
psbEmailWords <- 110
psbArticleWords <- 210

# from PSB study
psbEmailNormalSec <- 35.3
psbEmailSpritzSec <- 23.8

psbArticleNormalSec <- 63.8
psbArticlSpritzSec <- 45.4

# using google to convert to minutes
psbEmailNormalMin <- 0.588
psbEmailSpritzMin <- 0.397

psbArticleNormalMin <- 1.063
psbArticlSpritzMin <- 0.757


psbEmailNormalwpm <- psbEmailWords/psbEmailNormalMin
psbEmailSpritzwpm <- psbEmailWords/psbEmailSpritzMin

psbArticleNormalwpm <- psbArticleWords/psbArticleNormalMin
psbArticleSpritzwpm <- psbArticleWords/psbArticlSpritzMin

psbEmailNormalwpm
psbEmailSpritzwpm

psbArticleNormalwpm
psbArticleSpritzwpm

