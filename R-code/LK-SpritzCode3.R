# Analyzing the Spritz Experiment results

setwd("/Users/lkirch/spritz-experiment/R-code")
SpritzMain2 <- read.csv("SpritzMain2.csv")

library("ggplot2")
library("pastecs")
require("foreign")
require("MASS")

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

