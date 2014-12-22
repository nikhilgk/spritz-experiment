# Load SpritzMain2.csv:
SpritzMain2 <- read.csv("~/Desktop/SpritzMain2.csv")

SpritzMain2$isSAD <- ifelse(SpritzMain2$FS == 'SAD', 1, 0)

# Add variable for time on reading pages:
# Coding for time outcome variable
# Gratuitous commenting since it'll be a one line assignment
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

SpritzMain2$Y_timeInSecs <- SpritzMain2$Y_time/1000
SpritzMain2$Y_timeInMins <- SpritzMain2$Y_time/(1000*60)
sad_words <- 458
fun_words <- 345
SpritzMain2$Y_wpm <- (SpritzMain2$isSAD * sad_words +
                        (1 - SpritzMain2$isSAD) * fun_words) / SpritzMain2$Y_timeInMins


# Spritz experiment data analysis
# Uses SpritzMain2 table: each subject has two observations
options(scipen=100)
options(digits=4)
library("pastecs", lib.loc="~/R/win-library/2.15")
library("sqldf", lib.loc="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")
table(SpritzMain2$FS)
table(SpritzMain2$Age)

# For univariate descriptive statistics useSpritzMain: one observation per subject
#Age distribution
c  <-ggplot(SpritzMain, aes(Age)) + geom_bar(binwidth=5) + coord_flip()+
     labs( title = "Age distribution of the subjects", x="Age", y="Count")+
     theme(panel.grid.major.x  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        panel.grid.major.y  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=8), 
        panel.background = element_blank(),
        panel.border = element_blank())
c

#Gender
c  <-ggplot(SpritzMain, aes(Female)) + geom_bar(binwidth=1) + #coord_flip()+
  labs( title = "Gender distribution", x="Gender (Female=1)", y="Count")+
  theme(panel.grid.major.x  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        panel.grid.major.y  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=8), 
        panel.background = element_blank(),
        panel.border = element_blank())+
  scale_x_discrete(name  ="",
                       breaks=c(0,1),
                       labels=c("Male", "Female")) 
c

table(SpritzMain$PrimEng)
table(SpritzMain$Female)
table(SpritzMain$RaceCode)
c  <-ggplot(SpritzMain, aes(DegreeCode)) + geom_bar(binwidth=1) + #coord_flip()+
  labs( title = "Highest degree or level of education", x="Gender (Female=1)", y="Count")+
  theme(panel.grid.major.x  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        panel.grid.major.y  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=8), 
        panel.background = element_blank(),
        panel.border = element_blank())+
  scale_x_discrete(name  ="",
                   breaks=c(1,2,3,4,5,6),
                   labels=c("","HS","","","Bach.","Grad.")) 
c

table(SpritzMain$readTM)
table(SpritzMain$readBook)
table(SpritzMain$readSci)
table(SpritzMain$readMag)
table(SpritzMain$readProf)
table(SpritzMain$SpritzExp)

# For each subject, so use half of the observations
sqldf("select firstNormal, firstFun, count(*) from SpritzMain2 where FS='FUN' group by firstNormal, firstFun")
sqldf("select firstFun, count(*) from SpritzMain2 where FS='FUN' 
              and firstNormal = 1 ")

#Univariate statistics
stat.desc(SpritzMain2$Y)
stat.desc(SpritzMain2$trt)
t.test(Y ~ FS, data = SpritzMain2)
t.test(Y ~ firstNormal, data = SpritzMain2)
t.test(Y ~ firstNormal, data = SpritzMain2)
t.test(Y ~ trt, data = SpritzMain2)

sqldf("select FS, TRT, avg(Y) from SpritzMain2 group by FS, TRT")


ols(Y ~ trt,SpritzMain2,cluster="Code")
summary(lm(Y ~ trt,SpritzMain2))

# There doesn't seem to be an effect of whether the first article was normal or Spritz
summary(lm(Y ~ firstNormal,SpritzMain2))

# People have done worse with SAD article overall
summary(lm(SpritzMain2$Y ~ SpritzMain2$isSAD))

# 1: Basic effects
summary(lm(Y ~ trt,SpritzMain2))
ols(Y ~ trt,SpritzMain2,cluster="Code")

summary(lm(Y ~ isSAD, SpritzMain2))
ols(Y ~ isSAD,SpritzMain2,cluster="Code")

summary(lm(Y ~ firstNormal, SpritzMain2))
ols(Y ~ firstNormal,SpritzMain2,cluster="Code")

# 2: Basic effects plus interactions
summary(lm(Y ~ trt*isSAD, SpritzMain2))
ols(Y ~ trt*isSAD,SpritzMain2,cluster="Code")
summary(lm(Y ~ trt*firstNormal, SpritzMain2))
ols(Y ~ trt*firstNormal,SpritzMain2,cluster="Code")
summary(lm(Y ~ trt*firstNormal*isSAD, SpritzMain2))
ols(Y ~ trt*firstNormal*isSAD,SpritzMain2,cluster="Code")

#3: All variables
summary(lm(Y ~ trt*isSAD*firstNormal 
           + Age 
           + SpritzExp + UsesGlasses + DegreeCode
           + PrimEng + Female + RaceCode 
           + readTM + readBook + readSci 
           + readMag + readProf, SpritzMain2))

ols(Y ~ trt*isSAD*firstNormal + Age + SpritzExp + UsesGlasses + DegreeCode
    + PrimEng + Female + RaceCode + readTM + readBook + readSci 
    + readMag + readProf,SpritzMain2,cluster="Code")


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


#Summaries for subgroups
summary(SpritzMain2$Y[SpritzMain2$FS == "FUN" & SpritzMain2$trt == 1]) 
summary(SpritzMain2$Y[SpritzMain2$FS == "FUN" & SpritzMain2$trt == 0]) 
summary(SpritzMain2$Y[SpritzMain2$FS == "SAD" & SpritzMain2$trt == 1]) 
summary(SpritzMain2$Y[SpritzMain2$FS == "SAD" & SpritzMain2$trt == 0]) 
means <- aggregate(SpritzMain2$Y, by=list(SpritzMain2$FS, SpritzMain2$trt), FUN=mean)
table(SpritzMain2$FSTRT)
SpritzMain2$FSTRT <- ifelse(SpritzMain2$trt == 1, paste(SpritzMain2$FS, "SPR"), paste(SpritzMain2$FS, "NOR"))
ggplot(data=SpritzMain2, aes(x=FSTRT, y=Y, fill=FSTRT)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=4, show_guide = FALSE) + 
  stat_summary(fun.y=mean, colour="red", geom="text", show_guide = FALSE, vjust=-1, aes(label=round(..y.., digits=1)))+
  labs( title = "A summary of the experiment results", x="Article and type (and mean correct answer)", y="Correct answers")+
  theme(panel.grid.major.x  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        panel.grid.major.y  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=8), 
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")+
  scale_fill_manual(values=(c('gold', 'gold3','cyan','cyan3')))



###############################
# Analysis for WPM Outcome
###############################

#Univariate statistics
stat.desc(SpritzMain2$Y_timeInSecs)
t.test(Y_timeInSecs ~ FS, data = SpritzMain2)
t.test(Y_timeInSecs ~ firstNormal, data = SpritzMain2)
t.test(Y_timeInSecs ~ trt, data = SpritzMain2)

sqldf("select FS, TRT, avg(Y_timeInSecs) from SpritzMain2 group by FS, TRT")


ols(Y_timeInSecs ~ trt,SpritzMain2,cluster="Code")
summary(lm(Y_timeInSecs ~ trt,SpritzMain2))

# There doesn't seem to be an effect of whether the first article was normal or Spritz
summary(lm(Y_timeInSecs ~ firstNormal,SpritzMain2))

# People have done worse with SAD article overall
summary(lm(SpritzMain2$Y_timeInSecs ~ SpritzMain2$isSAD))

cl <- function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

# 1: Basic effects
model1_time <- lm(Y_timeInSecs ~ trt,SpritzMain2)
cl(model1_time,SpritzMain2$Code)

model2_time <- lm(Y_timeInSecs ~ isSAD, SpritzMain2)
cl(model2_time,SpritzMain2$Code)

model3_time <- lm(Y_timeInSecs ~ firstNormal, SpritzMain2)
cl(model3_time,SpritzMain2$Code)

# 2: Basic effects plus interactions
model4_time <- lm(Y_timeInSecs ~ trt*isSAD, SpritzMain2)
cl(model4_time,SpritzMain2$Code)

model5_time <- lm(Y_timeInSecs ~ trt*firstNormal, SpritzMain2)
cl(model5_time,SpritzMain2$Code)

model6_time <- lm(Y_timeInSecs ~ trt*firstNormal*isSAD, SpritzMain2)
cl(model6_time,SpritzMain2$Code)

#3: All variables
model7_time <- lm(Y_timeInSecs ~ trt*isSAD*firstNormal 
               + Age 
               + SpritzExp + UsesGlasses + DegreeCode
               + PrimEng + Female + RaceCode 
               + readTM + readBook + readSci 
               + readMag + readProf, SpritzMain2)

cl(model7_time,SpritzMain2$Code)



