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

SpritzMain2$isSAD <- ifelse(SpritzMain2$FS == 'SAD', 1, 0)

# There doesn't seem to be an effect of whether the first article was normal or Spritz
summary(lm(SpritzMain2$Y ~ SpritzMain2$firstNormal))

# People have done worse with SAD article overall
summary(lm(SpritzMain2$Y ~ SpritzMain2$isSAD))

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt))
ols(Y ~ trt,SpritzMain2,cluster="Meta_code")

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt * SpritzMain2$isSAD))
ols(Y ~ trt*isSAD,SpritzMain2,cluster="Meta_code")

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt * SpritzMain2$isSAD + SpritzMain2$firstNormal))

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt + SpritzMain2$isSAD + SpritzMain2$firstNormal 
           + SpritzMain2$Age 
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
