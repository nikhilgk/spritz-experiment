# Spritz experiment data analysis
# Uses SpritzMain2 table: each subject has two observations
options(scipen=100)
options(digits=4)
library("pastecs", lib.loc="~/R/win-library/2.15")
library("sqldf", lib.loc="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")
table(SpritzMain2$FS)

# For each subject, so use half of the observations
sqldf("select firstNormal, firstFun, count(*) from SpritzMain2 where FS='FUN' group by firstNormal, firstFun")
sqldf("select firstFun, count(*) from SpritzMain2 where FS='FUN' 
              and firstNormal = 1 ")

library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")
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
ols(Y ~ trt,SpritzMain2,cluster="Meta_code")

summary(lm(SpritzMain2$Y ~ SpritzMain2$trt * SpritzMain2$isSAD))
ols(Y ~ trt*isSAD,SpritzMain2,cluster="Meta_code")

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


