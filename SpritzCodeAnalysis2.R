# Spritz experiment data analysis
# Uses SpritzMain2 table: each subject has two observations
options(scipen=100)
options(digits=4)
library("pastecs", lib.loc="~/R/win-library/2.15")
table(SpritzMain2$FS)
sqldf("select FS, TRT, avg(Y) from SpritzMain2 group by FS, TRT")

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


  scale_fill_discrete(name="",
                      breaks=c("0", "1"),
                      labels=c("Normal", "Spritz"))
  guides(fill=guide_legend(title=NULL))+
  scale_y_discrete(breaks = c(0,1,2,3,4,5), 
                   labels = c("","1","","3","","5")) 
g



SpritzMain2$isFUN <- ifelse(SpritzMain2$FS == 'FUN', 1, 0)
stat.desc(SpritzMain2$Y)
stat.desc(SpritzMain2$trt)
summary(lm(SpritzMain2$Y ~ SpritzMain2$trt + SpritzMain2$firstNormal + SpritzMain2$isFUN))
       
summary(lm(SpritzMain2$Y ~ SpritzMain2$trt + SpritzMain2$firstNormal 
           + SpritzMain2$SpritzExp + SpritzMain2$UsesGlasses + SpritzMain2$DegreeCode
           + SpritzMain2$PrimEng + SpritzMain2$Female + SpritzMain2$RaceCode))

