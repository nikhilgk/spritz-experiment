# This program reads Spritz survey responses in CSV fomat,
# then recodes all the answers into variables that can then be analyzed.
# Input file is Resp[onse2.csv for this version

SpritzAll <- read.csv("~/Desktop/Responses2.csv")

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


names(Spritz)
oldSpritzMain <- SpritzMain
# This dataset has all the coded variables
SpritzMain <- subset(Spritz,select=-c(FUN_Q1,FUN_Q2,FUN_Q3,FUN_Q4,FUN_Q5,          
                                      SAD_Q1,SAD_Q2,SAD_Q3,SAD_Q4,SAD_Q5,          
                                      English,Gender,Race,Degree,Reading,
                                      Glasses,SpritzAns,Comments))
SpritzMain$FUN <- SpritzMain$FUN_ANS1 + SpritzMain$FUN_ANS2 + SpritzMain$FUN_ANS3 + SpritzMain$FUN_ANS4 + SpritzMain$FUN_ANS5
SpritzMain$SAD <- SpritzMain$SAD_ANS1 + SpritzMain$SAD_ANS2 + SpritzMain$SAD_ANS3 + SpritzMain$SAD_ANS4 + SpritzMain$SAD_ANS5
SpritzMain <- na.omit(SpritzMain)
# 12/14: exclude the observations where the metadata parses as NA

library("sqldf", lib.loc="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")
a1 <- sqldf("select a.*, a.FUN as Y, 0 as trt from SpritzMain a where firstNormal = 1 and firstFUN =1")
a1$FS <- 'FUN'
a2 <- sqldf("select a.*, a.SAD as Y, 1 as trt from SpritzMain a where firstNormal = 1 and firstFUN =1")
a2$FS <- 'SAD'

a3 <- sqldf("select a.*, a.FUN as Y, 1 as trt from SpritzMain a where firstNormal = 0 and firstFUN =1")
a3$FS <- 'FUN'
a4 <- sqldf("select a.*, a.SAD as Y, 0 as trt from SpritzMain a where firstNormal = 0 and firstFUN =1")
a4$FS <- 'SAD'

a5 <- sqldf("select a.*, a.SAD as Y, 0 as trt from SpritzMain a where firstNormal = 1 and firstFUN =0")
a5$FS <- 'SAD'
a6 <- sqldf("select a.*, a.FUN as Y, 1 as trt from SpritzMain a where firstNormal = 1 and firstFUN =0")
a6$FS <- 'FUN'

a7 <- sqldf("select a.*, a.SAD as Y, 1 as trt from SpritzMain a where firstNormal = 0 and firstFUN =0")
a7$FS <- 'SAD'
a8 <- sqldf("select a.*, a.FUN as Y, 0 as trt from SpritzMain a where firstNormal = 0 and firstFUN =0")
a8$FS <- 'FUN'

b1 <- rbind (a1, a2)
b2 <- rbind (a3, a4)
b3 <- rbind (a5, a6)
b4 <- rbind (a7, a8)

c1 <- rbind (b1, b2)
c2 <- rbind (b3, b4)

# SpritzMain2 has two observations for each subject: one where trt = 0 and one where trt =1

SpritzMain2 <- rbind(c1,c2) 
rm(a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,c1,c2)
write.csv(SpritzMain2, "SpritzMain2.csv")

