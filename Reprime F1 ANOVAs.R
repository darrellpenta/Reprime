rm(list = ls()) # clears environment
library(languageR) # calls languageR library

#
# -----------------------------------PREPARE DATA FILE FOR ANALYSES---------------------------------
#
f1errout <- read.table("data/SR_F1_errordata.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file

d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)  

#aggregates d with dysfluencies 
data.subj <- aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) 

colnames(data.subj) <- c("subj", "semint", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
integ <- subset(data.subj, semint   ==  "integ") 
unint <- subset(data.subj, semint   ==  "unint")
relat <- subset(data.subj, related  ==  "rel") 
unrel <- subset(data.subj, related  ==  "unrel") 
sing  <- subset(data.subj, n2num    ==  "sing") 
plur  <- subset(data.subj, n2num    ==  "plur")

#Below, additional subsetted groups
relat.int.plur   <- subset(data.subj, related == "rel"   & semint  == "integ" & n2num == "plur") 
relat.int.sing   <- subset(data.subj, related == "rel"   & semint  == "integ" & n2num == "sing") 
relat.unint.plur <- subset(data.subj, related == "rel"   & semint  == "unint" & n2num == "plur") 
relat.unint.sing <- subset(data.subj, related == "rel"   & semint  == "unint" & n2num == "sing")
unrel.int.plur   <- subset(data.subj, related == "unrel" & semint  == "integ" & n2num == "plur")
unrel.int.sing   <- subset(data.subj, related == "unrel" & semint  == "integ" & n2num == "sing")
unrel.unint.plur <- subset(data.subj, related == "unrel" & semint  == "unint" & n2num == "plur")
unrel.unint.sing <- subset(data.subj, related == "unrel" & semint  == "unint" & n2num == "sing") 
relat.plur       <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing       <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur       <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing       <- subset(data.subj, related == "unrel" & n2num   == "sing")
integ.plur       <- subset(data.subj, semint  == "integ" & n2num   == "plur") 
integ.sing       <- subset(data.subj, semint  == "integ" & n2num   == "sing")
unint.plur       <- subset(data.subj, semint  == "unint" & n2num   == "plur")
unint.sing       <- subset(data.subj, semint  == "unint" & n2num   == "sing")
integ.relat      <- subset(data.subj, semint  == "integ" & related == "rel") 
integ.unrel      <- subset(data.subj, semint  == "integ" & related == "unrel")
unint.relat      <- subset(data.subj, semint  == "unint" & related == "rel") 
unint.unrel      <- subset(data.subj, semint  == "unint" & related == "unrel") 


ds <- data.frame(data = c(
  "gmean",
  "integ",
  "unint",
  "relat",
  "unrel",
  "integrel",
  "integunrel",
  "unintrel",
  "unintunrel",
  "plur",
  "sing",
  "intplur",
  "intsing",
  "unintplur",
  "unintsing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing",
  "relintplur",
  "relintsing",
  "relunintplur",
  "relunintsing",
  "unrelintplur",
  "unrelintsing",
  "unrelunintplur",
  "unrelunintsing"),
  
  n = c(length(data.subj$error),
        length(integ$error),
        length(unint$error),
        length(relat$error),
        length(unrel$error),
        length(integ.relat$error),
        length(integ.unrel$error),
        length(unint.relat$error),
        length(unint.unrel$error),
        length(plur$error),
        length(sing$error),
        length(integ.plur$error),
        length(integ.sing$error),
        length(unint.plur$error),
        length(unint.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error),
        length(relat.int.plur$error),
        length(relat.int.sing$error),
        length(relat.unint.plur$error),
        length(relat.unint.sing$error),
        length(unrel.int.plur$error),
        length(unrel.int.sing$error),
        length(unrel.unint.plur$error),
        length(unrel.unint.sing$error)),
  
  N = c(length(data.subj$error),
        length(integ$error),
        length(unint$error),
        length(relat$error),
        length(unrel$error),
        length(integ.relat$error),
        length(integ.unrel$error),
        length(unint.relat$error),
        length(unint.unrel$error),
        length(plur$error),
        length(sing$error),
        length(integ.plur$error),
        length(integ.sing$error),
        length(unint.plur$error),
        length(unint.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error),
        length(relat.int.plur$error),
        length(relat.int.sing$error),
        length(relat.unint.plur$error),
        length(relat.unint.sing$error),
        length(unrel.int.plur$error),
        length(unrel.int.sing$error),
        length(unrel.unint.plur$error),
        length(unrel.unint.sing$error)),
  
  mean = c(mean(data.subj$error),
           mean(integ$error),
           mean(unint$error),
           mean(relat$error),
           mean(unrel$error),
           mean(integ.relat$error),
           mean(integ.unrel$error),
           mean(unint.relat$error),
           mean(unint.unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(integ.plur$error),
           mean(integ.sing$error),
           mean(unint.plur$error),
           mean(unint.sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error),
           mean(relat.int.plur$error),
           mean(relat.int.sing$error),
           mean(relat.unint.plur$error),
           mean(relat.unint.sing$error),
           mean(unrel.int.plur$error),
           mean(unrel.int.sing$error),
           mean(unrel.unint.plur$error),
           mean(unrel.unint.sing$error)),
  
  sd = c(sd(data.subj$error),
         sd(integ$error),
         sd(unint$error),
         sd(relat$error),
         sd(unrel$error),
         sd(integ.relat$error),
         sd(integ.unrel$error),
         sd(unint.relat$error),
         sd(unint.unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(integ.plur$error),
         sd(integ.sing$error),
         sd(unint.plur$error),
         sd(unint.sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error),
         sd(relat.int.plur$error),
         sd(relat.int.sing$error),
         sd(relat.unint.plur$error),
         sd(relat.unint.sing$error),
         sd(unrel.int.plur$error),
         sd(unrel.int.sing$error),
         sd(unrel.unint.plur$error),
         sd(unrel.unint.sing$error)),
  
  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(integ$error) / sqrt(length(integ$error)),
         sd(unint$error) / sqrt(length(unint$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(integ.relat$error) / sqrt(length(integ.relat$error)),
         sd(integ.unrel$error) / sqrt(length(integ.unrel$error)),
         sd(unint.relat$error) / sqrt(length(unint.relat$error)),
         sd(unint.unrel$error) / sqrt(length(unint.unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(integ.plur$error) / sqrt(length(integ.plur$error)),
         sd(integ.sing$error) / sqrt(length(integ.sing$error)),
         sd(unint.plur$error) / sqrt(length(unint.plur$error)),
         sd(unint.sing$error) / sqrt(length(unint.sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error) ),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error)),
         sd(relat.int.plur$error) / sqrt(length(relat.int.plur$error)),
         sd(relat.int.sing$error) / sqrt(length(relat.int.sing$error)),
         sd(relat.unint.plur$error) / sqrt(length(relat.unint.plur$error)),
         sd(relat.unint.sing$error) / sqrt(length(relat.unint.sing$error)),
         sd(unrel.int.plur$error) / sqrt(length(unrel.int.plur$error)),
         sd(unrel.int.sing$error) / sqrt(length(unrel.int.sing$error)),
         sd(unrel.unint.plur$error) / sqrt(length(unrel.unint.plur$error)),
         sd(unrel.unint.sing$error) / sqrt(length(unrel.unint.sing$error))
  ))





#
# --------------------------------2 X 2 X 2 ANOVA-----------------------------------------------------

sink("output/SemRel F1 Factor Analyses.txt")

cat(" ", "\n")
cat("BY-SUBJECTS FACTOR ANALYSES RUN ON:", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2X2 ANOVA: SEMREL", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) # prints descrip stats for 2x2x2 ANOVA
cat(" ", "\n")

# Computes the anova
a.2x2x2 <- aov(error ~ semint * related * n2num + Error(subj / (semint * related * n2num)), data = data.subj)
print(summary(a.2x2x2)) 
cat(" ", "\n")
cat(" ", "\n")



#
#========================================================================================================
#
#------------------------------------RELATED - UNRELATED ITEMS PAIRED COMPARISONS--------------------------------------
#

f1errout <- read.table("data/SR_F1_errrel.txt", header = T) #reads in Related data, ignoring integration
d <- f1errout 
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd  == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100) 
data.subj <- aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) 
colnames(data.subj) <- c("subj", "semint", "related", "n2num", "error") 

relat      <- subset(data.subj, related == "rel") 
unrel      <- subset(data.subj, related == "unrel") 
sing       <- subset(data.subj, n2num   == "sing") 
plur       <- subset(data.subj, n2num   == "plur")
relat.plur <- subset(data.subj, related == "rel"   & n2num == "plur") 
relat.sing <- subset(data.subj, related == "rel"   & n2num == "sing")
unrel.plur <- subset(data.subj, related == "unrel" & n2num == "plur")
unrel.sing <- subset(data.subj, related == "unrel" & n2num == "sing")

ds <- data.frame(data = c(
  "gmean",
  "relat",
  "unrel",
  "plur",
  "sing",
  "relatplur",
  "relatsing",
  "unrelplur",
  "unrelsing"
),

n = c(length(data.subj$error),
      length(relat$error),
      length(unrel$error),
      length(plur$error),
      length(sing$error),
      length(relat.plur$error),
      length(relat.sing$error),
      length(unrel.plur$error),
      length(unrel.sing$error)
),

N = c(length(data.subj$error),
      length(relat$error),
      length(unrel$error),
      length(plur$error),
      length(sing$error),
      length(relat.plur$error),
      length(relat.sing$error),
      length(unrel.plur$error),
      length(unrel.sing$error)
),

mean = c(mean(data.subj$error),
         mean(relat$error),
         mean(unrel$error),
         mean(plur$error),
         mean(sing$error),
         mean(relat.plur$error),
         mean(relat.sing$error),
         mean(unrel.plur$error),
         mean(unrel.sing$error)
),

sd = c(sd(data.subj$error),
       sd(relat$error),
       sd(unrel$error),
       sd(plur$error),
       sd(sing$error),
       sd(relat.plur$error),
       sd(relat.sing$error),
       sd(unrel.plur$error),
       sd(unrel.sing$error)
),

se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
       sd(relat$error) / sqrt(length(relat$error)),
       sd(unrel$error) / sqrt(length(unrel$error)),
       sd(plur$error) / sqrt(length(plur$error)),
       sd(sing$error) / sqrt(length(sing$error)),
       sd(relat.plur$error) / sqrt(length(relat.plur$error)),
       sd(relat.sing$error) / sqrt(length(relat.sing$error)),
       sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
       sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
       
))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("RELATED ITEMS ANALYSES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) 
cat(" ", "\n")

a.2x2 <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj) 
print(summary(a.2x2)) 
cat(" ", "\n")
cat(" ", "\n")

# -------------------------RELATED ITEMS ANALYSES------------------------------

ds.relat <- data.frame(data = c("n2num","plur","sing"),
                       
                       n = c(length(relat$error),
                             length(relat.plur$error),
                             length(relat.sing$error)),
                       
                       N = c(length(relat$error),
                             length(relat.plur$error),
                             length(relat.sing$error)),
                       
                       mean = c(mean(relat$error),
                                mean(relat.plur$error),
                                mean(relat.sing$error)),
                       
                       sd = c(sd(relat$error),
                              sd(relat.plur$error),
                              sd(relat.sing$error)),
                       
                       se = c(sd(relat$error) / sqrt(length(relat$error)),
                              sd(relat.plur$error) / sqrt(length(relat.plur$error)),
                              sd(relat.sing$error) / sqrt(length(relat.sing$error))))


cat(">>>  RELATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.relat) 
cat(" ", "\n")

a.relat <- aov(error ~ n2num + Error(subj / n2num), data = relat) 
print(summary(a.relat)) 
cat(" ", "\n")
cat(" ", "\n")
#------------------------------UNRELATED PAIRED COMPARISION----------------
ds.unrel <- data.frame(data = c("n2num", "plur", "sing"),
                       
                       n = c(length(unrel$error),
                             length(unrel.plur$error),
                             length(unrel.sing$error)),
                       
                       N = c(length(unrel$error),
                             length(unrel.plur$error),
                             length(unrel.sing$error)),
                       
                       mean = c(mean(unrel$error),
                                mean(unrel.plur$error),
                                mean(unrel.sing$error)),
                       
                       sd = c(sd(unrel$error),
                              sd(unrel.plur$error),
                              sd(unrel.sing$error)),
                       
                       se = c(sd(unrel$error) / sqrt(length(unrel$error)),
                              sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
                              sd(unrel.sing$error) / sqrt(length(unrel.sing$error)))) 

cat(">>>  UNRELATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.unrel) 
cat(" ", "\n")

a.unrel <- aov(error ~ n2num + Error(subj / n2num), data = unrel) 
print(summary(a.unrel))
cat(" ", "\n")
cat(" ", "\n")


#==========================================================================================================
#
# ----------------------------INTEGRATED - UNINTEGRATED PAIRED COMPARISONS --------------------------------
#

f1errout <- read.table("data/SR_F1_errint.txt", header = T)  # reads in data 
d <- f1errout 
d$subj <- as.factor(d$subj)  
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100) 
data.subj <- aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) 
colnames(data.subj) <- c("subj", "semint", "related", "n2num", "error") 

integ      <- subset(data.subj, semint == "integ") 
unint      <- subset(data.subj, semint == "unint") 
sing       <- subset(data.subj, n2num  == "sing") 
plur       <- subset(data.subj, n2num  == "plur") 
integ.plur <- subset(data.subj, semint == "integ" & n2num == "plur") 
integ.sing <- subset(data.subj, semint == "integ" & n2num == "sing")
unint.plur <- subset(data.subj, semint == "unint" & n2num == "plur")
unint.sing <- subset(data.subj, semint == "unint" & n2num == "sing")

ds <- data.frame(data = c(
  "gmean",
  "integ",
  "unint",
  "plur",
  "sing",
  "intplur",
  "intsing",
  "unintplur",
  "unintsing"
),

n = c(length(data.subj$error),
      length(integ$error),
      length(unint$error),
      length(plur$error),
      length(sing$error),
      length(integ.plur$error),
      length(integ.sing$error),
      length(unint.plur$error),
      length(unint.sing$error)
),

N = c(length(data.subj$error),
      length(integ$error),
      length(unint$error),
      length(plur$error),  
      length(sing$error),
      length(integ.plur$error),  
      length(integ.sing$error),
      length(unint.plur$error),
      length(unint.sing$error)
),

mean = c(mean(data.subj$error),
         mean(integ$error),
         mean(unint$error),
         mean(plur$error),
         mean(sing$error),
         mean(integ.plur$error),
         mean(integ.sing$error),
         mean(unint.plur$error),
         mean(unint.sing$error)
),

sd = c(sd(data.subj$error),
       sd(integ$error),
       sd(unint$error),
       sd(plur$error),
       sd(sing$error),
       sd(integ.plur$error),
       sd(integ.sing$error),
       sd(unint.plur$error),
       sd(unint.sing$error)
),

se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
       sd(integ$error) / sqrt(length(integ$error)),
       sd(unint$error) / sqrt(length(unint$error)),
       sd(plur$error) / sqrt(length(plur$error)),
       sd(sing$error) / sqrt(length(sing$error)),
       sd(integ.plur$error) / sqrt(length(integ.plur$error)),
       sd(integ.sing$error) / sqrt(length(integ.sing$error)),
       sd(unint.plur$error) / sqrt(length(unint.plur$error)),
       sd(unint.sing$error) / sqrt(length(unint.sing$error)) 
       
))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("INTEGRATED ITEMS ANALYSES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)  
cat(" ", "\n")

a.2x2 <- aov(error ~ semint * n2num + Error(subj / (semint * n2num)), data = data.subj)  # 2x2 anova
print(summary(a.2x2)) 
cat(" ", "\n")
cat(" ", "\n")

#
# -----------------------INTEGRATED PAIRED COMPARISONS------------------------------------------------------
#

ds.integ <- data.frame(data = c("n2num", "plur", "sing"),
                       
                       n = c(length(integ$error),
                             length(integ.plur$error),
                             length(integ.sing$error)),
                       
                       N = c(length(integ$error),
                             length(integ.plur$error),
                             length(integ.sing$error)),
                       
                       mean = c(mean(integ$error),
                                mean(integ.plur$error),
                                mean(integ.sing$error)),
                       
                       sd = c(sd(integ$error),
                              sd(integ.plur$error),
                              sd(integ.sing$error)),
                       
                       se = c(sd(integ$error) / sqrt(length(integ$error)),
                              sd(integ.plur$error) / sqrt(length(integ.plur$error)),
                              sd(integ.sing$error) / sqrt(length(integ.sing$error)))) 

cat(">>>  INTEGRATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.integ) 
cat(" ", "\n")


a.integ <- aov(error ~ n2num + Error(subj / n2num), data = integ) 
print(summary(a.integ)) 
cat(" ", "\n")
cat(" ", "\n")
#
#----------------------UNINTEGRATED PAIRED COMPARISONS--------------------------------------------------
#

ds.uninteg <- data.frame(data = c("n2num", "plur", "sing"),
                         
                         n = c(length(unint$error),
                               length(unint.plur$error),
                               length(unint.sing$error)),
                         
                         N = c(length(unint$error),
                               length(unint.plur$error),
                               length(unint.sing$error)),
                         
                         mean = c(mean(unint$error),
                                  mean(unint.plur$error),
                                  mean(unint.sing$error)),
                         
                         sd = c(sd(unint$error),
                                sd(unint.plur$error),
                                sd(unint.sing$error)),
                         
                         se = c(sd(unint$error) / sqrt(length(unint$error)),
                                sd(unint.plur$error) / sqrt(length(unint.plur$error)),
                                sd(unint.sing$error) / sqrt(length(unint.sing$error)))) 

cat(">>>  UNINTEGRATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.uninteg) 
cat(" ", "\n")

a.uninteg <- aov(error ~ n2num + Error(subj / n2num), data = unint)
print(summary(a.uninteg)) 
cat(" ", "\n")
cat(" ", "\n")


# ---------------------------------------Paired comparisions for each condition------------------
#

f1errout <- read.table("data/SR_F1_errordata.txt", header = T) 
d <- f1errout 
d$subj <- as.factor(d$subj) 
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord))*100) 
data.subj <- aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) 
colnames(data.subj) <- c("subj", "semint", "related", "n2num", "error")

# ----------------Integrated Related paired----------------------
integ.relat    <- subset(data.subj, semint  == "integ" & related == "rel") 
relat.int.plur <- subset(data.subj, related == "rel"   & semint  == "integ" & n2num == "plur") 
relat.int.sing <- subset(data.subj, related == "rel"   & semint  == "integ" & n2num == "sing") 

ds.integrel <- data.frame(data = c("n2num","plur","sing"),
                          
                          n = c(length(integ.relat$error),
                                length(relat.int.plur$error),
                                length(relat.int.sing$error)),
                          
                          N = c(length(integ.relat$error),
                                length(relat.int.plur$error),
                                length(relat.int.sing$error)),
                          
                          mean = c(mean(integ.relat$error),
                                   mean(relat.int.plur$error),
                                   mean(relat.int.sing$error)),
                          
                          sd = c(sd(integ.relat$error),
                                 sd(relat.int.plur$error),
                                 sd(relat.int.sing$error)),
                          
                          se = c(sd(integ.relat$error) / sqrt(length(integ.relat$error)),
                                 sd(relat.int.plur$error) / sqrt(length(relat.int.plur$error)),
                                 sd(relat.int.sing$error) / sqrt(length(relat.int.sing$error)))) 

cat(rep(c("-"), times=40, quote=F),"\n")
cat("VARIOUS PAIRED COMBINATIONS", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
cat(" ", "\n")

cat(">>>  INTEGRATED -RELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.integrel) 
print(rep(c("-"), times = 50), quote = F)
cat(" ", "\n")

a.integrel <- aov(error ~ n2num + Error(subj / n2num), data = integ.relat) 
print(summary(a.integrel))

#-------------------Integrated Unrelated paired--------------------
integ.unrel    <- subset(data.subj, semint  == "integ" & related == "unrel") 
unrel.int.plur <- subset(data.subj, related == "unrel" & semint  == "integ" & n2num == "plur") 
unrel.int.sing <- subset(data.subj, related == "unrel" & semint  == "integ" & n2num == "sing") 

ds.integunrel <- data.frame(data = c("n2num", "plur", "sing"),
                            
                            n = c(length(integ.unrel$error),
                                  length(unrel.int.plur$error),
                                  length(unrel.int.sing$error)),
                            
                            N = c(length(integ.unrel$error),
                                  length(unrel.int.plur$error),
                                  length(unrel.int.sing$error)),
                            
                            mean = c(mean(integ.unrel$error),
                                     mean(unrel.int.plur$error),
                                     mean(unrel.int.sing$error)),
                            
                            sd = c(sd(integ.unrel$error),
                                   sd(unrel.int.plur$error),
                                   sd(unrel.int.sing$error)),
                            
                            se = c(sd(integ.unrel$error) / sqrt(length(integ.unrel$error)),
                                   sd(unrel.int.plur$error) / sqrt(length(unrel.int.plur$error)),
                                   sd(unrel.int.sing$error) / sqrt(length(unrel.int.sing$error)))) 

cat(">>>  INTEGRATED -UNRELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.integunrel)
cat(" ", "\n")

a.integunrel <- aov(error ~ n2num + Error(subj / n2num), data = integ.unrel) 
print(summary(a.integunrel)) 
cat(" ", "\n")
cat(" ", "\n")

# ----------------------Uninegrated Related paired-------------------------
unint.relat      <- subset(data.subj, semint  == "unint" & related == "rel") 
relat.unint.plur <- subset(data.subj, related == "rel"   & semint  == "unint" & n2num == "plur") 
relat.unint.sing <- subset(data.subj, related == "rel"   & semint  == "unint" & n2num == "sing") 

ds.unintrel <- data.frame(data = c("n2num", "plur", "sing"),
                          
                          n = c(length(unint.relat$error),
                                length(relat.unint.plur$error),
                                length(relat.unint.sing$error)),
                          
                          N = c(length(unint.relat$error),
                                length(relat.unint.plur$error),
                                length(relat.unint.sing$error)),
                          
                          mean = c(mean(unint.relat$error),
                                   mean(relat.unint.plur$error),
                                   mean(relat.unint.sing$error)),
                          
                          sd = c(sd(unint.relat$error),
                                 sd(relat.unint.plur$error),
                                 sd(relat.unint.sing$error)),
                          
                          se = c(sd(unint.relat$error) / sqrt(length(unint.relat$error)),
                                 sd(relat.unint.plur$error) / sqrt(length(relat.unint.plur$error)),
                                 sd(relat.unint.sing$error) / sqrt(length(relat.unint.sing$error)))) 

cat(">>>  UNINTEGRATED - RELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.unintrel) 
cat(" ", "\n")
a.unintrel <- aov(error ~ n2num + Error(subj / n2num), data = unint.relat) 
print(summary(a.unintrel)) 
cat(" ", "\n")
cat(" ", "\n")

# -------------------------Unintegrated Unrelated comparisons-------------------
unint.unrel      <- subset(data.subj, semint  == "unint" & related == "unrel") 
unrel.unint.plur <- subset(data.subj, related == "unrel" & semint  == "unint" & n2num == "plur")
unrel.unint.sing <- subset(data.subj, related == "unrel" & semint  == "unint" & n2num == "sing")

ds.unintunrel <- data.frame(data=c("n2num","plur","sing"),
                            
                            n = c(length(unint.unrel$error),
                                  length(unrel.unint.plur$error),
                                  length(unrel.unint.sing$error)),
                            
                            N = c(length(unint.unrel$error),
                                  length(unrel.unint.plur$error),
                                  length(unrel.unint.sing$error)),
                            
                            mean = c(mean(unint.unrel$error),
                                     mean(unrel.unint.plur$error),
                                     mean(unrel.unint.sing$error)),
                            
                            sd = c(sd(unint.unrel$error),
                                   sd(unrel.unint.plur$error),
                                   sd(unrel.unint.sing$error)),
                            
                            se = c(sd(unint.unrel$error) / sqrt(length(unint.unrel$error)),
                                   sd(unrel.unint.plur$error) / sqrt(length(unrel.unint.plur$error)),
                                   sd(unrel.unint.sing$error) / sqrt(length(unrel.unint.sing$error))))

cat(">>>  UNINTEGRATED - UNRELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.unintunrel) 
cat(" ", "\n")

a.unintunrel <- aov(error ~ n2num + Error(subj / n2num), data = unint.unrel) 
print(summary(a.unintunrel)) 
cat(" ", "\n")
cat(" ", "\n")




sink()