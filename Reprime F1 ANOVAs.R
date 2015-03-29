rm(list = ls()) 
library(languageR) 
library(reshape2)
library(plyr)
library(stringr)
library(ggplot2) 
# CATEGORY ITEMS ==============================================================================================

f1vtimeout <- read.table("data/vtimeoutc.txt", header = F) 
colnames(f1vtimeout) <- c("subj", "item", "subexp", "headrel", "localrel", "vtime")
d.cat.base     <- subset(f1vtimeout, subexp == "Cat")
d.cat          <- subset(f1vtimeout, subexp == "Cat")

# CAT: Collapse over subject
d.cat$unique.id <- paste(d.cat[, 1], d.cat[, 3], d.cat[, 4], d.cat[, 5], sep = "_")
d.cat           <- ddply(d.cat, "unique.id", function(X) data.frame(vtime = mean(X$vtime)))
vtime           <- data.frame(d.cat$vtime)
d.cat           <- colsplit(d.cat$unique.id, "_", c("subj", "subexp", "headrel", "localrel"))
d.cat$subj      <- as.factor(d.cat$subj)
d.cat           <- cbind(d.cat,vtime)
colnames(d.cat)[5] <- "vtime"
rm(vtime)

# CAT: All subjects subsetting
HeadN.base         <- subset(d.cat.base, headrel   ==  "HeadN") 
UnrHead.base       <- subset(d.cat.base, headrel   ==  "UnrHead")
NRel.base          <- subset(d.cat.base, localrel  ==  "NRel") 
NUnr.base          <- subset(d.cat.base, localrel  ==  "NUnr") 
HeadN.NRel.base    <- subset(d.cat.base, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr.base    <- subset(d.cat.base, headrel   ==  "HeadN"   & localrel  ==  "NUnr") 
UnrHead.NRel.base  <- subset(d.cat.base, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr.base  <- subset(d.cat.base, headrel   ==  "UnrHead" & localrel  ==  "NUnr")

# CAT: Collapsed over subject sbsetting
HeadN         <- subset(d.cat, headrel   ==  "HeadN") 
UnrHead       <- subset(d.cat, headrel   ==  "UnrHead")
NRel          <- subset(d.cat, localrel  ==  "NRel") 
NUnr          <- subset(d.cat, localrel  ==  "NUnr") 
HeadN.NRel    <- subset(d.cat, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr    <- subset(d.cat, headrel   ==  "HeadN"   & localrel  ==  "NUnr") 
UnrHead.NRel  <- subset(d.cat, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr  <- subset(d.cat, headrel   ==  "UnrHead" & localrel  ==  "NUnr")


ds <- data.frame(data = c(
  "Grand Mean",
  "HeadN",
  "UnrHead",
  "NRel",
  "NUnr",
  "HeadN-NRel",
  "HeadN-NUnr",
  "UnrHead-NRel",
  "UnrHead-NUnr"),
  
  n = c(length(d.cat.base$vtime),
        length(HeadN.base$vtime), 
        length(UnrHead.base$vtime),
        length(NRel.base$vtime), 
        length(NUnr.base$vtime), 
        length(HeadN.NRel.base$vtime),
        length(HeadN.NUnr.base$vtime),
        length(UnrHead.NRel.base$vtime),
        length(UnrHead.NUnr.base$vtime)),
  
  N = c(length(d.cat$vtime),
        length(HeadN$vtime), 
        length(UnrHead$vtime),
        length(NRel$vtime), 
        length(NUnr$vtime), 
        length(HeadN.NRel$vtime),
        length(HeadN.NUnr$vtime),
        length(UnrHead.NRel$vtime),
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(d.cat$vtime),
           mean(HeadN$vtime), 
           mean(UnrHead$vtime),
           mean(NRel$vtime), 
           mean(NUnr$vtime), 
           mean(HeadN.NRel$vtime),
           mean(HeadN.NUnr$vtime),
           mean(UnrHead.NRel$vtime),
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(d.cat$vtime),
         sd(HeadN$vtime), 
         sd(UnrHead$vtime),
         sd(NRel$vtime), 
         sd(NUnr$vtime), 
         sd(HeadN.NRel$vtime),
         sd(HeadN.NUnr$vtime),
         sd(UnrHead.NRel$vtime),
         sd(UnrHead.NUnr$vtime)),
  
  se = c(sd(d.cat$vtime)       / sqrt(length(d.cat$vtime)),
         sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)), 
         sd(UnrHead$vtime)     / sqrt(length(UnrHead$vtime)),
         sd(NRel$vtime)        / sqrt(length(NRel$vtime)), 
         sd(NUnr$vtime)        / sqrt(length(NUnr$vtime)), 
         sd(HeadN.NRel$vtime)  / sqrt(length(HeadN.NRel$vtime)),
         sd(HeadN.NUnr$vtime)  / sqrt(length(HeadN.NUnr$vtime)),
         sd(UnrHead.NRel$vtime)/ sqrt(length(UnrHead.NRel$vtime)),
         sd(UnrHead.NUnr$vtime)/ sqrt(length(UnrHead.NUnr$vtime))
  ))


# CAT: All 2X2 ANOVA-----------------------------------------------------

sink("output/Reprime Category F1 Factorial Analyses.txt")

cat(" ", "\n")
cat("BY-SUBJECTS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: CATEGORY COORDINATES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) 
cat(" ", "\n")

a.2x2 <- aov(vtime ~ headrel * localrel + Error(subj / (headrel * localrel)), data = d.cat)
print(summary(a.2x2)) 
cat(" ", "\n")
cat(" ", "\n")



# CAT: Figure -----------------------------
cat.fig <- ds[6:9,c(1,4,6)]
dodge  <- position_dodge(width = 0.9)
g1     <- ggplot(data = cat.fig, aes(x = data, y = mean, fill=data)) +
  layer(geom="bar", stat="identity", position = position_dodge())+
  coord_cartesian(ylim = c(450, 500))+
geom_errorbar(aes(ymax = mean + se, ymin = mean - se), position = dodge, width = 0.2)
g1
ggsave(filename = "figures/Category Coordiates F1.png")

#   scale_fill_manual(values=c("#990000", "#CC6666", "#000099", "#9999CC")) +
#   guides(fill=FALSE)+
#  + 
#   +
#   scale_y_continuous(breaks=seq(0, 14, 2))+
#   annotate("text", x = 1:4, y = -1, label = rep(c("Integrated", "Unintegrated"), 2), size=6) +
#   annotate("text", c(1.5, 3.5), y = -2, label = c("Related", "Unrelated"), size=6) +
#   
#   theme_classic() +
#   theme(text = element_text(size=18.5)) +
#   ylab("Mismatch effect (%)") +
#   theme(axis.title.y=element_text(vjust=1.5)) +
#   theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), axis.title.x = element_blank(), axis.text.x = element_blank())



# CAT: RELATED HEAD-------------------------------------------------------------------------------

ds <- data.frame(data = c(
  "HNoun",
  "NRel",
  "NUnrel"),
  
  n = c(length(HeadN.base$vtime),
        length(HeadN.NRel.base$vtime), 
        length(HeadN.NUnr.base$vtime)),
  
  N = c(length(HeadN$vtime),
        length(HeadN.NRel$vtime), 
        length(HeadN.NUnr$vtime)),
  
  mean = c(mean(HeadN$vtime),
           mean(HeadN.NRel$vtime), 
           mean(HeadN.NUnr$vtime)),
  
  sd = c(sd(HeadN$vtime),
         sd(HeadN.NRel$vtime), 
         sd(HeadN.NUnr$vtime)),
  
  se = c(sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)),
         sd(HeadN.NRel$vtime)  / sqrt(length(HeadN.NRel$vtime)), 
         sd(HeadN.NUnr$vtime)  / sqrt(length(HeadN.NUnr$vtime))
  ))

cat("\n")
cat("RELATED HEAD: NRel vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds) 
cat(" ", "\n")

a.h.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = HeadN) 
print(summary(a.h.relunrel)) 
cat(" ", "\n")
cat(" ", "\n")


# CAT: UNRELATED HEAD  -------------------------------------------------------------------------------------------

ds <- data.frame(data = c(
  "UnrHead",
  "NRel",
  "NUnrel"),

  n = c(length(UnrHead.base$vtime),
        length(UnrHead.NRel.base$vtime), 
        length(UnrHead.NUnr.base$vtime)),
  
  N = c(length(HeadN$vtime),
        length(UnrHead.NRel$vtime), 
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(HeadN$vtime),
           mean(UnrHead.NRel$vtime), 
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(HeadN$vtime),
         sd(UnrHead.NRel$vtime), 
         sd(UnrHead.NUnr$vtime)),
  
  se = c(sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)),
         sd(UnrHead.NRel$vtime)  / sqrt(length(UnrHead.NRel$vtime)), 
         sd(UnrHead.NUnr$vtime)  / sqrt(length(UnrHead.NUnr$vtime))
  ))

cat("\n")
cat("UNRELATED HEAD: NRel vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds) 
cat(" ", "\n")

a.h.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = UnrHead) 
print(summary(a.h.relunrel)) 


sink()

# PROPERTY ITEMS ==========================================================================

rm(list = ls()) 
library(languageR) 
library(reshape2)
library(plyr)
library(stringr)

f1vtimeout <- read.table("data/vtimeoutc.txt", header = F) 
colnames(f1vtimeout) <- c("subj", "item", "subexp", "headrel", "localrel", "vtime")

d.prop         <- subset(f1vtimeout, subexp == "Attr")
d.prop.base    <- subset(f1vtimeout, subexp == "Attr")


# PROP: Collapse over subject
d.prop$unique.id <- paste(d.prop[, 1], d.prop[, 3], d.prop[, 4], d.prop[, 5], sep = "_")
d.prop           <- ddply(d.prop, "unique.id", function(X) data.frame(vtime = mean(X$vtime)))
vtime            <- data.frame(d.prop$vtime)
d.prop           <- colsplit(d.prop$unique.id, "_", c("subj", "subexp", "headrel", "localrel"))
d.prop$subj      <- as.factor(d.prop$subj)
d.prop           <- cbind(d.prop,vtime)
colnames(d.prop)[5] <- "vtime"
rm(vtime)

# PROP: All subjects subsetting
HeadN.base         <- subset(d.prop.base, headrel   ==  "HeadN") 
UnrHead.base       <- subset(d.prop.base, headrel   ==  "UnrHead")
NRel.base          <- subset(d.prop.base, localrel  ==  "NRel") 
NUnr.base          <- subset(d.prop.base, localrel  ==  "NUnr") 
NAssc.base         <- subset(d.prop.base, localrel  ==  "NAssc")
HeadN.NRel.base    <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr.base    <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NUnr")
HeadN.NAssc.base   <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NAssc")
UnrHead.NRel.base  <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr.base  <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NUnr")
UnrHead.NAssc.base <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NAssc")
# ----
HeadN.Rel.Un.base      <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NRel" & localrel  ==  "NUnr") 
UnrHead.Rel.Un.base    <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NRel" & localrel  ==  "NUnr")
HeadN.Ass.Un.base      <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NAssc" & localrel  ==  "NUnr") 
UnrHead.Ass.Un.base    <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NAssc" & localrel  ==  "NUnr")
HeadN.Rel.Ass.base      <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NAssc" & localrel  ==  "NRel") 
UnrHead.Rel.Ass.base    <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NAssc" & localrel  ==  "NRel")

# PROP: Collapsed subjects subsetting
HeadN         <- subset(d.prop, headrel   ==  "HeadN") 
UnrHead       <- subset(d.prop, headrel   ==  "UnrHead")
NRel          <- subset(d.prop, localrel  ==  "NRel") 
NUnr          <- subset(d.prop, localrel  ==  "NUnr") 
NAssc         <- subset(d.prop, localrel  ==  "NAssc")
HeadN.NRel    <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr    <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NUnr")
HeadN.NAssc   <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NAssc")
UnrHead.NRel  <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr  <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NUnr")
UnrHead.NAssc <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NAssc")
# ----
HeadN.Rel.Un      <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NRel" & localrel  ==  "NUnr") 
UnrHead.Rel.Un    <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NRel" & localrel  ==  "NUnr")
HeadN.Ass.Un      <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NAssc" & localrel  ==  "NUnr") 
UnrHead.Ass.Un    <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NAssc" & localrel  ==  "NUnr")
HeadN.Rel.Ass      <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NAssc" & localrel  ==  "NRel") 
UnrHead.Rel.Ass    <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NAssc" & localrel  ==  "NRel")

ds <- data.frame(data = c(
  "Grand Mean",
  "HeadN",
  "UnrHead",
  "NRel",
  "NAssc",
  "NUnr",
  "HeadN-NRel",
  "NeadN-NAssc",
  "HeadN-NUnr",
  "UnrHead-NRel",
  "UnrHead-NAssc",
  "UnrHead-NUnr"),
  
  n = c(length(d.prop.base$vtime),
        length(HeadN.base$vtime), 
        length(UnrHead.base$vtime),
        length(NRel.base$vtime), 
        length(NAssc.base$vtime), 
        length(NUnr.base$vtime), 
        length(HeadN.NRel.base$vtime),
        length(HeadN.NAssc.base$vtime),
        length(HeadN.NUnr.base$vtime),
        length(UnrHead.NRel.base$vtime),
        length(UnrHead.NAssc.base$vtime),
        length(UnrHead.NUnr.base$vtime)),
  
  N = c(length(d.prop$vtime),
        length(HeadN$vtime), 
        length(UnrHead$vtime),
        length(NRel$vtime), 
        length(NAssc$vtime), 
        length(NUnr$vtime), 
        length(HeadN.NRel$vtime),
        length(HeadN.NAssc$vtime),
        length(HeadN.NUnr$vtime),
        length(UnrHead.NRel$vtime),
        length(UnrHead.NAssc$vtime),
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(d.prop$vtime),
           mean(HeadN$vtime), 
           mean(UnrHead$vtime),
           mean(NRel$vtime), 
           mean(NAssc$vtime), 
           mean(NUnr$vtime), 
           mean(HeadN.NRel$vtime),
           mean(HeadN.NAssc$vtime),
           mean(HeadN.NUnr$vtime),
           mean(UnrHead.NRel$vtime),
           mean(UnrHead.NAssc$vtime),
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(d.prop$vtime),
         sd(HeadN$vtime), 
         sd(UnrHead$vtime),
         sd(NRel$vtime), 
         sd(NAssc$vtime), 
         sd(NUnr$vtime), 
         sd(HeadN.NRel$vtime),
         sd(HeadN.NAssc$vtime),
         sd(HeadN.NUnr$vtime),
         sd(UnrHead.NRel$vtime),
         sd(UnrHead.NAssc$vtime),
         sd(UnrHead.NUnr$vtime)),
  
  se = c(
    sd(d.prop$vtime)         / sqrt(length(d.prop$vtime)),
    sd(HeadN$vtime)         / sqrt(length(HeadN$vtime)), 
    sd(UnrHead$vtime)       / sqrt(length(UnrHead$vtime)),
    sd(NRel$vtime)          / sqrt(length(NRel$vtime)),
    sd(NAssc$vtime)         / sqrt(length(NAssc$vtime)), 
    sd(NUnr$vtime)          / sqrt(length(NUnr$vtime)), 
    sd(HeadN.NRel$vtime)    / sqrt(length(HeadN.NRel$vtime)),
    sd(HeadN.NAssc$vtime)   / sqrt(length(HeadN.NAssc$vtime)),
    sd(HeadN.NUnr$vtime)    / sqrt(length(HeadN.NUnr$vtime)),
    sd(UnrHead.NRel$vtime)  / sqrt(length(UnrHead.NRel$vtime)),
    sd(UnrHead.NAssc$vtime)  /sqrt(length(UnrHead.NAssc$vtime)),
     sd(UnrHead.NUnr$vtime)  / sqrt(length(UnrHead.NUnr$vtime))  
  ))

# PROP: All 2X3 ANOVA-----------------------------------------------------

sink("output/Reprime Property F1 Factorial Analyses.txt")

cat(" ", "\n")
cat("BY-SUBJECTS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X3 ANOVA: PROPERTY ITEMS", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) 
cat(" ", "\n")


a.2x2 <- aov(vtime ~ headrel * localrel + Error(subj / (headrel * localrel)), data = d.prop)
print(summary(a.2x2)) 
cat(" ", "\n")
cat(" ", "\n")



# PROP: Figure ----------------------------------------------------
prop.fig <- ds[7:12,c(1,4,6)]
dodge  <- position_dodge(width = 0.9)
g1     <- ggplot(data = prop.fig, aes(x = data, y = mean, fill=data)) +
  layer(geom="bar", stat="identity", position = position_dodge())+
  coord_cartesian(ylim = c(450, 525))+
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), position = dodge, width = 0.2)
g1
ggsave(filename = "figures/Property F1.png")

# PROP: (NRel vs. NUnrel) -------------------------------------------------------------------------------
rm(list = ls()) 
library(languageR) 
library(reshape2)
library(plyr)
library(stringr)

f1vtimeout <- read.table("data/vtimeoutc.txt", header = F) 
colnames(f1vtimeout) <- c("subj", "item", "subexp", "headrel", "localrel", "vtime")

d.prop         <- subset(f1vtimeout, subexp == "Attr")
d.prop.base    <- subset(f1vtimeout, subexp == "Attr")
d.prop         <- subset(d.prop, localrel != "NAssc")
d.prop.base    <- subset(d.prop, localrel != "NAssc")

# PROP (Rel vs. Unr): Collapse over subject
d.prop$unique.id <- paste(d.prop[, 1], d.prop[, 3], d.prop[, 4], d.prop[, 5], sep = "_")
d.prop           <- ddply(d.prop, "unique.id", function(X) data.frame(vtime = mean(X$vtime)))
vtime            <- data.frame(d.prop$vtime)
d.prop           <- colsplit(d.prop$unique.id, "_", c("subj", "subexp", "headrel", "localrel"))
d.prop$subj      <- as.factor(d.prop$subj)
d.prop           <- cbind(d.prop,vtime)
colnames(d.prop)[5] <- "vtime"
rm(vtime)

# PROP (Rel vs. Unr): All subjects subsetting
HeadN.base         <- subset(d.prop.base, headrel   ==  "HeadN") 
UnrHead.base       <- subset(d.prop.base, headrel   ==  "UnrHead")
NRel.base          <- subset(d.prop.base, localrel  ==  "NRel") 
NUnr.base          <- subset(d.prop.base, localrel  ==  "NUnr") 
HeadN.NRel.base    <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr.base    <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NUnr")
UnrHead.NRel.base  <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr.base  <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NUnr")
# ----


# PROP (Rel vs. Unr): Collapsed subjects subsetting
HeadN         <- subset(d.prop, headrel   ==  "HeadN") 
UnrHead       <- subset(d.prop, headrel   ==  "UnrHead")
NRel          <- subset(d.prop, localrel  ==  "NRel") 
NUnr          <- subset(d.prop, localrel  ==  "NUnr") 
HeadN.NRel    <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr    <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NUnr")
UnrHead.NRel  <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr  <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NUnr")

# ----
ds <- data.frame(data = c(
  "Grand Mean",
  "HeadN",
  "UnrHead",
  "NRel",
  "NUnr",
  "HeadN-NRel",
  "HeadN-NUnr",
  "UnrHead-NRel",
  "UnrHead-NUnr"),
  
  n = c(length(d.prop.base$vtime),
        length(HeadN.base$vtime), 
        length(UnrHead.base$vtime),
        length(NRel.base$vtime), 
        length(NUnr.base$vtime), 
        length(HeadN.NRel.base$vtime),
        length(HeadN.NUnr.base$vtime),
        length(UnrHead.NRel.base$vtime),
        length(UnrHead.NUnr.base$vtime)),
  
  N = c(length(d.prop$vtime),
        length(HeadN$vtime), 
        length(UnrHead$vtime),
        length(NRel$vtime), 
        length(NUnr$vtime), 
        length(HeadN.NRel$vtime),
        length(HeadN.NUnr$vtime),
        length(UnrHead.NRel$vtime),
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(d.prop$vtime),
           mean(HeadN$vtime), 
           mean(UnrHead$vtime),
           mean(NRel$vtime), 
           mean(NUnr$vtime), 
           mean(HeadN.NRel$vtime),
           mean(HeadN.NUnr$vtime),
           mean(UnrHead.NRel$vtime),
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(d.prop$vtime),
         sd(HeadN$vtime), 
         sd(UnrHead$vtime),
         sd(NRel$vtime), 
         sd(NUnr$vtime), 
         sd(HeadN.NRel$vtime),
         sd(HeadN.NUnr$vtime),
         sd(UnrHead.NRel$vtime),
         sd(UnrHead.NUnr$vtime)),
  
  se = c(
    sd(d.prop$vtime)         / sqrt(length(d.prop$vtime)),
    sd(HeadN$vtime)         / sqrt(length(HeadN$vtime)), 
    sd(UnrHead$vtime)       / sqrt(length(UnrHead$vtime)),
    sd(NRel$vtime)          / sqrt(length(NRel$vtime)),
    sd(NUnr$vtime)          / sqrt(length(NUnr$vtime)), 
    sd(HeadN.NRel$vtime)    / sqrt(length(HeadN.NRel$vtime)),
    sd(HeadN.NUnr$vtime)    / sqrt(length(HeadN.NUnr$vtime)),
    sd(UnrHead.NRel$vtime)  / sqrt(length(UnrHead.NRel$vtime)),
    sd(UnrHead.NUnr$vtime)  / sqrt(length(UnrHead.NUnr$vtime))  
  ))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("RELATED vs. UNRELATED HEAD", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) 
cat(" ", "\n")


a.2x2 <- aov(vtime ~ headrel * localrel + Error(subj / (headrel * localrel)), data = d.prop)
print(summary(a.2x2)) 
cat(" ", "\n")
cat(" ", "\n")


# PROP: RelHead (NRel vs. NUnr)----------------

ds.relhead <- data.frame(data = c(
  "HNoun",
  "NRel",
  "NUnrel"),
  
  n = c(length(HeadN.base$vtime),
        length(NRel.base$vtime), 
         length(NUnr.base$vtime)),
  
  N = c(length(HeadN$vtime),
        length(HeadN.NRel$vtime),
        length(HeadN.NUnr$vtime)),
  
  mean = c(mean(HeadN$vtime),
           mean(HeadN.NRel$vtime),
           mean(HeadN.NUnr$vtime)),
  
  sd = c(sd(HeadN$vtime),
         sd(HeadN.NRel$vtime),
         sd(HeadN.NUnr$vtime)),
  
  se = c(sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)),
         sd(HeadN.NRel$vtime)  / sqrt(length(HeadN.NRel$vtime)),
         sd(HeadN.NUnr$vtime)  / sqrt(length(HeadN.NUnr$vtime))
  ))

cat("\n")
cat("RELATED HEAD: NRel vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds.relhead) 
cat(" ", "\n")

a.h.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = HeadN) 
print(summary(a.h.relunrel)) 
cat(" ", "\n")
cat(" ", "\n")


# PROP: UnrelHead (NRel vs. NUnr)----------------

ds.unrel <- data.frame(data = c(
  "UnrHead",
  "NRel",
  "NUnrel"),
  
  n = c(length(UnrHead.base$vtime),
        length(NRel.base$vtime), 
        length(NUnr.base$vtime)),
  
  N = c(length(UnrHead$vtime),
        length(UnrHead.NRel$vtime),
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(UnrHead$vtime),
           mean(UnrHead.NRel$vtime),
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(UnrHead$vtime),
         sd(UnrHead.NRel$vtime),
         sd(UnrHead.NUnr$vtime)),
  
  se = c(sd(UnrHead$vtime)       / sqrt(length(UnrHead$vtime)),
         sd(UnrHead.NRel$vtime)  / sqrt(length(UnrHead.NRel$vtime)),
         sd(UnrHead.NUnr$vtime)  / sqrt(length(UnrHead.NUnr$vtime))
  ))

cat("\n")
cat("UNRELATED HEAD: NRel vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds.unrel) 
cat(" ", "\n")

a.uh.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = UnrHead) 
print(summary(a.uh.relunrel)) 
cat(" ", "\n")
cat(" ", "\n")
#--------------------------------------------
# PROP: (Assc vs. Unrel)
rm(list = ls()) 
library(languageR) 
library(reshape2)
library(plyr)
library(stringr)

f1vtimeout <- read.table("data/vtimeoutc.txt", header = F) 
colnames(f1vtimeout) <- c("subj", "item", "subexp", "headrel", "localrel", "vtime")

d.prop         <- subset(f1vtimeout, subexp == "Attr")
d.prop.base    <- subset(f1vtimeout, subexp == "Attr")
d.prop         <- subset(d.prop, localrel != "NRel")
d.prop.base    <- subset(d.prop, localrel != "NRel")

# PROP (Assc vs. Unr): Collapse over subject
d.prop$unique.id <- paste(d.prop[, 1], d.prop[, 3], d.prop[, 4], d.prop[, 5], sep = "_")
d.prop           <- ddply(d.prop, "unique.id", function(X) data.frame(vtime = mean(X$vtime)))
vtime            <- data.frame(d.prop$vtime)
d.prop           <- colsplit(d.prop$unique.id, "_", c("subj", "subexp", "headrel", "localrel"))
d.prop$subj      <- as.factor(d.prop$subj)
d.prop           <- cbind(d.prop,vtime)
colnames(d.prop)[5] <- "vtime"
rm(vtime)

# PROP (Assc vs. Unr): All subjects subsetting
HeadN.base         <- subset(d.prop.base, headrel   ==  "HeadN") 
UnrHead.base       <- subset(d.prop.base, headrel   ==  "UnrHead")
NUnr.base          <- subset(d.prop.base, localrel  ==  "NUnr") 
NAssc.base         <- subset(d.prop.base, localrel  ==  "NAssc")
HeadN.NUnr.base    <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NUnr")
HeadN.NAssc.base   <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NAssc")
UnrHead.NUnr.base  <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NUnr")
UnrHead.NAssc.base <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NAssc")
# ----


# PROP (Assc vs. Unr): Collapsed subjects subsetting
HeadN         <- subset(d.prop, headrel   ==  "HeadN") 
UnrHead       <- subset(d.prop, headrel   ==  "UnrHead")
NUnr          <- subset(d.prop, localrel  ==  "NUnr") 
NAssc         <- subset(d.prop, localrel  ==  "NAssc")
HeadN.NUnr    <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NUnr")
HeadN.NAssc   <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NAssc")
UnrHead.NUnr  <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NUnr")
UnrHead.NAssc <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NAssc")
# ----
ds <- data.frame(data = c(
  "Grand Mean",
  "HeadN",
  "UnrHead",
  "NAssc",
  "NUnr",
  "HeadN-NAssc",
  "HeadN-NUnr",
  "UnrHead-NAssc",
  "UnrHead-NUnr"),
  
  n = c(length(d.prop.base$vtime),
        length(HeadN.base$vtime), 
        length(UnrHead.base$vtime),
        length(NAssc.base$vtime), 
        length(NUnr.base$vtime), 
        length(HeadN.NAssc.base$vtime),
        length(HeadN.NUnr.base$vtime),
        length(UnrHead.NAssc.base$vtime),
        length(UnrHead.NUnr.base$vtime)),
  
  N = c(length(d.prop$vtime),
        length(HeadN$vtime), 
        length(UnrHead$vtime),
        length(NAssc$vtime), 
        length(NUnr$vtime), 
        length(HeadN.NAssc$vtime),
        length(HeadN.NUnr$vtime),
        length(UnrHead.NAssc$vtime),
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(d.prop$vtime),
           mean(HeadN$vtime), 
           mean(UnrHead$vtime),
           mean(NAssc$vtime), 
           mean(NUnr$vtime), 
           mean(HeadN.NAssc$vtime),
           mean(HeadN.NUnr$vtime),
           mean(UnrHead.NAssc$vtime),
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(d.prop$vtime),
         sd(HeadN$vtime), 
         sd(UnrHead$vtime),
         sd(NAssc$vtime), 
         sd(NUnr$vtime), 
         sd(HeadN.NAssc$vtime),
         sd(HeadN.NUnr$vtime),
         sd(UnrHead.NAssc$vtime),
         sd(UnrHead.NUnr$vtime)),
  
  se = c(
    sd(d.prop$vtime)         / sqrt(length(d.prop$vtime)),
    sd(HeadN$vtime)         / sqrt(length(HeadN$vtime)), 
    sd(UnrHead$vtime)       / sqrt(length(UnrHead$vtime)),
    sd(NAssc$vtime)          / sqrt(length(NAssc$vtime)),
    sd(NUnr$vtime)          / sqrt(length(NUnr$vtime)), 
    sd(HeadN.NAssc$vtime)    / sqrt(length(HeadN.NAssc$vtime)),
    sd(HeadN.NUnr$vtime)    / sqrt(length(HeadN.NUnr$vtime)),
    sd(UnrHead.NAssc$vtime)  / sqrt(length(UnrHead.NAssc$vtime)),
    sd(UnrHead.NUnr$vtime)  / sqrt(length(UnrHead.NUnr$vtime))  
  ))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("ASSOCIATED vs. UNRELATED HEAD", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) 
cat(" ", "\n")
# 
# 
# a.2x2 <- aov(vtime ~ headrel * localrel + Error(subj / (headrel * localrel)), data = d.prop)
# print(summary(a.2x2)) 
# cat(" ", "\n")
# cat(" ", "\n")


# PROP: RelHead (NAssc vs. NUnr)----------------

ds.relhead <- data.frame(data = c(
  "HNoun",
  "NAssc",
  "NUnrel"),
  
  n = c(length(HeadN.base$vtime),
        length(NAssc.base$vtime), 
        length(NUnr.base$vtime)),
  
  N = c(length(HeadN$vtime),
        length(HeadN.NAssc$vtime),
        length(HeadN.NUnr$vtime)),
  
  mean = c(mean(HeadN$vtime),
           mean(HeadN.NAssc$vtime),
           mean(HeadN.NUnr$vtime)),
  
  sd = c(sd(HeadN$vtime),
         sd(HeadN.NAssc$vtime),
         sd(HeadN.NUnr$vtime)),
  
  se = c(sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)),
         sd(HeadN.NAssc$vtime)  / sqrt(length(HeadN.NAssc$vtime)),
         sd(HeadN.NUnr$vtime)  / sqrt(length(HeadN.NUnr$vtime))
  ))

cat("\n")
cat("RELATED HEAD: NAssc vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds.relhead) 
cat(" ", "\n")

a.h.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = HeadN) 
print(summary(a.h.relunrel)) 
cat(" ", "\n")
cat(" ", "\n")


# PROP: UnrelHead (NAssc vs. NUnr)----------------

ds.unrel <- data.frame(data = c(
  "UnrHead",
  "NAssc",
  "NUnrel"),
  
  n = c(length(UnrHead.base$vtime),
        length(NAssc.base$vtime), 
        length(NUnr.base$vtime)),
  
  N = c(length(UnrHead$vtime),
        length(UnrHead.NAssc$vtime),
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(UnrHead$vtime),
           mean(UnrHead.NAssc$vtime),
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(UnrHead$vtime),
         sd(UnrHead.NAssc$vtime),
         sd(UnrHead.NUnr$vtime)),
  
  se = c(sd(UnrHead$vtime)       / sqrt(length(UnrHead$vtime)),
         sd(UnrHead.NAssc$vtime)  / sqrt(length(UnrHead.NAssc$vtime)),
         sd(UnrHead.NUnr$vtime)  / sqrt(length(UnrHead.NUnr$vtime))
  ))

cat("\n")
cat("UNRELATED HEAD: NAssc vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds.unrel) 
cat(" ", "\n")

# a.uh.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = UnrHead) 
# print(summary(a.uh.relunrel)) 
# cat(" ", "\n")
# cat(" ", "\n")

#--------------------------------------------
# PROP: (NRel vs. NAssc)
rm(list = ls()) 
library(languageR) 
library(reshape2)
library(plyr)
library(stringr)

f1vtimeout <- read.table("data/vtimeoutc.txt", header = F) 
colnames(f1vtimeout) <- c("subj", "item", "subexp", "headrel", "localrel", "vtime")

d.prop         <- subset(f1vtimeout, subexp == "Attr")
d.prop.base    <- subset(f1vtimeout, subexp == "Attr")
d.prop         <- subset(d.prop, localrel != "NUnr")
d.prop.base    <- subset(d.prop, localrel != "NUnr")

# PROP (Assc vs. Rel): Collapse over subject
d.prop$unique.id <- paste(d.prop[, 1], d.prop[, 3], d.prop[, 4], d.prop[, 5], sep = "_")
d.prop           <- ddply(d.prop, "unique.id", function(X) data.frame(vtime = mean(X$vtime)))
vtime            <- data.frame(d.prop$vtime)
d.prop           <- colsplit(d.prop$unique.id, "_", c("subj", "subexp", "headrel", "localrel"))
d.prop$subj      <- as.factor(d.prop$subj)
d.prop           <- cbind(d.prop,vtime)
colnames(d.prop)[5] <- "vtime"
rm(vtime)

# PROP (Assc vs. Rel): All subjects subsetting
HeadN.base         <- subset(d.prop.base, headrel   ==  "HeadN") 
UnrHead.base       <- subset(d.prop.base, headrel   ==  "UnrHead")
NRel.base          <- subset(d.prop.base, localrel  ==  "NRel") 
NAssc.base         <- subset(d.prop.base, localrel  ==  "NAssc")
HeadN.NRel.base    <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NRel")
HeadN.NAssc.base   <- subset(d.prop.base, headrel   ==  "HeadN"   & localrel  ==  "NAssc")
UnrHead.NRel.base  <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NRel")
UnrHead.NAssc.base <- subset(d.prop.base, headrel   ==  "UnrHead" & localrel  ==  "NAssc")
# ----


# PROP (Assc vs. Rel): Collapsed subjects subsetting
HeadN         <- subset(d.prop, headrel   ==  "HeadN") 
UnrHead       <- subset(d.prop, headrel   ==  "UnrHead")
NRel          <- subset(d.prop, localrel  ==  "NRel") 
NAssc         <- subset(d.prop, localrel  ==  "NAssc")
HeadN.NRel    <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NRel")
HeadN.NAssc   <- subset(d.prop, headrel   ==  "HeadN"   & localrel  ==  "NAssc")
UnrHead.NRel  <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NRel")
UnrHead.NAssc <- subset(d.prop, headrel   ==  "UnrHead" & localrel  ==  "NAssc")
# ----
ds <- data.frame(data = c(
  "Grand Mean",
  "HeadN",
  "UnrHead",
  "NAssc",
  "NUnr",
  "HeadN-NAssc",
  "HeadN-NRel",
  "UnrHead-NAssc",
  "UnrHead-NRel"),
  
  n = c(length(d.prop.base$vtime),
        length(HeadN.base$vtime), 
        length(UnrHead.base$vtime),
        length(NAssc.base$vtime), 
        length(NRel.base$vtime), 
        length(HeadN.NAssc.base$vtime),
        length(HeadN.NRel.base$vtime),
        length(UnrHead.NAssc.base$vtime),
        length(UnrHead.NRel.base$vtime)),
  
  N = c(length(d.prop$vtime),
        length(HeadN$vtime), 
        length(UnrHead$vtime),
        length(NAssc$vtime), 
        length(NRel$vtime), 
        length(HeadN.NAssc$vtime),
        length(HeadN.NRel$vtime),
        length(UnrHead.NAssc$vtime),
        length(UnrHead.NRel$vtime)),
  
  mean = c(mean(d.prop$vtime),
           mean(HeadN$vtime), 
           mean(UnrHead$vtime),
           mean(NAssc$vtime), 
           mean(NRel$vtime), 
           mean(HeadN.NAssc$vtime),
           mean(HeadN.NRel$vtime),
           mean(UnrHead.NAssc$vtime),
           mean(UnrHead.NRel$vtime)),
  
  sd = c(sd(d.prop$vtime),
         sd(HeadN$vtime), 
         sd(UnrHead$vtime),
         sd(NAssc$vtime), 
         sd(NRel$vtime), 
         sd(HeadN.NAssc$vtime),
         sd(HeadN.NRel$vtime),
         sd(UnrHead.NAssc$vtime),
         sd(UnrHead.NRel$vtime)),
  
  se = c(
    sd(d.prop$vtime)         / sqrt(length(d.prop$vtime)),
    sd(HeadN$vtime)         / sqrt(length(HeadN$vtime)), 
    sd(UnrHead$vtime)       / sqrt(length(UnrHead$vtime)),
    sd(NAssc$vtime)          / sqrt(length(NAssc$vtime)),
    sd(NRel$vtime)          / sqrt(length(NRel$vtime)), 
    sd(HeadN.NAssc$vtime)    / sqrt(length(HeadN.NAssc$vtime)),
    sd(HeadN.NRel$vtime)    / sqrt(length(HeadN.NRel$vtime)),
    sd(UnrHead.NAssc$vtime)  / sqrt(length(UnrHead.NAssc$vtime)),
    sd(UnrHead.NRel$vtime)  / sqrt(length(UnrHead.NRel$vtime))  
  ))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("ASSOCIATED vs. RELATED HEAD", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) 
cat(" ", "\n")
# 
# 
# a.2x2 <- aov(vtime ~ headrel * localrel + Error(subj / (headrel * localrel)), data = d.prop)
# print(summary(a.2x2)) 
# cat(" ", "\n")
# cat(" ", "\n")


# PROP: RelHead (NAssc vs. NRel)----------------

ds.relhead <- data.frame(data = c(
  "HNoun",
  "NAssc",
  "NRelel"),
  
  n = c(length(HeadN.base$vtime),
        length(NAssc.base$vtime), 
        length(NRel.base$vtime)),
  
  N = c(length(HeadN$vtime),
        length(HeadN.NAssc$vtime),
        length(HeadN.NRel$vtime)),
  
  mean = c(mean(HeadN$vtime),
           mean(HeadN.NAssc$vtime),
           mean(HeadN.NRel$vtime)),
  
  sd = c(sd(HeadN$vtime),
         sd(HeadN.NAssc$vtime),
         sd(HeadN.NRel$vtime)),
  
  se = c(sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)),
         sd(HeadN.NAssc$vtime)  / sqrt(length(HeadN.NAssc$vtime)),
         sd(HeadN.NRel$vtime)  / sqrt(length(HeadN.NRel$vtime))
  ))

cat("\n")
cat("RELATED HEAD: NAssc vs. NRelel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds.relhead) 
cat(" ", "\n")

a.h.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = HeadN) 
print(summary(a.h.relunrel)) 
cat(" ", "\n")
cat(" ", "\n")


# PROP: UnrelHead (NAssc vs. NRel)----------------

ds.unrel <- data.frame(data = c(
  "UnrHead",
  "NAssc",
  "NUnrel"),
  
  n = c(length(UnrHead.base$vtime),
        length(NAssc.base$vtime), 
        length(NRel.base$vtime)),
  
  N = c(length(UnrHead$vtime),
        length(UnrHead.NAssc$vtime),
        length(UnrHead.NRel$vtime)),
  
  mean = c(mean(UnrHead$vtime),
           mean(UnrHead.NAssc$vtime),
           mean(UnrHead.NRel$vtime)),
  
  sd = c(sd(UnrHead$vtime),
         sd(UnrHead.NAssc$vtime),
         sd(UnrHead.NRel$vtime)),
  
  se = c(sd(UnrHead$vtime)       / sqrt(length(UnrHead$vtime)),
         sd(UnrHead.NAssc$vtime)  / sqrt(length(UnrHead.NAssc$vtime)),
         sd(UnrHead.NRel$vtime)  / sqrt(length(UnrHead.NRel$vtime))
  ))

cat("\n")
cat("UNRELATED HEAD: NAssc vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds.unrel) 
cat(" ", "\n")

# a.uh.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = UnrHead) 
# print(summary(a.uh.relunrel)) 
# cat(" ", "\n")
# cat(" ", "\n")

sink()







# SEMREL ITEMS ==========================================================================

rm(list = ls())
library(languageR) 
library(reshape2)
library(plyr)
library(stringr)

f1vtimeout <- read.table("data/vtimeoutc.txt", header = F) 
colnames(f1vtimeout) <- c("subj", "item", "subexp", "headrel", "localrel", "vtime")

d.semrel       <- subset(f1vtimeout, subexp == "SemRel")
d.semrel.base  <- subset(f1vtimeout, subexp == "SemRel")

# SEMREL: Collapse over subject
d.semrel$unique.id <- paste(d.semrel[, 1], d.semrel[, 3], d.semrel[, 4], d.semrel[, 5], sep = "_")
d.semrel           <- ddply(d.semrel, "unique.id", function(X) data.frame(vtime = mean(X$vtime)))
vtime              <- data.frame(d.semrel$vtime)
d.semrel           <- colsplit(d.semrel$unique.id, "_", c("subj", "subexp", "headrel", "localrel"))
d.semrel$subj      <- as.factor(d.semrel$subj)
d.semrel           <- cbind(d.semrel,vtime)
colnames(d.semrel)[5] <- "vtime"
rm(vtime)

# SEMREL:  All subjects subsetting
HeadN.base         <- subset(d.semrel.base, headrel   ==  "HeadN") 
UnrHead.base       <- subset(d.semrel.base, headrel   ==  "UnrHead")
NRel.base          <- subset(d.semrel.base, localrel  ==  "NRel") 
NUnr.base          <- subset(d.semrel.base, localrel  ==  "NUnr") 
HeadN.NRel.base    <- subset(d.semrel.base, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr.base    <- subset(d.semrel.base, headrel   ==  "HeadN"   & localrel  ==  "NUnr") 
UnrHead.NRel.base  <- subset(d.semrel.base, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr.base  <- subset(d.semrel.base, headrel   ==  "UnrHead" & localrel  ==  "NUnr")

# SEMREL: Collapsed subjects subsetting
HeadN         <- subset(d.semrel, headrel   ==  "HeadN") 
UnrHead       <- subset(d.semrel, headrel   ==  "UnrHead")
NRel          <- subset(d.semrel, localrel  ==  "NRel") 
NUnr          <- subset(d.semrel, localrel  ==  "NUnr") 
HeadN.NRel    <- subset(d.semrel, headrel   ==  "HeadN"   & localrel  ==  "NRel") 
HeadN.NUnr    <- subset(d.semrel, headrel   ==  "HeadN"   & localrel  ==  "NUnr") 
UnrHead.NRel  <- subset(d.semrel, headrel   ==  "UnrHead" & localrel  ==  "NRel") 
UnrHead.NUnr  <- subset(d.semrel, headrel   ==  "UnrHead" & localrel  ==  "NUnr")

ds <- data.frame(data = c(
  "Grand Mean",
  "HeadN",
  "UnrHead",
  "NRel",
  "NUnr",
  "HeadN-NRel",
  "HeadN-NUnr",
  "UnrHead-NRel",
  "UnrHead-NUnr"),
  
  n = c(length(d.semrel.base$vtime),
        length(HeadN.base$vtime), 
        length(UnrHead.base$vtime),
        length(NRel.base$vtime), 
        length(NUnr.base$vtime), 
        length(HeadN.NRel.base$vtime),
        length(HeadN.NUnr.base$vtime),
        length(UnrHead.NRel.base$vtime),
        length(UnrHead.NUnr.base$vtime)),
  
  N = c(length(d.semrel$vtime),
        length(HeadN$vtime), 
        length(UnrHead$vtime),
        length(NRel$vtime), 
        length(NUnr$vtime), 
        length(HeadN.NRel$vtime),
        length(HeadN.NUnr$vtime),
        length(UnrHead.NRel$vtime),
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(d.semrel$vtime),
           mean(HeadN$vtime), 
           mean(UnrHead$vtime),
           mean(NRel$vtime), 
           mean(NUnr$vtime), 
           mean(HeadN.NRel$vtime),
           mean(HeadN.NUnr$vtime),
           mean(UnrHead.NRel$vtime),
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(d.semrel$vtime),
         sd(HeadN$vtime), 
         sd(UnrHead$vtime),
         sd(NRel$vtime), 
         sd(NUnr$vtime), 
         sd(HeadN.NRel$vtime),
         sd(HeadN.NUnr$vtime),
         sd(UnrHead.NRel$vtime),
         sd(UnrHead.NUnr$vtime)),
  
  se = c(sd(d.semrel$vtime)       / sqrt(length(d.semrel$vtime)),
         sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)), 
         sd(UnrHead$vtime)     / sqrt(length(UnrHead$vtime)),
         sd(NRel$vtime)        / sqrt(length(NRel$vtime)), 
         sd(NUnr$vtime)        / sqrt(length(NUnr$vtime)), 
         sd(HeadN.NRel$vtime)  / sqrt(length(HeadN.NRel$vtime)),
         sd(HeadN.NUnr$vtime)  / sqrt(length(HeadN.NUnr$vtime)),
         sd(UnrHead.NRel$vtime)/ sqrt(length(UnrHead.NRel$vtime)),
         sd(UnrHead.NUnr$vtime)/ sqrt(length(UnrHead.NUnr$vtime))
  ))

# SEMREL: Figure -------------------------------------------------
semrel.fig <- ds[6:9,c(1,4,6)]
dodge  <- position_dodge(width = 0.9)
g1     <- ggplot(data = semrel.fig, aes(x = data, y = mean, fill=data)) +
  layer(geom="bar", stat="identity", position = position_dodge())+
  coord_cartesian(ylim = c(450, 525))+
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), position = dodge, width = 0.2)
g1
ggsave(filename = "figures/SemRel F1.png")
#   scale_fill_manual(values=c("#990000", "#CC6666", "#000099", "#9999CC")) +
#   guides(fill=FALSE)+
#  + 
#   +
#   scale_y_continuous(breaks=seq(0, 14, 2))+
#   annotate("text", x = 1:4, y = -1, label = rep(c("Integrated", "Unintegrated"), 2), size=6) +
#   annotate("text", c(1.5, 3.5), y = -2, label = c("Related", "Unrelated"), size=6) +
#   
#   theme_classic() +
#   theme(text = element_text(size=18.5)) +
#   ylab("Mismatch effect (%)") +
#   theme(axis.title.y=element_text(vjust=1.5)) +
#   theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), axis.title.x = element_blank(), axis.text.x = element_blank())



# SEMREL: ALL 2X2 ANOVA-----------------------------------------------------

sink("output/Reprime SemRel F1 Factorial Analyses.txt")

cat(" ", "\n")
cat("BY-SUBJECTS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: SEMREL ITEMS", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) 
cat(" ", "\n")

a.2x2 <- aov(vtime ~ headrel * localrel + Error(subj / (headrel * localrel)), data = d.semrel)
print(summary(a.2x2)) 
cat(" ", "\n")
cat(" ", "\n")

# SEMREL: RELATED HEAD-------------------------------------------------------------------------------

ds <- data.frame(data = c(
  "HNoun",
  "NRel",
  "NUnrel"),
  
  n = c(length(HeadN.base$vtime),
        length(HeadN.NRel.base$vtime), 
        length(HeadN.NUnr.base$vtime)),
  
  N = c(length(HeadN$vtime),
        length(HeadN.NRel$vtime), 
        length(HeadN.NUnr$vtime)),
  
  mean = c(mean(HeadN$vtime),
           mean(HeadN.NRel$vtime), 
           mean(HeadN.NUnr$vtime)),
  
  sd = c(sd(HeadN$vtime),
         sd(HeadN.NRel$vtime), 
         sd(HeadN.NUnr$vtime)),
  
  se = c(sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)),
         sd(HeadN.NRel$vtime)  / sqrt(length(HeadN.NRel$vtime)), 
         sd(HeadN.NUnr$vtime)  / sqrt(length(HeadN.NUnr$vtime))
  ))

cat("\n")
cat("RELATED HEAD: NRel vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds) 
cat(" ", "\n")

a.h.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = HeadN) 
print(summary(a.h.relunrel)) 
cat(" ", "\n")
cat(" ", "\n")


# SEMREL: UNRELATED HEAD  -------------------------------------------------------------------------------------------

ds <- data.frame(data = c(
  "UnrHead",
  "NRel",
  "NUnrel"),
  
  n = c(length(UnrHead.base$vtime),
        length(UnrHead.NRel.base$vtime), 
        length(UnrHead.NUnr.base$vtime)),
  
  N = c(length(HeadN$vtime),
        length(UnrHead.NRel$vtime), 
        length(UnrHead.NUnr$vtime)),
  
  mean = c(mean(HeadN$vtime),
           mean(UnrHead.NRel$vtime), 
           mean(UnrHead.NUnr$vtime)),
  
  sd = c(sd(HeadN$vtime),
         sd(UnrHead.NRel$vtime), 
         sd(UnrHead.NUnr$vtime)),
  
  se = c(sd(HeadN$vtime)       / sqrt(length(HeadN$vtime)),
         sd(UnrHead.NRel$vtime)  / sqrt(length(UnrHead.NRel$vtime)), 
         sd(UnrHead.NUnr$vtime)  / sqrt(length(UnrHead.NUnr$vtime))
  ))

cat("\n")
cat("RELATED HEAD: NRel vs. NUnrel", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
print(ds) 
cat(" ", "\n")

a.h.relunrel <- aov(vtime ~ localrel + Error(subj / (localrel)), data = UnrHead) 
print(summary(a.h.relunrel)) 

sink()