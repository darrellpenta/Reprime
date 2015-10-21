rm(list = ls())
library(ggplot2)
library(languageR)
library(lme4)
library(lmerTest)
library(grid)
library(plyr)
library(reshape2)
library(gtable)

data<- read.csv("output/priming_figures_data.csv", row.names=1)
errbar<- read.csv("output/priming_effect_errbars.csv", row.names=1)
prime.eff <- data.frame(
Exp     = c(rep(c("Experiment 1"),times=2), 
          rep(c("Exp2:Coordinates"), times=2), 
          rep(c("Exp2:Semantic-Associative"), times=3)),  
LN      = c("Related",
         "Unrelated",
         "Cordinate", 
         "Noncoordinate", 
         "Attribute", 
         "Associate", 
         "Unrel" ),
Priming = c((data$mean[11]-data$mean[13]),
           (data$mean[12]-data$mean[14]),
           (data$mean[1]-data$mean[3]),
           (data$mean[2]-data$mean[4]),
           (data$mean[5]-data$mean[8]),
           (data$mean[6]-data$mean[9]),
           (data$mean[7]-data$mean[10])),
Err     = c(errbar$data[6],
            errbar$data[7],
            errbar$data[1],
            errbar$data[2],
            errbar$data[3],
            errbar$data[4],
            errbar$data[5])
) 
  



prime.eff$LN <- factor(prime.eff$LN, levels=c("Related", "Unrelated","Cordinate", "Noncoordinate", "Attribute", "Associate", "Unrel"))
dodge  <- position_dodge(width = 0.9) # set position doge

g2 <- ggplot(data = prime.eff, 
             aes(y=Priming, 
                 x = interaction(Exp,LN), 
                 fill = interaction(Exp,LN))
             ) +
      layer(geom="bar", 
            stat="identity", 
            position = position_dodge(),
            color = c(
              "#990000", # SemRel Related, red
              "#990000", # SemRel Unrelated, light red
              "#FFA500", # Coordinates, orange
              "#FFA500", # Non-Coordinates, light orange
              "#800080", # Attributes, strong purple
              "#800080", # Associates, light purple
              "#800080"  # Unrelated, very light purple
            ) ) +
        scale_y_reverse() +
      theme_classic(base_size = 22) +
      scale_fill_manual(values = c(
        "#990000", # SemRel Related, red
        "#d69999", # SemRel Unrelated, light red
        "#FFA500", # Coordinates, orange
        "#ffd27f", # Non-Coordinates, light orange
        "#800080", # Attributes, strong purple
        "#cc99cc", # Associates, light purple
        "#f7eff7"  # Unrelated, very light purple
        )) +
      # theme(axis.text.x = element_text(angle = 90)
      theme(text = element_text(size=22)) +
      guides(fill=FALSE) +
      scale_x_discrete(labels=c("Related", "Unrelated", "Related", "Unrelated", "Attribute", "Unrelated")) +        theme(legend.position="none") +
      geom_hline(yintercept = 0)    +
      xlab("\n\nLocal Noun Condition\n") +
      theme(axis.title.x=element_text(vjust=-.45, size=22)) +      
      ylab("Priming Effect (ms)") +
       annotate("segment", x = 2.5, xend = 2.5, y = -25, yend = -12,
         colour = "gray") + 
     annotate("segment", x = 4.5, xend = 4.5, y = -25, yend = -12,
              colour = "gray")   
  g2
  
 