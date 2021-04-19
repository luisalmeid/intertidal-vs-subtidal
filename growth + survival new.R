### setup
rm(list=ls())
setwd("C:/Users/Lula/Desktop/Reefolution/R")

### packages
install.packages("car")
install.packages("dplyr")
install.packages("Rmisc")
install.packages("ggplot2")

library(car)
library(dplyr)
library(Rmisc) 
library(ggplot2)

##### GROWTH #####

### load data
grow <- read.csv("g.csv")

### explore data
summary(grow)
str(grow)
table(grow$treatment)
table(grow$origin)
table(is.na(grow$SGRavg))

boxplot(SGRavg~treatment,data=grow,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

hist(grow$SGRavg, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(grow$SGRavg), sd=sd(grow$SGRavg)), add=TRUE, col="blue")

## normality + homocedasticity
SGR <- grow$SGRavg
shapiro.test(SGR)
leveneTest(SGR~treatment,data=grow)

### ANOVA
aov<-aov(SGR~treatment, data=grow)
summary(aov)

TukeyHSD(x=aov,"treatment",conf.level = 0.95)

### bar graphs
gr<-summarySE(grow, measurevar="SGRavg",groupvars=c("treatment","origin"))

ggplot(gr, aes(x=treatment, y=SGRavg, fill=treatment)) + 
  theme_classic()+ ggtitle("Growth")+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGRavg-se, ymax=SGRavg+se), width=.15,
                position=position_dodge(.9))


##### SURVIVAL #####

### load data
surv <- read.csv("s.csv")

### explore data
summary(surv)
str(surv)
table(surv$treatment)
table(surv$origin)
table(is.na(surv))

boxplot(survival~treatment,data=surv,
        main = "",
        xlab="Treatment",
        ylab="Survival",
        ylim = c(0.7,1.1))

## Data skewness 
hist(surv$survival, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(surv$survival), sd=sd(surv$survival)), add=TRUE, col="blue")

#arcsin transformation
hist(surv$su, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(surv$su), sd=sd(surv$su)), add=TRUE, col="blue")

## normality + homocedasticity
su <- surv$su
shapiro.test(su)
leveneTest(su~treatment,data=surv)

### ANOVA
aov <- aov(su~treatment, data=surv)
summary(aov)

TukeyHSD(x=aov,"treatment",conf.level = 0.95)

### bar graphs
s <-summarySE(surv, measurevar="survival",groupvars=c("treatment","origin"))

ggplot(s, aes(x=treatment, y=survival, fill=treatment)) + 
  theme_classic()+ ggtitle("Survival")+ labs(y= "Survival (%)", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,1))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.15,
                position=position_dodge(.9))
