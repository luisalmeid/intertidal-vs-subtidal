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

### load data
bleac <- read.csv("bleac.csv")
day1 <- read.csv("day1.csv")

### explore data
summary(bleac)
str(bleac)
table(bleac$treatment)
table(bleac$origin)
table(is.na(bleac$bleach))

boxplot(bleach~treatment,data=bleac,
        main="bleaching",
        xlab="treatment",
        ylab="brightness",
        ylim = c(0,200))

## Data skewness 
hist(bleac$bleach, freq=FALSE,breaks = 10, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(bleac$bleach), sd=sd(bleac$bleach)), add=TRUE, col="blue")

hist(bleac$bleachln, freq=FALSE,breaks = 10, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(bleac$bleachln), sd=sd(bleac$bleach)), add=TRUE, col="blue")


### 5-day brigthness differences
## Two way ANOVA
res.aov <- aov(bleachln ~ treatment * day, data = bleac)
summary(res.aov)

plot(res.aov, 1)
x <- as.factor(bleac$day)
leveneTest(bleachln ~ treatment * x, data = bleac )

plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov)
shapiro.test(x = aov_residuals )

## Line graphs 5 days
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

bleac.avg <- data_summary(bleac, varname="bleach", 
                          groupnames=c("treatment", "day"))

ggplot(bleac.avg, aes(x=day, y=bleach, group=treatment, color=treatment)) + 
  geom_line() +
  geom_point()+coord_cartesian(ylim=c(0,150))+theme_classic()+
  ggtitle("Thermal stress response")+ labs(y= "Brightness (bpp)", x = "Day")+guides(fill=FALSE)+ scale_color_brewer(palette = "RdBu")

ggplot(bleac.avg, aes(x = day, y = bleach, color=treatment)) +  
  geom_point() +theme_classic()+coord_cartesian(ylim=c(0,150))+ stat_smooth(method="lm", se=F)+
  ggtitle("Thermal stress response")+ labs(y= "Brightness (bpp)", x = "Day")+guides(fill=FALSE)+ 
  scale_color_brewer(palette = "RdBu")

### day 1 brightness differences 
## normality
shapiro.test(day1$X1)
X2<-log(day1$X1)
shapiro.test(X2)

## homocedasticity 
leveneTest(X2~treatment,data=day1)

## ANOVA
fit.X2<-aov(X2~treatment, data=day1)
summary(fit.X2)
TukeyHSD(x=fit.X2,"treatment",conf.level = 0.95)

## Bar graphs  day 1
bl<-summarySE(day1, measurevar="X1",groupvars=c("treatment","origin"))

ggplot(bl, aes(x=treatment, y=X1, fill=treatment)) + 
  theme_classic()+ ggtitle("Thermal stress response: Day 1")+ labs(y= "Brightness (bpp)", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,150))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=X1-se, ymax=X1+se), width=.15,
                position=position_dodge(.9))
