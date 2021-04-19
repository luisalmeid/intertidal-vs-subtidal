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
reco <- read.csv("reco.csv")

### explore data
summary(reco)
str(reco)
table(reco$treatment)
table(reco$origin)
table(is.na(reco$bleach))

boxplot(bleach~treatment,data=reco,
        main="recovery",
        xlab="treatment",
        ylab="brightness",
        ylim = c(0,150))

boxplot(bleach~origin,data=reco,
        main="recovery",
        xlab="origin",
        ylab="brightness",
        ylim = c(0,150))

## Data skewness 
hist(reco$bleach, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(reco$bleach), sd=sd(reco$bleach)), add=TRUE, col="blue")

hist(reco$bleachln, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(reco$bleach), sd=sd(reco$bleach)), add=TRUE, col="blue")

hist(reco$bleachsqrt, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(reco$bleach), sd=sd(reco$bleach)), add=TRUE, col="blue")

### 13-day recovery differences
## Two way ANOVA
res.aov <- aov(bleachsqrt ~ origin * day, data = reco)
summary(res.aov)

plot(res.aov, 1)
y <- as.factor(reco$day)
leveneTest(bleachsqrt ~ origin*y, data = reco )

plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov)
shapiro.test(x = aov_residuals )

### Line graphs 13 days
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

reco.avg <- data_summary(reco, varname="bleach", 
                      groupnames=c("origin", "day"))

ggplot(reco.avg, aes(x=day, y=bleach, group=origin, color=origin)) + 
  geom_line() +
  geom_point()+coord_cartesian(ylim=c(0,150))+theme_classic()+
  ggtitle("Recovery")+ labs(y= "Brightness (bpp)", x = "Day")+guides(fill=FALSE)+
  scale_color_brewer(palette = "Set1")

ggplot(reco.avg, aes(x = day, y = bleach, color=origin)) +  
  geom_point() +theme_classic()+coord_cartesian(ylim=c(0,150))+ stat_smooth(method="lm", se=F)+
  ggtitle("Recovery")+ labs(y= "Brightness (bpp)", x = "Day")+guides(fill=FALSE)+ 
  scale_color_brewer(palette = "Set1")
