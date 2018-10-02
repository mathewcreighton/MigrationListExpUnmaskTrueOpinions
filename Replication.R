################################################################
#Programmer:  Mathew J Creighton
#Input: List experiment from LISS
#Output: Results for ESRA
#Date Began: 8/3/2105
#Date Completed: ...
################################################################

# Steps:
# 1) Load each dataset (there is one for each of the 4 list experiments)

#loading "list" package
library("list")
library("foreign")
library("Hmisc")
library("extrafont")
library("calibrate")

#setting working directory
#MAC
#setwd("/Users/mathew.creighton/Google Drive/Work/LISS/Data")

#PC
# setwd("C:\\Users\\mathe\\Google Drive\\Work\\LISS\\Data")

#loading data
same.race.independent <- stata.get("same_race_independent.dta")
different.race.independent <- stata.get("different_race_independent.dta")
poor.independent <- stata.get("poor_independent.dta")

same.race <- stata.get("same_race.dta")
different.race <- stata.get("different_race.dta")
poor <- stata.get("poor.dta")

#setting font preferences
windowsFonts(TimesNewRoman=windowsFont("TT Times New Roman"))

#Same Race Models

#List models (lm)
same.race.list <- ictreg(y ~ 1, data = same.race.independent, treat = "treat", J = 3, method = "lm")          
same.race.list.predict <- predict(same.race.list, newdata = same.race.independent, se.fit = TRUE, avg = TRUE)

#Direct models (logit )
same.race.direct <- glm(formula = d ~ 1, data = same.race, family = binomial(link = "logit"), na.action = na.omit)
same.race.direct.predict <- predict(same.race.direct, newdata = same.race, se.fit = TRUE, avg = TRUE, type = "response")

#Predicted values for list models
same.race.list.var<- (same.race.list.predict$se.fit)^2
same.race.list.fit<- (same.race.list.predict$fit)
same.race.list.lwr<- same.race.list.fit-(1.96*sqrt(same.race.list.var))
same.race.list.upr<- same.race.list.fit+(1.96*sqrt(same.race.list.var))

#Predicted values for direct models
same.race.direct.var<-(mean((same.race.direct.predict$se.fit)^2))
same.race.direct.fit<-(mean(same.race.direct.predict$fit))
same.race.direct.lwr<- same.race.direct.fit-(1.96*sqrt(same.race.direct.var))
same.race.direct.upr<- same.race.direct.fit+(1.96*sqrt(same.race.direct.var))

#Predicted values for social desirability bias
same.race.direct.list.diff<- (same.race.direct.fit-same.race.list.fit)
same.race.diff.upr<- same.race.direct.list.diff+(1.96*(sqrt(same.race.list.var+same.race.direct.var)))
same.race.diff.lwr<- same.race.direct.list.diff-(1.96*(sqrt(same.race.list.var+same.race.direct.var)))

same.race.results.nocov<- c(same.race.list.fit, same.race.list.lwr, same.race.list.upr, same.race.direct.fit, same.race.direct.lwr, same.race.direct.upr, same.race.direct.list.diff, same.race.diff.lwr, same.race.diff.upr)

x<- c(1.25,2,2.75)
plot(x, ylim=c(-0.1,0.8),xaxt="n", xlab="", ylab="", bty="n")
segments(y0=c(same.race.results.nocov[5], same.race.results.nocov[2], same.race.results.nocov[8]), 
         x0=c(1.25,2,2.75), 
         y1=c(same.race.results.nocov[6], same.race.results.nocov[3], same.race.results.nocov[9]))
points(x=c(1.25,2,2.75), 
       y=c(same.race.results.nocov[4],same.race.results.nocov[1], same.race.results.nocov[7]))
abline(c(0,0), lty=5)      
title(main="Plot X: Social Desirability Bias \nThe Netherlands should allow immigrants of the same race", ylab="Proportion agree")
axis<- c(1.25, 2, 2.75)
axislbl<- c("Explicit", "Implicit", "Difference")
axis(1, at=axis, labels=axislbl)

#Different Race Models

#List models (lm)
different.race.list <- ictreg(y ~ 1, data = different.race.independent, treat = "treat", J = 3, method = "lm")          
different.race.list.predict <- predict(different.race.list, newdata = different.race.independent, se.fit = TRUE, avg = TRUE)

#Direct models (logit )
different.race.direct <- glm(formula = d ~ 1, data = different.race, family = binomial(link = "logit"), na.action = na.omit)
different.race.direct.predict <- predict(different.race.direct, newdata = different.race, se.fit = TRUE, avg = TRUE, type = "response")

#Predicted values for list models
different.race.list.var<- (different.race.list.predict$se.fit)^2
different.race.list.fit<- (different.race.list.predict$fit)
different.race.list.lwr<- different.race.list.fit-(1.96*sqrt(different.race.list.var))
different.race.list.upr<- different.race.list.fit+(1.96*sqrt(different.race.list.var))

#Predicted values for direct models
different.race.direct.var<-(mean((different.race.direct.predict$se.fit)^2))
different.race.direct.fit<-(mean(different.race.direct.predict$fit))
different.race.direct.lwr<- different.race.direct.fit-(1.96*sqrt(different.race.direct.var))
different.race.direct.upr<- different.race.direct.fit+(1.96*sqrt(different.race.direct.var))

#Predicted values for social desirability bias
different.race.direct.list.diff<- (different.race.direct.fit-different.race.list.fit)
different.race.diff.upr<- different.race.direct.list.diff+(1.96*(sqrt(different.race.list.var+different.race.direct.var)))
different.race.diff.lwr<- different.race.direct.list.diff-(1.96*(sqrt(different.race.list.var+different.race.direct.var)))

different.race.results.nocov<- c(different.race.list.fit, different.race.list.lwr, different.race.list.upr, different.race.direct.fit, different.race.direct.lwr, different.race.direct.upr, different.race.direct.list.diff, different.race.diff.lwr, different.race.diff.upr)

x<- c(1.25,2,2.75)
plot(x, ylim=c(-0.1,0.8),xaxt="n", xlab="", ylab="", bty="n")
segments(y0=c(different.race.results.nocov[5], different.race.results.nocov[2], different.race.results.nocov[8]), 
         x0=c(1.25,2,2.75), 
         y1=c(different.race.results.nocov[6], different.race.results.nocov[3], different.race.results.nocov[9]))
points(x=c(1.25,2,2.75), 
       y=c(different.race.results.nocov[4],different.race.results.nocov[1], different.race.results.nocov[7]))
abline(c(0,0), lty=5)      
title(main="Plot X: Social Desirability Bias \nThe Netherlands should allow immigrants of a different race", ylab="Proportion agree")
axis<- c(1.25, 2, 2.75)
axislbl<- c("Explicit", "Implicit", "Difference")
axis(1, at=axis, labels=axislbl)

#Poor Models

#List models (lm)
poor.list <- ictreg(y ~ 1, data = poor.independent, treat = "treat", J = 3, method = "lm")          
poor.list.predict <- predict(poor.list, newdata = poor.independent, se.fit = TRUE, avg = TRUE)

#Direct models (logit )
poor.direct <- glm(formula = d ~ 1, data = poor, family = binomial(link = "logit"), na.action = na.omit)
poor.direct.predict <- predict(poor.direct, newdata = poor, se.fit = TRUE, avg = TRUE, type = "response")

#Predicted values for list models
poor.list.var<- (poor.list.predict$se.fit)^2
poor.list.fit<- (poor.list.predict$fit)
poor.list.lwr<- poor.list.fit-(1.96*sqrt(poor.list.var))
poor.list.upr<- poor.list.fit+(1.96*sqrt(poor.list.var))

#Predicted values for direct models
poor.direct.var<-(mean((poor.direct.predict$se.fit)^2))
poor.direct.fit<-(mean(poor.direct.predict$fit))
poor.direct.lwr<- poor.direct.fit-(1.96*sqrt(poor.direct.var))
poor.direct.upr<- poor.direct.fit+(1.96*sqrt(poor.direct.var))

#Predicted values for social desirability bias
poor.direct.list.diff<- (poor.direct.fit-poor.list.fit)
poor.diff.upr<- poor.direct.list.diff+(1.96*(sqrt(poor.list.var+poor.direct.var)))
poor.diff.lwr<- poor.direct.list.diff-(1.96*(sqrt(poor.list.var+poor.direct.var)))

poor.results.nocov<- c(poor.list.fit, poor.list.lwr, poor.list.upr, poor.direct.fit, poor.direct.lwr, poor.direct.upr, poor.direct.list.diff, poor.diff.lwr, poor.diff.upr)

x<- c(1.25,2,2.75)
plot(x, ylim=c(-0.1,0.8),xaxt="n", xlab="", ylab="", bty="n")
segments(y0=c(poor.results.nocov[5], poor.results.nocov[2], poor.results.nocov[8]), 
         x0=c(1.25,2,2.75), 
         y1=c(poor.results.nocov[6], poor.results.nocov[3], poor.results.nocov[9]))
points(x=c(1.25,2,2.75), 
       y=c(poor.results.nocov[4],poor.results.nocov[1], poor.results.nocov[7]))
abline(c(0,0), lty=5)      
title(main="Plot X: Social Desirability Bias \nThe Netherlands should allow poor immigrants", ylab="Proportion agree")
axis<- c(1.25, 2, 2.75)
axislbl<- c("Explicit", "Implicit", "Difference")
axis(1, at=axis, labels=axislbl)


#All list estimates - same, different, poor
item.count.lwr.nocov<- c(same.race.list.lwr, different.race.list.lwr, poor.list.lwr)
item.count.upr.nocov<- c(same.race.list.upr, different.race.list.upr, poor.list.upr)
item.count.fit.nocov<- c(same.race.list.fit, different.race.list.fit, poor.list.fit)

x<- c(1.25,2,2.75)
plot(x, ylim=c(-0.1,0.8),xaxt="n", xlab="", ylab="", bty="n")
segments(y0=c(item.count.lwr.nocov[1],item.count.lwr.nocov[2],item.count.lwr.nocov[3]), 
         x0=c(1.25,2,2.75), 
         y1=c(item.count.upr.nocov[1],item.count.upr.nocov[2],item.count.upr.nocov[3])) 
points(x=c(1.25,2,2.75), 
       y=c(item.count.fit.nocov[1],item.count.fit.nocov[2],item.count.fit.nocov[3]))
abline(c(0,0), lty=5)      
title(main="Plot X: The Netherlands should allow immigrants who are... ", ylab="Proportion who implicitly agree")
axis<- c(1.25,2,2.75)
axislbl<- c("...of the same race", "...of a different Race", "...poor")
axis(1, at=axis, labels=axislbl)

#All estimates - same, different, poor
direct.lwr.nocov<- c(same.race.direct.lwr, different.race.direct.lwr, poor.direct.lwr)
direct.upr.nocov<- c(same.race.direct.upr, different.race.direct.upr, poor.direct.upr)
direct.fit.nocov<- c(same.race.direct.fit, different.race.direct.fit, poor.direct.fit)

x<- c(1.25,2,2.75)
plot(x=NULL, y=NULL, ylim=c(0,0.8), xlim=c(1,3), xaxt="n", xlab="", ylab="", bty="n", family="TimesNewRoman", pch=NULL)
segments(y0=c(item.count.lwr.nocov[1],item.count.lwr.nocov[2],item.count.lwr.nocov[3]), 
         x0=x, 
         y1=c(item.count.upr.nocov[1],item.count.upr.nocov[2],item.count.upr.nocov[3])) 
segments(y0=c(direct.lwr.nocov[1],direct.lwr.nocov[2],direct.lwr.nocov[3]), 
         x0=x, 
         y1=c(direct.upr.nocov[1],direct.upr.nocov[2],direct.upr.nocov[3])) 
points(x=x, pch=15, 
       y=c(item.count.fit.nocov[1],item.count.fit.nocov[2],item.count.fit.nocov[3]))
points(x=x, pch=16,
       y=c(direct.fit.nocov[1],direct.fit.nocov[2],direct.fit.nocov[3]))

textxy(1.25, item.count.fit.nocov[1]+0.05, "Covert/List (L)", offset=0.6, cex=0.8, family="TimesNewRoman")
textxy(1.25, direct.fit.nocov[1]+0.05, "Overt/Direct (D)", offset=0.6, cex=0.8, family="TimesNewRoman")

textxy(1.25, item.count.fit.nocov[1], formatC(round(item.count.fit.nocov[1],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")
textxy(1.25, direct.fit.nocov[1], formatC(round(direct.fit.nocov[1],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")

textxy(2, item.count.fit.nocov[2], formatC(round(item.count.fit.nocov[2],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")
textxy(2, direct.fit.nocov[2], formatC(round(direct.fit.nocov[2],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")

textxy(2.75, item.count.fit.nocov[3], formatC(round(item.count.fit.nocov[3],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")
textxy(2.75, direct.fit.nocov[3], formatC(round(direct.fit.nocov[3],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")

abline(c(0,0), lty=5)      
title(main="Plot 2: Overt/Direct (D) and Covert/List (L) Estimates \nThe Netherlands should allow immigrants who are... ", ylab="Proportion who agree", family="TimesNewRoman")
axis<- c(1.25,2,2.75)
axislbl<- c("...of the same race", "...of a different Race", "...poor")
axis(1, at=axis, tick=FALSE, labels=axislbl, family="TimesNewRoman")

#Social desirability bias - same, different, poor
diff.lwr.nocov<- c(same.race.diff.lwr, different.race.diff.lwr, poor.diff.lwr)
diff.upr.nocov<- c(same.race.diff.upr, different.race.diff.upr, poor.diff.upr)
diff.fit.nocov<- c(same.race.direct.list.diff, different.race.direct.list.diff, poor.direct.list.diff)

x<- c(1.25,2,2.75)

plot(x=NULL, y=NULL, ylim=c(0,0.8), xlim=c(1,3), xaxt="n", xlab="", ylab="", bty="n", family="TimesNewRoman", pch=NULL)
segments(y0=c(diff.lwr.nocov[1],diff.lwr.nocov[2],diff.lwr.nocov[3]), 
         x0=c(1.25,2,2.75), 
         y1=c(diff.upr.nocov[1],diff.upr.nocov[2],diff.upr.nocov[3])) 
points(x=x, 
       y=c(diff.fit.nocov[1],diff.fit.nocov[2],diff.fit.nocov[3]))
textxy(1.25, diff.fit.nocov[1]+0.05, "Overt/Direct (D)-Covert/List (L)", offset=0.6, cex=0.8, family="TimesNewRoman")

textxy(1.25, diff.fit.nocov[1], formatC(round(diff.fit.nocov[1],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")

textxy(2, diff.fit.nocov[2], formatC(round(diff.fit.nocov[2],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")

textxy(2.75, diff.fit.nocov[3], formatC(round(diff.fit.nocov[3],2), format="f", digits=2), cex=0.8, family="TimesNewRoman")


abline(c(0,0), lty=5)      
title(main="Plot 3: Social Desirability Bias \nThe Netherlands should allow immigrants who are... ", ylab="Overt/Direct (D) - Covert/List (L)", family="TimesNewRoman")
axis<- c(1.25,2,2.75)
axislbl<- c("...of the same race", "...of a different Race", "...poor")
axis(1, at=axis, tick=FALSE, labels=axislbl, family="TimesNewRoman")






