#Applied Statistics Project


install.packages("robustbase")
install.packages("tigerstats")
install.packages("AID")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("datarium")
install.packages("onewaytests")
install.packages("ggpubr")
install.packages("e1071")
library(onewaytests)
library(ggpubr)
library(rstatix)
library(AID)
library(robustbase)
library(tidyverse)
library(ggplot2)
library(MASS)
library(BSDA)
library(tigerstats)
library(e1071)
getwd()
setwd("C:/Users/90507/Desktop")
data_set_13 <- read.csv("data13.csv")
#In our data set we have NA value that is not so much. Thus, we can replace them with mean of column that have NA values.
data_set_13[28,6] <- mean(data_set_13$birth.week, na.rm = T)
data_set_13[28,7] <- mean(data_set_13$FBS, na.rm = T)
data_set_13[28,8] <- mean(data_set_13$PP1, na.rm = T)
data_set_13[28,9] <- mean(data_set_13$PP2, na.rm = T)
data_set_13[28,10] <- 0
attach(data_set_13)
a <- summary(data_set_13)
as.data.frame(a)
str(data_set_13)

#SIMPLE RANDOM SAMPLING

sub1 <- popsamp(6,data_set_13)
sub1

#SYSTEMATIC SAMPLING
start=sample(1:5,1)
start
systematic.sampling<-c()
for (i in 1:nrow(data_set_13)) {
  if (i %% 5==0) {
    systematic.sampling= rbind(systematic.sampling,data_set_13[i,])
  }
}
systematic.sampling

#STRATIFIED SAMPLING
table(data_set_13$glukoz)
glukoz1 <- subset(data_set_13, glukoz == "1")
glukoz1

glukoz0 <- subset(data_set_13, glukoz == "0")
glukoz0

glukoz.sample <- popsamp(2, glukoz1)
glukoz.sample

glukoz0.sample <- popsamp(5, glukoz0)
glukoz0.sample


stratified.sample <- rbind(glukoz.sample, glukoz0.sample)
stratified.sample

#CLUSTER SAMPLING
class <- unique(age)
class
max(class)
min(class)
class2 <- unique(birth.week)
max(class2)
min(class2)
cl <- sample(class, 1)
cl

cluster.sample <- data_set_13[data_set_13$age == "30",]
cluster.sample

# Research question 1: "Is there positive or significant relationship between mother's
# BMI and birth weight of child?"

BMI_B.WEIGHT=data_set_13[,c(3,5)]

#simple linear reg.


x=BMI
y=birth.weight


# A : Graphical Analysis

# Scatter plot: Scatter plots can help visualize any linear relationships between the dependent (response) variable and independent (predictor) variables. 

plot(BMI,birth.weight,xlab ="BMI",ylab="Birth Weight",pch=16)
abline(fit, col = "red")

# Box plot: To spot any outlier observations in the variable. 

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(x, main="BMI", sub=paste("Outlier rows: ", boxplot.stats(x)$out))  # box plot for 'x : BMI'
boxplot(y, main="Birth Weight", sub=paste("Outlier rows: ", boxplot.stats(y)$out))  # box plot for 'y : birth weight'

# Density plot - Check if the response variable is close to normality

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(x), main="Density Plot: BMI", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(x), 2)))  # density plot for 'speed'
polygon(density(x),col="red")
plot(density(y), main="Density Plot: Birt Weight", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(y), 2)))  # density plot for 'dist'
polygon(density(y), col="red")


# B : second part

fit=lm(birth.weight~BMI,data=data_set_13)  #fit the model by lm()
fit

#Acording to model, intercept is equal to 21.4099 and BMI is equal to 0.4516.


#part C : third part

# The linear relationship seems not fine. But we will check the assumptions by using residuals

yhat=predict(fit)  ### obtaining yhat by predict()
resid=fit$residuals
plot(yhat,resid,pch=16,xlab="fitted values",ylab="residuals")
abline(h=0)


shapiro.test(resid)    # not normal ,transformation method tried.
# The p-value is smaller than 0.05,hence the date not normally distributed.
qqnorm(resid)
qqline(resid)    # there is some outliers.


# Also ,by using CLT ,we can asume the normality since we have 75 obs.However, to use transformation,some methods are tried.

boxcox(fit)   
fit

hist(birth.weight)

shapiro.test(log(birth.weight))    # to check transformed data.The p value >0.05 ,so transformation work.

hist(log(birth.weight))   # it seems like normal.

# parametric approach on transformed data :

birth.weight.transformed = log(birth.weight)
fit.transformed=lm(birth.weight.transformed~BMI,data=data_set_13)  #fit the model by lm()


yhat.trasnsformed=predict(fit.transformed)  ### obtaining yhat by predict()
resid.transformed=fit.transformed$residuals
plot(yhat,resid.transformed,pch=16,xlab="fitted values",ylab="residuals")
abline(h=0)

qqnorm(resid.transformed)
qqline(resid.transformed)

# Identify the sum of squares for the model from the ANOVA table.

anova(fit.transformed) #gives the anova table for regression

summary(fit.transformed)

confint(fit.transformed, level=0.95)
confint(fit.transformed, level=0.99)
confint(fit.transformed, level=0.90)

x=BMI
y=birth.weight

cor(birth.weight.transformed,BMI)

#plot:
ggplot(BMI_B.WEIGHT, aes(birth.weight,BMI)) +
  geom_point() +
  stat_smooth(method = fit)




plot(y~x, data = data_set_13,
     xlab = "birth.weight",
     ylab = "BMI",
     main = "Birth weight vs Body mass index of mother",
     pch  = 16,
     cex  = 1,
     col  = "blue")
abline(fit, lwd = 3, col = "darkorange")


#RQ2

# Research question 2: ??Which independent variable have more effect on child birth-weight.??
#Since arsiv.no cannot affect birt.week, it is ommitted from the model.
plot(data_set_13)
abline(data_set_13)


#we can clearly see distribution is not normal, we need to apply some 
#transformations to change the type of distribution


fitted_model <- lm(birth.weight~.-arsiv.no -glukoz ,data=data_set_13)
shapiro.test(fitted_model$residuals)

qqnorm(fitted_model$residuals)
qqline(fitted_model$residuals)

# data's residuals have nearly normal distrubution let's check other assumptions,


plot(fitted_model,pch=16,1)
abline(fitted_model, col = "red")

# residuals are not balanced around 0 mean

yhat=predict(fitted_model)  ### obtaining yhat by predict()
resid=fitted_model$residuals
plot(yhat,resid,pch=16,xlab="fitted values",ylab="residuals")
abline(h=0, col = "red")

## residual plot

boxcox(lm(birth.weight~.-arsiv.no -glukoz, data = data_set_13))

#In boxcox plot, intervals are closed to   0, that means we should use log transformation


# As it shown we need some transformaitons to fit resudials

fitted_model_t <-lm(log(birth.weight)~.-arsiv.no -glukoz ,data=data_set_13)


qqnorm(fitted_model_t$residuals)
qqline(fitted_model_t$residuals)

anova(fitted_model_t)

# log transformation made our model more accuratee and d??strubution is close to normal now

fitted_model_t=lm(log(birth.weight)~.-arsiv.no -glukoz ,data=data_set_13)  #fit the model by lm()
plot(fitted_model_t,pch=16,1)
abline(fitted_model_t, col = "red")



yhat=predict(fitted_model_t)  ### obtaining yhat by predict()
resid=fitted_model_t$residuals
plot(yhat,resid,pch=16,xlab="fitted values",ylab="residuals", main=("Transformed Residuals vs Fitted"))
abline(h=0, col = "red")

# Residuals are distrubuted equally

fitted_model_t$coefficients

# Our model become like 
# birth.weight = -4.6785677712 + -0.0147579998*age + 0.0263746905*BMI + 0.0258265932*WGDP 
# + 0.1734437516*birth.week +  0.0065684590*FBS + 0.0015968531*PP1 + -0.0008792591*PP2

# If we increase birth.week 1 unit, our birth.weight increase 0.1734437516 unit
# and this is the largest increase in our model

# Research Question 3: Is the average weight gained during pregnancy more than 10?
# h0: mu <= 10
# ha: mu > 10
mean(WGDP)
var(WGDP)
x <- WGDP
par(mfrow=c(1,2))
h<-hist(x, breaks=10, col="red", xlab="Weight Gained Pregnancy",
        main="Weight Gained During Pregnancy ")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

qqnorm(WGDP)
qqline(WGDP, col = "red")
shapiro.test(WGDP)
t.test(WGDP, alternative = "greater", mu = 10, conf.level = 0.95)
# Since p-value<0.05, we reject the H0.It is assumed that the data is not normally distributed.

# Research Question 4:

#Is there any significiant relationship between mother??s FBS and child birth-weight?

FBS_WEIGHT <- data_set_13[,c(7,5)]

fitted_model=lm(birth.weight~FBS,data=data_set_13)  #fit the model by lm()
plot(FBS,birth.weight,xlab ="FBS",ylab="Birth Weight",pch=16)
abline(fitted_model, col = "red")

#Now, we will check the assumptions by using residuals
yhat=predict(fitted_model)  ### obtaining yhat by predict()
resid=fitted_model$residuals
plot(yhat,resid,pch=16,xlab="fitted values",ylab="residuals")
abline(h=0,col="red")

qqnorm(resid)
qqline(resid) 

#From Normal QQ plot residuals are not lined on the straight dashed line. So, normality do not seem to be satisfied.

#We can conclude that, our data is not normal by looking Q-Q Plot.

#Shapiro Wilk test checks the normality of the variable. In this approach;

#H0: the data is normally distributed
#H1: the data is not normally distributed


shapiro.test(resid) 


#The p value of shapiro wilk test is smaller than 0.05 so we can reject the null hypothesis, 
#and we can say that the response is not normally distributed.



boxcox(fitted_model)

hist(birth.weight)

hist(log(birth.weight)) 

# After log transformation, we can obtain normal data.
#Now, we can do linear regression on transformed data. 

fitted_transformed <- lm(log(birth.weight)~FBS,data=data_set_13)
plot(FBS,log(birth.weight),xlab ="FBS",ylab="Birth Weight",pch=16)
abline(fitted_transformed, col = "red")



yhat_trans=predict(fitted_transformed)  ### obtaining yhat by predict()
resid_trans=fitted_transformed$residuals
plot(yhat_trans,resid_trans,pch=16,xlab="fitted values",ylab="residuals")
abline(h=0)

#The residuals bounce randomly around the 0 line which suggests that the linear relationship assumption is reasonable. 
#Moreover, there is no pattern in the plot which indicates that it is reasonable to assume that error term has 0 mean and constant variance


qqnorm(resid_trans)
qqline(resid_trans)

#Again we can see that this data is normally distributed after transformation.

anova(fitted_transformed)

#Firstly, we define the hypothesis about the significance of the model

#H0: Model is insignificant
#H1: Model is significant


#F value is 4.465 and p value is 0.038 which less than 0.05. Thus, we reject the null hypothesis. 
#It can be concluded that the model is significant


confint(fitted_transformed, level=0.90)
confint(fitted_transformed, level=0.99)
#The confidence interval for ß0 is (2.211702 , 3.374492) which does not contains 0 so the ß0 is significant.

#The confidence interval for ß1 is (0.000369 , 0.012909) which does not contains 0 so the ß1 is significant.

# Question 5 :
# Is the average birth weight of the children of mother under 30, 30-35 and over 35 is equal to each other?

attach(data_set_13)

group1= data_set_13[age<30,]
group2 =data_set_13[age>30 & age<35,]
group3=data_set_13[age>35,]




#Ho= : µ(group1)=µ(group2)=µ(group3)
#Ha: not all equal


# In order to get an idea about the mean of each group, let's visualize the data




boxplot(group1$birth.weight,group2$birth.weight,group3$birth.weight)



# to perform anova test :
group1$age ="under30"
group1

group2$age = "btw30-35"
group2

group3$age ="35+"
group3


grouped_age =rbind(group1,group2,group3)



anova=aov(grouped_age$birth.weight ~ grouped_age$age , data = data_set_13)
summary(anova)


# According to ANOVA test, we cannot reject the null hypothesis which is H0:??1=??2=??3=0H0:??1=??2=??3=0 because the p value(0.265) is greater than 0.05




# pairwise comparision :
TukeyHSD(anova)

# it can be seen from the test output that p-adj values are higher than 0.05.Thus, their means are equal to each other.

# Check assumptions :
# Independence of the observations. Each subject should belong to only one group. There is no relationship between the observations in each group. Having repeated measures for the same participants is not allowed.
# Normality. the data for each design cell should be approximately normally distributed.and no significant outliers.
# Homogeneity of variances. The variance of the outcome variable should be equal in every cell of the design.
# variances are equal

# for normalyt check :
shapiro.test(residuals(anova))

# shapiro.test(boxcox(anova))

plot(anova,2)


# to check HOMOGENEITY OF THE VARIANCE

plot(anova, 1)

bartlett.test( grouped_age$birth.weight ~ grouped_age$age , data = data_set_13)


# Normality. the data for each design cell should be approximately normally distributed and no major outliers .
# significant outliers checked with qqnorm and qqline .


# normalty assumption and homogenty of variance assumptions are  not satisfied.Hence , use a non-parametric alternative (Kruskal-Wallis test)


# all groups are not  normally distributed .Hence , use a non-parametric alternative (Kruskal-Wallis test) .


group1$age ="under30"
group1

group2$age = "btw30-35"
group2

group3$age ="35+"
group3


grouped_age =rbind(group1,group2,group3)



out=kw.test(grouped_age$birth.weight ~ grouped_age$age , data = data_set_13)
#We are going to get an error now because Pairwise comparisons could not be performed since difference is not statistically significant (alpha = 0.05).
paircomp(out)

describe(grouped_age$birth.weight ~ grouped_age$age , data = grouped_age)
