# We first load necessary libraries and functions
library('ggplot2')
library(dplyr)
library(plyr)
library(hrbrthemes)
library(car)

### START Open data file ###
# Insert code below that loads your dataset into a variable called 'rawdata'

setwd("/your/working/directory/here")

#Link to your own Helper functions
source('helper_functions.r')

rawdata = read.csv("dataset16.csv")

### END Open data file ###


### START Explore through visual inspection ###
# Insert code below to check your dataset by looking at the raw data
rawdata
head(rawdata)

#missing data? 
unique(is.na(rawdata))

plot(rawdata$ID, rawdata$rt)

### END Explore through visual inspection ###


### START How large is N? ###
# Insert code below to check how many subjects does the dataset contains

unique <- unique(rawdata$ID)
n<-length(unique)
n
length(unique)

### END How large is N? ###


### START Change condition and correct to factor ###
# For ggplot, we need to make sure our condition and correct columns are coded as factors
# Insert code below to change these columns to factors

rawdata$condition <- as.factor(rawdata$condition)
rawdata$correct <- as.factor(rawdata$correct)

### END Change condition and correct to factor ###



### START Show histogram of distribution ###
# Insert code below to look at the distribution of response times, using either ggplot or hist()
# It may be necessary to reduce the limits of the x-axis

ggplot(data=rawdata, aes(x=rt))+
  geom_histogram(bins=500)+
  xlim(250,1200)

### END Show histogram of distribution ###

### START Remove extreme values

# Insert code below to remove outliers
boxplot(rawdata$rt)

#one option would be to exclude outliers based on quantiles 
#we are not going to use this. See below for solution with 2 Standard Deviations
#Quantile Cutoff
#Q99 <- quantile(rawdata$rt, probs=c(.99), na.rm = FALSE)
#Q99
#rawdataq99 <- rawdata[!(rawdata$rt > Q99),]
#boxplot(rawdataq99$rt)
#max(rawdataq99$rt)

#Q95 <- quantile(rawdata$rt, probs=c(.95), na.rm = FALSE)
#Q95
#rawdataq95 <- rawdata[!(rawdata$rt > Q95),]
#boxplot(rawdataq95$rt)
#max(rawdataq95$rt)

#ggplot(rawdata5, aes(group=condition, x=condition, y=rt))+
#  geom_boxplot()
  

#the more widely applied way is via standard deviation
#determining cutoff value for outliers via 2SD, because lexical decision task

summary(rawdata)
twosd <- 2*sd(rawdata$rt)
twosd

cutoff<- mean(rawdata$rt) + twosd
cutoff

mean(rawdata$rt) - twosd
min(rawdata$rt)


cutoff_values <- rawdata[(rawdata$rt > cutoff),]
cutoff_values
summary(cutoff_values)
#132 excluded datapoints

rawdata2 <- rawdata[!(rawdata$rt > cutoff),]
boxplot(rawdata2$rt)
max(rawdata2$rt)

#Plot it again
ggplot(data=rawdata2, aes(x=rt, fill=condition)) +
  geom_histogram(bins=100, color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(name = "Condition", labels = c("Distractors", "No Distractors", "C"), values=c("#5C9090", "#E9B666")) +
  theme_ipsum() +
  labs(y = "Count", x = "Reaction time [in ms]", fill="")+
  xlim(250, 1300)

#Berger & Kiefer, 2021: using 2SD above mean 

### END Remove extreme values


### START Show histogram for correct and incorrect
# Insert ggplot code to show RTs for both correct and incorrect as overlaying distributions
# HINT: http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# Find the mean of each group

rawdata2_corr <- ddply(rawdata2, "correct", summarise, rt.mean=mean(rt))
rawdata2_corr

rawdata2_rt <- ddply(rawdata2, "condition", summarise, rt.median=median(rt))
rawdata2_rt

rawdata2_sd <- ddply(rawdata2, "condition", summarise, rt.sd=sd(rt))
rawdata2_sd

#Histograms with means
ggplot(rawdata2, aes(x=rt, colour=correct)) +
  geom_histogram(bins=200) +
  geom_vline(data=rawdata2_corr, aes(xintercept=rt.mean,  colour=correct),
             linetype="dashed", size=0.75)

### END Show histogram for correct and incorrect



### START Do the same for condition 1 vs condition 2
# Insert ggplot code to show RTs for both condition 1 and condition 2 as overlaying distributions
rawdata2_cond <- ddply(rawdata2, "condition", summarise, rt.median=median(rt))
rawdata2_cond

ggplot(rawdata2, aes(x=rt, colour=condition)) +
  geom_histogram(bins=100) +
  geom_vline(data=rawdata2_cond, aes(xintercept=rt.median,  colour=condition),
             linetype="dashed", size=0.75)

### END Do the same for condition 1 vs condition 2

### START Inspect data: paired t-test over RT data by condition
# Insert code below to check whether RTs differ by condition. Note: you should do this by aggregating data
# by subject, using the median (due to skewed distribution of response times!)

#find median reaction time, also per condition (and standard deviation for report)

rawdata_median <- aggregate(rawdata2$rt, by = list(ID = rawdata2$ID, condition = rawdata2$condition), FUN="median")
rawdata_median

rawdata_median1 <- rawdata_median[(rawdata_median$condition ==1),]
summary(rawdata_median1)
sd(rawdata_median1$x)

rawdata_median2 <- rawdata_median[(rawdata_median$condition ==2),]
summary(rawdata_median2)
sd(rawdata_median2$x)

rawdata3 <- rawdata2
rawdata3$correct <- as.numeric(rawdata3$correct)
rawdata3$correct <- rawdata3$correct - 1
rawdata3

#find mean accuracy, for both conditions
rawdata_mean_acc <- aggregate(rawdata3$correct, by = list(ID = rawdata2$ID, condition = rawdata2$condition), FUN="mean")
rawdata_mean_acc

mean_acc1 <- rawdata_mean_acc[(rawdata_mean_acc$condition == 1),]
summary(mean_acc1)

mean_acc2 <- rawdata_mean_acc[(rawdata_mean_acc$condition == 2),]
summary(mean_acc2)

res <- t.test(x ~ condition, data = rawdata_median, paired = TRUE)
res

acc <- t.test(x ~ condition, data  = rawdata_mean_acc, paired= TRUE)
acc

### END Inspect data: paired t-test over RT data by condition


### START Fit model for each participant x condition
# Now we get down to business. The fit_data() function takes one input: a dataframe. It returns a named list of length 5,
# for the 5 parameters we are estimating. It is your job to think of a good data structure to use for subsequent analysis.
# Make sure you pass only a subset of your dataframe! Insert code below.

#!!! Need to re run the whole code WITHOUT the following two lines: 
#rawdata$condition <- as.factor(rawdata$condition)
#rawdata$correct <- as.factor(rawdata$correct)

#data for participants 01-12 on condition 01-02
fit_data(subset(rawdata2, ID==1 & condition ==1))
fit_data(subset(rawdata2, ID==2 & condition ==1))
fit_data(subset(rawdata2, ID==3 & condition ==1))
fit_data(subset(rawdata2, ID==4 & condition ==1))
fit_data(subset(rawdata2, ID==5 & condition ==1))
fit_data(subset(rawdata2, ID==6 & condition ==1))
fit_data(subset(rawdata2, ID==7 & condition ==1))
fit_data(subset(rawdata2, ID==8 & condition ==1))
fit_data(subset(rawdata2, ID==9 & condition ==1))
fit_data(subset(rawdata2, ID==10 & condition ==1))
fit_data(subset(rawdata2, ID==11 & condition ==1))
fit_data(subset(rawdata2, ID==12 & condition ==1))

fit_data(subset(rawdata2, ID==1 & condition ==2))
fit_data(subset(rawdata2, ID==2 & condition ==2))
fit_data(subset(rawdata2, ID==3 & condition ==2))
fit_data(subset(rawdata2, ID==4 & condition ==2))
fit_data(subset(rawdata2, ID==5 & condition ==2))
fit_data(subset(rawdata2, ID==6 & condition ==2))
fit_data(subset(rawdata2, ID==7 & condition ==2))
fit_data(subset(rawdata2, ID==8 & condition ==2))
fit_data(subset(rawdata2, ID==9 & condition ==2))
fit_data(subset(rawdata2, ID==10 & condition ==2))
fit_data(subset(rawdata2, ID==11 & condition ==2))
fit_data(subset(rawdata2, ID==12 & condition ==2))

fit_con_id <- c()

for (id in 1:12){
  for(cond in 1:2){
    fit_con_id <- rbind(fit_con_id, c(fit_data(subset(rawdata2, condition == cond & ID == id)), id, cond))
  }
}

as.matrix(fit_con_id)
colnames(fit_con_id) <- c("s", "A", "ter", "b", "v1", "ID", "condition")
fit2 <- data.frame(fit_con_id)

#t-tests on parameters
ts <- t.test(s ~ condition, data = fit2, paired = TRUE)
ts
tA <- t.test(A ~ condition, data = fit2, paired = TRUE)
tA
tter <- t.test(ter ~ condition, data = fit2, paired = TRUE)
tter
tb <- t.test(b ~ condition, data = fit2, paired = TRUE)
tb
tv1 <- t.test(v1 ~ condition, data = fit2, paired = TRUE)
tv1

fit2
summary(fit2$s)

#Boxplots for individual parameters

par(mfrow = c(3, 2))
Boxplot(fit2$s~ fit2$condition,ylab="s - Standard Deviation Drift Rate",xlab="", id.method ="y", names = c("Distractor", "No Distractor"))
Boxplot(fit2$A~ fit2$condition,ylab="A - Bias",xlab="", id.method ="y", names = c("Distractor", "No Distractor"))
Boxplot(fit2$ter~ fit2$condition,ylab="t0 - Non-Decision Time",xlab="", id.method ="y", names = c("Distractor", "No Distractor"))
Boxplot(fit2$b~ fit2$condition,ylab="b - ResponseThreshold",xlab="", id.method ="y", names = c("Distractor", "No Distractor"))
Boxplot(fit2$v1~ fit2$condition,ylab="v - Mean Drift Rate",xlab="", id.method ="y", names = c("Distractor", "No Distractor"))

### END Fit model for each participant x condition


#Extra:
## Defective Cumulative Probability Graph
library(rtdists)
curve(pLBA(x, response = 1, A=315.212, b=384.858, t0 = 267.444, mean_v=c(0.624, 1.039), sd_v=c(0.206,0.279)), 
      col=c("red"),xlim = c(0, 1200), ylim = c(0,1), 
      ylab = "probability", xlab = "response time [in ms]",
      main = "Cumulative Probability")
curve(pLBA(x, response = 2, A=349.841, b=383.254, t0 = 319.241, mean_v=c(0.624, 1.039), sd_v=c(0.206,0.279)), 
      add=TRUE, lty = 2, col=c("blue"))
legend("topleft", legend=c("Distrator", "No Distractor"), col=c("red", "blue"), title="Condition", lty=1:2, cex=0.7)

#no time for the interpretation of this graph
