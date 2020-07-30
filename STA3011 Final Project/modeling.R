library(ggplot2)
library(reshape2)
library(DAAG)
library(lattice)
library(caret)
library(pROC)
library(randomForest)
library(heuristica)
library(car)
library(leaps)
setwd("~/Downloads/online-News-Popularity-Prediction-master/data set/Raw")
raw<-read.csv("OnlineNewsPopularity.csv")
raw[,61] <- as.numeric(raw[,61])


#Plot all the variable data as boxplot
par(mfrow=c(3,4))
for(i in 2:length(raw)){boxplot(raw[,i], xlab=names(raw)[i])}

#i=1 is not considered because 1 is URL which is not numeric and hence cannot be plotted
#Plot all the variable data by histogram to check the distributions
par(mfrow=c(3,4))
for(i in 2:length(raw)){hist(raw[,i], xlab=names(raw)[i])}

#Remove missing data i.e remove rows with n_token_content as 0. 
#1181 rows are removed
modifiedData<-raw[raw[,4]!=0,]
dim(modifiedData)

#Removing outlier - delete rows with value >1 for n_non_stop_words. 1 row is removed
modifiedData<-modifiedData[modifiedData[,6]<=1,]

par(mfrow=c(1,1))	  #Use this to display only one plot in the page
hist(modifiedData$shares,main=" Histogram on number of shares ", xlab=" Number of shares ", ylab=" Frequency " ,sub="Before transformation")
summary(powerTransform(modifiedData$shares))



modifiedData$normalizedshares<-log(modifiedData$shares) #log transformation
boxplot(modifiedData$normalizedshares,names = "Shares",main="Box plot on number of shares",
        ylab="Number of shares",sub="After transformation")
hist(modifiedData$normalizedshare,main="Histogram on number of shares",
     xlab="Normalized number of shares",ylab="Frequency",sub="After transformation")	


boxplotstats<-boxplot(modifiedData$normalizedshare)$stats
print(boxplotstats)
minimum<-boxplotstats[,1][1]
lowerhinge<-boxplotstats[,1][2]
median<-boxplotstats[,1][3]
upperhinge<-boxplotstats[,1][4]
maximum<-boxplotstats[,1][5]
#calculation for mild outliers 
hspread<-upperhinge-lowerhinge
lowerouterfence<-lowerhinge-3*hspread
upperouterfence<-upperhinge+3*hspread
print(hspread)
print(lowerouterfence)
print(upperouterfence)

#removing values beyond the fences
modifiedData<-modifiedData[!(modifiedData$normalizedshare>=upperouterfence | modifiedData$normalizedshare<=lowerouterfence),]
boxplot(modifiedData$normalizedshare,xlab="Normalized Shares",main="Shares after extreme oulier removal")

#4,6不能动
for(i in c(4,9,10,11,21,23,24,27,28,29,30,31,52))
{
  if(min(modifiedData[i])==0){
    print(names(modifiedData[i]))
    modifiedData[i]<-sqrt(modifiedData[i]); names(modifiedData)[i] <- paste("sqrt_",names(modifiedData)[i], sep="")
  }
  else{
    print(names(modifiedData[i]))
    modifiedData[i]<-log(modifiedData[i]); names(modifiedData)[i] <- paste("log_",names(modifiedData)[i], sep="")
  } 
}

#Transformation
modifiedData[8]<-log(modifiedData[8]+1)
names(modifiedData)[8]<- paste("sqrt_",names(modifiedData)[8], sep="")

validattrs<-3:62
validset<-modifiedData[validattrs]

fit1<-lm(normalizedshares~.,data=validset)
summary(fit1)

colinear <- c(4,36,37,42)
validset2<-validset[-colinear]
fit1<-lm(normalizedshares~.,data=validset2)
summary(fit1)
vif(fit1)

plot(sort(vif(fit1)), ylab = 'vif of fit', type='h')
abline(h=5,col='red')


#delete some highly dependent attributes
fit2<-lm(normalizedshares~n_tokens_title+sqrt_num_hrefs+sqrt_num_self_hrefs+sqrt_num_imgs+sqrt_num_videos+average_token_length 
         +num_keywords+data_channel_is_lifestyle+data_channel_is_entertainment+data_channel_is_socmed+kw_min_min+sqrt_kw_max_min
         +kw_avg_min+sqrt_kw_min_max+sqrt_kw_max_max+kw_avg_max+kw_min_avg+weekday_is_monday+weekday_is_tuesday+
           weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+weekday_is_saturday+LDA_00+LDA_01+LDA_02+LDA_04+global_subjectivity
         +global_rate_positive_words+avg_positive_polarity+sqrt_min_positive_polarity+max_positive_polarity+min_negative_polarity+
           max_negative_polarity+title_subjectivity+title_sentiment_polarity+abs_title_subjectivity+abs_title_sentiment_polarity)

vif(fit2)
plot(sort(vif(fit2)), ylab = 'vif of fit', type='h')

regfit.full<-regsubsets(normalizedshares~n_tokens_title+sqrt_num_hrefs+sqrt_num_self_hrefs+sqrt_num_imgs+sqrt_num_videos+average_token_length 
                        +num_keywords+data_channel_is_lifestyle+data_channel_is_entertainment+data_channel_is_socmed+kw_min_min+sqrt_kw_max_min
                        +kw_avg_min+sqrt_kw_min_max+sqrt_kw_max_max+kw_avg_max+kw_min_avg+weekday_is_monday+weekday_is_tuesday+
                          weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+weekday_is_saturday+LDA_00+LDA_01+LDA_02+LDA_04+global_subjectivity
                        +global_rate_positive_words+avg_positive_polarity+sqrt_min_positive_polarity+max_positive_polarity+min_negative_polarity+
                          max_negative_polarity+title_subjectivity+title_sentiment_polarity+abs_title_subjectivity+abs_title_sentiment_polarity,
                        data=validset,nvmax = 30)

regsum<- summary(regfit.full)
#不想用太多参数，希望简化模型，使用BIC
regsum$bic

print(regsum$which[26,])

reduced<-lm(normalizedshares~sqrt_num_hrefs+sqrt_num_self_hrefs+sqrt_num_imgs+sqrt_num_videos+
              average_token_length+num_keywords+data_channel_is_entertainment+data_channel_is_socmed+kw_min_min+
              sqrt_kw_max_min+sqrt_kw_min_max+sqrt_kw_max_max+kw_avg_max+kw_min_avg+sqrt_kw_max_max+
              kw_avg_max+kw_min_avg+weekday_is_monday+weekday_is_tuesday+weekday_is_wednesday+
              weekday_is_thursday + weekday_is_friday+LDA_00+LDA_02+global_subjectivity+sqrt_min_positive_polarity+
              title_subjectivity+title_sentiment_polarity+abs_title_subjectivity)

sum1<- summary(reduced)
par(mfrow=c(2,2))
plot(reduced)

#remove high leverage points, 36704, 23985, 31004, 16134, 27761, 9574
validset<-validset[-c(9574,16134,31004,23985,36704,27761,16120),]
fit3<-lm(normalizedshares~sqrt_num_hrefs+sqrt_num_self_hrefs+sqrt_num_imgs+sqrt_num_videos+
              average_token_length+num_keywords+data_channel_is_entertainment+data_channel_is_socmed+kw_min_min+
              sqrt_kw_max_min+sqrt_kw_min_max+sqrt_kw_max_max+kw_avg_max+kw_min_avg+sqrt_kw_max_max+
              kw_avg_max+kw_min_avg+weekday_is_monday+weekday_is_tuesday+weekday_is_wednesday+
              weekday_is_thursday + weekday_is_friday+LDA_00+LDA_02+global_subjectivity+sqrt_min_positive_polarity+
              title_subjectivity+title_sentiment_polarity+abs_title_subjectivity,data=validset)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

#predict using test set
set.seed(1)
test=sample(seq(1,length(validset$normalizedshares)), size=0.2*length(validset$normalizedshares),replace = FALSE)
trainset<- validset[-test,]
testset<- validset[test,]
predictlm<-lm(normalizedshares~sqrt_num_hrefs+sqrt_num_self_hrefs+sqrt_num_imgs+sqrt_num_videos+
                average_token_length+num_keywords+data_channel_is_entertainment+data_channel_is_socmed+kw_min_min+
                sqrt_kw_max_min+sqrt_kw_min_max+sqrt_kw_max_max+kw_avg_max+kw_min_avg+sqrt_kw_max_max+
                kw_avg_max+kw_min_avg+weekday_is_monday+weekday_is_tuesday+weekday_is_wednesday+
                weekday_is_thursday + weekday_is_friday+LDA_00+LDA_02+global_subjectivity+sqrt_min_positive_polarity+
                title_subjectivity+title_sentiment_polarity+abs_title_subjectivity,data=trainset)
predictvalue<-predict(predictlm,newdata = testset)
mse<-mean((predictvalue-log(testset$shares))^2)

residual<-predictvalue-testset$normalizedshares
plot(predictvalue,residual)
abline(lm(residual~predictvalue),col='red')


#covariate part
covariateset<-validset[c(6,7,8,9,10,11,13,15,18,19,21,22,23,24,30,31,32,33,34,38,40,43,50,55,57,60)]
detach(validset)
attach(covariateset)
fit4<-lm(normalizedshares~.+.^2,data=covariateset)

null<-lm(normalizedshares~1,data=covariateset)

#use BIC to do step functions
fwd<-step(null,scope = formula(fit4),direction = 'forward',k=log(length(fit4$residuals)))
summary(fwd)
par(mfrow=c(2,2))
plot(fwd)

#test sets
testcov<-covariateset[test,]
traincov<-covariateset[-test,]

predictlm2<-lm(formula = formula(fwd), data=traincov)
predictvalue2<-predict(predictlm2,newdata = testcov)
residual2<-predictvalue2-testcov$normalizedshares
plot(predictvalue2,residual2)
abline(lm(residual2~predictvalue2),col='red')
mse=mean(residual2^2)
mse


#mmps
par(mfrow=c(3,3))
mmps(fwd)
