rm(list=ls(all=TRUE))
library(data.table) 
library(caTools)
library(neuralnet)
#library(h2o)

context = read.csv('OnlineNewsPopularity.csv')
context1 = context[,-c(1,2)]
summary(context1)
context1=context1[!context1$n_unique_tokens==701,]
summary(context1)
mean(context1$shares)

context1$shares<-ifelse(context1$shares>3395,1,0)
summary(context1)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

context2 <- as.data.frame(lapply(context1, normalize))

set.seed(100)
split = sample.split(context2$shares, SplitRatio = 0.70)
training_set = subset(context2, split == TRUE)
test_set = subset(context2, split == FALSE)

training_set[,1:58] = scale(training_set[,1:58])
test_set[,1:58] = scale(test_set[,1:58])

#I manually ran code for different threshold values and hidden layers as mentoned in report

library(neuralnet)
nn <- neuralnet(shares ~ n_tokens_title + n_tokens_content + n_unique_tokens + n_non_stop_words + n_non_stop_unique_tokens + num_hrefs + num_self_hrefs + num_imgs + num_videos + average_token_length + num_keywords + data_channel_is_lifestyle + data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech + data_channel_is_world + kw_min_min + kw_max_min + kw_avg_min + kw_min_max + kw_max_max + kw_avg_max + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + self_reference_max_shares + self_reference_avg_sharess + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday + is_weekend + LDA_00 + LDA_01 + LDA_02 + LDA_03 + LDA_04 + global_subjectivity + global_sentiment_polarity + global_rate_positive_words + global_rate_negative_words + rate_positive_words + rate_negative_words + avg_positive_polarity + min_positive_polarity + max_positive_polarity + avg_negative_polarity + min_negative_polarity + max_negative_polarity + title_subjectivity + title_sentiment_polarity + abs_title_subjectivity + abs_title_sentiment_polarity , data=training_set, hidden=c(3,1),act.fct="logistic", linear.output=FALSE, threshold=0.5)
nn$result.matrix
plot(nn)

temp_test <- subset(test_set, select = c("n_tokens_title", "n_tokens_content", "n_unique_tokens", "n_non_stop_words", "n_non_stop_unique_tokens", "num_hrefs", "num_self_hrefs", "num_imgs", "num_videos", "average_token_length", "num_keywords", "data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world", "kw_min_min", "kw_max_min", "kw_avg_min", "kw_min_max", "kw_max_max", "kw_avg_max", "kw_min_avg", "kw_max_avg", "kw_avg_avg", "self_reference_min_shares", "self_reference_max_shares", "self_reference_avg_sharess", "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday", "is_weekend", "LDA_00", "LDA_01", "LDA_02", "LDA_03", "LDA_04", "global_subjectivity", "global_sentiment_polarity", "global_rate_positive_words", "global_rate_negative_words", "rate_positive_words", "rate_negative_words", "avg_positive_polarity", "min_positive_polarity", "max_positive_polarity", "avg_negative_polarity", "min_negative_polarity", "max_negative_polarity", "title_subjectivity", "title_sentiment_polarity", "abs_title_subjectivity", "abs_title_sentiment_polarity"))
head(temp_test)
nn.results <- compute(nn, temp_test)
nn.results

#Accuracy
results <- data.frame(actual = test_set$shares, prediction = nn.results$net.result)
results
results[,2]<-ifelse(results[,2]>mean(results[,2]),1,0)
roundedresults<-sapply(results,round)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
X<-table(actual,prediction)
X
prediction_rate2=(X[1,1]+X[2,2])/sum(X)
print(paste("Prediction rate: ",prediction_rate2))
print(paste("Error rate: ",1-prediction_rate2))

