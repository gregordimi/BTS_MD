


summarydata <- s_data[!is.na(s_data$Q44),]


aggregate(as.numeric(s_data$Duration..in.seconds.),by=list("Status") , FUN = mean)

sum(s_data$CJNT1==1)

sum(s_data$BCJNT1==1)

sum(s_data$CJNT9==1)

sum(s_data$BCJNT9==1)

sum(s_data$cjnt11==1)

sum(s_data$bcjnt11==1)


summary(factor(summarydata$Q44, levels = 1:2,labels = c("male","female")))



summary(factor(summarydata$Q45, levels = 11:19, labels = c("18-","18-24","25-34","35-44","45-54","55-64","65-74","76-84","85+")))

summary(factor(summarydata$Q46, levels = 1:2,labels = c("student","employed")))


