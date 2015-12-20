---
title: "Assignment Reproducible research"
output: html_document
---

```{r}
rep_data<-read.table("rep_data.csv",header = TRUE,sep=",")
rep_data$date<-as.Date(rep_data$date,format("%m/%d/%Y"))
```

### Mean number of steps

We have to first clean the data and then aggregate for dates, this can be done using the function tapply
```{r}
clean.data<-rep_data[!is.na(rep_data$steps),]
agg.data<-tapply(clean.data[,1],clean.data[,2],sum)
agg.data<-agg.data[!is.na(agg.data)]
head(agg.data)
hist(agg.data,breaks=20,ylab="Number of steps")
mean(agg.data)
median(agg.data)
```
Figure 1 gives the histogram of the number of steps.
We see that the mean is 10766.19 and the median is 10765 steps.

Another option would have been to use the by function, but this doesn't give an array.
```{r}
agg.data.by<-by(clean.data[,1],clean.data$date,sum)
head(agg.data.by[!is.na(agg.data.by)])
```

### Average daily activity pattern

```{r}
agg.data2<-tapply(clean.data[,1],clean.data[,3],mean)
agg.data2<-agg.data2[!is.na(agg.data2)]
plot(agg.data2,type="l",main="Average activity per day",xlab="interval",ylab="Average number of steps")
match(max(agg.data2),agg.data2)
max(agg.data2)
```
As can be seen above, the maximum value is found for interval 104, the value is 206.1698. Figure 2 gives the time series of the average activity per day.

### Missing values

Let's first look, how many missing values there are. The function summary gives us that there are 2304 NA's in steps: 

```{r}
length(which(is.na(rep_data$steps)))
NA_index<-which(is.na(rep_data$steps))

rep_data2<-rep_data

rep_data2$steps[NA_index] <- agg.data2[as.character(rep_data2$interval[NA_index])]
agg.data.supplemented<-tapply(rep_data2$steps,rep_data2$date,sum)
hist(agg.data.supplemented,20,xlab="steps",main="Supplemented data")

mean(agg.data.supplemented)
median(agg.data.supplemented)
mean(agg.data.supplemented)-mean(agg.data)
```
We see that the mean and median are equal to 10766.19 steps. The difference between the supplemented and mean of the incomplete data is zero. Figure 3 gives a histogram of the supplemented data.

Filling in the missing values doesn't change the mean number of steps of the entire dataset as can be seen above.

### Comparison of weekdays and weekends

```{r}
#Weekday selection
dayVector<-weekdays(rep_data2$date)
fullVector=1:length(dayVector)
weekendVector<-which(dayVector %in% c("Saturday","Sunday"))
weekVector<-setdiff(fullVector,weekendVector)

#Aggregation and plots
rep_data.week<-rep_data2[weekVector,]
rep_data.weekend<-rep_data2[weekendVector,]
agg.data.week<-tapply(rep_data.week[,1],rep_data.week[,3],mean)
agg.data.weekend<-tapply(rep_data.weekend[,1],rep_data.weekend[,3],mean)

xweek<-as.numeric(names(agg.data.week))
xweekend<-as.numeric(names(agg.data.weekend))

par(mfrow=c(2,1))
plot(xweek,agg.data.week,type="l",main="Activity week",xlab="interval",ylab="steps",col='red')
plot(xweekend,agg.data.weekend,type="l",main="Activity weekends",xlab="interval",ylab="steps",col='blue')
```

Figure 4 gives the time series of the weekly and weekend data.