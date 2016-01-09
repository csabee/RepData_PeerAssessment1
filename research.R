##
## Activity.csv should be in the R work directory
##
## Read csv file
f <- read.csv("activity.csv")
## omitted NA values
fwithoutna <- f[!is.na(f$steps),]
fsub <- data.frame(fwithoutna$date,fwithoutna$steps)
names(fsub) <- c("date", "steps")
## sum
agg_sum <- aggregate(fsub$steps,list(fsub$date),sum)
## plot sum
barplot(
    height=agg_sum$x,
    main="Total Steps per Day",
    xlab="Date",
    ylab="Steps",
    names.arg=agg_sum$Group.1,
    space=c(0)
)
## mean
agg_mean <- aggregate(fsub$steps,list(fsub$date),mean)
steps_mean <- mean(agg_sum$x)
print(steps_mean)
## median
steps_median <- median(agg_sum$x)
print(steps_median)

##
fsub2 <- data.frame(fwithoutna$interval,fwithoutna$steps)
names(fsub2) <- c("interval","steps")
## aggregate interval mean
agg_stepsinterval <- aggregate(fsub2$steps,list(fsub2$interval),mean)
plot(
    x=agg_stepsinterval$Group.1,
    y=agg_stepsinterval$x,
    type="l",
    main="Time-Series of daily Average Steps",
    xlab="Interval",
    ylab="Average Steps"
)

## calculate NAs
fisna <- f[is.na(f$steps),]
length(fisna$steps)

## transform a new variable to contain the interval average step instead of the NA values
ftrans <- f
for( counter in 1:length(f$steps) ){
    if(is.na(f$steps[counter])){
        newsteps <-
            agg_stepsinterval[agg_stepsinterval$Group.1 == f$interval[counter],]$x    
        ftrans$steps[counter] <- newsteps
    }
}
## without missing values
agg_sumall <- aggregate(ftrans$steps,list(ftrans$date),sum)
##plot it
barplot(
    height=agg_sumall$x,
    main="Total Steps per Day",
    xlab="Date",
    ylab="Steps",
    names.arg=agg_sumall$Group.1,
    space=c(0)
)

## mean for all values
steps_meanall <- mean(agg_sumall$x)
print(steps_meanall)
## median for all values
steps_medianall <- median(agg_sumall$x)
print(steps_medianall)

## 
fweekdays <- data.frame(ftrans$steps,ftrans$date, ftrans$interval, c(1:length(ftrans$steps)) )
names(fweekdays )<-c("steps","date","interval","weekday")

for( counter in 1:length(fweekdays$steps) ){
    
    currday <- weekdays(as.Date(fweekdays$date[counter]))
    
    if(currday == "szombat" || currday == "vasárnap" 
       || currday == "saturday" || currday == "sunday"){
        fweekdays$weekday[counter] <- 0
    }else{
        fweekdays$weekday[counter] <- 1
    }
}

## filtered variables 
fweek0 <- fweekdays[fweekdays$weekday == 0,]
fweek1 <- fweekdays[fweekdays$weekday == 1,]
## plot 2 beside each other
par(mfrow=c(1,2))
## weekdays
agg_fweek1 <- aggregate(fweek1$steps,list(fweek1$interval),mean)
plot(
    x=agg_fweek1$Group.1,
    y=agg_fweek1$x,
    type="l",
    main="Weekday steps by interval",
    xlab="Interval",
    ylab="Steps"
)
## weekend
agg_fweek0 <- aggregate(fweek0$steps,list(fweek0$interval),mean)
plot(
    x=agg_fweek0$Group.1,
    y=agg_fweek0$x,
    type="l",
    main="Weekend steps by interval",
    xlab="Interval",
    ylab="Steps"
)