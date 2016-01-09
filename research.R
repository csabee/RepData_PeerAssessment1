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
    counter <- counter + 1
}
