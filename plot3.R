library(ggplot2)
library("lubridate")
library(reshape2)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
dat <- read.table(unz(temp, "household_power_consumption.txt"), header=T, sep=";")
unlink(temp)
head(dat)


#convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions.
dt <- dat
dt$Date <- as.Date(dt$Date, format="%d/%m/%Y")
dt$DT <- with(dt, ymd(Date) + hms(Time))

head(dt)


#We will only be using data from the dates 2007-02-01 and 2007-02-02
start <- as.Date("2007-02-01") 
end <- as.Date("2007-02-02")
dt1 <- subset(dt, Date >= start & Date <= end)
dt1$Day <- weekdays(dt1$DT)
dt1$Global_active_power <- as.numeric(dt1$Global_active_power)
head(dt1)


dt2 <- dt1[,7:10]
head(dt2)
dt3 <- melt(dt2, id = "DT")
head(dt3)
dt3$value <- as.numeric(dt3$value)


removebackground <-  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                           panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                           panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                           panel.border = element_rect(colour = "black", fill=NA, size=2))


plot3 <- ggplot(dt3, aes(y=value, x= DT, color = variable)) +
  geom_line() +
  ylab("Energy cub metering") +
  xlab("") +
  theme(legend.position = c(0.85, 0.9)) +
  removebackground
plot3


#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels
png("plot3.png", units="px", width=480, height=480)
plot(plot3)
dev.off()
