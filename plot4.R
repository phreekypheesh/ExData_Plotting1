library(ggplot2)
library("lubridate")
library(reshape2)
library(ggpubr)

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
dt1$Voltage <- as.numeric(dt1$Voltage)
dt1$Global_reactive_power <- as.numeric(dt1$Global_reactive_power)
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


plotA <- ggplot(dt1, aes(y=Global_active_power, x= DT)) +
  geom_line() +
  ylab("Global Active Power (kilowatts)") +
  xlab("") +
  removebackground
plotA



plotB <- ggplot(dt1, aes(y=Voltage, x= DT)) +
  geom_line() +
  ylab("Global Active Power (kilowatts)") +
  xlab("") +
  removebackground
plotB



plotC <- ggplot(dt3, aes(y=value, x= DT, color = variable)) +
  geom_line() +
  ylab("Energy cub metering") +
  xlab("") +
  theme(legend.position = c(0.85, 0.85)) +
  removebackground
plotC


plotD <- ggplot(dt1, aes(y=Global_reactive_power, x= DT)) +
  geom_line() +
  ylab("Global Reactive Power") +
  xlab("") +
  removebackground
plotD



plot4 <- ggarrange(plotA,plotB,plotC,plotD, nrow = 2, ncol = 2) 
plot4

#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels
png("plot4.png", units="px", width=480, height=480)
plot(plot4)
dev.off()
