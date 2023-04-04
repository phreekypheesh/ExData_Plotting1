library(ggplot2)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
data <- read.table(unz(temp, "household_power_consumption.txt"), header=T, sep=";")
unlink(temp)
head(data)



#convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions.
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
head(data)


#We will only be using data from the dates 2007-02-01 and 2007-02-02
start <- as.Date("2007-02-01") 
end <- as.Date("2007-02-02")
dt1 <- subset(data, Date >= start & Date <= end)

dt1$Global_active_power <- as.numeric(dt1$Global_active_power)


#Plot 1: Histogram of Global Active Power (kilowatts). Red fill.
removebackground <-  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                  panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                  panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                  panel.border=element_blank()) #gets rid of square going around the entire graph
                  

plot1 <- ggplot(dt1, aes(x=Global_active_power)) +
  geom_histogram(color = "black", fill= "red", bins=18) +
  scale_y_discrete(name ="Frequency",limits=c(0,200,400, 600, 800, 1000, 1200)) +
  xlab("Global Active Power (kilowatts)") +
  ggtitle("Global Active Power") +
  theme(plot.title = element_text(hjust = 0.5)) +
  removebackground
plot1


#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels
png("plot1.png", units="px", width=480, height=480)
plot(plot1)
dev.off()
