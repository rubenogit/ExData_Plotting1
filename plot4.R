#getting the data

#create a temporary file and download the dataset
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)

#use read.delim to read the whole file (use unz to unzip it)
hpc_full <- read.delim(unz(temp, "household_power_consumption.txt"), sep=";", header=TRUE)
#remove temporary file
unlink(temp)

#We only want to select the rows with dates 1/2/2007 and 2/2/2007
hpc_full_sel <- which(hpc_full$Date == "1/2/2007" | hpc_full$Date == "2/2/2007")

#create a data.frame with just the desired selection
hpc_orig <- hpc_full[hpc_full_sel,]

#some house keeping for the sake of memory:
#remove the full data.frame en the selection vector
rm(hpc_full, hpc_full_sel)

#make a copy, so I can go back to hpc_orig when I screw up
hpc <- hpc_orig

#Convert the date string to a real date
#as.Date(hpc$Date,"%d/%m/%Y")

#Convert the time string to a real time
hpc$DateTime <- as.vector(strptime(paste(hpc$Date,hpc$Time), "%d/%m/%Y %H:%M:%S"))

#Convert factors to numeric
hpc$Global_active_power <-as.numeric(as.character(hpc$Global_active_power))
hpc$Global_reactive_power <-as.numeric(as.character(hpc$Global_reactive_power))
hpc$Sub_metering_1 <- as.numeric(as.character(hpc$Sub_metering_1))
hpc$Sub_metering_2 <- as.numeric(as.character(hpc$Sub_metering_2))
hpc$Sub_metering_3 <- as.numeric(as.character(hpc$Sub_metering_3))
hpc$Voltage <-as.numeric(as.character(hpc$Voltage))


globalactivepower <- function() {
#plot Global Active Power
#preparing a line plot

#x and y variable
x <- as.numeric(hpc$DateTime)
y <- hpc$Global_active_power

#x and y label
xlabel <- ""
ylabel <- "Global Active Power (kilowatts)"

#labels for x-axis, that contain the day
label_as <- c("Thu", "Fri", "Sat")

#positions for the labels, at the start, half way and at the end of the axis
label_at <- c(min(x), (min(x)+max(x))/2, max(x))


#plotting the graph

#make the graph, without dots
plot(y ~ x, type="n", ylab=ylabel, xlab=xlabel, xaxt = "n")
#plot the line
lines(y ~ x)
#add labels to the axis
axis(1, at=label_at, labels=label_as)

#cleanup
rm(x,y,xlabel,ylabel,label_at,label_as)
}
#globalactivepower()

energysubmetering <- function(bty="") {
#energy sub metering plot
x <- as.numeric(hpc$DateTime)
y1 <- hpc$Sub_metering_1
y2 <- hpc$Sub_metering_2
y3 <- hpc$Sub_metering_3

#x and y label
xlabel <- ""
ylabel <- "Energy sub metering"

#labels for x-axis, that contain the day
label_as <- c("Thu", "Fri", "Sat")

#positions for the labels, at the start, half way and at the end of the axis
label_at <- c(min(x), (min(x)+max(x))/2, max(x))

#legend
legends <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
legend_colors <- c("black", "red", "blue")

#make the graph, without dots
plot(y1 ~ x, type="n", ylab=ylabel, xlab=xlabel, xaxt = "n")
#plot the lines
lines(y1 ~ x, col="black")
lines(y2 ~ x, col="red")
lines(y3 ~ x, col="blue")
#add labels to the axis
axis(1, at=label_at, labels=label_as)

#the making of a legend, topright, showing a line
legend("topright", legend=legends, col=legend_colors, lty=1, bty=bty)

#cleanup
rm(x,y1,y2,y3,xlabel,ylabel,label_at,label_as,legends,legend_colors)
}
#energysubmetering()
#for legend without box: energysubmetering(bty="n")



globalreactivepower <- function() {
#plot Global Reactive Power
#preparing a line plot

#x and y variable
x <- as.numeric(hpc$DateTime)
y <- hpc$Global_reactive_power

#x and y label
xlabel <- "datetime"
ylabel <- "Global_reactive_power"

#labels for x-axis, that contain the day
label_as <- c("Thu", "Fri", "Sat")

#positions for the labels, at the start, half way and at the end of the axis
label_at <- c(min(x), (min(x)+max(x))/2, max(x))


#plotting the graph

#make the graph, without dots
plot(y ~ x, type="n", ylab=ylabel, xlab=xlabel, xaxt = "n")
#plot the line
lines(y ~ x)
#add labels to the axis
axis(1, at=label_at, labels=label_as)

#cleanup
rm(x,y,xlabel,ylabel,label_at,label_as)
}
#globalreactivepower()



voltage <- function() {
#plot Voltage
#preparing a line plot

#x and y variable
x <- as.numeric(hpc$DateTime)
y <- hpc$Voltage

#x and y label
xlabel <- "datetime"
ylabel <- "Voltage"

#labels for x-axis, that contain the day
label_as <- c("Thu", "Fri", "Sat")

#positions for the labels, at the start, half way and at the end of the axis
label_at <- c(min(x), (min(x)+max(x))/2, max(x))


#plotting the graph

#make the graph, without dots
plot(y ~ x, type="n", ylab=ylabel, xlab=xlabel, xaxt = "n")
#plot the line
lines(y ~ x)
#add labels to the axis
axis(1, at=label_at, labels=label_as)

#cleanup
rm(x,y,xlabel,ylabel,label_at,label_as)
}
#voltage()

#plot all four plots in a lattice
par(mfrow = c(2, 2))
globalactivepower()
voltage()
energysubmetering(bty="n")
globalreactivepower()

#save to png
dev.copy(png,'plot4.png')
dev.off()