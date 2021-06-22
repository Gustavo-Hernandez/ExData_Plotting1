library("lubridate")

#Data source https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip

#Load Data
power_consumption_origin <- read.table("./data/household_power_consumption.txt", sep=";", header=TRUE) # 2075259 obs. of 9 variables.

#Subset in the given date range.
power_consumption <- subset(power_consumption_origin, Date == "1/2/2007" | Date == "2/2/2007") #2880 obs. of 9 variables

#Parse DateTime for plot2
power_consumption$DateTime<- as.POSIXct(paste(power_consumption$Date,power_consumption$Time), format="%d/%m/%Y %H:%M:%S")

#convert to numeric values
power_consumption[,3:9] <- sapply(power_consumption[,3:9], as.numeric)

# Convert to Date.
power_consumption$Date <- dmy(power_consumption$Date)

# Convert to Time
power_consumption$Time <- hms(power_consumption$Time)

##### Generating Plots #####

## Function declaration to save current plots
savePlotAsPNG <- function(filename){
  dev.copy(png, paste(filename,".png"))
  dev.off()
}

## Setting individual plots
par(mfrow=c(1,1))

# Plot 1
with(power_consumption, hist(Global_active_power, col = "red", main = "Global Active Power", xlab="Global Active Power (kilowatts)"))
savePlotAsPNG("plot1")

# Plot 2
plot(power_consumption$DateTime,
     power_consumption$Global_active_power,
     xlab= "",
     ylab="Global Active Power (kilowatts)",
     type="l")

savePlotAsPNG("plot2")

#Plot 3
with(power_consumption,{
  plot(DateTime, Sub_metering_1, type = "l", ylab = "Energy sub metering")
  lines(DateTime, Sub_metering_2, type = "l", col="red")
  lines(DateTime, Sub_metering_3, type = "l", col="blue")
  legend("topright",
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
         col=c("black", "red", "blue"), 
         lty=1, 
         lwd=1)
})

savePlotAsPNG("plot3")

# Plot 4
par(mfcol = c(2,2))

with(power_consumption,{
  
  # Subplot 1
  plot(DateTime,
       Global_active_power,
       xlab= "",
       ylab="Global Active Power",
       type="l")
  
  # Subplot 2
  plot(DateTime, Sub_metering_1, type = "l", 
       ylab = "Energy sub metering",
       xlab="")
  lines(DateTime, Sub_metering_2, type = "l", col="red")
  lines(DateTime, Sub_metering_3, type = "l", col="blue")
  legend("topright",
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
         col=c("black", "red", "blue"), 
         lty=1, 
         lwd=1,
         cex = 0.5)
  
  # Subplot 3
  plot(DateTime, Voltage, type = "l", 
       xlab = "datetime")
  
  # Subplot 4
  plot(DateTime, Global_reactive_power, type = "l", 
       xlab = "datetime")
})

savePlotAsPNG("plot4")

