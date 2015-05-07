# Reading data utility
# Common functions to read and clean only the required data from the
# household power consumption dataset
read_housepower <- function(){
  dt <- read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings="?")
  dt <- dt[dt$Date == "1/2/2007" | dt$Date == "2/2/2007",]
  dt$Datetime <- as.POSIXlt(paste(dt$Date, dt$Time), format="%d/%m/%Y %T")
  dt <- dt[,!(names(dt) %in% c("Date","Time"))]
  rownames(dt) <- NULL
  dt
}

# Plot task 1 - Histogram
# X - Global Active Power (kilowatts)
# Y - Frequency
gen_plot_1 <- function(housepower){
  hist(housepower$Global_active_power,main="Global Active Power",xlab="Global Active Power (kilowatts)",col="red")
}

# Plot task 2 - Plot of type s - for stair steps
# X - Datetime
# Y - Global Active Power (kilowatts)
gen_plot_2 <- function(housepower){
  plot(housepower$Datetime,housepower$Global_active_power,type="s",ylab="Global Active Power (kilowatts)",xlab="")
}

# Plot task 3 - Plot of type s - for stair steps
# X - Datetime
# Y - Energy sub metering 1 to 3
gen_plot_3 <- function(housepower,bty="o"){
  plot(housepower$Datetime,pmax(housepower$Sub_metering_1,housepower$Sub_metering_2,housepower$Sub_metering_3),type="n",ylab="Energy sub metering",xlab="")
  points(housepower$Datetime,housepower$Sub_metering_1,type="s")
  points(housepower$Datetime,housepower$Sub_metering_2,type="s",col="red")
  points(housepower$Datetime,housepower$Sub_metering_3,type="s",col="blue")
  legend("topright", col = c("black","red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),lwd=2,bty=bty)
}


gen_plot_voltage_datetime <- function(housepower){
  plot(housepower$Datetime,housepower$Voltage,type="s",xlab="datetime",ylab="Voltage")
}

gen_plot_grp_datetime <- function(housepower){
  plot(housepower$Datetime,housepower$Global_reactive_power,type="s",xlab="datetime",ylab="Global_reactive_power")
}

# Plot task 4 - 4 graphs of type s - for stair steps
# Graph 1,1 - gen_plot_2()
# Graph 1,2 - gen_plot_voltage_datetime()
# Graph 2,1 - same as gen_plot_3
# Graph 2,2 - Global reactive power X Datetime

gen_plot_4 <- function(housepower){
   par(mfrow = c(2,2))
   gen_plot_2(housepower)
   gen_plot_voltage_datetime(housepower)
   gen_plot_3(housepower,bty="n")
   gen_plot_grp_datetime(housepower)
}