plot3 <- function()
{
    ## Read the initial data
    data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
    data$Date <- as.character(data$Date)
    data$Time <- as.character(data$Time)
    
    ## Set subsetting boundaries using grep
    jan <- data[grep("\\b1/2/2007\\b", data$Date), ]
    feb <- data[grep("\\b2/2/2007\\b", data$Date), ]
    subsetData <- rbind(jan, feb)
    
    ## Change to numeric and omit NA values
    subsetData$Global_active_power <- as.numeric(as.character(subsetData$Global_active_power))
    subsetData <- na.omit(subsetData)
    
    ## Create new column of Date-Time
    datetime <- paste(subsetData$Date, subsetData$Time, sep=" ", collapse=NULL)
    ## Merge new column with existing dataset
    subsetData <- cbind(datetime, subsetData)
    subsetData$datetime <- as.POSIXct(strptime(subsetData$datetime, "%d/%m/%Y %H:%M:%S"))
    
    ## Plot the graph
    plot3Dat <- subsetData[,c(1, 8, 9, 10)]
    
    ## Converts factor data type to numeric data type
    plot3Dat$Sub_metering_1 <- as.numeric(as.character(plot3Dat$Sub_metering_1))
    plot3Dat$Sub_metering_2 <- as.numeric(as.character(plot3Dat$Sub_metering_2))
    
    ## Sub metering 3 does not need to convert because it is originally a numeric data type
    
    with(plot3Dat, plot(datetime, Sub_metering_1, type="n", ylab="Energy sub metering"))
    with(plot3Dat, lines(datetime, Sub_metering_1, col="black"))
    with(plot3Dat, lines(datetime, Sub_metering_2, col="red"))
    with(plot3Dat, lines(datetime, Sub_metering_3, col="blue"))
    legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), col=c("black", "blue", "red"))

    ## Create an image
    dev.copy(png, file="plot3.png", width=480, height=480)
    dev.off()
}