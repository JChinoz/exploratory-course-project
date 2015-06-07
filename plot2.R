plot2 <- function()
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
    plot2Dat <- subsetData[,c(1,4)]
    plot(plot2Dat, type="l", ylab="Global Active Power (kilowatts)", xlab="")
    
    ## Create an image
    dev.copy(png, file="plot2.png", width=480, height=480)
    dev.off()
}