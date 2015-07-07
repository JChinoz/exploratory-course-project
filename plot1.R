plot1 <- function()
{
    ## Read the initial data
    data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
    data$Date <- as.character(data$Date)
    
    ## Set subsetting boundaries using grep
    jan <- data[grep("\\b1/2/2007\\b", data$Date), ]
    feb <- data[grep("\\b2/2/2007\\b", data$Date), ]
    subsetData <- rbind(jan, feb)
    
    ## Change to numeric and omit NA values
    subsetData$Global_active_power <- as.numeric(as.character(subsetData$Global_active_power))
    subsetData <- na.omit(subsetData)
    
    ## Plot the graph
    plot1Dat <- subsetData$Global_active_power
    hist(plot1Dat, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
    
    ## Create an image
    dev.copy(png, file="plot1.png", width=480, height=480)
    dev.off()
}