# This function reads in the input file "household_power_consumption.txt"
# and extracts rows corresponding to the header and Feb 1, 2007 and Feb 2, 2007
# and writes it to the ouput file "household_power_consumption_subset.txt".
# 
# The input file is expected to be in the current working directory.
# The output file is created in the current working directory.

extractDataSubset <- function() {
    inputFile <- "household_power_consumption.txt"
    outputFile <- "household_power_consumption_subset.txt"
    
    connForInputFile  <- file (inputFile, open = "r")
    connForOutputFile <- file (outputFile, open = "w")
    
    while (length (oneLine <- readLines (connForInputFile, n = 1, warn = FALSE)) > 0) {
        columns <- strsplit (oneLine, ";")
        if ((columns[[1]][1] == "Date") || (columns[[1]][1] == "1/2/2007") || 
                (columns[[1]][1] == "2/2/2007")) {
            #print (oneLine)
            writeLines (text = oneLine, con = connForOutputFile, sep = "\n")
        }
    } 
    
    close (connForInputFile)
    close (connForOutputFile)
}

# This function reads in the data in the file - "household_power_consumption_subset.txt" in the current
# working directory and returns a data frame containing that data

readData <- function() {
    df <- read.table ("household_power_consumption_subset.txt", header = TRUE, sep = ";", na.strings = "?", 
                      colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "numeric"))
}

# extract data to work with - data for Feb 1, 2007 and Feb 2, 2007 only - into a separate file
extractDataSubset()

# read that subset of data into a data frame
df <- readData()

# add a new column that combines the 2 separate date and time columns into one date/time column
df$DateTime <- strptime (paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S")

# determine the range for X-axis and Y-axis
rangeX <- range (df$DateTime)
rangeY <- range(df$Sub_metering_1, df$Sub_metering_2, df$Sub_metering_3)

# plot all 4 charts to one PNG file in 2 rows and 2 columns
png ("plot4.png", width = 480, height = 480, units = "px")
par (mfrow = c (2, 2))
with (df, {
    # plot a line chart of Global Active Power
    plot (DateTime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
    
    # plot a line chart of Voltage
    plot (DateTime, Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
    
    # plot a line chart of Sub_metering_1, Sub_metering_2 and Sub_metering_3
    plot (rangeX, rangeY, type = "n", xlab = "", ylab = "Energy sub metering")
    lines (df$DateTime, df$Sub_metering_1)
    lines (df$DateTime, df$Sub_metering_2, col="red")
    lines (df$DateTime, df$Sub_metering_3, col="blue")
    legend ("topright", legend = c ("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
            col = c ("black", "red", "blue"), lty=1, box.lwd = 0)
    
    # plot a line chart of Global Reactive Power
    plot (DateTime, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
})
dev.off()