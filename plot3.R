plot3 <- function() {
    library(dplyr)
    
    ## read input dataset
    pow_cons_data <- read.table("./household_power_consumption.txt", header = T, sep = ";", 
                                na.strings = "?")
    
    ## filter datasets only for two days
    pow_cons_data <- mutate(pow_cons_data, Date = as.Date(Date, "%d/%m/%Y"))
    pow_cons_data_2 <- pow_cons_data[((pow_cons_data$Date == "2007-02-01") | 
                                          (pow_cons_data$Date == "2007-02-02")), ]
    
    ## transform "Sub Metering columns" columns to numeric
    pow_cons_data_3 <- mutate(pow_cons_data_2, Sub_metering_1 = as.numeric(as.vector(Sub_metering_1)),
                              Sub_metering_2 = as.numeric(as.vector(Sub_metering_2)), 
                                Sub_metering_3 = as.numeric(as.vector(Sub_metering_3)))
    
    ## create a new column to plot days of week
    tmp <- paste(pow_cons_data_3$Date, pow_cons_data_3$Time)
    pow_cons_data_3$timestamp <- as.POSIXlt(tmp)
    
    ## Create and save plot in png format
    with (pow_cons_data_3, plot(timestamp, Sub_metering_1, type = 'l', col = "black", 
                                xlab = "", ylab = "Energy sub metering"))
    with (pow_cons_data_3, points(timestamp, Sub_metering_2, type = 'l', col = "red"))
    with (pow_cons_data_3, points(timestamp, Sub_metering_3, type = 'l', col = "blue"))
    legend("topright", pch = "_", col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    dev.copy(png, file = "./plot3.png")
    dev.off()
}