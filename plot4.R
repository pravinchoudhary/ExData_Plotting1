plot4 <- function() {
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
    
    ## store in png format
    png(file = "./plot4.png", width = 480, height=480, type="windows")
    #dev.copy(png, file = "./plot4.png")
    
    ## Create plot
    par(mfrow = c(2,2), mar = c(4, 6, 5, 1), oma = c(0, 0, 2, 0))
    ## plot @ 1,1: Global_active_powe vs datetime
    with(pow_cons_data_3, plot(timestamp, Global_active_power, type = 'l', ylab = "Global Active Power", xlab = ""))
    
    ## plot @ 1,2: Voltage vs datetime
    with(pow_cons_data_3, plot(timestamp, Voltage, type="l", xlab = "datetime"))
    
    ## plot @ 2,1: Entergy_sub_metering(s) vs datetime
    with(pow_cons_data_3, plot(timestamp, Sub_metering_1, type = 'l', col = "black", 
                               xlab = "", ylab = "Energy sub metering"))
    with(pow_cons_data_3, points(timestamp, Sub_metering_2, type = 'l', col = "red"))
    with(pow_cons_data_3, points(timestamp, Sub_metering_3, type = 'l', col = "blue"))
    legend("topright", pch = "_", col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    ## plot @ 2,2: Global_reactive_power vs datetime
    with(pow_cons_data_3, plot(timestamp, Global_reactive_power, type="l", xlab = "datetime"))
    
    dev.off()
}