plot2 <- function() {
    library(dplyr)
    
    ## read input dataset
    pow_cons_data <- read.table("./household_power_consumption.txt", header = T, sep = ";", 
                                na.strings = "?")
    
    ## filter datasets only for two days
    pow_cons_data <- mutate(pow_cons_data, Date = as.Date(Date, "%d/%m/%Y"))
    pow_cons_data_2 <- pow_cons_data[((pow_cons_data$Date == "2007-02-01") | 
                                          (pow_cons_data$Date == "2007-02-02")), ]
    
    ## transform "Global Active Power" column ploattable
    pow_cons_data_3 <- mutate(pow_cons_data_2, Global_active_power = 
                                  as.numeric(as.vector(Global_active_power)))
    
    ## create a new column to plot days of week
    tmp <- paste(pow_cons_data_3$Date, pow_cons_data_3$Time)
    pow_cons_data_4$timestamp <- as.POSIXlt(tmp)
    
    ## Create and save plot in png format
    plot(pow_cons_data_4$timestamp, pow_cons_data_4$Global_active_power, type = 'l', 
         ylab = "Global Active Power (kilowatts)", xlab = "")
    
    dev.copy(png, file = "./plot2.png")
    dev.off()
}