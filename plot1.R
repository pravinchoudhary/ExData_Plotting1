plot1 <- function() {
    library(dplyr)
    
    ## read input dataset
    pow_cons_data <- read.table("household_power_consumption.txt", header = T, sep = ";", 
                                na.strings = "?")
    
    ## filter datasets only for two days
    pow_cons_data <- mutate(pow_cons_data, Date = as.Date(Date, "%d/%m/%Y"))
    pow_cons_data_2 <- pow_cons_data[((pow_cons_data$Date == "2007-02-01") | 
                                          (pow_cons_data$Date == "2007-02-02")), ]
    
    ## transform "Global Active Power" column ploattable
    pow_cons_data_3 <- mutate(pow_cons_data_2, Global_active_power = 
                                  as.numeric(as.vector(Global_active_power)))
    
    ## create a new column to plot days of week
    hist(pow_cons_data_3$Global_active_power, col = "red", main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
    dev.copy(png, file = "./plot1.png")
    dev.off()
}