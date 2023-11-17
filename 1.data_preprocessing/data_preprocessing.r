library(quantmod)
library(dplyr)
library(lubridate)

start_date <- "2000-01-01"
end_date <- Sys.Date()

tryCatch(
  {
    getSymbols("^GSPC", from = start_date, to = end_date, src = "yahoo")
  },
  error = function(e) {
    print(paste("Error: ", e$message))
  }
)
sp500_data <- Cl(GSPC)
sp500_dates<-index(GSPC)

summary_stats <- sp500_data %>%
  summarise(
    Mean = mean(., na.rm = TRUE),
    Median = median(., na.rm = TRUE),
    StdDev = sd(., na.rm = TRUE),
    Min = min(., na.rm = TRUE),
    Max = max(., na.rm = TRUE)
  )

print(summary_stats)

# Additional Analysis (e.g., Yearly or Monthly Breakdown)
sp500_data <- xts::as.xts(sp500_data, order.by = sp500_dates)
sp500_data$Year <- year(index(sp500_data))
sp500_data$Month <- month(index(sp500_data))

# Yearly Breakdown
yearly_stats <- aggregate(sp500_data[,1], list(Year = sp500_data$Year), mean)
print(yearly_stats)

# Monthly Breakdown (in terms of years)
monthly_stats <- aggregate(sp500_data[,1], list(Year = sp500_data$Year, Month = sp500_data$Month), mean)
print(monthly_stats)