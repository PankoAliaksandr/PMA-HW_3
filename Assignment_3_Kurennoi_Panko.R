# Since graph in paper was constructed on Fama-French factor file on Ken French website, we use the same data
# Since daily market excess returns at this website are sofar avaiable until 31.10.2018, and the last full week in FOMC cycle for this data
# ends at 29.10.2018, we restrict our analysis up to 29.10.2018, taking FOMC meeting at 26.09.2018 as the last one

library(lubridate)
library(PerformanceAnalytics)
library(xts)
library(quantmod)

#####SELECT FOMC DATES FIRST, THEN SELECT FF_FACTORS#####

data_load <- function(){
  
   # load FOMC-dates
  cat("\nChoose Rdata file\n")
  load(file.choose(), envir = parent.frame())
 
   # load Fama-French factors
  cat("\nChoose .csv file\n")
  FF_factors <- read.csv2(file.choose(), sep="," ,dec=".")
  FF_factors$Date <- as.Date(as.character(FF_factors$Date), format="%Y%m%d")
  
  # Take Date and returns
  MktRF <- FF_factors[c("Date","Mkt.RF")]
  MktRF <- as.xts(MktRF, order.by = MktRF$Date)
  MktRF <- MktRF$Mkt.RF
  
  
  rassel2000 <- getSymbols("^RUT", src="yahoo", auto.assign = FALSE)
  rassel2000 <- Return.calculate(rassel2000$RUT.Adjusted)
  
  commodity_index <- getSymbols("DJP", src="yahoo", auto.assign = FALSE)
  commodity_index <- Return.calculate(commodity_index$DJP.Adjusted)
  
  EURUSD <- getSymbols("EURUSD=X", src="yahoo", auto.assign = FALSE)
  EURUSD <- Return.calculate(EURUSD$`EURUSD=X.Adjusted`)
  
  return_list <- list(MktRF = MktRF,
                      rassel =  rassel2000,
                      cmdt = commodity_index,
                      EURUSD = EURUSD)
  
  return(return_list)
}
determine_dates <- function(index_data, n_days, index_type){
  
  if(index_type == "Mkt.RF"){
    
    first_FOMC_date <- as.Date(DT$FOMC[1])
    end_date <- as.Date("2018-10-29")
    
  }else{
    first_FOMC_date <- as.Date("2007-01-31")
    end_date <- as.Date("2018-10-29")
  }
  
  
  index_of_fist_FOMC_date <- which(index(index_data) == first_FOMC_date)
  begin_date <- index(index_data[index_of_fist_FOMC_date - n_days])
  
  return_list <- list(begin_date = begin_date , end_date = end_date)
  
  return(return_list)
  
}
get_returns <- function(index_data, begin_end_dates){
  
  # Find required dates
  indices <- which(index(index_data) >= begin_end_dates$begin_date &
                   index(index_data) <= begin_end_dates$end_date)
  
  # Required market excess returns from FF website
  relevant_index_data <- index_data[indices,]
  
  # Sequence of dates with weekends and holidays
  all_dates <- seq(begin_end_dates$begin_date,
                   begin_end_dates$end_date,
                   by = "days")
  
  # Exclude weekends (holidays are still included)
  all_business_days_ind <- which(weekdays(all_dates) %in% c('Monday',
                                                            'Tuesday',
                                                            'Wednesday',
                                                            'Thursday',
                                                            'Friday'))
  all_business_days <- all_dates[all_business_days_ind]
  all_business_days <- data.frame(all_business_days)
  colnames(all_business_days) <- c("Date")
  
  # Default return is zero, will remain 0 during holidays
  returns <- rep(0,length(all_business_days))
  
  index_returns <- as.data.frame(cbind(all_business_days, returns))
  
  # Find all non-holidays dates and copy returns
  indices <- which(index_returns$Date %in% index(relevant_index_data))
  index_returns$returns[indices] <- as.double(relevant_index_data[,1]) / 100
  
  index_returns$Rolling_5d_ret <- rep(0,times = length(all_business_days))
  index_returns_xts <- xts(index_returns[,-1], order.by=index_returns$Date)
  index_returns_xts <- na.locf(index_returns_xts, na.rm = FALSE)
  
  # Calculate commulative returns for rolling 5 days window
  index_returns$Rolling_5d_ret <- apply.rolling(R = index_returns_xts$returns,
                                                 width = 5,
                                                 trim = TRUE,
                                                 by = 1,
                                                 FUN = "Return.cumulative")
  
  # We need to shift commulative returns back to make [t;t+4] cummulative return
  index_returns$Rolling_5d_ret <- lag(index_returns$Rolling_5d_ret, -4)
  
  return(index_returns)
}
calculate_FOMC_day_means <- function(begin_end_dates, returns){
  
  # remove FOMC meetings for analyzing of which we don't have data
  meetings_dates <- DT$FOMC[DT$FOMC >= begin_end_dates$begin_date &
                            DT$FOMC<= begin_end_dates$end_date]
  
  num_of_meetings <- length(meetings_dates)
  
  cycle_returns_df <- as.data.frame(matrix(rep(NA, times = 40), nrow = 1, ncol = 40))
  
  # Create a cycle as described in paper
  for (i in 1:num_of_meetings) {
   
    start_FOMC_index <- which(returns$Date == meetings_dates[i]) - 6
    if (i < num_of_meetings) {
      end_FOMC_index <- which(returns$Date == meetings_dates[i + 1]) - 1
    } else {
      end_FOMC_index <- nrow(returns)
    }
    
    if((end_FOMC_index - start_FOMC_index) > 39){
      end_FOMC_index <- start_FOMC_index + 39
    }
    
    cycle_returns <- returns[start_FOMC_index:end_FOMC_index, ]
    
    # number of days between meeting is not constant
    number_of_NA <- 40 - length(cycle_returns$Rolling_5d_ret)
    
    # Expand with NA
    cycle_returns_write <- c(as.vector(cycle_returns$Rolling_5d_ret),
                             rep(x = NA, times = number_of_NA))
    
    # Save the sigle cycle returns
    cycle_returns_df <- rbind(cycle_returns_df, t(cycle_returns_write))
  }
  
  # First row is NAs
  cycle_returns_df <- cycle_returns_df[-1,]
  
  # Find average returns for each day in the cycle
  means <- colMeans(cycle_returns_df, na.rm = TRUE) * 100
  
  return(means)
}
plot_FOMC_cycle <- function(means, main){
 
   # plot the graph
  plot(-6:33,
       means,
       type = "l",
       main = main,
       xaxt = "n",
       xlab = "Business days since FOMC meeting", 
       ylab = "Avg cumulative[t;t+4] MKT.RF (%)" 
  )
  
  axis(1, at = seq(-6, 33, by = 1))
  points(-6:33, means, col = "black", bg = "black", pch = 18)
  text(-6:33, means, -6:33, adj = c(-0.3, 1.5), cex = 0.7)
  abline(h = seq(-0.2, 0.6, 0.1), col = "gray")
}
test_robustness <- function(MktRF){
  
  # add dummy for even weeks separately and jointly
  num_days <- length(MktRF)
  
  # Intruduce dummy variables
  D0 <- D2 <- D4 <- D6 <- rep(0, num_days)
  
  for(i in 1:(nrow(DT) - 2)){
    k <- match(DT$FOMC[i], index(MktRF))
    # Week 0
    D0[(k - 1):(k + 3)] <- rep(1, 5)
    # Week 2
    D2[(k + 9):(k + 13)] <- rep(1, 5)
    # Week 4
    D4[(k + 19):(k + 23)] <- rep(1, 5)
    # Week 6
    D6[(k + 29):(k + 33)] <- rep(1, 5)
  }
  
  #dummy for all even weeks
  D6 <- D6[1:length(D0)]
  D <- D0 + D2 + D4 + D6 
  
  #dummy for weeks 2,4,6 
  D246 <- D - D0 
  
  reg1a <- lm(MktRF ~ D)
  reg1b <- lm(MktRF ~ D0 + D246)
  reg1c <- lm(MktRF ~ D0 + D2 + D4 + D6)
  
  print(summary(reg1a))
  print(summary(reg1b))
  print(summary(reg1c))
}


data_list <- data_load()

# Market excess returns
begin_end_dates_MktRF <- determine_dates(data_list$MktRF,
                                         n_days = 6,
                                         index_type = "Mkt.RF")

Sys.setlocale(category = "LC_ALL", locale = "english")
returns <- get_returns(data_list$MktRF, begin_end_dates_MktRF)
means <- calculate_FOMC_day_means(begin_end_dates_MktRF, returns)
plot_FOMC_cycle(means, main="MKT.RF over FOMC Cycle, 01.1994 - 10.2018")

# Rassel 2000
begin_end_dates_rassel <- determine_dates(data_list$rassel,
                                         n_days = 6,
                                         index_type = "rassel")

returns <- get_returns(data_list$rassel, begin_end_dates_rassel)
means <- calculate_FOMC_day_means(begin_end_dates_rassel, returns)
plot_FOMC_cycle(means, main="Rassel 2000 over FOMC Cycle, 01.1994 - 10.2018")

# Commodities
begin_end_dates_cmdt <- determine_dates(data_list$cmdt,
                                        n_days = 6,
                                        index_type = "cmdt")

returns <- get_returns(data_list$cmdt , begin_end_dates_cmdt)
means <- calculate_FOMC_day_means(begin_end_dates_cmdt, returns)
plot_FOMC_cycle(means, main="Commodities over FOMC Cycle, 01.1994 - 10.2018")

# Currency
begin_end_dates_EURUSD <- determine_dates(data_list$EURUSD,
                                          n_days = 6,
                                          index_type = "currency")

returns <- get_returns(data_list$EURUSD, begin_end_dates_EURUSD)
means <- calculate_FOMC_day_means(begin_end_dates_EURUSD, returns)
plot_FOMC_cycle(means, main="Currency over FOMC Cycle, 01.1994 - 10.2018")

# Robustness
test_robustness(data_list$MktRF)

