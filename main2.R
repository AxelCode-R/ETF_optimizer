source("global.R")

load_local_data <- FALSE

min_date <- as.Date("2010-01-01")
max_date <- Sys.Date()
min_date_history <- months(6)


if(load_local_data){
  load("DATA/choosen_tickers.rdata")
}else{
  choosen_tickers <- c("ACWI", "EEM", "EWA", "EWC", "EWD", "EWG", "EWH", "EWI", "EWJ", "EWL")
  # save(choosen_tickers, file="DATA/choosen_tickers.rdata)
}

if(load_local_data){
  load("DATA/listing_status.rdata")
}else{
  listing_status <- get_listing_status_av(choosen_tickers = choosen_tickers, max_date = max_date, min_date = min_date, min_date_history=min_date_history)
  # save(listing_status, file="DATA/listing_status.rdata")
}

if(load_local_data){
  load("DATA/listing_status.rdata")
}else{
  daily_data <- get_prices_and_returns_av(choosen_tickers = choosen_tickers, max_date = max_date, min_date = min_date, min_date_history=min_date_history)
  # save(daily_data, file="DATA/daily_data.rdata")
}


if(load_local_data){
  load("DATA/rsi.rdata")
}else{
  rsi <- get_indicator_av(indicator = "RSI", indicator_string = "function=RSI&interval=daily&time_period=30&series_type=open", choosen_tickers = choosen_tickers, max_date = max_date, min_date = min_date, min_date_history=min_date_history)
  # save(rsi, file="DATA/rsi.rdata")
}



sma <- get_indicator_av(indicator = "SMA", indicator_string = "function=SMA&interval=daily&time_period=10&series_type=open", choosen_tickers = choosen_tickers, max_date = max_date, min_date = min_date, min_date_history=min_date_history)


# prevent oversold or overbought





plotly_line_chart_xts(cbind.xts(return_to_cumret(daily_data$daily_returns[,"ACWI"]), rsi[,"ACWI"], sma[,"ACWI"]))



# tf <- 100
# ret <- daily_data$daily_returns[1:tf,"ACWI"]
# for(i in (tf+1):nrow(daily_data$daily_returns)){
#   date <- index(daily_data$daily_returns)[i]
#   s <- seq(0.99,1, length.out=tf)
#   #temp <- apply(s*daily_data$daily_returns[(i-tf):(i-1),],2, mean)#/apply(s*daily_data$daily_returns[(i-tf):(i-1),],2, sd)
#   temp <- -apply(s*daily_data$daily_returns[(i-tf):(i-1),],2, sd)
#   ret <- rbind.xts(ret, daily_data$daily_returns[date,which(temp==max(temp))])
# }
# plotly_line_chart_xts(return_to_cumret(cbind.xts(daily_data$daily_returns[,"ACWI"], ret)))
# 
# 
# plotly_line_chart_xts(return_to_cumret(cbind.xts(daily_data$daily_returns[,"ACWI"], ret, xts(daily_data$daily_returns %*% rep(1/ncol(daily_data$daily_returns), ncol(daily_data$daily_returns)), order.by=index(daily_data$daily_returns)) )))
# 
# 
# 
# tf <- c(270, 90, 30)
# all_returns <- all_returns
# ret <- NULL
# asset_n <- 100
# 
# start_at <- index(all_returns)[tf[1]]
# dates <- seq.Date(start_at, max(index(all_returns)), by="months")
# for(i in 1:(length(dates)-1)){
#   date <- dates[i]
#   temp <- rbind(
#     sapply(all_returns[paste0(date-tf[1],"/",date),], mean), 
#     sapply(all_returns[paste0(date-tf[2],"/",date),], mean),
#     sapply(all_returns[paste0(date-tf[3],"/",date),], mean)
#   )
#   temp2 <- apply(temp, 2, function(z){(z[1]>=z[2] && z[2] < z[3] && z[3] < z[1]) || (z[3]<z[2] & z[2]<z[1]) }) %>% as.numeric()
#   temp_sd <- sapply(all_returns[paste0(date-tf[1],"/",date),], sd)
#   temp2 <- temp2*colSums(temp)/(temp_sd+0.01)
#   print(paste0(date, "   n: ",sum(temp2!=0)))
#   if(sum(temp2!=0)>asset_n){
#     temp2[temp2< sort(temp,decreasing = T)[asset_n]] <- 0
#   }
#   ret <- rbind.xts(ret, xts(all_returns[paste0(date+1, "/", dates[i+1])] %*% (temp2/sum(temp2)), order.by=index(all_returns[paste0(date+1, "/", dates[i+1])])))
# }
# ret <- ret[!is.na(ret)]
# 
# plotly_line_chart_xts(return_to_cumret(cbind.xts(ret, all_returns[index(ret),"ACWI"]) ))
# 








