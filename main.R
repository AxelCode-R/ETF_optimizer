source("global.R")
#markowith gutes bsp: https://www.r-bloggers.com/2012/08/genetic-algorithms-a-simple-r-example/
# arithmretic and geometrix returns: https://www.portfolioprobe.com/2010/10/04/a-tale-of-two-returns/

# ETF kennzahl RSI 
# https://www.investors.com/etfs-and-funds/etfs/best-technical-indicators-for-etf-investors/


load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/etf_data_2.rdata")
ticker_order <- sort(names(etf_data$daily_prices))
all_prices <- etf_data$daily_prices[,ticker_order]
all_returns <- etf_data$daily_return[,ticker_order]
all_prices <- na.locf(all_prices)
all_returns <- na.locf(all_returns)
# prices und returns NA checken!

# change zu anzahl da wgt nicht bestehenbleibt monatsübergreifend oder?


fund <- NULL
anzahl <- NULL
nav <- 10000
trans_cost <- 1
max_assets_n <- 30
max_wgt <- 0.1
change <- 0.3
bm <- "ACWI"
return_target <- 0.08


opti_dates <- seq.Date(as.Date("2013-01-01"), as.Date("2022-05-01"), by="months")
opti_dates <- sapply(opti_dates, function(x){min(as.Date(index(all_prices))[index(all_prices)>=x])}) %>% as.Date()

infos <- list()
for(i in 1:length(opti_dates)){
  date <- opti_dates[i]
  print(date)
  
  data_dates <- seq.Date(date-months(24), date, by="days")
  
  cov <- cov(all_returns[data_dates,])
  mean_returns <- sapply(all_returns[data_dates,], mean)
  prices <- all_prices[date,]
  #ret_sd <- sapply(all_returns[data_dates,], sd)
  bm_ret <- all_returns[data_dates, bm]
  rets <- all_returns[data_dates,]
  betas <- lm(rets ~ bm_ret)$coefficients[2,]

  # verzinsen und neues nav setzen
  if(i != 1){
    nav <- sum(anzahl * coredata(prices[date,])) + (1-sum(fund))*nav
    fund <- anzahl * coredata(prices[date,]) / nav
  }
  
  par <- if(!is.null(fund)){
    as.vector(fund)
    # wgt verzinsen oder so
  }else{
    rep(0, ncol(prices))
  }
  par[par>max_wgt] <- max_wgt
  k <- 1
  while( k<=4){
    try({
      opt <- psoptim(
        par = par,
        fn = pso_pkg_obj_func,
        cov = cov,
        mean_returns = mean_returns,
        prices = prices,
        bm_ret = bm_ret,
        rets = rets,
        return_target = return_target,
        betas = betas,
        nav = nav,
        max_assets_n = max_assets_n,
        change = change,
        fund = fund,
        trans_cost = trans_cost,
        lower = rep(0, ncol(prices)),
        upper = rep(max_wgt, ncol(prices)),
        control = list(
          maxit = 40+k*10, # max iterations
          s = 200, # swarm size
          p = 0.5, # percentage of information ( 1 := each particle is fully informed )
          trace=1
        )
      )
    })
    par <- opt$par
    k <- k+1
  }
  
  fund_info <- pso_pkg_obj_func(
    x = par,
    cov = cov,
    mean_returns = mean_returns,
    prices = prices,
    bm_ret = bm_ret,
    rets = rets,
    return_target = return_target,
    betas = betas,
    nav = nav,
    max_assets_n = max_assets_n,
    change = change,
    fund = fund,
    trans_cost = trans_cost,
    return_details=TRUE
  )
  
  fund_new <- as.vector(floor(par*nav/prices)*prices/nav)
  names(fund_new) <- colnames(prices)
  anzahl_new <- as.vector(fund_new * nav/prices)
  names(anzahl_new) <- colnames(prices)
  
  
  infos[[as.character(date)]] <- list(
    fund_info = fund_info,
    wgt = fund_new,
    sum_wgts = sum(fund_new),
    anzahl = floor(par*nav/prices),
    anzahl_titel = sum(anzahl_new!=0),
    change = sum(abs(fund_new-fund))/2,
    trans_cost = sum(anzahl!=anzahl_new) * trans_cost,
    beta = sum(betas*fund_new),
    tr = sd(rets %*% fund_new - bm_ret),
    risk = sqrt(t(fund_new) %*% cov %*% fund_new),
    mean_return_expost = ((1 + mean_returns %*% fund_new)^251-1)
  )
  nav <- nav - sum(anzahl!=anzahl_new) * trans_cost
  fund <- fund_new
  anzahl <- anzahl_new
  
  print("fit: ")
  print(infos[[as.character(date)]]$fund_info)
  print("wgts: ")
  print(infos[[as.character(date)]]$wgt[infos[[as.character(date)]]$wgt!=0])
  print(paste0("sum_wgts: ", round(infos[[as.character(date)]]$sum_wgts,5) ))
}

bt_files <- list.files("BACKTESTS/")
if(length(bt_files)<1){
  save.image(file="BACKTESTS/test1.rdata")
}else{
  save.image(file=paste0("BACKTESTS/test",as.numeric(gsub(".rdata","",gsub("test","",bt_files))) + 1,".rdata"))
}








# plot
bt <- list(
  dates = as.Date(names(infos)),
  ret = NULL,
  ret_split = NULL
)

for(i in 1:(length(bt$dates))){
  from <- as.character(bt$dates[i])
  to <- if(!is.na(bt$dates[i+1])){as.character(bt$dates[i+1]-1)}else{bt$dates[i]+days(30)}
  date_interval <- paste0(from, "/", to)
  
  temp <- infos[[from]]
  wgt <- temp$wgt[temp$wgt!=0]
  
  ret <- xts(all_returns[date_interval, names(wgt)] %*% wgt, order.by=index(all_returns[date_interval,]))
  
  bt$ret_split <- return_to_cumret(ret)
  bt$ret <- rbind.xts(bt$ret, ret)
  
}


plotly_line_chart_xts( return_to_cumret( cbind.xts(bt$ret, all_returns[index(bt$ret), "ACWI"]) ))




print_infos(infos) %>% View()




#plotly_line_chart_xts( return_to_cumret( xts( all_returns %*% rep(1/ncol(all_returns), ncol(all_returns)), order.by=index(all_returns)) ))

# 
# 
# cbind.xts(all_returns[,1], xts(rollmean(all_returns[,1],50), order.by = index(all_returns)[-(1:49)]), xts(rollmean(all_returns[,1],200), order.by = index(all_returns)[-(1:199)]))  %>% .["2012-01-01/",] %>% return_to_cumret() %>% plotly_line_chart_xts()
# 
# 
# cbind.xts(all_returns[,1], xts(rollmean(all_returns[,1],25), order.by = index(all_returns)[-(1:24)]), xts(rollmean(all_returns[,1],50), order.by = index(all_returns)[-(1:49)]), xts(rollmean(all_returns[,1],100), order.by = index(all_returns)[-(1:99)]), xts(rollmean(all_returns[,1],200), order.by = index(all_returns)[-(1:199)]))  %>% .["2012-01-01/",] %>% return_to_cumret() %>% plotly_line_chart_xts()
# 
# tick <- 2
# 
# z = cbind.xts(all_returns[,tick], xts(rollmean(all_returns[,tick],20), order.by = index(all_returns)[-(1:19)]), xts(rollmean(all_returns[,tick],40), order.by = index(all_returns)[-(1:39)]), xts(rollmean(all_returns[,tick],80), order.by = index(all_returns)[-(1:79)]), xts(rollmean(all_returns[,tick],160), order.by = index(all_returns)[-(1:159)]))  %>% .["2012-01-01/",]
# z[,2:5] <- return_to_cumret(z[,2:5])[-1,]
# 
# z2 <- z %>% data.frame() %>% mutate(ACWI_only = if_else(ACWI.1 >= ACWI.2 & ACWI.2 >= ACWI.3 & ACWI.3 >= ACWI.4, ACWI, 0)) %>% xts(., order.by=as.Date(rownames(.)))
# 
# z2$ACWI <- return_to_cumret(z2$ACWI)[-1,]
# z2$ACWI_only <- return_to_cumret(z2$ACWI_only)[-1,]
# 
# z2  %>% plotly_line_chart_xts()
# 
# 
# 
# 
# 
# tick <- 1
# 
# z = cbind.xts(all_returns[,tick], xts(rollmean(all_returns[,tick],10), order.by = index(all_returns)[-(1:9)]), xts(rollmean(all_returns[,tick],50), order.by = index(all_returns)[-(1:49)]))  %>% .["2012-01-01/",]
# z[,2:3] <- return_to_cumret(z[,2:3])[-1,]
# 
# z2 <- z %>% data.frame() %>% mutate(AAXJ_only = if_else(AAXJ.1 >= AAXJ.2, AAXJ, 0)) %>% xts(., order.by=as.Date(rownames(.)))
# 
# z2$AAXJ <- return_to_cumret(z2$AAXJ)[-1,]
# z2$AAXJ_only <- return_to_cumret(z2$AAXJ_only)[-1,]
# 
# z2  %>% plotly_line_chart_xts()
# 
# m = httr::content(
#   httr::GET(paste0("https://www.alphavantage.co/query?function=RSI&symbol=AAXJ&interval=daily&time_period=30&series_type=open&datatype=csv&apikey=", api_key)), 
#   as = "text", 
#   encoding = "UTF-8"
# ) %>% 
#   read_csv()
# 
# m2 = httr::content(
#   httr::GET(paste0("https://www.alphavantage.co/query?function=RSI&symbol=AAXJ&interval=daily&time_period=100&series_type=open&datatype=csv&apikey=", api_key)), 
#   as = "text", 
#   encoding = "UTF-8"
# ) %>% 
#   read_csv()
# 
# cbind.xts(z2, xts(m$RSI, order.by=as.Date(m$time)), xts(m2$RSI, order.by=as.Date(m2$time))) %>% .["2012-01-01/",] %>% plotly_line_chart_xts()

