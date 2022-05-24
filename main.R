source("global.R")


load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/etf_data_hv.rdata")

date <- as.Date("2022-04-01")
data_dates <- seq.Date(date-months(1), date, by="days")

fund <- NULL
nav <- 2000
trans_cost <- 1
max_assets_n <- 20
max_wgt <- 0.2
change <- 0.2

ticker_order <- sort(names(etf_data_hv$daily_prices))
all_prices <- etf_data_hv$daily_prices[,ticker_order]
all_returns <- etf_data_hv$daily_return[,ticker_order]



pso_pkg_obj_func <- function(x, cov, mean_returns, prices, nav, max_assets_n, change, fund, trans_cost, return_details=FALSE){
  wgt <- as.vector(floor(x*nav/prices)*prices/nav)
  
  fit_mean_ret <- - mean_returns %*% wgt
  fit_risk <- sqrt(t(wgt) %*% cov %*% wgt)
  
  #fit_sharp <- fit_mean_ret/fit_risk
  
  fit_sum_wgts <- max(sum(wgt)-0.99,0)-min(sum(wgt)-0.95,0)
  
  fit_max_assets_n <- max(sum(wgt!=0)-max_assets_n, 0)
  
  fit_change <- if(!is.null(fund)){
    max(sum(abs(fund-wgt)), 2*change) - 2*change
  }else{
    0
  }
  
  fit_change_cost <- if(!is.null(fund)){
    sum((fund-wgt)!=0) * trans_cost
  }else{
    0
  }
  
  fit_vec <- c(
    "fit_mean_ret"=fit_mean_ret,
    "fit_risk"=fit_risk,
    #"fit_sharp"=fit_sharp, 
    "fit_sum_wgts"=fit_sum_wgts * 10,
    "fit_max_assets_n"=fit_max_assets_n,
    "fit_change"=fit_change,
    "fit_change_cost"=fit_change_cost
  )
  if(return_details){return(fit_vec)}
  
  fit <- sum(fit_vec)
    
  return(fit)
}


cov <- cov(all_returns[data_dates,])
mean_returns <- sapply(all_returns[data_dates,], mean)
prices <- all_prices[date,]

par <- if(!is.null(fund)){fund}else{rep(0, ncol(prices))}
for(i in 1:10){
  opt <- psoptim(
    par = par,
    fn = pso_pkg_obj_func,
    cov = cov,
    mean_returns = mean_returns,
    prices = prices,
    nav = nav,
    max_assets_n = max_assets_n,
    change = change,
    fund = fund,
    trans_cost = trans_cost,
    lower = rep(0, ncol(prices)),
    upper = rep(max_wgt, ncol(prices)),
    control = list(
      maxit = 100, # max iterations
      s = 200, # swarm size
      p = 0.5, # percentage of information ( 1 := each particle is fully informed )
      trace=1
    )
  )
  par <- opt$par
}

fund_info <- pso_pkg_obj_func(
  x = as.vector(floor(par*nav/prices)*prices/nav),
  cov = cov,
  mean_returns = mean_returns,
  prices = prices,
  nav = nav,
  max_assets_n = max_assets_n,
  change = change,
  fund = fund,
  trans_cost = trans_cost,
  return_details=TRUE
)















