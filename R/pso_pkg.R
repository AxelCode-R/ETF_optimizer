pso_pkg_obj_func <- function(x, cov, mean_returns, prices, bm_ret, rets, betas, nav, max_assets_n, change, fund, trans_cost, return_details=FALSE){
  wgt <- as.vector(floor(x*nav/prices)*prices/nav)
  
  
  fit_te <- sd(rets %*% wgt - bm_ret)
  
  fit_beta <- (sum(betas * wgt)-1)^2
  
  #fit_mean_ret <- - mean_returns %*% wgt
  fit_risk <- sqrt(t(wgt) %*% cov %*% wgt)
  
  #fit_sharp <- fit_mean_ret/fit_risk
  #if(is.na(fit_sharp)){fit_sharp <- 10}
  
  fit_sum_wgts <- max(sum(wgt)-0.99,0)-min(sum(wgt)-0.95,0)
  
  fit_max_assets_n <- max(sum(wgt!=0)-max_assets_n, 0)
  
  fit_change <- if(!is.null(fund)){
    max(sum(abs(fund-wgt)), 2*change) - 2*change
  }else{
    0
  }
  
  fit_change_cost <- if(!is.null(fund)){
    sum((fund-wgt)!=0) * trans_cost / nav / 25
  }else{
    0
  }
  
  #fit_sd <- (t(as.vector(ret_sd)) %*% wgt - ret_sd["ACWI"])^2
  
  fit_vec <- c(
    "fit_te" = fit_te*20,
    "fit_beta" = fit_beta*10,
    #"fit_mean_ret"=fit_mean_ret*2,
    "fit_risk"=fit_risk,
    #"fit_sharp"=fit_sharp*0.01, 
    #"fit_sd" = fit_sd,
    "fit_sum_wgts"=fit_sum_wgts * 10,
    "fit_max_assets_n"=fit_max_assets_n,
    "fit_change"=fit_change,
    "fit_change_cost"=fit_change_cost
  )
  if(return_details){return(fit_vec)}
  
  fit <- sum(fit_vec)
  
  return(fit)
}
