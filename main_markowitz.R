source("global.R")

load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/etf_data_2.rdata")
ticker_order <- sort(names(etf_data$daily_prices))
all_prices <- etf_data$daily_prices[,ticker_order]
all_returns <- etf_data$daily_return[,ticker_order[1:50]]
all_prices <- na.locf(all_prices)
all_returns <- na.locf(all_returns)


fund <- NULL
anzahl <- NULL
nav <- 10000
trans_cost <- 1
max_assets_n <- 30
max_wgt <- 0.1
change <- 0.3
bm <- "ACWI"





library(ROI)
library(foreach)
library(DEoptim)
library(iterators)
library(fGarch)
library(Rglpk)
library(quadprog)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
library(pso)
library(GenSA)
library(corpcor)
library(testthat)
library(nloptr)
library(MASS)
library(robustbase)
library(PortfolioAnalytics)
# initialise with asset names uses time series data
data_p2 = all_returns["2021-01-01/2022-01-01",]
# create specification
port = portfolio.spec(assets = c(colnames(data_p2)))
# add long only constraint
port = add.constraint(portfolio = port, type = "long_only")
# add full investment contraint
port = add.constraint(portfolio = port, type = "full_investment")




### RANDOM PORTS
# objective: manimise risk
port_rnd = add.objective(portfolio = port, type = "risk", name = "StdDev")

# objective: maximise return
port_rnd = add.objective(portfolio = port_rnd, type = "return", name = "mean")

# 1. optimise random portfolios

rand_p = optimize.portfolio(R = data_p2, portfolio = port_rnd, optimize_method = "random",
                            trace = TRUE, search_size = 1000)
# plot

chart.RiskReward(rand_p, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)  #also plots the equally weighted portfolio



### MIN RISK
port_msd = add.objective(portfolio = port, type = "risk", name = "StdDev")
minvar1 = optimize.portfolio(R = data_p2, portfolio = port_msd, optimize_method = "ROI",trace = TRUE)
minvar1



### EFFICIENT FRONTIER
library(fPortfolio)
data_p2 = as.timeSeries(data_p2)
pspec = portfolioSpec()  #initial specification

setNFrontierPoints(pspec) = 500  #random portfolios for the efficient frontier

eff_front2 = portfolioFrontier(data_p2, constraints = "LongOnly")  #strategy
plot(eff_front2, c(1, 2, 4, 5, 6))



