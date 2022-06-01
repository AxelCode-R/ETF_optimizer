options(scipen=999)
options(stringsAsFactors = FALSE)
options(max.print=100)


library(xts)
library(dplyr)
library(plotly)
library(lubridate)
library(pso)
library(tidyr)
library(alphavantager)
library(readr)


for(src in list.files("R")){
  source(paste0("R/", src))
}

api_key <- "W59FBR90IRDZ8LVX"