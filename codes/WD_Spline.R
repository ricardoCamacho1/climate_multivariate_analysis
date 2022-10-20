library(stats)
library(dplyr)
library(WDrrplot)
library(readxl)
library(openair)
library(dplyr)
library(hash)
library(tidyverse)
library(hrbrthemes)
library(reshape2)
library(splines)
library(Ecdat)

df_all <- read.csv("all_data.csv")


df_all <- df_all %>% rename(date = ï..date)

attach(df_all)
df_all$date = as.POSIXct(df_all$date, tz = "UTC")
df_all$date <- format(df_all$date, format = "%Y")

WD_datos <- df_all[df_all$date == "2018" | df_all$date == "2020",]
x <- 1:length(WD_datos$WD)


WD_datos <- data.frame(WD_datos$WD)
WD_datos <- WD_datos %>% rename(y = WD_datos.WD)

WD_datos$x <- x


modelWD <- lm(y~bs(x,knots = c(seq(from = 1,to =length(WD_datos$x),by = 2192))),data = WD_datos)
inv2lims <- range(WD_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelWD,newdata = list(inv2=inv2.grid),se=T)


plot(WD_datos$x,df_all$WD[1:length(WD_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)