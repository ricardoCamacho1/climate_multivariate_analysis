library(stats)
library(dplyr)
library(NOxrrplot)
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

NOx_datos <- df_all[df_all$date == "2017" | df_all$date == "2020",]
x <- 1:length(NOx_datos$NOx)


NOx_datos <- data.frame(NOx_datos$NOx)
NOx_datos <- NOx_datos %>% rename(y = NOx_datos.NOx)

NOx_datos$x <- x


modelNOx <- lm(y~bs(x,knots = c(seq(from = 1,to =length(NOx_datos$x),by = 2192))),data = NOx_datos)
inv2lims <- range(NOx_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelNOx,newdata = list(inv2=inv2.grid),se=T)


plot(NOx_datos$x,df_all$NOx[1:length(NOx_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)

