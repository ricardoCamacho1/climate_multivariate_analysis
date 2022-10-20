library(stats)
library(dplyr)
library(RHrrplot)
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

RH_datos <- df_all[df_all$date == "2018" | df_all$date == "2020",]
x <- 1:length(RH_datos$RH)


RH_datos <- data.frame(RH_datos$RH)
RH_datos <- RH_datos %>% rename(y = RH_datos.RH)

RH_datos$x <- x


modelRH <- lm(y~bs(x,knots = c(seq(from = 1,to =length(RH_datos$x),by = 2192))),data = RH_datos)
inv2lims <- range(RH_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelRH,newdata = list(inv2=inv2.grid),se=T)


plot(RH_datos$x,df_all$RH[1:length(RH_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)