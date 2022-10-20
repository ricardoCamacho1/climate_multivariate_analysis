library(stats)
library(dplyr)
library(corrplot)
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

SO2_datos <- df_all[df_all$date == "2019" | df_all$date == "2020",]
x <- 1:length(SO2_datos$SO2)


SO2_datos <- data.frame(SO2_datos$SO2)
SO2_datos <- SO2_datos %>% rename(y = SO2_datos.SO2)

SO2_datos$x <- x


modelSO2 <- lm(y~bs(x,knots = c(seq(from = 1,to =length(SO2_datos$x),by = 2192))),data = SO2_datos)
inv2lims <- range(SO2_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelSO2,newdata = list(inv2=inv2.grid),se=T)


plot(SO2_datos$x,df_all$SO2[1:length(SO2_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)

