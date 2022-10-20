library(stats)
library(dplyr)
library(NOrrplot)
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

NO_datos <- df_all[df_all$date == "2017" | df_all$date == "2020",]
x <- 1:length(NO_datos$NO)


NO_datos <- data.frame(NO_datos$NO)
NO_datos <- NO_datos %>% rename(y = NO_datos.NO)

NO_datos$x <- x


modelNO <- lm(y~bs(x,knots = c(seq(from = 1,to =length(NO_datos$x),by = 2192))),data = NO_datos)
inv2lims <- range(NO_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelNO,newdata = list(inv2=inv2.grid),se=T)


plot(NO_datos$x,df_all$NO[1:length(NO_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)

