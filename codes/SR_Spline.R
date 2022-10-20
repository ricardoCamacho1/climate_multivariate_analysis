library(stats)
library(dplyr)
library(SRrrplot)
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

SR_datos <- df_all[df_all$date == "2018" | df_all$date == "2020",]
x <- 1:length(SR_datos$SR)


SR_datos <- data.frame(SR_datos$SR)
SR_datos <- SR_datos %>% rename(y = SR_datos.SR)

SR_datos$x <- x


modelSR <- lm(y~bs(x,knots = c(seq(from = 1,to =length(SR_datos$x),by = 2192))),data = SR_datos)
inv2lims <- range(SR_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelSR,newdata = list(inv2=inv2.grid),se=T)


plot(SR_datos$x,df_all$SR[1:length(SR_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)

