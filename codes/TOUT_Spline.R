library(stats)
library(dplyr)
library(TOUTrrplot)
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

TOUT_datos <- df_all[df_all$date == "2018" | df_all$date == "2020",]
x <- 1:length(TOUT_datos$TOUT)


TOUT_datos <- data.frame(TOUT_datos$TOUT)
TOUT_datos <- TOUT_datos %>% rename(y = TOUT_datos.TOUT)

TOUT_datos$x <- x


modelTOUT <- lm(y~bs(x,knots = c(seq(from = 1,to =length(TOUT_datos$x),by = 2192))),data = TOUT_datos)
inv2lims <- range(TOUT_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelTOUT,newdata = list(inv2=inv2.grid),se=T)


plot(TOUT_datos$x,df_all$TOUT[1:length(TOUT_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)