library(stats)
library(dplyr)
library(RAINFrrplot)
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

RAINF_datos <- df_all[df_all$date == "2018" | df_all$date == "2020",]
x <- 1:length(RAINF_datos$RAINF)


RAINF_datos <- data.frame(RAINF_datos$RAINF)
RAINF_datos <- RAINF_datos %>% rename(y = RAINF_datos.RAINF)

RAINF_datos$x <- x


modelRAINF <- lm(y~bs(x,knots = c(seq(from = 1,to =length(RAINF_datos$x),by = 2192))),data = RAINF_datos)
inv2lims <- range(RAINF_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelRAINF,newdata = list(inv2=inv2.grid),se=T)


plot(RAINF_datos$x,df_all$RAINF[1:length(RAINF_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)

