library(stats)
library(dplyr)
library(PRSrrplot)
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

PRS_datos <- df_all[df_all$date == "2018" | df_all$date == "2020",]
x <- 1:length(PRS_datos$PRS)


PRS_datos <- data.frame(PRS_datos$PRS)
PRS_datos <- PRS_datos %>% rename(y = PRS_datos.PRS)

PRS_datos$x <- x


modelPRS <- lm(y~bs(x,knots = c(seq(from = 1,to =length(PRS_datos$x),by = 2192))),data = PRS_datos)
inv2lims <- range(PRS_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelPRS,newdata = list(inv2=inv2.grid),se=T)


plot(PRS_datos$x,df_all$PRS[1:length(PRS_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)