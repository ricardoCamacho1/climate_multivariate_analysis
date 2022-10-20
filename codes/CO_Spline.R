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

CO_datos <- df_all[df_all$date == "2017" | df_all$date == "2020",]
x <- 1:length(CO_datos$CO)


CO_datos <- data.frame(CO_datos$CO)
CO_datos <- CO_datos %>% rename(y = CO_datos.CO)

CO_datos$x <- x


modelCO <- lm(y~bs(x,knots = c(seq(from = 1,to =length(CO_datos$x),by = 2192))),data = CO_datos)
inv2lims <- range(CO_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelCO,newdata = list(inv2=inv2.grid),se=T)


plot(CO_datos$x,df_all$CO[1:length(CO_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)

