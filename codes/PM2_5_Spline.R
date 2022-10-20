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

PM2_5_datos <- df_all[df_all$date == "2019" | df_all$date == "2020",]
x <- 1:length(PM2_5_datos$PM2_5)


PM2_5_datos <- data.frame(PM2_5_datos$PM2_5)
PM2_5_datos <- PM2_5_datos %>% rename(y = PM2_5_datos.PM2_5)

PM2_5_datos$x <- x

#c(2192, 4384, 6576, 8768, 10960,13152,15344)   overfittear
#c(4384, 8768, 13152) underfittear
modelPM2_5 <- lm(y~bs(x,knots = c(seq(from = 1,to =length(PM2_5_datos$x),by = 2192))),data = PM2_5_datos)
inv2lims <- range(PM2_5_datos$x)
inv2.grid <- seq(from= inv2lims[1], to = inv2lims[2])
pred <- predict(modelPM2_5,newdata = list(inv2=inv2.grid),se=T)




#df_all$PM2_5[8768:26303] comparar con otros agnos
#PM2_5_datos$y comparar consigo mismo
plot(PM2_5_datos$x,df_all$PM2_5[1:length(PM2_5_datos$x)], main = "Regression Spline Plot")
lines(inv2.grid, pred$fit, col = "red",lwd = 3)