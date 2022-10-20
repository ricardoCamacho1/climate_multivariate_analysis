library(MASS)
library(greedy)
library(klaR)
library(gridExtra)
library(grid)


datos <- read.csv("all_data_spline_AQI.csv",header = T)
#datos <- read.csv("sima_final_data_spline_AQI.csv",header = T)
datos$date <- NULL
datos$SO2 <- NULL
datos$AQI_NO2_cualitativo <- NULL
datos$AQI_NO2 <- NULL
datos$AQI_CO_cualitativo <- NULL
datos$AQI_CO <- NULL 
datos$AQI_O3_cualitativo <- NULL
datos$AQI_O3 <- NULL
datos$AQI_PM10_cualitativo <- NULL
datos$AQI_PM10 <- NULL
datos$AQI_PM2_5 <- NULL
#datos$CO <- NULL


datos <- na.omit(datos)
datos$AQI_PM2_5_cualitativo <- factor(datos$AQI_PM2_5_cualitativo)
datos$AQI_PM2_5_cualitativo <- factor(datos$AQI_PM2_5_cualitativo, levels = c(0,1,2,3,4,5), labels = c("good", "moderate", "unhealthy fsg" , "unhealthy" , "very unhealthy","hazardous"))
library(caret)
library(MASS)
attach(datos)

tapply(PM2_5, AQI_PM2_5_cualitativo, mean)


index <- createDataPartition(datos$AQI_PM2_5_cualitativo, p = 0.8, list = F)

training <- datos[index,]
test <- datos[-index,]



modelo.train <- lda(AQI_PM2_5_cualitativo ~ PM10 + O3 + NO + NO2 + NOx + SR + RH + TOUT + PRS + WS + WD + CO, training)


modelo.values <- predict(modelo.train)
library(ggplot2)


p1 <- predict(modelo.train, test)$class
tab <- table(Predicted = p1, Actual = test$AQI_PM2_5_cualitativo)
tab

newdata <- data.frame(type = training[,16],lda = modelo.values$x)
ggplot(newdata) + geom_point(aes(newdata$lda.LD1, newdata$lda.LD2, colour = type),size = 2.5)

newdata <- data.frame(type = training[,16],lda = modelo.values$x)
ggplot(newdata) + geom_point(aes(newdata$lda.LD1, newdata$lda.LD2, colour = type),size = 2.5)


sum(diag(tab)) / sum(tab)
