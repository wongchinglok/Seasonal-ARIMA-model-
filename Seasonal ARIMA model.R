library(lubridate)
library(ggplot2)
install.packages("forecast", dependencies = TRUE)
library(forecast)
library(dplyr)
library(tidyr)


data <- read.delim('https://www.esrl.noaa.gov/gmd/webdata/ccgg/trends/co2/co2_mm_mlo.txt', comment.char = '#', header = FALSE, sep = '', col.names = c('Year','Month','Time','Co2_Concentration','Interpolated','#days','Trend','Days'))

which(is.na(data))

data_cc <- data %>% 
  mutate(
    Co2_Con = case_when(
      Co2_Concentration == -99.99 ~ Interpolated,
      TRUE ~ Co2_Concentration
    )
  )

sapply(data_cc, class)

data_cc$Date <- ymd(paste0(data$Year, " ", data$Month, " ", "15"))
data_cc_sel <- data_cc %>% 
  select(Year, Month, Date, Co2_Con )

data_cc_sel_test <- data_cc_sel %>% 
  filter(Year > 2016)
data_cc_sel_train <- data_cc_sel %>% 
  filter(Year <= 2016)

ggplot(data_cc_sel,aes(Date, Co2_Con)) +
  geom_line(color='blue') +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Atmospheric CO2 Concentration(ppm), 1958-2021")+
  xlab("Year, Month") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "5 year") +
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 45, hjust = 1)) +
  ylab("CO2 Concentration (ppm)") +
  #scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10))
  scale_y_continuous() +
  theme(axis.text.y = element_text(face = "bold", color = "#993333", 
                                   size = 10, hjust = 1),axis.title.y = element_text(size = 10))


p2 <- ggplot(data_cc_sel_train,aes(Date, Co2_Con)) +
  geom_line(color='blue') +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Training set")+
  xlab("Year, Month") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "5 year") +
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 45, hjust = 1)) +
  ylab("CO2 Concentration (ppm)") +
  #scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10))
  scale_y_continuous() +
  theme(axis.text.y = element_text(face = "bold", color = "#993333", 
                                   size = 10, hjust = 1), axis.title.y = element_text(size = 8))
p3 <- ggplot(data_cc_sel_test,aes(Date, Co2_Con)) +
  geom_line(color='blue') +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Testing set")+
  xlab("Year, Month") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 45, hjust = 1)) +
  ylab("CO2 Concentration (ppm)") +
  #scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10))
  scale_y_continuous() +
  theme(axis.text.y = element_text(face = "bold", color = "#993333", 
                                   size = 10, hjust = 1), axis.title.y = element_text(size = 8))
p2
p3

#draw acf and pacf of the original data
par(mfrow=c(1,2),mai=c(0.8,0.8,0.8,0.1))
acf(data_cc_sel_train$Co2_Con,ci.type='ma',lag=30,
    main = 'ACF of CO2_con', xlab= 'lag',ylab='ACF')
pacf(data_cc_sel_train$Co2_Con,lag=30,
     main = 'PACF of CO2_con',xlab= 'lag',ylab='PACF')
par(mfrow=c(1,1),mai=c(0.8,0.8,0.8,0.1))

# take the difference
Co2_train <- ts(data_cc_sel_train$Co2_Con, start = c(1958,3), frequency = 12)
Co2_train %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

## fit seasonal ARIMA(0,1,1)*(0,1,1)_12 model
aicsvalue <- function(p,q,P,Q) {
  fit <- Arima(Co2_train, order=c(p,1,q),seasonal=list(order=c(P,1,Q),period=12),
               lambda = "auto"
  )
  return(fit$aicc)
}
model_arima <- data.frame(Model_name=c("ARIMA(0,1,3)(3,1,1)[12]","ARIMA(0,1,1)(3,1,1)[12]","ARIMA(1,1,0)(1,1,0)[12]",
                                     "ARIMA(1,1,2)(1,1,0)[12]","ARIMA(1,1,3)(0,1,1)[12]","ARIMA(1,1,1)(1,1,0)[12]",
                                     "ARIMA(1,1,1)(1,1,0)[12]","ARIMA(1,1,0)(1,1,1)[12]","ARIMA(1,1,1)(0,1,1)[12]" ), AICc=c(aicsvalue(0,3,3,1),aicsvalue(0,1,3,1),aicsvalue(1,0,1,0),aicsvalue(1,2,1,0),aicsvalue(1,3,0,1),aicsvalue(1,1,1,0),aicsvalue(1,1,1,0),aicsvalue(1,0,1,1), aicsvalue(1,1,0,1)))
model_arima

#residuals
(fit_minaicc <- Arima(Co2_train, order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),
                      lambda = "auto"
))
checkresiduals(fit_minaicc, lag=36)
fit_minaicc$aicc

#RMSE check
Co2_test <- ts(data_cc_sel_test$Co2_Con, start = c(2017,1), frequency = 12)
mm <- accuracy(forecast(fit_minaicc,h=35)$mean, Co2_test )

rmse_eva <- function(p,d,q,P,D,Q) {
  fit <- Arima(Co2_train, order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=12),
               lambda = "auto"
  )
  mm <- accuracy(forecast(fit,h=35)$mean, Co2_test)
  return(mm[2])
  
}

rmse_eva <- data.frame(Model_name=c(
  "ARIMA(0,1,3)(3,1,1)[12]","ARIMA(0,1,1)(3,1,1)[12]","ARIMA(1,1,0)(1,1,0)[12]",
  "ARIMA(1,1,2)(1,1,0)[12]","ARIMA(1,1,3)(0,1,1)[12]","ARIMA(1,1,1)(1,1,0)[12]",
  "ARIMA(1,1,1)(1,1,0)[12]","ARIMA(1,1,0)(1,1,1)[12]","ARIMA(1,1,1)(0,1,1)[12]"
), RMSE=c(                        
  rmse_eva(0,1,3,3,1,1),rmse_eva(0,1,1,3,1,1),rmse_eva(1,1,0,1,1,0),                                                  rmse_eva(1,1,2,1,1,0),rmse_eva(1,1,3,0,1,1),rmse_eva(1,1,1,1,1,0),                                                  rmse_eva(1,1,1,1,1,0),rmse_eva(1,1,0,1,1,1),rmse_eva(1,1,1,0,1,1)))                                                  
print(rmse_eva)

#Forecast 
Co2_train %>%
  Arima(order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),
        lambda = "auto"
  ) %>%
  forecast(h=400) %>%
  autoplot() +
  ylab("Atmospheric CO2 concentration(ppm) ") + xlab("Year") +
  autolayer(Co2_test)

#Forecast vs test data
prediction <- forecast(fit_minaicc,h=51) 
data_cc_sel_test$prediction <- prediction$mean
data_test_pre_tidy <- gather(data_cc_sel_test, "type", "Co2", -Year,-Month,-Date)

ggplot(data_test_pre_tidy,aes(Date, Co2,color=type)) +
  geom_line() +
  xlab("Year, Month") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 45, hjust = 1)) +
  ylab("CO2 Concentration (ppm)") +
  #scale_x_continuous(breaks = trans_breaks(identity, identity, n = 10))
  scale_y_continuous() +
  theme(axis.text.y = element_text(face = "bold", color = "#993333", 
                                   size = 10, hjust = 1), axis.title.y = element_text(size = 8))

#2050
prediction1 <- forecast(fit_minaicc,h=396, level = c(80,90)) 
p10 <- prediction1$upper[396,2]
p50 <- prediction1$mean[396]
sd_calc <- (p10-p50)/1.28
Co2_con_2050 <- rnorm(10^6,p50,sd_calc)
cdf_co2_con_2050 <- ecdf(Co2_con_2050)
cdf_co2_con_2050_data <- data.frame(Co2_con_2050)
ggplot(cdf_co2_con_2050_data, aes(Co2_con_2050)) + stat_ecdf(geom = "step", color='blue') +
  geom_vline(xintercept = 460, color='red') +
  geom_hline(yintercept = cdf_co2_con_2050(460), color='red') +
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 0, hjust = 1)) +
  scale_x_continuous(breaks=c(400,425,450, 460,475,500,525, 550), limits = c(425,525)) +
  scale_y_continuous(breaks=c(seq(0,1,0.1)), limits = c(0,1)) +
  ylab('Cumulative Distribution') +
  xlab("Co2 Concentraion(ppm) at 2050")

cdf_co2_con_2050(460)
