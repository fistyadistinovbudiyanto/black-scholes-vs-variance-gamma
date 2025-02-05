# Import library yang diperlukan
library(OptionPricing)
library(VarianceGamma)
library(tseries)
library(moments)
library(readxl)
library(nortest)
library(quantmod)
library(pso)
library(factoextra)
library(tidyverse)
library(dplyr)
library(openxlsx)

# Fungsi untuk menghitung log return, statistik dasar, dan menampilkan visualisasi
calculate_return_stats <- function(data_path) {
  data <- read_excel(data_path)
  Return <- na.omit(diff(log(data$Price)))
  
  # Menghitung rata-rata dan varians
  cat("Rata-rata Log Return:", round(mean(Return), 6), "\n")
  cat("Varians Log Return:", round(var(Return), 6), "\n")
  
  # Visualisasi
  par(mfrow = c(2, 2))
  plot(Return, type = "l", main = "Log Return Time Series", ylab = "Log Return", xlab = "Time")
  hist(Return, prob = TRUE, main = "Histogram of Log Returns", xlab = "Log Return", border = "blue", col = "lightblue")
  qqnorm(Return, main = "QQ Plot of Log Returns")
  qqline(Return, col = "red")
  
  # Uji Normalitas
  cat("Hasil Uji KS:", ks.test(Return, "pnorm", mean(Return), sd(Return))$p.value, "\n")
  cat("Hasil Uji Jarque-Bera:", jarque.test(Return)$p.value, "\n")
  cat("Skewness:", skewness(Return), "\n")
  cat("Kurtosis:", kurtosis(Return), "\n")
  
  return(Return)  # Kembalikan nilai log return
}

# Fungsi untuk menghitung log return, statistik dasar, dan menampilkan visualisasi
calculate_return_stats2 <- function(data) {
  # data <- read_excel(data_path)
  Return <- na.omit(diff(log(data)))
  
  # Menghitung rata-rata dan varians
  cat("Rata-rata Log Return:", round(mean(Return), 6), "\n")
  cat("Varians Log Return:", round(var(Return), 6), "\n")
  
  # Visualisasi
  par(mfrow = c(2, 2))
  plot(Return, type = "l", main = "Log Return Time Series", ylab = "Log Return", xlab = "Time")
  hist(Return, prob = TRUE, main = "Histogram of Log Returns", xlab = "Log Return", border = "blue", col = "lightblue")
  qqnorm(Return, main = "QQ Plot of Log Returns")
  qqline(Return, col = "red")
  
  # Uji Normalitas
  cat("Hasil Uji KS:", ks.test(Return, "pnorm", mean(Return), sd(Return))$p.value, "\n")
  cat("Hasil Uji Jarque-Bera:", jarque.test(Return)$p.value, "\n")
  cat("Skewness:", skewness(Return), "\n")
  cat("Kurtosis:", kurtosis(Return), "\n")
  
  return(Return)  # Kembalikan nilai log return
}

# Fungsi untuk menghitung harga opsi Black-Scholes (Call dan Put)
calculate_option_price <- function(K, r, sigma, T, S0) {
  call_price <- BS_EC(K = K, r = r, sigma = sigma, T = T, S0 = S0)
  
  cat("Harga Call:", call_price, "\n")
 
  return(call_price)  # Kembalikan hanya harga Call (predicted price) untuk MAPE
}

# Fungsi untuk menghitung MAPE
calculate_mape <- function(actual_price, predicted_price) {
  mape <- mean(abs((actual_price - predicted_price) / actual_price)) * 100
  cat("MAPE:", round(mape, 5), "%\n")
}


######## FITRI #########
# Definisikan simbol LQ45 yang ingin diambil
saham <- c('GOOG', 'NVDA', 'MSFT')

# Tentukan rentang tanggal yang diinginkan
start_date <- "2023-10-13"
end_date <- "2024-10-14"

# Ambil data mingguan dari Yahoo Finance
getSymbols(saham, from = start_date, to = end_date, periodicity = "daily")

# Inisialisasi list untuk menyimpan data harga penutupan
close_prices_list <- list()

# Loop untuk mengambil data harga penutupan untuk setiap simbol yaitu saham
for (symbol in saham) {
  # Ambil data mingguan dari Yahoo Finance
  getSymbols(symbol, from = start_date, to = end_date, periodicity = "daily")
  # Simpan harga penutupan dalam list
  close_prices_list[[symbol]] <- Cl(get(symbol))
}

# Menggabungkan harga penutupan menjadi data frame
close_price <- data.frame(close_prices_list)
print(close_price)


########## GOOG ########## 
# Kalkulasi dan visualisasi untuk data NIO
cat("==== Data GOOG ====")
Return_GOOG <- calculate_return_stats2(close_price$GOOG.Close)

# Hitung harga opsi dan MAPE untuk GOOG
predicted_GOOG <- calculate_option_price(K = 70, r = 0.05, sigma = sqrt(0.000309), T = 31 / 252, S0 = 166.90)
calculate_mape(actual_price = 110.41, predicted_price = predicted_GOOG[1])

########## NVDA ########## 
# Kalkulasi dan visualisasi untuk data NVDA
cat("==== Data NVDA ====")
Return_NVDA <- calculate_return_stats2(close_price$NVDA.Close)

# Hitung harga opsi dan MAPE untuk NVDA
predicted_NVDA <- calculate_option_price(K = 0.5, r = 0.05, sigma = sqrt(0.001059), T = 31 / 252, S0 = 131.60)
calculate_mape(actual_price = 148.64, predicted_price = predicted_NVDA[1])

########## MSFT ########## 
# Kalkulasi dan visualisasi untuk data MSFT
cat("==== Data MSFT ====")
Return_MSFT <- calculate_return_stats2(close_price$MSFT.Close)

# Hitung harga opsi dan MAPE untuk MSFT
predicted_MSFT <- calculate_option_price(K = 210, r = 0.05, sigma = sqrt(0.000151), T = 31 / 252, S0 = 415.26)
calculate_mape(actual_price = 210.93, predicted_price = predicted_MSFT[1])






##############################################################################################
#================Black Scholes dengan Gram Charlier==============
call.gc=function(K,S0,sigma,T,r,skew,kurt){
  d1 <- (log(S0/K) + (r + sigma^2/2) * T)/(sigma * sqrt(T))
  x=exp(d1^2/-2)/sqrt(2*pi)
  a=S0*sigma*sqrt(T)
  q3=(a/6)*(x*(2*sigma*sqrt(T)-d1)+(sigma^2)*T*pnorm(d1))
  q4<-(1/24)*S0*sigma*sqrt(T)*(x*((d1^2)-3*sigma*sqrt(T)*(d1-sigma*sqrt(T))-1)+((sigma*sqrt(T))^3)*pnorm(d1))
  d2 <- d1 - sigma * sqrt(T)
  cbs=S0*pnorm(d1)-K*exp(-r*T)*pnorm(d2)
  cat('d1=',d1,'\n')
  cat('q3=',q3,'\n')
  cat('q4=',q4,'\n')
  
  price<-cbs+skew*q3+(kurt-3)*q4
  cat("call price gram charlier=",price,'\n')
}

##Goodness of Fit VG
gof_vg=function(data){
  fit<-vgFit(data)
  sequence<-seq(0,1,by=0.02)
  qualist<-qvg(sequence,param=fit$param)
  qualist[length(sequence)]<-max(data)
  dsample<-cut(data,breaks=qualist)
  iobs<-as.vector(table(dsample))
  pexp<-as.vector(rep(1/50,50))
  chisq.test(iobs,p=pexp)
}


##### GOOG ######

# Gram-Charlier untuk opsi GOOG
cat("==== Harga Opsi dengan Gram-Charlier untuk GOOG ====")
call.gc(K = 70, S0 = 166.90, sigma = sqrt(0.000309), T = (31) / 252, 
        r = 0.05, skew = skewness(Return_GOOG), kurt = kurtosis(Return_GOOG))
calculate_mape(actual_price = 110.41, predicted_price =  97.32923)



# Plot Variance Gamma
ppvg(Return_GOOG, main='Variance Gamma P-P Plot')
gof_vg(Return_GOOG)


##### NVDA ######

# Gram-Charlier untuk opsi NVDA
cat("==== Harga Opsi dengan Gram-Charlier untuk NVDA ====")
call.gc(K = 0.5, S0 = 131.60, sigma = sqrt(0.001059), T = (31) / 252, 
        r = 0.05, skew = skewness(Return_NVDA), kurt = kurtosis(Return_NVDA))
calculate_mape(actual_price = 148.64, predicted_price =  131.1031)


# Plot Variance Gamma
ppvg(Return_NVDA, main='Variance Gamma P-P Plot')
gof_vg(Return_NVDA)


##### MSFT ######

# Gram-Charlier untuk opsi MSFT
cat("==== Harga Opsi dengan Gram-Charlier untuk MSFT ====")
call.gc(K = 0.5, S0 = 415.26, sigma = sqrt(0.000151), T = (31) / 252, 
        r = 0.05, skew = skewness(Return_MSFT), kurt = kurtosis(Return_MSFT))
calculate_mape(actual_price = 210.93, predicted_price =  414.7631)


# Plot Variance Gamma
ppvg(Return_MSFT, main='Variance Gamma P-P Plot')
gof_vg(Return_MSFT)







# Estimasi parameter Variance Gamma
# GOOG
fit_GOOG <- vgFit(Return_GOOG)
print(fit_GOOG)

# NVDA
fit_NVDA <- vgFit(Return_NVDA)
print(fit_NVDA)

# MSFT
fit_MSFT <- vgFit(Return_MSFT)
print(fit_MSFT)




###############################
#Simulasi Harga Opsi Berdistribusi Varian Gamma
#VG
VG<-function(n, T, r, omega, sigmavg, nu, theta,K,s0){
  z<- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*rnorm(n,0,1))
  w<-(z-K)
  h<-pmax(w,0)
  m<-mean(h)
  call_VG<-m*exp(-r*T)
  callVG<-c()
  for(i in 1:n){
    callVG[i]<-w[i]*exp(-r*T)
  }
  se_callVG<-sd(callVG)/sqrt(n)
  output<-list(price_call=call_VG, sterr_call=se_callVG)
  return(output)
}

# omega=(1/nu)*log(1-theta*nu-0.5*sigmavg^2*nu)


### GOOG ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 2) * log(1 + 0.007087 * 2 - 0.5 * 0.020071^2 * 2)
# Panggil fungsi VG
result_GOOG <- VG(5,  T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.020071,
                  nu = 2, theta = -0.007087, K = 70, s0 = 166.90)
# Tampilkan hasil
print(result_GOOG)


### NVDA ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 0.510469) * log(1 + 0.002794 * 0.510469 - 0.5 * 0.032127^2 * 0.510469)
# Panggil fungsi VG
result_NVDA <- VG(5, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.0135346,
                  nu = 0.3076518, theta = 0.0007700, K = 0.5, s0 = 233.85)
# Tampilkan hasil
print(result_NVDA)

# omega=(1/nu)*log(1-theta*nu-0.5*sigmavg^2*nu)

### MSFT ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 1.310979) * log(1 + 0.002049 * 1.310979 - 0.5 * 0.015455^2 * 1.310979)
# Panggil fungsi VG
result_MSFT <- VG(5, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.015455,
                  nu = 1.310979, theta = -0.002049, K = 210, s0 = 415.26)
# Tampilkan hasil
print(result_MSFT)



#VG AVR
VG_AVR<-function(n, T, r, omega, sigmavg, nu, theta,K,s0){
  z1<- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*rnorm(n,0,1))
  z2 <- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*(-rnorm(n,0,1)))
  w1<-((z1-K)+(z2-K))/2
  h1<-pmax(w1,0)
  m1<-mean(h1)
  call_VG_AVR<-m1*exp(-r*T)

  callVGAVR<-c()
  for(i in 1:n){
    callVGAVR[i]<-w1[i]*exp(-r*T)
  }
  se_callVG_AVR<-sd(callVGAVR)/sqrt(n)
  output<-list(price_call=call_VG_AVR, sterr_call=se_callVG_AVR)
  return(output)
}
### GOOG ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 2) * log(1 + 0.007087 * 2 - 0.5 * 0.020071^2 * 2)
# Panggil fungsi VG
result_GOOG_2 <- VG_AVR(5, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.020071,
                        nu = 2, theta = -0.007087, K = 70, s0 = 166.90)
# Tampilkan hasil
print(result_GOOG_2)


### NVDA ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 0.510469) * log(1 + 0.002794 * 0.510469 - 0.5 * 0.032127^2 * 0.510469)
# Panggil fungsi VG
result_NVDA_2 <- VG_AVR(5, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.0135346,
                        nu = 0.3076518, theta = 0.0007700, K = 0.5, s0 = 233.85)
# Tampilkan hasil
print(result_NVDA_2)


### MSFT ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 1.310979) * log(1 + 0.002049 * 1.310979 - 0.5 * 0.015455^2 * 1.310979)
# Panggil fungsi VG
result_MSFT_2 <- VG_AVR(5, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.015455,
                        nu = 1.310979, theta = -0.002049, K = 210, s0 = 415.26)
# Tampilkan hasil
print(result_MSFT_2)



#VG IMS
VG_IMS<-function(n, T, r, omega, sigmavg, nu, theta,K,s0){
  price_call<-NULL
  sterr_call<-NULL

  Z <- rnorm(n, mean=0, sd=1)
  tau = T
  WT <- sqrt(tau) * Z
  ST = s0*exp((r+omega)*tau + theta*rgamma(n, shape=tau/nu, scale = nu) + sigmavg*sqrt(rgamma(n, shape=tau/nu, scale = nu))*Z)
  #Call option price and standard error
  simulated_call_payoffs <- (exp(-r*tau)*pmax(ST-K,0))[ST>K]
  price_call<- mean(simulated_call_payoffs*mean(ST>K))
  sterr_call<- sd(simulated_call_payoffs*mean(ST>K))/sqrt(n)

  output<-list(price_call=price_call, sterr_call=sterr_call)
  return(output)
}
### GOOG ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 2) * log(1 + 0.007087 * 2 - 0.5 * 0.020071^2 * 2)
# Panggil fungsi VG
result_GOOG_3 <- VG_IMS(5,  T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.020071,
                        nu = 2, theta = -0.007087, K = 70, s0 = 166.90)
# Tampilkan hasil
print(result_GOOG_3)


### NVDA ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 0.510469) * log(1 + 0.002794 * 0.510469 - 0.5 * 0.032127^2 * 0.510469)
# Panggil fungsi VG
result_NVDA_3 <- VG_IMS(5, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.0135346,
                        nu = 0.3076518, theta = 0.0007700, K = 0.5, s0 = 233.85)
# Tampilkan hasil
print(result_NVDA_3)


### MSFT ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 1.310979) * log(1 + 0.002049 * 1.310979 - 0.5 * 0.015455^2 * 1.310979)
# Panggil fungsi VG
result_MSFT_3 <- VG_IMS(5, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.015455,
                        nu = 1.310979, theta = -0.002049, K = 210, s0 = 415.26)
# Tampilkan hasil
print(result_MSFT_3)

