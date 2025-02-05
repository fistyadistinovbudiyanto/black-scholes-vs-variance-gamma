# Import library yang diperlukan
library(OptionPricing)
library(VarianceGamma)
library(tseries)
library(moments)
library(readxl)
library(nortest)

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

########## NIO #########
# Kalkulasi dan visualisasi untuk data NIO
cat("==== Data NIO ====\n")
Return_NIO <- calculate_return_stats("D:/a semester 5/pmi/Black-Scholes/NIO-Historical-Data.xlsx")

# Hitung harga opsi dan MAPE untuk NIO
predicted_NIO <- calculate_option_price(K = 1, r = 0.05, sigma = sqrt(0.001873), T = 31 / 252, S0 = 5.620)
calculate_mape(actual_price = 3.55, predicted_price =  4.626132)


########## AAPL #########
# Kalkulasi dan visualisasi untuk data AAPL
cat("==== Data AAPL ====\n")
Return_AAPL <- calculate_return_stats("D:/a semester 5/pmi/Black-Scholes/AAPL-Historical-Data.xlsx")

# Hitung harga opsi dan MAPE untuk AAPL
predicted_AAPL <- calculate_option_price(K = 5, r = 0.05, sigma = sqrt(0.000202), T = 31 / 252, S0 = 233.85)
calculate_mape(actual_price = 218.34, predicted_price = 228.8807)


########## AMZN #########
# Kalkulasi dan visualisasi untuk data AMZN
cat("==== Data AMZN ====\n")
Return_AMZN <- calculate_return_stats("D:/a semester 5/pmi/Black-Scholes/AMZN Historical Data.xlsx")

# Hitung harga opsi dan MAPE untuk AMZN
predicted_AMZN <- calculate_option_price(K = 85, r = 0.05, sigma = sqrt(0.000309), T = 31 / 252, S0 = 187.69)
calculate_mape(actual_price = 128.36, predicted_price = 103.2112)

###############################################
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

# Contoh penggunaan fungsi Gram-Charlier untuk opsi NIO
cat("==== Harga Opsi dengan Gram-Charlier untuk NIO ====\n")
call.gc(K = 1, S0 = 5.620, sigma = sqrt(0.001873), T = 31 / 252, r = 0.05, skew = skewness(Return_NIO), kurt = kurtosis(Return_NIO))
calculate_mape(actual_price = 3.55, predicted_price =  4.626133)

# Contoh penggunaan fungsi Gram-Charlier untuk opsi AAPL
cat("==== Harga Opsi dengan Gram-Charlier untuk AAPL ====\n")
call.gc(K = 5, S0 = 233.85, sigma = sqrt(0.000202), T = 31 / 252, r = 0.05, skew = skewness(Return_AAPL), kurt = kurtosis(Return_AAPL))
calculate_mape(actual_price = 218.34, predicted_price = 228.8807)

# Contoh penggunaan fungsi Gram-Charlier untuk opsi AMZN
cat("==== Harga Opsi dengan Gram-Charlier untuk AMZN ====\n")
call.gc(K = 85, S0 = 187.69, sigma = sqrt(0.000309), T = 31 / 252, r = 0.05, skew = skewness(Return_AAPL), kurt = kurtosis(Return_AAPL))
calculate_mape(actual_price = 128.36, predicted_price = 103.2112)


##############################################################
## Plot Variance Gamma untuk Return NIO, AAPL, dan AMZN
ppvg(Return_NIO, main='Variance Gamma P-P Plot - NIO')
ppvg(Return_AAPL, main='Variance Gamma P-P Plot - AAPL')
ppvg(Return_AMZN, main='Variance Gamma P-P Plot - AMZN')

## Fungsi Goodness of Fit VG
gof_vg <- function(data, name = "Data") {
  fit <- vgFit(data)
  sequence <- seq(0, 1, by = 0.02)
  qualist <- qvg(sequence, param = fit$param)
  qualist[length(sequence)] <- max(data)
  dsample <- cut(data, breaks = qualist)
  iobs <- as.vector(table(dsample))
  pexp <- as.vector(rep(1/50, 50))
  
  cat("\nGoodness of Fit Test for", name, ":\n")
  print(chisq.test(iobs, p = pexp))
}

## Memanggil Fungsi Goodness of Fit untuk Return NIO, AAPL, dan AMZN
gof_vg(Return_NIO, name = "Return NIO")
gof_vg(Return_AAPL, name = "Return AAPL")
gof_vg(Return_AMZN, name = "Return AMZN")


# Estimasi parameter Variance Gamma
# NIO
fit_NIO <- vgFit(Return_NIO)
print(fit_NIO)

# AAPL
fit_AAPL <- vgFit(Return_AAPL)
print(fit_AAPL)

# AMZN
fit_AMZN <- vgFit(Return_AMZN)
print(fit_AMZN)


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

### NIO ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 0.11840) * log(1 - 0.13136 * 0.11840 - 0.5 * 0.01151^2 * 0.11840)
# Panggil fungsi VG
result_NIO <- VG(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.01151, 
             nu = 0.11840, theta = 0.13136, K = 1, s0 = 5.620)
# Tampilkan hasil
print(result_NIO)


### AAPL ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 0.3076518) * log(1 - 0.0007700 * 0.3076518 - 0.5 * 0.0135346^2 * 0.3076518)
# Panggil fungsi VG
result_AAPL <- VG(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.0135346, 
                 nu = 0.3076518, theta = 0.0007700, K = 5, s0 = 233.85)
# Tampilkan hasil
print(result_AAPL)


### AMZN ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega <- (1 / 3.102051) * log(1 - 0.003719 * 3.102051 - 0.5 * 0.053301^2 * 3.102051)
# Panggil fungsi VG
result_AMZN <- VG(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.053301, 
                  nu = 3.102051, theta = 0.003719, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_AMZN)



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
### NIO ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG_AVR
omega <- (1 / 0.11840) * log(1 - 0.13136 * 0.11840 - 0.5 * 0.01151^2 * 0.11840)
# Panggil fungsi VG
result_NIO_2 <- VG_AVR(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.01151, 
                 nu = 0.11840, theta = 0.13136, K = 1, s0 = 5.620)
# Tampilkan hasil
print(result_NIO_2)


### AAPL ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG_AVR
omega <- (1 / 0.3076518) * log(1 - 0.0007700 * 0.3076518 - 0.5 * 0.0135346^2 * 0.3076518)
# Panggil fungsi VG
result_AAPL_2 <- VG_AVR(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.0135346, 
                  nu = 0.3076518, theta = 0.0007700, K = 5, s0 = 233.85)
# Tampilkan hasil
print(result_AAPL_2)


### AMZN ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG_AVR
omega <- (1 / 3.102051) * log(1 - 0.003719 * 3.102051 - 0.5 * 0.053301^2 * 3.102051)
# Panggil fungsi VG
result_AMZN_2 <- VG_AVR(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.053301, 
                  nu = 3.102051, theta = 0.003719, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_AMZN_2)



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
### NIO ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG_IMS
omega <- (1 / 0.11840) * log(1 - 0.13136 * 0.11840 - 0.5 * 0.01151^2 * 0.11840)
# Panggil fungsi VG
result_NIO_3 <- VG_IMS(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.01151, 
                       nu = 0.11840, theta = 0.13136, K = 1, s0 = 5.620)
# Tampilkan hasil
print(result_NIO_3)


### AAPL ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG_IMS
omega <- (1 / 0.3076518) * log(1 - 0.0007700 * 0.3076518 - 0.5 * 0.0135346^2 * 0.3076518)
# Panggil fungsi VG
result_AAPL_3 <- VG_IMS(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.0135346, 
                        nu = 0.3076518, theta = 0.0007700, K = 5, s0 = 233.85)
# Tampilkan hasil
print(result_AAPL_3)


### AMZN ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG_IMS
omega <- (1 / 3.102051) * log(1 - 0.003719 * 3.102051 - 0.5 * 0.053301^2 * 3.102051)
# Panggil fungsi VG
result_AMZN_3 <- VG_IMS(5000000, T = 31 / 252, r = 0.05, omega = omega, sigmavg = 0.053301, 
                        nu = 3.102051, theta = 0.003719, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_AMZN_3)





