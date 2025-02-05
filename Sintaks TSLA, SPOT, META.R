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

########## TSLA #########
cat("==== Data TSLA ====\n")
Return_TSLA <- calculate_return_stats("C:/Users/MUHAMMAD RAINNUL/Downloads/TSLA_Adj_Close_Data_1.xlsx")
print(Return_TSLA)  # Verifikasi apakah nilai Return_TSLA telah dihitung dengan benar


predicted_TSLA <- calculate_option_price(K = 50, r = 0.05, sigma = sqrt(0.001772), T = 31/252, S0 = 219.57)
calculate_mape(actual_price = 291.95, predicted_price = 169.8766)


########## SPOT #########
cat("==== Data SPOT ====\n")
Return_SPOT <- calculate_return_stats("C:/Users/MUHAMMAD RAINNUL/Downloads/SPOT_Adj_Close_Data_1.xlsx")

predicted_SPOT <- calculate_option_price(K = 200, r = 0.05, sigma = sqrt(0.000196), T = 31/252, S0 = 373.61)
calculate_mape(actual_price = 201.71, predicted_price = 174.8364)


########## META #########
cat("==== Data META ====\n")
Return_META <- calculate_return_stats("C:/Users/MUHAMMAD RAINNUL/Downloads/META_Adj_Close_Data_1.xlsx")

predicted_META <- calculate_option_price(K = 5, r = 0.05, sigma = sqrt(0.000196), T = 31/252, S0 = 590.16)
calculate_mape(actual_price = 573.94, predicted_price = 585.1907)

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

# Contoh penggunaan fungsi Gram-Charlier untuk opsi TSLA
cat("==== Harga Opsi dengan Gram-Charlier untuk TSLA ====\n")
call.gc(K = 50, S0 = 219.57, sigma = sqrt(0.001772), T = 31/252, r = 0.05, skew = skewness(Return_TSLA), kurt = kurtosis(Return_TSLA))
calculate_mape(actual_price = 291.95, predicted_price =  169.8766)

# Contoh penggunaan fungsi Gram-Charlier untuk opsi SPOT
cat("==== Harga Opsi dengan Gram-Charlier untuk SPOT ====\n")
call.gc(K = 200, S0 = 373.61, sigma = sqrt(0.000196), T = 31/252, r = 0.05, skew = skewness(Return_SPOT), kurt = kurtosis(Return_SPOT))
calculate_mape(actual_price = 201.71, predicted_price = 174.8364)

# Contoh penggunaan fungsi Gram-Charlier untuk opsi META
cat("==== Harga Opsi dengan Gram-Charlier untuk META ====\n")
call.gc(K = 5, S0 = 590.16, sigma = sqrt(0.000196), T = 31/252, r = 0.05, skew = skewness(Return_META), kurt = kurtosis(Return_META))
calculate_mape(actual_price = 573.94, predicted_price = 585.1907)


##############################################################
## Plot Variance Gamma untuk Return TSLA, SPOT, DAN META
ppvg(Return_TSLA, main='Variance Gamma P-P Plot - TSLA')
ppvg(Return_SPOT, main='Variance Gamma P-P Plot - SPOT')
ppvg(Return_META, main='Variance Gamma P-P Plot - META')

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

## Memanggil Fungsi Goodness of Fit untuk Return TSLA, SPOT DAN META
gof_vg(Return_TSLA, name = "Return TSLA")
gof_vg(Return_SPOT, name = "Return SPOT")
gof_vg(Return_META, name = "Return META")


# Estimasi parameter Variance Gamma
# TSLA
fit_TSLA <- vgFit(Return_TSLA)
print(fit_TSLA)

# SPOT
fit_SPOT <- vgFit(Return_SPOT)
print(fit_SPOT)

# META
fit_META <- vgFit(Return_META)
print(fit_META)


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


### TSLA ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega_TSLA<- (1 / 0.599239) * log(1 - (-0.003346) * 0.599239 - 0.5 * 0.034534^2 * 0.599239)
# Panggil fungsi VG
#n=5000000
result_TSLA_n1 <- VG(5000000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
             nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_n1)

#n=500000
result_TSLA_n2 <- VG(500000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                  nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_n2)

#n=5000
result_TSLA_n3 <- VG(5000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_n3)

#n=50
result_TSLA_n4 <- VG(50, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_n4)

#n=5
result_TSLA_n5 <- VG(5, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_n5)

### SPOT ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega_SPOT<- (1 / 0.1183963) * log(1 - 0.0024125 * 0.1183963 - 0.5 * 0.0314861^2 * 0.1183963)
# Panggil fungsi VG
#n=5000000
result_SPOT_n1 <- VG(5000000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                 nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_n1)

#n=500000
result_SPOT_n2 <- VG(500000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_n2)

#n=5000
result_SPOT_n3 <- VG(5000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_n3)

#n=50
result_SPOT_n4 <- VG(50, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_n4)

#n=5
result_SPOT_n5 <- VG(5, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_n5)

### META ###
# Hitung omega terlebih dahulu, dan kemudian panggil fungsi VG
omega_META<- (1 / 1.058061) * log(1 - 0.001169 * 1.058061 - 0.5 * 0.021349^2 * 1.058061)
# Panggil fungsi VG
#n=5000000
result_META_n1 <- VG(5000000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                  nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_n1)

#n=500000
result_META_n2 <- VG(500000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_n2)

#n=5000
result_META_n3 <- VG(5000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_n3)

#n=50
result_META_n4 <- VG(50, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_n4)

#n=5
result_META_n5 <- VG(5, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_n5)


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

####TSLA###
# Panggil fungsi VG
#n=5000000
result_TSLA_AVR_n1 <- VG_AVR(5000000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_AVR_n1)

#n=500000
result_TSLA_AVR_n2 <- VG_AVR(500000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_AVR_n2)

#n=5000
result_TSLA_AVR_n3 <- VG_AVR(5000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_AVR_n3)

#n=50
result_TSLA_AVR_n4 <- VG_AVR(50, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_AVR_n4)

#n=5
result_TSLA_AVR_n5 <- VG_AVR(5, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                     nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_AVR_n5)

### SPOT ###
# Panggil fungsi VG
#n=5000000
result_SPOT_AVR_n1 <- VG_AVR(5000000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_AVR_n1)

#n=500000
result_SPOT_AVR_n2 <- VG_AVR(500000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_AVR_n2)

#n=5000
result_SPOT_AVR_n3 <- VG_AVR(5000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_AVR_n3)

#n=50
result_SPOT_AVR_n4 <- VG_AVR(50, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_AVR_n4)

#n=5
result_SPOT_AVR_n5 <- VG_AVR(5, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                     nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_AVR_n5)

### META ###
# Panggil fungsi VG
#n=5000000
result_META_AVR_n1 <- VG_AVR(5000000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_AVR_n1)

#n=500000
result_META_AVR_n2 <- VG_AVR(500000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_AVR_n2)

#n=5000
result_META_AVR_n3 <- VG_AVR(5000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_AVR_n3)

#n=50
result_META_AVR_n4 <- VG_AVR(50, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_AVR_n4)

#n=5
result_META_AVR_n5 <- VG_AVR(5, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                     nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_AVR_n5)



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

####TSLA###
# Panggil fungsi VG
#n=5000000
result_TSLA_IMS_n1 <- VG_IMS(5000000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                             nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_IMS_n1)

#n=500000
result_TSLA_IMS_n2 <- VG_IMS(500000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                             nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_IMS_n2)

#n=5000
result_TSLA_IMS_n3 <- VG_IMS(5000, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                             nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_IMS_n3)

#n=50
result_TSLA_IMS_n4 <- VG_IMS(50, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                             nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_IMS_n4)

#n=5
result_TSLA_IMS_n5 <- VG_IMS(5, T = 31 / 252, r = 0.05, omega = omega_TSLA, sigmavg = 0.034534, 
                             nu = 0.599239, theta = -0.003346, K = 50, s0 = 219.57)
# Tampilkan hasil
print(result_TSLA_IMS_n5)

### SPOT ###
# Panggil fungsi VG
#n=5000000
result_SPOT_IMS_n1 <- VG_IMS(5000000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                             nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_IMS_n1)

#n=500000
result_SPOT_IMS_n2 <- VG_IMS(500000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                             nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_IMS_n2)

#n=5000
result_SPOT_IMS_n3 <- VG_IMS(5000, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                             nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_IMS_n3)

#n=50
result_SPOT_IMS_n4 <- VG_IMS(50, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                             nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_IMS_n4)

#n=5
result_SPOT_IMS_n5 <- VG_IMS(5, T = 31 / 252, r = 0.05, omega = omega_SPOT, sigmavg = 0.0314861, 
                             nu = 0.1183963, theta = 0.0024125, K = 200, s0 = 373.61)
# Tampilkan hasil
print(result_SPOT_IMS_n5)

### META ###
# Panggil fungsi VG
#n=5000000
result_META_IMS_n1 <- VG_IMS(5000000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                             nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_IMS_n1)

#n=500000
result_META_IMS_n2 <- VG_IMS(500000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                             nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_IMS_n2)

#n=5000
result_META_IMS_n3 <- VG_IMS(5000, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                             nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_IMS_n3)

#n=50
result_META_IMS_n4 <- VG_IMS(50, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                             nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_IMS_n4)

#n=5
result_META_IMS_n5 <- VG_IMS(5, T = 31 / 252, r = 0.05, omega = omega_META, sigmavg = 0.021349, 
                             nu = 1.058061, theta = 0.001169, K = 85, s0 = 187.69)
# Tampilkan hasil
print(result_META_IMS_n5)






