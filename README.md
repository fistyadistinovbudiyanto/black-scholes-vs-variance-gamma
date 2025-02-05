# Analisis Model Black-Scholes dan Variance Gamma pada Saham Teknologi

## Deskripsi
Studi ini menganalisis harga opsi call dari saham teknologi yang tercatat di bursa efek AS menggunakan model Black-Scholes dan Variance Gamma. Data closing price saham dikumpulkan dari **13 Oktober 2023 - 14 Oktober 2024** melalui Yahoo Finance.

## Data
- **Saham:** NIO, AAPL, AMZN, GOOG, NVDA, MSFT, TSLA, SPOT, META  
- **Periode:** 13 Oktober 2023 - 14 Oktober 2024  
- **Opsi:** Call, jatuh tempo 15 November 2024  
- **Tingkat Bunga:** 5%  

## Analisis
### 1. Log-Return Saham
- Menghitung mean, variansi, skewness, kurtosis, dan uji normalitas Jarque-Bera.
- Saham NIO berdistribusi normal, sedangkan saham lain tidak.

### 2. Perbandingan Model Black-Scholes
- Membandingkan model **standar** vs. **Gram-Charlier expansion**.
- Hasil menunjukkan perbedaan kecil antara kedua metode.

### 3. Variance Gamma Model
- Mengestimasi parameter menggunakan Maximum Likelihood Estimation (MLE).
- Menyesuaikan distribusi return saham dengan model Variance Gamma.
- Model ini lebih fleksibel dibanding Black-Scholes karena menangkap skewness dan kurtosis return saham.

## Hasil dan Kesimpulan
- Model Variance Gamma memberikan hasil yang lebih realistis dibanding Black-Scholes untuk saham teknologi.
- Black-Scholes cocok hanya untuk saham dengan distribusi return mendekati normal.
- Variance Gamma lebih baik dalam menangkap heavy tails dan skewness return saham.
