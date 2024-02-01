#install package
install.packages("forecast")
install.packages("MLmetrics")

#load package
library(forecast) #untuk analisis peramalan dan pemodelan time series
library("TTR") #untuk menghitung indikator teknis seperti moving averages
library("graphics") #untuk menggambar grafik dan visualisasi data
library(MLmetrics) #untuk pengukuran kinerja model machine learning
library(readxl) #untuk membaca data dari file excel

#membaca data dari file excel
data_penjualan_indihome <- read_excel("C:/Users/sausa/Documents/Analisis Prediktif/PRAK/data penjualan indihome.xlsx")
#membuat plot dari data penjualan
plot.ts(data_penjualan_indihome$sales)

#mengubah data menjadi format time series
data.ts<-ts(data_penjualan_indihome$sales)
data.ts
#membuat plot dengan x = time period dan y = sales
ts.plot(data.ts, xlab="Time Period", ylab="Sales", main="Time Series Plot Data Sales")
#menambahkan titik2 ke dalam plot 
points(data.ts)

#menghitung rata2 simple moving average 4 tahun terakhir 
data.sma<-SMA(data.ts, n=4)
data.sma
#menyimpan data ramalan dengan NA (nilai kosong) sebagai nilai pertama
#dan menggabungkannya dengan hasil perhitungan rata2 bergerak
data.ramal<-c(NA,data.sma)
data.ramal

data.sma2 <- SMA(data.ts, n=2) #menghitung rata2 simple moving average 2 tahun terakhir
data.sma3 <- SMA(data.ts, n=3) #menghitung rata2 simple moving average 3 tahun terakhir
data.sma5 <- SMA(data.ts, n=5) #menghitung rata2 simple moving average 5 tahun terakhir

#menggabungkan data aktual, hasil pemulusan (SMA), dan data ramalan
data<-cbind(aktual=c(data.ts,rep(NA,5)),pemulusan=c(data.sma,rep(NA,5)),
            ramalan=c(data.ramal,rep(data.ramal[length(data.ramal)],4)))
data
#menggabungkan hasil pemulusan dengan n berbeda ke dalam data frame "data"
data<-cbind(data.ts, data.sma2, data.sma3, data.sma, data.sma5)
data

#membuat plot dgn membandingkan data aktual, hasil pemulusan (SMA), dan data peramalan
#dengan x = time period, y = sales, dan judul grafik "SMA N=4 Data Sales
ts.plot(data.ts, xlab="Time Period", ylab="Sales", main="SMA N=4 Data Sales")
points(data.ts) #memberikan titik2 pada plot untuk data aktual
lines(data.sma,col="green",lwd=2) #menambahkan garis ke plot untuk hasil pemulusan dgn SMA
lines(data.ramal,col="red",lwd=2) #menambahkan garis ke plot untuk data peramalan
#memberikan keterangan pada grafik
legend("topleft",c("data aktual","data pemulusan","data peramalan"),
       lty=8, col=c("black","green","red", cex=0.8))

#membuat plot dgn membandingkan data aktual, hasil pemulusan dengan berbagai jendela moving average (MA), 
#dan menambahkan legenda untuk menjelaskan elemen-elemen dalam grafik
ts.plot(data.ts, xlab="Time Period ", ylab="Sales", main= "SMA N=4 Data Sales")
points(data.ts)
lines(data.sma,col="green",lwd=2)
lines(data.sma2,col="yellow",lwd=2)
lines(data.sma3,col="red",lwd=2)
lines(data.sma5,col="blue",lwd=2)
legend("topleft",c("data aktual","data MA 4","data MA 2", "data MA 3",
                   "data MA 5"), lty=8, col=c("black","green","yellow", "red", "blue"), cex=0.8)

#perhitungan beberapa metrik evaluasi kinerja digunakan untuk mengevaluasi pemodelan peramalan,
#Di sini kita mengevaluasi pemulusan moving average (MA) dgn jendela 2 (data.sma2) dgn data aktual (data.ts)
MAPE(data.sma2[-1], data.ts[-1])
MAE(data.sma2[-1], data.ts[-1])
MSE(data.sma2[-1], data.ts[-1])
RMSE(data.sma2[-1],data.ts[-1])

#membuat data frame yang memiliki 2 kolom, yaitu actual dan forecast
data <- data.frame(actual=c(54, 60, 55, 62, 62, 65, 63, 70),
                   forecast=c(58, 54, 60, 55, 62, 62, 65, 63))
data

#perhitungan beberapa metrik evaluasi kinerja digunakan untuk mengevaluasi pemodelan peramalan,
#berdasarkan data actual dan data forecast
MAPE(data$actual, data$forecast)
MAE(data$actual, data$forecast)
MSE(data$actual, data$forecast)
RMSE(data$actual, data$forecast)

# Double Moving Average
#Membuat Fungsi DMA
DMA <- function(data, orde) {
  #Mendefinisikan vektor MA Pertama
  s1=c()
  #Menghitung MA pertama
  for (i in orde:length(data)) {
    s1[i] = mean(data[(i-orde+1):i])
  }
  ##Mendefinisikan vektor MA Kedua
  s2=c()
  #Menghitung MA kedua
  for (j in (2*orde-1):length(data)) {
    s2[j] = mean(s1[(j-orde+1):j])
  }
  #Mendefinisikan Konstanta  dan KOefisien Slope
  a= c()
  b= c()
  #Menghitung Konstanta dan Koefisien Slope
  for (k in (2*orde-1):length(data)) {
    a[k] = s1[k] + (s1[k]-s2[k])
    b[k] = 2/(orde-1)*(s1[k]-s2[k])
  }
  #Mendefinisikan Ventor Peramalan
  f =c()
  #Menghitung Peramalan data
  f[2*orde-1] = a[2*orde-1] 
  for (l in (2*orde):(length(data)+1)) {
    f[l] = a[l-1]+b[l-1]
  }
  #Mendefinisikan Ventor Precentage Error
  PE = c()
  #menghitung PE
  for (m in (2*orde-1):length(data)){
    PE[m] = abs(data[m]-f[m])/data[m]*100
  }
  
  #Menghitung MAPE
  MAPE = mean(PE, na.rm = TRUE)
  
  #
  Hasil_Perhitungan = data.frame(Data = data, S1 = s1, S2 = s2, a = a, b = b, Ft= f[-length(f)], PE = PE)
  list (Hasil_perhitungan = Hasil_Perhitungan, MAPE = MAPE, Peramalan_1_Periode_kedepan = f[length(f)])
}

dma_2 = DMA(data.ts, 2) #DMA dengan orde 2
dma_2
dma_3 = DMA(data.ts, 3) #DMA dengan orde 3
dma_3
dma_4 = DMA(data.ts, 4) #DMA dengan orde 4
dma_4

#membuat plot time series data penjualan dan hasil peramalan menggunakan DMA dgn orde yg berbeda
ts.plot(data.ts, xlab="Time Period ", ylab="Sales", main= "DMA Data Sales")
points(data.ts)
lines(dma_2$Hasil_perhitungan$Ft,col="green",lwd=2)
lines(dma_3$Hasil_perhitungan$Ft,col="blue",lwd=2)
lines(dma_4$Hasil_perhitungan$Ft,col="red",lwd=2)
#menambahkan lagenda ke plot
legend("topleft",c("data aktual","data DMA 2", "data DMA 3", "data DMA 4"),
       lty=8, col=c("black","green", "blue",  "red"), cex=0.6)