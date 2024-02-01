#membuat dataframe
df <- data.frame(date = as.Date("2021-01-01") - 0:99,
                 sales = runif(100,10,500) + seq (50,149)^2)
#untuk melihat 6 baris pertama
head(df)
View(df)

#untuk menampilkan struktur dari suatu objek, termasuk dataframe
str(df)

#install package
install.packages("ggplot2")

#load package
library(ggplot2)

#membuat time series plot dengan x=date dan y=sales
p <- ggplot(df, aes(x = date, y = sales)) + geom_line() ; p

#mengubah data "EuStockMarkets ke dalam data frame "EuStock"
EuStock <- as.data.frame(EuStockMarkets)
#menampilkan beberapa baris pertama dari data frame "EuStock"
head(EuStock)

#menambahkan kolom "Date" ke data frame "EuStock"
EuStock$Date <- as.numeric(time(EuStockMarkets))
#menampilkan beberapa baris pertama dari data frame "EuStock" 
#setelah kolom "Date" ditambahkan
head(EuStock)
View(EuStock)

#membuat plot time series dengan x=date dan y=SMI
ggplot(EuStock, aes(x = Date, y = SMI)) + geom_line()
#membuat plot time series dengan x=date dan y=SMI
#dan memberikan label pada sumbu y
ggplot(EuStock, aes(x = Date, y = SMI)) + geom_line() + labs(y= "Closing Price of Switzerland (SMI) stock index")

#membuat plot time series untuk beberapa indeks saham (DAX, SMI, CAC, FTSE)
#setiap indeks saham diberi warna yang berbeda
ggplot(EuStock, aes(x= Date))+
  geom_line(aes(y=DAX), color = "black") + 
  geom_line(aes(y=SMI), color = "pink") + 
  geom_line(aes(y=CAC), color = "green") + 
  geom_line(aes(y=FTSE), color = "blue")

#membuat plot time series untuk beberapa indeks saham (DAX, SMI, CAC, FTSE)
#setiap indeks saham diberi warna yang berbeda menggunakan scale_color_manual
#lagenda warna juga diberi judul
ggplot(EuStock, aes(x= Date))+
  geom_line(aes(y = DAX, color = "DAX")) + 
  geom_line(aes(y = SMI, color = "SMI")) + 
  geom_line(aes(y = CAC, color = "CAC")) + 
  geom_line(aes(y = FTSE, color = "FTSE")) +
  scale_color_manual(values = c(DAX = "black", SMI ="pink", CAC = "green", FTSE = "blue")) +
  labs(color = "Stock Index", y = "Stock Index") #untuk memberi judul lagenda warna

#memuat data "kings" dari URL dan menampilkan isinya
#skip = 3 berarti bahwa R akan melewati (tidak membaca) tiga baris pertama 
#dari data yang diimpor dari URL tersebut
kings <- scan("https://robjhyndman.com/tsdldata/misc/kings.dat", skip=3)
kings
#membuat time series dengan data "kings" dan menampilkannya
kingstimeseries = ts(kings)
kingstimeseries
#membuat plot time series kings
plot.ts (kingstimeseries)

#memuat data "births" dari URL dan menampilkan isinya
births <- scan("https://robjhyndman.com/tsdldata/data/nybirths.dat")
#membuat time series dengan data "births" dengan frekuensi 12 dan dimulai dari tahun 1946
birthstimeseries = ts(births, frequency=12, start = c(1946,1))
birthstimeseries
#membuat plot time series births
plot.ts(birthstimeseries)