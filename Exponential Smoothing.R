#seasonal additive Holt-Winters
#membaca dataset
co2

#menampilkan plot time series data
plot(co2)

#menerapkan Holt-Winters aditif (default)
model1 <- HoltWinters(co2)
fore1 <- predict(model1, 50, prediction.interval = TRUE) #meramalkan 50 periode ke depan
plot(model1,fore1) #menampikan plot dari model dan ramalan 
plot(fitted(model1)) #menjelaskan rinci ttg komponen model Holt-Winters, seperti seasonal, trend, dan level (xhat)

model1 #hasil dari model Holt-Winters yang dibuat dengan data deret waktu `co2`
fore1 #hasil dari peramalan yang dilakukan dengan model Holt-Winters

#seasonal multiplicative Holt-Winters
#membaca dataset
AirPassengers

#menampilkan plot time series dari data 
plot(AirPassengers)

#menerapkan Holt-Winters multiplicative
model2 <- HoltWinters(AirPassengers, seasonal = 'mult')
fore2 <- predict(model2, 24, prediction.interval = TRUE) #meramalkan 24 periode ke depan
plot(model2, fore2) #menampikan plot dari model dan ramalan 
plot(fitted(model2)) #menjelaskan rinci ttg komponen model Holt-Winters, seperti seasonal, trend, dan level (xhat)

model2 #hasil dari model Holt-Winters yang dibuat dengan data deret waktu `AirPassengers`
fore2 #hasil dari peramalan yang dilakukan dengan model Holt-Winters

#model Holt-Winters Non Seasonal atau Model Eksponensial Ganda
#membaca dataset
uspop

#error N(0,5) ditambahkan ke data
x <- uspop + rnorm (uspop, sd = 5)
x

model3 <- HoltWinters(x, gamma = FALSE)
#gamma = FALSE jika objek time series tidak mengandung seasonal
model3
model3$SSE

fore3 <- predict(model3, 5, prediction.interval = TRUE) #meramalkan 5 periode ke depan
fore3 #hasil dari peramalan yang dilakukan dengan model Holt-Winters
#menampikan plot dari model dan ramalan 
plot(model3, fore3)

#eksponensial smoothing sederhana
#membaca dataset
uspop

#error N(0,5) ditambahkan ke data
x <- uspop +rnorm(uspop, sd = 5)
x

model4 <- HoltWinters(x, gamma = FALSE, beta = FALSE)
#gamma dan beta = FALSE jika objek time series tidak mengandung seasonal
model4
model4$SSE

fore4 <- predict(model4, 5, prediction.interval = TRUE) #meramalkan 5 periode ke depan
fore4 #hasil dari peramalan yang dilakukan dengan model Holt-Winters
#menampikan plot dari model dan ramalan 
plot(model4, fore4)