#membaca dataset iris
iris <- read.csv("C:/Users/LENOVO/Documents/Analisis Prediktif/PRAK/iris (1).csv")
head(iris) #menampilkan beberapa baris pertama dari data iris
str(iris) #menampilkan struktur data iris

#simple linear regression - coefficient
fit_1 = lm(sepal_length~petal_length, data=iris)
fit_1

#simple linear regression - without intercept/konstanta
fit_2 = lm(sepal_length~petal_length-1, data=iris)
fit_2

#comparation -- grafik perbandingan 
plot(iris$petal_length, iris$sepal_length)
abline(fit_1, col='red')
abline(fit_2, col='blue')

#simple linear regression - categorical variable
class(iris$species)
table(iris$species)

#simple linear regression - memodelkan hubungan antara sepal_length dengan species
fit_3 = lm(sepal_length~species, data=iris)
fit_3

#the design martix
model.matrix(fit_3)
