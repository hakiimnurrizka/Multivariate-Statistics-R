y1 = c(121, 108, 122, 77, 140, 108, 124, 130, 149,  129, 154, 145, 112, 120, 118, 141, 135, 151, 97, 109,
       132, 123, 129, 131, 110, 47, 125, 129, 130, 147, 159, 135, 100, 149, 149, 153, 136, 97, 141, 164)
y2 = c(22, 30, 49, 37, 35, 37, 39, 34, 55, 38, 37, 33, 40, 39, 21, 42, 49, 37, 46, 42,
       17, 32, 31, 23, 24, 22, 32, 29, 26, 47, 37, 41, 35, 37, 38, 27, 31, 36, 37, 32)
y3 = c(74, 80, 87, 66, 71, 57, 52, 89, 91, 72, 87, 88, 60, 73, 83,  80, 73, 76, 83, 82,
       77, 79, 96, 67, 96, 87, 87, 102, 104, 82, 80, 83, 83, 94, 78, 89, 83, 100, 105, 76)
y4 = c(223, 175, 266, 178, 175, 241, 194, 200, 198, 162, 170, 208, 232, 159, 152, 195, 152, 223, 164, 188,
       232, 192, 250, 291, 239, 231, 227, 234, 256, 240, 227, 216, 183, 227, 258, 283, 257, 252, 250, 187)
y5 = c(54, 40, 41, 80, 38, 59, 72, 85, 50, 47, 60, 51, 29, 39, 88, 36, 42, 74, 31, 57,
       50, 64, 55, 48, 42, 40, 30, 58, 58, 30, 58, 39, 57, 30, 42, 66, 31, 30, 27, 30)
y6 = c(254, 300, 223, 209, 261, 245, 242, 242, 277, 268, 244, 228, 279, 233, 233, 241, 249, 268, 243, 267,
       249, 315, 319, 310, 268, 217, 324, 300, 270, 322, 317, 306, 242, 240, 271, 291, 311, 225, 243, 264)
profesi = c(rep("engineer",20),rep("pilot",20))

#### PROFILE ANALYSIS
library(profileR)
data(spouse)
modprof = pbg(data=spouse[,1:4], group = spouse[,5], profile.plot = TRUE)
summary(modprof)

y = cbind(y1, y2, y3, y4, y5, y6)
ypepeg = cbind(scale(y1), scale(y2), scale(y3), scale(y4), scale(y5), scale(y6))
modasu = pbg(data = Data[,1:6], group = Data[,7], profile.plot = TRUE)
modprof2 = pbg(y, profesi , profile.plot = TRUE)
modprof3 = pbg(ypepeg, profesi, profile.plot = TRUE)
summary(modprof2)
summary(modprof3)


#### MANOVA
## Melihat KOLINEARITAS
library(dplyr)
library(Matrix)
names(iris)
la<-iris %>% group_by(Species)%>%
  summarise(mean_sepal_l=mean(Sepal.Length),
            mean_sepal_w=mean(Sepal.Width),
            mean_petal_l=mean(Petal.Length))

### Kolinear jika matriks mean memiliki rank =< 1
pepeg = la[,2:4]
pepeg = as.matrix(pepeg)
rankMatrix(pepeg)

### Kolinear secara grafis, jika titik2 yang terbentuk dapat dihubungkan oleh 1 garis lurus
library(car)
library(rgl)
la<-data.frame(la)
scatter3d(la$mean_sepal_l,la$mean_sepal_w,la$mean_petal_l,surface=FALSE)

fuk = Data %>% group_by(profesi) %>%
  summarise(mean_y1 = mean(as.numeric(y1)),
            mean_y2 = mean(as.numeric(y2)),
            mean_y3 = mean(as.numeric(y3)),
            mean_y4 = mean(as.numeric(y4)),
            mean_y5 = mean(as.numeric(y5)),
            mean_y6 = mean(as.numeric(y6)))
rankMatrix(fuk[,2:7])

### Jika Kolinear, kekuatan uji : roy > lawley > wilk > pillai. 
### Jika tidak, tanda berlaku sebaliknya
data(iris)
### UJI MANOVA 1 ARAH
library(RVAideMemoire)
library(biotools)
iris[,1:3]
man = manova(cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length)~iris$Species)
summary(man)
summary(man, test = 'Wilks')
summary(man, test ='Hotelling-Lawley')
summary(man, test = 'Roy')
#Uji MANOVA signifikan, lanjut ANOVA untuk tiap y
lm1 = lm(iris$Sepal.Length~iris$Species)
anova(lm1)
lm2 = lm(iris$Sepal.Width~iris$Species)
anova(lm2)
lm3 = lm(iris$Petal.Length~iris$Species)
anova(lm3)

## UJI asumsi univariat
leveneTest(iris$Sepal.Length~iris$Species)
#jika tidak homogen gunakan oneway.test
lm1 = oneway.test(iris$Sepal.Length~iris$Species)
lm1


## Uji Asumsi Multivariat
#Normalitas
mshapiro.test(man$residuals)
#Homogenitas Kovarian
boxM(man$residuals, iris[,5])

##Tidak memenuhi normalitas, cek jumlah data
table(iris[,5])
#Tiap kelompok >30, prediksi bisa dilanjut

##Tidak memnuhi homogenitas, cek kesamaan jumlah data
#Tiap kelompok sama, aman untuk lanjut orediksi


### UJI MANOVA  2 ARAH
iris$Species2 = c(rep("A",25),rep("B",50),rep("C",50),rep("A",25))
## Interaksi
man2 = manova(cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length)~iris$Species*iris$Species2)
van = summary(man2)
van
van$stats
# Interaksi tidak signifikan, keluarkan dari model
man2 = manova(cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length)~iris$Species+iris$Species2)
summary(man2)
#spec2 tdk signifikan : buang
#jika signifikan semua, gunakan lalu uji masing2

## Uji Asumsi Multivariat
#Normalitas
mshapiro.test(man2$residuals)
#Homogenitas Kovarian
boxM(man2$residuals, iris[,5])
boxM(man$residuals, iris[,6])

##Tidak memenuhi normalitas, cek jumlah data
table(iris[,5])
table(iris[,6])
#Tiap kelompok >30, prediksi bisa dilanjut

##misal interaksi signifikan
iris$species3 = paste0(iris$Species,iris$Species2)
## uji asumsi
boxM(man$residuals, iris[,7])
