###COI Analysis###
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)
library(psych)
library(readxl)
library(lavaan)
library(semTools)
library(semPlot)
library(corrplot)

##Dosen##
coi_dosen = read_excel("DATA - responden COI.xlsx", 
                        sheet = "Dosen_IRT", col_types = c("skip", 
                                                           "skip", "skip", "skip", "skip", "skip", 
                                                           "skip", "numeric", "skip", "skip", 
                                                           "skip", "skip", "skip", "numeric", 
                                                           "skip", "skip", "skip", "skip", "numeric", 
                                                           "skip", "skip", "skip", "skip", "skip", 
                                                           "numeric", "skip", "skip", "skip", 
                                                           "skip", "skip", "numeric"))
coi_dosen2 = read_excel("DATA - responden COI.xlsx",
                        sheet = "Dosen_IRT", col_types = c("skip",
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "skip", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "skip", 
                                                           "numeric", "numeric", "numeric", "numeric", "skip", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "skip", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "skip"))
colnames(coi_dosen2) = c("a1", "a2", "a3", "a4", "a5", "a6", "b1", "b2", "b3", "b4", "b5", "c1", "c2", "c3", "c4",
                         "d1", "d2", "d3", "d4", "d5", "e1", "e2", "e3", "e4", "e5")
coi_dosen3 = read_excel("DATA - responden COI.clean.xlsx",  sheet = "DOSEN (filter)")
#Reliability and validity
KMO(coi_dosen2)
alpha(coi_dosen2)#Good reliability score
#CFA
cor.plot(coi_dosen[,1:5])
model1 = "
komp =~ a1 + a2 + a3 + a4 + a5 + a6
sos =~ b1 + b2 + b3 + b4 + b5
komu =~ c1 + c2 + c3 + c4
moocs =~ d1 + d2 + d3 + d4 +d5
daring =~ e1 + e2 + e3 + e4 + e5
siap =~ a*sos + a*komu + a*moocs + a*daring
komp ~~ 0*siap"
cfact = cfa(model1, data=coi_dosen2, std.lv=TRUE, auto.cov.lv.x=FALSE) 
summary(cfact, fit.measures = T, standardized = T)
semPaths(cfact, 'path', 'std', style = 'lisrel',
         edge.color = 'black', intercepts = F)
#Chi square
chisq.test(coi_dosen3$`Pengalaman mengikuti MOOCs sebelumnya`, coi_dosen3$Kat_komp)
chisq.test(coi_dosen3$`Pengalaman mengikuti MOOCs sebelumnya`, coi_dosen3$Kat_sos)
chisq.test(coi_dosen3$`Pengalaman mengikuti MOOCs sebelumnya`, coi_dosen3$Kat_komu)
chisq.test(coi_dosen3$`Pengalaman mengikuti MOOCs sebelumnya`, coi_dosen3$Kat_MOOCS)
chisq.test(coi_dosen3$`Pengalaman mengikuti MOOCs sebelumnya`, coi_dosen3$Kat_daring)

chisq.test(coi_dosen3$`Pengalaman memfasilitasi pembelajaran daring di MOOCs sebelumnya`, coi_dosen3$Kat_komp)
chisq.test(coi_dosen3$`Pengalaman memfasilitasi pembelajaran daring di MOOCs sebelumnya`, coi_dosen3$Kat_sos)
chisq.test(coi_dosen3$`Pengalaman memfasilitasi pembelajaran daring di MOOCs sebelumnya`, coi_dosen3$Kat_komu)
chisq.test(coi_dosen3$`Pengalaman memfasilitasi pembelajaran daring di MOOCs sebelumnya`, coi_dosen3$Kat_MOOCS)
chisq.test(coi_dosen3$`Pengalaman memfasilitasi pembelajaran daring di MOOCs sebelumnya`, coi_dosen3$Kat_daring)

#Hierarchical Clustering
hier.coidosen = hclust(dist(coi_dosen), method = "complete")
plot(hier.coidosen)
fviz_nbclust(coi_dosen, hcut)
# Decide by dendogram we have 2 clusters
clusterCut.dosen = cutree(hier.coidosen, 2)
coi_dosen$cluster.hier = clusterCut.dosen
coi_dosen %>% group_by(cluster.hier) %>% 
  summarise(mean.komputer = mean(`Kompetensi Komputer`),
            mean.sosial = mean(`Kompetensi Sosialisasi`),
            mean.komunikasi = mean(`Kompetensi Komunikasi`),
            mean.moocs = mean(`Hasrat MOOCS`),
            mean.daring = mean(`Kompetensi Komunikasi Daring`))
#K Means
fviz_nbclust(coi_dosen, kmeans, method = "wss", k.max = 6)
fviz_nbclust(coi_dosen, kmeans, method = "silhouette", k.max = 6)
gap_stat.coidosen = clusGap(coi_dosen, FUN = kmeans, nstart = 25,
                   K.max = 6, B = 1000)
fviz_gap_stat(gap_stat.coidosen)
#KMeans seems to agree on 2 clusters as well
km_coidosen = kmeans(coi_dosen, iter.max = 1000, centers = 2, nstart = 100)
fviz_cluster(km_coidosen, coi_dosen, stand = F, geom = "point")
coi_dosen$cluster.km = km_coidosen$cluster
coi_dosen %>% group_by(cluster.km) %>% 
  summarise(mean.komputer = mean(`Kompetensi Komputer`),
            mean.sosial = mean(`Kompetensi Sosialisasi`),
            mean.komunikasi = mean(`Kompetensi Komunikasi`),
            mean.moocs = mean(`Hasrat MOOCS`),
            mean.daring = mean(`Kompetensi Komunikasi Daring`))


##Mahasiswa##
coi_mahasiswa = read_excel("DATA - responden COI.xlsx", 
                       sheet = "Mahasiswa_IRT", col_types = c("skip", 
                                                          "skip", "skip", "skip", "skip", "skip", 
                                                          "skip", "numeric", "skip", "skip", 
                                                          "skip", "skip", "skip", "numeric", 
                                                          "skip", "skip", "skip", "skip", "numeric", 
                                                          "skip", "skip", "skip", "skip", "skip", 
                                                          "numeric", "skip", "skip", "skip", 
                                                          "skip", "skip", "numeric"))
coi_mahasiswa2 = read_excel("DATA - responden COI.xlsx",
                            sheet = "Mahasiswa_IRT", col_types = c("skip", "numeric", "numeric", "numeric",
                                                                   "numeric", "numeric", "numeric", "skip", 
                                                                   "numeric", "numeric", "numeric", "numeric", 
                                                                   "numeric", "skip", "numeric", "numeric", 
                                                                   "numeric", "numeric", "skip",
                                                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                   "skip", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "skip"))
colnames(coi_mahasiswa2) = c("a1", "a2", "a3", "a4", "a5", "a6", "b1", "b2", "b3", "b4", "b5", "c1", "c2", "c3", "c4",
                         "d1", "d2", "d3", "d4", "d5", "e1", "e2", "e3", "e4", "e5")
#Reliability and validity
KMO(coi_mahasiswa2)
alpha(coi_mahasiswa2)#Good reliability score
#CFA
cor.plot(coi_mahasiswa)
model2 = "
komp =~ a1 + a2 + a3 + a4 + a5 + a6
sos =~ b1 + b2 + b3 + b4 + b5
komu =~ c1 + c2 + c3 + c4
moocs =~ d1 + d2 + d3 + d4 +d5
daring =~ e1 + e2 + e3 + e4 + e5
siap =~ komp + sos + komu + moocs + daring
siap~~siap"
cfact2 = cfa(model2, data=coi_dosen2, std.lv=TRUE, auto.cov.lv.x=FALSE) 
summary(cfact2, fit.measures = T, standardized = T)
semPaths(cfact2, 'path', 'std', style = 'lisrel',
         edge.color = 'black', intercepts = F)

#Hierarchical Clustering
hier.coimhs = hclust(dist(coi_mahasiswa), method = "complete")
fviz_nbclust(coi_mahasiswa, hcut)
# Decide by dendogram we have 2 clusters
clusterCut.mhs = cutree(hier.coimhs, 2)
coi_mahasiswa$cluster.hier = clusterCut.mhs
coi_mahasiswa %>% group_by(cluster.hier) %>% 
  summarise(mean.komputer = mean(`Kompetensi Komputer`),
            mean.sosial = mean(`Kompetensi Sosialisasi`),
            mean.komunikasi = mean(`Kompetensi Komunikasi`),
            mean.moocs = mean(`Hasrat MOOCS`),
            mean.daring = mean(`Kompetensi Komunikasi Daring`))
#K Means
fviz_nbclust(coi_mahasiswa, kmeans, method = "wss", k.max = 6)
fviz_nbclust(coi_mahasiswa, kmeans, method = "silhouette", k.max = 6)
gap_stat.coimhs = clusGap(coi_mahasiswa, FUN = kmeans, nstart = 25,
                            K.max = 6, B = 1000)
fviz_gap_stat(gap_stat.coimhs)
#KMeans seems to agree on 2 clusters as well
km_coimhs = kmeans(coi_mahasiswa, iter.max = 1000, centers = 2, nstart = 100)
fviz_cluster(km_coimhs, coi_mahasiswa, stand = F, geom = "point")
coi_mahasiswa$cluster.km = km_coimhs$cluster
coi_mahasiswa %>% group_by(cluster.km) %>% 
  summarise(mean.komputer = mean(`Kompetensi Komputer`),
            mean.sosial = mean(`Kompetensi Sosialisasi`),
            mean.komunikasi = mean(`Kompetensi Komunikasi`),
            mean.moocs = mean(`Hasrat MOOCS`),
            mean.daring = mean(`Kompetensi Komunikasi Daring`))
