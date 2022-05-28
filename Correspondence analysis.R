library(FactoMineR)
library(factoextra)
library(gplots)
library(vcd)
### Correspondence Analysis ###
# Analyzing categorical data in a contingency table.
# The method is based upon modelling around the contingency count table, inferring independence between row and
#column, and drawing graphical representation to see which parts of column/row in a contingency are close
#to each other.

##Data
# Data used to illustrate this method is about students level of adaptability in online education
std_adapt = read.csv("~/Multivariate-Statistics-R/std_adapt.csv")
str(std_adapt) #14 categorical variables and 1205 obs
# We can present the above data in contingency tables
table(std_adapt[,1:2]) #gender x age
structable(std_adapt[,4:7]) #4-way : location x load x institution x IT

##CA
# To analyze with CA, we first choose which variables to be included, lets focus on : education.level,
#institution, internet type, class duration, and adaptability level
std_adapt1 = std_adapt[,c(3,4,9,11,14)]
tbl = structable(std_adapt1)
tbl #Very sparse contingency table
#Lets use a simple CA first to analyze pair of two variables : adaptability level and education level
sc.std1 = CA(table(std_adapt1[,c(1,5)]), graph = F)
fviz_screeplot(sc.std1, addlabels = T) #First dimension represent 88% total inertia (information)
fviz_ca_biplot(sc.std1, repel = T)
#Lets compare distance with distance matrix
sc.std2 = get_ca_col(sc.std1) #Extract CA result on column
sc.std3 = get_ca_row(sc.std1) #Extract CA result on row
coord.sc = rbind(sc.std2$coord, sc.std3$coord)
dist(coord.sc)#Distance between adaptability level and education level
#From the result above, college student seems to have the lowest relative adaptability level while university
#student has the highest relative adaptability level.
#Next we use multiple correspondence
mc.std = MCA(std_adapt1, graph = F)
fviz_screeplot(mc.std, addlabels = T) #First 2 dims represent 43% total inertia (information)
fviz_mca_var(mc.std, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = T)#Plot for each categories in each variables, color shows the quality of representation
#Lets check the distance for each variables' categories
mc.std1 = get_mca_var(mc.std)
dist(mc.std1$coord[c(1:3,11:13),])#Distance between adaptability level and education level
#From the distance matrix above, we can see a trend where higher level of relative adaptability level 
#seems to lean closer to higher education level
dist(mc.std1$coord[c(4:5,11:13),])#Distance between adaptability level and institution type for the school