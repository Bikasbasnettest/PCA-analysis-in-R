setwd("E:/MNCH 74-75W Report to press basnet/PCA analyis Folder")
Biku<-read.csv("E:/MNCH 74-75W Report to press basnet/PCA analyis Folder/PCA anaylis of the unused data set of over Location with wrong value of RLOD and SlOD.csv")
attach(Biku)
print(Biku)
View(Biku)
Biku=as.factor(Gen)
library(factoextra)
str(Biku)
#to select the require Nemeric Column only or reject all the Nominal column.
Elimin<-scale(Biku[, -1],center = TRUE)
Elimin
options(max.print = 1000)
options(scipen = 100)
#This code will give the standard deviation of the al the dataset
#spectral decomposition approach

pca1<-princomp(Elimin)
pca1
#to get the Biplot value
# Assuming you have pca1 <- princomp(Elimin)
# Create a biplot, biplot(pca1) this code creates Error in plot.new() : figure margins too large
biplot(pca1, var.axes = TRUE, col = "darkgreen", cex = 1, arrow.len = 0.1,
       main = "Biplot for PCA of MNHCM data",
       xlab = "PC1 (Principal Component 1)", ylab = "PC2 (Principal Component 2)")
pca1$scores
#singular value decomposition
pca2<-prcomp(Elimin)
pca2
pca2$x
summary(pca2)
biplot(pca2)
pca1$scores == pca2$x
#to used the Facto extra Packages we can do by this code
fviz_pca(pca2)
#if the data are too Much overlapped in the Plot
fviz_pca_biplot(pca2, repel = TRUE)
#if not need the not Biplot only need the PCA plot than
fviz_pca_var(pca2, repel = T)
#to get the individual ponit not the arrow pOINT
fviz_pca_ind(pca2)
#too get the screep Plot
fviz_screeplot(pca2)
#for the 3d visualization of the pca daat
R.version
library(rgl)# this packages is used for the 3d visualization of data
?rgl
#facttro miner?
library(FactoMineR)
#i used 24 becausee i have 24 different variables in xls
Mpca<-PCA(Elimin, ncp=24)
Mpca
#to get the different value , here i Used the Eign value
Mpca$eig
library(writexl)

#aka scores
Mpca$ind$coord
#quality of representation It shows the importance of a principal component for a given observation
Mpca$ind$cos2 
Mpca$var$contrib # contributions 
# biplot
fviz_pca(Mpca) 
fviz_pca_biplot(Mpca, repel = T) # repulsion avoid overlap
fviz_pca_biplot(Mpca, repel = T, col.ind = "cos2") # quality of representation
fviz_pca_biplot(Mpca, repel = T, col.ind = "cos2", col.var = "darkgreen") 
#To get the scatter plot of the PCA
fviz_pca_ind(Mpca, repel = T, col.ind = "cos2")  
# scree plot
fviz_screeplot(Mpca)
fviz_screeplot(Mpca, ncp=18)
fviz_screeplot(Mpca, ncp=18, geom="line")
fviz_screeplot(Mpca, ncp=18, geom="bar")
fviz_screeplot(Mpca, ncp=18, geom="bar", barfill="yellow") # above line only
fviz_screeplot(Mpca, choice="eigenvalue")
fviz_screeplot(Mpca, choice="eigenvalue", ncp=24)
Mpca$eig
table1<-Mpca$eig
class(table1)
table1<-as.data.frame(table1)
library(writexl)
write_xlsx(table1, "elgien value of the PCA.xlsx")
plot(table1$`cumulative percentage of variance`)

#to calculate the rotated componets
library(psych)
rpca <-principal(Elimin, nfactors = 12, rotate = "varimax", scores = T)
rpca
rpca$communality
print(rpca$loadings, digits = 3, cutoff = 0)
barplot(rpca$loadings)
barplot(rpca$loadings, beside = T)
barplot(rpca$loadings, beside = T, col = "blue", main = "Rotated component matrix")
#to color the different level graph
library(pals)
barplot(rpca$loadings, beside = T, col = brewer.accent(18), main = "Rotated component matrix")
barplot(rpca$loadings, beside = T, col = brewer.greens(18), main = "Rotated component matrix")
barplot(rpca$loadings, beside = T, col = brewer.spectral(18), main = "Rotated component matrix")
barplot(rpca$loadings, beside = T, col = alphabet(n=18), main = "Rotated component matrix")
