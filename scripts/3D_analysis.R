setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/brassica_test/")
library(scatterplot3d)
?read.delim
ptcloud <- read.table("brassica_nopot_centered.xyz", header = FALSE)
head(ptcloud)
dim(ptcloud)
length(ptcloud)
tail(ptcloud)
colnames(ptcloud) <- paste(c("X","Y","Z"))
#downsample for plotting
?sample
keeps <- sample(rownames(ptcloud), 10000, replace = FALSE)
keeps
ptcld <- ptcloud[keeps,]
ptcld <- ptcld[order(row.names(ptcld)),]
ptcloud
head(ptcld)
colnames(ptcld) <- paste(c("X","Y","Z"))
ptcld
?scatterplot3d

scatterplot3d(ptcld$X, ptcld$Y, ptcld$Z)

library(rgl)
plot3d(ptcld$X, ptcld$Y, ptcld$Z, col="red", size=3) 

pc1 <- princomp(ptcld)
plot(pc1)

plot(ptcld$X, ptcld$Y)
plot(ptcld$X, ptcld$Z)
plot(ptcld$Y, ptcld$Z)

identify(ptcld$X, ptcld$Y, labels=row.names(ptcld))

hc <- hclust(dist(ptcld))
plot(hc)

ptkmeans <- kmeans(ptcld, 10)
plot(ptkmeans)

library(spatstat)
ptcldx <- ptcld$X
ptcldx
ptcldy <- ptcld$Y
ptcldz <- ptcld$Z
?pp3
X <- pp3(runif(10), runif(10), runif(10), box3(c(0,1)))
plot(X)
brdata <- pp3(x = ptcldx, y = ptcldy, z= ptcldz, box3(c(0,1)))
plot(brdata)

?K3est
?pcf3est
brdatakmeans <- K3est(brdata)
if(interactive()) plot(brdatakmeans)

distmat <- pairdist(brdata)
plot(distmat)
head(distmat)

Z <- pcf3est(brdata)
plot(Z)





#slices of the data
range(ptcld$Z)
?hist
hist(ptcld$Z)
hist(ptcld$X)
hist(ptcld$Y)

ptcld2 <- ptcloud
range(ptcld2$Z)
breaks <- seq(from = -0.015, to = 1.55, by = 0.05)
breaks
str(breaks)
breaks <- as.data.frame(breaks)
breaks$names<- row.names(breaks)
?cut
str(breaks)
ptcld2$brks <- cut(ptcld2$Z, breaks = breaks$breaks)


library(ggplot2)
head(ptcld2)
str(ptcld2)
?plot
plot(ptcld2$X, ptcld2$Y)

XYplot <- ggplot(ptcld2, aes(x= X, y= Y, fill = brks)) + stat_binhex(bins = 20) 
XYplot <- XYplot + guides(fill = FALSE) +  theme_bw()
XYplot
?stat_binhex
?geom_hex




# other play to rotate the point cloud etc.
# ptcld2 <- ptcld
# head(ptcld2)
# ptcld2$X <- ptcld2$X*-1
# ptcld2$Z <- ptcld2$Z*-1
# plot3d(ptcld2$X, ptcld2$Y, ptcld2$Z, col="red", size=3) 
# plot(ptcld2$X, ptcld2$Y)
# identify(ptcld2$X, ptcld2$Y, labels=row.names(ptcld2))
# ptcld2

# # make this point 0, 0, 0
# # 28967 0.933509 2.16680 3.06218
# ptcld2$X <- ptcld2$X + 0.940816
# ptcld2$Y <- ptcld2$Y - 2.15096
# ptcld2$Z <- ptcld2$Z + 3.05641
# plot3d(ptcld2$X, ptcld2$Y, ptcld2$Z, col="red", size=3) 

# # add min so all values are positive
# min(ptcld2$X)
# min(ptcld2$Y)
# min(ptcld2$Z)
# plot3d(ptcld2$X, ptcld2$Y, ptcld2$Z, col="red", size=3) 
# plot(ptcld2$X, ptcld2$Y)
# plot(ptcld2$X, ptcld2$Z)

# ptcld2$X <- t(ptcld2$X)


# library(vmrlgen)
# ?points3d
# vrml.open()
# points3d(ptcld2$X, ptcld2$Y, ptcld2$Z, pointstyle = "s")
# cloud3d(ptcld2$X, ptcld2$Y, ptcld2$Z)
# ?cloud3d
