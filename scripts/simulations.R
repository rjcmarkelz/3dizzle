install.packages("YplantQMC")
library(YplantQMC)
installQuasiMC

ls()
setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")
sugarmaple <- constructplant("sugarmaple.p", "leaffile.l")
plot(sugarmaple)
summary(sugarmaple)
str(sugarmaple)
class(sugarmaple)

?setLocation
laramie <- setLocation(lat = 41.3167, long = -105.5833)
plot(laramie)
?setMet
sunnyday <- setMet(laramie, month = 7, day = 12, nsteps = 12, Tmin = 12, Tmax = 28, PARday = 24)
plot(sunnyday)

?setPhy
clidlrc <- setPhy("lightresponse",leafpars=list(Amax=8.5, Rd=0.7, phi=0.045, theta=0.95, reflec=0.1, transmit=0.05))
YplantDay

?YplantDay
maplerun <- YplantDay(sugarmaple, met = sunnyday, phy=clidlrc, hemi=largegap)
head(maplerun)
str(maplerun)
?randomplant
summary(sugarmaple)

coneplant <- constructplant(randomplant(radius=250, height=500, leaflen=25, LA=1))

plot.leaffile(sugarmaple.l)
plot(readl("LeafFile.l"))
setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")

sugarmaple <- constructplant("PlantFile.p", "LeafFile.l")
plot(sugarmaple)
summary(sugarmaple)
str(sugarmaple)

coneplantQ <- randomplant(radius=250, height=500, crownshape="CONE",
leaflen=65, LA=0.5, crownbase=100, lfile="LeafFile.l")
plot(coneplantQ)

coneplant <- constructplant(coneplantQ, lfile = "LeafFile.l")
plot(coneplant)


testplant <- constructplant("PlantFiletest.p", "LeafFiletest.l")
plot(testplant)
summary(testplant)
str(testplant)

toona_fromabove <- projectplant(toona, azimuth=0, altitude=90)
plot(toona_fromabove)
?constructplant

plot(toona)
plot(toona, shiftxyz = c(750,0,0), add = TRUE)

constructplant
testplot <- plot(toona)
str(testplot)
head(testplot)
tail(testplot)
str(toona)

testplot <- as.data.frame(testplot)
testplot
scatterplot3d(testplot$X, testplot$Y, testplot$Z)
library(scatterplot3d)

plot.plant3d()
pc1 <- princomp(testplot)
plot(pc1)

testheader <- head(testplot, 100)
hc <- hclust(dist(testheader))
plot(hc)
head(hc)


head(testplot, 50)
?ashape3d

library(alphashape3d)
setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/brassica_test/")
ptcloud <- read.table("brassica_nopot_centered.xyz", header = FALSE)
ptcloud <- as.matrix(ptcloud)
str(ptcloud)
?sample
dim(ptcloud)

plot3d(ptcloud)

ptcloud2 <- ptcloud[sample(1:100, replace = FALSE), ]
dim(ptcloud2)

ptcloud3 <- ashape3d(ptcloud2, .01)

T1 <- rtorus(1000, 0.5, 2)
T2 <- rtorus(1000, 0.5, 2, ct = c(2, 0, 0), rotx = pi/2)
x <- rbind(T1, T2)
# Value of alpha
alpha <- 0.25
# 3D alpha-shape
ashape3d.obj <- ashape3d(x, alpha = alpha)
plot(ashape3d.obj)
















#voxR play!
library(VoxR)
data(treecloud)
head(treecloud)
dim(treecloud)

treecloud_vox <- vox(treecloud,res=0.02)
require(rgl)
library(rgl)
open3d()
plot3d(treecloud_vox,size=0.1)
plot3d(treecloud)

plot3d(ptcloud)
ptcloud_vox <- vox(ptcloud,res=0.05)
plot3d(ptcloud_vox)



dist <- axis.angle(ptcloud_vox,axis="X",projected=TRUE,plan="xy")

ptcloud_vox[,4] <- dist
#- density plot
plot(density(dist,na.rm=TRUE))
#- visualisation
z <- c(sort(unique(round(ptcloud_vox[,4],digits=0)),decreasing=TRUE))
col <- rainbow(n=length(z),start=0,end=2/6)
library(rgl)
open3d()
for(i in 1:length(z)){
a <- subset(ptcloud_vox,round(ptcloud_vox[,4],digits=0)==z[i])
plot3d(a,col=col[i],add=TRUE)}
#####################
#- without projection
#- computing angles
dist <- axis.angle(ptcloud_vox,axis="X",projected=FALSE)
ptcloud_vox[,4] <- dist
#- density plot
plot(density(dist,na.rm=TRUE))

#- visualisation
z <- c(sort(unique(round(ptcloud_vox[,4],digits=0)),decreasing=TRUE))

col <- rainbow(n=length(z),start=0,end=2/6)

open3d()

for(i in 1:length(z)){
a <- subset(ptcloud_vox,round(ptcloud_vox[,4],digits=0)==z[i])
plot3d(a,col=col[i],add=TRUE)
}

proj <- project(cloud_vox,dim="xy")
nvox <- c(proj[,3])
lev_vox <- level(nvox,by="quantiles",levels=c(0.2))
surf_nvox <- surface(proj,method="nvox",levels=lev_vox,res=0.02,proportion=TRUE)
par(mfrow=c(1,1),mai=c(1,0,1,0),omi=c(0,1,0,0))
raster.proj(proj,title="nvox",res=0.02,method="nvox",levels=lev_vox,
colors=c("lightblue","green","yellow","red","purple"),
contour=TRUE,surf=surf_nvox,dim="xy")
