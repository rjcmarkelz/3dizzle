library(YplantQMC)
# get leaf file
# make brassica plant
setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")

brassica <- constructplant("brassica_test6.p", "brassica_leaf_test.l")
brassica2 <- constructplant("brassica_single.p", "LeafFiletest.l")
brassica3 <- constructplant("brassica_all_leaves.p", "br_sh_hn_leaves.l")

plot(brassica3)
# individual plant
plot(brassica)

plot(brassica2)
plot(brassica3)
laramie <- setLocation(lat = 41.3167, long = -105.5833)
plot(laramie)

sunnyday <- setMet(laramie, month = 7, day = 12, nsteps = 12, Tmin = 12, Tmax = 28, PARday = 24)
plot(sunnyday)
sunnyday

brassica$phy <- setPhy("lightresponse",leafpars=list(Amax=25, Rd=0.7, phi=0.045, theta=0.95, reflec=0.1, transmit=0.05))
testrun <- YplantDay(brassica, met = sunnyday, phy = brassica$phy)

brass_1 <- psrdata(testrun)
brass_1
plot(testrun)
testrun
brassica_fromabove <- projectplant(brassica3, azimuth=0, altitude=90)
str(brassica_fromabove)
plot(brassica_fromabove)

summary(brassica)
plot(brassica, addcrownhull = TRUE)
plot(brassica)

library(reshape2)
library(ggplot2)
brass_melt <- melt(brass_1, id.vars = "timeofday")
brass_melt
ggplot(brass_1, aes(timeofday, A)) + geom_line() + xlab("Hour of Day") + ylab("Assimilation (umol CO2 m-2 s-1)")
ggplot(brass_1, aes(timeofday, LAproj)) + geom_line() + xlab("Hour of Day") + ylab("Leaf Area (m-2)")
ggplot(brass_1, aes(timeofday, LAproj)) + geom_line() + xlab("Hour of Day") + ylab("Leaf Area (m-2)")
head(brass_1)


#PAR Plots
head(brass_1)
brass_melt <- brass_1[,c(1,2,4, 5, 6)]
brass_melt <- melt(brass_melt, id = "timeofday")
head(brass_melt)
p <- ggplot(brass_melt) + 
  geom_line(aes(x = timeofday, y = value, color = variable), size = 3) +
  xlab("Hour of Day") + ylab("PAR umol photons m-2 s-1") +
  scale_colour_manual(values=c("black","green","blue","red"),
  	name ="PAR", labels=c("PAR Total", "PAR Direct", "PAR Leaf", "PAR Diffuse")) +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
p

A <- ggplot(data = brass_1, aes(x = timeofday, y = A)) + 
  geom_line(color = "black", size = 3) +
  xlab("Hour of Day") + ylab("Assimilation umol CO2 m-2 s-1") +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
A
head(brass_1)



#stand
br_stand <- makeStand(list(brassica, brassica, brassica),
                       xyz=data.frame(x=c(0,20,5),
                                      y=c(0,0,5),
                                      z=c(0,0,0)))
brassicastand <- makeStand(list(brassica,brassica,brassica),
                       xyz=data.frame(x=c(0,20,5),
                                      y=c(0,0,5),
                                      z=c(0,0,0)))

brassicastand <- makeStand(list(brassica,brassica,brassica3,brassica,brassica),
                       xyz=data.frame(x=c(-100,100,0,-100,100),
                                      y=c(-100,100,0,100,-100),
                                      z=c(0,0,0,0,0)))

plot(brassicastand)

?runYplant




run_dir <- runYplant(brassicastand, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
run_dir

brassicastand <- makeStand(list(brassica,brassica,brassica,brassica,brassica),
                       xyz=data.frame(x=c(-100,100,0,-100,100),
                                      y=c(-100,100,0,100,-100),
                                      z=c(0,0,0,0,0)))
plot(brassicastand)

brassicastand2 <- makeStand(list(brassica3,brassica3,brassica,brassica3,brassica3),
                       xyz=data.frame(x=c(-200,200,0,-200,200),
                                      y=c(-200,200,0,200,-200),
                                      z=c(0,0,0,0,0)))
plot(brassicastand2)
uncrowded <- runYplant(brassicastand2, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
uncrowded


brassicastand3 <- makeStand(list(brassica,brassica,brassica3,brassica,brassica),
                       xyz=data.frame(x=c(-100,100,0,-100,100),
                                      y=c(-100,100,0,100,-100),
                                      z=c(0,0,0,0,0)))
short <- runYplant(brassicastand3, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
short


shorttrans <- runYplant(brassicastand, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.7, intern = FALSE, rewriteplantfile = TRUE)
shorttrans

brassica4 <- constructplant("brassica_all_leaves_expanded.p", "br_sh_hn_leaves.l")
plot(brassica4)
leafstand <- makeStand(list(brassica,brassica,brassica4,brassica,brassica),
                       xyz=data.frame(x=c(-100,100,0,-100,100),
                                      y=c(-100,100,0,100,-100),
                                      z=c(0,0,0,0,0)))
plot(leafstand)
leafexpansion <- runYplant(leafstand, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
leafexpansion 

brassica5 <- constructplant("brassica_all_leaves_internodes_expanded.p", "br_sh_hn_leaves.l")
leafintstand <- makeStand(list(brassica,brassica,brassica5,brassica,brassica),
                       xyz=data.frame(x=c(-100,100,0,-100,100),
                                      y=c(-100,100,0,100,-100),
                                      z=c(0,0,0,0,0)))
plot(leafintstand)
leafintexpansion <- runYplant(leafintstand, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
leafintexpansion 

brassica6 <- constructplant("brassica_internodes_expanded.p", "br_sh_hn_leaves.l")
intstand <- makeStand(list(brassica,brassica,brassica6,brassica,brassica),
                       xyz=data.frame(x=c(-100,100,0,-100,100),
                                      y=c(-100,100,0,100,-100),
                                      z=c(0,0,0,0,0)))
plot(intstand)
intexpansion <- runYplant(intstand, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
intexpansion 

uncrowded3 <- subset(uncrowded, plantnr ==3)
dim(uncrowded3)
uncrowded3$leaf <- 1:10

run_dir
run_dir3 <- subset(run_dir, plantnr == 3)
run_dir3$leaf <- 1:10

short
short3 <- subset(short, plantnr == 3)
short3$leaf <- 1:10
short

shorttrans
shorttrans3 <- subset(shorttrans, plantnr == 3)
shorttrans3$leaf <- 1:10

leafexpansion
leafexpansion3 <- subset(leafexpansion, plantnr == 3)
leafexpansion3$leaf <- 1:10

leafintexpansion
leafintexpansion3 <- subset(leafintexpansion, plantnr == 3)
leafintexpansion3$leaf <- 1:10

intexpansion
intexpansion3 <- subset(intexpansion, plantnr == 3)
intexpansion3$leaf <- 1:10

merged <- as.data.frame(cbind(uncrowded3[,3],run_dir3[,3],short3[,3],leafexpansion3[,3],leafintexpansion3[,3],intexpansion3[,3]))
names(merged) <- c("Uncrowded", "Same", "Short", "LeafExp", "LeafIntExp","IntExp")
merged$leaf <- 1:10
merged <- melt(merged, id.vars = "leaf")
merged

#proper way
heightPAR <- ggplot(merged) + 
  geom_line(aes(x = leaf, y = value, color = variable), size = 3) +
  scale_colour_manual(values=c("black", "red", "orange","blue","green", "purple"),
  	name ="PAR", labels=c("Uncrowded", "Same Height", "Short", "Leaf Angle", "Angle + Internode", "Internode")) +
  scale_x_discrete("leaf") +  xlab("Leaf Number") + ylab("PAR umol photons m-2 s-1") +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
heightPAR


merged2 <- as.data.frame(cbind(short3[,3],shorttrans3[,3]))
names(merged2) <- c("Trans = 0.1", "Trans = 0.5")
merged2$leaf <- 1:10
merged2 <- melt(merged2, id.vars = "leaf")
merged2

transPAR <- ggplot(merged2) + 
  geom_line(aes(x = leaf, y = value, color = variable), size = 3) +
  scale_colour_manual(values=c("red","black"),
  	name ="PAR", labels=c("Trans = 0.1", "Trans = 0.5")) +
  scale_x_discrete("leaf") +  xlab("Leaf Number") + ylab("PAR umol photons m-2 s-1") +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
transPAR

#hackyway
# heightPAR <- ggplot() + 
#   geom_line(data = run_dir3, aes(x = leaf, y = PARleaf), color = "red", size = 3) +
#   geom_line(data = short3, aes(x = leaf, y = PARleaf), color = "black", size = 3) +
#   geom_line(data = leafexpansion3, aes(x = leaf, y = PARleaf), color = "blue", size = 3) +
#   geom_line(data = leafintexpansion3, aes(x = leaf, y = PARleaf), color = "green", size = 3) +
#   geom_line(data = intexpansion3, aes(x = leaf, y = PARleaf), color = "purple", size = 3) +
#   xlab("Leaf") + ylab("Absorbed PAR (umol photons m-2 s-1)") +
#   theme(axis.title.x = element_text(face="bold", size=20),
#            axis.text.x  = element_text(size=16),
#            axis.title.y = element_text(face="bold", size=20),
#            axis.text.y  = element_text(size=16))
# heightPAR

# heightPAR <- ggplot() + 
#   geom_line(data = run_dir3, aes(x = leaf, y = PARleaf), color = "red", size = 3) +
#   geom_line(data = short3, aes(x = leaf, y = PARleaf), color = "black", size = 3) +
#   geom_line(data = leafexpansion3, aes(x = leaf, y = PARleaf), color = "blue", size = 3) +
#   geom_line(data = leafintexpansion3, aes(x = leaf, y = PARleaf), color = "green", size = 3) +
#   geom_line(data = intexpansion3, aes(x = leaf, y = PARleaf), color = "purple", size = 3) +
#   ylab("Absorbed PAR (umol photons m-2 s-1)") +
#   scale_colour_manual(name = 'the colour', 
#          values =c('red'='red','black'='black', 'blue'='blue', 'green'='green', 'purple'='purple'),
#           labels = c('c2','c1','c3','c4','c5')) +
#   scale_x_discrete("leaf") +
#   theme(axis.title.x = element_text(face="bold", size=20),
#            axis.text.x  = element_text(size=16),
#            axis.title.y = element_text(face="bold", size=20),
#            axis.text.y  = element_text(size=16))
# heightPAR







  geom_line(data = run_dir3, aes(x = leaf, y = PARleaf), color = "black", size = 3) +

#format leaf shapes
leaffile <- as.data.frame(read.table("brassica_sh_hn2.txt", sep = "\t", header = TRUE))
leaffile
plot(leaffile[,1:2])
leaffile <- split(leaffile,leaffile$leaf)
leaf2 <- as.data.frame(leaffile[1])
leaf2 <- as.data.frame(leaffile[2])
leaf4 <- as.data.frame(leaffile[3])
leaf4 <- as.data.frame(leaffile[4])
leaf5 <- as.data.frame(leaffile[5])
leaf6 <- as.data.frame(leaffile[6])
leaf7 <- as.data.frame(leaffile[7])
leaf8 <- as.data.frame(leaffile[8])
leaf9 <- as.data.frame(leaffile[9])
leaf1

leaf9 <- leaf9[,-3]
dim(leaf9)
plot(leaf9)
head(leaf9)
leaf9 <- leaf9*25.4
leaf9$X9.X <- leaf9$X9.X - leaf9[1,1]
leaf9$X9.Y <- leaf9$X9.Y - leaf9[1,2]
leaf9 
plot(leaf9)
C <- matrix(c(1,0,0,-1), nrow = 2, ncol = 2)
test <- as.matrix(t(leaf9))
test
test <- t(C%*%test)
test
plot(test)
write.table(test, "br_sh_hn_leaf9.txt", row.names = FALSE, col.names = TRUE)





# various transformations and rotations of the leaf data in order to get 
leaffile <- as.data.frame(read.table("new_leaf_outline.txt", sep = "\t", header = TRUE))
test <- leaffile
test[,1]
plot(test)
test <- test*25.4
test$X <- test$X - test[1,1]
test$Y <- test$Y - test[1,2]
test 
plot(test)
C <- matrix(c(1,0,0,-1), nrow = 2, ncol = 2)
C

test2 <- as.matrix(t(test))
test2

test3 <- t(C%*%test2)
test3
plot(test3)
test3 <- (test3+1)*25 - 1 - 24
dim(test3)

write.table(test3, "brassica_leaf_test.txt", row.names = FALSE, col.names = TRUE)
test





#######
test <- as.data.frame(t(test))
plot(test)
test
C <- matrix(c(cos(1),asin(1),sin(1),cos(1)), nrow = 2, ncol = 2)
C
pi/2-newangle
pi/2
pi/4
B%*%test[,1]
B%*%test[,2]

newangle <- atan2(91.1098, 114.4778 )
newangle
plot(c(1, 6), c(2, 2), type="n", xlim=c(0, 7), ylim=c(-2, 6))
segments(1, 1, 6, 1)
segments(1, 3, 6, 3)



# coordinate transform: cartesian plane rotation
xyrot<-function(pairs,ang){
    # pairs must be Nx2 matrix w/ x in first column and y in second
    xrot <- pairs[,1]*cos(ang) - pairs[,2]*sin(ang)
    yrot <- pairs[,1]*sin(ang) + pairs[,2]*cos(ang)
    return(invisible(cbind(xrot,yrot)))
}

test2 <- as.matrix(test)
ang <- -(pi/2)
out <- xyrot(test2, ang)
out
plot(test2)
plot(out)
out <- xyrot(out, newangle)
out
plot(out)

out2 <- acos((114.4778-0)/sqrt((91.1098-0)^2 + (114.4778)^2))
out2

C <- matrix(c(cos(0.6722195),asin(0.6722195),sin(0.6722195),cos(0.6722195)), nrow = 2, ncol = 2)
C

out3 <- as.data.frame(t(out))
plot(out)
plot(out3)


out3[,1] <- C%*%out3[,1]
out3[,2] <- C%*%out3[,2]
out3[,3] <- C%*%out3[,3]
out3[,4] <- C%*%out3[,4]
out3[,5] <- C%*%out3[,5]
out3[,6] <- C%*%out3[,6]
out3
out4 <- as.data.frame(t(out3))
out4
plot(out4)

out4 <- apply(C,out3,prod)

x <-  1:10
y <-  1:10

v <-  as.matrix(cbind(x,y))
str(v)
plot(v)
dim(test)
plot(test)
plot(test2)
x_cen <- rotx
y_cen <- roty

center <- repmat(cbind(x_cen,y_cen), 45, 1)
center
theta <- pi/4
theta
theta <- 45*pi/180
theta

rotx <- min(test2[,1]) + (max(test2[,1]) - min(test2[,1]))/2 
roty <- min(test2[,2]) + (max(test2[,2]) - min(test2[,2]))/2
rotx
roty

R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
s <- as.matrix(test2 - center)
plot(s)

so <- s%*%R
so
test3 <- so
plot(test3)


x1 <- 0:ncol(test2)
y1 <- 0:nrow(test2)
z <- matrix(1, nrow=length(x1), ncol=length(y1))
col.mat <- t(apply(matrix(rgb(getValues(x)/255), nrow=nrow(x), byrow=TRUE), 2, rev))


outplot <- persp(x1, y1, z, zlim=c(0,1), theta = 45, phi = 90, scale=FALSE, border=NA, box=FALSE)
outplot
plot(outplot)

# http://stackoverflow.com/questions/1203135/what-is-the-fastest-way-to-find-the-center-of-an-irregularly-shaped-polygon
# get center of shape with simple bounding box
rotx <- min(test2[,1]) + (max(test2[,1]) - min(test2[,1]))/2 
roty <- min(test2[,2]) + (max(test2[,2]) - min(test2[,2]))/2
rotx
roty
plot(test2)
plot(rotx, roty, add = TRUE)
dim(test2)
centerx <- sum(test2[,1])/45
centery <- sum(test2[,2])/45
centerx
centery
theta