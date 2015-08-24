library(YplantQMC)
# get leaf file
# make brassica plant
setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")

brassica <- constructplant("brassica_test6.p", "brassica_leaf_test.l")

br_stand <- makeStand(list(brassica, brassica, brassica),
                       xyz=data.frame(x=c(0,20,5),
                                      y=c(0,0,5),
                                      z=c(0,0,0)))
brassicastand <- makeStand(list(brassica,brassica,brassica),
                       xyz=data.frame(x=c(0,20,5),
                                      y=c(0,0,5),
                                      z=c(0,0,0)))

brassicastand <- makeStand(list(brassica,brassica,brassica,brassica,brassica),
                       xyz=data.frame(x=c(-20,20,0,-20,20),
                                      y=c(-20,20,0,20,-20),
                                      z=c(0,0,0,0,0)))

plot(brassicastand)

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