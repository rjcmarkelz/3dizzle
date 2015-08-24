library(YplantQMC)
# get leaf file
# make brassica plant
setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")

brassica <- constructplant("brassica_test5.p", "LeafFiletest.l")

br_stand <- makeStand(list(brassica, brassica, brassica),
                       xyz=data.frame(x=c(0,20,5),
                                      y=c(0,0,5),
                                      z=c(0,0,0)))
brassicastand <- makeStand(list(brassica,brassica,brassica),
                       xyz=data.frame(x=c(0,20,5),
                                      y=c(0,0,5),
                                      z=c(0,0,0)))

brassicastand <- makeStand(list(brassica,brassica,brassica,brassica,brassica),
                       xyz=data.frame(x=c(-50,50,0,-50,50),
                                      y=c(-50,50,0,50,-50),
                                      z=c(0,0,0,0,0)))

plot(brassicastand)

leaffile <- as.data.frame(read.table("brassica_leaf.txt", sep = "\t", header = TRUE))
test <- leaffile
test <- test*25.4
test$X <- test$X - test[1,1]
test$Y <- test$Y - test[1,2]
test 
test <- as.data.frame(t(test))
plot(test)
test
C <- matrix(c(cos(0.6722195),asin(0.6722195),sin(0.6722195),cos(0.6722195)), nrow = 2, ncol = 2)
C

B%*%test[,1]
B%*%test[,2]

newangle <- atan2(91.1098, 114.4778 )

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


