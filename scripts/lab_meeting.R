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

