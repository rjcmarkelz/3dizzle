#brassica canopy modeling
library(YplantQMC)

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")
testplant <- constructplant("PlantFiletest2.p", "LeafFiletest.l")
plot(testplant)
summary(testplant)
str(testplant)

test2 <- constructplant("brassica_test5.p", "LeafFiletest.l")
plot(test2)
summary(test2)
str(test2)
test2$phy
