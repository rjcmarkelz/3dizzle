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

laramie <- setLocation(lat = 41.3167, long = -105.5833)
plot(laramie)

sunnyday <- setMet(laramie, month = 7, day = 12, nsteps = 12, Tmin = 12, Tmax = 28, PARday = 24)
plot(sunnyday)

test2$phy <- setPhy("lightresponse",leafpars=list(Amax=25, Rd=0.7, phi=0.045, theta=0.95, reflec=0.1, transmit=0.05))
testrun <- YplantDay(test2, met = sunnyday, phy = test2$phy)

psrdata(testrun)
plot(testrun)


