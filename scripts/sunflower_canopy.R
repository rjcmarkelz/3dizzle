#brassica canopy modeling
library(YplantQMC)

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")
testplant <- constructplant("PlantFiletest2.p", "LeafFiletest.l")
plot(testplant)
summary(testplant)
str(testplant)

west <- constructplant("sunflower_west.p", "LeafFiletest.l")
plot(west)
summary(west)
str(west)

davis <- setLocation(lat = 38.5539, long = -121.7381)
plot(davis)

?setMet
sunnyday <- setMet(davis, month = 7, day = 12, nsteps = 24, Tmin = 12, Tmax = 25, PARday = 24)
# plot(sunnyday)

?setPhy
west$phy <- setPhy("lightresponse",leafpars=list(Amax=25, Rd=0.7, phi=0.045, theta=0.95, reflec=0.1, transmit=0.5))
west_run <- YplantDay(west, met = sunnyday, phy = west$phy)

psrdata(west_run)
plot(west_run)

######
east <- constructplant("sunflower_east.p", "LeafFiletest.l")
plot(east)
summary(east)
str(east)

davis <- setLocation(lat = 38.5539, long = -121.7381)
plot(davis)

?setMet
sunnyday <- setMet(davis, month = 7, day = 12, nsteps = 24, Tmin = 12, Tmax = 25, PARday = 20)
# plot(sunnyday)

?setPhy
east$phy <- setPhy("lightresponse",leafpars=list(Amax=25, Rd=0.7, phi=0.045, theta=0.95, reflec=0.1, transmit=0.5))
east_run <- YplantDay(east, met = sunnyday, phy = east$phy)

psrdata(east_run)
plot(east_run)


#####
overhead <- constructplant("sunflower_overhead.p", "LeafFiletest.l")
plot(overhead)
summary(overhead)
str(overhead)

davis <- setLocation(lat = 38.5539, long = -121.7381)
plot(davis)

?setMet
sunnyday <- setMet(davis, month = 7, day = 12, nsteps = 24, Tmin = 12, Tmax = 25, PARday = 20)
# plot(sunnyday)

?setPhy
overhead$phy <- setPhy("lightresponse",leafpars=list(Amax=25, Rd=0.7, phi=0.045, theta=0.95, reflec=0.1, transmit=0.5))
overhead_run <- YplantDay(overhead, met = sunnyday, phy = overhead$phy)

psrdata(overhead_run)

#######
plot(east_run)
plot(overhead_run)
plot(west_run)

psrdata(east_run)
psrdata(overhead_run)
psrdata(west_run)


