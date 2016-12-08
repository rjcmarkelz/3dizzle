library(data.table)
setwd("/Users/rjcmarkelz1/git.repos/3D_reconstruction/data/shelf_light_calibration")

?fread

light <- fread("shelf_1_14_1.IRR")
head(light)
tail(light)
plot(light)