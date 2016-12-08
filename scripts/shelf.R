library(data.table)
library(rgl)
setwd("/Users/rjcmarkelz1/git.repos/3D_reconstruction/data/shelf_light_calibration")

?fread

light1 <- fread("shelf_2_7_1.IRR")
light2 <- fread("shelf_2_7_2.IRR")
light3 <- fread("shelf_2_7_3.IRR")
light4 <- fread("shelf_2_7_4.IRR")
light5 <- fread("shelf_2_7_5.IRR")

head(light)
tail(light)
plot(light1)
plot(light2)
plot(light3)
plot(light4)
plot(light5)

head(iris)
with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, type="s", col=as.numeric(Species)))