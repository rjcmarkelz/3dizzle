library(rgl)

triangles3d(cbind(x=rnorm(9), y=rnorm(9), z=rnorm(9)), col = "green")
decorate3d()
bg3d("lightgray")
aspect3d(1,1,1)

with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                  type="s", col=as.numeric(Species)))


library(MASS)
# from the fitdistr example
set.seed(123)
x <- rgamma(100, shape = 5, rate = 0.1)
fit <- fitdistr(x, dgamma, list(shape = 1, rate = 0.1), lower = 0.001)
loglik <- function(shape, rate) sum(dgamma(x, shape=shape, rate=rate, 
                                           log=TRUE))
loglik <- Vectorize(loglik)
xlim <- fit$estimate[1]+4*fit$sd[1]*c(-1,1)
ylim <- fit$estimate[2]+4*fit$sd[2]*c(-1,1)

mfrow3d(1, 2, sharedMouse = TRUE)
persp3d(loglik, 
        xlim = xlim, ylim = ylim,
        n = 30)
zlim <- fit$loglik + c(-qchisq(0.99, 2)/2, 0)
next3d()
persp3d(loglik, 
        xlim = xlim, ylim = ylim, zlim = zlim,
        n = 30)


open3d()
cols <- rainbow(7)
?layout3d
layout3d(matrix(1:16, 4,4), heights=c(1,3,1,3))
text3d(0,0,0,"tetrahedron3d"); next3d()
shade3d(tetrahedron3d(col=cols[1])); next3d()

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/brassica_test")
brassica <- read.table("brassica_meshlab_test_clean_nopot.xyz")
brassica2 <- readSTL("brassica_meshlab_test_clean_2.stl", ascii = FALSE)
head(brassica)
head(brassica2)
str(brassica)
plot3d(brassica)
select3d(brassica)


plot3d(head(brassica, 2000))
brassica <- head(brassica, 2000)

identify3d(brassica)
brassica <- as.matrix(brassica)
str(brassica)
head(brassica)
identityMatrix(brassica)

brassica_scale <- scaleMatrix(brassica$V1,brassica$V2,brassica$V3)
head(brassica_scale, 10)
brassica_hom <- asHomogeneous(brassica)


head(brassica_hom)

?rotate3d
out <- rotate3d(brassica2, pi/4, 0, 1, 0)
plot3d(out)
rotationMatrix(pi/2, 1, 0, 0)
x <- asHomogeneous(c(2, 0, 0))
y  <- x
asEuclidean(y)
rotate3d(c(2, 0, 0), pi/4, 0, 1, 0)

head(brassica)
?scale
brass_mean <- scale(brassica, center = TRUE)
plot3d(brass_mean)
plot3d(brassica)
brass_svd <- svd(brassica)
str(brassica)
brassica2 <- as.matrix(brassica)
str(brass_svd)
brass_rotate <- brassica2 %*% brass_svd$v
plot3d(brass_rotate)



toonastand <- makeStand(list(toona,toona,toona),
                       xyz=data.frame(x=c(0,20,5),
                                      y=c(0,0,5),
                                      z=c(0,0,0)))
summary(toonastand)
plot(toonastand)
maplerun <- YplantDay(toonastand, met = sunnyday, phy=clidlrc, hemi=largegap)

?runYplant
run_dir <- runYplant(toonastand, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
head(run_dir)
plot
str(run_dir)
tail(run_dir)
run_diff <- runYplant(pilularis, fbeam=0, reflec=0.15, transmit=0.1)
str(run_diff)
plot(density(run_dir$PARleaf, from=0, to=1), xlim=c(0,1), main="", lwd=2, col="blue",
	xlab="Absorbed PAR (relative units)")
lines(density(run_diff$PARleaf, from=0, to=1), lwd=2, col="red")
legend("topright",c("Diffuse","Direct"), lwd=2, col=c("red","blue"))




