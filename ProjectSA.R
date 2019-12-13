
#using maptools

library(maptools)
library(spatstat)
library(sp)
library(splancs)
setwd("C:\\Master\\Spatial_Analysis\\Project\\shapfiles")
points <- readShapeSpatial("points.shp")
plot(points, col="blue", pch=16)
points
setwd(system.file("shapes", package="maptools"))
polyg <- readShapeSpatial("area.shp")
win<-as(polyg, "owin")  
plot(polyg, main="UK  Calderdale")

#
proj4string(polyg)=CRS("+init=epsg:2078")
proj4string(points)=CRS("+init=epsg:2078")

plot(polyg, main="UK Calderdale ")
plot(points, add=TRUE, col='blue', pch=16)
summary(points)

pp <- as.ppp(x)
#density
w<-as.owin(polyg)
wpp<-pp[w]
d<-density(wpp)
plot(d)
plot(points, main="Density", pch="*", add=TRUE)
#perspective density plots
persp(d)
contour(d, axes = FALSE)
adapd <- adaptive.density(wpp, f = 0.01, nrep = 10)
plot(adapd, main = "Adaptive intensity")
plot(wpp, add = TRUE, pch="*", cex = 0.5)

#Quadrat count
Q<-quadratcount(points, nx = 6, ny = 3)
plot(x, add=TRUE, col='blue', pch=16)
plot(Q, add=TRUE, cex=2)
#
#Quadrat Test ??2 test based on quadrat counts
q<-quadrat.test(pp)
plot(q, col='blue',main="??2 Test", cex=1,lty=1, lwd=3)
q
#
#
ktest<-Kest(pp)
ktest
plot(ktest)
plot(envelope(pp, Kest))
#kinhom of all data
kinh<-Kinhom(pp)
kinh
plot(kinh)
env<-envelope(pp, Kinhom)
plot(env)
