library(spatstat)
data(bei) 

quadratcount(bei, nx = 4, ny = 2) 

Q =quadratcount(bei, nx = 6, ny = 3) 
 plot(bei, cex = 0.5, pch = "+") #to show two plot in one page use ,Add=T
 plot(Q, add = TRUE, cex = 2) 

den = density(bei, sigma = 70) 
 plot(den)
 plot(bei, add = TRUE, cex = 0.5) 

persp(den)
contour(den, axes=FALSE)

aden = adaptive.density(bei, f = 0.01, nrep = 10) 
 
plot(aden, main = "Adaptive intensity")

 plot(bei, add = TRUE, cex = 0.5) 
#
library(spatstat)

Inspecting data

data(swedishpines) 
X <- swedishpines 
plot(X)
X
summary(X)
plot(density(X,10))
contour(density(X, 10), axes = FALSE) 

Q <- quadratcount(X, nx = 4, ny = 3) 
Q 
plot(X); plot(Q,add=T,cex=2)

K=Kest(X);plot(K)
#
plot(rpoispp(100))
data(letterR)
plot(rpoispp(100, win = letterR))
runifpoint(100, win = letterR) # fixed number of points
 #

data(bei)
Z <- bei.extra$grad
b <- quantile(Z, probs = (0:4)/4)
Zcut <- cut(Z, breaks = b, labels = 1:4)
V <- tess(image = Zcut)
plot(V)
plot(bei, add = TRUE, pch = "+")