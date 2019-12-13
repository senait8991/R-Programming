#install.packages("XLConnect")
library("XLConnect")
excel.file <- file.path("D:\\mg\\SIW004-M-LyS\\Homework2\\1.foodstuffs.xlsx")
foodstuffs_ds<- readWorksheetFromFile(excel.file, sheet=1)
attach(foodstuffs_ds)
row.names(foodstuffs_ds) <- value
foodstuffs_ds
foodstuffs1<-cbind(Energy,Protein,Fat, Calcium, Iron)
foodstuffs1
pairs(foodstuffs1)
epf<-cbind(Energy,Protein,Fat)
fci<-cbind(Fat,Calcium,Iron)
ei<-cbind(Energy,Iron)
pi<-cbind(Protein,Iron)
ec<-cbind(Energy,Calcium)
pc<-cbind(Protein,Calcium)
names = substr(Foodstuffs, 1, 2)
duplicated(names)
row.names(foodstuffs_ds) <- names
pairs(foodstuffs1,panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})
pairs(jitter(foodstuffs1),panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})
pairs(jitter(epf),panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})
pairs(jitter(fci),panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})
pairs(jitter(ei),panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})
pairs(jitter(pi),panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})
pairs(jitter(ec),panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})
pairs(jitter(pc),panel=function(x,y) {
  abline(lsfit(x,y)$coef, lty=2, lwd=1, col="darkorange")
  lines(lowess(x,y),lty=2,lwd=1, col="deepskyblue4")
  points(x,y, col="darkolivegreen", pch=1, lwd=2)
  text(x,y,labels=names,lwd=2, pos=2) # show text in plot
})

cor(foodstuffs_ds[,-1])
foodstuffs.pc<-princomp(foodstuffs_ds[,-1],cor=TRUE)
summary(foodstuffs.pc,loadings=TRUE) #Review Cumulative Proportion
foodstuffs.pc$scores[,1:3]
par(mfrow=c(1,3))
plot(foodstuffs.pc$scores[,1],Fat,xlab="PC1")
plot(foodstuffs.pc$scores[,2],Fat,xlab="PC2")
plot(foodstuffs.pc$scores[,3],Fat,xlab="PC3")

plot(foodstuffs.pc$scores[,1],
     foodstuffs.pc$scores[,2],
     ylim=range(foodstuffs.pc$scores[,1]),
     xlab="PC1",ylab="PC2",type="n",lwd=2)
text(foodstuffs.pc$scores[,1],
     foodstuffs.pc$scores[,2],
     labels=row.names(foodstuffs_ds),
     cex=0.7,lwd=2)

plot(foodstuffs.pc$scores[,1],
     foodstuffs.pc$scores[,3],
     ylim=range(foodstuffs.pc$scores[,1]),
     xlab="PC1",ylab="PC3",type="n",lwd=2)
text(foodstuffs.pc$scores[,1],
     foodstuffs.pc$scores[,3],
     labels=row.names(foodstuffs_ds),
     cex=0.7,lwd=2)

plot(foodstuffs.pc$scores[,2],
     foodstuffs.pc$scores[,3],
     ylim=range(foodstuffs.pc$scores[,2]),
     xlab="PC2",ylab="PC3",type="n",lwd=2)
text(foodstuffs.pc$scores[,2],
     foodstuffs.pc$scores[,3],
     labels=abbreviate(row.names(foodstuffs_ds)),
     cex=0.7,lwd=2)