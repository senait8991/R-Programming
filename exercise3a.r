#setwd("D:/mg/SIW004-M-LyS/Homework2")
#life <- source("D://mg//SIW004-M-LyS//Homework2//chap4lifeexp.dat")$value
#require(xlsx)
#foodstuffs <- read.xlsx("1.foodstuffs.xlsx", sheetName = "foodstuffs")
Tibet <- source("D://mg//SIW004-M-LyS//Homework2//chap7tibetskull.dat")$value
attach(Tibet)
cor(Tibet[,2:5])
pairs(Tibet[,2:5])

Tibet.pc<-princomp(Tibet[,2:5],cor=TRUE) # Using Correlation matrix. # Same scale CAN use covariances
summary(Tibet.pc,loadings=TRUE)
Tibet.pc$scores[,1:2]
Tibet.pc$scores
plot(Tibet.pc$scores[,1],Length,xlab="PC1")
plot(Tibet.pc$scores[,2],Length,xlab="PC2")
dev.off()
#plot(Tibet.pc$scores[,3],Length,xlab="PC3")

#

#
par(pty="s")
plot(Tibet.pc$scores[,1],Tibet.pc$scores[,2],
     ylim=range(Tibet.pc$scores[,1]),
     xlab="PC1",ylab="PC2",type="n",lwd=2)
text(Tibet.pc$scores[,1],Tibet.pc$scores[,2],
     labels=abbreviate(row.names(Tibet)),cex=0.7,lwd=2)
#
#
# par(pty="s")
# plot(Tibet.pc$scores[,1],Tibet.pc$scores[,3],
#      ylim=range(Tibet.pc$scores[,1]),
#      xlab="PC1",ylab="PC3",type="n",lwd=2)
# text(Tibet.pc$scores[,1],Tibet.pc$scores[,3],
#      labels=abbreviate(row.names(Tibet)),cex=0.7,lwd=2)
#
#
# par(pty="s")
# plot(Tibet.pc$scores[,2],Tibet.pc$scores[,3],
#      ylim=range(Tibet.pc$scores[,2]),
#      xlab="PC2",ylab="PC3",type="n",lwd=2)
# text(Tibet.pc$scores[,2],Tibet.pc$scores[,3],
#      labels=abbreviate(row.names(Tibet)),cex=0.7,lwd=2)
#
#
lm(Length~Tibet.pc$scores[,1]+Tibet.pc$scores[,2]+ Tibet.pc$scores[,3])
summary(lm(Length~Tibet.pc$scores[,1]+Tibet.pc$scores[,2]+
             Tibet.pc$scores[,3])) # information of the significance
#
plot(Tibet.pc$scores[,1],Length,xlab="PC1",ylab="Length")
#
load(lqs)
#
Tibet.mve<-cov.mve(Tibet.dat[,-1],cor=TRUE)
#
Tibet.mve$cor
Tibet.pc1<-princomp(Tibet.dat[,-1],covlist=Tibet.mve,cor=TRUE)
summary(Tibet.pc1,loadings=T)
#
#
#Chapter 8
Tibet.fit<-lm(SO2~Neg.Temp+Manuf+Pop+Wind+Precip+Days)
summary(Tibet.fit)

