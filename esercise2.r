life<-source("D:\\mg\\SIW004-M-LyS\\Homework2\\chap4lifeexp.dat")$value

attach(life)
country<-row.names(life)
life.men<- life[,1:4]
life.women<- life[,5:8]
life.diff<- life.men - life.women
#
life.diff
#
colnames(life.diff)<- c("d0", "d25", "d50", "d75")
life.diff
#

#Aglomerative Hierarchical Clustering
#par(mfrow=c(1,3))
plclust(hclust(dist(life.diff),method="single"),labels=country,ylab="Distance")
title("(a) Single linkage")
plclust(hclust(dist(life.diff),method="average"),labels=country,ylab="Distance")
title("(c) Average linkage")
dev.off()
#
plclust(hclust(dist(life.diff),method="complete"),labels=country,ylab="Distance")
title("(b) Complete linkage")
#
#Nacho, in h=4, we have seven clusters!! + Trinidad(62)
seven<-cutree(hclust(dist(life.diff),method="complete"),h=4)
country.clus<-lapply(1:8,function(nc)country[seven==nc])
country.mean<-lapply(1:8,function(nc)apply(life.diff[seven==nc,],2,mean))
country.mean
country.clus
#

#k-mean clustering
rge<-apply(life.diff,2,max)-apply(life.diff,2,min) 
life.diff.range<-sweep(life.diff,2,rge,FUN="/") 
life.diff.range
#
n<-length(life.diff.range[,1])
wss1<-(n-1)*sum(apply(life.diff.range,2,var)) 
wss<-numeric(0)
for(i in 2:11) { 
	   W<-sum(kmeans(life.diff.range,i)$withinss)
	   wss<-c(wss,W)
}
wss<-c(wss1,wss) 
plot(1:11,wss,type="l",xlab="Number of groups",ylab="Within groups sum of squares",lwd=2)
#
#---NOOOOOOO According to the graphic the optimal number of cluster is five (5), when h = 5
#----SIIIIII According to the graphic the optimal number of cluster is four (4), when h = 6

life.diff.range.kmean<-kmeans(life.diff.range,5)
life.diff.range.kmean #In this variable the means are for standarized data

#Next line, will calculate mean for original data (difference)
lapply(1:5,function(nc) apply(life.diff[life.diff.range.kmean$cluster==nc,],2,mean))
dev.off()

#The original data (difference) and cluster info in one data frame
#but using clustering k-means
life.diff.final <- data.frame(life.diff, Typekmean = life.diff.range.kmean$cluster)
life.diff.final

#With the optimal number, we can do now Alomerative Hierarchical Clustering
#---------NOOO ---Taking into account that to get five clusters, the distance need to be 5
#---------NOOO ---Nacho, in h=4, we have seven clusters!!
#SIII ---Taking into account that to get four clusters, the distance need to be 6


plclust(hclust(dist(life.diff),method="complete"),labels=country,ylab="Distance")
title("(b) Complete linkage")

#five<-cutree(hclust(dist(life.diff),method="complete"),h=5)
#five # Look inside this variable. We have from 1 to 6, because Trinidad (62) is alone in 6 (no cluster there)
#country.clus<-lapply(1:6,function(nc)country[five==nc])
#country.clus #Look inside, pay attention that Seychelles and Tunisia are not alone
#country.mean<-lapply(1:6,function(nc)apply(life.diff[five==nc,],2,mean))
#country.mean
#life.diff.final.2methods <- data.frame(life.diff.final, TypeHierar = five)
#life.diff.final.2methods

four<-cutree(hclust(dist(life.diff),method="complete"),h=6)
four
#four # Look inside this variable. We have from 1 to 5, because Trinidad (62) is alone in 5 (no cluster there)
#Trinidad (62) is so different to the others
country.clus<-lapply(1:5,function(nc)country[four==nc])
country.clus #Look inside, pay attention that Seychelles and Tunisia are not alone
country.mean<-lapply(1:5,function(nc)apply(life.diff[four==nc,],2,mean))
country.mean

life.diff.final.2methods <- data.frame(life.diff.final, TypeHierar = four)
life.diff.final.2methods

####
#part 2

library(MASS)

dis<-lda(TypeHierar~d0+d25+d50+d75, data=life.diff.final.2methods, prior=c(0.20,0.20,0.20,0.20,0.20))
dis

plot(dis)
#
#
newlife.men<- rbind(c(65,50,33,15), c(59,46,31,15))
newlife.women<- rbind(c(69,57,37,16), c(64,56,33,16))
newlife.diff<- newlife.men - newlife.women
newlife.diff

colnames(newlife.diff)<-colnames(life.diff)
newlife.diff
#
newdata<-data.frame(newlife.diff)
predict(dis,newdata=newdata)
#
#
group<-predict(dis,method="plug-in")$class
group
life.diff.final.2methods$TypeHierar
#
table(group,life.diff.final.2methods$TypeHierar)