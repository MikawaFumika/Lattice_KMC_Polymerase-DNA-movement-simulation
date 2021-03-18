#混合高斯分布
library(mvtnorm)

heatmap=heatmaplattice*20

heatmaparray=c()


for(i in 1:50)
{
  for(j in 1:50)
  {
   
    if(heatmap[i,j]>0)
    {
      count=0
      while(count<heatmap[i,j])
      {count=count+1
        heatmaparray=rbind(heatmaparray,c(j,size-i))
      }
    }
   
      
  }
}


data = matrix(nrow=20, ncol=2)
for(j in 1:10)
  data[j,1]=rnorm(1, mean = 0, sd = 0.5)
for(j in 11:20)
  data[j,1]=rnorm(1, mean = 2, sd = 0.5)
for(j in 1:10)
  data[j,2]=rnorm(1, mean = 0, sd = 0.5)
for(j in 11:20)
  data[j,2]=rnorm(1, mean = 2, sd = 0.5)


plot(data[,1],data[,2],xlim=c(min(data[,1]),max(data[,1])),ylim=c(min(data[,2]),max(data[,2])))
par(new=T)
plot(as.vector(list$u[,1]),as.vector(list$u[,2]),col='red',xlim=c(min(data[,1]),max(data[,1])),ylim=c(min(data[,2]),max(data[,2])))


data=heatmaparray


k=10
a = rep(1/k,k)
xu=seq(1,k,1)
yu=seq(1,k,1)
for(xu1 in 1:k)
  xu[xu1]=choj[xu1,45]
for(yu1 in 1:k)
  yu[yu1]=size-choi[yu1,45]
  
u = rbind(xu,yu)
u=t(u)
cov0 <- matrix(c(1,0,0,1),ncol = 2)
list=mulGaussCluster(data,k,a,u,cov0)
cluster = list$cluster

mulGaussCluster(data,k,a,u,cov0)

library(mvtnorm)
mulGaussCluster <- function(data,k,a,u,cov0){
  #data:数据集,向量或数据框
  #k:聚类的个数，或高斯分布的个数
  #a:高斯分布的先验概率,选择各个高斯分布的概率,向量
  #u:高斯分布的初始均值
  #cov0:多元高斯分布的初始协方差阵
  N = nrow(data)
  covList = list()
  for(i in 1:k){
    covList[[i]] <- cov0
  }

for(count in 1:20)
    {
    u0 <- u
    p = c()
    for(j in 1:k){
      prob = apply(data,1,dmvnorm,mean=u[j,],sigma = covList[[j]])
      p = cbind(p,prob)
    }
    amatrix <- matrix(rep(a,N),nrow=N,byrow = T)
    r = (p * amatrix) / rowSums(p * amatrix)  
    u <- t(r) %*% data / colSums(r) #求和可以转化为向量相乘的形式，简化计算
    for(j in 1:k){
      sigma = matrix(rep(0,4),ncol=2)
      for(i in 1:N){
        sigma = sigma + r[i,j] * (data[i,]-u0[j,]) %*% t(data[i,]-u0[j,])  #R中向量默认为列向量
      }
      covList[[j]]= sigma/sum(r[,j]) 
    }
    a <- colSums(r)/N
    print(count)
    
  }
  cluster <- which(r == apply(r,1,max),arr.ind = T)
  cluster <- cluster[order(cluster[,1]),]
  
  
  return(list(u = u,covList = covList,a = a,cluster = cluster))
}





dev.off()
a=1/max(heatmaplattice)*220/255
par(mar=c(1,1,1,1))
plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
for(i in 1:size)
{
  for(j in 1:size)
  {
    par(new=T)
    polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = rgb(heatmaplattice[i,j]*a,heatmaplattice[i,j]*a,heatmaplattice[i,j]*a))
    
  }
}

par(new=T)
plot(as.vector(u[,1]),as.vector(u[,2]),type="p",tck=0.03,cex=0.5,las=1,xlab="",col='red',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")



n <- 1000
mean_s <- c(1, 7)
y <- sample(c("head", "tail"), size = n, replace = TRUE, prob = c(0.25, 0.75))
x <- rnorm(n = 1000, mean = mean_s[1])
tails <- y %in% c("tail")
x[tails] <- rnorm(sum(tails), mean = mean_s[2])

library(em)
require(mixtools)

em <- normalmixEM(x, mu = c(0, 1), sigma = c(1, 1), sd.constr = c(1, 1))

## number of iterations= 6

print(em$lambda)

plot(em, whichplots = 2)

install.packages("mclust");

require(mclust)

data(diabetes)

summary(diabetes)

install.packages("MASS")

library(MASS)

install.packages("EMCluster")

library(EMCluster)

x <- synth.te[,-3]

#高斯模糊
install.packages('spatstat')
library(spatstat)
blur(spatstat)

Z <- as.im(function(x,y) { 4 * x^2 + 3 * y }, letterR)
par(mfrow=c(1,3))
plot(Z)
plot(letterR, add=TRUE)
plot(blur(Z, 1, bleed=TRUE))
plot(letterR, add=TRUE)
plot(blur(Z, 0.5, bleed=FALSE))
plot(letterR, add=TRUE)



#code
Z=as.im(-(lattice_1-5))
Z1=blur(Z, 1, bleed=TRUE)
Z1=Z1$v

a=250/255
par(mar=c(1,1,1,1))
plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
for(i in 1:size)
{
  for(j in 1:size)
  {
    par(new=T)
    polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = rgb(Z1[i,j]*a*225/255,Z1[i,j]*a*152/255,Z1[i,j]*a*192/255))
  }
}






data(demopat)
# window object[窗口对象]
W <- demopat$window
plot(W)
Z <- as.im(W)
image(Z)
# function[功能]
Z <- as.im(function(x,y) {x^2 + y^2}, unit.square())
image(Z)
# function with extra arguments[功能与额外的参数]
f <- function(x, y, x0, y0) {
  sqrt((x - x0)^2 + (y-y0)^2)
}
Z <- as.im(f, unit.square(), x0=0.5, y0=0.5)
image(Z)
# Revisit the Sixties[重温上世纪六十年代]
data(letterR)
Z <- as.im(f, letterR, x0=2.5, y0=2)
image(Z)
# usual convention in S[通常的惯例在S]
stuff <- list(x=1:10, y=1:10, z=matrix(1:100, nrow=10))
Z <- as.im(stuff)
# convert to finer grid[转换到更精细的网格]
Z <- as.im(Z, dimyx=256)

# pixellate the Dirichlet tessellation[像素化的Dirichlet Tessellation（曲面细分）]
Di <- dirichlet(runifpoint(10))
plot(as.im(Di))
plot(Di, add=TRUE)

