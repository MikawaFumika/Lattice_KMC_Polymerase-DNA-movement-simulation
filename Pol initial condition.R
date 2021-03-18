#simulation for 

#difine matrix and initial condition
size=30
kbt=1
kp=1
polprob=0.1
totalpol=0
indexpoli=c()
indexpolj=c()
indexpol=0



lattice_1=matrix(nrow=size, ncol=size)
lattice_2=matrix(nrow=size, ncol=size)
heatmaplattice=matrix(nrow=size, ncol=size)
for(i in 1:size)
{
  for(j in 1:size)
  {
    heatmaplattice[i,j]=0
  }
}

coefficientmatrix=matrix(0,nrow=5,ncol=5)
coefficientmatrix[1,1]=-0.2
coefficientmatrix[2,4]=-0.5
coefficientmatrix[4,2]=-0.5
coefficientmatrix[3,4]=0.5
coefficientmatrix[4,3]=0.5
coefficientmatrix[4,4]=-0.25
coefficientmatrix[4,5]=0
coefficientmatrix[5,4]=0


for(i in 1:size)
{
    for(j in 1:size)
    {
      if(runif(1)<polprob)
      {
        lattice_1[i,j]=4
        indexpol=indexpol+1
        indexpoli=c(indexpoli,i)
        indexpolj=c(indexpolj,j)
        totalpol=totalpol+1
      }
      else
        lattice_1[i,j]=5
    }
}
lattice_2=lattice_1


par(mar=c(1,1,1,1))
plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
for(i in 1:size)
{
  for(j in 1:size)
  {
    par(new=T)
    if(lattice_1[i,j]==4)
      polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'red')
    if(lattice_1[i,j]==5)
      polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'white')
  }
}

for(i in 1:size)
{
  for(j in 1:size)
  {
    par(new=T)
    if(lattice_cho1[i,j]==1)
      polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size-i+0.2,size-i+0.2,size-i+1-0.2,size-i+1-0.2), density = NULL, border = F, col = 'black')
    if(lattice_cho1[i,j]==2)
      polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size-i+0.2,size-i+0.2,size-i+1-0.2,size-i+1-0.2), density = NULL, border = F, col = 'blue')
    if(lattice_cho1[i,j]==3)
      polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size-i+0.2,size-i+0.2,size-i+1-0.2,size-i+1-0.2), density = NULL, border = F, col = 'grey')
  }
}

