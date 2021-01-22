#simulation for 

#difine matrix and initial condition
size=50
kbt=1
kp=1

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
coefficientmatrix[1,1]=-0.5
coefficientmatrix[2,4]=-0.5
coefficientmatrix[4,2]=-0.5
coefficientmatrix[3,4]=0.5
coefficientmatrix[4,3]=0.5
coefficientmatrix[4,4]=-0.35
coefficientmatrix[4,5]=0
coefficientmatrix[5,4]=0


for(i in 1:size)
{
    for(j in 1:size)
    {
      if(runif(1,0,1)>0.6)
        lattice_1[i,j]=4
      else
        lattice_1[i,j]=5
    }
}
lattice_2=lattice_1

#movepol and simulation
Sys.time()
successunmber=0
totalsteps=1000000
for(steps in 1:totalsteps)
{
  lattice_i=sample(1:size,size=1)
  lattice_j=sample(1:size,size=1)
  neiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
  neibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
  newmove=sample(1:8,size=1)
  if(neiborj[newmove]>0 & neiborj[newmove]<(size+1) & neibori[newmove]>0 & neibori[newmove]<(size+1))
  {
    if(lattice_1[lattice_i,lattice_j]==4 & lattice_1[neibori[newmove],neiborj[newmove]]==5)
    {
      successunmber=successunmber+1
      
      lattice_2[lattice_i,lattice_j]=5
      lattice_2[neibori[newmove],neiborj[newmove]]=4
      
      newsitei=neibori[newmove]
      newsitej=neiborj[newmove]
      
      oldenergy=0
      for(oldneibor in 1:8)
      {
        if(neiborj[oldneibor]<1 | neiborj[oldneibor]>size | neibori[oldneibor]<1 | neibori[oldneibor]>size)
          oldenergy=oldenergy
        else
          oldenergy=oldenergy+coefficientmatrix[lattice_1[neibori[oldneibor],neiborj[oldneibor]],4]
      }
      
      newenergy=0
      newneibori=c(newsitei-1,newsitei,newsitei+1,newsitei+1,newsitei+1,newsitei,newsitei-1,newsitei-1)
      newneiborj=c(newsitej-1,newsitej-1,newsitej-1,newsitej,newsitej+1,newsitej+1,newsitej+1,newsitej)
      for(newneibor in 1:8)
      {
        if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
          newenergy=newenergy
        else
          newenergy=newenergy+coefficientmatrix[lattice_2[newneibori[newneibor],newneiborj[newneibor]],4]
      }
      
      if(newenergy<oldenergy)
      {
        lattice_1[lattice_i,lattice_j]=5
        lattice_1[neibori[newmove],neiborj[newmove]]=4
      }
      else
      {
        if(runif(1)<0.1*exp(-(newenergy-oldenergy)))
        {lattice_1[lattice_i,lattice_j]=5
        lattice_1[neibori[newmove],neiborj[newmove]]=4}
        else
        {lattice_2[lattice_i,lattice_j]=4
        lattice_2[neibori[newmove],neiborj[newmove]]=5}
      }
      
  
    }
  }
}

sum4=0
for(i in 1:size)
  {for(j in 1:size)
    {if(lattice_1[i,j]==4)
    {sum4=sum4+1}}}
  

{
  par(mar=c(3,4,1,1))
  plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
  for(i in 1:size)
  {
    for(j in 1:size)
    {
      par(new=T)
      if(lattice_1[i,j]==4)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'red')
      if(lattice_1[i,j]==5)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'white')
    }
  }
}
Sys.time()










  #邻居总能量及channal 每个channal都要计算neibor能
  channal=c(0,0,0,0,0,0,0,0)
  for(neiborlattice in 1:8)
  {
    if(neibori[neiborlattice]<1 | neibori[neiborlattice]>size | neiborj[neiborlattice]<1 | neiborj[neiborlattice]>size)
      {channal[neiborlattice]=0}
    else
    {
        newneiborj=c(neiborj[neiborlattice]-1,neiborj[neiborlattice]-1,neiborj[neiborlattice]-1,neiborj[neiborlattice],neiborj[neiborlattice]+1,neiborj[neiborlattice]+1,neiborj[neiborlattice]+1,neiborj[neiborlattice])
        newneibori=c(neibori[neiborlattice]-1,neibori[neiborlattice],neibori[neiborlattice]+1,neibori[neiborlattice]+1,neibori[neiborlattice]+1,neibori[neiborlattice],neibori[neiborlattice]-1,neibori[neiborlattice]-1)
       for(newneiborlattice in 1:8)
       {
         if(newneibori[newneiborlattice]<1 | newneibori[newneiborlattice]>size | newneiborj[newneiborlattice]<1 | newneiborj[newneiborlattice]>size)
           channal[neiborlattice]=channal[neiborlattice]
         else
           channal[neiborlattice]=channal[neiborlattice]+coefficientmatrix[lattice_1[newneibori[newneiborlattice],newneiborj[newneiborlattice]],4]
       }
        if(lattice_1[lattice_i,lattice_j]!=lattice_1[neibori[neiborlattice],neiborj[neiborlattice]])
      channal[neiborlattice]=channal[neiborlattice]+0.35
    }
  }
  
  energyoringal=0
  for(op in 1:8)
  {
    if(neibori[op]<1 | neibori[op]>size | neiborj[op]<1 | neiborj[op]>size)
    {energyoringal=energyoringal}
    else
    energyoringal=energyoringal+coefficientmatrix[lattice_1[neibori[op],neiborj[op]],4]
  }
    
  totalenergy8=channal[1]+channal[2]+channal[3]+channal[4]+channal[5]+channal[6]+channal[7]+channal[8]-8*energyoringal
  totalenergy7=channal[1]+channal[2]+channal[3]+channal[4]+channal[5]+channal[6]+channal[7]-7*energyoringal
  totalenergy6=channal[1]+channal[2]+channal[3]+channal[4]+channal[5]+channal[6]-6*energyoringal
  totalenergy5=channal[1]+channal[2]+channal[3]+channal[4]+channal[5]-5*energyoringal
  totalenergy4=channal[1]+channal[2]+channal[3]+channal[4]-4*energyoringal
  totalenergy3=channal[1]+channal[2]+channal[3]-3*energyoringal
  totalenergy2=channal[1]+channal[2]-2*energyoringal
  totalenergy1=channal[1]-1*energyoringal
  totalenergy=c(totalenergy1,totalenergy2,totalenergy3,totalenergy4,totalenergy5,totalenergy6,totalenergy7,totalenergy8)
  #move
  channaljudge=sample(1:8,size=1)
  if(totalenergy[channaljudge]<0)
    {
    lattice_1[lattice_i,lattice_j]=5
    lattice_1[neibori[channaljudge],neiborj[channaljudge]]=4
  }
  else
  {
    if(runif(1)<exp(-totalenergy[channaljudge]))
    {
      lattice_1[lattice_i,lattice_j]=5
      lattice_1[neibori[channaljudge],neiborj[channaljudge]]=4
    }
    
  }
    

  }
  
}

Sys.time()



#vitural


{
par(mar=c(3,4,1,1))
plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
for(i in 1:size)
{
  for(j in 1:size)
  {
par(new=T)
    if(lattice_1[i,j]==4)
      polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'red')
    if(lattice_1[i,j]==5)
      polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'white')
  }
}
}


Sys.time()

