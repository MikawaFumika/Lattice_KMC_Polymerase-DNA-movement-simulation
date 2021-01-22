

#difine matrix and initial condition
size=50
kbt=1

lattice_cho1=matrix(nrow=size, ncol=size)
lattice_cho2=matrix(nrow=size, ncol=size)

totalcho=14
totallengthofcho=60

centerarea=15

#record coordinace
choi=matrix(nrow=totalcho, ncol=totallengthofcho)
choj=matrix(nrow=totalcho, ncol=totallengthofcho)

lengthofchoic=40
lengthofchopc=10
lengthofchoac=10
lengthofcho=c(lengthofchoic-1,lengthofchopc,lengthofchoac)


for(i in 1:size)
{
  for(j in 1:size)
  {
    lattice_cho1[i,j]=5
  }
}

for(i in 1:size)
{
  for(j in 1:size)
  {
    lattice_cho2[i,j]=5
  }
}

for(loop in 1:totalcho)
{
  
repeat
{

  #startcho_i=sample(5:(size-5),size=1)
  #startcho_j=sample(5:(size-5),size=1)
  #center area
  startcho_i=sample(centerarea:(size-centerarea),size=1)
  startcho_j=sample(centerarea:(size-centerarea),size=1)
  
  while(lattice_cho1[startcho_i,startcho_j]!=5)
  {
    #startcho_i=sample(1:size,size=1)
    #startcho_j=sample(1:size,size=1)
    #center area
    startcho_i=sample(centerarea:(size-centerarea),size=1)
    startcho_j=sample(centerarea:(size-centerarea),size=1)
  }
  lattice_cho1[startcho_i,startcho_j]=1
  
  choi[loop,1]=startcho_i
  choj[loop,1]=startcho_j
  
  
  
  
  oldchositei=startcho_i
  oldchositej=startcho_j
  
  steplength=1
  
  for(partsofcho in 1:3)
  {
    for(lengthcho1 in 1:(lengthofcho[partsofcho]))
    {
      newchositei=c(oldchositei,oldchositei+1,oldchositei,oldchositei-1)
      newchositej=c(oldchositej-1,oldchositej,oldchositej+1,oldchositej)
      newchomove=sample(1:4,size=1)
      
      for(ooo in 1:1000)
      {
        if(newchositej[newchomove]<1 | newchositej[newchomove]>size | newchositei[newchomove]<1 | newchositei[newchomove]>size)
        {
          newchomove=sample(1:4,size=1)
        }
        else
        {
          if(lattice_cho1[newchositei[newchomove],newchositej[newchomove]]!=5)
            newchomove=sample(1:4,size=1)
          else
            break
        }
      }
      
      if(ooo<1000)
      {
      lattice_cho1[newchositei[newchomove],newchositej[newchomove]]=partsofcho
      oldchositei=newchositei[newchomove]
      oldchositej=newchositej[newchomove]
      choi[loop,(steplength+1)]=newchositei[newchomove]
      choj[loop,(steplength+1)]=newchositej[newchomove]
      steplength=steplength+1
      }
      
      
      
    }
  }

  if(sum(is.na(choi[loop,]))==0)
  {
    lattice_cho2=lattice_cho1
    break
  }
  else
  {
    lattice_cho1=lattice_cho2
  }
}

  print(loop)
}

lattice_cho2=lattice_cho1



length(which(lattice_cho1==1))
length(which(lattice_cho1==2))
length(which(lattice_cho1==3))
length(which(lattice_1==4))

dev.off()

{
  par(mar=c(1,1,1,1))
  plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
  for(i in 1:size)
  {
    for(j in 1:size)
    {
      par(new=T)
      if(lattice_cho1[i,j]==4)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'red')
      if(lattice_cho1[i,j]==5)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'white')
      if(lattice_cho1[i,j]==1)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'black')
      if(lattice_cho1[i,j]==2)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'blue')
      if(lattice_cho1[i,j]==3)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'grey')
    }
  }
}
sum(is.na(choi))
sum(is.na(choj))


#movechromatin and simulation
for(chomovesteps in 1:1000000)
{
  numberofcho=sample(1:(totallengthofcho),size=1)
    
  
  #头部
  if(numberofcho==1)
  {
    if(choi[1,1]+choi[1,3]==(2*choi[1,2]) | choj[1,1]+choj[1,3]==(2*choj[1,2]))
    {
      movechositei=c(choi[1,2],choi[1,2]+1,choi[1,2],choi[1,2]-1)
      movechositej=c(choj[1,2]-1,choj[1,2],choj[1,2]+1,choj[1,2])
      nextmovesite=sample(1:4,size=1)
      if(lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]==5 & movechositei[nextmovesite]>0 & movechositei[nextmovesite]<(size+1) & movechositej[nextmovesite]>0 & movechositej[nextmovesite]<(size+1))
      {
        lattice_cho2[movechositei[nextmovesite],movechositej[nextmovesite]]=lattice_cho1[choi[1,1],choj[1,1]]
        lattice_cho2[choi[1,1],choj[1,1]]=5
        
        oldchoenergy=0
        oldneibori=c(choi[1,1]-1,choi[1,1],choi[1,1]+1,choi[1,1]+1,choi[1,1]+1,choi[1,1],choi[1,1]-1,choi[1,1]-1)
        oldneiborj=c(choj[1,1]-1,choj[1,1]-1,choj[1,1]-1,choj[1,1],choj[1,1]+1,choj[1,1]+1,choj[1,1]+1,choj[1,1])
        for(oldneibor in 1:8)
        {
          if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
            oldchoenergy=oldchoenergy
          else
            oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],1]
        }
        
        newchoenergy=0
        newneibori=c(movechositei[nextmovesite]-1,movechositei[nextmovesite],movechositei[nextmovesite]+1,movechositei[nextmovesite]+1,movechositei[nextmovesite]+1,movechositei[nextmovesite],movechositei[nextmovesite]-1,movechositei[nextmovesite]-1)
        newneiborj=c(movechositej[nextmovesite]-1,movechositej[nextmovesite]-1,movechositej[nextmovesite]-1,movechositej[nextmovesite],movechositej[nextmovesite]+1,movechositej[nextmovesite]+1,movechositej[nextmovesite]+1,movechositej[nextmovesite])
        for(newneibor in 1:8)
        {
          if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
            newchoenergy=newchoenergy
          else
            newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],1]
        }
        
        if(newchoenergy<oldchoenergy)
        {
          lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]=1
          lattice_cho1[choi[1,1],choj[1,1]]=5
          choi[1,1]=movechositei[nextmovesite]
          choj[1,1]=movechositej[nextmovesite]
        }
        else
        {
          if(runif(1)<0.1*exp(-(newchoenergy-oldchoenergy)))
          {lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]=1
          lattice_cho1[choi[1,1],choj[1,1]]=5
          choi[1,1]=movechositei[nextmovesite]
          choj[1,1]=movechositej[nextmovesite]}
          else
          {lattice_cho2[movechositei[nextmovesite],movechositej[nextmovesite]]=5
          lattice_cho2[choi[1,1],choj[1,1]]=1}
        }
        
        
        
      }
      
    }
      
  }
  
  #尾部
  if(numberofcho==totallengthofcho)
  {
    if(choi[1,totallengthofcho]+choi[1,(totallengthofcho-2)]==(2*choi[1,(totallengthofcho-1)]) | choj[1,totallengthofcho]+choj[1,(totallengthofcho-2)]==(2*choj[1,(totallengthofcho-1)]))
    {
      movechositei=c(choi[1,(totallengthofcho-1)],choi[1,(totallengthofcho-1)]+1,choi[1,(totallengthofcho-1)],choi[1,(totallengthofcho-1)]-1)
      movechositej=c(choj[1,(totallengthofcho-1)]-1,choj[1,(totallengthofcho-1)],choj[1,(totallengthofcho-1)]+1,choj[1,(totallengthofcho-1)])
      nextmovesite=sample(1:4,size=1)
      if(lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]==5 & movechositei[nextmovesite]>0 & movechositei[nextmovesite]<(size+1) & movechositej[nextmovesite]>0 & movechositej[nextmovesite]<(size+1))
      {
        lattice_cho2[movechositei[nextmovesite],movechositej[nextmovesite]]=3
        lattice_cho2[choi[1,totallengthofcho],choj[1,totallengthofcho]]=5
        
        oldchoenergy=0
        oldneibori=c(choi[1,totallengthofcho]-1,choi[1,totallengthofcho],choi[1,totallengthofcho]+1,choi[1,totallengthofcho]+1,choi[1,totallengthofcho]+1,choi[1,totallengthofcho],choi[1,totallengthofcho]-1,choi[1,totallengthofcho]-1)
        oldneiborj=c(choj[1,totallengthofcho]-1,choj[1,totallengthofcho]-1,choj[1,totallengthofcho]-1,choj[1,totallengthofcho],choj[1,totallengthofcho]+1,choj[1,totallengthofcho]+1,choj[1,totallengthofcho]+1,choj[1,totallengthofcho])
        for(oldneibor in 1:8)
        {
          if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
            oldchoenergy=oldchoenergy
          else
            oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],3]
        }
        
        newchoenergy=0
        newneibori=c(movechositei[nextmovesite]-1,movechositei[nextmovesite],movechositei[nextmovesite]+1,movechositei[nextmovesite]+1,movechositei[nextmovesite]+1,movechositei[nextmovesite],movechositei[nextmovesite]-1,movechositei[nextmovesite]-1)
        newneiborj=c(movechositej[nextmovesite]-1,movechositej[nextmovesite]-1,movechositej[nextmovesite]-1,movechositej[nextmovesite],movechositej[nextmovesite]+1,movechositej[nextmovesite]+1,movechositej[nextmovesite]+1,movechositej[nextmovesite])
        for(newneibor in 1:8)
        {
          if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
            newchoenergy=newchoenergy
          else
            newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],3]
        }
        
        if(newchoenergy<oldchoenergy | newchoenergy==oldchoenergy)
        {
          lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]=3
          lattice_cho1[choi[1,totallengthofcho],choj[1,totallengthofcho]]=5
          choi[1,totallengthofcho]=movechositei[nextmovesite]
          choj[1,totallengthofcho]=movechositej[nextmovesite]
        }
        else
        {
          if(runif(1)<0.1*exp(-(newchoenergy-oldchoenergy)))
          {lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]=1
          lattice_cho1[choi[1,1],choj[1,1]]=5
          choi[1,1]=movechositei[nextmovesite]
          choj[1,1]=movechositej[nextmovesite]}
          else
          {lattice_cho2[movechositei[nextmovesite],movechositej[nextmovesite]]=5
          lattice_cho2[choi[1,1],choj[1,1]]=1}
        }
        
        
        
      }
      
    }
    
  }
  
  #中部
  if(numberofcho>1 & numberofcho<totallengthofcho)
  {
    if(choi[1,numberofcho+1]+choi[1,(numberofcho-1)]!=(2*choi[1,(numberofcho)]) | choj[1,(numberofcho-1)]+choj[1,(numberofcho+1)]!=(2*choj[1,(numberofcho)]))
    {
      movechositei=c(choi[1,(numberofcho)]-1,choi[1,(numberofcho)]+1,choi[1,(numberofcho)]+1,choi[1,(numberofcho)]-1)
      movechositej=c(choj[1,(numberofcho)]-1,choj[1,(numberofcho)]-1,choj[1,(numberofcho)]+1,choj[1,(numberofcho)]+1)
      nextmovesite=sample(1:4,size=1)
      if(lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]==5 & movechositei[nextmovesite]>0 & movechositei[nextmovesite]<(size+1) & movechositej[nextmovesite]>0 & movechositej[nextmovesite]<(size+1))
      {
        if((movechositei[nextmovesite]+choi[1,(numberofcho)])==(choi[1,(numberofcho-1)]+choi[1,(numberofcho+1)]) & (movechositej[nextmovesite]+choj[1,(numberofcho)])==(choj[1,(numberofcho-1)]+choj[1,(numberofcho+1)]))
        {
        lattice_cho2[movechositei[nextmovesite],movechositej[nextmovesite]]=lattice_cho1[choi[1,numberofcho],choj[1,numberofcho]]
        lattice_cho2[choi[1,numberofcho],choj[1,numberofcho]]=5
        
        lattice_cho1[movechositei[nextmovesite],movechositej[nextmovesite]]=lattice_cho2[movechositei[nextmovesite],movechositej[nextmovesite]]
        lattice_cho1[choi[1,numberofcho],choj[1,numberofcho]]=5
        choi[1,numberofcho]=movechositei[nextmovesite]
        choj[1,numberofcho]=movechositej[nextmovesite]
        }
        
      }
    }
  }
  
  
}



{
  par(mar=c(3,4,1,1))
  plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
  for(i in 1:size)
  {
    for(j in 1:size)
    {
      par(new=T)
      if(lattice_cho1[i,j]==4)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'red')
      if(lattice_cho1[i,j]==5)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'white')
      if(lattice_cho1[i,j]==1)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'black')
      if(lattice_cho1[i,j]==2)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'blue')
      if(lattice_cho1[i,j]==3)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'grey')
    }
  }
}



{
  par(mar=c(3,4,1,1))
  plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
  for(i in 1:size)
  {
    for(j in 1:size)
    {
      par(new=T)
      if(lattice_cho2[i,j]==4)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'white')
      if(lattice_cho2[i,j]==5)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'white')
      if(lattice_cho2[i,j]==1)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'black')
      if(lattice_cho2[i,j]==2)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'blue')
      if(lattice_cho2[i,j]==3)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = T, col = 'grey')
    }
  }
}


