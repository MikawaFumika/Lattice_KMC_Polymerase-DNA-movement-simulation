

#difine matrix and initial condition
size=30
kbt=1

lattice_cho1=matrix(nrow=size, ncol=size)
lattice_cho2=matrix(nrow=size, ncol=size)

totalcho=3


centerarea=1


lengthofchoarray=c(6,4,4,6)



totallengthofcho=sum(lengthofchoarray)

#record coordinace
choi=matrix(nrow=totalcho, ncol=totallengthofcho)
choj=matrix(nrow=totalcho, ncol=totallengthofcho)


lengthofcho=c(lengthofchoarray[1]-1,lengthofchoarray[-1])


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

#startcho_i=sample(1:(size),size=1)
#startcho_j=sample(1:(size),size=1)
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
  
  for(partsofcho in 1:length(lengthofchoarray))
  {
    for(lengthcho1 in 1:(lengthofcho[partsofcho]))
    {
      newchositei=c(oldchositei,oldchositei+1,oldchositei,oldchositei-1)
      newchositej=c(oldchositej-1,oldchositej,oldchositej+1,oldchositej)
      newchomove=sample(1:4,size=1)
      
      for(ooo in 1:100)
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
      
      if(ooo<100)
      {
      if(partsofcho%%3==0)
        {lattice_cho1[newchositei[newchomove],newchositej[newchomove]]=3}
      else
        {lattice_cho1[newchositei[newchomove],newchositej[newchomove]]=partsofcho%%3}
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



#fix endpoint
#4cor



size=30
kbt=1

lattice_cho1=matrix(nrow=size, ncol=size)
lattice_cho2=matrix(nrow=size, ncol=size)

totalcho=4


centerarea=1


lengthofchoarray=c(10,15,10,1)



totallengthofcho=sum(lengthofchoarray)

#record coordinace
choi=matrix(nrow=totalcho, ncol=sum(lengthofchoarray))
choj=matrix(nrow=totalcho, ncol=sum(lengthofchoarray))

disfori=5
disforj=5
touguelength=(totallengthofcho-(size-2*disfori))/2



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





#cho1
choi[1,]=c(seq(disfori+1,size/2,1),rep(size/2,touguelength),rep(size/2+1,touguelength),seq(size/2+1,size-disfori,1))
choj[1,]=c(rep(disforj,totallengthofcho/2-touguelength),seq(disforj+1,disforj+touguelength,1),seq(disforj+touguelength,disforj+1,-1),rep(disforj,totallengthofcho/2-touguelength))

#cho2
choj[2,]=c(seq(disfori+1,size/2,1),rep(size/2,touguelength),rep(size/2+1,touguelength),seq(size/2+1,size-disfori,1))
choi[2,]=c(rep(size-disforj+1,totallengthofcho/2-touguelength),seq(size-disforj,size-disforj-touguelength+1,-1),seq(size-disforj-touguelength+1,size-disforj,1),rep(size-disforj+1,totallengthofcho/2-touguelength))

#cho3
choi[3,]=c(seq(size-disfori,size/2+1,-1),rep(size/2+1,touguelength),rep(size/2,touguelength),seq(size/2,disfori+1,-1))
choj[3,]=c(rep(size-disforj+1,totallengthofcho/2-touguelength),seq(size-disforj,size-disforj-touguelength+1,-1),seq(size-disforj-touguelength+1,size-disforj,1),rep(size-disforj+1,totallengthofcho/2-touguelength))

#cho4
choj[4,]=c(seq(size-disfori,size/2+1,-1),rep(size/2+1,touguelength),rep(size/2,touguelength),seq(size/2,disfori+1,-1))
choi[4,]=c(rep(disforj,totallengthofcho/2-touguelength),seq(disforj+1,disforj+touguelength,1),seq(disforj+touguelength,disforj+1,-1),rep(disforj,totallengthofcho/2-touguelength))

for(j in 1:totalcho)
{
for(ranse in 1:lengthofchoarray[1])
lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=1
for(ranse in (lengthofchoarray[1]+1):(lengthofchoarray[1]+lengthofchoarray[2]))
lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=2
for(ranse in (lengthofchoarray[1]+lengthofchoarray[2]+1):(lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]))
lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=3
for(ranse in (lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]+1):(lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]+lengthofchoarray[4]))
lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=1
}

lattice_cho2=lattice_cho1



#fix endpoint
#small with large



size=30
kbt=1

lattice_cho1=matrix(nrow=size, ncol=size)
lattice_cho2=matrix(nrow=size, ncol=size)

totalcho=1


centerarea=1


lengthofchoarray=c(6,9,1,6)



totallengthofcho=sum(lengthofchoarray)

#record coordinace
choi=matrix(nrow=totalcho, ncol=sum(lengthofchoarray))
choj=matrix(nrow=totalcho, ncol=sum(lengthofchoarray))

disfori=5
disforj=10
touguelength=(totallengthofcho-(size-2*disfori))/2



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





#cho1
choi[1,]=c(seq(disfori+1,size/2,1),rep(size/2,touguelength),rep(size/2+1,touguelength),seq(size/2+1,size-disfori,1))
choj[1,]=c(rep(disforj,totallengthofcho/2-touguelength),seq(disforj+1,disforj+touguelength,1),seq(disforj+touguelength,disforj+1,-1),rep(disforj,totallengthofcho/2-touguelength))


for(j in 1:totalcho)
{
  for(ranse in 1:lengthofchoarray[1])
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=1
  for(ranse in (lengthofchoarray[1]+1):(lengthofchoarray[1]+lengthofchoarray[2]))
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=2
  for(ranse in (lengthofchoarray[1]+lengthofchoarray[2]+1):(lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]))
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=3
  for(ranse in (lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]+1):(lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]+lengthofchoarray[4]))
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=1
}

lattice_cho2=lattice_cho1




#fix endpoint
#croissant

size=30
kbt=1

lattice_cho1=matrix(nrow=size, ncol=size)
lattice_cho2=matrix(nrow=size, ncol=size)

totalcho=2


centerarea=1


lengthofchoarray=c(6,15,1,6)



totallengthofcho=sum(lengthofchoarray)

#record coordinace
choi=matrix(nrow=totalcho, ncol=sum(lengthofchoarray))
choj=matrix(nrow=totalcho, ncol=sum(lengthofchoarray))

disfori=5
disforj=5
touguelength=(totallengthofcho-(size-2*disfori))/2



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


#cho1
choi[1,]=c(seq(size/2,size/2-touguelength+1,-1),rep(size/2-touguelength,size-2*disfori),seq(size/2-touguelength+1,size/2,1))
choj[1,]=c(rep(disfori+1,touguelength),seq(disfori+1,size-disfori,1),rep(size-disfori,touguelength))


#cho2
choi[2,]=c(seq(size/2+1,size/2+touguelength,1),rep(size/2+touguelength+1,size-2*disfori),seq(size/2+touguelength,size/2+1,-1))
choj[2,]=c(rep(disfori+1,touguelength),seq(disfori+1,size-disfori,1),rep(size-disfori,touguelength))


for(j in 1:totalcho)
{
  for(ranse in 1:lengthofchoarray[1])
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=1
  for(ranse in (lengthofchoarray[1]+1):(lengthofchoarray[1]+lengthofchoarray[2]))
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=2
  for(ranse in (lengthofchoarray[1]+lengthofchoarray[2]+1):(lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]))
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=3
  for(ranse in (lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]+1):(lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]+lengthofchoarray[4]))
    lattice_cho1[choi[j,][ranse],choj[j,][ranse]]=1
}

lattice_cho2=lattice_cho1


#draw pic


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
length(which(lattice_cho1==1))
length(which(lattice_cho1==2))
length(which(lattice_cho1==3))
length(which(lattice_1==4))
