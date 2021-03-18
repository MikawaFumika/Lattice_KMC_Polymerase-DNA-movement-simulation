#open -cd and direct the files
convert -delay 10 *.pdf -loop 1 animated1.gif

#fix endpoint
#4cor



size=30
kbt=1

lattice_cho1=matrix(nrow=size, ncol=size)
lattice_cho2=matrix(nrow=size, ncol=size)

totalcho=4


centerarea=1


lengthofchoarray=c(14,9,1,12)



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
#3cor



size=30
kbt=1

lattice_cho1=matrix(nrow=size, ncol=size)
lattice_cho2=matrix(nrow=size, ncol=size)

totalcho=3


centerarea=1


lengthofchoarray=c(11,9,5,3)



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

#cho2
choj[2,]=c(seq(disfori+1,size/2,1),rep(size/2,touguelength),rep(size/2+1,touguelength),seq(size/2+1,size-disfori,1))
choi[2,]=c(rep(size-disforj+1,totallengthofcho/2-touguelength),seq(size-disforj,size-disforj-touguelength+1,-1),seq(size-disforj-touguelength+1,size-disforj,1),rep(size-disforj+1,totallengthofcho/2-touguelength))


touguelength=(totallengthofcho-(10+10+6))/2

#cho1
choi[1,]=c(rep(disfori+5,10),seq(disfori+5,size/2,1),rep(size/2,touguelength),rep(size/2+1,touguelength),seq(size/2+1,size-disfori,1))
choj[1,]=c(seq(15,6,-1),rep(disforj,6),seq(disforj+1,disforj+touguelength,1),seq(disforj+touguelength,disforj+1,-1),rep(disforj,10))

#cho3
choi[3,]=c(seq(size-disfori,size/2+1,-1),rep(size/2+1,touguelength),rep(size/2,touguelength),seq(size/2,disfori+5,-1),rep(disfori+5,10))
choj[3,]=c(rep(size-disforj+1,10),seq(size-disforj,size-disforj-touguelength+1,-1),seq(size-disforj-touguelength+1,size-disforj,1),rep(size-disforj+1,6),seq(25,16,-1))


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


#simu

dir="/Users/yeyusong/Desktop/德国工作 project/计算方法 notes/Create_movie"
dir1="/Users/yeyusong/Desktop/德国工作 project/计算方法 notes/Create_blur"
dir2="/Users/yeyusong/Desktop/德国工作 project/计算方法 notes/Create_blur/blur_black"
dir3="/Users/yeyusong/Desktop/德国工作 project/计算方法 notes/Create_blur/blur_blue"
dir4="/Users/yeyusong/Desktop/德国工作 project/计算方法 notes/Create_blur/blur_grey"

setwd(dir) 
library(spatstat)

moviestep=100000
#simulation and create movie
Sys.time()
successunmber=0
successunmberend=0
successunmberkink=0
successunmbercrank=0
totalsteps=10000000
countsteps=totalsteps/moviestep


extendlengthofgrey=10

clusterindex=seq(1:countsteps)
weightofpol=1
layer=totalpol/(totalpol+totalcho*totallengthofcho)*weightofpol
for(steps in 1:totalsteps)
{
  
  twolayer=runif(1,0,1)
  if(twolayer<layer)
  {
    indexpol=sample(1:totalpol,size=1)
    lattice_i=indexpoli[indexpol]
    lattice_j=indexpolj[indexpol]
  }
  else
  {
    #normal
    # randomcho=sample(1:totalcho,size=1)
    # randomchosite=sample(1:totallengthofcho,size=1)
    # lattice_i=choi[randomcho,randomchosite]
    # lattice_j=choj[randomcho,randomchosite]
    
    #fix endpoint for cor
    randomcho=sample(1:totalcho,size=1)
    randomchosite=sample(2:(totallengthofcho-1),size=1)
    lattice_i=choi[randomcho,randomchosite]
    lattice_j=choj[randomcho,randomchosite]
    
    #move small
    # randomcho=sample(1:totalcho,size=1)
    # randomchosite=sample(1:(totallengthofcho),size=1)
    # lattice_i=choi[randomcho,randomchosite]
    # lattice_j=choj[randomcho,randomchosite]
  }
  
  neiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
  neibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
  newmove=sample(1:8,size=1)
  
  
  if(neiborj[newmove]>0 & neiborj[newmove]<(size+1) & neibori[newmove]>0 & neibori[newmove]<(size+1))
  {
    
    # 动pol
    if(twolayer<layer)
    {
      if(lattice_1[neibori[newmove],neiborj[newmove]]==5)
      {
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
            oldenergy=oldenergy+coefficientmatrix[lattice_1[neibori[oldneibor],neiborj[oldneibor]],4]+coefficientmatrix[lattice_cho1[neibori[oldneibor],neiborj[oldneibor]],4]
        }
        
        
        
        newenergy=0
        newneibori=c(newsitei-1,newsitei,newsitei+1,newsitei+1,newsitei+1,newsitei,newsitei-1,newsitei-1)
        newneiborj=c(newsitej-1,newsitej-1,newsitej-1,newsitej,newsitej+1,newsitej+1,newsitej+1,newsitej)
        for(newneibor in 1:8)
        {
          if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
            newenergy=newenergy
          else
            newenergy=newenergy+coefficientmatrix[lattice_2[newneibori[newneibor],newneiborj[newneibor]],4]+coefficientmatrix[lattice_cho1[newneibori[newneibor],newneiborj[newneibor]],4]
        }
        
        if(newenergy<=oldenergy)
        {
          lattice_1[lattice_i,lattice_j]=5
          lattice_1[neibori[newmove],neiborj[newmove]]=4
          successunmber=successunmber+1
          indexpoli[indexpol]=neibori[newmove]
          indexpolj[indexpol]=neiborj[newmove]
        }
        else
        {
          if(runif(1)<kp*exp(-(newenergy-oldenergy))/exp(-(oldenergy-newenergy)))
          {lattice_1[lattice_i,lattice_j]=5
          lattice_1[neibori[newmove],neiborj[newmove]]=4
          successunmber=successunmber+1
          indexpoli[indexpol]=neibori[newmove]
          indexpolj[indexpol]=neiborj[newmove]}
          else
          {lattice_2[lattice_i,lattice_j]=4
          lattice_2[neibori[newmove],neiborj[newmove]]=5}
        }
        
        
      }
      
    }
    
    
    
    # 动cho
    if(twolayer>layer)
    {
      if(lattice_cho1[lattice_i,lattice_j]!=5)
      {
        for(chonumber1 in 1:totalcho)
        {
          for(numberofchosite1 in 1:totallengthofcho)
          {
            if(choi[chonumber1,numberofchosite1]==lattice_i & choj[chonumber1,numberofchosite1]==lattice_j)
            {
              chonumber=chonumber1
              numberofcho=numberofchosite1
            }
          }
        }
        
        
        #头部
        if(numberofcho==1 & lattice_cho1[neibori[newmove],neiborj[newmove]]==5)
        {
          
          if((neibori[newmove]!=choi[chonumber,1] & neiborj[newmove]!=choj[chonumber,1]) &     ( neibori[newmove]==choi[chonumber,2] | neiborj[newmove]==choj[chonumber,2]))
          {
            lattice_cho2[neibori[newmove],neiborj[newmove]]=1
            lattice_cho2[choi[chonumber,1],choj[chonumber,1]]=5
            
            oldchoenergy=0
            oldneibori=c(choi[chonumber,1]-1,choi[chonumber,1],choi[chonumber,1]+1,choi[chonumber,1]+1,choi[chonumber,1]+1,choi[chonumber,1],choi[chonumber,1]-1,choi[chonumber,1]-1)
            oldneiborj=c(choj[chonumber,1]-1,choj[chonumber,1]-1,choj[chonumber,1]-1,choj[chonumber,1],choj[chonumber,1]+1,choj[chonumber,1]+1,choj[chonumber,1]+1,choj[chonumber,1])
            for(oldneibor in 1:8)
            {
              if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                oldchoenergy=oldchoenergy
              else
                oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],1]+coefficientmatrix[lattice_1[oldneibori[oldneibor],oldneiborj[oldneibor]],1]
            }
            
            newchoenergy=0
            newneibori=c(neibori[newmove]-1,neibori[newmove],neibori[newmove]+1,neibori[newmove]+1,neibori[newmove]+1,neibori[newmove],neibori[newmove]-1,neibori[newmove]-1)
            newneiborj=c(neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove],neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove])
            for(newneibor in 1:8)
            {
              if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                newchoenergy=newchoenergy
              else
                newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],1]+coefficientmatrix[lattice_1[newneibori[newneibor],newneiborj[newneibor]],1]
            }
            
            if(newchoenergy<=oldchoenergy)
            {
              lattice_cho1[neibori[newmove],neiborj[newmove]]=1
              lattice_cho1[choi[chonumber,1],choj[chonumber,1]]=5
              choi[chonumber,1]=neibori[newmove]
              choj[chonumber,1]=neiborj[newmove]
              successunmberend=successunmberend+1
            }
            else
            {
              if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
              {lattice_cho1[neibori[newmove],neiborj[newmove]]=1
              lattice_cho1[choi[chonumber,1],choj[chonumber,1]]=5
              choi[chonumber,1]=neibori[newmove]
              choj[chonumber,1]=neiborj[newmove]
              successunmberend=successunmberend+1}
              else
              {lattice_cho2[neibori[newmove],neiborj[newmove]]=5
              lattice_cho2[choi[chonumber,1],choj[chonumber,1]]=1}
            }
            
            
            
          }
          
          
          
        }
        
        
        #尾部
        if(numberofcho==totallengthofcho & lattice_cho1[neibori[newmove],neiborj[newmove]]==5)
        {
          
          if((neibori[newmove]!=choi[chonumber,totallengthofcho] & neiborj[newmove]!=choj[chonumber,totallengthofcho]) & (neibori[newmove]==choi[chonumber,(totallengthofcho-1)] | neiborj[newmove]==choj[chonumber,(totallengthofcho-1)]))
          {
            lattice_cho2[neibori[newmove],neiborj[newmove]]=1
            lattice_cho2[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=5
            
            oldchoenergy=0
            oldneibori=c(choi[chonumber,totallengthofcho]-1,choi[chonumber,totallengthofcho],choi[chonumber,totallengthofcho]+1,choi[chonumber,totallengthofcho]+1,choi[chonumber,totallengthofcho]+1,choi[chonumber,totallengthofcho],choi[chonumber,totallengthofcho]-1,choi[chonumber,totallengthofcho]-1)
            oldneiborj=c(choj[chonumber,totallengthofcho]-1,choj[chonumber,totallengthofcho]-1,choj[chonumber,totallengthofcho]-1,choj[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]+1,choj[chonumber,totallengthofcho]+1,choj[chonumber,totallengthofcho]+1,choj[chonumber,totallengthofcho])
            for(oldneibor in 1:8)
            {
              if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                oldchoenergy=oldchoenergy
              else
                oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],3]+coefficientmatrix[lattice_1[oldneibori[oldneibor],oldneiborj[oldneibor]],3]
            }
            
            newchoenergy=0
            newneibori=c(neibori[newmove]-1,neibori[newmove],neibori[newmove]+1,neibori[newmove]+1,neibori[newmove]+1,neibori[newmove],neibori[newmove]-1,neibori[newmove]-1)
            newneiborj=c(neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove],neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove])
            for(newneibor in 1:8)
            {
              if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                newchoenergy=newchoenergy
              else
                newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],3]+coefficientmatrix[lattice_1[newneibori[newneibor],newneiborj[newneibor]],3]
            }
            
            if(newchoenergy<=oldchoenergy)
            {
              lattice_cho1[neibori[newmove],neiborj[newmove]]=1
              lattice_cho1[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=5
              choi[chonumber,totallengthofcho]=neibori[newmove]
              choj[chonumber,totallengthofcho]=neiborj[newmove]
              successunmberend=successunmberend+1
            }
            else
            {
              if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
              {lattice_cho1[neibori[newmove],neiborj[newmove]]=1
              lattice_cho1[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=5
              choi[chonumber,totallengthofcho]=neibori[newmove]
              choj[chonumber,totallengthofcho]=neiborj[newmove]
              successunmberend=successunmberend+1}
              else
              {lattice_cho2[neibori[newmove],neiborj[newmove]]=5
              lattice_cho2[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=1}
            }
            
            
            
          }
          
          
          
        }
        
        
        #中部kink
        if(numberofcho>1 & numberofcho<totallengthofcho & lattice_cho1[neibori[newmove],neiborj[newmove]]==5)
        {
          if(TRUE)
          {
            
            if((neibori[newmove]+choi[chonumber,(numberofcho)])==(choi[chonumber,(numberofcho-1)]+choi[chonumber,(numberofcho+1)]) & (neiborj[newmove]+choj[chonumber,(numberofcho)])==(choj[chonumber,(numberofcho-1)]+choj[chonumber,(numberofcho+1)]))
            {
              lattice_cho2[neibori[newmove],neiborj[newmove]]=lattice_cho1[lattice_i,lattice_j]
              lattice_cho2[lattice_i,lattice_j]=5
              
              
              oldchoenergy=0
              oldneibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
              oldneiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
              for(oldneibor in 1:8)
              {
                if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                  oldchoenergy=oldchoenergy
                else
                  oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]+coefficientmatrix[lattice_1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]
              }
              
              newchoenergy=0
              newneibori=c(neibori[newmove]-1,neibori[newmove],neibori[newmove]+1,neibori[newmove]+1,neibori[newmove]+1,neibori[newmove],neibori[newmove]-1,neibori[newmove]-1)
              newneiborj=c(neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove],neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove])
              for(newneibor in 1:8)
              {
                if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                  newchoenergy=newchoenergy
                else
                  newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho1[lattice_i,lattice_j]]+coefficientmatrix[lattice_1[newneibori[newneibor],newneiborj[newneibor]],lattice_cho1[lattice_i,lattice_j]]
              }
              
              if(newchoenergy<=oldchoenergy)
              {
                lattice_cho1[neibori[newmove],neiborj[newmove]]=lattice_cho2[neibori[newmove],neiborj[newmove]]
                lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=5
                choi[chonumber,numberofcho]=neibori[newmove]
                choj[chonumber,numberofcho]=neiborj[newmove]
                successunmberkink=successunmberkink+1
              }
              else
              {
                if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                {lattice_cho1[neibori[newmove],neiborj[newmove]]=lattice_cho2[neibori[newmove],neiborj[newmove]]
                lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=5
                choi[chonumber,numberofcho]=neibori[newmove]
                choj[chonumber,numberofcho]=neiborj[newmove]
                successunmberkink=successunmberkink+1}
                else
                {lattice_cho2[neibori[newmove],neiborj[newmove]]=5
                lattice_cho2[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]}
              }
              
            }
            
            
          }
          
          
          
          
          
          
        }
        
        # 中部 crankshaft
        
        if(numberofcho>3 & numberofcho<(totallengthofcho-1))
        {
          # i相等（-3 2）
          if(choi[chonumber,numberofcho-3]==choi[chonumber,numberofcho-2] & choi[chonumber,numberofcho-3]==choi[chonumber,numberofcho+1] & choi[chonumber,numberofcho-3]==choi[chonumber,numberofcho+2])
          {
            icrankshaft=2*choi[chonumber,numberofcho-3]-lattice_i
            if(icrankshaft>0 & icrankshaft<(size+1))
            {
              if(lattice_cho2[icrankshaft,lattice_j]==5 & lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]==5)
              {
                
                lattice_cho2[icrankshaft,lattice_j]=lattice_cho1[lattice_i,lattice_j]
                lattice_cho2[lattice_i,lattice_j]=5
                lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]=lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]
                lattice_cho2[lattice_i,choj[chonumber,numberofcho-1]]=5
                
                
                oldchoenergy=0
                
                oldneibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
                oldneiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
                
                for(oldneibor in 1:8)
                {
                  if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                    oldchoenergy=oldchoenergy
                  else
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]+coefficientmatrix[lattice_1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]
                }
                
                oldneibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
                oldneiborj=c(choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1],choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1])
                
                for(oldneibor in 1:8)
                {
                  if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                    oldchoenergy=oldchoenergy
                  else
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]]+coefficientmatrix[lattice_1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]]
                }
                
                newchoenergy=0
                
                newneibori=c(icrankshaft-1,icrankshaft,icrankshaft+1,icrankshaft+1,icrankshaft+1,icrankshaft,icrankshaft-1,icrankshaft-1)
                newneiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[icrankshaft,lattice_j]]+coefficientmatrix[lattice_1[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[icrankshaft,lattice_j]]
                }
                
                newneibori=c(icrankshaft-1,icrankshaft,icrankshaft+1,icrankshaft+1,icrankshaft+1,icrankshaft,icrankshaft-1,icrankshaft-1)
                newneiborj=c(choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1],choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1])
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]]+coefficientmatrix[lattice_1[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]]
                }
                
                
                if(newchoenergy<=oldchoenergy)
                {
                  lattice_cho1[icrankshaft,lattice_j]=lattice_cho2[icrankshaft,lattice_j]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choi[chonumber,numberofcho]=icrankshaft
                  
                  
                  lattice_cho1[icrankshaft,choj[chonumber,numberofcho-1]]=lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]
                  lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]=5
                  choi[chonumber,numberofcho-1]=icrankshaft
                  
                  successunmbercrank=successunmbercrank+1
                  
                }
                else
                {
                  if(runif(1)<0.25*kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                  {lattice_cho1[icrankshaft,lattice_j]=lattice_cho2[icrankshaft,lattice_j]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choi[chonumber,numberofcho]=icrankshaft
                  
                  
                  lattice_cho1[icrankshaft,choj[chonumber,numberofcho-1]]=lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]
                  lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]=5
                  choi[chonumber,numberofcho-1]=icrankshaft
                  
                  successunmbercrank=successunmbercrank+1}
                  else
                  {
                    lattice_cho2[icrankshaft,lattice_j]=5
                    lattice_cho2[lattice_i,lattice_j]=lattice_cho1[lattice_i,lattice_j]
                    
                    lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]=5
                    lattice_cho2[lattice_i,choj[chonumber,numberofcho-1]]=lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]
                  }
                }
                
              }
              
            }
            
          }
          
          # j相等（-3 2）
          if(choj[chonumber,numberofcho-3]==choj[chonumber,numberofcho-2] & choj[chonumber,numberofcho-3]==choj[chonumber,numberofcho+1] & choj[chonumber,numberofcho-3]==choj[chonumber,numberofcho+2])
          {
            jcrankshaft=2*choj[chonumber,numberofcho-3]-lattice_j
            if(jcrankshaft>0 & jcrankshaft<(size+1))
            {
              if(lattice_cho2[lattice_i,jcrankshaft]==5 & lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]==5)
              {
                
                lattice_cho2[lattice_i,jcrankshaft]=lattice_cho1[lattice_i,lattice_j]
                lattice_cho2[lattice_i,lattice_j]=5
                lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]=lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]
                lattice_cho2[choi[chonumber,numberofcho-1],lattice_j]=5
                
                
                oldchoenergy=0
                
                oldneibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
                oldneiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
                
                for(oldneibor in 1:8)
                {
                  if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                    oldchoenergy=oldchoenergy
                  else
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]+coefficientmatrix[lattice_1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]
                }
                
                oldneibori=c(choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1]-1)
                oldneiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
                
                for(oldneibor in 1:8)
                {
                  if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                    oldchoenergy=oldchoenergy
                  else
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]]+coefficientmatrix[lattice_1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]]
                }
                
                newchoenergy=0
                
                newneibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
                newneiborj=c(jcrankshaft-1,jcrankshaft-1,jcrankshaft-1,jcrankshaft,jcrankshaft+1,jcrankshaft+1,jcrankshaft+1,jcrankshaft)
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[lattice_i,jcrankshaft]]+coefficientmatrix[lattice_1[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[lattice_i,jcrankshaft]]
                }
                
                newneibori=c(choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1]-1)
                newneiborj=c(jcrankshaft-1,jcrankshaft-1,jcrankshaft-1,jcrankshaft,jcrankshaft+1,jcrankshaft+1,jcrankshaft+1,jcrankshaft)
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]]+coefficientmatrix[lattice_1[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]]
                }
                
                
                if(newchoenergy<=oldchoenergy)
                {
                  lattice_cho1[lattice_i,jcrankshaft]=lattice_cho2[lattice_i,jcrankshaft]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choj[chonumber,numberofcho]=jcrankshaft
                  
                  
                  lattice_cho1[choi[chonumber,numberofcho-1],jcrankshaft]=lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]
                  lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]=5
                  choj[chonumber,numberofcho-1]=jcrankshaft
                  
                  successunmbercrank=successunmbercrank+1
                }
                else
                {
                  if(runif(1)<0.25*kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                  {lattice_cho1[lattice_i,jcrankshaft]=lattice_cho2[lattice_i,jcrankshaft]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choj[chonumber,numberofcho]=jcrankshaft
                  
                  
                  lattice_cho1[choi[chonumber,numberofcho-1],jcrankshaft]=lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]
                  lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]=5
                  choj[chonumber,numberofcho-1]=jcrankshaft
                  
                  successunmbercrank=successunmbercrank+1}
                  else
                  {
                    lattice_cho2[lattice_i,jcrankshaft]=5
                    lattice_cho2[lattice_i,lattice_j]=lattice_cho1[lattice_i,lattice_j]
                    
                    lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]=5
                    lattice_cho2[choi[chonumber,numberofcho-1],lattice_j]=lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]
                  }
                }
                
              }
              
            }
            
          }
          
        }
        
        
      }
      
    }
    
  }
  if(as.integer(steps/moviestep)==(steps/moviestep))
  {
    a=steps/moviestep
    b=10
    c=1
    while(a/b>=1){b=b*10
    c=c+1}
    print(steps/moviestep)
    numberofmovie=as.character(steps/moviestep)
    NAME=paste0(numberofmovie,".pdf",sep='')
    for(loopadd0 in 1:(6-c))
      NAME=paste0(0,NAME)
    
    #check if extend grey
    if((steps/moviestep)==50)
    {
      bbb=lengthofchoarray[1]+lengthofchoarray[2]+lengthofchoarray[3]+1
      for(zzz in 1:4)
      {
      for(extendsteps in 1:extendlengthofgrey)
      {
      lattice_cho1[choi[zzz,bbb+extendsteps-1],choj[zzz,bbb+extendsteps-1]]=3
      }
      }
      lattice_cho2=lattice_cho1
    }
    
    #gaosi blur code
    #blur red
    setwd(dir1)
    pdf(file=NAME,width=5,height=5)
    
    Z=as.im(-(lattice_1-5))
    Z1=blur(Z, 1, bleed=TRUE)
    Z1=Z1$v
    Z1=Z1-min(Z1)
    Z1=Z1/max(Z1)
    
    a=250/255
    
    par(mar=c(1,1,1,1),mfrow=c(2,2))
    plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
    for(i in 1:size)
    {
      for(j in 1:size)
      {
        par(new=T)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = rgb(Z1[i,j]*a*225/255,Z1[i,j]*a*152/255,Z1[i,j]*a*192/255))
      }
    }
    
    
    #blur black
    Z=as.im(lattice_cho1%%5%%3%%2)
    Z1=blur(Z, 1, bleed=TRUE)
    Z1=Z1$v
    Z1=Z1-min(Z1)
    Z1=Z1/max(Z1)
    
    
    par(mar=c(1,1,1,1))
    plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
    for(i in 1:size)
    {
      for(j in 1:size)
      {
        par(new=T)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = rgb(Z1[i,j]*a*255/255,Z1[i,j]*a*255/255,Z1[i,j]*a*255/255))
      }
    }
    
    
    #blur blue
    Z=as.im((lattice_cho1-1)%%4%%2)
    Z1=blur(Z, 1, bleed=TRUE)
    Z1=Z1$v
    Z1=Z1-min(Z1)
    Z1=Z1/max(Z1)
    
    
    par(mar=c(1,1,1,1))
    plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
    for(i in 1:size)
    {
      for(j in 1:size)
      {
        par(new=T)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = rgb(Z1[i,j]*a*0/255,Z1[i,j]*a*255/255,Z1[i,j]*a*255/255))
      }
    }
    
    
    #blur grey
    Z=as.im(((lattice_cho1-1)%%4-(lattice_cho1-1)%%4%%2)/2)
    Z1=blur(Z, 1, bleed=TRUE)
    Z1=Z1$v
    Z1=Z1-min(Z1)
    Z1=Z1/max(Z1)
    
    
    par(mar=c(1,1,1,1))
    plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
    for(i in 1:size)
    {
      for(j in 1:size)
      {
        par(new=T)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = rgb(Z1[i,j]*a*124/255,Z1[i,j]*a*252/255,Z1[i,j]*a*0/255))
      }
    }
    dev.off()
    
    
    
    setwd(dir) 
    pdf(file=NAME,width=4,height=4)
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
    dev.off()
    
    
    
    #statistic
    heatmaplattice=heatmaplattice-lattice_1+5
    
    
    
  }
  
}










