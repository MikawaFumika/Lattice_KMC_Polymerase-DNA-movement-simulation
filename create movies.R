
saveGIF({for (i in 1:10) plot(runif(10), ylim = 0:1)})

saveHTML({for (i in 1:10) plot(runif(10), ylim = 0:1)})

saveMovie({for (i in 1:10) plot(runif(10), ylim = 0:1)})

setwd("/Users/yeyusong/Desktop/德国工作 project/计算方法 notes/Create_movie")
imagesinfiles=list.files(getwd()) 

saveGIF({for (i in 1:length(list.files(getwd()))) imagesinfiles[i]}, movie.name = 'imagesinfiles' , img.name = "Rplot", convert = "magick", 
        cmd.fun, clean = TRUE)



convert -delay 10 *.pdf -loop 1 animated1.gif







dir="/Users/yeyusong/Desktop/德国工作 project/计算方法 notes/Create_movie"
setwd(dir) 


moviestep=100000
#simulation and create movie
Sys.time()
successunmber=0
totalsteps=20000000
countsteps=totalsteps/moviestep

clusterindex=seq(1:countsteps)
for(steps in 1:totalsteps)
{
  lattice_i=sample(1:size,size=1)
  lattice_j=sample(1:size,size=1)
  neiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
  neibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
  newmove=sample(1:8,size=1)
  
  twolayer=runif(1,0,1)
  
  if(neiborj[newmove]>0 & neiborj[newmove]<(size+1) & neibori[newmove]>0 & neibori[newmove]<(size+1))
  {
    
    # 动pol
    if(lattice_1[lattice_i,lattice_j]==4 & lattice_1[neibori[newmove],neiborj[newmove]]==5 & twolayer>0.5)
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
      
      if(newenergy<oldenergy)
      {
        lattice_1[lattice_i,lattice_j]=5
        lattice_1[neibori[newmove],neiborj[newmove]]=4
        successunmber=successunmber+1
      }
      else
      {
        if(runif(1)<kp*exp(-(newenergy-oldenergy))/exp(-(oldenergy-newenergy)))
        {lattice_1[lattice_i,lattice_j]=5
        lattice_1[neibori[newmove],neiborj[newmove]]=4
        successunmber=successunmber+1}
        else
        {lattice_2[lattice_i,lattice_j]=4
        lattice_2[neibori[newmove],neiborj[newmove]]=5}
      }
      
      
    }
    
    
    
    # 动cho
    if(lattice_cho1[lattice_i,lattice_j]!=5 & lattice_cho1[neibori[newmove],neiborj[newmove]]==5 & twolayer<0.5)
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
      if(numberofcho==1)
      {
        if(choi[chonumber,1]+choi[chonumber,3]==(2*choi[chonumber,2]) | choj[chonumber,1]+choj[chonumber,3]==(2*choj[chonumber,2]))
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
            
            if(newchoenergy<oldchoenergy)
            {
              lattice_cho1[neibori[newmove],neiborj[newmove]]=1
              lattice_cho1[choi[chonumber,1],choj[chonumber,1]]=5
              choi[chonumber,1]=neibori[newmove]
              choj[chonumber,1]=neiborj[newmove]
              successunmber=successunmber+1
            }
            else
            {
              if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
              {lattice_cho1[neibori[newmove],neiborj[newmove]]=1
              lattice_cho1[choi[chonumber,1],choj[chonumber,1]]=5
              choi[chonumber,1]=neibori[newmove]
              choj[chonumber,1]=neiborj[newmove]
              successunmber=successunmber+1}
              else
              {lattice_cho2[neibori[newmove],neiborj[newmove]]=5
              lattice_cho2[choi[chonumber,1],choj[chonumber,1]]=1}
            }
            
            
            
          }
          
        }
        
      }
      
      
      #尾部
      if(numberofcho==totallengthofcho)
      {
        if(choi[chonumber,totallengthofcho]+choi[chonumber,(totallengthofcho-2)]==(2*choi[chonumber,(totallengthofcho)]) | choj[chonumber,totallengthofcho]+choj[chonumber,(totallengthofcho-2)]==(2*choj[chonumber,(totallengthofcho-1)]))
        {
          if((neibori[newmove]!=choi[chonumber,totallengthofcho] & neiborj[newmove]!=choj[chonumber,totallengthofcho]) & (neibori[newmove]==choi[chonumber,(totallengthofcho-1)] | neiborj[newmove]==choj[chonumber,(totallengthofcho-1)]))
          {
            lattice_cho2[neibori[newmove],neiborj[newmove]]=3
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
            
            if(newchoenergy<oldchoenergy)
            {
              lattice_cho1[neibori[newmove],neiborj[newmove]]=3
              lattice_cho1[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=5
              choi[chonumber,totallengthofcho]=neibori[newmove]
              choj[chonumber,totallengthofcho]=neiborj[newmove]
              successunmber=successunmber+1
            }
            else
            {
              if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
              {lattice_cho1[neibori[newmove],neiborj[newmove]]=3
              lattice_cho1[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=5
              choi[chonumber,totallengthofcho]=neibori[newmove]
              choj[chonumber,totallengthofcho]=neiborj[newmove]
              successunmber=successunmber+1}
              else
              {lattice_cho2[neibori[newmove],neiborj[newmove]]=5
              lattice_cho2[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=3}
            }
            
            
            
          }
          
        }
        
      }
      
      
      #中部kink
      if(numberofcho>1 & numberofcho<totallengthofcho)
      {
        if(choi[chonumber,numberofcho+1]+choi[chonumber,(numberofcho-1)]!=(2*choi[chonumber,(numberofcho)]) | choj[chonumber,(numberofcho-1)]+choj[chonumber,(numberofcho+1)]!=(2*choj[chonumber,(numberofcho)]))
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
            
            if(newchoenergy<oldchoenergy)
            {
              lattice_cho1[neibori[newmove],neiborj[newmove]]=lattice_cho2[neibori[newmove],neiborj[newmove]]
              lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=5
              choi[chonumber,numberofcho]=neibori[newmove]
              choj[chonumber,numberofcho]=neiborj[newmove]
              successunmber=successunmber+1
            }
            else
            {
              if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
              {lattice_cho1[neibori[newmove],neiborj[newmove]]=lattice_cho2[neibori[newmove],neiborj[newmove]]
              lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=5
              choi[chonumber,numberofcho]=neibori[newmove]
              choj[chonumber,numberofcho]=neiborj[newmove]
              successunmber=successunmber+1}
              else
              {lattice_cho2[neibori[newmove],neiborj[newmove]]=5
              lattice_cho2[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]}
            }
            
          }
          
          
        }
        
        
        
        
        
        
      }
      
      # 中部 crankshaft
      
      if(numberofcho>3 & numberofcho<(totallengthofcho-2))
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
              
              
              if(newchoenergy<oldchoenergy)
              {
                lattice_cho1[icrankshaft,lattice_j]=lattice_cho2[icrankshaft,lattice_j]
                lattice_cho1[lattice_i,lattice_j]=5
                choi[chonumber,numberofcho]=icrankshaft
                
                
                lattice_cho1[icrankshaft,choj[chonumber,numberofcho-1]]=lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]
                lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]=5
                choi[chonumber,numberofcho-1]=icrankshaft
                
                successunmber=successunmber+1
                
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
                
                successunmber=successunmber+1}
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
              
              
              if(newchoenergy<oldchoenergy)
              {
                lattice_cho1[lattice_i,jcrankshaft]=lattice_cho2[lattice_i,jcrankshaft]
                lattice_cho1[lattice_i,lattice_j]=5
                choj[chonumber,numberofcho]=jcrankshaft
                
                
                lattice_cho1[choi[chonumber,numberofcho-1],jcrankshaft]=lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]
                lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]=5
                choj[chonumber,numberofcho-1]=jcrankshaft
                
                successunmber=successunmber+1
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
                
                successunmber=successunmber+1}
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
    
    #clusterindex[steps/moviestep]=0
    
    #for(x in 1:size)
    #{
    #for(y in 1:size)
    #{
    #for(x1 in 1:size)
    #{
    #for(y1 in 1:size)
    #{
    #clusterindex[steps/moviestep]=clusterindex[steps/moviestep]+lattice[x,y]*lattice[x1,y1]*((x-x1)^2+(y-y1)^2)
    #}
    #}
    #}
    #}
    
    
  }
  
}

heatmaplattice=heatmaplattice/countsteps

clusterindex=clusterindex/(size)^2

plot(seq(1:countsteps),clusterindex,type='p',lty=1,ylim=c(175000,190000),lwd=2,tck=0.03,las=1,,mgp=c(0,0.2,0),col="blue",cex=0.3,xlab='',ylab="")
mtext("t(10^4)",side=1,line=2,cex=1.0)
mtext("Index",side=2,line=3,cex=1.0)


par(mar = c(7,18,4,2),oma=c(0.2,0.2,0.2,0.2),mex=0.5)           # 设定边缘
image(x=1:nrow(heatmaplattice),y=1:ncol(heatmaplattice),z=heatmaplattice,axes=FALSE,xlab="",ylab="",col=heat.colors(16),main="")  


library(rgl)
library(gplots)
mycolors <- gray(0:900/1000)

heatmap(heatmaplattice, Rowv=NA, Colv=NA, col=cm.colors(256), revC=FALSE, scale='column')

par(mar = c(4,4,4,4))   

heatmap.2(heatmaplattice,col=mycolors,Rowv=F,Colv=F,margins=c(1,1),trace="none",cexCol=1,cexRow=1,xlab="",ylab="",cex.lab=4.0,keysize=1.0,key.title="",key.xlab="",key.ylab="",xaxt='n',yaxt='n')

dev.off()