dev.off()
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
  
  for(i in 1:size)
  {
    for(j in 1:size)
    {
      par(new=T)
      if(lattice_cho1[i,j]==1)
        polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size-i+0.2,size-i+0.2,size-i+1-0.2,size-i+1-0.2), density = NULL, border = T, col = 'black')
      if(lattice_cho1[i,j]==2)
        polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size-i+0.2,size-i+0.2,size-i+1-0.2,size-i+1-0.2), density = NULL, border = T, col = 'blue')
      if(lattice_cho1[i,j]==3)
        polygon(c(j-1+0.2,j-0.2,j-0.2,j-1+0.2),c(size-i+0.2,size-i+0.2,size-i+1-0.2,size-i+1-0.2), density = NULL, border = T, col = 'grey')
    }
  }
  

  
  #simulation
  Sys.time()
  successunmber=0
  totalsteps=5000000
  for(steps in 1:totalsteps)
  {
    randomnumcho=sample(1:totalcho,size=1)
    randomlengthcho=sample(1:totallengthofcho,size=1)
    lattice_i=choi[randomnumcho,randomlengthcho]
    lattice_j=choj[randomnumcho,randomlengthcho]
    neiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
    neibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
    newmove=sample(1:8,size=1)
    

    
    if(neiborj[newmove]>0 & neiborj[newmove]<(size+1) & neibori[newmove]>0 & neibori[newmove]<(size+1))
    {
      
      
      
      # 动cho
      if(lattice_cho1[lattice_i,lattice_j]!=5 & lattice_cho1[neibori[newmove],neiborj[newmove]]==5)
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
                  oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],1]
              }
              
              newchoenergy=0
              newneibori=c(neibori[newmove]-1,neibori[newmove],neibori[newmove]+1,neibori[newmove]+1,neibori[newmove]+1,neibori[newmove],neibori[newmove]-1,neibori[newmove]-1)
              newneiborj=c(neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove],neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove])
              for(newneibor in 1:8)
              {
                if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                  newchoenergy=newchoenergy
                else
                  newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],1]
              }
              
              if(newchoenergy<oldchoenergy)
              {
                lattice_cho1[neibori[newmove],neiborj[newmove]]=1
                lattice_cho1[choi[chonumber,1],choj[chonumber,1]]=5
                choi[chonumber,1]=neibori[newmove]
                choj[chonumber,1]=neiborj[newmove]
              }
              else
              {
                if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                {lattice_cho1[neibori[newmove],neiborj[newmove]]=1
                lattice_cho1[choi[chonumber,1],choj[chonumber,1]]=5
                choi[chonumber,1]=neibori[newmove]
                choj[chonumber,1]=neiborj[newmove]}
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
                  oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],3]
              }
              
              newchoenergy=0
              newneibori=c(neibori[newmove]-1,neibori[newmove],neibori[newmove]+1,neibori[newmove]+1,neibori[newmove]+1,neibori[newmove],neibori[newmove]-1,neibori[newmove]-1)
              newneiborj=c(neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove],neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove])
              for(newneibor in 1:8)
              {
                if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                  newchoenergy=newchoenergy
                else
                  newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],3]
              }
              
              if(newchoenergy<oldchoenergy)
              {
                lattice_cho1[neibori[newmove],neiborj[newmove]]=3
                lattice_cho1[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=5
                choi[chonumber,totallengthofcho]=neibori[newmove]
                choj[chonumber,totallengthofcho]=neiborj[newmove]
              }
              else
              {
                if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                {lattice_cho1[neibori[newmove],neiborj[newmove]]=3
                lattice_cho1[choi[chonumber,totallengthofcho],choj[chonumber,totallengthofcho]]=5
                choi[chonumber,totallengthofcho]=neibori[newmove]
                choj[chonumber,totallengthofcho]=neiborj[newmove]}
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
                  oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]
              }
              
              newchoenergy=0
              newneibori=c(neibori[newmove]-1,neibori[newmove],neibori[newmove]+1,neibori[newmove]+1,neibori[newmove]+1,neibori[newmove],neibori[newmove]-1,neibori[newmove]-1)
              newneiborj=c(neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove]-1,neiborj[newmove],neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove]+1,neiborj[newmove])
              for(newneibor in 1:8)
              {
                if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                  newchoenergy=newchoenergy
                else
                  newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho1[lattice_i,lattice_j]]
              }
              
              if(newchoenergy<oldchoenergy)
              {
                lattice_cho1[neibori[newmove],neiborj[newmove]]=lattice_cho2[neibori[newmove],neiborj[newmove]]
                lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=5
                choi[chonumber,numberofcho]=neibori[newmove]
                choj[chonumber,numberofcho]=neiborj[newmove]
              }
              else
              {
                if(runif(1)<kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                {lattice_cho1[neibori[newmove],neiborj[newmove]]=lattice_cho2[neibori[newmove],neiborj[newmove]]
                lattice_cho1[choi[chonumber,numberofcho],choj[chonumber,numberofcho]]=5
                choi[chonumber,numberofcho]=neibori[newmove]
                choj[chonumber,numberofcho]=neiborj[newmove]}
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
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]
                }
                
                oldneibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
                oldneiborj=c(choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1],choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1])
                
                for(oldneibor in 1:8)
                {
                  if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                    oldchoenergy=oldchoenergy
                  else
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]]
                }
                
                newchoenergy=0
                
                newneibori=c(icrankshaft-1,icrankshaft,icrankshaft+1,icrankshaft+1,icrankshaft+1,icrankshaft,icrankshaft-1,icrankshaft-1)
                newneiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[icrankshaft,lattice_j]]
                }
                
                newneibori=c(icrankshaft-1,icrankshaft,icrankshaft+1,icrankshaft+1,icrankshaft+1,icrankshaft,icrankshaft-1,icrankshaft-1)
                newneiborj=c(choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1]-1,choj[chonumber,numberofcho-1],choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1]+1,choj[chonumber,numberofcho-1])
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]]
                }
                
                
                if(newchoenergy<oldchoenergy)
                {
                  lattice_cho1[icrankshaft,lattice_j]=lattice_cho2[icrankshaft,lattice_j]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choi[chonumber,numberofcho]=icrankshaft
                  
                  
                  lattice_cho1[icrankshaft,choj[chonumber,numberofcho-1]]=lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]
                  lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]=5
                  choi[chonumber,numberofcho-1]=icrankshaft
                  
                }
                else
                {
                  if(runif(1)<0.25*kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                  {lattice_cho1[icrankshaft,lattice_j]=lattice_cho2[icrankshaft,lattice_j]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choi[chonumber,numberofcho]=icrankshaft
                  
                  
                  lattice_cho1[icrankshaft,choj[chonumber,numberofcho-1]]=lattice_cho2[icrankshaft,choj[chonumber,numberofcho-1]]
                  lattice_cho1[lattice_i,choj[chonumber,numberofcho-1]]=5
                  choi[chonumber,numberofcho-1]=icrankshaft}
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
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[lattice_i,lattice_j]]
                }
                
                oldneibori=c(choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1]-1)
                oldneiborj=c(lattice_j-1,lattice_j-1,lattice_j-1,lattice_j,lattice_j+1,lattice_j+1,lattice_j+1,lattice_j)
                
                for(oldneibor in 1:8)
                {
                  if(oldneibori[oldneibor]<1 | oldneibori[oldneibor]>size | oldneiborj[oldneibor]<1 | oldneiborj[oldneibor]>size)
                    oldchoenergy=oldchoenergy
                  else
                    oldchoenergy=oldchoenergy+coefficientmatrix[lattice_cho1[oldneibori[oldneibor],oldneiborj[oldneibor]],lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]]
                }
                
                newchoenergy=0
                
                newneibori=c(lattice_i-1,lattice_i,lattice_i+1,lattice_i+1,lattice_i+1,lattice_i,lattice_i-1,lattice_i-1)
                newneiborj=c(jcrankshaft-1,jcrankshaft-1,jcrankshaft-1,jcrankshaft,jcrankshaft+1,jcrankshaft+1,jcrankshaft+1,jcrankshaft)
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[lattice_i,jcrankshaft]]
                }
                
                newneibori=c(choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1]+1,choi[chonumber,numberofcho-1],choi[chonumber,numberofcho-1]-1,choi[chonumber,numberofcho-1]-1)
                newneiborj=c(jcrankshaft-1,jcrankshaft-1,jcrankshaft-1,jcrankshaft,jcrankshaft+1,jcrankshaft+1,jcrankshaft+1,jcrankshaft)
                
                for(newneibor in 1:8)
                {
                  if(newneiborj[newneibor]<1 | newneiborj[newneibor]>size | newneibori[newneibor]<1 | newneibori[newneibor]>size)
                    newchoenergy=newchoenergy
                  else
                    newchoenergy=newchoenergy+coefficientmatrix[lattice_cho2[newneibori[newneibor],newneiborj[newneibor]],lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]]
                }
                
                
                if(newchoenergy<oldchoenergy)
                {
                  lattice_cho1[lattice_i,jcrankshaft]=lattice_cho2[lattice_i,jcrankshaft]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choj[chonumber,numberofcho]=jcrankshaft
                  
                  
                  lattice_cho1[choi[chonumber,numberofcho-1],jcrankshaft]=lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]
                  lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]=5
                  choj[chonumber,numberofcho-1]=jcrankshaft
                  
                }
                else
                {
                  if(runif(1)<0.25*kp*exp(-(newchoenergy-oldchoenergy))/(exp(-(oldenergy-newenergy))))
                  {lattice_cho1[lattice_i,jcrankshaft]=lattice_cho2[lattice_i,jcrankshaft]
                  lattice_cho1[lattice_i,lattice_j]=5
                  choj[chonumber,numberofcho]=jcrankshaft
                  
                  
                  lattice_cho1[choi[chonumber,numberofcho-1],jcrankshaft]=lattice_cho2[choi[chonumber,numberofcho-1],jcrankshaft]
                  lattice_cho1[choi[chonumber,numberofcho-1],lattice_j]=5
                  choj[chonumber,numberofcho-1]=jcrankshaft}
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
    if(as.integer(steps/1000000)==(steps/1000000))
      print(steps)
    
  }
  
  
  dev.off()
  par(mar=c(1,1,1,1))
  plot(1,1,type="p",tck=0.03,cex=0.5,las=1,xlab="",col='white',pch=19, ylab="", main="",xlim=c(0,size),ylim=c(0,size),xaxt="n",yaxt="n",bty="n")
  for(i in 1:size)
  {
    for(j in 1:size)
    {
      par(new=T)
      if(lattice_1[i,j]==4)
        polygon(c(j-1,j,j,j-1),c(size-i,size-i,size-i+1,size-i+1), density = NULL, border = F, col = 'white')
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
  
  
  length(which(lattice_cho1==1))
  length(which(lattice_cho1==2))
  length(which(lattice_cho1==3))
  length(which(lattice_1==4))
  