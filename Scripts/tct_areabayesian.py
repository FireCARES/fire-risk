from __future__ import division
import pylab as pl
import numpy as np
import pandas as pd

class tctGibbs:
    #initialization
    # inputs: 
    def __init__(self,cro=76,cbu=20,cwo=4,talup=120,tallow=90,
                 tdisup=80,tdislow=40,tturup=100,tturlow=60,tarrup=420,
                 tarrlow=300,tsupup=180,tsuplow=60,arearoomlow=72,
                 arearoomhigh=380,areabldglow=1088,areabldghigh=9004,
                 upAo=1,lowAo=0.1,uptheta=0.009900,lowtheta=0.003564):
#0.009900 0.003564

        #observed bin values
        self.cro = cro #contained to room
        self.cbu = cbu #contained to building
        self.cwo = cwo #contained to world


        #specify constraints on the fixed parameters
        self.talup = talup # upper bound on t_alarm
        self.tallow = tallow # lower bound on t_alarm
        self.tdisup = tdisup # upper bound on t_dispatch
        self.tdislow = tdislow # lower bound on t_dispatch
        self.tturup = tturup # upper bound on t_turnout
        self.tturlow = tturlow # lower bound on t_turnout
        self.tarrup = tarrup # upper bound on t_arrival
        self.tarrlow = tarrlow # lower bound on t_arrival
        self.tsupup = tsupup # upper bound on t_suppression
        self.tsuplow = tsuplow # lower bound on t_suppression

        #specify floor area ranges(sq. ft)
        self.arearoomlow = arearoomlow
        self.arearoomhigh = arearoomhigh
        self.areabldglow = areabldglow
        self.areabldghigh = areabldghigh
        #specify original bounds on Ao and theta
        self.upAo = upAo
        self.lowAo = lowAo
        self.uptheta = uptheta
        self.lowtheta = lowtheta

        #calculate total number of fires
        self.n_fires=cro+cbu+cwo

        #instantiate initial draws for Ao and theta
        self.AoRoom= self.upAo#np.random.uniform(lowAo,upAo)
        self.AoBldg= self.upAo#np.random.uniform(lowAo,upAo)
        self.AoBeyond= self.upAo#np.random.uniform(lowAo,upAo)
        self.thetaRoom= np.mean([self.uptheta,self.lowtheta])#np.random.uniform(lowtheta,uptheta)
        self.thetaBldg= np.mean([self.uptheta,self.lowtheta])#np.random.uniform(lowtheta,uptheta)
        self.thetaBeyond= np.mean([self.uptheta,self.lowtheta])#np.random.uniform(lowtheta,uptheta)
        #instantiate variables for tcor fires
        self.tcorRoom = 0
        self.tcorBldg = 0
        self.tcorBeyond = 0

        #create initial containers for all of the task time variables
        self.tal = np.random.uniform(tallow,talup) # upper bound on t_alarm
        self.tdis = np.random.uniform(tdislow,tdisup) # upper bound on t_dispatch
        self.ttur = np.random.uniform(tturlow,tturup) # upper bound on t_turnout
        self.tarr = np.random.uniform(tarrlow,tarrup)# upper bound on t_arrival
        self.tsup = np.random.uniform(tsuplow,tsupup) # upper bound on t_suppression
        self.tfiretasks = self.tal+self.tdis+self.ttur+self.tarr+self.tsup

    #Create draw functions for the Gibbs sampler 
    #Draw new values for fire department timing
    def draw_tfiretasks(self):
        self.tal = np.random.uniform(self.tallow,self.talup) # upper bound on t_alarm
        self.tdis = np.random.uniform(self.tdislow,self.tdisup) # upper bound on t_dispatch
        self.ttur = np.random.uniform(self.tturlow,self.tturup) # upper bound on t_turnout
        self.tarr = np.random.uniform(self.tarrlow,self.tarrup)# upper bound on t_arrival
        self.tsup = np.random.uniform(self.tsuplow,self.tsupup) # upper bound on t_suppression
        self.tfiretasks = self.tal+self.tdis+self.ttur+self.tarr+self.tsup
    #Draw the tcor values for relevant fires
    #Inputs: relevant Amin and Amax thresholds and current Ao and theta values 
    def draw_tcor(self,Amin,Amax,Ao,theta):
        lowtcor = (np.log(Amin)-np.log(Ao))/theta-self.tfiretasks
        uptcor = (np.log(Amax)-np.log(Ao))/theta-self.tfiretasks
        return np.random.uniform(lowtcor,uptcor)
    #Draw the Ao values for relevant fires
    #Inputs: relevant Amin and Amax thresholds and current tcor and theta values 
    def draw_Ao(self,Amin,Amax,tcor,theta):
        #return np.random.uniform(max(lowAo,self.lowAo),min(upAo,self.upAo))
        #return np.random.uniform(lowAo,upAo)
        return self.upAo
    #Draw the tcor values for room fires
    #Inputs: relevant Amin and Amax thresholds and current tcor and Ao values 
    def draw_theta(self,Amin,Amax,tcor,Ao):
        #return np.random.uniform(max(self.lowtheta,lowtheta),min(self.uptheta,uptheta))
        #return np.random.uniform(self.lowtheta,self.uptheta)
        return np.mean([self.uptheta,self.lowtheta])
    def draw_Abounds(self,Aboundmin,Aboundmax):
        return np.random.uniform(Aboundmin,Aboundmax)
        
    
    #Gibbs sampling function
    def fireGibbs(self,n_iter,burn,thin,Aminboundmin,Aminboundmax,Amaxboundmin,Amaxboundmax,tcor,Ao,theta):
        print 'fireGibbs called'
        n_store = int(np.ceil((n_iter-burn))/thin+0.00001)
        gibbstcor = np.full(n_store,-1)
        gibbsAo = np.full(n_store,-1)
        gibbstheta = np.full(n_store,-1)
        s = 0
        for i in range(0,n_iter):
            self.draw_tfiretasks()
            if(Aminboundmin == Amaxboundmin and Aminboundmax == Amaxboundmax):
                Aminbound = self.draw_Abounds(Aminboundmin,Aminboundmax)
                Amaxbound = Aminbound
            else:
                Aminbound = self.draw_Abounds(Aminboundmin,Aminboundmax)
                Amaxbound = self.draw_Abounds(Amaxboundmin,Amaxboundmax)
            Ao = self.draw_Ao(Aminbound,Amaxbound,tcor,theta)
            theta = self.draw_theta(Aminbound,Amaxbound,tcor,Ao)
            tcor = self.draw_tcor(Aminbound,Amaxbound,Ao,theta)
            if(i >= burn and i%thin==0):
                gibbstcor[s] = tcor
                gibbsAo[s] = Ao
                gibbstheta[s] = theta
                s = s+1
        return(gibbstcor,gibbsAo,gibbstheta)
    #output storage function
    def gibbs_store(self,gibbsoutputlist,filenameoutputlist):
        for i in range(0,len(gibbsoutputlist)):
            f=open('../RawOutput/'+filenameoutputlist[i],'wb')
            np.savetxt(f,gibbsoutputlist[i],delimiter=',')
            f.close()


    #Main class running function
    def runGibbs(self,n_iter=1000,burn=500,thin=5):
        #Run room fires first and output
        gibbstcor,gibbsAo,gibbstheta = self.fireGibbs(n_iter,burn,thin,self.upAo,
                                               self.upAo,self.arearoomlow,self.arearoomhigh,
                                               self.tcorRoom,self.AoRoom,self.thetaRoom)
        #store output
        self.gibbs_store([gibbstcor,gibbsAo,gibbstheta],['tcorRoom.csv',
                      'AoRoom.csv','thetaRoom.csv'])
         #Run building fires next and output
        gibbstcor,gibbsAo,gibbstheta = self.fireGibbs(n_iter,burn,thin,self.arearoomlow,
                                               self.arearoomhigh,self.areabldglow,
                                               self.areabldghigh,self.tcorBldg,
                                               self.AoBldg,self.thetaBldg)
        #store output
        self.gibbs_store([gibbstcor,gibbsAo,gibbstheta],['tcorBldg.csv',
                      'AoBldg.csv','thetaBldg.csv'])
         #Run beyond building fires last and output
        gibbstcor,gibbsAo,gibbstheta = self.fireGibbs(n_iter,burn,thin,self.areabldglow,
                                               self.areabldghigh,self.areabldglow,
                                               self.areabldghigh,self.tcorBeyond,
                                               self.AoBeyond,self.thetaBeyond)
        #store output
        self.gibbs_store([gibbstcor,gibbsAo,gibbstheta],['tcorBeyond.csv',
                      'AoBeyond.csv','thetaBeyond.csv'])



test = tctGibbs()
test.runGibbs(10000,0,1)
