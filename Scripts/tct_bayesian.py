from __future__ import division
import pylab as pl
import numpy as np
import pandas as pd

class tctGibbs:
    #initialization
    # inputs: 
    def __init__(self,cro=76,cbu=20,cwo=4,talup=120,tallow=90,
                 tdisup=80,tdislow=40,tturup=100,tturlow=60,tarrup=420,
                 tarrlow=300,tsupup=180,tsuplow=60,sizemin=100,sizeroom=2000,
                 sizebldg=10000,sizemax=3000000,
                 upA=0.047,lowA=0.0029,upalph=2,lowalph=1):


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

        #specify expected fire sizes for spread behavior (kW)
        self.sizemin = sizemin #natively set to 0.01 to avoid numerical singularities on lower bound
        self.sizeroom = sizeroom #threshold for binning a fire into contained to room
        self.sizebldg = sizebldg #threshold on binning a fire into contained to building
        self.sizemax = sizemax #reasonable physical threshold for a structure fire
                                  #Note: value of 3,000,000 taken from FDS prediction
                                  #of peak HRR for WTC 1 fire on 9/11, from NCSTAR 1-5
                                  #Figure 6-30
        #specify original bounds on A and alpha
        self.upA = upA
        self.lowA = lowA
        self.upalph = upalph
        self.lowalph = lowalph

        #calculate total number of fires
        self.n_fires=cro+cbu+cwo

        #instantiate initial draws for A and alpha
        self.ARoom=np.random.uniform(lowA,upA)
        self.ABldg=np.random.uniform(lowA,upA)
        self.ABeyond=np.random.uniform(lowA,upA)
        self.alphRoom=np.random.uniform(lowalph,upalph)
        self.alphBldg=np.random.uniform(lowalph,upalph)
        self.alphBeyond=np.random.uniform(lowalph,upalph)
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
    #Inputs: relevant Qmin and Qmax thresholds and current A and alph values 
    def draw_tcor(self,Qmin,Qmax,A,alph):
        lowtcor = (Qmin/A)**(1/alph)-self.tfiretasks
        uptcor = (Qmax/A)**(1/alph)-self.tfiretasks
        return np.random.uniform(lowtcor,uptcor)
    #Draw the A values for relevant fires
    #Inputs: relevant Qmin and Qmax thresholds and current tcor and alph values 
    def draw_A(self,Qmin,Qmax,tcor,alph):
        lowA = (Qmin)/(max(tcor+self.tfiretasks,0.0001)**(alph))
        upA = min((Qmax)/(max(tcor+self.tfiretasks,0.0001)**(alph)),
                  Qmin/self.tfiretasks**2)
        #return np.random.uniform(max(lowA,self.lowA),min(upA,self.upA))
        return np.random.uniform(lowA,upA)
        
    #Draw the tcor values for room fires
    #Inputs: relevant Qmin and Qmax thresholds and current tcor and A values 
    def draw_alph(self,Qmin,Qmax,tcor,A):
        lowalph = (pl.log(Qmin)-pl.log(A))/pl.log(max(tcor+self.tfiretasks,0.0001))
        upalph = (pl.log(Qmax)-pl.log(A))/pl.log(max(tcor+self.tfiretasks,0.0001))
        if(upalph < self.lowalph):
            upalph = self.lowalph
        #return np.random.uniform(max(self.lowalph,lowalph),min(self.upalph,upalph))
        #return np.random.uniform(self.lowalph,self.upalph)
        return self.upalph
        
    
    #Gibbs sampling function
    def fireGibbs(self,n_iter,burn,thin,Qmin,Qmax,tcor,A,alph):
        print 'fireGibbs called'
        n_store = int(np.ceil((n_iter-burn))/thin+0.00001)
        gibbstcor = np.full(n_store,-1)
        gibbsA = np.full(n_store,-1)
        gibbsalph = np.full(n_store,-1)
        s = 0
        for i in range(0,n_iter):
            self.draw_tfiretasks()
            A = self.draw_A(Qmin,Qmax,tcor,alph)
            tcor = self.draw_tcor(Qmin,Qmax,A,alph)
            alph = self.draw_alph(Qmin,Qmax,tcor,A)
            if(i >= burn and i%thin==0):
                gibbstcor[s] = tcor
                gibbsA[s] = A
                gibbsalph[s] = alph
                s = s+1
        return(gibbstcor,gibbsA,gibbsalph)
    #output storage function
    def gibbs_store(self,gibbsoutputlist,filenameoutputlist):
        for i in range(0,len(gibbsoutputlist)):
            f=open('../RawOutput/'+filenameoutputlist[i],'wb')
            np.savetxt(f,gibbsoutputlist[i],delimiter=',')
            f.close()


    #Main class running function
    def runGibbs(self,n_iter=1000,burn=500,thin=5):
        #Run room fires first and output
        gibbstcor,gibbsA,gibbsalph = self.fireGibbs(n_iter,burn,thin,self.sizemin,
                                               self.sizeroom,self.tcorRoom,
                                               self.ARoom,self.alphRoom)
        #store output
        self.gibbs_store([gibbstcor,gibbsA,gibbsalph],['tcorRoom.csv',
                      'ARoom.csv','alphRoom.csv'])
         #Run building fires next and output
        gibbstcor,gibbsA,gibbsalph = self.fireGibbs(n_iter,burn,thin,self.sizeroom,
                                               self.sizebldg,self.tcorBldg,
                                               self.ABldg,self.alphBldg)
        #store output
        self.gibbs_store([gibbstcor,gibbsA,gibbsalph],['tcorBldg.csv',
                      'ABldg.csv','alphBldg.csv'])
         #Run beyond building fires last and output
        gibbstcor,gibbsA,gibbsalph = self.fireGibbs(n_iter,burn,thin,self.sizebldg,
                                               self.sizemax,self.tcorBeyond,
                                               self.ABeyond,self.alphBeyond)
        #store output
        self.gibbs_store([gibbstcor,gibbsA,gibbsalph],['tcorBeyond.csv',
                      'ABeyond.csv','alphBeyond.csv'])



test = tctGibbs()
test.runGibbs(100000,100,10)
