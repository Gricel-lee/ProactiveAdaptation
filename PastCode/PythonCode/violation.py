import matplotlib.pyplot as plt
import pandas as pd

# Functions
p1_f = lambda M1   : M1*0.0381941 + 0.962500
p2_f = lambda M1,M3: 0.65301 + 0.05833 * M1 - 0.11750 * M3 - 0.04865 * M1 * M3       # before only M3
p3_f = lambda M1,M3: 0.16218 - 0.04194 * M1 - 0.01444 * M3 - 0.04167 * M1 * M3       # before only M1
p4_f = lambda M3   : 0.80629 + 0.21175 * M3
pR_f = lambda x : 0.6
T1_f = lambda M1,M2: 134.171 - 11.929 * M1 - 7.999 * M2     
T2_f = lambda M3 : 25.3942 - 5.0085 * M3
T3_f = lambda M2: 53.7                          
T1F_f= lambda M2: 349.772 - 10.97 * M2                                               # fixed before
T2F_f= lambda x :59.1 
TR_f = lambda M1,M3: 52.8953 - (0.2339 *M1) + (8.5904 *M3) + (15.6654 * M1 * M3)     # before M


#-------------------------------- In Violation 
class Violation():
    '''Check if in violation'''
    #==================================== 
    # Init
    def __init__(self,hist_file,R1_bound,R2_bound,R3_bound, minM1=0.5, maxM1= 1, minM2=0.25, maxM2=0.5, minM3=0.5, maxM3=1):
        self.R1_bound=R1_bound; self.R2_bound=R2_bound; self.R3_bound=R3_bound
        self.minM1=minM1; self.maxM1= maxM1
        self.minM2=minM2; self.maxM2=maxM2
        self.minM3=minM3; self.maxM3=maxM3
        # get day run into df
        df_day= self.read_Day_csv(hist_file)

        # get df with violations columns
        self.df = self.add_violation_cols(df_day)


    #====================================
    def checkViolation(self,p1,p2,p3,p4,T1,T1F,T2,T2F,TR,T3,Pretry):
        violation = 0
        r1,r2,r3 =0,0,0
        # as functions require !=0
        if (1-Pretry*p3)==0:
            return [R1,R2,R3,1,1,1,1] #return violation
        # compute R1-R3
        R1= (p1*p2*p4)/(1-Pretry*p3)
        R2= -1*(p1*(p3*T2-p2*T3-p3*T2F-T1-T2+T1F)+Pretry*p3*(T1F-p1*TR-p1*T1F)-T1F) / (1-Pretry*p3)
        R3 = (p1*p2*(1-p4))/(1-Pretry*p3)
        # check violation   (i.e., does not comply with innequalities, hence innequality symbols are flipped)
        if R1 <= self.R1_bound:  violation = 1; r1=1
        if R2 >= self.R2_bound:  violation = 1; r2=1
        if R3 >= self.R3_bound:  violation = 1; r3=1
        return [R1,R2,R3,r1,r2,r3,violation]
    
    def getEnvVar(self,M1,M2,M3,x):
        '''Env. variables'''
        p1=p1_f(M1);
        p2=p2_f(M1,M3);
        p3=p3_f(M1,M3);
        p4=p4_f(M3);
        pR=pR_f(x);
        T1=T1_f(M1,M2);
        T2=T2_f(x);
        T3=T3_f(M2);
        T1F=T1F_f(M2);
        T2F=T2F_f(x);
        TR=TR_f(M1,M3);
        return p1,p2,p3,p4,T1,T1F,T2,T2F,TR,T3,pR
    
    def check_out_Bounds(self,M1,M2,M3):
        '''Check if out of bounds. Return code representing which bound is violated or 0.'''
        if M1>self.maxM1: return -1    # max M1 violated
        elif M1<self.minM1: return -11 # min M1 violated
        elif M2>self.maxM2: return -2  #...
        elif M2<self.minM2: return -22
        elif M3>self.maxM3: return -3
        elif M3<self.minM3: return -33
        else: return 0

    def add_violation_cols(self,df,save=True):
        '''Add violation columns for a dataframe with environmental values M1,M2,M3'''
        for index,row in df.iterrows():
            M1 = row['M1']; M2= row['M2']; M3 = row['M3']
            x = row['M3'] #dummy
            p1,p2,p3,p4,T1,T1F,T2,T2F,TR,T3,pR = self.getEnvVar(M1,M2,M3,x)
            # check bounds M1,M2,M3
            b = self.check_out_Bounds(M1,M2,M3)
            if b<0:
                # if outside bounds box values of M1, M2 or M3
                [R1,R2,R3,r1,r2,r3,violation] = [b,b,b,b,b,b,b] #return code of bound surpassed
            else:    
                #check violation
                [R1,R2,R3,r1,r2,r3,violation] = self.checkViolation(p1,p2,p3,p4,T1,T1F,T2,T2F,TR,T3,pR) #return violation
            #add to df
            df.loc[index,'p1'],df.loc[index,'p2'], df.loc[index,'p3'],
            df.loc[index,'p4'], df.loc[index,'T1'],df.loc[index,'T1F'],
            df.loc[index,'T2'],df.loc[index,'T2F'],df.loc[index,'TR'],
            df.loc[index,'T3'], df.loc[index,'Pretry']= p1,p2,p3,p4,T1,T1F,T2,T2F,TR,T3,pR
            df.loc[index,'R1'],df.loc[index,'R2'],df.loc[index,'R3'] = R1,R2,R3
            df.loc[index,'r1'],df.loc[index,'r2'],df.loc[index,'r3'] = r1,r2,r3
            df.loc[index,'V'] = violation
        if save:
            df.to_csv('_violation.csv', index=False)
        df.head(20)
        return df
    

    def print_results_violation(self):
        df = self.df
        '''Info of num of violations'''
        total_violation = df['V'].sum()
        total = len(df['V'])
        print(" Total data points: {}.\n Total data points in violation: {}.\n Percentage in violation: {}%."
            .format(total,total_violation,total_violation/total*100))
        
    
    def plot_M_violation(self,df,title):
        fig, ax1 = plt.subplots()
        #
        ax1.plot(df["Time"], df["M1"], color='b', label='M1')
        ax1.set_xlabel('Time'); ax1.set_ylabel('M1', color='b'); ax1.tick_params('y', colors='b')
        #
        ax2 = ax1.twinx()
        ax2.plot(df["Time"], df["M2"], color='r', label='M2'); ax2.set_ylabel('M2', color='r'); ax2.tick_params('y', colors='r')
        #
        ax3 = ax1.twinx();ax3.spines['right'].set_position(('outward', 60))  # Adjust the position of the third y-axis
        ax3.plot(df["Time"], df["M3"], color='g', label='M3'); ax3.set_ylabel('M3', color='g'); ax3.tick_params('y', colors='g')
        #
        ax4 = ax1.twinx();ax4.spines['right'].set_position(('outward', 110))  # Adjust the position of the third y-axis
        ax4.plot(df["Time"], df["V"], color='y', label='M1'); ax4.set_ylabel('V', color='y'); ax4.tick_params('y', colors='y')
        # Adding legend
        lines, labels = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        lines3, labels3 = ax3.get_legend_handles_labels()
        lines4, labels4 = ax4.get_legend_handles_labels()
        ax3.legend(lines + lines2 + lines3+ lines4, labels + labels2 + labels3+ labels4, loc='upper right')
        #
        plt.title(title)
        plt.show()


    def read_Day_csv(self,hist_file):
        dfM = pd.read_csv(hist_file);  dfM.columns=["Time","M1","M2","M3","state"]
        return dfM