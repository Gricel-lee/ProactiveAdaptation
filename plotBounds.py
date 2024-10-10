import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def print_measurements_limits():
    df = pd.read_csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv", header=None)
    historicmeans = pd.read_csv("/Users/grisv/GitHub/Manifest/1historicmeans.csv")["x"]
    lims = pd.read_csv("/Users/grisv/GitHub/Manifest/1lims.csv")["x"]
    
    print(df.head())

    df.columns = ['time', 'm1', 'm2', 'm3','state','NA']

    # ------- Create a nxm grid of subplots
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(4.5, 3.5), gridspec_kw={'height_ratios': [0.8, 0.8, 0.8]}, layout='constrained')  # (width, height) in inches

    ax1.set_ylabel("light")
    ax2.set_ylabel("gripper")
    ax3.set_ylabel("floor")
    plt.xlabel("time (s)")
    
    # # No x/y scale
    ax1.xaxis.set_ticks([]);
    ax2.xaxis.set_ticks([]);


    # plot data
    p1 = ax1.plot(df['time'], df['m1'],      color='k', label="Light", linewidth=1)
    p2 = ax2.plot(df['time'], df['m2'],      color='k', label="Light", linewidth=1)
    p3 = ax3.plot(df['time'], df['m3'],      color='k', label="Light", linewidth=1)

    # ------- Measurement limits
    # x-axis (time)
    xline = [min(df['time']), max(df['time'])]

    # limits
    p=[p1,p2,p3]
    ax=[ax1,ax2,ax3]
    for i in range(0,3):
        # limits
        yline = [historicmeans[i]+lims[i], historicmeans[i]+lims[i]]
        p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        yline = [historicmeans[i]-lims[i], historicmeans[i]-lims[i]]
        p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        # mean
        yline = [historicmeans[i], historicmeans[i]]
        p[i] = ax[i].plot(xline,yline,color='g',linestyle='-',linewidth=1)
        


    plt.show()






def print_trends_limits():
    df = pd.read_csv("/Users/grisv/GitHub/Manifest/2change.csv")
    lims = pd.read_csv("/Users/grisv/GitHub/Manifest/2lims.csv")["x"]
    
    df.columns = ['trend1', 'trend2', 'trend3']
    
    # add time as a column
    df_time = pd.read_csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv", header=None)
    df['time'] = df_time[0]
    print(df.head())

    # ------- Create a nxm grid of subplots
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(4.5, 3.5), gridspec_kw={'height_ratios': [0.8, 0.8, 0.8]}, layout='constrained')  # (width, height) in inches

    ax1.set_ylabel("light\ntrend\nchange")
    ax2.set_ylabel("gripper\ntrend\nchange")
    ax3.set_ylabel("floor\ntrend\nchange")
    plt.xlabel("time (s)")

    # # No x/y scale
    ax1.xaxis.set_ticks([]);
    ax2.xaxis.set_ticks([]);

    # plot data
    p1 = ax1.plot(df['time'], df['trend1'],      color='k', label="Light", linewidth=1)
    p2 = ax2.plot(df['time'], df['trend2'],      color='k', label="Light", linewidth=1)
    p3 = ax3.plot(df['time'], df['trend3'],      color='k', label="Light", linewidth=1)

    # ------- Measurement limits
    # x-axis (time)
    xline = [min(df['time']), max(df['time'])]

    # limits
    p=[p1,p2,p3]
    ax=[ax1,ax2,ax3]
    for i in range(0,3):
        # limits
        yline = [lims[i], lims[i]]
        p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        yline = [lims[i], lims[i]]
        p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        yline = [-1*lims[i], -1*lims[i]]
        p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        yline = [-1*lims[i], -1*lims[i]]
        p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        
        # mean (NO MEAN FOR TRENDS)
        # yline = [historicmeans[i]+lims[i], historicmeans[i]+lims[i]]
        # p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        # yline = [historicmeans[i]-lims[i], historicmeans[i]-lims[i]]
        # p[i] = ax[i].plot(xline,yline,color='b',linestyle='--',linewidth=1)
        
        #yline = [historicmeans[i], historicmeans[i]]
        #p[i] = ax[i].plot(xline,yline,color='g',linestyle='-',linewidth=1)
    
    plt.show()


print_measurements_limits()

#print_trends_limits()
