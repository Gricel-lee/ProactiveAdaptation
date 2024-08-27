import matplotlib.pyplot as plt
import pandas as pd
plt.close('all') 


'''-1 in violation, 1 safe'''
def in_violation(time,M1, M2, M3):
    R1 = 0.5
    R2 = 100
    R3 = 0.35

    P1 = 0.2018 + (0.8191 * M1)
    P2 = 0.414 + (0.4612 * M1)
    P3 = 0.1
    P4 = -0.1618 + (1.2523 * M3)
    PR = 0.8
    T1 = 0
    T2 = 19.765 + (15.627 * M1)
    T3 = 63.15
    T1F = 0
    T2F = 7.343 + (72.234 * M1) + (161.485 * M2) - (231.337 * M1 * M2)
    TR = 6.498 - (5.482 * M1) - (6.855 * M3) + (8.301 * M1 * M3)
    
    Rfail=0

    # start safe
    violation = 1
    r1 = (P1 * P2 * P4) / (1 - PR * P3)
    r2 = (P1 * (P3 * T2 - P2 * T3 - P3 * T2F - T1 - T2 + T1F) + PR * P3 * (T1F - P1 * TR - P1 * T1F) - T1F) / (PR * P3 - 1)
    r3 = (P1 * P2 * (1 - P4)) / (1 - PR * P3)
    #print("violation1 ", violation)
    if (1 - PR * P3) == 0:
        violation = -1
        #input("Equal 0. Press Enter to continue...")

    else:
        if r1 < R1:
            violation = -1
            Rfail = 1
        if r2 > R2:
            violation = -1
            Rfail = 2
        if r3 > R3:
            violation = -1
            Rfail = 3
    
    #if violation == -1:
        #input("Press Enter to continue...")
        #print(time,M1,M2,M3, r1, r2, r3, Rfail)
    return violation, Rfail






# Create a 3x1 grid of subplots
fig, (aax1, aax3, aax4) = plt.subplots(3, 1, figsize=(10, 4), gridspec_kw={'height_ratios': [4, 1, 1]}, layout='constrained')  # (width, height) in inches


# --------- First subplot
host = aax1
ax2 = host.twinx()
ax3 = host.twinx()
    
# host.set_xlim(0, 2)
host.set_ylim(0.3, 1)
ax2.set_ylim(0.0, 0.6)
ax3.set_ylim(0.3, 1)

host.set_xlabel("Time")
host.set_ylabel("Light")
ax2.set_ylabel("Floor friction")
ax3.set_ylabel("Gripper friction")
color1, color2, color3 = plt.cm.viridis([0, .6, .3])

# Read the CSV file
df = pd.read_csv('/Users/grisv/GitHub/Manifest/outputlg.csv')

p1 = host.plot(df['time'], df['m1'],      color='k', label="Light")#linestyle='--',
p2 = ax2.plot(df['time'], df['m2'],  color='red', label="Floor friction")
p3 = ax3.plot(df['time'], df['m3'], color='green', linestyle='-', label="Gripper friction")


host.legend(handles=p1+p2+p3, loc='lower left')#loc='best')
# right, left, top, bottom
ax3.spines['right'].set_position(('outward', 50))

# Set x-ticks and x-tick labels
x_ticks = df['time'][::10]  # Set x-ticks at intervals of 10
host.set_xticks(x_ticks)
host.set_xticklabels(x_ticks, rotation=45)

# plot adaptation needed
tv= 600 # 10 min
for i in range(1,len(df['time'])):
    if df['edgeORboundary'][i-1]!='-9999999': # problem detected
        if df['time2problem'][i-1] > tv and df['time2problem'][i] <= tv and df['time2problem'][i] >= 0:
            host.axvline(x=df['time'][i], color='g', linestyle=':', linewidth=1.5)


# --------- Second subplot
for i in range(1,len(df['time'])):
    if df['time2problem'][i] <= 0 and df['time2problem'][i]!=-9999999:
        print("time ", df['time'][i])
        if df['edgeORboundary'][i]=='b':
            aax3.axvline(x=df['time'][i], color='r', linestyle='-', linewidth=2.5)
        if df['edgeORboundary'][i]=='e':
            aax3.axvline(x=df['time'][i], color='g', linestyle='-', linewidth=2.5)


# --------- Fourth subplot
for i in range(1,len(df['time'])):
    M1 = df['m1'][i]
    M2 = df['m2'][i]
    M3 = df['m3'][i]
    v,fail = in_violation(df['time'][i],M1, M2, M3)
    if v==-1:
        if fail==1:
            aax4.axvline(x=df['time'][i], color='b', linestyle='-', linewidth=2.5)
        if fail==2:
            aax4.axvline(x=df['time'][i], color='g', linestyle='-', linewidth=2.5)
        if fail==3:
            aax4.axvline(x=df['time'][i], color='r', linestyle='-', linewidth=2.5)
        
    

# Set x limits
host.set_xlim([min(df['time']), max(df['time'])])
aax3.set_xlim([min(df['time']), max(df['time'])])
aax4.set_xlim([min(df['time']), max(df['time'])])

# Set y labels
aax3.set_ylabel("predict. \nproblem", rotation=45)#, ha='right')
aax4.set_ylabel("\nreq.\nviolated", rotation=45)#, ha='right')




# --------- Next subplots
# no x-ticks                 
#aax2.xaxis.set_ticks([]); aax2.yaxis.set_ticks([])
aax3.xaxis.set_ticks([]); aax3.yaxis.set_ticks([])
#aax4.xaxis.set_ticks([]);
aax4.yaxis.set_ticks([])

# host.yaxis.label.set_color(p1[0].get_color())
# ax2.yaxis.label.set_color(p2[0].get_color())
# ax3.yaxis.label.set_color(p3[0].get_color())



# For professional typesetting, e.g. LaTeX, use .pgf or .pdf
# For raster graphics use the dpi argument. E.g. '[...].png", dpi=300)'
plt.savefig("pyplot_multiple_y-axis.pdf", bbox_inches='tight')


plt.show()
