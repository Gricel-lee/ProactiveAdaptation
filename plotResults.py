import matplotlib.pyplot as plt
import pandas as pd
plt.close('all') 

cyan = '#00FFFF'
silver = '#C0C0C0'
indigo = '#4B0082'
brown = '#A52A2A'
grey = '#C5C9C7'
gainsboro='#DCDCDC' #lightGrey

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
fig, (aax1, aax2, aax3, aax4, aax5) = plt.subplots(5, 1, figsize=(10, 6), gridspec_kw={'height_ratios': [4, 3, 1, 1, 1]}, layout='constrained')  # (width, height) in inches

# Set background color to grey
#fig.patch.set_facecolor('grey')



aax1.set_facecolor(gainsboro)
aax2.set_facecolor(gainsboro)
aax3.set_facecolor(gainsboro)
aax4.set_facecolor(gainsboro)
aax5.set_facecolor(gainsboro)

# --------- First subplot
host = aax1
host_ax1 = host.twinx()
host_ax2 = host.twinx()
    
# host.set_xlim(0, 2)
host.set_ylim(0.3, 1)
host_ax1.set_ylim(0.0, 0.6)
host_ax2.set_ylim(0.3, 1)

host.set_xlabel("Time")
host.set_ylabel("Light")
host_ax1.set_ylabel("Floor friction")
host_ax2.set_ylabel("Gripper friction")
color1, color2, color3 = plt.cm.viridis([0, .6, .3])

# Read the CSV file
df = pd.read_csv('/Users/grisv/GitHub/Manifest/outputlg.csv')
#df = pd.read_csv('/Users/grisv/GitHub/Manifest/output1-lightDrop.csv')
#df = pd.read_csv('/Users/grisv/GitHub/Manifest/outputlg3-gripper.csv')
#df = pd.read_csv('/Users/grisv/GitHub/Manifest/outputlg4-lightGripper.csv')

p1 = host.plot(df['time'], df['m1'],      color='k', label="Light", linewidth=.9)
p2 = host_ax1.plot(df['time'], df['m2'],  color=brown, label="Floor friction", linewidth=.9,linestyle='--')
p3 = host_ax2.plot(df['time'], df['m3'], color='g', linestyle=':', label="Gripper friction", linewidth=.8)


host.legend(handles=p1+p2+p3, loc='lower left')#loc='best')
# right, left, top, bottom
host_ax2.spines['right'].set_position(('outward', 50))

# Set x-ticks and x-tick labels
x_ticks = df['time'][::10]  # Set x-ticks at intervals of 10
host.set_xticks(x_ticks)
host.set_xticklabels(x_ticks, rotation=45)

# plot adaptation needed
tv= 600 # 10 min
for i in range(1,len(df['time'])):
    if df['edgeORboundary'][i-1]!='-9999999': # problem detected
        if df['time2problem'][i-1] > tv and df['time2problem'][i] <= tv and df['time2problem'][i] >= 0:
            host.axvline(x=df['time'][i], color='b', linestyle=':', linewidth=.9)


# --------- Second subplot: time to problem
for i in range(1, len(df['time'])):
    if df['time2problem'][i] >= 0 and df['time2problem'][i] != -9999999:
        aax2.scatter(x=df['time'][i], y=df['time2problem'][i], color='k', marker='o', s=1)
    if df['time2problem'][i] < 0 and df['time2problem'][i] != -9999999:
        aax2.scatter(x=df['time'][i], y=-1, color='b', marker='o', s=1)
    

# --------- Thrid subplot
for i in range(1,len(df['time'])):
    if df['time2problem'][i] <= 0 and df['time2problem'][i]!=-9999999:
        print("time ", df['time'][i])
        if df['edgeORboundary'][i]=='b':
            aax3.axvline(x=df['time'][i], color='#E50000', linestyle='-', linewidth=2.5)
        if df['edgeORboundary'][i]=='e':
            gold = '#FFD700'
            aax3.axvline(x=df['time'][i], color=gold, linestyle='-', linewidth=2.5)


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
            aax4.axvline(x=df['time'][i], color='k', linestyle='-', linewidth=2.5)
        if fail==3:
            aax4.axvline(x=df['time'][i], color='#ADD8E6', linestyle='-', linewidth=2.5)

# --------- Fifth subplot: new trend
for i in range(1,len(df['time'])):
    if df['trend'][i] == 'new trend':
        aax5.axvline(x=df['time'][i], color='k', linestyle='-', linewidth=1)

# --------- ------------- --------------
    

# Set x limits
host.set_xlim([min(df['time']), max(df['time'])])
aax2.set_xlim([min(df['time']), max(df['time'])])
aax3.set_xlim([min(df['time']), max(df['time'])])
aax4.set_xlim([min(df['time']), max(df['time'])])
aax5.set_xlim([min(df['time']), max(df['time'])])

# Set y labels
aax2.set_ylabel("time to\nproblem (s)")#, rotation=45)#, ha='right')
aax3.set_ylabel("predict. \nproblem", rotation=45)#, ha='right')
aax4.set_ylabel("\nreq.\nviolated", rotation=45)#, ha='right')
aax5.set_ylabel("\nnew tren\ndetected", rotation=45)#, ha='right')




# --------- Next subplots
# no x,y-ticks
aax2.xaxis.set_ticks([]);
#aax2.yaxis.set_ticks([])
aax3.xaxis.set_ticks([]); aax3.yaxis.set_ticks([])
aax4.xaxis.set_ticks([]); aax4.yaxis.set_ticks([])
aax5.yaxis.set_ticks([])

# Set y-axis to the left
#aax2.yaxis.tick_left()
aax2.yaxis.set_ticks_position('right')
#host_ax2.spines['right'].set_position(('outward', 50))
#aax2.yaxis.set_label_position("right")

# host.yaxis.label.set_color(p1[0].get_color())
# ax2.yaxis.label.set_color(p2[0].get_color())
# ax3.yaxis.label.set_color(p3[0].get_color())



# For professional typesetting, e.g. LaTeX, use .pgf or .pdf
# For raster graphics use the dpi argument. E.g. '[...].png", dpi=300)'
plt.savefig("pyplot_multiple_y-axis.pdf", bbox_inches='tight')


plt.show()
