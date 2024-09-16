import matplotlib.pyplot as plt
import pandas as pd
from matplotlib import colors as mcolors

plt.close('all') 

colors = dict(mcolors.BASE_COLORS, **mcolors.CSS4_COLORS)
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
    
    Rfail=[]
    # check requirements
    r1 = (P1 * P2 * P4) / (1 - PR * P3)
    r2 = (P1 * (P3 * T2 - P2 * T3 - P3 * T2F - T1 - T2 + T1F) + PR * P3 * (T1F - P1 * TR - P1 * T1F) - T1F) / (PR * P3 - 1)
    if r1 < R1:
        Rfail.append(1)
    if r2 > R2:
        Rfail.append(2)
    if (1 - PR * P3) == 0:
        Rfail.append(3)
    else:
        r3 = (P1 * P2 * (1 - P4)) / (1 - PR * P3)
        if r3 > R3:
            Rfail.append(3)
    
    #if Rfail!=[]:
        #input("Press Enter to continue...")
        #print(time,M1,M2,M3, r1, r2, r3, Rfail)
    return Rfail









# Read the CSV file
df = pd.read_csv('/Users/grisv/GitHub/Manifest/outputlg.csv')
df_Day = pd.read_csv('/Users/grisv/GitHub/Manifest/dataplus.csv')


# ------- Create a 3x1 grid of subplots
fig, (aax1, aax2, aax3, aax4, aax5) = plt.subplots(5, 1, figsize=(10, 6), gridspec_kw={'height_ratios': [4, 3, 1, 1, 1]}, layout='constrained')  # (width, height) in inches

#Set x scale and x labels
x_ticks = df_Day['time'][::10]  # Set x-ticks at intervals of 10
# Set x-ticks from 0 to 14500
x_ticks = range(0, max(df_Day['time'])+500, 500)  # Example: Set x-ticks every 1000 units

# Set x-ticks and labels for all subplots (has to be done in a for to work -- separated causes  problems)
for ax in [aax1, aax2, aax3, aax4, aax5]:
    ax.set_xticks(x_ticks)
    ax.set_xticklabels(x_ticks, rotation=45)
    ax.set_xlim(0, max(df_Day['time']))  # Ensure the x-axis starts from 0


# Set background color to grey  #fig.patch.set_facecolor('grey')
aax1.set_facecolor(gainsboro)
aax2.set_facecolor(gainsboro)
aax3.set_facecolor(gainsboro)
aax4.set_facecolor(gainsboro)
aax5.set_facecolor(gainsboro)

# --------- First subplot
host_ax1 = aax1.twinx()
host_ax2 = aax1.twinx()
    
# host.set_xlim(0, 2)
aax1.set_ylim(0.3, 1)
host_ax1.set_ylim(0.0, 0.6)
host_ax2.set_ylim(0.3, 1)

aax1.set_xlabel("Time")
aax1.set_ylabel("Light")
host_ax1.set_ylabel("Floor friction")
host_ax2.set_ylabel("Gripper friction")
color1, color2, color3 = plt.cm.viridis([0, .6, .3])


# Plot the day data
p1 = aax1.plot(df_Day['time'], df_Day['m1'],      color='k', label="Light", linewidth=.9)
p2 = host_ax1.plot(df_Day['time'], df_Day['m2'],  color=brown, label="Floor friction", linewidth=.9,linestyle='--')
p3 = host_ax2.plot(df_Day['time'], df_Day['m3'], color='k', linestyle=':', label="Gripper friction", linewidth=.8)

# Set legend in lower left corner
aax1.legend(handles=p1+p2+p3, loc='lower left', bbox_to_anchor=(0.07, 0))#loc='best')
host_ax2.spines['right'].set_position(('outward', 50)) # right, left, top, bottom



# plot adaptation needed
tv= 600 # 10 min
for i in range(1,len(df)):
    if df['edgeORboundary'][i-1]!='-9999999': # problem detected
        if df['time2problem'][i-1] > tv and df['time2problem'][i] <= tv and df['time2problem'][i] >= 0:
            aax1.axvline(x=df['time'][i], color='b', linestyle=':', linewidth=.9)


# --------- Second subplot: time to problem
for i in range(1, len(df['time'])):
    if df['time2problem'][i] >= 0:
        aax2.scatter(x=df['time'][i], y=df['time2problem'][i], color='g', marker='o', s=1)
    if df['time2problem'][i] < 0:
        aax2.scatter(x=df['time'][i], y=-1, color='r', marker='o', s=1)
        


# --------- Thrid subplot
# first fill empty values after first prediction
predicted = ""
for i in range(1,len(df['time'])):
    if df['edgeORboundary'][i]=='b' or df['edgeORboundary'][i]=='v':
        predicted = df['edgeORboundary'][i]
    df.at[i,'edgeORboundary'] = predicted

# then plot predicted problem
for i in range(1,len(df['time'])):
    if df['time2problem'][i] <= 0:
        if df['edgeORboundary'][i]=='b':
            aax3.axvline(x=df['time'][i], color='#FF4500', linestyle='-', linewidth=2.5)
        if df['edgeORboundary'][i]=='v':
            gold = '#FFD700'
            aax3.axvline(x=df['time'][i], color=gold, linestyle='-', linewidth=2.5)


# --------- Fourth subplot

color123,color23,color13,color12,color3,color2,color1 = colors['darkslateblue'], colors['blueviolet'], colors['mediumorchid'],colors['purple'], colors['magenta'], colors['hotpink'], colors['pink']


for i in range(1,len(df['time'])):
    M1 = df['m1'][i]
    M2 = df['m2'][i]
    M3 = df['m3'][i]
    Rfail = in_violation(df['time'][i],M1, M2, M3)
    #print(Rfail,"RFAIL")
    Rfail = set(Rfail)
    if Rfail!={}:
        print(Rfail)
        if Rfail == {1}:
            aax4.axvline(x=df['time'][i], color=color1, linestyle='-', linewidth=2.5)
        if Rfail=={2}:
            aax4.axvline(x=df['time'][i], color=color2, linestyle='-', linewidth=2.5)
        if Rfail=={3}:
            aax4.axvline(x=df['time'][i], color=color3, linestyle='-', linewidth=2.5)
        if Rfail=={1,2}:
            aax4.axvline(x=df['time'][i], color=color12, linestyle='-', linewidth=2.5)
        if Rfail=={1,3}:
            aax4.axvline(x=df['time'][i], color=color13, linestyle='-', linewidth=2.5)
        if Rfail=={2,3}:
            aax4.axvline(x=df['time'][i], color=color23, linestyle='-', linewidth=2.5)
        if Rfail=={1,2,3}:
            aax4.axvline(x=df['time'][i], color=color123, linestyle='-', linewidth=2.5)

# # --------- Fifth subplot: new trend
# for i in range(1,len(df['time'])):
#     if df['trend'][i] == 'new trend':
#         aax5.axvline(x=df['time'][i], color='k', linestyle='-', linewidth=1)

# # --------- ------------- --------------
    

# # Set x limits
# host.set_xlim([min(df['time']), max(df['time'])])
# aax2.set_xlim([min(df['time']), max(df['time'])])
# aax3.set_xlim([min(df['time']), max(df['time'])])
# aax4.set_xlim([min(df['time']), max(df['time'])])
# aax5.set_xlim([min(df['time']), max(df['time'])])

# # Set y labels
# aax2.set_ylabel("time to\nproblem (s)")#, rotation=45)#, ha='right')
# aax3.set_ylabel("predict. \nproblem", rotation=45)#, ha='right')
# aax4.set_ylabel("\nreq.\nviolated", rotation=45)#, ha='right')
# aax5.set_ylabel("\nnew tren\ndetected", rotation=45)#, ha='right')




# --------- Next subplots
# # no x,y-ticks
# aax2.xaxis.set_ticks([]);
# #aax2.yaxis.set_ticks([])
# aax3.xaxis.set_ticks([]); aax3.yaxis.set_ticks([])
# aax4.xaxis.set_ticks([]); aax4.yaxis.set_ticks([])
# aax5.yaxis.set_ticks([])

# Set y-axis to the left
#aax2.yaxis.tick_left()
aax2.yaxis.set_ticks_position('right')
#host_ax2.spines['right'].set_position(('outward', 50))
#aax2.yaxis.set_label_position("right")

# # host.yaxis.label.set_color(p1[0].get_color())
# # ax2.yaxis.label.set_color(p2[0].get_color())
# # ax3.yaxis.label.set_color(p3[0].get_color())



# # For professional typesetting, e.g. LaTeX, use .pgf or .pdf
# # For raster graphics use the dpi argument. E.g. '[...].png", dpi=300)'
# plt.savefig("pyplot_multiple_y-axis.pdf", bbox_inches='tight')


plt.show()
