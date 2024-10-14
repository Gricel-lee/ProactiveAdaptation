import math
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib import colors as mcolors
from matplotlib.lines import Line2D

plt.close('all') 

colors = dict(mcolors.BASE_COLORS, **mcolors.CSS4_COLORS)
cyan = '#00FFFF'
silver = '#C0C0C0'
indigo = '#4B0082'
brown = '#A52A2A'
grey = '#C5C9C7'
gainsboro='#DCDCDC' #lightGrey

# Hyperparameters
tv= 1 # 5 minutes time window for prediction > previous 600
xlabel_incr_time = 2

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

####>> NOT WORKING
##Replace data when adaptation happens
# for i in df['time']:
#     print("i",i)
#     df_row = df.loc[df['time'] == i]
#     if not df_row.iloc[i, 1:3].equals(df_Day.loc[df_Day['time'] == i].iloc[i, 1:3]):
#         df_Day.loc[df_Day['time'] == i, 'm1'] = df_row['m1'].values[0]
#         df_Day.loc[df_Day['time'] == i, 'm2'] = df_row['m2'].values[0]
#         df_Day.loc[df_Day['time'] == i, 'm3'] = df_row['m3'].values[0]
# df_Day.to_csv('df_Day_output.csv', index=False)
####<< NOT WORKING


# ------- Create a nxm grid of subplots
fig, (ax1, ax2, ax3, ax4) = plt.subplots(4, 1, figsize=(5.5, 5), gridspec_kw={'height_ratios': [4, 2, 1, 1]}, layout='constrained')  # (width, height) in inches

#Set x scale and x labels
x_ticks = df_Day['time'][::10]  # Set x-ticks at intervals of 10
# Set x values
x_ticks = range(0, math.ceil(max(df_Day['time']))+1, xlabel_incr_time)  # Example: Set x-ticks every X units

# Set x-ticks and labels for all subplots (has to be done in a for to work -- separated causes  problems)
for ax in [ax1, ax2, ax3, ax4]:
    ax.set_xticks(x_ticks)
    ax.set_xticklabels(x_ticks, rotation=90)
    ax.set_xlim(0, max(df_Day['time']))  # Ensure the x-axis starts from 0


# Set background color to grey  #fig.patch.set_facecolor('grey')
ax1.set_facecolor(gainsboro)
ax2.set_facecolor(gainsboro)
ax3.set_facecolor(gainsboro)
ax4.set_facecolor(gainsboro)


# # Set x limits
# host.set_xlim([min(df['time']), max(df['time'])])
# ax2.set_xlim([min(df['time']), max(df['time'])])
# ax3.set_xlim([min(df['time']), max(df['time'])])
# ax4.set_xlim([min(df['time']), max(df['time'])])
# ax5.set_xlim([min(df['time']), max(df['time'])])

# Set y labels
ax2.set_ylabel("predicted\ntime to\nproblem (s)")#, rotation=45)#, ha='right')
ax3.set_ylabel("\nreq.\nviol.")#, rotation=45)#, ha='right')
ax4.set_ylabel("\nnew\ntrend")#, rotation=45)#, ha='right')

# # No x/y scale
ax1.xaxis.set_ticks([]);
ax2.xaxis.set_ticks([]); ax2.yaxis.set_ticks_position('right') #ax2.yaxis.set_ticks([])
ax3.xaxis.set_ticks([]); ax3.yaxis.set_ticks_position('right') #ax3.yaxis.set_ticks([])
ax4.yaxis.set_ticks_position('right') #ax4.yaxis.set_ticks([]) #ax4.xaxis.set_ticks([]);

# Change scale
ax3.set_yticks([0.1, 0.5, 0.9]); ax3.set_yticklabels(["R1", "R2", "R3"])
ax4.set_yticks([0.1, 0.5, 0.9]); ax4.set_yticklabels(["Light", "Floor", "Gripper"])


# Set y-axis to the left
#ax2.yaxis.tick_left()

#host_ax2.spines['right'].set_position(('outward', 50))
#ax2.yaxis.set_label_position("right")

# # host.yaxis.label.set_color(p1[0].get_color())
# # ax2.yaxis.label.set_color(p2[0].get_color())
# # ax3.yaxis.label.set_color(p3[0].get_color())


# --------- First subplot
host_ax1 = ax1.twinx()
host_ax2 = ax1.twinx()

# host.set_xlim(0, 2)
ax1.set_ylim(0.3, 1)
host_ax1.set_ylim(0.0, 0.6)
host_ax2.set_ylim(0.3, 1)


ax1.set_ylabel("Light")
host_ax1.set_ylabel("Floor friction")
host_ax2.set_ylabel("Gripper friction")


ax1.set_xlabel("(a)")
ax2.set_xlabel("(b)")
ax3.set_xlabel("(c)")
ax4.set_xlabel("time (min)\n(d)")

# Plot the day data
p1 = ax1.plot(df_Day['time'], df_Day['m1'],      color='g', label="Light", linewidth=1)
p2 = host_ax1.plot(df_Day['time'], df_Day['m2'],  color='r', label="Floor", linewidth=1,linestyle='-')
p3 = host_ax2.plot(df_Day['time'], df_Day['m3'], color='b', label="Gripper", linewidth=1, linestyle='-')
# plot adaptation line

adaptTime = -1
for i in range(1,len(df)):
    if df['edgeORboundary'][i-1]!='-9999999': # problem detected
        if df['time2problem'][i-1] > tv and df['time2problem'][i] <= tv and df['time2problem'][i] >= 0:
            ax1.axvline(x=df['time'][i], color='k', linestyle=':', linewidth=1)
            # save first adaptation time
            if adaptTime == -1:
                adaptTime = df['time'][i]

# Legends
# new legend line for adaptation
adaptation_line = Line2D([0], [0], color='k', linestyle=':', linewidth=1, label='adapt.')
# legend ax1
ax1.legend(handles=p1+p2+p3+[adaptation_line], loc='lower left', bbox_to_anchor=(0.07, 0))#loc='best')
host_ax2.spines['right'].set_position(('outward', 50)) # right, left, top, bottom


# --------- Second subplot: time to problem
# Legends
gold = '#FFD700'
orange = '#FF4500'
red='#FF0000'
legend_dots = [
    Line2D([0], [0], color='black', marker='o', linestyle='None', markersize=3, label='safe'),
    Line2D([0], [0], color=orange, marker='o', linestyle='None', markersize=3, label='in violation'),
    Line2D([0], [0], color=gold, marker='o', linestyle='None', markersize=3, label='out of edge')
]
ax2.legend(handles=legend_dots, loc='upper right', ncol=1, handletextpad=0.5, labelspacing=0.2)  # Adjust spacing

# Black dots for safe, orange for in violation, gold for out of edge
for i in range(1, len(df['time'])):
    if (df['time2problem'][i] > 0) and (df['time2problem'][i] <= 4000):
        ax2.scatter(x=df['time'][i], y=df['time2problem'][i], color='k', marker='o', s=2)


# first fill empty values after first prediction (e or b)
predicted = ""
for i in range(1,len(df['time'])):
    if df['edgeORboundary'][i]=='b' or df['edgeORboundary'][i]=='e':
        predicted = df['edgeORboundary'][i]
    df.at[i,'edgeORboundary'] = predicted
#print(df['edgeORboundary'])

# second plot the problem colour coded
for i in range(1,len(df['time'])):
    if df['time2problem'][i] <= 0:
        if df['edgeORboundary'][i]=='b':
            ax2.scatter(x=df['time'][i], y=-1, color=orange, marker='o', s=2)
        if df['edgeORboundary'][i]=='e':
            ax2.scatter(x=df['time'][i], y=-1, color=gold, marker='o', s=2)
            #ax3.axvline(x=df['time'][i], color=gold, linestyle='-', linewidth=2.5)


# --------- Third subplot
requirementViolationTime = -1;
color123,color23,color13,color12,color3,color2,color1 = colors['darkslateblue'], colors['blueviolet'], colors['mediumorchid'],colors['purple'], colors['magenta'], colors['hotpink'], colors['pink']

def line(x, y1, y2, color, linestyle):
    return Line2D([x, x], [y1,y2], color=color, linestyle=linestyle, linewidth=2)


for i in range(1,len(df['time'])):
    # get violated req.
    M1 = df['m1'][i]
    M2 = df['m2'][i]
    M3 = df['m3'][i]
    Rfail = in_violation(df['time'][i],M1, M2, M3)

    # save first violation time
    if requirementViolationTime == -1 and Rfail != []:
        requirementViolationTime = df['time'][i]

    Rfail = set(Rfail)
    # print lines
    # Rfail={1,2,3} # for tests
    if Rfail != {}:
        if 1 in Rfail:
            l = line(df['time'][i],0,0.33,color2,'-') #Line2D([df['time'][i], df['time'][i]], [0, 0.3], color='g', linestyle='-', linewidth=2) 
            ax3.add_line(l)
        if 2 in Rfail:
            l= line(df['time'][i],0.33,0.66,color13,'-')
            ax3.add_line(l)
        if 3 in Rfail:
            l= line(df['time'][i],0.66,1,colors['lightseagreen'],'-')
            ax3.add_line(l)


# --------- Four subplot: new trend
# modify first change from none to light/floor/gripper
for i in range(1,len(df['time'])):
    if df['light'][i]!=df['light'][i-1] or df['floor'][i]!=df['floor'][i-1] or df['gripper'][i]!=df['gripper'][i-1]:
        df.at[i,'trend'] = 'new trend'

for i in range(0,len(df['time'])):
    if df['trend'][i] == 'new trend':
        print(i)
        if df['light'][i]!='none':
            l = line(df['time'][i],0,0.33,'g','-') #Line2D([df['time'][i], df['time'][i]], [0, 0.3], color='g', linestyle='-', linewidth=2) 
            ax4.add_line(l)
        if df['floor'][i]!='none':
            l= line(df['time'][i],0.33,0.66,'r','-')
            ax4.add_line(l)
        if df['gripper'][i]!='none':
            l= line(df['time'][i],0.66,1,'b','-')
            ax4.add_line(l)
        
# # --------- ------------- --------------


print("First adaptation time:", adaptTime)
print("First requirement violation time:", requirementViolationTime)
print("Difference (violation-adaptation time):", requirementViolationTime-adaptTime)

plt.show()
