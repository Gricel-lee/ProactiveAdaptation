import matplotlib.pyplot as plt
import pandas as pd

fig, host = plt.subplots(figsize=(8,5), layout='constrained') # (width, height) in inches
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

p1 = host.plot(df['time'], df['m1'],  linestyle='--',    color='k', label="Light")
p2 = ax2.plot(df['time'], df['m2'],  color='red', label="Floor friction")
p3 = ax3.plot(df['time'], df['m3'], linestyle=':', label="Gripper friction")


host.legend(handles=p1+p2+p3, loc='lower left')#loc='best')
# right, left, top, bottom
ax3.spines['right'].set_position(('outward', 50))

# Set x-ticks and x-tick labels
x_ticks = df['time'][::10]  # Set x-ticks at intervals of 10
host.set_xticks(x_ticks)
host.set_xticklabels(x_ticks, rotation=45)

# no x-ticks                 
#host.xaxis.set_ticks([])

# host.yaxis.label.set_color(p1[0].get_color())
# ax2.yaxis.label.set_color(p2[0].get_color())
# ax3.yaxis.label.set_color(p3[0].get_color())


# For professional typesetting, e.g. LaTeX, use .pgf or .pdf
# For raster graphics use the dpi argument. E.g. '[...].png", dpi=300)'
plt.savefig("pyplot_multiple_y-axis.pdf", bbox_inches='tight')


plt.show()
