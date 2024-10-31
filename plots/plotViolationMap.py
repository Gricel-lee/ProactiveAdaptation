# plot 3D graph of violation map, file from data/violationMap.csv
# Usage: python plotViolationMap.py

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# read data from file
data = np.genfromtxt('/Users/grisv/GitHub/Manifest/gen_files/violationMap.csv', delimiter=',', skip_header=1)
x = data[:,0]
y = data[:,1]
z = data[:,2]
color = data[:,3]

# Create a custom color map
color_map = {-1: 'black', 0: 'red', 1: 'green', 2: 'blue'}
colors = [color_map[val] for val in color]

# Create the scatter plot
fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(111, projection='3d')
ax.scatter(x, y, z, c=colors, marker='*', s=0.5)

# Set y-ticks to only show min and max values
y_min, y_max = 0.25, 0.5
ax.set_yticks([y_min, y_max])

# Set axis labels with padding
ax.set_xlabel('light', labelpad=20)
ax.set_ylabel('floor friction', labelpad=20)
ax.set_zlabel('gripper friction', labelpad=20)
# Change axis numbering padding
ax.tick_params(axis='x', pad=10)
ax.tick_params(axis='y', pad=10)
ax.tick_params(axis='z', pad=10)

plt.show()