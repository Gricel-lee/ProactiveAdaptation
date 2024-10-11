import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def make_data_spiky(file_path, noise_level=0.1):
    # Read the CSV file
    spiky_day = pd.read_csv(file_path, header=None)
    # Add random noise to the data
    np.random.seed(4)
    noise = np.random.normal(0, noise_level, spiky_day.shape[0])
    #print("noise",noise)
    spiky_day[2] = spiky_day[2] + noise
    return spiky_day

def make_data_spiky2(file_path, noise_level=1.2):
    # Read the CSV file
    spiky_day = pd.read_csv(file_path, header=None)
    average = spiky_day[2].mean()
    diff = spiky_day[2] - average
    #print("diff",diff)
    # Add n times error from mean
    spiky_day[2] = spiky_day[2] + noise_level * diff
    return spiky_day


def make_data_spiky3(file_path, noise_level=1.2,lim=0.01):
    # Read the CSV file
    spiky_day = pd.read_csv(file_path, header=None)
    average = spiky_day[2].mean()
    print(average)
    #lim removed values above to limit to reduce spikes
    # Replace all values greater than limit+average with limit+average
    spiky_day[2] = np.where(spiky_day[2] > lim+average, lim+average, spiky_day[2])
    # Replace all values greater than -lim+average
    spiky_day[2] = np.where(spiky_day[2] < -lim+average, -lim+average, spiky_day[2])
    
    diff = spiky_day[2] - average
    #print("diff",diff)
    # Add n times error from mean
    spiky_day[2] = spiky_day[2] + noise_level * diff
    return spiky_day



# ======= Run
file = "/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv"
day = pd.read_csv(file, header=None)


# add random noise
#spiky_day = make_data_spiky(file, noise_level=0.1)
# increase error from mean
#spiky_grip_day = make_data_spiky2(file, noise_level=5)
# increase error from mean
spiky_grip_day = make_data_spiky3(file, noise_level=10,lim=0.01)

# Print the first few rows to verify
print(spiky_grip_day[2].head())
print(day[2].head())

# Plot the original and spiky data
plt.figure(figsize=(12, 6))

time = day[0]
plt.plot(time, day[2], '-' ,label='Original Day Data')
plt.plot(time, spiky_grip_day[2],'--', label='Original Day Data')

# Save spiky data to a new file
spiky_grip_day.to_csv('/Users/grisv/GitHub/Manifest/R code/data/spiky_gripper_day.csv', header=False, index=False)

# # Plot original data
# plt.subplot(2, 1, 1)
# plt.plot(day, label='Original Day Data')
# plt.title('Original Day Data')
# plt.xlabel('Index')
# plt.ylabel('Value')
# plt.legend()

# # Plot spiky data
# plt.subplot(2, 1, 2)
# plt.plot(spiky_day, label='Spiky Day Data', color='r')
# plt.title('Spiky Day Data')
# plt.xlabel('Index')
# plt.ylabel('Value')
# plt.legend()

# plt.tight_layout()
plt.show()

