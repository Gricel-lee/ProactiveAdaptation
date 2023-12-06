import pandas as pd
import matplotlib.pyplot as plt
import os 

# Print the current working directory
print("Current Working Directory:", os.getcwd())

# List files in the current working directory
print("Files in Current Directory:", os.listdir())

# Read the Excel file
file_path = 'data.xlsx'  # Update with your file path
df = pd.read_excel(file_path)

# Extract column names
columns = df.columns

# Plot all combinations of two columns
for i in range(len(columns)):
    for j in range(i + 1, len(columns)):
        # Scatter plot
        plt.scatter(df[columns[i]], df[columns[j]])
        
        # Add labels and title
        plt.xlabel(columns[i])
        plt.ylabel(columns[j])
        plt.title(f'Scatter Plot: {columns[i]} vs {columns[j]}')
        
        # Show the plot
        plt.show()


