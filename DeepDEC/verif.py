import numpy as np
import pandas as pd
import random


'''Class predicted and with what probability'''
def DNN(x):
    ### DNN model here ###
    x_predicted = random.choice(['happy', 'sad'])
    x_predicted_probability = 0.85
    ####
    return x_predicted, x_predicted_probability

'''Minimum Confidence Threshold'''
def verif1(x,t):
    x_pred, prob = DNN(x)
    if prob < t: #t is a threshold
        return False
    return  True

'''Local Robustness Certification'''
def verif2(x,X,l2_metric):
    x_pred, prob = DNN(x)
    for x2 in X:
        if x != x2:
            # for each point close to x
            if _in_distance(x, x2, l2_metric):
                x2_pred, x2_prob = DNN(x2) # get class prediction
                if x_pred != x2_pred:
                    return False
    return True

'''Check if two inputs are close'''
def _in_distance(point, point2, l2_metric):
    if type(point) is list:    
        point = np.array(point)
        point2 = np.array(point2)
    
    distance = np.linalg.norm(point - point2)
    if distance < l2_metric:
        return True
    return False



# ----- Example -----
# Testing dataset
x1 = [0.3, 0.4, 0.5, 0.65, 0.3] #input 1
x2 = [0.2, 0.8, 0.5, 0.65, 0.3] #input 2
#x3...
X=[x1,x2] # testing set

# Hyperparameters
t = 0.7 # threshold
l2_metric = 0.05 # l2 distance metric

# Output
V = pd.DataFrame(columns=['verif1', 'verif2','xDNN','xReal','x'])  #verification results

# Run algorithm
for x,count in zip(X,range(len(X))):
    # get prection using DNN
    xDNN  = DNN(x)[0]
    
    # get real class from test dataset
    xReal = 'happy'

    # get verification results and store them
    V.loc[count] = [verif1(x,t) , verif2(x,X,l2_metric),xDNN,xReal,x]

# Results
print(V)
