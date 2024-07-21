from violation import *
from smooth import *


# ---- Problem set-up
# Req bounds
R1_bound = 0.58; R2_bound = 200; R3_bound = 0.05 # 40.625% violated

# Permissible ranges for prediction
minM1=0.5; maxM1= 1
minM2=0.25; maxM2=0.5
minM3=0.5; maxM3=1

#file
hist_file = "Data/Historical logs/day_faulty_light.csv"           # one day run

# smooth window
smoothN = 100 # update threshold: capacity of buffer (initially empty) used to store the last N observations - triggers an update of the linear regression model 


# Prediction hyperparameters
N = 400; tau= 400; epsilon= 0
t = 2000 # prediction window within which any disruptions are to be predicted

# ---- Run
v = Violation(hist_file, R1_bound,R2_bound,R3_bound, minM1=0.5, maxM1= 1, minM2=0.25, maxM2=0.5, minM3=0.5, maxM3=1, )

v.print_results_violation()