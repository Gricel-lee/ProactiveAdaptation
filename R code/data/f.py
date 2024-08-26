import numpy as np
import math

def angleRadians(x,y):
    x = np.array(x)
    y = np.array(y)

    dot_product = np.dot(x,y)
    x_modulus = np.sqrt((x*x).sum())
    y_modulus = np.sqrt((y*y).sum())
    cos_angle = dot_product / x_modulus / y_modulus
    angle = math.acos(cos_angle)
    print(angle)
    return angle

x = [1,0]
y = [1,1]

angleRadians(x,y)
print(math.pi/2)
print(1/60)