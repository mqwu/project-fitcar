
## load pkg
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import glob


## set working dir
print(os.getcwd())
os.chdir("Z:\project\FitCar\data")


## load data
csv_files = glob.glob('*.csv')


dataframes = {}
for csv in csv_files:
    
    
    df = pd.read_csv(csv)  # read csv file
    df_name = csv.replace('.csv', '')  # extract csv file name
    
    dataframes[df_name] = df  # load data into dataframes dict
    

for key, val in dataframes.items():
    with open('count.txt', 'a') as f:
        print(key + " dim: " + str(val.shape), file=f)
