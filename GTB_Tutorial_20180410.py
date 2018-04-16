# -*- coding: utf-8 -*-
"""
Created on Tue Apr 10 10:34:45 2018

@author: b.four
"""

import numpy as np
import pandas as pd

columnNames = ['HouseVal','MedInc','HouseAge','AveRooms','AveBedrms','Population','AveOccup','Latitude','Longitud']

df = pd.read_csv('cadata.txt',skiprows=27, sep='\s+',names=columnNames)
df  # F9 to run the current line 
df.describe()

# Now we have to split the datasets into training and validation. The training
# data will be used to generate the trees that will constitute the final
# averaged model.

import random

X = df[df.columns - ['HouseVal']]
X
Y = df['HouseVal']
rows = random.sample(df.index, int(len(df)*.80))
rows
x_train, y_train = X.ix[rows],Y.ix[rows]
x_test,y_test  = X.drop(rows),Y.drop(rows)

#test
df['HouseVal']
df.columns
df.index
len(df)
































































































