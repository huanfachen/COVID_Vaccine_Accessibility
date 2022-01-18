import logging
import pandas as pd
import requests
import json
from compute_distance import compute_distance

euclidean_distance_matrix=pd.read_csv('euclidean_distance_30mile.csv', low_memory=False)

# compute distance
compute_distance(0,100000)
compute_distance(100000,300000)
compute_distance(300000,600000)
compute_distance(600000,900000)
compute_distance(900000,1200000)
compute_distance(1200000,1300000)
compute_distance(1300000,1400000)
compute_distance(1400000,1500000)
compute_distance(1500000,1600000)
compute_distance(1600000,1700000)
compute_distance(1700000,1800000)
compute_distance(1800000,1819885)

# merge all results
distance_matrix0 = pd.read_csv('Distance0_100000.csv')
distance_matrix1 = pd.read_csv('Distance100000_300000.csv')
distance_matrix2 = pd.read_csv('Distance300000_600000.csv')
distance_matrix3 = pd.read_csv('Distance600000_900000.csv')
distance_matrix4 = pd.read_csv('Distance900000_1200000.csv')
distance_matrix4 = pd.read_csv('Distance1200000_1300000.csv')
distance_matrix4 = pd.read_csv('Distance1300000_1400000.csv')
distance_matrix4 = pd.read_csv('Distance1400000_1500000.csv')
distance_matrix4 = pd.read_csv('Distance1500000_1600000.csv')
distance_matrix5 = pd.read_csv('Distance1600000_1700000.csv')
distance_matrix5 = pd.read_csv('Distance1700000_1800000.csv')
distance_matrix6 = pd.read_csv('Distance1800000_1819885.csv')

distance_matrix = pd.concat([distance_matrix0, distance_matrix1,distance_matrix2,distance_matrix3,distance_matrix4,distance_matrix5,distance_matrix6], axis=0, ignore_index=True)
distance_matrix.to_csv('distance_cost_final_OSRM.csv',index=False)

if len(distance_matrix[distance_matrix['Distance in meter'].isnull()==True])==0:
    print('There is no empty result.')
