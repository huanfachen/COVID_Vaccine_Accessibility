import logging
import pandas as pd
import requests
import json

euclidean_distance_matrix=pd.read_csv('euclidean_distance_30mile.csv', low_memory=False)
# Add the URL of OSRM server here
server_ucl = "http://1.1.1.1"
# a function to compute distance on a subset of rows
def compute_distance(row_start, row_end):
    euclidean_distance_matrix_sub=euclidean_distance_matrix[row_start:row_end]
    distance_list = []
    origin_id_list = []
    destination_id_list = []
    
    for (i1, row1) in euclidean_distance_matrix_sub.iterrows():
        LatOrigin = str(row1['MSOA_lat'])
        LongOrigin = str(row1['MSOA_lon'])
        origin_id = row1['origin']
        
        LatDestination = str(row1['site_lat'])
        LongDestination = str(row1['site_lon'])
        destination_id = row1['dest']
        
        url = server_ucl + "/route/v1/driving/" + LongOrigin +","+ LatOrigin + ";" + LongDestination +","+ LatDestination + "?steps=false"
        r = requests.get(url)
        
        if 'json' in r.headers.get('Content-Type') and 'routes' in r.json():
            result_distance = r.json()['routes'][0]['distance']
            distance_list.append(result_distance)
            origin_id_list.append(origin_id)
            destination_id_list.append(destination_id)
        else:
            print('Response content is not in JSON format-- origin:',row1['origin'],';dest:',row1['dest'])
            result_distance = 0
            distance_list.append(result_distance)
            origin_id_list.append(origin_id)
            destination_id_list.append(destination_id)
        
    
    output = pd.DataFrame(distance_list, columns = ['Distance in meter'])
    output['origin_id'] = origin_id_list
    output['destination_id'] = destination_id_list
    output.to_csv( 'Distance{start}_{end}.csv'.format(start=row_start,end=row_end),index=False)
    
    del distance_list
    del origin_id_list
    del destination_id_list
