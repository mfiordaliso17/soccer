
import pandas as pd
import numpy as np

df = pd.read_csv('/Users/markusfiordaliso/Documents/coding_projects/sports_bet/soccer_bet_win_draw/data/soccer_epl_20150201_20201210.csv')

df = df.iloc[:, 0:15]

# functions

def odd_decimal(odd):
    if odd > 100:
        return odd/100
    else:
        return 100/odd
    

print(odd_decimal(df['pinnacle_h']))
