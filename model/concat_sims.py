import pandas as pd
import os

base = "tmp/three_mine_sim/"

def load(f):
    df = pd.read_csv(base + f)
    df['thetas'] = f[2:f.index('.')]
    return df

df = pd.concat(load(f) for f in os.listdir(base))
df.to_csv('../Results/three_mine_sim.csv')
