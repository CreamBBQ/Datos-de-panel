import pandas as pd
import numpy as np

df = pd.read_stata('mus03data.dta')
y = df[['ltotexp']].to_numpy()
x = df[df.columns.difference(['b'])]
print(x)