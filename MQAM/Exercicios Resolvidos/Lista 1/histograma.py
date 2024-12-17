import pandas as pd
import matplotlib.pyplot as plt
from unidecode import unidecode
import numpy as np

df=pd.DataFrame({'frequencia_relativa': {'0-1': 0.282, '1-2': 0.316, '2-3': 0.171, '3-4': 0.068, '4-5': 0.085, '5-6': 0.026, '6-7': 0.026, '7-8': 0.009, '8-9': 0.009, '9-10': 0.0, '10-11': 0.0, '11-12': 0.009}})

df=df.reindex(['0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7',
       '7-8', '8-9', '9-10', '10-11', '11-12'])


fig, ax=plt.subplots()
ax.bar(x=df.index, height=df.frequencia_relativa)
ax.set_yticklabels(["{:.2f}".format(k).replace(".",",") for k in np.arange(0.,0.35,0.05)])
plt.show()