import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

df = pd.read_csv("test.csv")
plt.hist(df["bias"],bins=np.linspace(0,1,100))
plt.show()
