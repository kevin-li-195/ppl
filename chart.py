import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("test.csv")
plt.hist(df["bias"])
plt.show()
