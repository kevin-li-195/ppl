import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import numpy as np

df = pd.read_csv("test.csv")
# plt.hist(df["w"], bins=np.linspace(0,3,100))
# plt.show()

se = df["w"].std() * 1.96
mean = df["w"].mean()

print(mean, df["w"].var())
print(mean-se, mean+se)
