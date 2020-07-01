import pandas as pd
import numpy as np

filename = 'PDP_Parabolik.dat'
data = pd.read_csv(filename, sep='\s+', header=None)

xdata = data.iloc[:,0].values
ydata = data.iloc[:,1].values
zdata = data.iloc[:,2].values

import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

fig = plt.figure()
ax = Axes3D(fig)

x, y = np.meshgrid(xdata,ydata)
z = np.tile(zdata,(len(zdata),1))

surf = ax.plot_surface(x, y, z, cmap='coolwarm')
fig.colorbar(surf, ax=ax, shrink=0.5, aspect=9)
ax.set_xlabel('L(m)')
ax.set_ylabel('t(s)')
ax.set_zlabel('T($^{\circ}C$)')
plt.show()
