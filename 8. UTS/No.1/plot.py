import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')

orde = [3,5,8,9]
filename = ['dataorde'+ str(i) + '.dat' for i in orde]
data = [pd.read_csv(filename[i],sep='\s+', header=None) for i in range(len(orde))]

def plots(x, fx, filename):
    plt.plot(x, fx, label=filename[:-4])
    plt.xlabel('t(s)')
    plt.ylabel('v(km/h)')
    plt.legend()
    plt.show()
    plt.savefig(filename[:-3]+'.png')

xdata = data[0].iloc[:,0].values
ydata1 = data[0].iloc[:,1].values
ydata2 = data[1].iloc[:,1].values
ydata3 = data[2].iloc[:,1].values
ydata4 = data[3].iloc[:,1].values

plt.plot(xdata,ydata1, linestyle='solid', label='Orde 3')
plt.plot(xdata,ydata2, linestyle='dotted', label='Orde 5')
plt.plot(xdata,ydata3, linestyle='dashed', label='Orde 8')
plt.plot(xdata,ydata4, linestyle='dashdot',label='Orde 9')
plt.xlim(xdata[0],xdata[-1])
plt.xlabel('t(s)')
plt.ylabel('v(km/h)')
plt.legend()
plt.savefig('plot.png',dpi=400)

