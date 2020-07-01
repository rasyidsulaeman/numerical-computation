using Plots; gr()
using DelimitedFiles

hm_kubik = readdlm("hermite_kubik_output.dat")
lg_kubik = readdlm("lagrange_kubik_output.dat")
lg = readdlm("lagrange_output.dat")

data = readdlm("xdata.dat")

x = lg[:,1]

plot(x,hm_kubik[:,2], label="Hermite cubic")
plot!(x,lg_kubik[:,2], label="Lagrange cubic")
plot!(x,lg[:,2], label="Lagrange",legend=:topleft)
xaxis!((10.,55.))
savefig("plot.png")
