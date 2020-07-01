using Plots;gr()
using DelimitedFiles

rk4 = readdlm("rk4_output.dat")
analytic = readdlm("bessel_analytic.dat")

x = LinRange(0.0,50.0,100)

plot(x,rk4[:,2], label = "Runge kutta 4th")
plot!(x, analytic[:,2], label = "Analytic")
xaxis!((0.0,50.0), xlabel="x")
yaxis!(ylabel="J0(x)")
savefig("test.png")
