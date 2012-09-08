meanval = 0.0
sdval = 0.2
xlow = meanval - 3*sdval
xhigh = meanval + 3*sdval
dx = 0.02
x = seq(from=xlow, to ＝ xhigh, by = dx)
y = ( 1/(sdval*sqrt(2*pi)))* exp(-.5 * ((x-meanval)/sdval)^2)
plot(x , y , type = "h", lwd = 1, cex.axis=1.5, xlab="x", ylab="p(x)", cex.lab = 1.5,
	main="Normal Probability Density", cex.main=1.5)

#Why not show off? I don't know
#lines(x , y)
#area = sum(dx*y)

#text(-sdval, 9*max(y) , bquote(paste(mu, "=", (meanval))), adj=c(1, 5))
#text(-sdval, 8*max(y) , bquote(paste(sigma, "=", (sdval))), adj=c(1,5))
#text(sdval, 9*max(y), bquote(paste(Delta, "x = ", (dx))), adj=c(0, 5))
#dev.copy2eps(file = "~/Density.eps")
