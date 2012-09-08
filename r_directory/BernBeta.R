BernBeta = function(priorShape, dataVec, credMass=0.95, saveGraph=F) {
#Bayesian updating for Bernoulli likelihood and beta prior
#Input arguments
# PriorShape  vector of parameter values for the prior beta distribution
# dataVec  vector of 1's and 0's
# credMass the probability mass of the equal tailed credible interval
# Output:
# postShape vector of parameter values for the posterior beta distribution.
# Graphics: Create a three-panel graph of prior. Likelihood and posterior with highest posterior density interval
a = priorShape[1]
b = priorShape[2]
z = sum(dataVec == 1)
N = length(dataVec)

postShape = c(a+z , b+N-z)

pData = beta(z+a, N-z+b) / beta(a , b)
#did this relative as jsp file?
#It should or it just too troublesome
#source("HDIofICDF.R")
#hpdLim = HDIofICDF(qbeta , shape1 = postShape[1], shape2 = postShape[2])

binwidth = 0.005;
Theta = seq(from = binwidth/2, to = 1-(binwidth/2), by = binwidth)
pTheta = dbeta(Theta, a, b)

#compute the likelihood of the data at each value of theta
pDataGivenTheta = Theta^z * (1 - Theta)^(N-z)
pThetaGivenData = dbeta(Theta, a+z, b+N-z)

#quartz("hello", 7, 10)
#layout(matrix(	c(1, 2, 3), nrow = 3, ncol = 1 ,byrow=FALSE))
#par( mar=c(3,3,1,0), mgp=c(2,1,0), mai=c(.5, .5, .3, .1) )
maxY = max( c(pTheta, pThetaGivenData))
plot( Theta, pTheta, type="l", lwd = 3, xlim = c(0,1) , ylim = c(0,maxY), cex.axis=1.2,
	xlab=bquote(theta), ylab=bquote(p(theta)), cex.lab = 1.5, main = "Prior", cex.main = 1.5)
#if(a > b) { textx = 0; textadj = c(0,1) }
#else { textx = 1; textadj = c(1,1) }

#plot(Theta, pDataGivenTheta, type = "l", lwd=3, xlim=c(0,1), cex.axis=1.2, xlab=bquote(theta),
#ylim=c(0, 1.1*max(pDataGivenTheta)), ylab=bquote("p(D|" * theta * ")"), 
#	cex.lab=1.5, main="likelihood", cex.main=1.5)
print( bquote(completed))

#plot(Theta, pThetaGivenData, type = "l", lwd=3, xlim=c(0,1), cex.axis=1.2, xlab=bquote(theta),
#ylim=c(0, maxY), ylab=bquote("p(" * theta * "|D)"), 
#	cex.lab=1.5, main="Posterior", cex.main=1.5)
}

