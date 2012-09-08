#start with the foundamentals. It all comes to foundamentals and foundamentals. 
N = 500
#why 47405, why not other values?
set.seed(47405)

#What's the meaning of this? Let's check the help result for detail
flipsequence = sample(x=c(0,1), prob=c(.5,.5), size = N, replace = TRUE)

#what's the cumsum for? interesting let reduce it from the source code
#I guess I got the functionality of cumsum
#given sequence of 1 2 3 4 5
#The result sequence is 1 3 6 10 15
r = cumsum(flipsequence)
n = 1:N
#What's the purpose of this array? This is a array of average Head count
#Should we expect it to be a larger fluc line and turn to smaller fluc?
#I guess we could
runprop = r/n

plot(n, runprop, type="o", log = "x", xlim=c(1,N) , ylim = c(0.0,1.0), cex.axis=1.5,
	xlab="Flip Number", ylab = "Proportion Heads", cex.lab = 1.5, main = "Running Proportion of heads",
	cex.main = 1.5)

lines(c(1,N), c(0.5,0.5), lty=3)
flipletters = paste(c("T","H")[flipsequence[1:10] +1], collapse="")
displaystring = paste("Flip Sequence = ", flipletters, "...", sep="")
text(5, 9, displaystring, adj=c(0,1), cex=1.3)
text(N, .3, paste("End Proportion = ", runprop[N], collapse=""), adj=c(1,0), cex=1.3)
dev.copy2eps(file="~/FlipCoin.eps")
	

