#This is a chapter about Cross validation and Regularization
#Cool, Let's typing the code and find the core of the matter.
#Generate Fake random number for test purpose
set.seed(1)
x <- seq(0, 1, by = 0.01)

#Adding noise which distributed as normal distribution
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)
#Take 10 minutes to revisit the definition about Standard diviation for Normal distribution.
#Square Root of sqrt(sum((x-mean)^2))

#following code is to randomly pick 50% data as training data and 50% data as validation data. 
#Notice the random pick code
n <- length(x)
#as it's name imply, sample mean pick something from the squence according to requirement
indices <- sort(sample(1:n, round(0.5 * n)))
training.x <- x[indices]
training.y <- y[indices]

#Minus mean don't include. This is very handy man. I love this
test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h)
{
	return(sqrt(mean((y - h)^2)))
}

performance <- data.frame()

#What's the purpose of this loop for?
for (d in 1:12)
{
	poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
	#What's the purpose of Rbind?
	#It is like add a data frame to a parent data frame.
	#It means add a row into the performance data frame.
	performance <- rbind(performance, 
		data.frame(Degree = d,
			Data = "Training",
			Complexity = sum(coef(poly.fit)^2),
			RMSE = rmse(training.y, predict(poly.fit))))
	
	performance <- rbind(performance,
		data.frame(Degree = d,
			Data = "Test",
			Complexity = sum(coef(poly.fit)^2),
			RMSE = rmse(test.y, predict(poly.fit, newdata = test.df))))
}

#One picture is clearer than a thousand word. 
#The machine learning book is upwind to machanical trade.
ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) + geom_point() + geom_line()

#Why the value of coefficient is also being part of the complexity?
#This is strange. 











