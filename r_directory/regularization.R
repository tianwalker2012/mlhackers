#Both cross validation and regularization share the same code. 
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
#Following is the regularization code.
library("glmnet")
glmnet.fit <- with(training.df, glmnet(poly(X, degree = 10), Y))
lambdas <- glmnet.fit$lambda

performance <- data.frame()

for(lambda in lambdas)
{
	performance <- rbind(performance,
		data.frame(Lambda = lambda,
			RMSE = rmse(test.y, with(test.df, predict(glmnet.fit, poly(X, degree = 10), s = lambda)))))
}

ggplot(performance, aes(x = Lambda, y = RMSE)) + geom_point() + geom_line()
















