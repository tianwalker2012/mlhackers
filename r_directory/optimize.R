heights.weights <- read.csv("/Users/apple/work_foot/ml4hackers/02-Exploration/data/01_heights_weights_genders.csv");
coef(lm(Weight ~ Height, data = heights.weights))
#Will check for the coefficient. if the optimization could get optimized value?

squared.error <- function(heights.weights, a, b)
{
	predictions <- with(heights.weights, height.to.weight(Height, a, b))
	errors <- with(heights.weights, Weight - predictions)
	return (sum(errors ^2))
}

optim(c(0,0), function(x)
	{
		return(squared.error(heights.weights, x[1], x[2]))
	})
