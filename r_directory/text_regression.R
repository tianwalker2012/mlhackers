#Will use content of a book discription to predict it's sales figure. 
#Interesting.
#This can act as trend prediction. 
#Cool, I love this. Use data to show the real trend, not the unvalidated guts feeling
ranks <- read.csv('/Users/apple/work_foot/ml4hackers/06-Regularization/data/oreilly.csv', stringsAsFactors = FALSE)
library('tm')

documents <- data.frame(Text = ranks$Long.Desc.)
#My guess is use the number as the row name
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(corpus)


x <- as.matrix(dtm)

#Mean reverse ordered sequence
y <- rev(1:100)

performance <- data.frame()

for(lambda in c(0.1, 0.25, 0.5, 1, 2, 5))
{
	for (i in 1:50)
	{
		indices <- sample(1:100, 80)
		training.x <- x[indices,]
		training.y <- y[indices]
		test.x <- x[-indices,]
		test.y <- y[-indices]
		glm.fit <- glmnet(training.x, training.y)
		predict.y <- predict(glm.fit, test.x, s = lambda)
		rmse <- sqrt(mean((predict.y - test.y)^2))
		performance <- rbind(performance, data.frame(Lambda = lambda,
			Iteration = i,
			RMSE = rmse))
	}
}

ggplot(performance, aes(x = Lambda, y = RMSE)) + stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar')
	+ stat_summary(fun.data = 'mean_cl_boot', geom = 'point')

#Following is the logistic regression for the data
y <- rep(c(1,0), each = 50)

regularized.fit <- glmnet(x, y, family = 'binomial')

performance <- data.frame()

for(i in 1:250)
{
	indices <- sample(1:100, 80)
	training.x <- x[indices, ]
	training.y <- y[indices]
	
	test.x <- x[-indices,]
	test.y <- y[-indices]
	
	for(lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1))
	{
		glm.fit <- glmnet(training.x, training.y, family = 'binomial')
		predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
		error.rate <- mean(predicted.y != test.y)
		performance <- rbind(performance,
			data.frame(Lambda = lambda,
				Iteration = i,
				ErrorRate = error.rate))
	}
}

ggplot(performance, aes(x = lambda, y = ErrorRate)) + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar")
	+ scale_x_log10()

