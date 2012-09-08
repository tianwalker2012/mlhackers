sinefun <- function(t)
{
	return (0.3*sin(t*pi)+ 0.3*cos(2*pi*t) + 0.4*sin(3*pi*t))
}

t = seq(from = 0, to = 1, by = 0.01)
res = sinefun(t)

ggplot(data.frame(X = t, Y = res), aes(x = X, y = Y)) + geom_point() + geom_line()
