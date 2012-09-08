us.states <- c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id",
	"il","in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh"
	,"nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt",
	"wa","wi","wv","wy")
ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA
#Why not use subset as a filter funtion?
#Let's check what we can simplifed?
ufo.us <- subset(ufo, !is.na(USState))

#Get some statistic information regarding the dateOccurred
summary(ufo.us$DateOccurred)

#Following code used to draw the histogram, let's check the detail to understand it fully.
#Enjoy the data manipulation. 
#What's the meaning of plus symbol in following syntax?
quick.hist <- ggplot(ufo.us, aes(x=DateOccurred)) + geom_histogram() +
	scale_x_date(major="50 years")
#Something wrong with te scale_x_date, let's check if image generate.

ggsave(plot=quick.hist, filename="~/work_foot/r_directory/quick_hist.png", height=6, width=8)

ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1900-01-01"))
#What's the difference between length and nrow?
#nrow will tell us how many rows we have while length only have the column of datas
nrow(ufo.us)
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")
#Is it like the group operation in SQL?
#Group by the columns then give count for each of them
#Seems it support the passing the function as object. 
sighttings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)
date.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)), to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings <- strftime(date.range,"%Y-%m)

#How to merge 2 data frame according to the conditions. 
#why we still need the rep after the merge?
#Don't get it.
#following is my guessing
#The merge is to simply the data mean only keep the state and month and sighting times
#Will later the rep will just keep the data range, then we can use the date to query from the data frame about particular sighting times
#for each state
all.sightings <- merge(states.dates, sighttings.counts, by.x=c("s","date.strings"), by.y=c("USState", "YearMonth"), all=TRUE)

names(all.sightings) <- c("State", "YearMonth", "Sightings")
#Interesting, it have jquery like property,mean the [] is a filter condition. 
#Put you boolean experssion into it. you could get your result out.
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))
#factor is a data type

#How to use ggplot wisely and professionally?
#Professional is a dull word. A lot of garbage covered by it.
state.plot <- ggplot(all.sightings, aes(x=YearMonth, y=Sightings))+
			geom_line(aes(color="darkblue"))+
			facet_wrap(~State,nrow=10,ncol=5)+
			theme_bw()+
			xlab("Time")+ylab("Number of Sightings")+
			opts(title="Number of UFO in US(1990-2010)")
ggsave(plot=state.plot, filename="~/work_foot/r_directory/state.png", width=14, height=8.5)
#R is deep water, it is a powerful tools. let's make it a powerful tools in my toolset. 

#calculate the mean 
my.mean <- function(x) {
	return (sum(x) / length(x))
}

#calculate the median
my.median <- function(x) {
	sorted.x = sort(x)
	if(length(x) %% 2 == 1){
		added.mid = c(sorted.x[length(x)/2], sorted.x[length(x)/2 + 1])
		return (mean(added.mid))
	}else{
		return sorted.x[length(x)/2]
	}
}

data.file <- file.path('~/work_foot/ml4hackers/02-Exploration/data/', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ",")
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)
ggplot(heights.weights, aes(x = Height)) + geom_density()
ggplot(heights.weights, aes(x = Weight, fill = Gender))+ geom_density()
ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density() + facet_grid(Gender ~ .)

#What's the meaning of Cauchy distribution?
#It is the bell curve with heavy tail. 

#Gamma distrbition. 
gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma.values), aes(x = X))+ geom_density()
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()

ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

heights.weights <- transform(heights.weights, Male = ifelse(Gender == "Male", 1, 0))
logit.model <- glm(Male ~Height + Weight, data = heights.weights, family = binomial(link = "logit"))
ggplot(heights.weights, aes(x = Weight, y = Height, Color = Gender))+ geom_point() + 
stat_abline(intercept = - coef(logit.model)[1]/ coef(logit.model)[2], slope = -coef(logit.model)[3]/coef(logit.model)[2],
geom = "abline",
color = "black")

library(tm)
spam.path <- "data/spam/"
spam2.path <- "data/spam_2/"
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

get.msg <- function(path) {
	con <- file(path, open="rt", encoding="latin1")
	text <- readlines(con)
	#All it did is to remove the empty line
	msg <- text[seq(which(text=="")[1]+1, length(text), 1)]
	close(con)
	return (paste(msg, collapse="\n"))
}

spam.docs <- dir(spam.path)
#This is act as a filter.
spam.docs <- spam.docs[which(spam.docs !="cmds")]
#Why do we need sapply?
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep="")))
#what's the purpose of following function?
#Look at it carefully, found out what he is doing
get.tdm <- function(doc.vec){
	doc.corpus <- Corpus(VectorSource(doc.vec))
	control <- list(stopword=TRUE, removePunctuation=TRUE, removeNumbers=TRUE,minDocFreq=2)
	doc.dtm <- TermDocumentMatrix(doc.corpus, control)
	return(doc.dtm)
}
