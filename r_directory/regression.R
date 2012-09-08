##My first taste for Regeression
##Initially I only get busy with my brain, It turns out no an optimized way to do things
ages <- read.csv('/Users/apple/work_foot/ml4hackers/05-Regression/data/longevity.csv')

#To calculate the mean of AgeAtDeath in ages
constants.guess <- with(ages, mean(AgeAtDeath))
#Why nobody hold this value?
#What with is like?
#Save the sign for ages$AgeAtDeath?
#Write a test to verify this later
#Why do nothing with the calculated data?
with(ages, sqrt(mean((AgeAtDeath - constants.guess) ^2)))

#Calcualte the average age for the smoker's death. 
#It is so convinient. 
#How to programming a domain language suit exactly to your domain problem.
#Only by doing this you could get neat and clean code like this.
smokers.guess <- with(subset(ages, Smokes ==1), mean(AgeAtDeath))

non.smokers.guess <- with(subset(ages, Smokes == 0), mean(AgeAtDeath))

#Add a column into the ages.
ages <- transform(ages, NewPrediction = ifelse(Smokes == 0, non.smokers.guess, smokers.guess))

#Why add into the ages, why not refer directly?
#Not with a straight mind. 
#We tend to emphasize what we don't have. 
#Is this good? 
with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))
#Just typy in the code and immerse yourself into the material. You will know some of the very basics about the
#language. 

