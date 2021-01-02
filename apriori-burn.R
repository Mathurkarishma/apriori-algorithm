#This script will run the Apriori algorithm on the burn dataset

#Install all packages required for the Apriori algorithm
install.packages("arules")
install.packages("TSP")
install.packages("data.table")
install.packages("arulesViz")
library("arules")
library("arulesViz")

#Import burn file as the dataset
burn<-read.csv(file="burn.csv", head=TRUE, sep=",", as.is=FALSE)

##DATA EXPLORATION

#Display the first 10 rows of the dataset
head(burn,10)

#Display count of variables, observations, descriptions of each variable and examples
str(burn)

#Display basic statistical descriptions of data for all variables
summary(burn)

#Explore data through histograms of each variable
hist(burn$FACILITY) #Less interesting
hist(burn$DEATH)
hist(burn$AGE)
hist(burn$GENDER) #Less interesting
hist(burn$RACEC) #Less interesting
hist(burn$TBSA) #Less interesting
hist(burn$INH_INJ)
hist(burn$FLAME)

##DATA PREPROCESSING

#Eliminate the key identifier and check
burn$ID<-NULL
head(burn,10)

#Discretize numeric variables and view summary
burn$AGE<-discretize(burn$AGE, method="frequency", breaks=6)
summary(burn$AGE)
burn$TBSA<-discretize(burn$TBSA, method="frequency", breaks=6)
summary(burn$TBSA)

#Factor remaining numeric variables and view summary
burn$FACILITY<-factor(burn$FACILITY)
summary(burn$FACILITY)
burn$DEATH<-factor(burn$DEATH)
summary(burn$DEATH)
burn$GENDER<-factor(burn$GENDER)
summary(burn$GENDER)
burn$RACEC<-factor(burn$RACEC)
summary(burn$RACEC)
burn$INH_INJ<-factor(burn$INH_INJ)
summary(burn$INH_INJ)
burn$FLAME<-factor(burn$FLAME)
summary(burn$FLAME)

#Create Apriori rules and inspect
rules<-apriori(burn)
inspect(rules[1:10])

#Test out combinations of different support and confidence levels, remove blanks
rules <- apriori(burn, parameter= list(supp=0.4, conf=0.9, minlen=2))
inspect(rules)

#Generate rules with only flame=0 or flame=1 on the RHS, sort by lift, then inspect
rules<-apriori(burn, parameter= list(supp=0.1, conf=0.7, minlen=2), appearance=list(rhs=c("FLAME=0", "FLAME=1"), default="lhs"))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#Find redundancies by pruning rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#Plot as a matrix
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))

#End of script
