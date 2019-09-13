library(rpart)
library(rpart.plot)
library(dplyr)
library(randomForest)

###Import the data file into R
StockSpxDataRev3c <- read.csv("~/Documents/Excel/xlsx/StockSpxDataRev3c.csv")
data <- StockSpxDataRev3c
### Define data frame containing all numeric columns.
df <- subset(data, select = -c(1,13) )
anyNA(df)

##Add uniqu ID column for each stock.
data$ID <- seq.int(nrow(data))

##Add a categorical variable about the different between Close and Open.
data <- mutate(data, profit_group = ifelse(Close - Open > 0 ,"Positive Stock",
                                            ifelse(Close - Open <= 0 ,"Negative Stock",
                                                   "N/A")))

#Creat a new numeric variable RangeNet_per_Weekday.
data <- mutate(data, RangeNet_per_Weekday = RangeNet / WeekdayNum)

# Define seed for random number generator
set.seed(1111)

# sample takes a random sample of the specified size from the elements of x
train <- sample(nrow(df), 0.7*nrow(df))

# Define training data frame using random sample of observations
df.train <- df[train,]
# Define validation data frame using all observations not in training data frame
df.validate <- df[-train,]

# Decision Tree

# Define seed for random number generator
set.seed(1111)
dtree <- rpart(target ~ ., data=df.train, method="class",
               parms=list(split="information"))

# Summarize the decision tree including decision nodes and leaf nodes
# The decision tree nodes are described row by row, then left to right
#summary(dtree)

# Display decision tree.  The true values follow the left branches.
plot(dtree,margin = 0.1);text(dtree)

# Plot complexity parameter table
plotcp(dtree)

# Determine CP that corresponds to the lowest xerror
# Get index of CP with lowest xerror
opt <- which.min(dtree$cptable[,"xerror"])
# get its CP value
cp <- dtree$cptable[opt, "CP"]

# Prune decision tree to decrease overfitting
dtree.pruned <- prune(dtree, cp)

# Display pruned decision tree.  The true values follow the left branches.
plot(dtree.pruned, margin = 0.1);text(dtree.pruned)

# Determine proportion of categorical dependent variable
# CUSTOMIZE DATA: The categorical dependent variable is called target
table(df.train$target)/nrow(df.train)

# prp plots an rpart model
prp(dtree.pruned, type=2, extra=104, fallen.leaves=TRUE, main="Decision Tree")

# predict evaluates the application of a model to a data frame
dtree.pred <- predict(dtree.pruned, df.validate, type="class")
# define classification matrix
# CUSTOMIZE DATA: The categorical dependent variable is called target
dtree.perf <- table(df.validate$target, dtree.pred, dnn=c("Actual", "Predicted"))
dtree.perf

