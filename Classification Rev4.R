# Calculating and plotting entropy for the binary case

?log2
# define function to measure the performance of a classification model
entropy <- function(p){
  ifelse(p==0 | p==1, 0, -(p*log2(p)+(1-p)*log2(1-p)))
}

entropy(0.5)
entropy(0.99999)
entropy(1)
entropy(0)

x <- c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
y <- entropy(x)

plot(x,y, type="b", xlab="p", ylab="Entropy")

IG <- function(p, p1, p2, prop1){
  entropy(p) - (prop1*entropy(p1)+(1-prop1)*entropy(p2))
}

IG(0.53, 0.92, 0.24, 0.43)

# We will now use the rpart package for decision tree classification

pkgs <- c("rpart", "rpart.plot", "party", "randomForest", "e1071")
#install.packages(pkgs,lib="C:/Rick/R/Rlib",depend=TRUE)
install.packages(pkgs, depend=TRUE)

?install.packages

?sample
?table
?rpart

# CUSTOMIZE DATA FILE: Define website that provides data file
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
# CUSTOMIZE DATA FILE: Define data file
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
# Paste converts its arguments (via as.character) to character strings, and concatenates them (separating them by the string given by sep)
url <- paste(loc, ds, sep="")

# CUSTOMIZE DATA FILE: read.table reads a file in table format and creates a data frame from it
breast <- read.table(url, sep=",", header=FALSE, na.strings="?")

# CUSTOMIZE DATA FILE: Define column names
# CUSTOMIZE DATA FILE: The categorical dependent variable is called target
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "marginalAdhesion", 
                   "singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "target")

# Define data frame containing all columns except first column
df <- breast[-1]
# CUSTOMIZE DATA FILE: factor relabels categorical values for a categorical variable
# CUSTOMIZE DATA FILE: The categorical dependent variable is called target
df$target <- factor(df$target, levels=c(2,4),
                   labels=c("benign", "malignant"))

# Define seed for random number generator
set.seed(1234)
# sample takes a random sample of the specified size from the elements of x
train <- sample(nrow(df), 0.7*nrow(df))

# Define training data frame using random sample of observations
df.train <- df[train,]
# Define validation data frame using all observations not in training data frame
df.validate <- df[-train,]

# CUSTOMIZE DATA: Table counts the observations for each categorical value of target
# CUSTOMIZE DATA: The categorical dependent variable is called target
table(df.train$target)
table(df.validate$target)

# Decision Tree

# library loads add-on packages
#library(rpart,lib="C:/Rick/R/Rlib")
library(rpart)

# Define seed for random number generator
set.seed(1234)

# Fit a recursive partitioning model
# CUSTOMIZE DATA: the first parameter target specifies the categorical dependent variable 
# CUSTOMIZE DATA: The categorical dependent variable is called target
dtree <- rpart(target ~ ., data=df.train, method="class",
               parms=list(split="information"))

# Summarize the decision tree including decision nodes and leaf nodes
# The decision tree nodes are described row by row, then left to right
summary(dtree)

# Display decision tree.  The true values follow the left branches.
plot(dtree);text(dtree)

# Display decision tree complexity parameter table which is a matrix of information on the optimal prunings based on a complexity parameter
# Identify CP that corresponds to the lowest xerror
dtree$cptable

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
plot(dtree.pruned);text(dtree.pruned)

# class displays the object class
class(dtree$cptable)

# names displays the names of an object
names(dtree)

#library(rpart.plot,lib="C:/Rick/R/Rlib")
library(rpart.plot)

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

# Random Forest

#library(randomForest,lib="C:/Rick/R/Rlib")
library(randomForest)

# Define seed for random number generator
set.seed(1234)
# Fit a random forest model
# The na.action=na.roughfix option replaces missing values on numeric variables with column medians and missing values on categorical variables with the modal category for that variable
# CUSTOMIZE DATA: The categorical dependent variable is called target
forest <- randomForest(target ~ ., data=df.train,
                           na.action=na.roughfix,
                           importance=TRUE)

# Display a summary for a random forest model
forest

# Display variable importance measures for a random forest model
importance(forest, type=2)

# predict evaluates the application of a model to a data frame
forest.pred <- predict(forest, df.validate)
# define classification matrix
# CUSTOMIZE DATA: The categorical dependent variable is called target
forest.perf <- table(df.validate$target, forest.pred,
                     dnn=c("Actual", "Predicted"))
forest.perf