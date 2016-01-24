library(dplyr)
library(caret)
library(randomForest)
# library(parallel)
# library(doParallel)
# library(doMC)

### Load data files
# testing_raw <- read.csv('data/pml-testing.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'),
#                    colClasses = c('character', 'character', 'numeric', 'numeric', 'Date', 'character', rep('numeric', 153), 'character'))
testing_raw <- read.csv('data/pml-testing.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'))
testing_raw <- testing_raw %>% mutate(is_train = FALSE, classe = NA)

# training_raw = read.csv('data/pml-training.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'),
#                     colClasses = c('character', 'character', 'numeric', 'numeric', 'Date', 'character', rep('numeric', 153), 'character'))
training_raw <- read.csv('data/pml-training.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'))
training_raw <- training_raw %>% mutate(is_train = TRUE, problem_id = NA)

### Merge data sets for clean up
merged <- rbind(testing_raw, training_raw)

merged <- merged %>%
  select(
    -starts_with('min'),
    -starts_with('max'),
    -starts_with('avg'),
    -starts_with('stddev'),
    -starts_with('var'),
    -starts_with('amplitude'),
    -starts_with('skewness'),
    -starts_with('kurtosis'),
    -one_of('X', 'user_name'),
    -matches('timestamp'),
    -matches('window')
  ) %>%
  mutate(
    classe = factor(classe)
  ) %>%
  mutate_each(funs(as.numeric), matches("arm")) %>%
  mutate_each(funs(as.numeric), matches("belt")) %>%
  mutate_each(funs(as.numeric), matches("dumbbell"))

training <- merged %>%
  filter(is_train == TRUE) %>%
  select(-is_train, -problem_id)

testing <- merged %>%
  filter(is_train == FALSE) %>%
  select(-is_train, -classe)

rm(training_raw, testing_raw, merged)

### Generate a model
set.seed(130991)

# tuneRes <- tuneRF(select(training, -classe), training$classe, stepFactor=1.5, doBest=TRUE)

iter <- 10
ntree <- 25

ctrl <- trainControl(
  number = iter,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# ncols <- dim(training)[2]
# grid <- expand.grid(mtry = c(floor(sqrt(ncols)), floor(ncols/3), floor(ncols/2), ncols - 1))

predictors = select(training, -classe)
classe = training$classe

# registerDoMC(cores = 4)
# cluster <- makeCluster(4)
# registerDoParallel(cluster)

model <- train(
  x = predictors,
  y = classe,
  method = 'rf',
  preProcess = NULL,
  importance = TRUE,
  do.trace = TRUE,
  proximity = TRUE,
  ntree = ntree,
  trControl = ctrl)
#   trControl = ctrl,
#   tuneGrid <- grid)

# stopCluster(cluster)

varImpPlot(model$finalModel)
plot(model)

trainPred <- predict(model, newdata = training)
testPred <- predict(model, newdata = testing)

trainPredProb <- predict(model, newdata = training, type = "prob")
testPredProb <- predict(model, newdata = testing, type = "prob")
