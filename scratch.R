library(dplyr)
library(caret)
library(parallel)
library(doMC)
registerDoMC(cores = 4)

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
    -starts_with('kurtosis')
  ) %>%
  mutate(
    X = as.character(X),
    user_name = factor(user_name),
    new_window = (new_window == "yes"),
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

# rm(training_raw, testing_raw, merged)

### Generate a model
set.seed(1309916410)

# Remove attributes not used in the model
training <- training %>%
  select(-X, -user_name, -matches('timestamp'), -matches('window'))

in_xvalid <- createDataPartition(y = training$classe, p = 0.3, list = FALSE)
xvalid <- training[in_xvalid, ]
training <- training[-in_xvalid,]

iter <- 1
ntree <- 20
mtry <- 5

ctrl <- trainControl(
  number = iter,
  verboseIter = TRUE,
  method = 'boot'
)

grid <- data.frame(mtry = mtry)

model <- train(
  select(training, -classe), training$classe,
  method = 'rf',
  do.trace = TRUE,
  proximity = TRUE,
  ntree = ntree,
  trainControl = ctrl,
  tuneGrid <- grid,
  preProcess = c('center', 'scale'),
  tuneLength = 1)

model

pred <- predict(model, newdata = select(xvalid, -classe))
table(pred, xvalid$classe)
