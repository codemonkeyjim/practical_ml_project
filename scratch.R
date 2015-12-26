library(dplyr)

### Load data files
# testing <- read.csv('data/pml-testing.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'),
#                    colClasses = c('character', 'character', 'numeric', 'numeric', 'Date', 'character', rep('numeric', 153), 'character'))
testing <- read.csv('data/pml-testing.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'))
testing <- testing %>% mutate(is_train = FALSE, classe = NA)

# training = read.csv('data/pml-training.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'),
#                     colClasses = c('character', 'character', 'numeric', 'numeric', 'Date', 'character', rep('numeric', 153), 'character'))
training <- read.csv('data/pml-training.csv', stringsAsFactors = FALSE, na.strings = c("#DIV/0!", '""'))
training <- training %>% mutate(is_train = TRUE, problem_id = NA)

keep_cols <- colnames(testing)[colSums(is.na(testing)) != nrow(testing)] # Columns that aren't all NA in testing
keep_cols <- append(keep_cols, "classe")

### Merge data sets for clean up
dataset <- rbind(testing, training) %>% select(one_of(keep_cols))

dataset <- dataset %>%
  mutate(
    X = as.character(X),
    user_name = factor(user_name),
    classe = factor(classe),
    new_window = (new_window == "yes")
  ) %>%
  mutate_each(funs(as.numeric), matches("arm")) %>%
  mutate_each(funs(as.numeric), matches("belt")) %>%
  mutate_each(funs(as.numeric), matches("dumbbell")) %>%
  rename(max_pitch_forearm = max_picth_forearm)
