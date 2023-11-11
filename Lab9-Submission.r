# Consider a library as the location where packages are stored.
# Execute the following command to list all the libraries available in your
# computer:
.libPaths()

# One of the libraries should be a folder inside the project if you are using
# renv

# Then execute the following command to see which packages are available in
# each library:
lapply(.libPaths(), list.files)



if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 1. Install and Load the Required Packages ----
## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RRF ----
if (require("RRF")) {
  require("RRF")
} else {
  install.packages("RRF", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


# STEP 2. Load the Dataset ----
data("BreastCancer")
dataset1 <- BreastCancer
BreastCancer_independent_variables <- dataset1[, 1:10]
BreastCancer_dependent_variables <- dataset1[, 11]

# STEP 3. Train the Model ----
# The default random forest algorithm exposes the "mtry" parameter to be tuned.

## The "mtry" parameter ----
# Number of variables randomly sampled as candidates at each split.

# We start by identifying the accuracy by using
# the recommended defaults for each parameter, i.e.,
# mtry=floor(sqrt(ncol(BreastCancer_independent_variables))) or mtry=7

seed <- 7
metric <- "Accuracy"

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(seed)
mtry <- sqrt(ncol(BreastCancer_independent_variables))
tunegrid <- expand.grid(.mtry = mtry)
BreastCancer_model_default_rf <- train(Class ~ ., data = dataset, method = "rf",
                                metric = metric,
                                # enables us to maintain mtry at a constant
                                tuneGrid = tunegrid,
                                trControl = train_control)
print(BreastCancer_model_default_rf)

# STEP 4. Apply a "Random Search" to identify the best parameter value ----
# A random search is good if we are unsure of what the value might be and
# we want to overcome any biases we may have for setting the parameter value
# (like the suggested "mtry" equation above).

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "random")
set.seed(seed)
mtry <- sqrt(ncol(BreastCancer_independent_variables))

BreastCancer_model_random_search_rf <- train(Class ~ ., data = dataset1, method = "rf",
                                      metric = metric,
                                      # enables us to randomly search 12 options
                                      # for the value of mtry
                                      tuneLength = 12,
                                      trControl = train_control)

print(BreastCancer_model_random_search_rf)
plot(BreastCancer_model_random_search_rf)

# STEP 5. Apply a "Grid Search" to identify the best parameter value ----
# Each axis of the grid is an algorithm parameter, and points on the grid are
# specific combinations of parameters.

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "grid")
set.seed(seed)

getModelInfo("RRFglobal")



tunegrid <- expand.grid(.mtry = c(1:10),
                        .coefReg = seq(from = 0.1, to = 1, by = 0.1))

sonar_model_grid_search_rrf_global <- train(Class ~ ., data = dataset, # nolint
                                            method = "RRFglobal",
                                            metric = metric,
                                            tuneGrid = tunegrid,
                                            trControl = train_control)
print(sonar_model_grid_search_rrf_global)
plot(sonar_model_grid_search_rrf_global)