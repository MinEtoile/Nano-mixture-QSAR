# Written by Minju Na
# Calculate residuals and euclidean distance
# Define applicability domain

# load library
library(svDialogs) 
library(openxlsx)
library(caret)
library(ggplot2)
library(easyGgplot2)

QSARFile <- dlg_open(title = "Select file")$res
Table1 <- read.xlsx(QSARFile, sheet = 1, startRow = 1, colNames = TRUE,
                    rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                    skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                    namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

Table2 <- read.xlsx(QSARFile, sheet = 2, startRow = 1, colNames = TRUE,
                    rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                    skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                    namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

test_observed <- vector()
test_predicted <- vector()

train_observed <- vector()
train_predicted <- vector()

train_euc <- vector()
test_euc <- vector()

fin_test <- vector()
fin_train <- vector()

test_data <- Table1
test_observed <- Table1$observed
test_predicted <- Table1$predicted
train_data <- Table2
train_observed <- Table2$observed
train_predicted <- Table2$predicted

# calculate the standardized residuals of test data
test_lm <- lm(predicted ~ observed, data = test_data)
standard_test <- rstandard(test_lm)

# calculate the standardized residuals of train data
train_lm <- lm(predicted_t ~ observed_t, data = train_data)
standard_train <- rstandard(train_lm)


# Define euclidean distance function of test data
for (i in 1:nrow(test_data)) {
  a <- test_observed[i]
  b <- test_predicted[i]
  test_euclidean <- function(a, b) sqrt(sum((a - b)^2))
  # calculate euclidean distance of test data
  test_euc[i] <- test_euclidean(a,b)
  
  fin_test <- cbind(test_euc[i], standard_test[i])
}

# Define euclidean distance function of train data
for (k in 1:nrow(train_data)) {
  c <- train_observed[k]
  d <- train_predicted[k]
  train_euclidean <- function(c, d) sqrt(sum((c - d)^2))
  # calculate euclidean distance of train data
  train_euc[k] <- train_euclidean(c,d)
  
  fin_train <- cbind(train_euc[k], standard_train[k])
}

euc <- c(train_euc, test_euc)
res <- c(standard_train, standard_test)

# plot applicability domain
library(car)

dataEllipse(train_euc, standard_train, levels = c(0.95), col = c('grey', 'blue'), xlim=c(-0.5,0.8), ylim = c(-5.0, 5.0), pch = 22, ylab = 'Standardized Residuals', xlab = 'Euclidean Distance')
dataEllipse(train_euc, standard_train, levels = c(0.99), add=TRUE, col = c('grey', 'red'), xlim=c(-0.5,1.2), ylim = c(-4.5, 4.5), ylab = 'Standardized Residuals', xlab = 'Euclidean Distance')
dataEllipse(test_euc, standard_test, col = c('orange', 'NA'),add=TRUE)

# END
