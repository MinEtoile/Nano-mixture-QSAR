## RF Algorithm ##

# Load library source
source("source.R")

# Load file
QSARFile <- dlg_open(title = "Select file")$res
Table1 <- read.xlsx(QSARFile, sheet = 1, startRow = 1, colNames = TRUE,
                    rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                    skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                    namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
Table2 <- Table1

TestPerfomance <- data.frame( Seed = 0,
                              R2_test = 0,
                              R2_CV = 0,
                              RMSE_test = 0,
                              RMSE_CV = 0,
                              MAE_test = 0,
                              MAE_CV = 0)

# Set seed number
seednumber <- c(4965); set.seed(seednumber)

## 80% of the sample size
in_rows <- createDataPartition(y = Table1$logEC, p = 0.8, list = FALSE)

# Training models
train <- rbind(Table1[in_rows, ])
test <- Table1[-in_rows, ]

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 20)
RFmodel <- train(logEC ~ ., data = train, method = "rf", trControl = train.control)
print(RFmodel)



# Model performance
predictions <- RFmodel %>% predict(test)
predictions_train <- RFmodel %>% predict(train)



# Compute the R2, RMSE and MAE for train, test and cross validation

TestPerfomance[1,1] <- seednumber

TestPerfomance[1,2] <- R2(predictions,test$logEC)
TestPerfomance[1,3] <- R2(predictions_train,train$logEC)
TestPerfomance[1,4] <- RMSE(predictions,test$logEC)
TestPerfomance[1,5] <- RMSE(predictions_train,train$logEC)
TestPerfomance[1,6] <- MAE(predictions,test$logEC)
TestPerfomance[1,7] <- MAE(predictions_train,train$logEC)


# Estimate Descriptor importance
DescImportance <- data.frame(Descriptor = row.names(varImp(RFmodel, scale=TRUE)$importance),
                             Value = varImp(RFmodel, scale=TRUE)$importance)
colnames(DescImportance) <- c("Descriptor", "Value")


# Save output to excel file
Table3 <- train; Table3$Predict.logEC10 <- predictions_train
Table4 <- test; Table4$Predict.logEC10 <- predictions

ExcelFile <- createWorkbook("QNAR_RF")
addWorksheet(ExcelFile, "train")
writeData(ExcelFile, sheet = 1, Table3)
addWorksheet(ExcelFile, "test")
writeData(ExcelFile, sheet = 2, Table4)
addWorksheet(ExcelFile, "Performance")
writeData(ExcelFile, sheet = 3, TestPerfomance)
addWorksheet(ExcelFile, "Desc_Importance")
writeData(ExcelFile, sheet = 4, DescImportance)
addWorksheet(ExcelFile, "ModelInfo")
writeData(ExcelFile, sheet = 5, capture.output(RFmodel))
saveWorkbook(ExcelFile, paste(dirname(QSARFile),"/RF_",tools::file_path_sans_ext(basename(QSARFile)),".xlsx", sep = ""), overwrite = TRUE)


# Save model for later use
RFDmix <- RFmodel
save(RFDmix, file = paste(dirname(QSARFile),"/RF",tools::file_path_sans_ext(basename(QSARFile)),".Rdata", sep = ""))

