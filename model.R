library(caret)

#### Read Data ####

train_data = read.csv("./data/train_data.csv")
train_data = train_data[-which(names(train_data) %in% "X")]
train_data$Made.Donation.in.March.2007 = make.names(train_data$Made.Donation.in.March.2007)
test_data = read.csv("./data/test_data.csv")
sample_submit = read.csv("./data/BloodDonationSubmissionFormat.csv")

# Train validation split in train dataset
train_idx = createDataPartition(train_data$Made.Donation.in.March.2007, p = .7, list = FALSE, times = 1)
training_data = train_data[train_idx,]
testing_data = train_data[-train_idx,]

# Setup Cross Validation
tr_control = trainControl(method = "repeatedcv", number = 10, repeats = 10,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

# Logistics Regression
log_reg = train(Made.Donation.in.March.2007 ~ ., data = training_data,  method = "glm", trControl = tr_control, metric = "ROC")

