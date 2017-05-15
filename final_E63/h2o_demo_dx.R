library(SparkR)
library(h2o)
h2o.init()



# read in data and import into h2o environment
train_x = read.csv("/home/dx/Data/CashBus/train_x.csv")
setDT(train_x)
train_y = read.csv("/home/dx/Data/CashBus/train_y.csv")
setDT(train_y)
myX = names(train_x)[names(train_x) != "uid"]
myY = names(train_y)[names(train_y) != "uid"]

cashbus_data <- merge(train_x, train_y, by="uid")

# Split data into train and validation set
data  <- as.h2o(cashbus_data)
rand  <- h2o.runif(data)
train <- data[rand$rnd <= 0.8, ]
valid <- data[rand$rnd > 0.8, ]

# Tune GBM model using grid search
models <- c()
for(ntrees in seq(100, 1000, 100)){
  for(maxDepths in seq(1, 5, 1)){
    for(learnRate in seq(0.05, 10, 0.05)){
      gbm_model <- h2o.gbm(x = myX, y = myY, training_frame = train, validation_frame = valid, balance_classes = T,
                           learn_rate = learnRate, score_each_iteration = T, ntrees = ntrees, max_depth = maxDepths, 
                           model_id = paste0("LR_", learnRate, "_NT_", ntrees, "_MD_", maxDepths))
      models <- c(models, gbm_model)
    }
  }
}

# Evaluate models on validation set
min_rmse_on_valid <- c()
for(model in models) {
  sh <- h2o.scoreHistory(model)
  best_model <- sh[sh$validation_rmse == min(sh$validation_rmse),]
  min_rmse_on_valid <- rbind(min_rmse_on_valid, best_model)
}

# Pick the best model measured by min rmse
best_model = which(min_rmse_on_valid$validation_rmse == min(min_rmse_on_valid$validation_rmse))
gbm_model = models[[8]]

# Print best model 
print("The variable importance for the GBM model...")
print(h2o.varimp(gbm_model))
h2o.rmse(gbm_model)
h2o.rmse(gbm_model, valid = T)

