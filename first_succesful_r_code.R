#install.packages("randomForest")
library(data.table)
library(tidyverse)
library(rpart)
library(rstudioapi)
library(rpart)
library(dplyr)
data.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
train <- fread(file.path(data.dir, 'data/train.csv')) %>% as.data.frame()
test <- fread(file.path(data.dir, 'data/test.csv')) %>% as.data.frame()
ss <- fread(file.path(data.dir, 'data/samplesubmission.csv')) %>% as.data.frame()
#str()
#table()#summary()
test$total_cost<-0
test_ids <- test_df$ID
train$dataset <- 'train'
test$dataset <- 'test'
df <- bind_rows(train, test)
df <- df %>%
  mutate(across(where(is.character), as.factor))#%>% #drop_na()
train <- df %>% filter(dataset == 'train') %>% select(-dataset)
test_f <- df %>% filter(dataset == 'test') %>% select(-dataset,total_cost)
train_f <- train %>% select(-total_cost) 
train_target <- train$total_cost
model <- rpart(total_cost ~ ., data = train, method = "anova")
test_pred <- predict(model, newdata = test_f)
finalOutput <- data.frame(ID = test_ids, Predicted_total_cost = test_pred)
write.csv(finalOutput, file.path(data.dir, 'predictions.csv'), row.names = FALSE)

