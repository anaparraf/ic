library(tidyverse)
library(caret)
library(gtsummary)
library(gt)
library(randomForest)
library(entropy)

setwd("C:/Users/anapa/Iniciação científica/git")
dados <- read_delim("dados/censo.csv", delim = ",") |> select(-c(code_muni_censo,abbrev_state))
# dados <- dados |> 
#   mutate(rowIndex = row_number())

set.seed(42)

# fazendo sample  
dados <- sample_n(dados, size = 10000) |> 
  mutate(across(c(sexo, cor_raca,nivel_instrucao,ocupacao_emprego),~as.factor(.))) |> 
  select(sexo, rendimento, nivel_instrucao, idade)


# treinando com dados originais
trainIndex <- createDataPartition(dados$rendimento, p = 0.7, list = FALSE)
trainData <- dados[trainIndex, ]
testData <- dados[-trainIndex, ]

# Define the training control

folds <- createFolds(trainData$rendimento, k = 10, list = TRUE, returnTrain = TRUE)

# Definir controle de treinamento usando os mesmos folds
trainControl <- trainControl(method = "cv", number = 10, 
                             index = folds, savePredictions = "all")
# 
# trainControl <- trainControl(method = "cv", number = 10, savePredictions = "all")

# Train the Random Forest model
rfModel <- train(rendimento ~ ., data = trainData, method = "rf", 
                 trControl = trainControl, ntree=500)

# Print the model
print(rfModel)


# Predict on the test data
predictions <- predict(rfModel, newdata = testData)

dados_predicao <- testData |> 
  mutate(predicao = predictions)

# -----------------------------------------------------------------------------------------
# treinando com dados alternados 
# dados_alternation <- dados |>  
#   mutate(sexo = ifelse(dados$sexo == "Masculino", "Feminino", "Masculino"))

# trainIndex_alternation <- createDataPartition(dados_alternation$rendimento, p = 0.7, list = FALSE)
# trainData_alternation <- dados_alternation[trainIndex_alternation, ]
# testData_alternation <- dados_alternation[-trainIndex_alternation, ]

# funçaõ de alternancia
trainData_alternation <- trainData |> 
  mutate(sexo = ifelse(trainData$sexo == "Masculino", "Feminino", "Masculino"))
  
testData_alternation <- testData |> 
  mutate(sexo = ifelse(testData$sexo == "Masculino", "Feminino", "Masculino"))

# Train the Random Forest model
rfModel_alternation <- train(rendimento ~ ., data = trainData_alternation, method = "rf", 
                             trControl = trainControl, ntree = 500)

# Print the model
print(rfModel_alternation)

# Predict on the test data
predictions_alternation <- predict(rfModel_alternation, newdata = testData_alternation)

dados_pred_alternation <- testData_alternation |> 
  mutate(predicao = predictions_alternation)

# -----------------------------------------------------------------
# calculado a divergencia KL para cada fold

# predições
pred_originals <- rfModel$pred
pred_alternations <- rfModel_alternation$pred

# group_by predições por fold (sem genero)
pred_originals |> 
  group_by(Resample) |> 
  summarise(mean_prediction = mean(pred))

pred_alternations |> 
  group_by(Resample) |> 
  summarise(mean_prediction = mean(pred))

# plotar mean prediction por fold pros dados originais e alternados


# base com predições e genero original, genero alternado, pred original e pred alternada pra mtry == 2


# juntando predições com dado original e alternado
todas_pred <- pred_originals |> 
  filter(mtry==2) |>
  rename(pred_orig = pred, fold_orig= Resample, obs_orig = obs) |> 
  select(rowIndex, pred_orig, fold_orig, obs_orig) |> 
  left_join(pred_alternations |> filter(mtry ==2), join_by(rowIndex))

# juntando predições com genero original e alternado
testData |>
  rename(sexo_orig = sexo) |> 
  mutate(rowIndex = row_number()) |> 
  select(rowIndex, sexo_orig, rendimento) |> 
  left_join(testData_alternation |> mutate(rowIndex = row_number()) |> select(sexo,  rowIndex), 
            join_by(rowIndex)) |> 
  left_join(todas_pred, join_by(rowIndex)) |> 
  View()

  


# # Predict on the reordered test data
# predictions <- predict(rfModel, newdata = testData)
# predictions_alternation <- predict(rfModel_alternation, newdata = testData_alternation)
# 
# # Combine predictions into a single dataset
# teste <- testData |> 
#   mutate(pred1 = predictions, pred2 = predictions_alternation, diff = pred1-pred2)
# 
# summary(teste$diff)