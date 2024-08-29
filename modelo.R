library(tidyverse)
library(caret)
library(gtsummary)
library(gt)

setwd("C:/Users/anapa/Iniciação científica/git")
dados <- read_delim("dados/censo.csv", delim = ",") |> select(-c(code_muni_censo,abbrev_state))
# dados <- dados |> 
#   mutate(rowIndex = row_number())

# fazendo sample  
dados <- sample_n(dados, size = 10000) |> 
  mutate(across(c(sexo, cor_raca,nivel_instrucao,ocupacao_emprego),~as.factor(.)))

# usando lm()
# lm(rendimento ~ idade + sexo + cor_raca + nivel_instrucao + ocupacao_emprego, data = dados) |> 
#   # summary() |> 
#   tbl_regression(pvalue_fun = label_style_pvalue(digits = 3)) |> 
#   as_gt() |>
#   gtsave("tabelas/summary_lm.tex")

# usando train()
X <- dados |>  select(-c(rendimento))  # Remover a variável dependente
y <- dados$rendimento


train_control <- trainControl(method = "cv", number = 10, savePredictions = "all")  # Validação cruzada com 10 dobras

model <- train(
  x = X, 
  y = y,
  method = "cforest",
  trControl = train_control
)


# modelo
linear_model <- model$finalModel

# tabela regressores com train 
# tbl_regression(linear_model,pvalue_fun = label_style_pvalue(digits = 3)) |> 
#   as_gt() |>
#   gtsave("tabelas/summary_train.tex")

predictions <- model$pred
predictions |> 
  left_join(dados |> select(sexo, rowIndex), join_by(rowIndex))


# predicoes com dados originais
previsoes_originais <- predict(linear_model, newdata = dados)
# predictions <- predictions[order(predictions$rowIndex), ]

dados_predicao <- dados |> 
  mutate(predicao = previsoes_originais)

# Aplicação da Função de Alternância
# Alterar o sexo e gerar previsões com o modelo treinado

dados_alternation <- dados |>  
  mutate(sexo = ifelse(dados$sexo == "Masculino", "Feminino", "Masculino"))

# treinando o modelo pros dados alternados
X_alternation <- dados_alternation |>  select(-c(rendimento))  # Remover a variável dependente
y_alternation <- dados_alternation$rendimento

model_alternation <- train(
  x = X_alternation, 
  y = y_alternation,
  method = "lm",
  trControl = train_control
)

linear_model_alternation <- model_alternation$finalModel

# predicoes com dados originais
previsoes_alternation <- predict(linear_model_alternation, newdata = dados_alternation)

dados_alternation_predicao <- dados_alteration |> 
  mutate(predicao = previsoes_alternation)





teste <- dados |> 
  mutate(pred1=previsoes_originais, pred2=previsoes_alternation)


















# # média das previsões por sexo e dobra
# predictions
# predictions_summary <- predictions %>%
#   group_by(Resample) %>%
#   summarise(mean_prediction = mean(pred))



# # Calcular a média das previsões por sexo e por dobra
# # predictions_summary <- 
# predictions |> 
#   group_by(Resample, sexo) |> 
#   summarise(mean_prediction = mean(pred))
# 
# # função de alternância
# dados_alternado <- dados
# dados_alternado$sexo <- ifelse(dados_alternado$sexo == "Masculino", "Feminino", "Masculino")
# # Previsões originais e alternadas
# previsoes_originais <- predict(linear_model, newdata = dados)
# previsoes_alternadas <- predict(linear_model, newdata = dados_alternado)
# 
# # 3. Cálculo da Divergência KL
# # Normalizar as previsões para garantir que somem 1 (necessário para a divergência KL)
# 
# p <- previsoes_originais / sum(previsoes_originais)
# q <- previsoes_alternadas / sum(previsoes_alternadas)
# 
# kl_divergence <- KL.divergence(p, q)
# print(paste("Divergência KL:", kl_divergence))

