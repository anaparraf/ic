library(tidyverse)
library(caret)
library(gtsummary)
library(gt)


setwd("C:/Users/anapa/Iniciação científica/git")
dados <- read_delim("dados/censo.csv", delim = ",") |> select(-c(code_muni_censo,abbrev_state))

# lm(rendimento ~ idade + sexo + cor_raca + nivel_instrucao, data = dados) |> 
#   summary()

# usnado lm()
lm(rendimento ~ idade + sexo + cor_raca + nivel_instrucao + ocupacao_emprego, data = dados) |> 
  # summary() |> 
  tbl_regression(pvalue_fun = label_style_pvalue(digits = 3)) |> 
  as_gt() |>
  gtsave("tabelas/summary_lm.tex")

# -----------------------------

# usando train()
dados$sexo <- as.factor(dados$sexo)  # Certifique-se de que "Sexo" é uma variável categórica
X <- dados |>  select(-rendimento)  # Remover a variável dependente
y <- dados$rendimento

train_control <- trainControl(method = "cv", number = 10)  # Validação cruzada com 10 dobras

model <- train(
  x = X, 
  y = y,
  method = "lm",
  trControl = train_control
)
# print(model)

linear_model <- model$finalModel
# summary(linear_model)

tbl_regression(linear_model,pvalue_fun = label_style_pvalue(digits = 3)) |> 
  as_gt() |>
  gtsave("tabelas/summary_train.tex")


# predictions <- predict(model, newdata = X)
# 
# dados$predictions <- predictions

