library(tidyverse)
library(caret)
library(gtsummary)
library(gt)
library(randomForest)
# library(entropy)
library(philentropy)

# set diretório e random seed
setwd("C:/Users/anapa/Iniciação científica/git")
set.seed(42)

# dados do censo
dados <- read_delim("dados/censo.csv", delim = ",") |> select(-c(code_muni_censo,abbrev_state))

# fazendo sample  pra modelar
dados <- sample_n(dados, size = 10000) |> 
  mutate(across(c(sexo, cor_raca,nivel_instrucao,ocupacao_emprego),~as.factor(.))) |> 
  select(sexo, rendimento, nivel_instrucao, idade, cor_raca, ocupacao_emprego)

# definir folds pra comparação
folds <- createFolds(dados$rendimento, k = 10, list = TRUE, returnTrain = TRUE)

# controle de treinamento usando os mesmos folds
trainControl <- trainControl(method = "cv", number = 10, 
                             index = folds, savePredictions = "all")

# treina random forest
rfModel <- train(rendimento ~ ., data = dados, method = "rf", 
                 trControl = trainControl, ntree=500)

print(rfModel) #mtry = 3

# alternância das variáveis de interesse: homem->mulher; mulher->homem
dados_alternation <- dados |> 
  mutate(sexo = ifelse(dados$sexo == "Masculino", "Feminino", "Masculino"))

# treina random forest com dados alternados
rfModel_alternation <- train(rendimento ~ ., data = dados_alternation, method = "rf", 
                             trControl = trainControl, ntree = 500)

print(rfModel_alternation) # mtry = 2

# predições
pred_originals <- rfModel$pred
pred_alternations <- rfModel_alternation$pred

# group_by predições por fold - homem dados originais
homem_original <- pred_originals |>
  filter(mtry ==3) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Masculino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado = 'Homem')

# group_by predições por fold - homem dados alternados
homem_alternation <- pred_alternations |>
  filter(mtry ==2) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Masculino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado='Mulher->Homem')


homem_original |> 
  bind_rows(homem_alternation) |> 
  ggplot(aes(x = as.factor(Resample) |> as.numeric(), y = mean_prediction))+
  geom_line(aes(color =as.factor(alterado))) +
  geom_point(aes(color =as.factor(alterado))) +
  scale_x_continuous(labels = 1:10, breaks = 1:10) +
  ylim(c(700,1200)) +
  labs(x = '10-Fold', y = 'Média do salário', color = '')+
# , title = 'Previsão média de renda por gênero')
  theme_classic()

# group_by predições por fold - mulher dados originais
mulher_original <- pred_originals |>
  filter(mtry ==3) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Feminino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado= 'Mulher')

# group_by predições por fold - mulher dados alternados
mulher_alternation <- pred_alternations |>
  filter(mtry ==2) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Feminino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado = 'Homem->Mulher')


mulher_original |> 
  bind_rows(mulher_alternation) |> 
  ggplot(aes(x = as.factor(Resample) |> as.numeric(), y = mean_prediction))+
  geom_line(aes(color =as.factor(alterado))) +
  geom_point(aes(color =as.factor(alterado))) +
  scale_x_continuous(labels = 1:10, breaks = 1:10) +
  ylim(c(700,1200)) +
  labs(x = '10-Fold', y = 'Média do salário', color = '')+
  theme_classic()
# , title = 'Previsão média de renda por gênero')
  





















# base com predições e genero original, genero alternado, pred original e pred alternada pra mtry == 2


predicoes_teste <- 
  testData |> 
  select(sexo) |>
  mutate(predi_orig = predictions, predi_alt = predictions_alternation)
  
# juntando predições com dado original e alternado
todas_pred <- pred_originals |> 
  filter(mtry==2) |>
  rename(pred_orig = pred, fold_orig= Resample, obs_orig = obs) |> 
  select(rowIndex, pred_orig, fold_orig, obs_orig) |> 
  left_join(pred_alternations |> filter(mtry ==2), join_by(rowIndex)) |> 
  rename(pred_alt = pred, obs_alt = obs, fold_alt = Resample) 

# juntando predições com genero original e alternado
resultados <- testData |>
  rename(sexo_orig = sexo) |> 
  mutate(rowIndex = row_number()) |> 
  select(rowIndex, sexo_orig, rendimento) |> 
  left_join(testData_alternation |> mutate(rowIndex = row_number()) |> select(sexo,  rowIndex), 
            join_by(rowIndex)) |> 
  left_join(todas_pred, join_by(rowIndex)) |> 
  rename(sexo_alt=sexo) 


# ------------------------
# densidade e KL

# density_orig <- density(todas_pred$pred_orig)
# density_alt <- density(todas_pred$pred_alt)
# 
# # Interpolando as densidades para os mesmos pontos
# # interp_density_alt <- approx(density_alt$x, density_alt$y, xout = density_orig$x)$y
# 
# # Normalizando para obter distribuições de probabilidade discretas
# prob_orig <- density_orig$y / sum(density_orig$y)
# prob_alt <- density_alt$y / sum(density_alt$y)
# 
# # Calculando a divergência KL
# kl_value <- KL.empirical(prob_orig, prob_alt)

# density()), você precisa convertê-las em uma distribuição de probabilidade discreta que soma 1.


# divergencia KL geral

density_orig <- density(todas_pred$pred_orig)
density_alt <- density(todas_pred$pred_alt)

# Transformar as densidades em distribuições de probabilidade discretas
P <- density_orig$y / sum(density_orig$y)
Q <- density_alt$y / sum(density_alt$y)


P <- todas_pred$pred_orig
Q <- todas_pred$pred_alt

#rbind distributions into one matrix
x <- rbind(P,Q)

#calculate KL divergence
KL(x, unit='log')

# # Compute KL divergence
# kl_divergence <- KL(x, unit = "log")
# 
# print(kl_divergence)


# -----------------
 # P DO ORIGINAL
calculate_prob_by_fold <- function(df) {
  # Calcular a densidade para pred_orig
  density_orig <- density(df$pred_orig)
  
  # Normalizar para obter a distribuição de probabilidade P
  prob_orig <- density_orig$y / sum(density_orig$y)
  
  # Retornar como data frame com P_fold
  data.frame(P_fold = prob_orig)
}

# Aplicar a função para calcular P por fold e desaninhando os resultados
prob_results <- todas_pred |> 
  group_by(fold_orig) |> 
  do(calculate_prob_by_fold(.)) |> 
  ungroup() |> 

# Visualizar os resultados
print(prob_results)


 # Q DO ALTERNADO
calculate_prob_alt_by_fold <- function(df) {
  # Calcular a densidade para pred_alt
  density_alt <- density(df$pred_alt)
  
  # Normalizar para obter a distribuição de probabilidade Q
  prob_alt <- density_alt$y / sum(density_alt$y)
  
  # Retornar como data frame com Q_fold
  data.frame(Q_fold = prob_alt)
}

# Aplicar a função para calcular Q por fold e desaninhando os resultados
prob_results_alt <- todas_pred |> 
  group_by(fold_orig) |> 
  do(calculate_prob_alt_by_fold(.)) |> 
  ungroup()

# Visualizar os resultados
print(prob_results_alt)


# div KL por fold

folds_p_q <- prob_results |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt |> mutate(row=row_number()), join_by(row))

prob_results |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt |> mutate(row=row_number()), join_by(row)) |> 
  group_by(fold_orig.x) |> 
  mutate(x = rbind(P_fold,Q_fold))
 

#calculate KL divergence
# KL(x, unit='log')
#   mutate(kl =  )
#   

# Função para calcular a divergência KL entre P e Q
calculate_kl <- function(P, Q) {
  # Combinar as distribuições em uma matriz
  kl_matrix <- rbind(P, Q)
  
  # Calcular a divergência KL
  kl_value <- KL(kl_matrix, unit = 'log')
  
  return(kl_value)
}

# Aplicar o cálculo da divergência KL por fold

kl_geral <- prob_results |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt |> mutate(row=row_number()), join_by(row)) |> 
  group_by(fold_orig.x) |> 
  mutate(kl = calculate_kl(P_fold,Q_fold)) |> 
  summarise(kl = mean(kl))
  
print(kl_geral)
# 
# library(ggplot2)
# 
# # Convertendo o nome dos folds para um formato numérico para o eixo x
# kl_geral <- kl_geral %>%
#   mutate(Fold = as.numeric(gsub("Fold", "", fold_orig.x)))
# 
# # Criando o gráfico
# ggplot(kl_geral, aes(x = Fold, y = kl)) +
#   geom_line(color = "red", linetype = "dashed", size = 1) +
#   geom_point(color = "red", size = 3) +
#   scale_x_continuous(breaks = 1:10) +
#   scale_y_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
#   labs(title = "Bias Evaluation Using KL Divergence Male/Female",
#        x = "10-fold",
#        y = "KL Divergence") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
#     axis.title = element_text(size = 12),
#     axis.text = element_text(size = 10),
#     panel.grid.major = element_line(color = "gray", linetype = "dotted"),
#     panel.grid.minor = element_blank()
#   )




                 # MALE TO FEMALE

# -----------------

# Aplicar a função para calcular P por fold e desaninhando os resultados
prob_results <- todas_pred |> 
  group_by(fold_orig) |> 
  do(calculate_prob_by_fold(.)) |> 
  ungroup() |> 
  
  # Visualizar os resultados
  print(prob_results)


# Q DO ALTERNADO
calculate_prob_alt_by_fold <- function(df) {
  # Calcular a densidade para pred_alt
  density_alt <- density(df$pred_alt)
  
  # Normalizar para obter a distribuição de probabilidade Q
  prob_alt <- density_alt$y / sum(density_alt$y)
  
  # Retornar como data frame com Q_fold
  data.frame(Q_fold = prob_alt)
}

# Aplicar a função para calcular Q por fold e desaninhando os resultados
prob_results_alt <- todas_pred |> 
  group_by(fold_orig) |> 
  do(calculate_prob_alt_by_fold(.)) |> 
  ungroup()

# Visualizar os resultados
print(prob_results_alt)


# div KL por fold

folds_p_q <- prob_results |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt |> mutate(row=row_number()), join_by(row))

prob_results |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt |> mutate(row=row_number()), join_by(row)) |> 
  group_by(fold_orig.x) |> 
  mutate(x = rbind(P_fold,Q_fold))


#calculate KL divergence
# KL(x, unit='log')
#   mutate(kl =  )
#   

# Função para calcular a divergência KL entre P e Q
calculate_kl <- function(P, Q) {
  # Combinar as distribuições em uma matriz
  kl_matrix <- rbind(P, Q)
  
  # Calcular a divergência KL
  kl_value <- KL(kl_matrix, unit = 'log')
  
  return(kl_value)
}

# Aplicar o cálculo da divergência KL por fold

kl_geral <- prob_results |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt |> mutate(row=row_number()), join_by(row)) |> 
  group_by(fold_orig.x) |> 
  mutate(kl = calculate_kl(P_fold,Q_fold)) |> 
  summarise(kl = mean(kl))

print(kl_geral)












