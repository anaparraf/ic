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
dados <- sample_n(dados, size = 30000) |> 
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

print(rfModel) #mtry = 3  novo =2

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
  filter(mtry ==2) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Masculino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado = 'Masculino')

# group_by predições por fold - homem dados alternados
homem_alternation <- pred_alternations |>
  filter(mtry ==2) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Masculino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado='Feminino->Masculino')

# group_by predições por fold - mulher dados originais
mulher_original <- pred_originals |>
  filter(mtry ==2) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Feminino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado= 'Feminino')

# group_by predições por fold - mulher dados alternados
mulher_alternation <- pred_alternations |>
  filter(mtry ==2) |> 
  left_join(dados |> mutate(rowIndex = row_number()), join_by(rowIndex)) |> 
  group_by(Resample) |>
  filter(sexo == 'Feminino') |> 
  summarise(mean_prediction = mean(pred)) |> 
  mutate(alterado = 'Masculino->Feminino')

# ggplot mulheres
plot_mulher <- mulher_original |> 
  bind_rows(homem_alternation) |> 
  ggplot(aes(x = as.factor(Resample) |> as.numeric(), y = mean_prediction))+
  geom_line(aes(color =as.factor(alterado))) +
  geom_point(aes(color =as.factor(alterado))) +
  scale_x_continuous(labels = 1:10, breaks = 1:10) +
  ylim(c(700,1200)) +
  labs(x = '10-Fold', y = 'Média do salário', color = '')+
  theme_classic()
# , title = 'Previsão média de renda por gênero')

ggsave("pred_mulher.pdf", plot = plot_mulher, width = 8, height = 6)

# ggplot homens
plot_homem <- homem_original |> 
  bind_rows(mulher_alternation) |> 
  ggplot(aes(x = as.factor(Resample) |> as.numeric(), y = mean_prediction))+
  geom_line(aes(color =as.factor(alterado))) +
  geom_point(aes(color =as.factor(alterado))) +
  scale_x_continuous(labels = 1:10, breaks = 1:10) +
  ylim(c(700,1200)) +
  labs(x = '10-Fold', y = 'Média do salário', color = '')+
  # , title = 'Previsão média de renda por gênero')
  theme_classic()

ggsave("pred_homem.pdf", plot = plot_homem, width = 8, height = 6)

# juntando predições com dado original e alternado
todas_pred <- pred_originals |> 
  filter(mtry==2) |>
  rename(pred_orig = pred, fold_orig= Resample, obs_orig = obs) |> 
  select(rowIndex, pred_orig, fold_orig, obs_orig) |> 
  left_join(pred_alternations |> filter(mtry ==2), join_by(rowIndex)) |> 
  rename(pred_alt = pred, obs_alt = obs, fold_alt = Resample) 

# juntando predições com genero original e alternado
resultados <- dados |>
  rename(sexo_orig = sexo) |> 
  mutate(rowIndex = row_number()) |> 
  select(rowIndex, sexo_orig, rendimento) |> 
  left_join(dados_alternation |> mutate(rowIndex = row_number()) |> select(sexo,  rowIndex), 
            join_by(rowIndex)) |> 
  left_join(todas_pred, join_by(rowIndex)) |> 
  rename(sexo_alt=sexo) 

# -----------------
 # P DO ORIGINAL

# funçoes  calcula Q
calculate_prob_by_fold <- function(df) {
  # Calcular a densidade para pred_orig
  density_orig <- density(df$pred_orig)
  # Normalizar para obter a distribuição de probabilidade P
  prob_orig <- density_orig$y / sum(density_orig$y)
  # Retornar como data frame com P_fold
  data.frame(P_fold = prob_orig)}

# função calcula Q
calculate_prob_alt_by_fold <- function(df) {
  # Calcular a densidade para pred_alt
  density_alt <- density(df$pred_alt)
  # Normalizar para obter a distribuição de probabilidade Q
  prob_alt <- density_alt$y / sum(density_alt$y)
  # Retornar como data frame com Q_fold
  data.frame(Q_fold = prob_alt)}

# função para calcular a divergência KL entre P e Q
calculate_kl <- function(P, Q){
  # Combinar as distribuições em uma matriz
  kl_matrix <- rbind(P, Q)
  # Calcular a divergência KL
  kl_value <- KL(kl_matrix, unit = 'log2')
  return(kl_value)}

# KL - homem
# Aplicar a função para calcular P por fold e desaninhando os resultados
prob_results_homem <- resultados |> 
  filter(sexo_orig == "Masculino") |> 
  group_by(fold_orig) |> 
  do(calculate_prob_by_fold(.)) |> 
  ungroup() 
print(prob_results_homem)

# Aplicar a função para calcular Q por fold e desaninhando os resultados
prob_results_alt_homem <- resultados |>
  filter(sexo_alt == "Masculino") |> 
  group_by(fold_alt) |> 
  do(calculate_prob_alt_by_fold(.)) |> 
  ungroup()
print(prob_results_alt_homem)

# KL - mulher
# Aplicar a função para calcular P por fold e desaninhando os resultados
prob_results_mulher <- resultados |> 
  filter(sexo_orig == "Feminino") |> 
  group_by(fold_orig) |> 
  do(calculate_prob_by_fold(.)) |> 
  ungroup() 
print(prob_results_mulher)

# Aplicar a função para calcular Q por fold e desaninhando os resultados
prob_results_alt_mulher <- resultados |>
  filter(sexo_alt == "Feminino") |> 
  group_by(fold_alt) |> 
  do(calculate_prob_alt_by_fold(.)) |> 
  ungroup()
print(prob_results_alt_mulher)

# Aplicar o cálculo da divergência KL por fold pra homem
kl_mulher <- prob_results_mulher |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt_homem |> mutate(row=row_number()), join_by(row)) |> 
  group_by(fold_orig) |> 
  mutate(kl = calculate_kl(P_fold,Q_fold)) |> 
  summarise(kl = mean(kl)) |> 
  mutate(bias = "Viés com Feminino")

print(kl_mulher)

# Aplicar o cálculo da divergência KL por fold pra homem
kl_homem <- prob_results_homem |> 
  mutate(row=row_number()) |> 
  left_join(prob_results_alt_mulher |> mutate(row=row_number()), join_by(row)) |> 
  group_by(fold_orig) |> 
  mutate(kl = calculate_kl(P_fold,Q_fold)) |> 
  summarise(kl = mean(kl)) |> 
  mutate(bias = "Viés com Masculino")

print(kl_homem)

plot_kl <- kl_homem |>
  bind_rows(kl_mulher) |> 
  mutate(fold_num = as.numeric(gsub("Fold", "", fold_orig))) |> 
  ggplot(aes(x = fold_num |> as.numeric(), y = kl))+
  geom_line(aes(color =as.factor(bias))) +
  geom_point(aes(color =as.factor(bias))) +
  scale_x_continuous(labels = 1:10, breaks = 1:10) +
  ylim(c(0,1)) +
  labs(x = '10-Fold', y = 'Divergência KL', color = '')+
  theme_classic()

# ggsave("div_kl.pdf", plot = plot_kl, width = 8, height = 6)



