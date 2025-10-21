# 3.1
# Carregando os dados
bike <- read.csv("HW1_bike_sharing.csv", sep = ",", header = TRUE)

# Ver as primeiras linhas
head(bike)

# Identificando os tipos
str(bike)

# Ajustando os tipos
bike$dteday <- as.Date(bike$dteday)
bike$season <- as.factor(bike$season)
bike$weathersit <- as.factor(bike$weathersit)

# Numero de observacoes
n_obs <- nrow(bike)

# Datas de inicio e fim
data_inicio <- min(bike$dteday)
data_fim <- max(bike$dteday)

n_obs
data_inicio
data_fim

#3.2
dados_numericos <- bike[, c("temp", "casual", "registered")]

# Calcular medidas descritivas
resumo <- data.frame(
  Variavel = colnames(dados_numericos),
  Media = sapply(dados_numericos, mean),
  Mediana = sapply(dados_numericos, median),
  Q1 = sapply(dados_numericos, quantile, 0.25),
  Q3 = sapply(dados_numericos, quantile, 0.75)
)

# Exibir tabela
print(resumo)

#3.3

# Indicar ao R o que cada número(índice) significa
bike$season<- factor(bike$season,levels=c(1,2,3,4),labels = ("primavera","verao","outono","inverno"))
dados$weathersit<-factor(bike$weathersit, levels=c(1,2,3,4),labels=c("ceu limpo","nublado","chuva fraca","chuva forte"))

# Gerar graficos de barra
barplot(table(bike$season), main = "Registros por estacao", col = "skyblue")
barplot(table(bike$weathersit), main = "Registros por clima", col = "lightgreen")

# Calcular o numero de usuários totais
bike$total <- bike$casual+bike$registered

# Fazer a soma dos dados totais com cada estação e com cada clima, respectivamente
tapply(bike$total,dados$season,sum)
tapply(bike$total,dados$weathersit,sum)

#3.4

# Converter a variável temp para a temperatura real
bike$temp_real <- bike$temp * 41

# Plotar o gráfico da série temporal da temperatura 
plot(bike$dteday, bike$temp_real, type = "l", col = "red",main = "Temperatura ao longo do tempo",xlab = "Data", ylab = "Temperatura (°C)")

# Plotar o gráfico da série temporal do número total de usuários
plot(bike$dteday, bike$total, type = "l", col = "blue",main = "Número total de usuários por dia",xlab = "Data", ylab = "Total de usuários")
