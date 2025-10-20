# Criar o data frame com os dados
dados <- data.frame(
  Idade = c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46),
  Nacionalidade = c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola",
                    "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa",
                    "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana",
                    "Alemana", "Italiana"),
  Renda = c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2),
  Experiencia = c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
)
# 2.1
# Calcular as estatisticas
media_idade <- mean(dados$Idade)
mediana_idade <- median(dados$Idade)
desvio_idade <- sd(dados$Idade)

media_renda <- mean(dados$Renda)
mediana_renda <- median(dados$Renda)
desvio_renda <- sd(dados$Renda)

media_exp <- mean(dados$Experiencia)
mediana_exp <- median(dados$Experiencia)
desvio_exp <- sd(dados$Experiencia)

# Exibir resultados
cat("\nIdade:\n")
cat("Media:", round(media_idade, 2), "\n")
cat("Mediana:", mediana_idade, "\n")
cat("Desvio padrao:", round(desvio_idade, 2), "\n")

cat("\nRenda:\n")
cat("Media:", round(media_renda, 2), "\n")
cat("Mediana:", mediana_renda, "\n")
cat("Desvio padrao:", round(desvio_renda, 2), "\n")

cat("\nExperiencia:\n")
cat("Media:", round(media_exp, 2), "\n")
cat("Mediana:", mediana_exp, "\n")
cat("Desvio padrao:", round(desvio_exp, 2), "\n")

# 2.2
# Calculo das medias por nacionalidade
medias <- aggregate(cbind(Renda, Experiencia) ~ Nacionalidade, data = dados, FUN = mean)
# Exibe o resultado
print(medias)

# 2.3
# Carregar pacote
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Calcular o coeficiente de correlacao de Pearson
r <- cor(dados$Experiencia, dados$Renda, method = "pearson")

p <- ggplot(dados, aes(x = Experiencia, y = Renda)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, linetype = "solid") +
  labs(
    title = "Relacao entre Anos de Experiencia e Renda Desejada",
    subtitle = "Cada ponto = um candidato (renda em milhares de euros)",
    x = "Anos de experiencia",
    y = "Renda desejada (mil €)",
    caption = paste(r_text, " — ", p_text)
  ) +
  theme_minimal(base_size = 14)
print(p) # Mostra o grafico
cat("Coeficiente de Pearson:", r) # Mostra o resultado do coeficiente no Terminal

# 2.4
# Filtrar candidatos com os criterios
candidatos_filtrados <- subset(dados, Experiencia >= 10 & Renda < 2.0)
# Exibir os resultados
print(candidatos_filtrados[, c("Idade", "Nacionalidade", "Renda", "Experiencia")])
# Mostrar quantos candidatos atendem aos criterios
cat("Numero de candidatos que atendem aos criterios:", nrow(candidatos_filtrados), "\n")

# 2.5
# Histograma da idade por nacionalidade
p1 <- ggplot(dados, aes(x=Idade, fill=Nacionalidade)) +
  geom_histogram(binwidth = 5, position="dodge", color="black") +
  labs(title="Distribuicao da Idade por Nacionalidade",
       x="Idade (anos)", y="Contagem") +
  theme_minimal()

print(p1)  # <- garante que o grafico apareca

# Histograma da renda por nacionalidade
p2 <- ggplot(dados, aes(x=Renda, fill=Nacionalidade)) +
  geom_histogram(binwidth = 0.5, position="dodge", color="black") +
  labs(title="Distribuicao da Renda Desejada por Nacionalidade",
       x="Renda (mil €)", y="Contagem") +
  theme_minimal()

print(p2)

# Boxplot da idade
p3 <- ggplot(dados, aes(x=Nacionalidade, y=Idade, fill=Nacionalidade)) +
  geom_boxplot() +
  labs(title="Boxplot da Idade por Nacionalidade",
       x="Nacionalidade", y="Idade (anos)") +
  theme_minimal()

print(p3)

# Boxplot da renda
p4 <- ggplot(dados, aes(x=Nacionalidade, y=Renda, fill=Nacionalidade)) +
  geom_boxplot() +
  labs(title="Boxplot da Renda Desejada por Nacionalidade",
       x="Nacionalidade", y="Renda (mil €)") +
  theme_minimal()

print(p4)