#Carregar os dados
dados<-c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8, 21.9, 10.5, 17.3, 6.2, 18.0, 22.9, 24.6, 19.4, 12.3, 15.9, 20.1, 17.0, 22.3, 27.5, 23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 20.9, 21.4, 18.0, 24.3, 11.8, 17.9, 18.7, 12.8, 15.5, 19.2, 13.9, 28.6, 19.4, 21.6, 13.5, 24.6, 20.0, 24.1, 9.0, 17.6, 25.7, 20.1, 13.2, 23.7, 10.7, 19.0, 14.5, 18.1, 31.8, 28.5, 22.7, 15.2, 23.0, 29.6, 11.2, 14.7, 20.5, 26.6, 13.3, 18.1, 24.8, 26.1, 7.7, 22.5, 19.3, 19.4, 16.7, 16.9, 23.5, 18.4)

# 1.1

#calcular estatísticas
media <- mean(dados)
mediana <- median(dados)
moda <- function(v){ 
        uniqv <- unique(v)
        freq <- tabulate(match(v,uniqv)) 
        uniqv[freq == max(freq)] }
moda_val <- moda(dados) 
amplitude <- max(dados) - min(dados)
variancia <- var(dados) 
desvio_padrao <- sd(dados)
coeficiente_de_variacao <- (desvio_padrao / media) * 100
#imprimir resultados
cat("Medidas de tendencia central \n") 
cat("Media : ", round(media,2),"\n")
cat("Mediana : ", round(mediana,2), "\n")
cat("Moda : ", moda_val, "\n")
cat("Medidas de dispersão \n") 
cat("Amplitude : ", round(amplitude,2),"\n")
cat("Variancia : ", round(variancia,2),"\n");
cat("Desvio Padrao : ", round(desvio_padrao,2),"\n")
cat("Coeficiente de variacao (%) : ", round(coeficiente_de_variacao, 2),"\n") 

#1.2

#encontrar histograma e boxplot
hist(dados, main="Histograma", xlab="Emissoes(unidade de medida)",ylab="Frequencia",col="lightblue",border="black") 
boxplot(dados, main="Boxplot", ylabel="Emissoes(unidade de medida)", col="lightgreen", horizontal = TRUE)

#1.3

#achar quartios e o IQR
quartis <- quantile(dados, probs = c(0.25, 0.5, 0.75)) 
IQR_val <- IQR(dados) 
#imprimir resultados
cat("Quartil 1 (Q1):", quartis[1], "\n") 
cat("Quartil 2 (Q2):", quartis[2], "\n") 
cat("Quartil 3 (Q3):", quartis[3], "\n") 
cat("Intervalo Interquartil (IQR):", IQR_val, "\n") 
#achar outliers e imprimir
lim_inferior <- Q1 - 1.5 * IQR_val 
lim_superior <- Q3 + 1.5 * IQR_val 
outliers <- dados[dados < lim_inferior | dados > lim_superior] 
cat("Número de outliers:", length(outliers), "\n")

#1.4

#achar dados que passam do limite e fazer a proporcao
limite<-25 
dias_excedidos <- sum(dados>limite) 
proporcao <- dias_excedidos / length(dados) 
porcentagem <- proporcao * 100 
#imprimir resultados
cat("Dias que excederam o limite(unidades): ", dias_excedidos,"\n") 
cat("Proporcao: ", round(proporcao,3),"\n") 
cat("Porcentagem: ", round(porcentagem,3),"%\n")
