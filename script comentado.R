#instalando e ativando os pacotes
install.packages("tidyverse")
library(tidyverse)
library(haven)
install.packages("skimr")
library(skimr)
#####
#abrindo e nomeando a base
dados <- read_dta("C:/Users/Nathally.Barbosa/Downloads/Trabalho Met II/AnscombeQuartet.dta")
#o comando abre no console as primeiras 6 linhas da base. Como a base é pequena e quero visualizar tudo incluo um ",(número de linhas que quero ver)"
head(dados, 11)
#o comando informa o número de linhas (oberservações) e colunas (variáveis)
dim(anscombe)
#um comando de análise descritiva básica na base, mostra a média, o desvio padrão, o valor mínimo e máximo da variável, a mediana, o valor do terceiro quartil (75% das observações possuem um valor menor do que o valor do terceiro quartil) e um mini histograma
skim(anscombe)
#outro comando que traz algumas análises descritivas, a diferença entre o summary e skim é basicamente as informações sobre primeiro quartil, desvio padrão e o mini histograma 
summary(dados)
#comando para captar a variância de cada variável. O $ serve para indicar que a variável está dentro da base indicada anteriormente
var(dados$x1)
var(dados$x2)
var(dados$x3)
var(dados$x4)
var(dados$y1)
var(dados$y2)
var(dados$y3)
var(dados$y4)
#comando para captar a variância de todas as variáveis de uma só vez
sapply(dados[,c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")], var)

#aqui vamos estimar uma regressão linear simples
#estou guardando e chamando de reg1 o modelo que mostra como X1 impacta Y1
reg1 <- lm(y1 ~ x1, data=dados)
#o comando summary nesse caso nos dá os resultados da regressão que foi estimada, como o intercepto (B0), o coeficiente de X (B1), o t-test e o R quadrado que indica a proporção da variação de Y que pode ser explicada pelo modelo.
summary(reg1)
#Agora fazemos o mesmo para cada dupla de variável independente e dependente
reg2 <- lm(y2 ~ x2, data=dados)
summary(reg2)
reg3 <- lm(y3 ~ x3, data=dados)
summary(reg3)
reg4 <- lm(y4 ~ x4, data=dados)
summary(reg4)

#agora para construir os gráficos que mostram a relação entre as duas variáveis. 

#plot(dados$x1, dados$y1) = vou plotar no eixo horizontal de um gráfico a variável X1 que está na base chamada dados e, no eixo vertical, a variável Y1 que está na base chamada dados
#esse comando indica que na mesma imagem vai ter os 4 graficos distribuidos entre duas colunas e duas linhas
par(mfrow = c(2, 2))

#personalizando o gráfixo
#"xlab ="/"ylab =" indica a labble que vai aparecer em cada um dos eixos
# "xlim = c(3,19)" o comando define os limites do eixo x ou y. Quando rodamos o comando skim, identificamos que o menor valor das 8 variáveis era 3.1 e o maior 19, então esse intervalo vai abranger todas as observações
#"xaxp = c(4, 18, 7)" significa que no gráfico terão 7 marcações entre o intervalo 4 e 18 no eixo x
#"yaxp = c(4, 12, 4)" significa que no gráfico terão 4 marcações entre o intervalo 4 e 12 no eixo Y.As marcações só vão até o 12 pois o valor mais alto das variáveis Y é 12,7.
# pch indica o formato que cada observação será representada no gráfico. ex: pch = 16 é um circulo preenchido, pch = 21 é um circulo com uma margem em volta
# cex é para aumentar o tamanho dos pontos, nesse caso, circulos rosa
plot(dados$x1, dados$y1,
     xlab = "X1",
     ylab = "Y1",
     xlim = c(3, 19),
     ylim = c(3, 13),
     xaxp = c(4, 18, 7),
     yaxp = c(4, 12, 4),
     col = "#E6007E",
     pch = 21,
     bg = "pink", 
     cex = 1.2)
#adicionando a reta de regressão. 
#abline vai desenhar uma reta
#reg1 vai indicar onde a reta inicia(BO) e qual o seu ângulo (B1)
abline(reg1,
       col = "#6EC6F5")

#agora repito para o restante
plot(dados$x2, dados$y2,
     xlab = "X2",
     ylab = "Y2",
     xlim = c(3, 19),
     ylim = c(3, 13),
     xaxp = c(4, 18, 7),
     yaxp = c(4, 12, 4),
     col = "#E6007E",
     pch = 21,
     bg = "pink", 
     cex = 1.2)

abline(reg2,
       col = "#6EC6F5")

#para x3, y3
plot(dados$x3, dados$y3,
     xlab = "X3",
     ylab = "Y3",
     xlim = c(3, 19),
     ylim = c(3, 13),
     xaxp = c(4, 18, 7),
     yaxp = c(4, 12, 4),
     col = "#E6007E",
     pch = 21,
     bg = "pink", 
     cex = 1.2)

abline(reg3,
       col = "#6EC6F5")

# para x4, y4
plot(dados$x4, dados$y4,
     xlab = "X4",
     ylab = "Y4",
     xlim = c(3, 19),
     ylim = c(3, 13),
     xaxp = c(4, 18, 7),
     yaxp = c(4, 12, 4),
     col = "#E6007E",
     pch = 21,
     bg = "pink", 
     cex = 1.2)

abline(reg4,
       col = "#6EC6F5")

#agora vou ver um gráfico de cada vez
par(mfrow = c(1, 1))

# Analisando correlações
#cor.test calcula a correlação fazendo um teste de hipótese sobre ela
cor.test(dados$x1, dados$y1) 
cor.test(dados$x2, dados$y2)
cor.test(dados$x3, dados$y3) 
cor.test(dados$x4, dados$y4)

#vamos modificar algumas observações da variável X1
#primeiro vamos observar a variável original
dados$x1
#agora vamos criar uma copia dessa variável na mesma base, antes tinhamos 8 variáveis, agora temos 9
dados$x1_mod <- dados$x1

# vamos alterar os valores das observações 5 e 6 para 14 e 11 respectivamente
dados$x1_mod[5] <- 14
dados$x1_mod[6] <- 11
#visualizar a nova base
head(dados)
#verificar a média e variância dessa nova variável 
mean(dados$x1_mod)
var(dados$x1_mod)

#Agora vamos repetir a regressão mas dessa vez analisando o efeito de X1_mod sobre Y 
reg1_mod <- lm(y1 ~ x1_mod, data=dados)
summary(reg1_mod)

#fazendo o gráfico das duas regressões em uma mesma imagem para visualizar melhor
par(mfrow = c(2, 2))
plot(dados$x1, dados$y1,
     xlab = "X1",
     ylab = "Y1",
     xlim = c(3, 19),
     ylim = c(3, 13),
     xaxp = c(4, 18, 7),
     yaxp = c(4, 12, 4),
     col = "#E6007E",
     pch = 21,
     bg = "pink", 
     cex = 1.2)

abline(reg1,
       col = "#6EC6F5")

#regressão do x1_mod
plot(dados$x1_mod, dados$y1,
     xlab = "X1_mod",
     ylab = "Y1",
     xlim = c(3, 19),
     ylim = c(3, 13),
     xaxp = c(4, 18, 7),
     yaxp = c(4, 12, 4),
     col = "pink",
     pch = 21,
     bg = "#6EC6F5", 
     cex = 1.2)

abline(reg1_mod,
       col = "#E6007E")

#visualizando o gráfico com duas retas em uma só imagem
#ity = 2 representa uma linha tracejada
par(mfrow = c(1, 1))
plot(dados$x1, dados$y1,
     xlab = "X1",
     ylab = "Y1",
     xlim = c(3, 19),
     ylim = c(3, 13),
     xaxp = c(4, 18, 7),
     yaxp = c(4, 12, 4),
     col = "#E6007E",
     pch = 21,
     bg = "pink", 
     cex = 1.2)

abline(reg1,
       col = "#6EC6F5")
abline(reg1_mod,
       col = "orange", lty = 2)

#colocando legendase cores referentes as suas respectivas retas
legend("topleft",
       legend = c("Reta ajustada: y1 ~ x1", "Reta ajustada: y1 ~ x1_mod"),
       col    = c("#6EC6F5", "orange"),
       lty    = c(1, 2),
       bty    = "n")  
#para comparar a correlação
cor.test(dados$x1, dados$y1) 
cor.test(dados$x1_mod, dados$y1) 

