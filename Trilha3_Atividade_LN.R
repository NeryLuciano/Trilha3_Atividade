

library(ggplot2)
library(tidyverse)
library(ggpubr)
data(diamonds)

diamantes <- diamonds

nrow(diamantes)
ncol(diamantes)

# Rodando o "str" para verificar a estrutura dos dados

str(diamantes)

# Explorando o inicio e o fim dos dados

head(select(diamantes, c(6:7,10)))

tail(select(diamantes, c(6:7,10)))

# Análise Uivariada

summary (diamantes)

# Verificando exclusivamente str da coluna color

str(diamantes$color)

# Corrigindo dados conforme descrição

diamantes$color <- factor(diamantes$color, levels=rev(levels(diamantes$color)))

str(diamantes)

# Seguindo com a exploração da variável price

summary(diamantes$price)

quantile(diamantes$price, seq(0,1,0.1))

# Verificando a distribuição em um histograma


ggplot(diamantes) + geom_histogram(aes(price), bins =30, fill = "green", color = "black") +
    geom_rug(aes(price)) +
  labs(title = "Margenta: mediana",
       x= "Preço", y = "Frequência",
       caption = "Fonte: Elaborado pelo autor") +
  geom_vline(xintercept = median(diamantes$price),
             color = "magenta", lwd = 2) + labs_pubr() +
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))




price <- (diamantes$price)

# Criando Boxplot para as variáveis numéricas

options(repr.plot.width = 30,repr.plot.height = 30)
Carat   <- ggplot(data = diamantes) + geom_boxplot(aes(y = carat)) + labs_pubr()
Price   <- ggplot(data = diamantes) + geom_boxplot(aes(y = price)) + labs_pubr()
Depth   <- ggplot(data = diamantes) + geom_boxplot(aes(y = depth)) + labs_pubr()
Table   <- ggplot(data = diamantes) + geom_boxplot(aes(y = table)) + labs_pubr()
X       <- ggplot(data = diamantes) + geom_boxplot(aes(y = x))     + labs_pubr()
Y       <- ggplot(data = diamantes) + geom_boxplot(aes(y = y))     + labs_pubr()
Z       <- ggplot(data = diamantes) + geom_boxplot(aes(y = z))     + labs_pubr()

ggarrange(ncol = 3, nrow = 3, Carat,Price,Depth,Table,X,Y,Z)

# Verificando as Variaveis categóricas, orodem

levels(diamantes$cut);
levels(diamantes$color);
levels(diamantes$clarity)

# Verificando quantidades de ITENS

table(diamantes$cut);
table(diamantes$color);
table(diamantes$clarity)

#Criando gráfico para as 3 variáveis categóricas juntas

options(repr.plot.width = 30,repr.plot.height = 15)

Cut     <- ggplot(data = diamantes) + geom_bar(aes(cut))     + labs_pubr() + ylab("Frequencia")
Color   <- ggplot(data = diamantes) + geom_bar(aes(color))   + labs_pubr() + ylab("Frequencia")
Clarity <- ggplot(data = diamantes) + geom_bar(aes(clarity)) + labs_pubr() + ylab("Frequencia")


ggarrange(ncol = 3, nrow = 1, Cut,Color,Clarity)

summary(diamantes)

#Explorando as variáveis quantitativas,carat, x, y, z, depth e table

# Variavel carat

summary(diamantes$carat)
quantile(diamantes$carat, seq(0,1,0.1))

# Variavel depth

summary(diamantes$depth)
quantile(diamantes$depth, seq(0,1,0.1))

# Variavel table

summary(diamantes$table)
quantile(diamantes$table, seq(0,1,0.1))


# Variavel x

summary(diamantes$x)
quantile(diamantes$x, seq(0,1,0.1))

# Variavel y

summary(diamantes$y)
quantile(diamantes$y, seq(0,1,0.1))

