
# Bibliotecas a serem utilizadas
library (dplyr)
library(corrplot)
library (psych)
library(ggplot2)

# Importando o dataset de Expectativa de vida como um dataframe no R
dados <- read.csv(file.choose(), header = TRUE)

# Removendo todos os missing data das variáveis que vão ser utilizadas
dados <- dados %>%
  filter (!is.na(dados$Adult.Mortality ) &
           !is.na(dados$infant.deaths ) &
           !is.na(dados$Alcohol ) &
           !is.na(dados$percentage.expenditure ) &
           !is.na(dados$thinness..1.19.years ) &
           !is.na(dados$thinness.5.9.years ) &
           !is.na(dados$Total.expenditure ) &
           !is.na(dados$BMI ) &
           !is.na(dados$GDP ) & 
           !is.na(dados$Life.expectancy) &
            !is.na(dados$Income.composition.of.resources) &
           (dados$Alcohol != 0) )

# Renomear nomes de colunas
colnames(dados)[1] <- "País"
colnames(dados)[2] <- "Ano"
colnames(dados)[3] <- "Status"
colnames(dados)[4] <- "Expectativa.Vida"
colnames(dados)[5] <- "Mortalidade.Adultos"
colnames(dados)[6] <- "Mortalidade.Infantil"
colnames(dados)[7] <- "Consumo.Alcool"
colnames(dados)[8] <- "Porcentagem.Gastos"
colnames(dados)[9] <- "Imunização.Hepatite.B"
colnames(dados)[10] <- "Casos.Sarampo"
colnames(dados)[11] <- "IMC"
colnames(dados)[12] <- "Sob.5.mortes"
colnames(dados)[13] <- "Imunizacao.Poliomelite"
colnames(dados)[14] <- "Gasto.Total.Saude"
colnames(dados)[15] <- "Imunizacao.Difteria"
colnames(dados)[16] <- "Morte.HIV.AIDS"
colnames(dados)[17] <- "PIB"
colnames(dados)[18] <- "Populacao"
colnames(dados)[19] <- "Magreza.10.19.anos"
colnames(dados)[20] <- "Magreza.5.9.anos"
colnames(dados)[21] <- "IDH"
colnames(dados)[22] <- "Escolaridade"


# Aqui excluímos todas as colunas que não serão utilizadas para fazermos
# a matriz de correlação

dados$Imunização.Hepatite.B <- NULL
dados$Casos.Sarampo <- NULL
dados$Sob.5.mortes <- NULL
dados$Imunizacao.Poliomelite <- NULL
dados$Imunizacao.Difteria <- NULL
dados$Morte.HIV.AIDS <- NULL
dados$Escolaridade <- NULL
dados$País <- NULL
dados$Ano <- NULL
dados$Status <- NULL
dados$Populacao <- NULL

# Aqui, geramos, a matriz de correlação das variáveis
corRes <- cor(dados)
corrplot(corRes, type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(corRes, type = "upper",order = "hclust", tl.col = "black", tl.srt = 45, method = "number")

# Calculando o KMO das variáveis sendo utilizadas
KMO(r=cor(dados))

# Fazendo o teste de Bartlett para verificar se a análise fatorial é adequada
cortest.bartlett(dados)

det(cor(dados))

# Grafico scree para analisarmos quantos fatores serão utilizados

fafitfree <- fa(dados,nfactors = ncol(dados), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() + theme_minimal() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Gráfico Scree")

# Segundo grafico Scree de outra biblioteca

parallel <- fa.parallel(dados)

# Graficos em barras do scree
fit <- princomp(dados, cor = TRUE)
screeplot(fit)

# Variância total explicada

fit <- princomp(dados, cor = TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

# Análise de componentes principais
fit <- principal(dados, nfactors=3, rotate="none") #antes da rotação
fit <- principal(dados, nfactors=3, rotate="varimax") #depois da rotação


res_fat <- fa(r= dados, nfactors =3, rotate = "none")


# -------------------------------------------------------------------------
library(ISwR)

ds <- thuesen

fit <- lm()

af <- anova(fit)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

