
mydata <- read.table("F:\USP 2022 - Semestre II\2 - MQAM\Grupo Projeto MQAM\Atividade 4 - Apresentação Regressão Linear/Life Expectancy Data.csv", header=TRUE,
                     sep=",")

quartiles <- quantile(mydata$infant.deaths, probs=c(.25, .75), na.rm=TRUE)
IQR <- IQR(mydata$infant.deaths, na.rm=TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
mydata <- subset(mydata, mydata$infant.deaths > Lower & mydata$infant.deaths < Upper)

quartiles <- quantile(mydata$Adult.Mortality, probs=c(.25, .75), na.rm=TRUE)
IQR <- IQR(mydata$Adult.Mortality, na.rm=TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
mydata <- subset(mydata, mydata$Adult.Mortality > Lower & mydata$Adult.Mortality < Upper)

dim(mydata)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#expectativa de vida média ao redor dos anos


dados1 <- aggregate(mydata[,c(4)], by= list(mydata$"Year"), FUN=mean, na.rm=TRUE)

b1 <- barplot(height=dados1[,c(2)],width=dados1[,c(1)], ylim=c(50, 80),main = "Média da expectativa de vida por ano", xpd=FALSE, names = dados1[,c(1)])

text(x=b1, y=dados1[,c(2)]+2, labels=round(dados1[,c(2)],1), cex=0.8)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#box-plot das vacinações

teste <- boxplot(mydata[,c(9,13,15)], main = "Box-plot das porcentagens de vacinações",
                 xlab = "Porcentagem de vacinados",
                 col = "orange",
                 border = "brown",
                 horizontal = TRUE,
                 notch = TRUE)



text(x=fivenum(mydata[,c(9)]), labels =fivenum(mydata[,c(9)]), y=1.25)
text(x=fivenum(mydata[,c(13)]), labels =fivenum(mydata[,c(13)]), y=2.25)
text(x=fivenum(mydata[,c(15)]), labels =fivenum(mydata[,c(15)]), y=3.25)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#variancia e desvio padrão das vacinações
var(mydata[,c(9)],na.rm = TRUE)
sd(mydata[,c(9)],na.rm = TRUE)

var(mydata[,c(13)],na.rm = TRUE)
sd(mydata[,c(13)],na.rm = TRUE)

var(mydata[,c(15)],na.rm = TRUE)
sd(mydata[,c(15)],na.rm = TRUE)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#box-plot das mortes infantis por mil

boxplot(mydata[,c(6)], main = "Box-plot das mortes infantis",
        xlab = "Número de mortos a cada 1000 pessoas",
        ylim= c(0,80),
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

text(x=fivenum(mydata[,c(6)]), labels =fivenum(mydata[,c(6)]), y=1.25)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#box-plot das mortes adultas por mil

boxplot(mydata[,c(5)], main = "Box-plot das mortes adultas",
        xlab = "Número de mortos a cada 1000 pessoas",
        ylim= c(0,1000),
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

text(x=fivenum(mydata[,c(5)]), labels =fivenum(mydata[,c(5)]), y=1.25)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#variancia e desvio padrão das mortes
var(mydata[,c(6)],na.rm = TRUE)
sd(mydata[,c(6)],na.rm = TRUE)

var(mydata[,c(5)],na.rm = TRUE)
sd(mydata[,c(5)],na.rm = TRUE)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#box-plot de anos usados na educação

boxplot(mydata[, c(22)], main = "Box-plot da educação",
        xlab = "Anos",
        ylim= c(0,21),
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

text(x=fivenum(mydata[, c(22)]), labels =fivenum(mydata[, c(22)]), y=1.25)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#pib per capita médio entre países desenvolvidos e subdesenvolvidos


dados2 <- aggregate(mydata[,c(17)], by= list(mydata$"Status"), FUN=mean, na.rm=TRUE)


b2 <- barplot(dados2[, c(2)],ylim=c(0,25000),main = "Média de dinheiro (USD) per capita", names = dados2[, c(1)])

text(x=b2, y=dados2[, c(2)] + 500, labels=round(dados2[, c(2)], 3) )

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#correlação entre mortes infantis e adultas e as variáveis

cor(mydata[7:22], mydata$infant.deaths,  method = "pearson", use = "complete.obs")

cor(mydata[7:22], mydata$Adult.Mortality,  method = "pearson", use = "complete.obs")

cor(mydata[7:22], mydata$Life.expectancy,  method = "pearson", use = "complete.obs")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#regresao linear mulxipla - mortes infantis

modelo1 <- lm(infant.deaths ~ Hepatitis.B + Diphtheria + Polio, data=mydata)

summary(modelo1)

plot(mydata$Hepatitis.B, mydata$infant.deaths, main = "Mortes infantis vs Imunização de Hepatite B",pch = 16, cex = 1.3, col = "blue")

abline(lm(infant.deaths ~ Hepatitis.B, data=mydata),col="red", lwd=3)

summary(lm(infant.deaths ~ Hepatitis.B, data=mydata))

plot(mydata$Diphtheria, mydata$infant.deaths, main = "Mortes infantis vs Imunização de Difteria",pch = 16, cex = 1.3, col = "blue")

abline(lm(infant.deaths ~ Diphtheria, data=mydata),col="red", lwd=3)

summary(lm(infant.deaths ~ Diphtheria, data=mydata))

plot(mydata$Polio, mydata$infant.deaths, main = "Mortes infantis vs Imunização de Poliomelite",pch = 16, cex = 1.3, col = "blue")

abline(lm(infant.deaths ~ Polio, data=mydata),col="red", lwd=3)

summary(lm(infant.deaths ~ Polio, data=mydata))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#regresao linear mulxipla - mortes adultas

modelo2 <- lm(Adult.Mortality ~ Hepatitis.B + Diphtheria + Polio, data=mydata)

summary(modelo2)

mod_hep <-lm(Adult.Mortality ~ Hepatitis.B, data=mydata)
mod_dip <- lm(Adult.Mortality ~ Diphtheria, data=mydata)
mod_pol <- lm(Adult.Mortality ~ Polio, data=mydata)

plot(mydata$Hepatitis.B, mydata$Adult.Mortality, main = "Mortes adultas vs Imunização de Hepatite B",pch = 16, cex = 1.3, col = "blue")
abline(mod_hep,col="red", lwd=3)

summary(mod_hep)

plot(mydata$Diphtheria, mydata$Adult.Mortality, main = "Mortes adultas vs Imunização de Difteria",pch = 16, cex = 1.3, col = "blue")
abline(mod_dip,col="red", lwd=3)

summary(mod_dip)

plot(mydata$Polio, mydata$Adult.Mortality, main = "Mortes adultas vs Imunização de Poliomelite",pch = 16, cex = 1.3, col = "blue")
abline(mod_pol,col="red", lwd=3)

summary(mod_pol)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot residuos
plot(modelo1)
plot(modelo2)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#verificação de outliers
summary(rstandard(modelo1))
summary(rstandard(modelo2))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#shapiro
b1 <- sample(modelo1$residuals, 500, replace=FALSE)
b2 <- sample(modelo2$residuals, 500, replace=FALSE)

shapiro.test(b1)
shapiro.test(b2)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#homocedasticidade
