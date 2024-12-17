## Fonte de Pesquisa para auxiliar no R : https://rpubs.com/EstatBasica/Cap3 Comandos R para análises estatísticas

#DataSet Principal (Importado CSV)
View(ds_salaries)


## Grafico Plot Salarios ==> Grafico Pontos ou Barras
salarios<-hist(ds_salaries$salary,right=FALSE, plot=F)
aux<-with(salarios, 100 * density* diff(breaks)[1])
labs <- paste(round(aux), "%", sep="")
plot(salarios, 
     freq = FALSE, labels = labs, 
     xlab="Salário",
     ylab="",
     col="darkgrey",
     border="white",
     yaxt="n",
     xlim=c(0,24), xaxp=c(0,24,6),
     ylim=c(0,.1), main="")


## Grafico Plot Locais 
local<-ds_salaries[order(ds_salaries$company_location,decreasing = TRUE),]


## Funcao BoxPlot Exemplo
boxplot(local$salary, 
        pch="*",  # tipo de marcador dos outliers
        col="lightblue", # cor do preenchimento do box plot
        border="darkgrey", # cor da linha do box plot
        boxwex=0.3 # Tamanho da caixa
)
# Os comandos 'text'a seguir imprimem no box plot os nomes das cidades dos 4 pontos destacados
text(x=local$salary[1],label=local$company_location[1],pos=4,cex=0.7) # Máximo
text(x=local$salary[2],label=local$company_location[2],pos=4,cex=0.7) # 2a. maior observacao
text(x=local$salary[3],label=local$company_location[3],pos=4,cex=0.7) # 3a. maior observacao
text(x=local$salary[15],label=local$company_location[15],pos=4,cex=0.7) # 15a. observacao  


## Grafico Qualitativo Nominal por barras: exemplo https://rstudio-pubs-static.s3.amazonaws.com/7342_3aee84b4bc9549adb3080f06c69174e1.html
tamanhoEmpresa <- c("Pequena", "Média", "Grande")
barplot(table(ds_salaries$company_size), names.arg = ident, col = c("pink", "lightblue", "orange"))

## Grafico de pizza (setores)
nivelExp <- c("EN", "MI", "SE", "EX")
pie(prop.table(table(ds_salaries$experience_level)) * 100, label = nivelExp, col = c("lightblue2", 
                                                                                     "lightblue3",
                                                                                     "lightblue1",
                                                                                     "orange"))
title(main = "Nivel de Experiencia - Funcionarios")



#Escolha dos tipos de Representacao de cada coluna
#Work Year, company_size - Colunas
#experience_level,job_title,employment_type - pizza
#salary, salary_currency - colunas
#employee_residence, company_location, remote_ratio - ?
#
# Preciso de ajuda para definir os graficos que serao plotados corretamente.