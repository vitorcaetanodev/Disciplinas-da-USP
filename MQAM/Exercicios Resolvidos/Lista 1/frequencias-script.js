
// link = https://pt.symbolab.com/solver/variance-calculator/Coeficiente%20de%20Variacao%2032%2C%2032%2C%2032%2C%2032%2C%2032%2C%2032%2C%2032%2C%2032%2C%2033%2C%2033%2C%2034%2C%2034%2C%2034%2C%2034%2C%2034%2C%2035%2C%2035%2C%2035%2C%2036%2C%2036?or=input
// link2 = https://calculareconverter.com.br/coeficiente-de-variacao/#:~:text=CV%20%3D%20s%20%2F%20X%20*%20100&text=Al%C3%A9m%20disso%2C%20h%C3%A1%20outro%20detalhe,m%C3%A9dia%20dos%20dados%20seja%20menor.
// Link3 = https://calculator-online.net/pt/standard-deviation-calculator/
// Link 4 = https://www.statskingdom.com/advanced-boxplot-maker.html

// Desafio : jogar na url os resultados ja prontos

// Media variancia e desvio Padrao
// Passo 1 - Variaveis e Frequencias
var variaveis = [10,40,60,20], frequencias= [1,1,1,2];
var saida = "";
for(x = 0;x<frequencias.length;x++){
  for(i = 0;i<frequencias[x];i++){
    saida += variaveis[x]*3;
    saida+=",";
  }
}
// Log to console
console.log(saida)
