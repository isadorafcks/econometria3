

library("CADFtest")
library("car")
library("lmtest")
library("sandwich")
library("vars")

#Pacote para testes de cointegração
library('urca')
library('vars')



# teste de JOHANSEN - detectando o numero de rela�oes de cointegra��o

# utiliza a estatistica de raz�o de verossimilhan�a

#1.selecionar a ordem do VAR (p)
# - ajustar um var em nivel
# - determinar o p do modelo por criterios de informa��o como AIC ou BIC

# n= 3

ibc = read.csv2("C:/Users/isado/Downloads/ibc.csv")
ibc = ts(log(ibc[,2]), start=c(substring(ibc[1,1],1,4),substring(ibc[1,1],6,7)),frequency=12)

m1 = read.csv2('C:/Users/isado/Downloads/m1.csv')
m1 = ts(log(m1[,2]), start=c(substring(m1[1,1],1,4),substring(m1[1,1],6,7)),frequency=12)

igp = read.csv2('C:/Users/isado/Downloads/igp.csv')
igp = ts(log(igp[,2]), start=c(substring(igp[1,1],1,4),substring(igp[1,1],6,7)),frequency=12)


dados = cbind(ibc,m1,igp)
dados = window(dados, start = c(2003,1), end = c(2024,2))

#Ao rodar o procedimento sequencial, notamos que todas as séries são I(1),
#e que m1 e igp exibem drift (intercepto na primeira diferença)

#Evidência de sazonalidade
acf(diff(dados),lag.max=40)

#Selecionando coeficientes com base no VAR em nível. Note que, como há drift,
#devemos incluir uma constante no VAR em nível (random walk + drift => tendencia estocastica e linear no nível)

# drift = incluir constante no vAR

#O "drift" refere-se a um termo constante na equa��o que modela as s�ries temporais.
#Isso indica que as s�ries podem ter uma tend�ncia estoc�stica ou uma tend�ncia linear no n�vel.

criterios = VARselect(dados, lag.max = 20, type = 'const', season=12)
criterios

#Selecionando com base no BIC

# o menor coeficiente da coluna SC 

# testando com uma defasagem =2 

modelo_nivel = VAR(dados, type = 'const', p=2, season=12)
summary(modelo_nivel)

#Obs: NÃO PODEMOS OLHAR OS VALORES CRÍTICOS do summary acima, pois há risco de inferência espúria

#A infer�ncia esp�ria ocorre quando as s�ries temporais s�o n�o estacion�rias, 
# levando a resultados enganosos. Isso � especialmente relevante em s�ries com drift ou tend�ncia estoc�stica.

#Teste de Defasagem: O teste de raz�o de verossimilhan�a (LM test) ajuda a decidir se incluir uma defasagem adicional
# � justificado, proporcionando um m�todo robusto para melhorar a especifica��o do modelo VAR.

#Teste da nula de que p-esima defasagem eh zero

#vamos ver se realmente precisamos de duas defasagens ee nao uma

#Verificamos se o modelo pode ser simplificado removendo defasagens, utilizando o teste de raz�o de verossimilhan�a (LR test).

LM = 2*(logLik(modelo_nivel)- logLik(VAR(dados, type = 'const', p=1, season=12)))
print(LM>qchisq(0.95,9))

# H0: r = q, 
# H1 : r = q + 1

# TRUE 

#Rejeitamos a nula a 5% 
# r = 1 + 1 =2

# continuamos com 2

############################

#Vamos olhar o teste de Breusch-Godfrey de nao correlacao serial  ( PORTMANTEAU)

# O teste de Breusch-Godfrey � utilizado para verificar a presen�a de autocorrela��o nos res�duos de um modelo.

# A presen�a de autocorrela��o indica que os res�duos n�o s�o independentes,
# o que pode violar os pressupostos do modelo VAR (Vector Autoregression) e levar a infer�ncias incorretas.

serial.test(modelo_nivel, type = 'BG', lags.bg =5)

# lags.bg = 5: Define o n�mero de defasagens (lags) a serem considerados no teste.

# Hip�tese Nula (H0): N�o h� autocorrela��o serial nos res�duos at� 5 defasagens.

# Hip�tese Alternativa (H1): H� autocorrela��o serial nos res�duos


#p-value > 0.05: N�o rejeitamos a hip�tese nula. N�o h� evid�ncia suficiente para afirmar que h� autocorrela��o serial nos res�duos.
#p-value = 0.05: Rejeitamos a hip�tese nula. H� evid�ncia de autocorrela��o serial nos res�duos.

#Muita evicencia contra nao correla��o dos erros 

###################################

#Aumentando defasagem para ver se melhoramos de 2 defasagens para 3

# Ajusta o modelo VAR com 3 defasagens

modelo_nivel = VAR(dados, type = 'const', p = 3, season = 12)

# Calcula a estat�stica do teste de raz�o de verossimilhan�a

LM = 2 * (logLik(modelo_nivel) - logLik(VAR(dados, type = 'const', p = 2, season = 12)))

# Compara a estat�stica com o valor cr�tico da distribui��o qui-quadrado para 9 graus de liberdade

print(LM > qchisq(0.95, 9))
print(LM > qchisq(0.9, 9))

serial.test(modelo_nivel, type = 'BG', lags.bg =5)+

#Teste BG não rejeita a nula a 5%

#p=3 parece ok, visto que apresenta bom balanco entre parcimonia e nao autocorrelacao dos erros 

#Vamos trabalhar com 3 defasagens

johansen = ca.jo(dados, type = 'eigen', ecdet = c('none'), K=3,spec = 'transitory', season=12)

summary(johansen)

#           test 10pct  5pct  1pct
# r <= 2 |  0.23  6.50  8.18 11.65
# r <= 1 |  8.90 12.91 14.90 19.19
# r = 0  | 39.48 18.90 21.07 25.75


#  r = 0 

#Test statistic: 39.48
#Critical values: 18.90 (10%), 21.07 (5%), 25.75 (1%)
#Decision: Como 39.48 > 25.75 (valor cr�tico a 1%), rejeitamos a hip�tese nula de que n�o h� vetores de cointegra��o. 
#H� pelo menos 1 vetor de cointegra��o.


# r=1 
# valor do teste: 8.9
#valores criticos: 12.91 (10%), 14.9 (5%), 19.19 (1%)
# como a 1%: 8.9 , 19.1 , nao rejeitamos a hipotese nula de que n�o ha vetores de cointegra��o.

# menor que n�o rejeita no caso � 1
# o primeiro que nao rejeitar a gnt aceita

#Conclusão do teste: UMA relacao de cointegração  ( r=1)

#Testando a nula de que atividade econômica NÃ0 participa da relação de cointegração

teste = blrtest(johansen, matrix(c(c(0,1,0), c(0,0,1)), nrow=3), r=1)
summary(teste)
#Claramente rejeitamos a nula


#Testando a nula de que equação quantitativa é boa descrição
teste = blrtest(johansen, matrix(c(1,-1,1),nrow=3), r=1)
summary(teste)
#Também rejeitamos a nula!


#Com mais de uma relação de cointegração, para testarmos hipóteses
# de que r1 < r relações de cointegração são iguais a uma matriz H,
# usamos bh5lrtest.
# Para testar restrições sobre r1 < r relações (por exemplo, que algumas,
# variáveis participam de r1 relações), usamos bh6lrtest.


#Estimando VECM
modelo = cajorls(johansen, r=1)
modelo$beta
summary(modelo$rlm)

#Representação em VAR
vs = vec2var(johansen, r=1)

#Predições
fanchart(predict(vs), plot.type = 'single')
