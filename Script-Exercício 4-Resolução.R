#Exemplo:

n <- 25
x <- c(rep(0,5), rep(1,5), rep(2,5), rep(3,5), rep(4,5))
y <- c(13.44,12.84,11.91,20.09,15.60,10.11,11.38,10.28,
       8.96,8.59,9.83,9,8.65,7.85,8.88,7.94,6.01,5.14,
       6.9,6.77,4.86,5.1,5.67,5.75,6.23)

fit.model <- lm(y~x)
plot(x,y,pch=16)
abline(fit.model)
summary(fit.model)

yyest <- fit.model$coefficients[1]+fit.model$coefficients[2]*x
yyest <- fit.model$fitted.values

ei <- y - yyest
ei <- fit.model$residuals

s <- summary(fit.model)$sigma

plot(ei,pch=16)
abline(0,0,col=2)
# com o grafico acima podemos notar:
#   - um ponto discrepante
#   - parece haver uma maior concentra��o dos valores abaixo do zero pra uma regi�o dos res�duos, e dps eles ficam concentrados acima do zero
#   - ent�o, pode ser que algumas suposi��es do modelos n�o estejam adequadas, pois temos uma valor discrepante, e os dados  n�o est�o distribuidos aleatoriamente em torno do zero,
#     distribuidos aleatoriamente em torno do zero,j� que em uma regi�o temos uma maior concetra��o abaixo do zero, e emoutro regi�o uma concetra��o maior acima do zero


#gr�fico de residuos em rela��o a variavel explicativa:

plot(x,ei,pch=16)
abline(0,0,col=2)
# fizemos o gr�fico acima, apenas para verificar o que foi dito anteriormente:
#   - percebe a presen�a do valor discrepante
#   - para 1,2 e 3, os valores est�o concentrados abaixo de zero, e, para 4, eles est�o concentrados acima de zero

#grafico de residuos em rela��o a variavel resposta(estimada):

plot(yyest,ei,pch=16)
abline(0,0,col=2)
# esse gr�fico tamb�m foi feito para verificar o que foi dito nos dois gr�ficos anteriores:
#    - presen� de valor discrepante
#    - para umas regi�es concentra��es abaixo do zero, e para outras, concentra��es acima do zero

#podemos trabalhar tamb�m com os outros res�duos, para ver se tem informa��o a mais:

#a) res�duo padronizado:

ri <- ei/s

#grafico em rela��o a variavel explicativa:

plot(x,ri,pch=16)
abline(0,0)
# mesmas conclus�es das feitas anteriormente
# obs: mudou a escala, dxando mais evidente/destacado o valor discrepante

#b) res�duo studentizado:

ti <- rstandard(fit.model)

#grafico em rela��o a variavel explicativa:

plot(x,ti,pch=16)
abline(0,0,col=2)
# mesmas conclus�es das feitas anteriormente
# obs: mudou a escala, dxando mais evidente/destacado ainda o valor discrepante

#c) res�duo com informa��o deletada(dist. exata t-Student com n-3 gl):

tsi <- rstudent(fit.model)

#grafico em rela��o a variavel explicativa:

plot(x,tsi,pch=16)
abline(h=0)
abline(h=2,col=2)
# mesmas conclus�es das feitas anteriormente
# obs: -mudou a escala, dxando mais evidente/destacado ainda o valor discrepante
#      -esse ponto discrepante puxou ainda mais a escala do grafico pra cima

#Podemos fazer tambem o uso do boxplot para tirar ou confirmar as conclus�es:

boxplot(ei) #percebe-se a presen�a do valor discrepante
boxplot(tsi) #tambem percebe-se a presen�a do valor discrepante (mais lomge ainda da caixa)

#Fazemos o qqnorm para verificar a suposi��o de normalidade:

qqnorm(ei)
abline(0,1)
# percebe-se um ponto que sai bastante da diagonal, e na extremidade inferios tamb�m sai um pouco

#Com todos esses resultados, vamos agora tentar, por meio de uma transforma��o, adequar esse conjunto de dados no modelos:

library(car)
boxCox(fit.model,lambda = c(-3,3,1/10))
# percebe-se que o lamnbda igual a 1 ou seja, sem transforma��o, n�o � o mais adequado, pois ele nao maximiza a fun��o de verossimilan�a
# a m�xima verosimilhan�a encontra-se no entre os valores -1 e 0 de lambda, ou seja, nesse intervalo tem-se uma maior capacidade de explica��o dos dados
# entao, o interessante � trabalhar com alguma trasnforma��o proximo desse intervalo
# o valor 0 � equivalente ao logaritmo, no qual temos mais facilidade em termos da interpreta��o dos dados

#vamos, entao, trabalhar com o logaritmo:

#y. <- log(y) 
ou
y. <- log10(y)
plot(x,y.,pch=16)

#vamos fazer uma compara��o:
par(mfrow=c(1,2))
plot(x,y,pch=16,main = "original")
plot(x,y.,pch=16,main="transforma��o log")
# percebe-se que ficou melhor, pois:
#   - aquele ponto que ficava la longe, o valor discrepante, n�o esta mais tao longe assim
#   - antes a queda parecia algo proximo de uma exponencial, e dps a queda � algo proximo de uma reta, uma queda linear
#   - nota-se que, quando aplicamos essa fun��o log, a gente lineariza fun��o, com algo mais adequado para o conjunto de dados

#Agora vamos ver como fica o modelo ajustado:

fit.model2 <- lm(y.~x)
summary(fit.model2)

#vamos verificar como fica a reta estimada:

par(mfrow=c(1,1))
plot(x,y.,pch=16,main="transforma��o log")
abline(fit.model2)
# percebe-se que agora:
#   - n�o temos mais aquela coisa de em uma regiao os dados estar mais concetrado embaixo, e em outra regiao estar mais concetrado em cima
#   - agora, os dados estao mais equilibrados em torno da reta, metade dos dados acima e metade abaixo da reta
#   - parece que agora temos os pontos oscilando em torno da reta

#Agora vamos avaliar os res�duos:

plot(residuals(fit.model2),pch=16)
abline(h=0,col=2)
# ainda observa-se um outlier, mas ele ta menos distante da massa de dados do q antes
# parece algo mais central, os residuos parecem estar distribuidos aleatoriamente em torno do zero
# os dados estao oscilando de forma aleatoria
# agora parece mais razoavel assumir uma variancia constante e media zero pro erro

#agora vamos analisar os residuos de informa��o deletada:

plot(rstudent(fit.model2),pch=16)
abline(h=0,col=2)
abline(h=2,col=2)
abline(h=-2,col=2)
# mesma analise do grafico anterior
# nao conseguimos resolver o problema do outlier, mas percebe-se que a distancia dele em rela��o a massa de dados diminuiu

#vamos fazer o boxplot agra so pra confirmar as analises anteriores:

boxplot(residuals(fit.model2)) # ainda nota-se o outlier, mas mais proximo da caixa
boxplot(rstudent(fit.model2))  # ainda nota-se o outlier, mas mais proximo da caixa

#vamos analisaer em rela��o a variavel explicativa:

plot(x,rstudent(fit.model2),pch=16)
abline(h=0)
abline(h=-2.07,lty=2,col=2)
abline(h=2.07,lty=2,col=2)

#fazendo o qqnorm:

qqnorm(rstudent(fit.model2))
abline(0,1)
# antes os pontos ficavam longe da diagonal, estavam fora dela, agora eles estao mais proximos dela, mais coladinho