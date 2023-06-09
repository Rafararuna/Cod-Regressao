########## Prova 1 - Regress�o - Rafael Santana Araruna##################

## 1.Introdu��o

library(tidyverse)
dados <- read.table(file.choose(), header = F)
set.seed(180026798) #colocar o n�mero da matr�cula
amostra <- sample(1:120,size = 40, replace = F)
y <- dados$V1[amostra]
x <- dados$V2[amostra]
n <- 40

## 2.An�lise Explorat�ria

##2.1 An�lise da Vari�vel Resposta (Y)

library(ggplot2)

box_y <- ggplot(dados, aes(x=factor(""), y=V1)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota m�dia no final do primeiro ano")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

medidas_y <- summary(dados$V1)

#2.2 An�lise da Vari�vel Resposta (X)

library(ggplot2)

box_x <- ggplot(dados, aes(x=factor(""), y=V2)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Pontua��o no Teste ACT")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

medidas_x <- summary(dados$V2)

## 3.An�lise Bidimensional

plo_xy <- plot(x,y, pch=16,col = "blue",xlab = "Pontua��o no Teste ACT", 
               ylab = "Nota m�dia no final do primeiro ano")

r <- cor(x,y)

cor.test(x,y)

## 4. An�lise das Estimativas

## 4.1 C�lculo das estimativas

#estimando beta1:
beta1 <-  sum(y*(x-mean(x))/sum((x-mean(x))^2))
beta1 <-  (sum(x*y)-sum(y)*sum(x)/n)/(sum(x^2)-sum(x)^2/n)
beta1 <-  (sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*mean(x)^2)

#estimando beta0:
beta0 <-  mean(y)-beta1*mean(x)

#monta o modelo e faz o plot:

Y = beta0 + beta1X

plot(x,y,col="blue", pch=16,xlab = "Pontua��o no Teste ACT", 
     ylab = "Nota m�dia no final do primeiro ano")
abline(beta0,beta1, col=2)

## 4.2 IC (com 95%) para as estimativas

#calculando estimativas necess�rias: 

##1) calculando o sigma2 estimado:
p.sigma2 = sum((y-beta0-beta1*x)^2)/(n-2) #sigma2 estimado n�o viesado

##2)c�lculo da variancia estimada para beta1:
v.beta1 <- sigma2/sum((x-mean(x))^2) # vari�ncia de beta1
sqrt(v.beta1) # desvio padr�o de beta1

pv.beta1 <- p.sigma2/sum((x-mean(x))^2) # vari�ncia estimada de beta1

##3)c�lculo da variancia estimada para beta0:
v.beta0 <- sigma2*((1/n) + (mean(x)^2/sum((x-mean(x))^2))) # vari�ncia de beta0
sqrt(v.beta0) # desvio padr�o de beta0

pv.beta0 <- p.sigma2*((1/n) + (mean(x)^2/sum((x-mean(x))^2))) # vari�ncia estimada de beta0

#calculando os intervalos de cada estimativa:

##1)IC de beta1 para 95% com sigma2 estimado:
LI2.beta1 = beta1 - qt(.975,n-2)*sqrt(pv.beta1) 
LS2.beta1 = beta1 + qt(.975,n-2)*sqrt(pv.beta1)
cbind(LI2.beta1,beta1,LS2.beta1)

##2)IC de beta0 para 95% com sigma2 estimado:
LI2.beta0 = beta0 - qt(.975,n-2)*sqrt(pv.beta0) 
LS2.beta0 = beta0 + qt(.975,n-2)*sqrt(pv.beta0) 
cbind(LI2.beta0,beta0,LS2.beta0)

#6) IC para 95% de sigma2 estimado:
y_estimado <- beta0+beta1*x
residuo <-  y - y_estimado

LI2_p.sigma2 <- sum(residuo^2)/qchisq(0.975,n-2)
LS2_p.sigma2 <- sum(residuo^2)/qchisq(0.025,n-2)
cbind(LI2_p.sigma2,p.sigma2,LS2_p.sigma2)

## 4.3 Testes de hip�teses:

#realizando testes:

#1) teste t:

#ho) ausencia de correla��o
#h1) existe correla��o 

est_t <- (beta1 - 0)/(sqrt(pv.beta1)) 

alfa <- 0.05
qt((1-alfa/2),n-2) 
#RC = {t<-2.024394,t>2.024394}

#Resposta: est_t pertence a RC, portanto, h� evid�ncias para rejeitar Ho.

est_t = beta1/sqrt(pv.beta1)
c(-qt(0.975,n-2),+qt(0.975,n-2))
valorp <- 2*(1-pt(est_t,n-2)) 

#teste de beta1 � a mesma ideia, s� muda as hip�teses.

#2) teste F:
modp <- lm(y~x)
summary(modp)
anova(modp)

## 5 Bandas de confian�a para 95% de confian�a

#calculando as observa��es ajustadas:
y_estimado <- beta0+beta1*x

#calculando as bandas de confian�a:
AA<-((1/n) + (((x-mean(x))^2)/((sum((x-mean(x))^2)))))

#intervalo das bandas de confian�a:
bconfI<- y_estimado -(qt(0.975,n-2)*(sqrt(p.sigma2))*(sqrt(AA)))
bconfS<- y_estimado + (qt(0.975,n-2)*(sqrt(p.sigma2))*(sqrt(AA)))
cbind(x,y,bconfI,y_estimado,bconfS)

#plotando no gr�fico:
ii<-order(y_estimado)
jpeg(file='banda_de_confianca.jpg',quality=100)
plot(x,y,col = "blue",pch=16, xlab= "Pontua��o no Teste ACT", 
     ylab="Nota m�dia no final do primeiro ano")
abline(beta0,beta1,col= "blue")
lines(x[ii],bconfI[ii], col=2, lty=2)
lines(x[ii],bconfS[ii], col=2, lty=2)
dev.off()

## 6 An�lise de Diagn�stico

modp <- lm(y~x)

#primeiro, vamos calcular os res�duos:

#res�duo:
residuo <-  y - y_estimado

#Res�duo Semistudentizado:
ri <- residuo/summary(modp)$sigma
ri <- residuo/sqrt(p.sigma2)

#Res�duo Studentizado:
n <- length(y)
residuo <- resid(modp)
p.sigma2 <- (summary(modp)$sigma)^2
h <- (1/n+(x-mean(x))^2/sum((x-mean(x))^2)) #hii
h2 <- lm.influence(modp)$hat #hii
ti <- residuo/(p.sigma2*sqrt(1-h))

#ou:
rstandard(modp) # nos fornece o res�duo studentizado

#res�duo studentizado com informa��o deletada:
tsi <- ti*((n-2-1)/(n-2-ti^2))^.5

#ou:
rstudent(modp) # nos fornece o res�duo studentizado com informa��o deletada

## 6.1 An�lise de Diagn�stico com gr�ficos

#1) Linearidade do modelo:

plot(x,y,col="blue", pch=16,xlab = "Pontua��o no Teste ACT", 
     ylab = "Nota m�dia no final do primeiro ano")
abline(beta0,beta1, col=2)

#2) Independencia do erros:

plot(modp$residuals, col = "blue", pch=16, xlab = "", 
     ylab = "Res�duos e")
abline(h=0, col=2)


#3) Vari�ncia constante (usa o res�duo normal ou o ti):

plot(x,modp$residuals,col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e")
abline(h=0,col=2)

plot(modp$fitted.values,modp$residuals,col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Res�duos e")
abline(h=0,col=2)
abline(h=0,col=2)

plot(x,abs(modp$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e absoluto")
abline(h=0,col=2)

plot(x,(modp$residuals)^2,col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e ao quadrado")
abline(h=0,col=2)

plot(modp$fitted.values,abs(modp$residuals),col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Res�duos e absoluto")
abline(h=0,col=2)

plot(modp$fitted.values,(modp$residuals)^2,col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Res�duos e ao quadrado")
abline(h=0,col=2)

#4) Normalidade (fazer com o res�duo normal e com o rstudent):

boxplot(modp$residuals, col="blue",  xlab = "Res�duos e")

hist(modp$residuals,col="blue", xlab = "Res�duos e", 
     ylab = "Frequ�ncia",title = "")

qqnorm(modp$residuals, col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra")
qqline(modp$residuals,col=2)

qqnorm(tsi, col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra")
abline(0,1,col=2)
# o grafico acima compara a dist normal com os quantis teoricos da dist normal, 
# o adequado � que esses pontos estejam proximos da diagonal.

#5) Pontos Extremos (melhor usar o padronizado ou o studentizado ou o ti):

boxplot(rstudent(modp), col = "blue", xlab = "Res�duos tsi")


plot(modp$fitted.values,rstudent(modp),col = "blue",pch=16, 
     xlab = "y ajustado", ylab = "tsi")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)

#6) Verificar se tem tend�ncia em rela��o a variavel explicativa x:

plot(x,ti,col = "blue", pch=16, xlab = "x", ylab = "ti") 
abline(0,0)
# para verificar alguma tendencia em rela��o a variavel explicativa, o adequado 
# abline(0,0) � que estejam aleatorizados em torno de 0. 

## 6.2 An�lise de Diagn�stico com testes

modp <- lm(y~x)

#1) Normalidade (fazer com o res�duo normal e com o rstudent):

shapiro.test(modp$residuals)

install.packages("olsrr")
library(olsrr)

ols_test_normality(modp)

#2) Correla��o Serial dos erros (pra ver se os erros sao independentes):

require(lmtest)

dwtest(modp) # teste de durbin watson

#3) Homogeneidade de variancia (usa o res�duo normal ou o ti):

# Teste de brown-forsythe:

install.packages("lawstat")
library(lawstat)
require(lawstat)

#temos que dividir em dois grupos:

grupo <- rep(1,length(x))
grupo[x>median(x)] <- 2
grupo

#agora aplicamos os teste:

#a)Teste de Levene:
leveneTest(modp$residuals,grupo)


#b) Teste de Breusch-Pagan:
install.packages("lmtest")
require(lmtest)
library(lmtest)

bptest(modp)

ols_test_breusch_pagan(modp)

#4) Lack of Fit (teste de falta de ajustamento do modelo):

install.packages("EnvStats")
library(EnvStats)

library(olsrr)
ols_pure_error_anova(modp)

anovaPE(modp) # anova do erro puro
#obs: - est� rejeitando a hip�tese nula do modelo do LF comparado com o EP, 
#       ent�o, temos evid�ncias de que o modelo n�o esta bem ajustado
#      - se os dados estao bem ajustados, a tendencia � de que Ho nao seja rejeitada, 
#      ou seja, a falta de ajuste nao estaria muito acima do que seria o erro puro 
#      do modelo.

## 7. Medida Corretiva

#gr�fico pra saber qual trasnforma��o realizar e qual lambda usar:

library(car)
boxCox(modp,col = "blue", lambda = c(0,5,0.5), ylab = "Log-Probabilidade")

#realizando a transforma��o pro lambda que maximiza a fun��o:

y. <- ((y^2)-1)/2
plot(x,y.,pch=16)

modp2 <- lm(y.~x)
summary(modp2)

r <- cor(x,y.)
# o r diminuiu em rela��o ao Y sem trasnforma��o

#an�lise de diagn�stico:

#a) com gr�ficos:

#1) Linearidade do modelo:

#estimando beta1:
beta1. <-  sum(y.*(x-mean(x))/sum((x-mean(x))^2))
beta1.  <-  (sum(x*y.)-sum(y)*sum(x)/n)/(sum(x^2)-sum(x)^2/n)
beta1. <-  (sum(x*y.)-n*mean(x)*mean(y.))/(sum(x^2)-n*mean(x)^2)

#estimando beta0:
beta0. <-  mean(y.)-beta1.*mean(x)

plot(x,y.,col="blue", pch=16,xlab = "Pontua��o no Teste ACT", 
     ylab = "Nota m�dia no final do primeiro ano")
abline(beta0.,beta1., col=2)

#2) Independencia do erros:

plot(modp2$residuals, col = "blue", pch=16, xlab = "", 
     ylab = "Res�duos e")
abline(h=0, col=2)

#3) Vari�ncia constante (usa o res�duo normal ou o ti):

plot(x,modp2$residuals,col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e")
abline(h=0,col=2)

plot(modp2$fitted.values,modp2$residuals,col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Res�duos e")
abline(h=0,col=2)
abline(h=0,col=2)

plot(x,abs(modp2$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e absoluto")
abline(h=0,col=2)

plot(x,(modp2$residuals)^2,col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e ao quadrado")
abline(h=0,col=2)

plot(modp2$fitted.values,abs(modp2$residuals),col = "blue", pch=16,
     xlab = "y ajustado", ylab = "Res�duos e absoluto")
abline(h=0,col=2)

plot(modp2$fitted.values,(modp2$residuals)^2,col = "blue", pch=16, 
     xlab = "x", ylab = "Res�duos e ao quadrado")
abline(h=0,col=2)

#4) Normalidade (fazer com o res�duo normal e com o rstudent):

boxplot(modp2$residuals, col="blue",  xlab = "Res�duos e", 
        ylab = "Frequ�ncia")

hist(modp2$residuals,col="Blue", xlab = "Res�duos e", 
     ylab = "Frequ�ncia",title = "")

qqnorm(modp2$residuals, col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra")
qqline(modp2$residuals,col=2)

qqnorm(rstudent(modp), col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra")
abline(0,1,col=2)
# o grafico acima compara a dist normal com os quantis teoricos da dist normal, 
# o adequado � que esses pontos estejam proximos da diagonal.

#5) Pontos Extremos (melhor usar o padronizado ou o studentizado ou o ti):

boxplot(modp2$residuals, col = "blue", xlab = "Res�duos e", ylab = "Frequ�ncia")

plot(modp2$fitted.values,rstudent(modp2), col = "blue",pch=16, 
     xlab = " y ajustado", ylab = "tsi")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)

#6) Verificar se tem tend�ncia em rela��o a variavel explicativa x:

plot(x,ti,col= "blue",pch=16, xlab = "x", ylab = "ti") 
abline(0,0)
# para verificar alguma tendencia em rela��o a variavel explicativa, 
# o adequado abline(0,0) � que estejam aleatorizados em torno de 0. 

#b) analise de diagnostico com testes:

#1) Normalidade (fazer com o res�duo normal e com o rstudent):

shapiro.test(modp2$residuals)

install.packages("olsrr")
library(olsrr)

ols_test_normality(modp2)

#2) Correla��o Serial dos erros (pra ver se os erros sao independentes):

require(lmtest)

dwtest(modp2) # teste de durbin watson

#3) Homogeneidade de variancia (usa o res�duo normal ou o ti):

# Teste de brown-forsythe:

install.packages("lawstat")
library(lawstat)
require(lawstat)

#temos que dividir em dois grupos:

grupo <- rep(1,length(x))
grupo[x>median(x)] <- 2
grupo

#agora aplicamos os teste:

#a)Teste de Levene:
leveneTest(modp2$residuals,grupo)

#b) Teste de Breusch-Pagan:
install.packages("lmtest")
require(lmtest)
library(lmtest)

bptest(modp2)

ols_test_breusch_pagan(modp2)

#4) Lack of Fit (teste de falta de ajustamento do modelo):

install.packages("EnvStats")
library(EnvStats)

library(olsrr)
ols_pure_error_anova(modp2)

anovaPE(modp2) # anova do erro puro
#obs: - est� rejeitando a hip�tese nula do modelo do LF comparado com o EP, ent�o, 
#      temos evid�ncias de que o modelo n�o esta bem ajustado
#      - se os dados estao bem ajustados, a tendencia � de que Ho nao seja rejeitada, 
#      ou seja, a falta de ajuste nao estaria muito acima do que seria o 
#      erro puro do modelo.

#comparando:

#nao mudou em rela��o a linearidade quanto aos gr�ficos:
par(mfrow=c(1,2))
plot(x,y,col="blue", pch=16,xlab = "Pontua��o no Teste ACT", 
     ylab = "Nota m�dia no final do primeiro ano",
     main = "Original")
abline(beta0,beta1, col=2)
plot(x,y.,col="blue", pch=16,xlab = "Pontua��o no Teste ACT",
     ylab = "Nota m�dia no final do primeiro ano", main = "Com Transforma��o")
abline(beta0.,beta1., col=2)

#nao mudou nada em rela��o a independencia dos erros quanto aos graficos:
par(mfrow=c(1,2))
plot(modp$residuals, col = "blue", pch=16, xlab = "", ylab = "Res�duos e")
abline(h=0, col=2)
plot(modp2$residuals, col = "blue", pch=16, xlab = "", ylab = "Res�duos e")
abline(h=0, col=2)

#nao mudou nada em rela��o a homogeneidade da var quanto aos graficos:
par(mfrow=c(1,2))
plot(x,abs(modp$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e absoluto", main = "Original")
abline(h=0,col=2)
plot(x,abs(modp2$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Res�duos e absoluto", main = "Com Transforma��o")
abline(h=0,col=2)

#mudou em rela��o a normalidade quanto aos gr�ficos:

par(mfrow=c(1,2))
qqnorm(modp$residuals, col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra", main = "Original")
qqline(modp$residuals,col=2)
qqnorm(modp2$residuals, col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra", main = " Com Transforma��o")
qqline(modp2$residuals,col=2)

par(mfrow=c(1,2))
qqnorm(rstudent(modp), col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra",main = "Original")
abline(0,1,col=2)
qqnorm(rstudent(modp2), col="blue",xlab = "Quantis Te�ricos", 
       ylab = "Quantis de amostra", main = "Com Transforma��o")
abline(0,1,col=2)

#mudou em rela��o aos valores at�picos quanto aos gr�ficos:

par(mfrow=c(1,2))
plot(modp$fitted.values,rstudent(modp), col = "blue",pch=16, xlab = "y ajustado", 
     ylab = "tsi", main = "Original")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
plot(modp2$fitted.values,rstudent(modp2), col = "blue",pch=16, xlab = " y ajustado",
     ylab = "tsi", main = "Com Transforma��o")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)

## 8. Capacidade Preditiva

SQRes <- sum(residuo^2)
QMRes <- SQRes/(n-2) # � o sigma2 estimado
sqrt(QMRes) # = summary(mod2)$sigma = desvio padr�o

#Soma de Quadrados e Quadrado M�dio da regres�o:
SQReg <- sum((y_estimado-mean(y))^2)
QMReg <- SQReg/1

#Soma de Quadrados Total:
SQT <- sum((y-mean(y))^2) 
SQT <- SQReg+SQRes

R2 <- SQReg/SQT