########## Prova 1 - Regressão - Rafael Santana Araruna##################

## 1.Introdução

library(tidyverse)
dados <- read.table(file.choose(), header = F)
set.seed(180026798) #colocar o número da matrícula
amostra <- sample(1:120,size = 40, replace = F)
y <- dados$V1[amostra]
x <- dados$V2[amostra]
n <- 40

## 2.Análise Exploratória

##2.1 Análise da Variável Resposta (Y)

library(ggplot2)

box_y <- ggplot(dados, aes(x=factor(""), y=V1)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota média no final do primeiro ano")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

medidas_y <- summary(dados$V1)

#2.2 Análise da Variável Resposta (X)

library(ggplot2)

box_x <- ggplot(dados, aes(x=factor(""), y=V2)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Pontuação no Teste ACT")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

medidas_x <- summary(dados$V2)

## 3.Análise Bidimensional

plo_xy <- plot(x,y, pch=16,col = "blue",xlab = "Pontuação no Teste ACT", 
               ylab = "Nota média no final do primeiro ano")

r <- cor(x,y)

cor.test(x,y)

## 4. Análise das Estimativas

## 4.1 Cálculo das estimativas

#estimando beta1:
beta1 <-  sum(y*(x-mean(x))/sum((x-mean(x))^2))
beta1 <-  (sum(x*y)-sum(y)*sum(x)/n)/(sum(x^2)-sum(x)^2/n)
beta1 <-  (sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*mean(x)^2)

#estimando beta0:
beta0 <-  mean(y)-beta1*mean(x)

#monta o modelo e faz o plot:

Y = beta0 + beta1X

plot(x,y,col="blue", pch=16,xlab = "Pontuação no Teste ACT", 
     ylab = "Nota média no final do primeiro ano")
abline(beta0,beta1, col=2)

## 4.2 IC (com 95%) para as estimativas

#calculando estimativas necessárias: 

##1) calculando o sigma2 estimado:
p.sigma2 = sum((y-beta0-beta1*x)^2)/(n-2) #sigma2 estimado não viesado

##2)cálculo da variancia estimada para beta1:
v.beta1 <- sigma2/sum((x-mean(x))^2) # variância de beta1
sqrt(v.beta1) # desvio padrão de beta1

pv.beta1 <- p.sigma2/sum((x-mean(x))^2) # variância estimada de beta1

##3)cálculo da variancia estimada para beta0:
v.beta0 <- sigma2*((1/n) + (mean(x)^2/sum((x-mean(x))^2))) # variância de beta0
sqrt(v.beta0) # desvio padrão de beta0

pv.beta0 <- p.sigma2*((1/n) + (mean(x)^2/sum((x-mean(x))^2))) # variância estimada de beta0

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

## 4.3 Testes de hipóteses:

#realizando testes:

#1) teste t:

#ho) ausencia de correlação
#h1) existe correlação 

est_t <- (beta1 - 0)/(sqrt(pv.beta1)) 

alfa <- 0.05
qt((1-alfa/2),n-2) 
#RC = {t<-2.024394,t>2.024394}

#Resposta: est_t pertence a RC, portanto, há evidências para rejeitar Ho.

est_t = beta1/sqrt(pv.beta1)
c(-qt(0.975,n-2),+qt(0.975,n-2))
valorp <- 2*(1-pt(est_t,n-2)) 

#teste de beta1 é a mesma ideia, só muda as hipóteses.

#2) teste F:
modp <- lm(y~x)
summary(modp)
anova(modp)

## 5 Bandas de confiança para 95% de confiança

#calculando as observações ajustadas:
y_estimado <- beta0+beta1*x

#calculando as bandas de confiança:
AA<-((1/n) + (((x-mean(x))^2)/((sum((x-mean(x))^2)))))

#intervalo das bandas de confiança:
bconfI<- y_estimado -(qt(0.975,n-2)*(sqrt(p.sigma2))*(sqrt(AA)))
bconfS<- y_estimado + (qt(0.975,n-2)*(sqrt(p.sigma2))*(sqrt(AA)))
cbind(x,y,bconfI,y_estimado,bconfS)

#plotando no gráfico:
ii<-order(y_estimado)
jpeg(file='banda_de_confianca.jpg',quality=100)
plot(x,y,col = "blue",pch=16, xlab= "Pontuação no Teste ACT", 
     ylab="Nota média no final do primeiro ano")
abline(beta0,beta1,col= "blue")
lines(x[ii],bconfI[ii], col=2, lty=2)
lines(x[ii],bconfS[ii], col=2, lty=2)
dev.off()

## 6 Análise de Diagnóstico

modp <- lm(y~x)

#primeiro, vamos calcular os resíduos:

#resíduo:
residuo <-  y - y_estimado

#Resíduo Semistudentizado:
ri <- residuo/summary(modp)$sigma
ri <- residuo/sqrt(p.sigma2)

#Resíduo Studentizado:
n <- length(y)
residuo <- resid(modp)
p.sigma2 <- (summary(modp)$sigma)^2
h <- (1/n+(x-mean(x))^2/sum((x-mean(x))^2)) #hii
h2 <- lm.influence(modp)$hat #hii
ti <- residuo/(p.sigma2*sqrt(1-h))

#ou:
rstandard(modp) # nos fornece o resíduo studentizado

#resíduo studentizado com informação deletada:
tsi <- ti*((n-2-1)/(n-2-ti^2))^.5

#ou:
rstudent(modp) # nos fornece o resíduo studentizado com informação deletada

## 6.1 Análise de Diagnóstico com gráficos

#1) Linearidade do modelo:

plot(x,y,col="blue", pch=16,xlab = "Pontuação no Teste ACT", 
     ylab = "Nota média no final do primeiro ano")
abline(beta0,beta1, col=2)

#2) Independencia do erros:

plot(modp$residuals, col = "blue", pch=16, xlab = "", 
     ylab = "Resíduos e")
abline(h=0, col=2)


#3) Variância constante (usa o resíduo normal ou o ti):

plot(x,modp$residuals,col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e")
abline(h=0,col=2)

plot(modp$fitted.values,modp$residuals,col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Resíduos e")
abline(h=0,col=2)
abline(h=0,col=2)

plot(x,abs(modp$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e absoluto")
abline(h=0,col=2)

plot(x,(modp$residuals)^2,col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e ao quadrado")
abline(h=0,col=2)

plot(modp$fitted.values,abs(modp$residuals),col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Resíduos e absoluto")
abline(h=0,col=2)

plot(modp$fitted.values,(modp$residuals)^2,col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Resíduos e ao quadrado")
abline(h=0,col=2)

#4) Normalidade (fazer com o resíduo normal e com o rstudent):

boxplot(modp$residuals, col="blue",  xlab = "Resíduos e")

hist(modp$residuals,col="blue", xlab = "Resíduos e", 
     ylab = "Frequência",title = "")

qqnorm(modp$residuals, col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra")
qqline(modp$residuals,col=2)

qqnorm(tsi, col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra")
abline(0,1,col=2)
# o grafico acima compara a dist normal com os quantis teoricos da dist normal, 
# o adequado é que esses pontos estejam proximos da diagonal.

#5) Pontos Extremos (melhor usar o padronizado ou o studentizado ou o ti):

boxplot(rstudent(modp), col = "blue", xlab = "Resíduos tsi")


plot(modp$fitted.values,rstudent(modp),col = "blue",pch=16, 
     xlab = "y ajustado", ylab = "tsi")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)

#6) Verificar se tem tendência em relação a variavel explicativa x:

plot(x,ti,col = "blue", pch=16, xlab = "x", ylab = "ti") 
abline(0,0)
# para verificar alguma tendencia em relação a variavel explicativa, o adequado 
# abline(0,0) é que estejam aleatorizados em torno de 0. 

## 6.2 Análise de Diagnóstico com testes

modp <- lm(y~x)

#1) Normalidade (fazer com o resíduo normal e com o rstudent):

shapiro.test(modp$residuals)

install.packages("olsrr")
library(olsrr)

ols_test_normality(modp)

#2) Correlação Serial dos erros (pra ver se os erros sao independentes):

require(lmtest)

dwtest(modp) # teste de durbin watson

#3) Homogeneidade de variancia (usa o resíduo normal ou o ti):

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
#obs: - está rejeitando a hipótese nula do modelo do LF comparado com o EP, 
#       então, temos evidências de que o modelo não esta bem ajustado
#      - se os dados estao bem ajustados, a tendencia é de que Ho nao seja rejeitada, 
#      ou seja, a falta de ajuste nao estaria muito acima do que seria o erro puro 
#      do modelo.

## 7. Medida Corretiva

#gráfico pra saber qual trasnformação realizar e qual lambda usar:

library(car)
boxCox(modp,col = "blue", lambda = c(0,5,0.5), ylab = "Log-Probabilidade")

#realizando a transformação pro lambda que maximiza a função:

y. <- ((y^2)-1)/2
plot(x,y.,pch=16)

modp2 <- lm(y.~x)
summary(modp2)

r <- cor(x,y.)
# o r diminuiu em relação ao Y sem trasnformação

#análise de diagnóstico:

#a) com gráficos:

#1) Linearidade do modelo:

#estimando beta1:
beta1. <-  sum(y.*(x-mean(x))/sum((x-mean(x))^2))
beta1.  <-  (sum(x*y.)-sum(y)*sum(x)/n)/(sum(x^2)-sum(x)^2/n)
beta1. <-  (sum(x*y.)-n*mean(x)*mean(y.))/(sum(x^2)-n*mean(x)^2)

#estimando beta0:
beta0. <-  mean(y.)-beta1.*mean(x)

plot(x,y.,col="blue", pch=16,xlab = "Pontuação no Teste ACT", 
     ylab = "Nota média no final do primeiro ano")
abline(beta0.,beta1., col=2)

#2) Independencia do erros:

plot(modp2$residuals, col = "blue", pch=16, xlab = "", 
     ylab = "Resíduos e")
abline(h=0, col=2)

#3) Variância constante (usa o resíduo normal ou o ti):

plot(x,modp2$residuals,col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e")
abline(h=0,col=2)

plot(modp2$fitted.values,modp2$residuals,col = "blue", pch=16, 
     xlab = "y ajustado", ylab = "Resíduos e")
abline(h=0,col=2)
abline(h=0,col=2)

plot(x,abs(modp2$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e absoluto")
abline(h=0,col=2)

plot(x,(modp2$residuals)^2,col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e ao quadrado")
abline(h=0,col=2)

plot(modp2$fitted.values,abs(modp2$residuals),col = "blue", pch=16,
     xlab = "y ajustado", ylab = "Resíduos e absoluto")
abline(h=0,col=2)

plot(modp2$fitted.values,(modp2$residuals)^2,col = "blue", pch=16, 
     xlab = "x", ylab = "Resíduos e ao quadrado")
abline(h=0,col=2)

#4) Normalidade (fazer com o resíduo normal e com o rstudent):

boxplot(modp2$residuals, col="blue",  xlab = "Resíduos e", 
        ylab = "Frequência")

hist(modp2$residuals,col="Blue", xlab = "Resíduos e", 
     ylab = "Frequência",title = "")

qqnorm(modp2$residuals, col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra")
qqline(modp2$residuals,col=2)

qqnorm(rstudent(modp), col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra")
abline(0,1,col=2)
# o grafico acima compara a dist normal com os quantis teoricos da dist normal, 
# o adequado é que esses pontos estejam proximos da diagonal.

#5) Pontos Extremos (melhor usar o padronizado ou o studentizado ou o ti):

boxplot(modp2$residuals, col = "blue", xlab = "Resíduos e", ylab = "Frequência")

plot(modp2$fitted.values,rstudent(modp2), col = "blue",pch=16, 
     xlab = " y ajustado", ylab = "tsi")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)

#6) Verificar se tem tendência em relação a variavel explicativa x:

plot(x,ti,col= "blue",pch=16, xlab = "x", ylab = "ti") 
abline(0,0)
# para verificar alguma tendencia em relação a variavel explicativa, 
# o adequado abline(0,0) é que estejam aleatorizados em torno de 0. 

#b) analise de diagnostico com testes:

#1) Normalidade (fazer com o resíduo normal e com o rstudent):

shapiro.test(modp2$residuals)

install.packages("olsrr")
library(olsrr)

ols_test_normality(modp2)

#2) Correlação Serial dos erros (pra ver se os erros sao independentes):

require(lmtest)

dwtest(modp2) # teste de durbin watson

#3) Homogeneidade de variancia (usa o resíduo normal ou o ti):

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
#obs: - está rejeitando a hipótese nula do modelo do LF comparado com o EP, então, 
#      temos evidências de que o modelo não esta bem ajustado
#      - se os dados estao bem ajustados, a tendencia é de que Ho nao seja rejeitada, 
#      ou seja, a falta de ajuste nao estaria muito acima do que seria o 
#      erro puro do modelo.

#comparando:

#nao mudou em relação a linearidade quanto aos gráficos:
par(mfrow=c(1,2))
plot(x,y,col="blue", pch=16,xlab = "Pontuação no Teste ACT", 
     ylab = "Nota média no final do primeiro ano",
     main = "Original")
abline(beta0,beta1, col=2)
plot(x,y.,col="blue", pch=16,xlab = "Pontuação no Teste ACT",
     ylab = "Nota média no final do primeiro ano", main = "Com Transformação")
abline(beta0.,beta1., col=2)

#nao mudou nada em relação a independencia dos erros quanto aos graficos:
par(mfrow=c(1,2))
plot(modp$residuals, col = "blue", pch=16, xlab = "", ylab = "Resíduos e")
abline(h=0, col=2)
plot(modp2$residuals, col = "blue", pch=16, xlab = "", ylab = "Resíduos e")
abline(h=0, col=2)

#nao mudou nada em relação a homogeneidade da var quanto aos graficos:
par(mfrow=c(1,2))
plot(x,abs(modp$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e absoluto", main = "Original")
abline(h=0,col=2)
plot(x,abs(modp2$residuals),col = "blue", pch=16, xlab = "x", 
     ylab = "Resíduos e absoluto", main = "Com Transformação")
abline(h=0,col=2)

#mudou em relação a normalidade quanto aos gráficos:

par(mfrow=c(1,2))
qqnorm(modp$residuals, col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra", main = "Original")
qqline(modp$residuals,col=2)
qqnorm(modp2$residuals, col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra", main = " Com Transformação")
qqline(modp2$residuals,col=2)

par(mfrow=c(1,2))
qqnorm(rstudent(modp), col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra",main = "Original")
abline(0,1,col=2)
qqnorm(rstudent(modp2), col="blue",xlab = "Quantis Teóricos", 
       ylab = "Quantis de amostra", main = "Com Transformação")
abline(0,1,col=2)

#mudou em relação aos valores atípicos quanto aos gráficos:

par(mfrow=c(1,2))
plot(modp$fitted.values,rstudent(modp), col = "blue",pch=16, xlab = "y ajustado", 
     ylab = "tsi", main = "Original")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
plot(modp2$fitted.values,rstudent(modp2), col = "blue",pch=16, xlab = " y ajustado",
     ylab = "tsi", main = "Com Transformação")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)

## 8. Capacidade Preditiva

SQRes <- sum(residuo^2)
QMRes <- SQRes/(n-2) # é o sigma2 estimado
sqrt(QMRes) # = summary(mod2)$sigma = desvio padrão

#Soma de Quadrados e Quadrado Médio da regresão:
SQReg <- sum((y_estimado-mean(y))^2)
QMReg <- SQReg/1

#Soma de Quadrados Total:
SQT <- sum((y-mean(y))^2) 
SQT <- SQReg+SQRes

R2 <- SQReg/SQT