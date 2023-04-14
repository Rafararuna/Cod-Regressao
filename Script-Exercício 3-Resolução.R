####################################EXEMPLO 1################################

x <- c(125,100,200,75,150,175,75,175,125,200,100)
y <- c(160,112,124,28,152,156,42,124,150,104,136)

plot(x,y,pch=16) # parece uma relação nao linear
cor(x,y) # = 0.5085084, uma correlação moderada

mod1 <- lm(y~x)
summary(mod1) 
#obs: pelo coeficiente angular, no teste do beta1, com Ho) beta1 = 0, a gente nao rejeita
#     a hipotese nula, pois o p-valor é maior que o alfa, então não temos argumentos para
#     comprovar a existência do modelo.

plot(x,y,pch=16)
abline(mod1)
#obs: dá pra ver que os pontos nao estao distribuidos em torno da reta

anova(mod1)

e <- resid(mod1)
plot(e,pch=16)
abline(h=0,col=2) 
#obs: nesse gráfico não parece ter nenhuma tendência

plot(x,e,pch=16)
abline(h=0,col=2) 
#obs: nesse já parece ter alguma tendência, no ínicio os pontos estão na parte
#     negativa, no meio estao na parte positiva, e no fim voltam pra parte negativa,
#     ou seja, os pontos nao estão aleatorizados em torno de zero.

#verificando a normalidade:

shapiro.test(e) # nao rejeitou Ho ???
qqnorm(e) # os pontos não estao mto proximos da diagonal
hist(e) # a gente esperaria um concentraçao em torno do zero, mas nao é o que esta acontecendo

t <- rstudent(mod1)
plot(x,t,pch=16)
abline(h=0,col=2) 

#verificando a normalidade:

shapiro.test(t) # também nao rejeitou Ho
qqnorm(t,pch=16) # os pontos não estao mto proximos da diagonal
hist(t) # a gente esperaria um concentraçao em torno do zero, mas nao é o que esta acontecendo

# obs: o shapiro.teste nao esta rejeitando, mas claramente tem uma tendencia dos residuo 
#      que n esta sendo capitada pelo modelo

#vamos avaliar agroa o teste da falta de ajuste, do erro puro:

install.packages("EnvStats")
library(EnvStats)

anovaPE(mod1) # anova do erro puro
#obs: - está rejeitando a hipótese nula do modelo do LF comparado com o EP, então, temos evidências de que o modelo não esta bem ajustado
#     - se os dados estao bem ajustados, a tendencia é de que Ho nao seja rejeitada, ou seja, a falta de ajuste nao estaria muito acima do que seria o erro puro do modelo.


#teste de breusch-pagan:

install.packages("lmtest")
library(lmtest)

bptest(mod1) # teste de variancia constante
#obs: - a 5% nao está rejeitando, mas a 10% a gente rejeitaria bem no limiar

# vamos fazer transformações pra ver se ajudam:

mod2 <- lm(log(y)~log(x))
summary(mod2)
#obs: - o R2 aumentou
#     - sobre o teste do beta1, a Ho passa a ser rejeitada

plot(log(x),log(y),pch=16)
#obs: - porem, quando fazemos a analise gráfica, a gente que ve que continou a mesma relação, ou seja
#       a relação de nao lineariade, melhora um pouco, mas nao resolve

#vamos fazer agora o modelo completo (onde a gente faz o ajuste de media para cada nivel):

mod3 <- lm(y~factor(x))
summary(mod3)
anova(mod1,mod3)


####################################EXEMPLO 2################################

x <- c(50,24,46,48,58,60,65,42,42,50,38,30,31,34,30,48,61,71,62,38,
       41,66,31,42,40)
y <- c(68,77,96,80,43,44,26,88,75,57,56,88,88,102,88,70,82,43,46,
       56,59,26,52,83,75)

plot(x,y,xlab = "gravidade da doença", ylab = "satisfação",pch=16)
cor(x,y) # = -0.6531434
#obs: -parece ter uma relação linear negativa

mod1 <- lm(y~x)
summary(mod1)

plot(x,y,xlab = "gravidade da doença", ylab = "satisfação",pch=16)
abline(mod1)
#obs: - parecem estar distribuidos de forma aleatoria em torno da reta

#vamos fazer uma analise de diagnostico:

#1) Linearidade:

plot(x,rstudent(mod1),pch=16)
abline(h=0,col=2)

plot(mod1$fitted.values, rstudent(mod1),pch=16)
abline(h=0,col=2)

#2) Independencia:

plot(mod1$residuals,pch=16)
abline(h=0)

#3) Variancia constante:

plot(x,mod1$residuals,pch=16)
abline(h=0,col=2)

plot(mod1$fitted.values,mod1$residuals,pch=16)
abline(h=0,col=2)

plot(x,abs(mod1$residuals),pch=16)
plot(x,(mod1$residuals)^2,pch=16)

plot(mod1$fitted.values,abs(mod1$residuals),pch=16)
plot(mod1$fitted.values,(mod1$residuals)^2,pch=16)

#4) Normalidade:

boxplot(mod1$residuals) #espera que a mediana esteja proxima do zero

hist(mod1$residuals)

qqnorm(mod1$residuals)
qqline(mod1$residuals)

#5) Pontos Extremos (melhor usar o padronizado ou o studentizado):

boxplot(mod1$residuals)

plot(mod1$fitted.values,rstudent(mod1),pch=16)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)


#Testes de hipótese

#1) Normalidade

shapiro.test(mod1$residuals)

install.packages("olsrr")
library(olsrr)

ols_test_normality(mod1)

#2) Correlação Serial:

require(lmtest)

dwtest(mod1) # teste de durbin watson

#3) Homogeneidade de variancia:

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
levene.test(mod1$residuals,grupo)

#b) Teste de Breusch-Pagan:
require(lmtest)
bptest(mod1)

ols_test_breusch_pagan(mod1)

#4) Correlação Serial dos erros:

dwtest(mod1)


############################EXEMPLO 3##############################

#Exemplo do dia 03 de março (Continuação):

x <- c(rep(0,5),rep(1,5),rep(2,5),rep(3,5),rep(4,5))
y <- c(13.44,12.84,11.91,20.09,15.60,10.11,11.38,10.28,
       8.96,8.59,9.83,9,8.65,7.85,8.88,7.94,6.01,5.14,
       6.9,6.77,4.86,5.1,5.67,5.75,6.23)

plot(x,y,pch=16)
cor(x,y) # = -0.8678991

#Vamos aplicar os teste antes da trasnformação:

#1) Normalidade:

shapiro.test(fit.model$residuals)

ols_test_normality(fit.model)

#2) Homogeneidade de variancia:

# Teste de Brown-Forsythe:

require(lawstat)

#temos que dividir em dois grupos:

grupo <- rep(1,length(x))
grupo[x>=median(x)] <- 2
grupo

#agora aplicamos os testes:

#a) Teste de Levene:

levene.test(fit.model$residuals,grupo)

#b) Teste de Breusch-Pagan:

require(lmtest)
bptest(fit.model)

ols_test_breusch_pagan(fit.model)

#4) Correlação Serial dos erros:

dwtest(fit.model)


#5) Lack of Fit:

library(EnvStats)
anovaPE(fit.model)

library(olsrr)
ols_pure_error_anova(fit.model)

#Vamos aplicar os teste depois da trasnformação:

#transformação:

y. <- log10(y)
plot(x,y.,pch=16)
fit.model2 <- lm(y.~x)
summary(fit.model2)

#agora aplicamos os testes:

#1) Normalidade:

shapiro.test(fit.model2$residuals)

ols_test_normality(fit.model2)

#2) Homogeneidade da variancia:

# Teste de Brown-Forsythe:

require(lawstat)

#temos que dividir em dois grupos:

grupo <- rep(1,length(x))
grupo[x>=median(x)] <- 2
grupo

#agora aplicamos os testes:

#a) Teste de Levene:

leveneTest(fit.model2$residuals,grupo)

#b) Teste de Breusch-Pagan:

require(lmtest)
bptest(fit.model2)

ols_test_breusch_pagan(fit.model2)

#4) Correlação Serial dos erros:

dwtest(fit.model2)

#5) Lack of Fit:

library(EnvStats)
anovaPE(fit.model2)

library(olsrr)
ols_pure_error_anova(fit.model2)

#agora vamos fazer uma analise sem o valor discrepante:

fit.model3 <- lm(y.[-4]~x[-4])
summary(fit.model3)
summary(fit.model2)
