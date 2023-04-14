#Exercício

n <- 25

x <- c(50,24,46,48,58,60,65,42,42,50,38,30,31,34,30,48,61,71,62,
       38,41,66,31,42,40)

y <- c(68,77,96,80,43,44,26,88,75,57,56,88,88,102,88,70,82,43,46,
       56,59,26,52,83,75)

#a)
plot(x,y,pch=16)
cor(x,y) # = 0.6531434
 
mod2 <- lm(y~x)
mod2
summary(mod2)

#b)

plot(x,y,pch=16)
abline(a=mod2$coefficients[1],b=mod2$coefficients[2], col=2)

#c)

confint(mod2)

#d)

bconf <- predict(mod2,interval="confidence")
cbind(mod2$fitted.values,x,bconf)
ii <- order(mod2$fitted.values)
matplot(x[ii],bconf[ii,],lty = c(1,2,2),col = c(1,2,2),type = "l",xlab = "x",ylab = "y")


#e,f)

anova(mod2)

SQT <- sum((y-mean(y))^2)
SQRes <- sum(resid(mod2)^2)
SQReg <- sum((mod2$fitted.values-mean(y))^2)
R2 <- SQReg/SQT


#analise dos residuos

e <- resid(mod2)
plot(e,pch=16)
abline(0,0)

sig <- summary(mod2)$sigma
ri <- e/sig
plot(ri,pch=16)
abline(0,0)

h <- (1/n+(x-mean(x))^2/sum((x-mean(x))^2))
ti <- e/(sig*sqrt(1-h)) #esse tem desvio padrao igual a 1
plot(ti,pch=16)
abline(0,0)
boxplot(ti) #verificar outliers, valores atípicos

plot(x,ti,pch=16)# para verificar alguma tendencia em relação a variavel explicativa, o adequado é que estejam aleatorizados em torno de 0. 
abline(0,0)

plot(mod2$fitted.values,ti,pch=16)
abline(0,0)


#para verificar se tem variancia constante, pega o quadrado ou o valor absoluto do ti no grafico anterior:

plot(mod2$fitted.values,ti^2,pch=16)
abline(0,0)


#residuo com informação deletada:

tsi <- ti*((n-2-1)/(n-2-ti^2))^.5
plot(tsi,pch=16)
abline(0,0)

abline(h=2,col=2)
abline(h=-2,col=2)
#com esses dois ultimos codigos percebos dois pontos q n estao entre as linhas vermelhas, observalçao o qual nao apareceu no resíduo semistudentizado, que n tem dist. exata t-student.

rstudent(mod2) # acha o residuo com informação deletada
plot(rstudent(mod2),pch=16)

qqnorm(tsi)
abline(0,1)
# o grafico acima compara a dist normal com os quantis teoricos da dist normal, o adequado é que esses pontos estejam proximos da diagonal.
# utilizado para verificar a suposição da normalidade


#obs:

lm.influence(mod2)$sigma # nos forcene o sigma com a informação deletada

 