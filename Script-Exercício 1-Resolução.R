# Exercicio

# a,b,c)
x <- c(35.3,29.7,30.8,58.8,61.4,71.3,74.4,76.7,70.7,57.5,46.4,28.9,
          28.1,39.1,46.8,48.5,59.3,70.0,70.0,74.5,72.1,58.1,44.6,33.4,
          28.6)

y <- c(10.98,11.13,12.51,8.40,9.27,8.73,6.36,8.50,7.82,9.14,8.24,12.19,
           11.88,9.57,10.94,9.58,10.09,8.11,6.83,8.88,7.68,8.47,8.86,10.36,
           11.08)

plot(x,y,pch=16)
cor(x,y) # = -0.8452441 
cor.test(x,y) # p-valor baixo, logo, temos forte evidencia pra confirmar que, de fato, existe uma correlacao linear negativa entre as variaveis.

n <- length(y)
numerador <-  sum(x*y) - (sum(x)*sum(y)/n)
denominador <-  sum(x^2) - (sum(x)^2/n)
beta_1 <- numerador/denominador # = -0.07982869

beta_0 <-  mean(y) - (beta_1*mean(x)) # = 13.62299

abline(a=beta_0,b=beta_1, col=2)

beta_0
beta_1

#Portanto, nosso modelo e dado por: Y = beta_0 + beta_1*x + e


# d)
y_estimado <- beta_0+beta_1*x


# e)
residuo <-  y - y_estimado
residuo
plot(residuo,pch=16)
abline(h=0,col=2)

sum(residuo) # = 1.332268e-14, note que e muito proximo de zero


# estimacao da variancia e do desvio padrao do erro
SQRes <-  sum(residuo^2) # = 18.2234
MSRes <-  SQRes/(n-2) # = 0.7923217, que e a estimacao da variancia do erro
dp_estimado_do_erro <- sqrt(MSRes) # = 0.8901245, que e a estimacao do desvio padrao do erro



################################Resolução Completa###################################

# Exercício

x<-c(35.3,29.7,30.8,58.8,61.4,71.3,74.4,76.7,70.7,57.5,46.4,28.9,
     28.1,39.1,46.8,48.5,59.3,70,70,74.5,72.1,58.1,44.6,33.4,28.6)

y<-c(10.98,11.13,12.51,8.40,9.27,8.73,6.36,8.50,7.82,9.14,8.24,12.19,
     11.88,9.57,10.94,9.58,10.09,8.11,6.83,8.88,7.68,8.47,8.86,10.36,11.08)

plot(x,y,xlab= "temperatura média", ylab="quantidade de vapor")
cor(x,y)

n<-length(y)
beta1<-(sum(x*y)-((sum(x))*(sum(y))/n))/(sum(x^2)-((sum(x))^2/n))

beta0<-mean(y) - beta1*mean(x)

abline(a=beta0,b=beta1,col=2)

yyest=beta0 + beta1*x
yyest

plot(x,y,xlab= "temperatura média", ylab="quantidade de vapor")
abline(beta0,beta1)

residuo<-y-yyest
sum(residuo)

sum(residuo^2)/(n-2)

cbind(y,yyest,residuo)


sigmaa2<-(sum(residuo^2))/(n-2)
sqrt(sigmaa2)

varbeta1<-sigmaa2/(sum((x-mean(x))^2))
sqrt(varbeta1)

varbeta0<-sigmaa2*((1/n)+ ((mean(x))^2/(sum((x-mean(x))^2))))
sqrt(varbeta0)


##IC 95% - beta0##
beta0-qt(0.975,n-2)*sqrt(varbeta0)
beta0+qt(0.975,n-2)*sqrt(varbeta0)
ICB0<-c(beta0-qt(0.975,n-2)*sqrt(varbeta0),beta0+qt(0.975,n-2)*sqrt(varbeta0))


##Teste: H0 beta0=0 e H1 beta0 != 0 ; ao nível de 5%##
tt0=beta0/sqrt(varbeta0)
c(-qt(0.975,n-2),+qt(0.975,n-2))

valorp<-pt(-tt0,n-2) + pt(tt0,n-2, lower.tail = FALSE) 

2*pt(-tt0,n-2)


##IC 95% - beta1##

beta1-qt(0.975,n-2)*sqrt(varbeta1)
beta1+qt(0.975,n-2)*sqrt(varbeta1)
ICB1<-c(beta1-qt(0.975,n-2)*sqrt(varbeta1),beta1+qt(0.975,n-2)*sqrt(varbeta1))


##Teste: H0 beta1=0 ## teste bilateral ao nível de 5%
tt1=beta1/sqrt(varbeta1)
c(-qt(0.975,n-2),+qt(0.975,n-2))

valorp<- 2*pt(tt1,n-2)


##IC 95% - sigma2##

sum(residuo^2)/qchisq(0.975,n-2)
sum(residuo^2)/qchisq(0.025,n-2)
ICsig2<-c(sum(residuo^2)/qchisq(0.975,n-2),sum(residuo^2)/qchisq(0.025,n-2))


##IC - E(Y)##
X0=35.3
yyest0<-beta0 + beta1*X0
A0<-((1/n) + (((X0-mean(x))^2)/((sum((x-mean(x))^2)))))
sigmaa2*A0


#IC de 95% 

bconfI0<-yyest0 -(qt(0.975,n-2)*(sqrt(sigmaa2*A0)))
bconfS0<-yyest0 + (qt(0.975,n-2)*(sqrt(sigmaa2*A0)))
ICY0<-c(yyest0 -(qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(A0))),yyest0 + (qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(A0))))


##IC - E(Y)##
#considerando X0 os valores que ocorreram na amostra, ou seja, x
AA<-((1/n) + (((x-mean(x))^2)/((sum((x-mean(x))^2)))))

bconfI<-yyest -(qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(AA)))
bconfS<-yyest + (qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(AA)))
cbind(x,y,bconfI,yyest,bconfS)

ii<-order(yyest)
plot(x,y,xlab= "temperatura média", ylab="quantidade de vapor")
abline(beta0,beta1)
lines(x[ii],bconfI[ii], col=2, lty=2)
lines(x[ii],bconfS[ii], col=2, lty=2)

lines(x,bconfI,col=2,lty=2)


##predição de novas observações##
xn<-seq(0,120,3)
ynew<-beta0+beta1*xn
cbind(xn,ynew)


##Int Predição de 95%##
BB<-(1+(1/n) + (((xn-mean(x))^2)/((sum((x-mean(x))^2)))))

bpredI<-ynew -(qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(BB)))
bpredS<-ynew + (qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(BB)))
IPRED<-c(ynew -(qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(BB))),ynew + (qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(BB))))
cbind(xn,ynew,bpredI,bpredS)


##IC de 95%
AA<-((1/n) + (((xn-mean(x))^2)/((sum((x-mean(x))^2)))))

bconfI<-ynew -(qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(AA)))
bconfS<-ynew + (qt(0.975,n-2)*(sqrt(sigmaa2))*(sqrt(AA)))


matplot(xn, cbind(ynew,bconfI,bconfS, bpredI,bpredS),
        lty = c(1,2,2,3,3), col=c(1,2,2,3,3), type = "l",xlab= "temperatura média", ylab="quantidade de vapor")


####################################Outra forma de Resolver###################

x<-c(35.3,29.7,30.8,58.8,61.4,71.3,74.4,76.7,70.7,57.5,46.4,28.9,
     28.1,39.1,46.8,48.5,59.3,70,70,74.5,72.1,58.1,44.6,33.4,28.6)

y<-c(10.98,11.13,12.51,8.40,9.27,8.73,6.36,8.50,7.82,9.14,8.24,12.19,
     11.88,9.57,10.94,9.58,10.09,8.11,6.83,8.88,7.68,8.47,8.86,10.36,11.08)

mod1 <- lm(y~x)
summary(mod1)

plot(x,y,xlab = "temperatura média", ylab = "quantidade de vapor")
abline(mod1)

yyest <- mod1$coefficients[1]+mod1$coefficients[2]*x
yyest

cbind(yyest,mod1$fitted.values)
sum(yyest)
sum(y)

residuo <- mod1$residuals
resid(mod1)
sum(residuo)

confint(mod1)

bconfEy <- predict(lm(y~x),interval = "confidence")
cbind(yyest,x,bconfEy)
ii <- order(yyest)
matplot(x[ii],bconfEy[ii,],lty = c(1,2,3),col = c(1,2,3), type = "l",xlab = "x",ylab = "y")


####################################Novas Obserações############

new <- data.frame(x=seq(0,25,3))

pred.w.plim <- predict(lm(y~x),new,interval = "prediction")
pred.w.clim <- predict(lm(y~x),new,interval = "confidence")

matplot(new$x, cbind(pred.w.clim,pred.w.clim[,-1]),
        lty = c(1,2,2,3,3), col = c(1,2,2,3,3), xlab = "x", ylab = "y")

plot(residuo,pch=16) 
abline(0,0)

plot(x,residuo,pch=16)
abline(0,0)

plot(yyest,residuo,pch=16)
abline(0,0)


###############################Resíduo##########################

#Resíduo Semistudentizado

ri <- residuo/summary(mod1)$sigma
plot(ri,pch=16)
abline(0,0)


#Resíduo Studentizado

n <- length(y)
ei <- resid(mod1)
s <- summary(mod1)$sigma
h <- (1/n+(x-mean(x))^2/sum((x-mean(x))^2)) #hii
h2 <- lm.influence(mod1)$hat #hii
cbind(h,h2)

ti <- ei/(s*sqrt(1-h))
plot(ti,pch=16)
abline(0,0)


#resíduo studentizado com informação deletada

tsi <- ti*((n-2-1)/(n-2-ti^2))^.5

plot(tsi,pch=16   )
abline(0,0)

rstandard(mod1) # nos fornece o resíduo studentizado
rstudent(mod1) # nos fornece o resíduo studentizado com   informação deletada
cbind(ti,rstandard(mod1),tsi,rstudent(mod1))


###########################ANOVA#############################

SQRes <- sum(residuo^2)
QMRes <- SQRes/(n-2)
sqrt(QMRes) # = summary(mod2)$sigma
SQReg <- sum((yyest-mean(y))^2)
QMReg <- SQReg/1

Fest <- QMReg/QMRes # = estatistica F
SQReg/sum((y-mean(y))^2)
p_valor <- 1-pf(Fest,1,n-2)

anova(mod1)


#obtendo as somas quadraticas diretamente da função anova

aa <- anova(mod1)
aa$`Sum Sq`[1]
aa$`Sum Sq`[2]


##################R^2(Coeficiente de Determinação)#############

SQT <- sum((y-mean(y))^2) 
SQT <- SQReg+SQRes
SQReg/SQT # = R^2

bb <- aa$`Sum Sq`[1]+aa$`Sum Sq`[2] # = SQT
R2 <- aa$`Sum Sq`[1]/bb
R2


############Coeficiente de Correlação###########################

r <- sqrt(R2)
cor(x,y)