# Exercício 1:

n <- 25

x <- c(50,24,46,48,58,60,65,42,42,50,38,30,31,34,30,48,61,71,62,38,
       41,66,31,42,40)
y <- c(68,77,96,80,43,44,26,88,75,57,56,88,88,102,88,70,82,43,46,
       56,59,26,52,83,75)

mod1 <- lm(y~x)

#Vamos construir a matrix x:

x <- cbind(rep(1,n),x)

#a) x'x :

t(x)%*%x
#linha1-coluna1 = n
#liha1-coluna2 = soma de x, sum(x)
#linha2-coluna1 = soma de x, sum(x)
#linha2-coluna2 = soma de x ao quadrado, sum(x^2)

#b) x'y:

t(x)%*%y
#linha1-coluna1 = soma de y, sum(y)
#linha2-coluna1 = soma de x*y, sum(x*y)

#c) equação normal:

(t(x)%*%x)%*%beta_chapeu = t(x)%*%y

#d) vamos obter a matriz beta_chapeu

solve(t(x)%*%x) # para achar a matriz inversa

beta_chapeu <- solve(t(x)%*%x)%*%(t(x)%*%y)

#e) vamos obter os valores ajustados:
  
y_estimado <- x%*%beta_chapeu
cbind(x%*%beta_chapeu,mod1$fitted.values)

H <- x%*%(solve(t(x)%*%x))%*%t(x)
H%*%y # outro jeito de achar os valores ajustados
cbind(H%*%y,x%*%beta_chapeu)

#f) vamos obter os residuos:

I <- diag(1,n) #matriz identidade

(I - H)%*%y # formula do residuo
cbind((I - H)%*%y, mod1$residuals)


#g) vamos obter o estimador de sigma2:

t(y)%*%y-t(beta_chapeu)%*%t(x)%*%y # = SQRes
sum(mod1$residuals^2) # = SQRes

sigma2_chapeu <- (t(y)%*%y-t(beta_chapeu)%*%t(x)%*%y)/(n-2) # = MSRes

#Exercício 2:

J <- matrix(1,n,n)

sum(y^2) 
#ou,
t(y)%*%y

sum(y)^2
#ou,
t(y)%*%J%*%y

#a) Defina SQT:

t(y)%*%y-(1/n)*t(y)%*%J%*%y # = SQT
sum((y-mean(y))^2)

#b) Defina SQRes:

t((I-H)%*%y)%*%(I-H)%*%y # = SQRes
#ou,
sum(mod1$residuals^2)

#c) Defina SQReg:

(t(y)%*%y-(1/n)*t(y)%*%J%*%y) - (t((I-H)%*%y)%*%(I-H)%*%y) # = SQT - SQRes = SQReg
#ou,
sum((y-mean(y))^2) - sum(mod1$residuals^2)