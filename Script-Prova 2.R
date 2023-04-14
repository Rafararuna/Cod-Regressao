###############Avaliação 2####################

###Carregando Pacotes:

library(tidyverse)
library(lawstat)
library(lmtest)
library(car)
library(EnvStats)
library(olsrr)
library(nortest)
library(PMCMRplus)
library(ggplot2)
library(MASS)
library(leaps)
library(corrplot)
library(corrgram)

###Abrindo o banco de dados:

base.total <- read.table(file.choose(), header = F)
set.seed(180026798) #colocar o número da matr´icula
amostra <- sample(1:113,size = 55, replace = F)
#97  96  26  52  50  85 112  73   4  35  39   2  81  62
#13  84  28  34 113   5  94  93  58  24  31  80  88  66
#44  53  55  74  78  71  95  21  54  77  32  76  42  29
#41  47 102 110  36  86  10  46 104  57  38  12  48
base.treino <- base.total[amostra,]
base.valida <- base.total[-amostra,]

yt <- base.treino$V2
x1t <- base.treino$V3
x2t <- base.treino$V4
x3t <- base.treino$V5
x4t <- base.treino$V6
x5t <- base.treino$V7
x6t <- factor(base.treino$V8)
x7t <- factor(base.treino$V9)
x8t <- base.treino$V10
x9t <- base.treino$V11
x10t <- base.treino$V12
nt <- length(yt)

yv <- base.valida$V2
x1v <- base.valida$V3
x2v <- base.valida$V4
x3v <- base.valida$V5
x4v <- base.valida$V6
x5v <- base.valida$V7
x6v <- factor(base.valida$V8)
x7v <- factor(base.valida$V9)
x8v <- base.valida$V10
x9v <- base.valida$V11
x10v <- base.valida$V12
nv <- length(yv)

##########################################################

##a)

#análise descritiva da variavel - tempo de permanêcia:

ggplot(base.treino, aes(x=factor(""), y=yt)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Tempo de permaência")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_y.png", width = 158, height = 93, units = "mm")

summary(yt)
#Min.    1st Qu.  Median    Mean   3rd Qu.    Max.     Sd
#6.700   8.645    9.530     9.958  10.595     19.560   2.294788

#análise descritiva da variável Idade:

ggplot(base.treino, aes(x=factor(""), y=x1t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Idade (em anos)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_idade.png", width = 158, height = 93, units = "mm")

summary(x1t)
#Min.    1st Qu.  Median    Mean    3rd Qu.    Max.    SD
#42.00   51.00    53.70     53.40   56.25      65.90   4.591235

#analise descrititiva da variavel Risco de infecção:

ggplot(base.treino, aes(x=factor(""), y=x2t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Risco de Infecção (em %)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_riscoinfeccao.png", width = 158, height = 93, units = "mm")

summary(x2t)
#Min.    1st Qu.  Median    Mean    3rd Qu.    Max.   Sd
#1.300   4.100    4.600     4.664   5.550      7.800  1.389959

#analise descrititiva da variavel Razão de cultura de rotina:

ggplot(base.treino, aes(x=factor(""), y=x3t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Razão de cultura de rotina")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_razaoculturarotina.png", width = 158, height = 93, units = "mm")

summary(x3t)
#Min.   1st Qu.  Median    Mean    3rd Qu.    Max.   Sd
#2.20   10.35    14.90     16.93   20.90     52.40   10.68542

#analise descrititiva da variavel Raio X do tórax:

ggplot(base.treino, aes(x=factor(""), y=x4t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Raio X do tórax")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_RaioXtorax.png", width = 158, height = 93, units = "mm")

summary(x4t)
#Min.    1st Qu.  Median    Mean    3rd Qu.   Max.     Sd
#42.60   70.20    82.60     83.17   93.45     133.50   19.92232

#analise descrititiva da variavel Nº de camas:

ggplot(base.treino, aes(x=factor(""), y=x5t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Número de camas")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_ncamas.png", width = 158, height = 93, units = "mm")

summary(x5t)
#Min.    1st Qu.  Median    Mean    3rd Qu.   Max.   Sd
#29.0    98.5     195.0     269.1   337.5     835.0  207.0401

#analise descrititiva da variavel Afiliação Médica:

Fr<-table(x6t)
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by.x = "x6t", by.y = "Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=x6t, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#2171B5") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Afiliação Médica", y="Frequência") +
  ylim(0,75)+
  expand_limits(y = c(0,605)) +
  scale_x_discrete(labels=c("Sim", "Não")) +
  theme_bw() 
ggsave("barras_afiliacaomedica.png", width = 158, height = 93, units = "mm")

table(x6t)
#1  2 
#9 46 

#analise descrititiva da variavel Região:

Fr<-table(x7t)
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by.x = "x7t", by.y = "Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=x7t, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#2171B5") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Região", y="Frequência") +
  ylim(0,30)+
  expand_limits(y = c(0,50)) +
  scale_x_discrete(labels=c("NE", "NC","S","W")) +
  theme_bw() 
ggsave("barras_regiao.png", width = 158, height = 93, units = "mm")

table(x7t)
#1   2  3   4 
#17 16  14  8 

#analise descrititiva da variavel Média diária:

ggplot(base.treino, aes(x=factor(""), y=x8t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Média Diária")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_mediadiaria.png", width = 158, height = 93, units = "mm")

summary(x8t)
#Min.    1st Qu.  Median    Mean    3rd Qu.    Max.    Sd
#20.0    60.0     156.0     205.8   271.5      791.0   170.6668

#analise descrititiva da variavel Nº de enfermeiros:

ggplot(base.treino, aes(x=factor(""), y=x9t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Número de enfermeiros")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_nenfermeiros.png", width = 158, height = 93, units = "mm")

summary(x9t)
#Min.    1st Qu.  Median    Mean    3rd Qu.    Max.    Sd
#14.0    72.0     151.0     183.3   225.0      629.0   145.2202

#analise descrititiva da variavel Serviços disponiveis:

ggplot(base.treino, aes(x=factor(""), y=x10t)) +
  geom_boxplot(fill=c("#2171B5"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="serviços disponíveis")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("box_servdisp.png", width = 158, height = 93, units = "mm")

summary(x10t)
#Min.   1st Qu.  Median    Mean    3rd Qu.    Max.   Sd
#5.7    34.3     45.7      45.3    54.3       74.3   15.17411

###########################################################################

#a)

#Análise bidimensional:

cor(base.treino[,-c(1,8,9)])

plot(base.treino[,-c(1,8,9)],col=4)

M <- cor(base.treino[,-c(1,8,9)])
corrplot(M, method = "number")

corrgram(base.treino[,-c(1,8,9)], lower.panel = panel.pts, 
         upper.panel= panel.conf, diag.panel = panel.density, col=4)

hist(yt, xlab = "Y", ylab = "Frequência", main = "", col = 4)

###########################################################################

#b)

#montando o modelo treino sem seleção:

mod.treino <- lm(yt~x1t+x2t+x3t+x4t+x5t+x6t+x7t+x8t+x9t+x10t)

a <- summary(mod.treino)
xtable::xtable(a)

anova(mod.treino)


#Logo, o modelo ajustado seria: 
#yt = 1.117 + 0.085x1t + 0.369x2t + 0.016x3t + 0.020x4t +
#     - 0.006x5t + 0.218x6t2 - 0.738x7t2 - 1.201x7t3 - 1.352x7t4
#     + 0.0189x8t - 0.008x9t + 0.009x10t

#analise de diagnostico do modelo.treino:

par(mfrow=c(1,2))

plot(mod.treino$fitted.values,mod.treino$residuals,pch=16, 
     xlab = "Valores ajustados",
     ylab = "Resíduos", col = 4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod.treino$fitted.values,mod.treino$residuals)

plot(mod.treino$residuals,pch=16, xlab = "", ylab = "Resíduos", col = 4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod.treino$residuals)

par(mfrow=c(2,5))

plot(x1t,mod.treino$residuals,pch=16,xlab = "Idade", ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x1t,mod.treino$residuals) #13 44 49

plot(x2t,mod.treino$residuals,pch=16,xlab = "Risco de infecção", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x2t,mod.treino$residuals) #13 44 49

plot(x3t,mod.treino$residuals,pch=16,xlab = "Razão de cultura de rotina", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x3t,mod.treino$residuals) #13 44 49

plot(x4t,mod.treino$residuals,pch=16,xlab = "Raio X tórax",
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x4t,mod.treino$residuals) #13 44 49

plot(x5t,mod.treino$residuals,pch=16,xlab = "Número de camas", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x5t,mod.treino$residuals) #13 44 49

plot(x6t,mod.treino$residuals,pch=16,xlab = "Afiliação Médica", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x6t,mod.treino$residuals) #13 44 49

plot(x7t,mod.treino$residuals,pch=16,xlab = "Região", ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x7t,mod.treino$residuals) #13 44 49

plot(x8t,mod.treino$residuals,pch=16,xlab = "Média Diária", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x8t,mod.treino$residuals) #13 44 49

plot(x9t,mod.treino$residuals,pch=16,xlab = "Número de enfermeiros",
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x9t,mod.treino$residuals) #13 44 49

plot(x10t,mod.treino$residuals,pch=16,xlab = "Serviços Disponíveis", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(x10t,mod.treino$residuals) #13 44 49

#verificando normalidade:

par(mfrow=c(1,1))

hist(yt, xlab = "y", ylab = "Frequência", main = "", col = 4)

qqnorm(mod.treino$residuals,col=4,xlab = "Quantis Teóricos", 
       ylab = "Quantis Amostrais",
       main = "")
qqline(mod.treino$residuals,col=2)

ols_plot_resid_qq(mod.treino)

qqPlot(mod.treino$residuals)

shapiro.test(mod.treino$residuals)

#Correlação Serial dos erros:

dwtest(mod.treino) # teste de durbin watson

#homogeneidade da variancia:

bptest(mod.treino)

#verificando a necessidade de transformação:

boxcox(mod.treino,lambda = seq(-2,0,by=0.5), 
       ylab = "Log-Verossimilhança") #lambda = -1

#transformando a variavel:
lambda <- -1 # => fazer a inversa
y. <- 1/yt

#analise bidimensional:

dados. <- data.frame(cbind(x1t,x2t,x3t,x4t,x5t,x6t,x7t,x8t,x9t,x10t,y.))

c <- cor(dados.[,-c(6,7)])
xtable::xtable(c)
plot(dados.[,-c(6,7)],col=4)

M <- cor(dados.[,-c(6,7)])
corrplot(M, method = "number")

corrgram(dados.[,-c(6,7)], lower.panel = panel.pts,
         upper.panel= panel.conf, diag.panel = panel.density)

#criando o modelo completo transformado:
mod. <- lm(y.~x1t+x2t+x3t+x4t+x5t+x6t+x7t+x8t+x9t+x10t)
b <- summary(mod.)
xtable::xtable(b)

#fazendo a analise de diagnostico do modelo completo transformado:

par(mfrow=c(1,2))

plot(mod.$fitted.values,mod.$residuals,pch=16, col = 4, 
     xlab = "y tranformado ajustado (y.)", ylab = "Resíduos")
abline(h=0.00,col=2)
abline(h=-0.02,col=2)
abline(h=0.02,col=2)
identify(mod.$fitted.values,mod.$residuals)

plot(mod.$residuals,pch=16,col=4,ylab = "Resíduos", xlab = "")
abline(h=0,col=2)
abline(h=-0.02,col=2)
abline(h=0.02,col=2)
identify(mod.$residuals)

#verificando a normalidade:

hist(y., xlab = "y transformado (y.)", ylab = "Frequência", main = "", col = 4)

qqnorm(mod.$residuals,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(mod.$residuals,col=2)

ols_plot_resid_qq(mod.)

qqPlot(mod.$residuals)

shapiro.test(mod.$residuals)

#Correlação Serial dos erros:

dwtest(mod.) # teste de durbin watson

#homogeneidade da variancia:
bptest(mod.)


##########################################################################

#c) seleção de variaveis:

#pra ver qual o numero de melhor de variaveis pra entrar no modelo:
k <- ols_step_all_possible(mod.)
plot(k)

#ver quais os melhores modelos pra cada quantidade de variaveis:
sele1 <- regsubsets(y.~.,data=dados.,nbest = 10)
summary(sele1) 
names(summary(sele1))

cbind(summary(sele1)$which,summary(sele1)$rsq, summary(sele1)$adjr2,
      summary(sele1)$cp,summary(sele1)$bic)

#os modelos selecionados foram:
mod4 <- lm(y.~x2t+x7t+x8t+x9t)
mod5 <- lm(y.~x1t+x2t+x7t+x8t+x9t)
mod6 <- lm(y.~x1t+x2t+x4t+x7t+x8t+x9t)

#realizando metodo automáticos:

##Forward##

modmin <- lm(y.~1,data=dados.)
step(modmin,direction = 'forward',
     scope = ( ~ x1t+x2t+x3t+x4t+x5t+x6t+x7t+x8t+x9t+x10t))

#(Intercept)          x2t          x7t          x8t          x9t          x1t  
#1.547e-01        -5.351e-03    7.280e-03   -7.086e-05    4.661e-05   -6.653e-04 

#resultado do forward:
modfor <- lm(y.~x1t+x2t+x7t+x8t+x9t)
summary(modfor)

##Backward##

mod. <- lm(y.~x1t+x2t+x3t+x4t+x5t+x6t+x7t+x8t+x9t+x10t)
step(mod.,direction = 'backward')

#(Intercept)          x1t          x2t         x7t2         x7t3         x7t4          x8t          x9t  
#1.601e-01        -6.161e-04   -5.418e-03    7.214e-03    1.188e-02    2.407e-02   -6.861e-05    4.358e-05  

#resultado do backward:
modback <- lm(y.~x1t+x2t+x7t2+x7t3+x7t4+x8t+x9t)
summary(modback)

##Stepwise##

modmin <- lm(y.~1,data=dados.)
step(modmin,scope = list(lower = modmin, upper = mod.),direction = 'both')

#(Intercept)          x2t          x7t          x8t          x9t          x1t  
#1.547e-01   -5.351e-03    7.280e-03   -7.086e-05    4.661e-05   -6.653e-04  

#resultado do stepwise:
modstep <- lm(y.~x1t+x2t+x7t+x8t+x9t) # = mod4
summary(modstep)


#analise de diagnostico dos tres modelos:

#Mod4

mod4 <- lm(y.~x2t+x7t+x8t+x9t)

#residuo excluido studentizado

stud4 <- rstudent(mod4)

par(mfrow=c(1,2))

plot(mod4$fitted.values,stud4,pch=16, xlab = "y transformado ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod4$fitted.values,stud4)

plot(stud4,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud4)

#verificando normalidade:

hist(stud4, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4)

qqnorm(stud4,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud4,col=2)

ols_plot_resid_qq(mod4)

shapiro.test(mod4$residuals)

#independcia dos erros:

dwtest(mod4)

#homogeneidade da variancia:

bptest(mod4)

#Multicolinearidade:
g <- (vi4 <- vif(mod4))
xtable::xtable(g)
mean(vi4)

#VERIFICANDO OBSERVAÇÕES INFLUENTES:

#DFBETAS:
ols_plot_dfbetas(mod4) #mais ajeitado
#o dfbeta sao pontos que estao tendo uma influencia acima 
#da desejada em relação a estimação daquele parametro, daquele beta.


medinflu4 <- influence.measures(mod4)
indice <- c(1:nt)

#DFCOOK:
#no dcook, é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod4,which=4) 

ols_plot_cooksd_chart(mod4)



#Resíduo excluido - para calcular o PRESS:

rexc4 <- mod4$residuals/(1-medinflu4$infmat[,8])
plot(indice,rexc4)

PRESS4 <- sum((rexc4)^2)

rstandard(mod4,type="predictive")
#isso é o erro de previsão, o erro equivalente ao PRESS
sum((rstandard(mod4,type="predictive"))^2)


# Mod5

mod5 <- lm(y.~x1t+x2t+x7t+x8t+x9t)
h <- summary(mod5)
xtable::xtable(h)

#residuo excluido studentizado

stud5 <- rstudent(mod5)

par(mfrow=c(1,2))

plot(mod5$fitted.values,stud5,pch=16, xlab = "y transformado ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod5$fitted.values,stud5)

plot(stud5,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud5)

#verificando normalidade:

hist(stud5, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4)

qqnorm(stud5,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud5,col=2)

ols_plot_resid_qq(mod5)

shapiro.test(mod5$residuals)

#independencia dos erros:
dwtest(mod5)

#homogeneidade da variancia:

bptest(mod5)

#Multicolinearidade:
h <- (vi5 <- vif(mod5))
xtable::xtable(h)
mean(vi5)

#VERIFICANDO OBSERVAÇÕES INFLUENTES:

#DFBETAS:
ols_plot_dfbetas(mod5) #mais ajeitado
#o dfbeta sao pontos que estao tendo uma influencia acima 
#da desejada em relação a estimação daquele parametro, daquele beta


medinflu5 <- influence.measures(mod5)
indice <- c(1:n)

#DFCOOK:
#no dcook, é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod5,which=4) 

ols_plot_cooksd_chart(mod5)

#Resíduo excluido - para calcular o PRESS:

rexc5 <- mod5$residuals/(1-medinflu5$infmat[,8])

PRESS5 <- sum((rexc5)^2)

rstandard(mod5,type="predictive")
#isso é o erro de previsão, o erro equivalente ao PRESS
sum((rstandard(mod5,type="predictive"))^2) 


# Mod6

mod6 <- lm(y.~x1t+x2t+x4t+x7t+x8t+x9t)

#residuo excluido studentizado

stud6 <- rstudent(mod6)

par(mfrow=c(1,2))

plot(mod6$fitted.values,stud6,pch=16, xlab = "y transformado ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod6$fitted.values,stud6)

plot(stud6,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud6)

#verificando normalidade:

hist(stud6, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4)

qqnorm(stud6,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud6,col=2)

ols_plot_resid_qq(mod6)

shapiro.test(mod6$residuals)

#independecia dos erros:
dwtest(mod6)

#homogeneidade da variancia:

bptest(mod6)

#Multicolinearidade:
j <- (vi6 <- vif(mod6))
xtable::xtable(j)
mean(vi6)

#VERIFICANDO OBSERVAÇÕES INFLUENTES:

#DFBETAS:
ols_plot_dfbetas(mod6) #mais ajeitado
#o dfbeta sao pontos que estao tendo uma influencia acima 
#da desejada em relação a estimação daquele parametro, daquele beta


medinflu6 <- influence.measures(mod6)
indice <- c(1:n)

#DFCOOK:
#no dcook, é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod6,which=4) 

ols_plot_cooksd_chart(mod6)

#Resíduo excluido - para calcular o PRESS:

rexc6 <- mod6$residuals/(1-medinflu6$infmat[,8])

PRESS6 <- sum((rexc6)^2)

rstandard(mod6,type="predictive")
#isso é o erro de previsão, o erro equivalente ao PRESS
sum((rstandard(mod6,type="predictive"))^2) 

#pela analise de diagnostico e pelos metodos automaticos, o mod5 é o melhor.

############################################################

#d)

#Comparando o mod5(modelo treino) com o modelo de validação:

mod5 <- lm(y.~x1t+x2t+x7t+x8t+x9t)
summary(mod5)

medinflu5 <- influence.measures(mod5)
indice <- c(1:nt)
rexc5 <- mod5$residuals/(1-medinflu5$infmat[,8])
PRESS5 <- sum((rexc5)^2) # = 0.006812227

rstandard(mod5,type="predictive") 
sum((rstandard(mod5,type="predictive"))^2) # = 0.007895105

SQRes <- sum(mod5$residuals^2)
QMRes <- SQRes/(55-2) # = MSE

#montando modelo de validação:

yv <- base.valida$V2
yv. <- 1/yv
x1v <- base.valida$V3
x2v <- base.valida$V4
x3v <- base.valida$V5
x4v <- base.valida$V6
x5v <- base.valida$V7
x6v <- factor(base.valida$V8)
x7v <- factor(base.valida$V9)
x7v2 <- rep(0,nv)
x7v2[base.valida$V9 == 2] <- 1
x7v3 <- rep(0,nv)
x7v3[base.valida$V9 == 3] <- 1
x7v4 <- rep(0,nv)
x7v4[base.valida$V9 == 4] <- 1
x8v <- base.valida$V10
x9v <- base.valida$V11
x10v <- base.valida$V12
nv <- length(yv)

mod.valid <- lm(yv.~x1v+x2v+x7v+x8v+x9v)
v <- summary(mod.valid) #coeficientes de determinação
xtable::xtable(v)

#erro de previsão médio
medinfluv <- influence.measures(mod.valid)
indice <- c(1:nv)

rexcv <- mod.valid$residuals/(1-medinfluv$infmat[,8])

PRESSv <- sum((rexcv)^2) # = 0.008910494

rstandard(mod.valid,type="predictive")
#isso é o erro de previsão, o erro equivalente ao PRESS
sum((rstandard(mod.valid,type="predictive"))^2) # = 0.01116797

#erro quadrático médio
yv.pred5 = mod5$coefficients[1] +
  mod5$coefficients[2]*x1v +
  mod5$coefficients[3]*x2v +
  mod5$coefficients[4]*x7v2 +
  mod5$coefficients[5]*x7v3+
  mod5$coefficients[6]*x7v4 +
  mod5$coefficients[7]*x8v +
  mod5$coefficients[8]*x9v

MSPR = sum((yv.-yv.pred5)^2)/nv #medida de erro preditivo 

#####################################################################################

#e) e #f)

#montando a o modelo final:


yf <- base.total$V2
yf. <- 1/yf
x1f <- base.total$V3
x2f <- base.total$V4
x3f <- base.total$V5
x4f <- base.total$V6
x5f <- base.total$V7
x6f <- factor(base.total$V8)
x7f <- factor(base.total$V9)
x8f <- base.total$V10
x9f <- base.total$V11
x10f <- base.total$V12
nf <- length(yf)

mod.final <- lm(yf.~x1f+x2f+x7f+x8f+x9f)
r <- summary(mod.final)
xtable::xtable(r)

#analise de diagnostico:

#residuo excluido studentizado

studf <- rstudent(mod.final)

par(mfrow=c(1,2))

plot(mod.final$fitted.values,studf,pch=16, xlab = "y transformado ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod.final$fitted.values,studf)

plot(studf,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(studf)

#verificando normalidade:

hist(yf., xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4)

qqnorm(mod.final$residuals,col=4,xlab = "Quantis Teóricos", 
       ylab = "Quantis Amostrais",
       main = "")
qqline(mod.final$residuals,col=2)

ols_plot_resid_qq(mod.final)

shapiro.test(mod.final$residuals)

#independecia dos erros:
dwtest(mod.final)

#homogeneidade da variancia:

bptest(mod.final)

#Multicolinearidade:
j <- (viff <- vif(mod.final))
xtable::xtable(j)
mean(viff)

#VERIFICANDO OBSERVAÇÕES INFLUENTES:

#DFBETAS:
ols_plot_dfbetas(mod.final) #mais ajeitado
#o dfbeta sao pontos que estao tendo uma influencia acima 
#da desejada em relação a estimação daquele parametro, daquele beta


medinfluf <- influence.measures(mod.final)
indice <- c(1:n)

#DFCOOK:
#no dcook, é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod.final,which=4) 

ols_plot_cooksd_chart(mod.final)

#capacidade preditiva:

yf <- base.total$V2
yf. <- 1/yf
x1f <- base.total$V3
x2f <- base.total$V4
x3f <- base.total$V5
x4f <- base.total$V6
x5f <- base.total$V7
x6f <- factor(base.total$V8)
x7f <- factor(base.total$V9)
x7f2 <- rep(0,nf)
x7f2[base.total$V9 == 2] <- 1
x7f3 <- rep(0,nf)
x7f3[base.total$V9 == 3] <- 1
x7f4 <- rep(0,nf)
x7f4[base.total$V9 == 4] <- 1
x8f <- base.total$V10
x9f <- base.total$V11
x10f <- base.total$V12
nf <- length(yf)


yf.predf = mod5$coefficients[1] +
  mod5$coefficients[2]*x1f +
  mod5$coefficients[3]*x2f +
  mod5$coefficients[4]*x7f2 +
  mod5$coefficients[5]*x7f3+
  mod5$coefficients[6]*x7f4 +
  mod5$coefficients[7]*x8f +
  mod5$coefficients[8]*x9f

MSPRf = sum((yf.-yf.predf)^2)/nf #0.0001280926

#erro de previsão médio
medinfluf <- influence.measures(mod.final)
indice <- c(1:nf)

rexcf <- mod.final$residuals/(1-medinfluf$infmat[,8])

PRESSf <- sum((rexcf)^2) # = 0.008910494

rstandard(mod.final,type="predictive")
#isso é o erro de previsão, o erro equivalente ao PRESS
sum((rstandard(mod.final,type="predictive"))^2) # = 0.01667442