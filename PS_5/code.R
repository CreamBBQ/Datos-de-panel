rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps5") 
library("haven"); library("plm"); library("dplyr"); library("pglm");
library("sampleSelection"); library("lmtest")
data <- read_dta('wagepan.dta')

data <- data %>% group_by(nr) %>%  mutate(union_lag = dplyr::lag(union))

pdata <- pdata.frame(data, index = c("nr", "year"))

probit <- pglm(union ~ union_lag, 
               family = binomial("probit"), 
               model = "pooling", 
               method = "bfgs",
               data = pdata)
summary(probit)
p1 <- pnorm(probit$estimate[1]+probit$estimate[2])
#prob de que esté afiliado a un sindicato si lo estaba antes. 
p2 <- pnorm(probit$estimate[1])
#prob de que esté afiliado a un sindicato si no lo estaba antes.
#Los desvíos estándar de estas estimaciones de hacen usando el método delta. 
#https://github.com/benjaminguinaudeau/margins.pglm
#Otra opción es haciendo boostraping. 
#La variable unión no es continua así que el efecto marginal de calcula de la siguiente manera: 
#P[u_it = 1 | u_it-1 = 1] - P[u_it = 1 | u_it-1 = 0]
#a.k.a p1 - p2

probit2 <- pglm(union ~ union_lag +  d82 + d83 + d84 + d85 + d86 + d87, 
               family = binomial("probit"), 
               model = "pooling", 
               method = "bfgs",
               data = pdata)
summary(probit2)
#Siempre hay que dejar las pimeras dummys de tiempo sin agregar al módelo por un problema 
#de multicolinealidad perfecta.

#De esta forma la probabilidad de estar afiliado a un sindicato en el año 82 
#dado que estuvo afiliado en el año 81 es de: 
p3 <- pnorm(probit2$estimate[1]+probit2$estimate[2]+probit2$estimate[3])
#Y la probabilidad de estár afiliado a un sindicato en el año 82 dado que no 
#estuvo afiliado en el año 81 es de: 
p4 <- pnorm(probit2$estimate[1]+probit2$estimate[3])

#------------------------------------------------------------------------------#
#----------------- Modelo de efectos no observables dinámico-------------------#

data[, "union_oc"] <- NA

for(i in seq(1,nrow(data),8)){
  union_ochenta <- data[i, "union"]
  data[seq(i,i+7), "union_oc"] <- union_ochenta
}

pdata <- pdata.frame(data, index = c("nr", "year"))
probit3 <- pglm(union ~ union_lag + union_oc + d82 + d83 + d84 + d85 + d86 + d87, 
                family = binomial("probit"), 
                model = "random", 
                method = "bfgs",
                data = pdata)
summary(probit3)
#Ojo que acá se va a remportar un "sigma" que es el el desvío estándar que voy 
#a tener que usar en lo que sigue del ejercicio 


psi <- probit3$estimate[1]
rho <- probit3$estimate[2]
eps_80 <- probit3$estimate[3]
delta_87 <- probit3$estimate[9]
sigma_de <- (summary(probit3)$estimate[10,1])


data87 <- data %>% filter(year == 1987) %>% mutate(fe = psi + 
                                                     rho + 
                                                     eps_80*union_oc+
                                                     delta_87, 
                                                   se = fe/sqrt(1+sigma_de^2), 
                                                   pm = pnorm(se))

mean(data87$pm)
