rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/tp4") 
library("haven"); library("plm"); library("dplyr"); library("pglm");
data <- read_dta('keane.dta')

#Solo hombres negros y creo la variable employ rezagada un periodo 
data <- data %>% 
  filter(black == 1) %>% 
  group_by(id) %>% 
  mutate(employ_lag = dplyr::lag(employ))

#Inciso A. 

pdata <- pdata.frame(data, index = c("id", "year")) #Equiv. a declarar el panel en Stata
probit <- pglm(employ ~ employ_lag, 
               family = binomial("probit"), 
               model = "pooling", 
               method = "bfgs",
               data = pdata)
summary(probit)

#Inciso B. 

#P[employ_it=1|employ_it-1=1] = pnorm(delta + rho*1)
p1 <- pnorm(probit$estimate[1]+probit$estimate[2])
#P[employ_it=1|employ_it-1=0] = pnorm(delta)
p2 <- pnorm(probit$estimate[1])

#Inciso C. 

probit2 <- pglm(employ ~ employ_lag +  y83 + y84 + y85 + y86 + y87, 
                family = binomial("probit"), 
                model = "pooling", 
                method = "bfgs",
                data = pdata)
summary(probit2)

#P[employ_i87=1|employ_it-1=1] = pnorm(delta + rho*1 + d87)
p3 <- pnorm(probit$estimate[1]+probit$estimate[2]+probit2$estimate[7])
#P[employ_it=1|employ_it-1=0] = pnorm(delta + rho*0 + d87)
p4 <- pnorm(probit$estimate[1]+probit2$estimate[7])

#Inciso D. 

data <- data %>% group_by(id) %>% mutate(employ_81 = sum(employ*y81))
pdata <- pdata.frame(data, index = c("id", "year")) # Defino el panel de nuevo
probit3 <- pglm(employ ~ employ_lag + employ_81 + y83 + y84 + y85 + y86 + y87, 
                family = binomial("probit"), 
                model = "random", 
                method = "bfgs",
                data = pdata)
summary(probit3)

#Inciso F. 

delta <- probit3$estimate[1] 
rho <- probit3$estimate[2]
eps_81 <- probit3$estimate[3]
gamma_87 <- probit3$estimate[8] #Coeficiente de dummy de tiempo 
sigma_de <- (summary(probit3)$estimate[9,1]) #idéntico al sigma_u que reporta STATA en Probit RE

data %>% ungroup() %>%
  filter(year == 87) %>% 
  mutate(step1 = delta + 
         rho + 
         eps_81*employ_81+
         gamma_87, 
         step2 = step1/sqrt(1+sigma_de^2), 
         pm = pnorm(step2)) %>% 
  summarise(p = mean(pm))

data %>% ungroup() %>%
  filter(year == 87) %>% 
  mutate(step1 = delta + 
           eps_81*employ_81+
           gamma_87, 
         step2 = step1/sqrt(1+sigma_de^2), 
         pm = pnorm(step2)) %>% 
  summarise(p = mean(pm))

