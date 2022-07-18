rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps4") 
library("haven"); library("plm"); library("dplyr"); library("pglm");
library("sampleSelection"); library("lmtest")
data <- read_dta('keane.dta')


table(obs_wage = data$obswage, choice_labour = data$choice)


#Ejercicio 1.A
fe <- plm(lwage ~ exper + educ,  
          fixed = c("id", "year"), 
          effect = "individual", 
          model = "within", data = data)
#summary(fe, vcov=vcovHC(fe, method="white1", type="HC1"))
summary(fe) 


#Ejercicio 1.B Mundlak (1978): 
#Todo esto coincide con stata, no tocar el código. 

data <- data %>% group_by(id) %>% mutate(m_exper = mean(exper), 
                                         m_educ = mean(educ))
pdata <- pdata.frame(data, index = c("id", "year"))

probit <- pglm(obswage ~ exper + m_exper + educ + m_educ, 
               family = binomial("probit"), 
               model = "pooling", 
               method = "bfgs",
               data = pdata)

data <- data %>% mutate(pred = probit$estimate[1] + 
                          probit$estimate[2]*exper + 
                          probit$estimate[3]*m_exper + 
                          probit$estimate[4]*educ + 
                          probit$estimate[5]*m_educ, 
                        lambda = dnorm(pred)/pnorm(pred))

test <- plm(lwage ~ exper + educ + lambda,  
          fixed = c("id", "year"), 
          effect = "individual", 
          model = "within", data = data)
coeftest(test, vcov.=function(x) vcovHC(x, type="sss"))

#Ejercicio 1.C Chamberlain (1980):
#Todo coincide con stata no tocar el código 

data <- read_dta('keane.dta')
data <- data %>% group_by(id) %>% mutate(exper_81 = sum(exper * y81), 
                                         exper_82 = sum(exper * y82), 
                                         exper_83 = sum(exper * y83), 
                                         educ_81 = sum(educ * y81), 
                                         educ_82 = sum(educ * y82), 
                                         educ_83 = sum(educ * y83))

pdata <- pdata.frame(data, index = c("id", "year"))
probit <- pglm(obswage ~ exper +  educ + exper_81 + exper_82 + exper_83 +
                    educ_81 + educ_82 + educ_83, 
                  family = binomial("probit"), 
                  model = "pooling", 
                  method = "bfgs",
                  data = pdata)
summary(probit)



test <- plm(lwage ~ exper + educ + lambda,  
            fixed = c("id", "year"), 
            effect = "individual", 
            model = "within", data = data)
coeftest(test, vcov.=function(x) vcovHC(x, type="sss"))





