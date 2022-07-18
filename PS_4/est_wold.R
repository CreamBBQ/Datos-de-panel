rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps4") 
library("haven"); library("plm"); library("dplyr"); library("pglm");
library("sampleSelection"); library("lmtest")


#Ejercicio 2.A Chamberlain (1980): 
#Mismos resultados de Stata, no tocar el código 

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

data <- data %>% mutate(pred = probit$estimate[1] + 
                          probit$estimate[2]*exper + 
                          probit$estimate[3]*educ + 
                          probit$estimate[4]*exper_81 + 
                          probit$estimate[5]*exper_82 + 
                          probit$estimate[6]*exper_83 + 
                          probit$estimate[7]*educ_81 + 
                          probit$estimate[8]*educ_82 + 
                          probit$estimate[9]*educ_83, 
                        lambda = dnorm(pred)/pnorm(pred), 
                        lambda_81 = y81*lambda, 
                        lambda_82 = y82*lambda,
                        lambda_83 = y83*lambda,
                        lambda_84 = y84*lambda,
                        lambda_85 = y85*lambda,
                        lambda_86 = y86*lambda,
                        lambda_87 = y87*lambda)
pdata <- pdata.frame(data, index = c("id", "year"))

pOls <- plm(lwage ~ exper +  educ + exper_81 + exper_82 + exper_83 +
              educ_81 + educ_82 + educ_83 + lambda_81 + lambda_82 + lambda_83 + 
              lambda_84 + lambda_85 + lambda_86 + lambda_87,  
            fixed = c("id", "year"), effect = "individual", model = "pooling", data = pdata)
summary(pOls)

#Ejercicio 2.A Mundlak (1978):
#Mismos resultados que stata, no tocar el código 

data <- read_dta('keane.dta')
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
                        lambda = dnorm(pred)/pnorm(pred), 
                        lambda_81 = y81*lambda, 
                        lambda_82 = y82*lambda,
                        lambda_83 = y83*lambda,
                        lambda_84 = y84*lambda,
                        lambda_85 = y85*lambda,
                        lambda_86 = y86*lambda,
                        lambda_87 = y87*lambda)

pdata <- pdata.frame(data, index = c("id", "year"))

pOls <- plm(lwage ~ exper + m_exper + educ + m_educ + 
              lambda_81 + lambda_82 + lambda_83 + 
              lambda_84 + lambda_85 + lambda_86 + lambda_87,  
            fixed = c("id", "year"), effect = "individual", model = "pooling", data = pdata)
summary(pOls)



