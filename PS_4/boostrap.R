rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps4") 
library("haven"); library("plm"); library("dplyr"); library("pglm");
library("sampleSelection"); library("lmtest"); library("boot")

data <- read_dta('keane.dta')
################### Buenisimo para entender como funciona ######################
set.seed(1)
x = rnorm(50, mean=10, sd=2) 
xx <- data.frame(x)
function_1 <- function(data, i){
  print(i)
  print("-----------------------------------------\n\n")
  d2 <- data[i,] 
  mean(d2)
}
ejemplo <- boot(xx,function_1,R=10)
ejemplo
################################################################################
#Creo que el problema está en querer almacenar todo en un mismo df, hay que 
#construir uno para cada resultado. 

test <- data[1:1000, ]
test <- test %>% group_by(id) %>% mutate(m_exper = mean(exper), 
                                         m_educ = mean(educ), 
                                         pred = NA, 
                                         lambda = NA, 
                                         lambda_81 = NA, 
                                         lambda_82 = NA,
                                         lambda_83 = NA,
                                         lambda_84 = NA,
                                         lambda_85 = NA,
                                         lambda_86 = NA,
                                         lambda_87 = NA)
test <- test %>% mutate(lambda = 1)

get_probit <- function(datos, i){
  
  sample <- datos[i, ]
  pdata <- pdata.frame(sample, index = c("id", "year"))
  probit <- pglm(obswage ~ exper + m_exper + educ + m_educ, 
                 family = binomial("probit"), 
                 model = "pooling", 
                 method = "bfgs",
                 data = pdata)
  print("hola")
  sample <- sample %>% mutate(pred = probit$estimate[1] + 
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
  
  pdata <- pdata.frame(sample, index = c("id", "year"))
  
  pOls <- plm(lwage ~ exper + m_exper + educ + m_educ + 
                lambda_81 + lambda_82 + lambda_83 + 
                lambda_84 + lambda_85 + lambda_86 + lambda_87,  
              fixed = c("id", "year"), effect = "individual", model = "pooling", data = pdata)
  
  print(pOls$coefficients[1])
  return(pOls$coefficients[1])
  
}

first <- boot(test,get_probit,R=100)
