rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps2") 
library("haven"); library("fastDummies");library("plm")


data <- read_dta('cornwell.dta')


pOls <- plm(lcrmrte~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + 
              factor(year), 
            fixed = c("country", "year"), effect = "individual", model = "pooling", data = data)
summary(pOls)

#no hace falta crear las dummys :D


fe <- plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
            lpolpc + d82 + d83 + d84 + d85 + d86 + d87, 
          fixed = c("country", "year"), effect = "individual", model = "within", data = data)
summary(fe)


df <- plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
            lpolpc + d82 + d83 + d84 + d85 + d86 + d87, 
          fixed = c("country", "year"), effect = "individual", model = "fd", data = data)
summary(df)

# ---------------------------------------------------------------------------- #

data2 <- read_dta('murder.dta')

pOls_2 <- plm(mrdrte ~ exec + unem + d90 + d93, fixed = c("id", "year"), effect = "individual", model = "pooling", data = data2)
summary(pOls_2)
#stargazer(summary(pOls_2)$coef)

fe_2 <- plm(mrdrte ~ exec + unem + d90 + d93, fixed = c("id", "year"), effect = "individual", model = "within", data = data2)
summary(fe_2)

df_2 <- plm(mrdrte ~ exec + unem + d90 + d93, fixed = c("id", "year"), effect = "individual", model = "fd", data = data2)
summary(df_2)

re_2 <- plm(mrdrte ~ exec + unem + d90 + d93, fixed = c("id", "year"), effect = "individual", model = "random", data = data2)
summary(re_2)

#consistente vs eficiente 

phtest(fe_2, re_2, method = "aux") 
#Esto da igual que lo que vimos en clase, estoy feliz

?phtest




















