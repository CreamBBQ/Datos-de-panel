#------------------------------- COMENTARIOS --------------------------------#
#Contrario al tp anterior, este no tiene problemas de optimizacion y corre 
#relativamente rapido, sin embargo, dejo igualmente el link a una notebook con 
#todos los resultados cargados por si resulta mas comoda la correcion (excepto 
#por el ejercicio 1.F que tiene problemas en la notebook pero en el Rstudio
#funciona perfecto):
# ***  https://github.com/CreamBBQ/Datos-de-panel/blob/master/TP2/resultados.ipynb ****
#
#
#----------------------------------------------------------------------------#

rm(list = ls())
setwd("/home/creambbq/facu/Datos de panel/TP2") 
library("haven"); library("plm"); library("dplyr")
data <- read_dta('wagepan.dta')

#EJERCICIO 1.A: 

pOls <- plm(lwage~d81 + d82 + d83 + d84 + d85 + d86 + d87+educ+black+hisp+exper+expersq+married+union-1,  
            fixed = c("nr", "year"), effect = "individual", model = "pooling", data = data)
summary(pOls)
sqrt(diag(vcovHC(pOls, type = "HC3"))) #Correción de las matrices var-cov 

#EJERCICIO 1.B: 

re <- plm(lwage ~ d81 + d82 + d83 + d84 + d85 + d86 + d87 + educ + black + hisp + exper + expersq + married + union-1,  
          fixed = c("nr", "year"), effect = "individual", model = "random", data = data)
summary(re)

#EJERCICIO 1.C: 

fe <- plm(lwage ~ d81 + d82 + d83 + d84 + d85 + d86 + d87 + educ + black + hisp + exper + expersq + married + union -1,  
          fixed = c("nr", "year"), effect = "individual", model = "within", data = data)
summary(fe)

#EJERCICIO 1.D: 

fd <- plm(lwage ~ d81 + d82 + d83 + d84 + d85 + d86 + d87 + educ + black + hisp + exper + expersq + married + union -1,  
          fixed = c("nr", "year"), effect = "individual", model = "fd", data = data)
summary(fd)
#as.data.frame(summary(fd)$coef)["Estimate"] - as.data.frame(summary(fe)$coef)["Estimate"]

#EJERCICIO 1.E: 

edu <- plm(lwage ~ d81 + d82 + d83 + d84 + d85 + d86 + d87 + 
                  d81*educ + d82*educ + d83*educ + d84*educ + d85*educ + d86*educ + d87*educ +
                  south + nrthcen +  nrtheast +
                  educ + black + hisp + exper + expersq + married + union -1,  
                fixed = c("nr", "year"), effect = "individual", model = "within", data = data)
summary(edu)

#EJERCICIO 1.F: 

data2 <- data %>% group_by(nr) %>% mutate(union_adl = lead(union, order_by = year))
adl <- plm(lwage ~ d81 + d82 + d83 + d84 + d85 + d86 + d87 + educ + black + hisp + exper + expersq + married + union + union_adl -1,  
          fixed = c("nr", "year"), effect = "individual", model = "within", data = data2)
summary(adl)

#EJERCICIO 1.G: 

pOls_g1 <- plm(lwage ~  
                 union + educ + exper + expersq + hisp + black + rur + married + poorhlth + 
                 d81 + d82 + d83 + d84 + d85 + d86 + d87 +
                 #occ1 + occ2 + occ3 + occ4 + occ5 + occ6 + occ7 + occ8 + 
                 agric+ min+ construc+ trad+ tra+ fin+ bus+ per+ ent+ manuf+ pro+ pub +
                 nrthcen + nrtheast + south,  
               fixed = c("nr", "year"), effect = "individual", model = "pooling", data = data)
summary(pOls_g1)
sqrt(diag(vcovHC(pOls_g1, type = "HC3")))


pOls_g2 <- plm(lwage ~  
                union + educ + exper + expersq + hisp + black + rur + married + poorhlth + 
                d81 + d82 + d83 + d84 + d85 + d86 + d87 +
                occ1 + occ2 + occ3 + occ4 + occ5 + occ6 + occ7 + occ8 + 
                agric+ min+ construc+ trad+ tra+ fin+ bus+ per+ ent+ manuf+ pro+ pub +
                nrthcen + nrtheast + south,  
              fixed = c("nr", "year"), effect = "individual", model = "pooling", data = data)
summary(pOls_g2)
sqrt(diag(vcovHC(pOls_g2, type = "HC3")))


fe_g1 <- plm(lwage ~  
               union + educ + exper + expersq + hisp + black + rur + married + poorhlth + 
               #d81 + d82 + d83 + d84 + d85 + d86 + d87 +
               occ1 + occ2 + occ3 + occ4 + occ5 + occ6 + occ7 + occ8 + 
               agric+ min+ construc+ trad+ tra+ fin+ bus+ per+ ent+ manuf+ pro+ pub +
               nrthcen + nrtheast + south,  
             fixed = c("nr", "year"), effect = "individual", model = "within", data = data)
summary(fe_g1)


fe_g2 <- plm(lwage ~  
               union + educ + exper + expersq + hisp + black + rur + married + poorhlth + 
               #d81 + d82 + d83 + d84 + d85 + d86 + d87 +
               #occ1 + occ2 + occ3 + occ4 + occ5 + occ6 + occ7 + occ8 + 
               agric+ min+ construc+ trad+ tra+ fin+ bus+ per+ ent+ manuf+ pro+ pub +
               nrthcen + nrtheast + south,  
             fixed = c("nr", "year"), effect = "individual", model = "within", data = data)
summary(fe_g2)






