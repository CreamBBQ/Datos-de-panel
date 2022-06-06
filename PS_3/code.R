rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps3") 
library("haven"); library("dplyr"); library("plm"); library("AER")

data <- read_dta('mod_abdata.dta')

#Ejercicio 1.A 

pOls <- plm(n ~ nL1 -1, 
            fixed = c("id", "year"), 
            effect = "individual",
            model = "pooling", 
            data = data)
summary(pOls)
#Problema: sobrestima el estimador ro. ver el sesgo en el libro que recomienda 
#Fiona. 

#Ejercicio 1.B

pdata <- pdata.frame(data, index = c("id", "year"))
fe <- plm(n ~ nL1 ,  
          fixed = c("id", "year"), 
          effect = "individual",
          model = "within", 
          data = pdata)
summary(fe)
#Problema: subestima porque si el ro es positivo el sesgo en negativo. Entonces 
#podemos ver como es más pequeño que en ols 

#Ejercicio 1.C

fd <- plm(n ~ nL1 ,  
          fixed = c("id", "year"), 
          effect = "individual",
          model = "fd", 
          data = pdata)
summary(fd)
#Problema: se sigue subestimando porque la esperanza entra la variable rezagada
#por el error presente es distinto de cero por construcción 


#Ejercicio 1.D 

data2 <- data %>% group_by(id) %>% mutate(dif_n = c(NA, diff(n)), 
                                           dif_nL1 = c(NA, diff(nL1)))
ivreg1 <- ivreg(dif_n ~ dif_nL1 -1 | nL2 -1, data=data2)
summary(ivreg1, diagnostics=TRUE)
#Se rompió todo, no nos puede dar un estimador cercano a 2

#Ejercicio 1.E


ab_1e <- pgmm(log(emp) ~ lag(log(emp), 1) | lag(log(emp), 2:99),
           data = pdata, 
           effect = "individual", 
           model = "onestep", 
           transformation = "d", 
           collapse = FALSE)
summary(ab_1e, robust = TRUE)

ab_2e <- pgmm(log(emp) ~ lag(log(emp), 1) | lag(log(emp), 2:99),
              data = pdata, 
              effect = "individual", 
              model = "twosteps", 
              transformation = "d", 
              collapse = FALSE)
summary(ab_2e, robust = TRUE)


#Ejercicio 1.F

bb_1e <- pgmm(log(emp) ~ lag(log(emp), 1) | lag(log(emp), 2:99),
           data = pdata, 
           effect = "individual", 
           model = "onestep", 
           transformation = "ld", 
           collapse = FALSE)
summary(bb_1e, robust = TRUE)

bb_2e <- pgmm(log(emp) ~ lag(log(emp), 1) | lag(log(emp), 2:99),
              data = pdata, 
              effect = "individual", 
              model = "twosteps", 
              transformation = "ld", 
              collapse = FALSE)
summary(bb_2e, robust = TRUE)

#---------------------------- EJERCICIO 2 -------------------------------------#
#La idea principal acá es ver como funcionan los estimadores cuando tengo más 
#variables explicativas a parte de los rezagos de la dependiente. 

rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps3") 
library("haven"); library("dplyr"); library("plm"); library("AER")

data <- read_dta('mod_abdata.dta')


#Ejercicio 2.A

pdata <- pdata.frame(data, index = c("id", "year"))
pOls <- plm(n ~ nL1 + nL2 + w + wL1 + k + kL1 + kL2 + ys + ysL1 + ysL2 + yr1977 + 
                yr1978 + yr1979 + yr1980 + yr1981 + yr1982 , 
            fixed = c("id", "year"), 
            effect = "individual",
            model = "pooling", 
            data = pdata)
summary(pOls, robust = TRUE)

#Ejercicio 2.B 

fe <- plm(n ~ nL1 + nL2 + w + wL1 + k + kL1 + kL2 + ys + ysL1 + ysL2 + yr1977 + 
              yr1978 + yr1979 + yr1980 + yr1981 + yr1982 ,  
          fixed = c("id", "year"), 
          effect = "individual",
          model = "within", 
          data = pdata)
summary(fe)

#Ejercicio 2.C 

dif <- function(x, na.rm = FALSE) (c(NA, diff(x)))
data2 <- data %>% group_by(id) %>% 
                  select(n, nL1, nL2, w, wL1, k, kL1, kL2, ys, ysL1, ysL2, yr1977, yr1978, yr1979, 
                         yr1980, yr1981, yr1982) %>% 
                  transmute_if(is.numeric, dif)
colnames(data2) <- paste("dif", colnames(data2), sep = "_")
data3 <- cbind(data, data2)


ivreg1 <- ivreg(dif_n ~ dif_nL1 + dif_nL2 + dif_w + dif_wL1 + dif_k + dif_kL1 + 
                dif_kL2 + dif_ys + dif_ysL1 + dif_ysL2 + dif_yr1977 + 
                dif_yr1978 + dif_yr1979 + dif_yr1980 + dif_yr1981 + dif_yr1982 -1| 
                . -dif_nL1 + nL2, 
                data=data3)
summary(ivreg1)

#Ejercicio 2.D 

pdata <- pdata.frame(data, index = c("id", "year"))

ab_1e <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
                lag(log(cap), 0:2) + lag(log(indoutpt), 0:2)| lag(log(emp), 2:99),
              data = pdata, 
              effect = "twoways", 
              model = "onestep", 
              transformation = "d", 
              collapse = FALSE)
summary(ab_1e, robust = TRUE, time.dummies = TRUE)

#Ejercicio 2.E 

ab_c <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
               lag(log(cap), 0:2) + lag(log(indoutpt), 0:2) | lag(log(emp), 2:99) +
               lag(log(wage), 2:99) + lag(log(cap), 2:99),
             data = pdata, effect = "twoways", model = "onestep", 
             transformation = "d")
summary(ab_c, robust = TRUE, time.dummies = TRUE)

ab_c2e <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
               lag(log(cap), 0:2) + lag(log(indoutpt), 0:2) | lag(log(emp), 2:99) +
               lag(log(wage), 2:99) + lag(log(cap), 2:99),
             data = pdata, effect = "twoways", model = "twosteps", 
             transformation = "d")
summary(ab_c2e, robust = TRUE, time.dummies = TRUE)

#Ejercicio 2.F 

z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
             lag(log(cap), 0:1) | lag(log(emp), 2:99) +
             lag(log(wage), 2:99) + lag(log(cap), 2:99),
           data = pdata, effect = "twoways", model = "twosteps", 
           transformation = "ld")
summary(z2, robust = TRUE, time.dummies = TRUE)
#cUIDADO: Los resultados de Stata se hacen sin la matriz robusta

#--------------------------- EJERCICIO 3 --------------------------------------#
#Este ejercicio es basicámente poner en los estimadores de pgmm un cambio en el 
#rango de diferencias post | p.e lag(log(emp), 2:5). Y, por otro lado, colapsar
#la matriz, esta opción ya la tiene incorporada pgmm con collapse = TRUE 

#--------------------------- EJERCICIO 4 --------------------------------------#
rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps3") 
library("haven"); library("dplyr"); library("plm"); library("AER"); library("foreign")

data <- read_dta('mod_abdata.dta')
pdata <- pdata.frame(data, index = c("id", "year"))

lsdv <-lm(n ~ nL1 - 1, data=pdata)
coef <- lsdv$coefficients 

N <- nrow(data %>% group_by(id) %>% summarise())
t <- nrow(data %>% group_by(year) %>% summarise())
NT <- N*t
k <- 1
#Cambiar la notación de Z 
Z <- data %>% group_by(id) %>% transmute(ddot_n = n - mean(n, na.rm = TRUE), 
                                         ddot_nL1 = nL1 - mean(nL1, na.rm = TRUE)) 
Z <- na.omit(Z)


u <- Z$ddot_n-Z$ddot_nL1*coef
SRC <- t(u)%*%u 
sigma2e <- SRC/(NT-N-t-k+1)
se <- sqrt(solve(t(Z$ddot_nL1)%*%Z$ddot_nL1)*sigma2e)
ttest <- coef/se
df = (NT-k)
pvalue <- pt(abs(ttest), df, lower.tail = FALSE, log.p = FALSE)*2