# ¡Hola! Se pueden consultar los resultados sin ejecutar el codigo visitando 
# la siguiente notebook: 
#       
#   **https://github.com/CreamBBQ/Datos-de-panel/blob/master/TP3/code.ipynb**
#
# La notacion TT hace referencia al periodo de tiempo (T en el tp), lo cambie 
# porque T es una palabra reservada en R y traia algunos problemas en la ejecucion


rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/tp3") 
library("dplyr"); library("plm"); library("AER") 
set.seed(1331)

get_data <- function(N, TT, alpha, beta){
  
  df <- setNames(data.frame(matrix(0, ncol = 4, nrow = N*(TT+10))), c("j", "t", "y", "x"))
  aux <- 1
  
  for (j in seq(1:N)){
    
    start <- j
    y <- 0 
    x <- rnorm(1,0,0.9)
    
    for(t in seq(1:(TT+10))){
      
      c <- rnorm(1,0,1)
      u <- rnorm(1,0,1)
      v <- rnorm(1,0,0.9)
      x <- (0.8)*x + v 
      y <- (alpha)*y + (beta)*x + c + u
      df[aux, ] <- c(j, t, y, x)
      aux <- aux + 1
      
    }
    
  }
  
  df <- df %>% group_by(j) %>% 
    mutate(yL1 = dplyr::lag(y), 
           yL2 = dplyr::lag(yL1),
           xL1 = dplyr::lag(x), 
           xL2 = dplyr::lag(xL1),
           dif_y = c(NA, diff(y)), 
           dif_yL1 = c(NA, diff(yL1)), 
           dif_x = c(NA, diff(x)), 
           dif_xL1 = c(NA, diff(xL1))) %>% 
    do(tail(., n = 10))
  
  return(df)
  
}

get_LSDV <- function(data, N, TT, alpha) {
  
  df <- setNames(data.frame(matrix(0, ncol = 7, nrow = 1)),
                 c("estimador", "alpha", "st_alpha",
                   "beta", "st_beta", "RMSE", "tamano"))
  
  LSDV <- plm(y ~ yL1 + x  -1, 
              data = data, 
              method = "within", 
              effect = "individual")
  
  pred <- data$yL1*(summary(LSDV)$coefficients[1,1]) +
    data$x*(summary(LSDV)$coefficients[2,1])
  
  t <- (summary(LSDV)$coefficients[1,1] - alpha)/summary(LSDV)$coefficients[1,2]
  if ((pt(abs(t), (N*TT-2), lower.tail = FALSE, log.p = FALSE)*2) < 0.05){
    tamano_LSDV <- 1
  } else { 
    tamano_LSDV <- 0
  }
  
  
  df[1, ] <- c("LSDV", 
               summary(LSDV)$coefficients[1,1],
               summary(LSDV)$coefficients[1,2],
               summary(LSDV)$coefficients[2,1],
               summary(LSDV)$coefficients[2,2],
               sqrt(mean((data$y - pred)^2)), 
               tamano_LSDV)
  return(df)
  
}

get_AB1 <- function(data, N, TT, alpha) { 

  
  pdata <- pdata.frame(data, index = c("j", "t"))
  
  df <- setNames(data.frame(matrix(0, ncol = 7, nrow = 1)),
                 c("estimador", "alpha", "st_alpha",
                   "beta", "st_beta", "RMSE", "tamano"))
  
  AB1 <- pgmm(y ~ lag(y, 1) + x |
                lag(y, 2:99),
              data = pdata,
              effect = "individual",
              model = "onestep",
              transformation = "d",
              collapse = FALSE)
  
  pred <- data$yL1*(summary(AB1)$coefficients[1,1]) +
    data$x*(summary(AB1)$coefficients[2,1])
  
  t <- (summary(AB1)$coefficients[1,1] - alpha)/summary(AB1)$coefficients[1,2]
  if ((pt(abs(t), (N*TT-2), lower.tail = FALSE, log.p = FALSE)*2) < 0.05){
    tamano_AB1 <- 1
  } else { 
    tamano_AB1 <- 0
  }

  
  df[1, ] <- c("AB1", 
               summary(AB1)$coefficients[1,1],
               summary(AB1)$coefficients[1,2],
               summary(AB1)$coefficients[2,1],
               summary(AB1)$coefficients[2,2],
               sqrt(mean((data$y - pred)^2)), 
               tamano_AB1)
  return(df)
  
}

get_AB2 <- function(data, N, TT, alpha) { 
  
  
  pdata <- pdata.frame(data, index = c("j", "t"))
  
  df <- setNames(data.frame(matrix(0, ncol = 7, nrow = 1)),
                 c("estimador", "alpha", "st_alpha",
                   "beta", "st_beta", "RMSE", "tamano"))
  
  AB2 <- pgmm(y ~ lag(y, 1) + x |
                lag(y, 2:99),
              data = pdata,
              effect = "individual",
              model = "twosteps",
              transformation = "d",
              collapse = FALSE)
  
  pred <- data$yL1*(summary(AB2)$coefficients[1,1]) +
    data$x*(summary(AB2)$coefficients[2,1])
  
  t <- (summary(AB2)$coefficients[1,1] - alpha)/summary(AB2)$coefficients[1,2]
  if ((pt(abs(t), (N*TT-2), lower.tail = FALSE, log.p = FALSE)*2) < 0.05){
    tamano_AB2 <- 1
  } else { 
    tamano_AB2 <- 0
  }
  
  df[1, ] <- c("AB2", 
               summary(AB2)$coefficients[1,1],
               summary(AB2)$coefficients[1,2],
               summary(AB2)$coefficients[2,1],
               summary(AB2)$coefficients[2,2],
               sqrt(mean((data$y - pred)^2)), 
               tamano_AB2)
  return(df)
  
}

get_BB1 <- function(data, N, TT, alpha) {
  
  pdata <- pdata.frame(data, index = c("j", "t"))
  df <- setNames(data.frame(matrix(0, ncol = 7, nrow = 1)), 
                 c("estimador", "alpha", "st_alpha",
                   "beta", "st_beta", "RMSE", "tamano"))
  
  BB1 <- pgmm(y ~ lag(y, 1) + x | 
                lag(y, 2:99) + lag(x, 1:99),
              data = pdata, 
              effect = "individual", 
              model = "onestep", 
              transformation = "ld",
              collapse = FALSE)
  
  pred <- data$yL1*(summary(BB1)$coefficients[1,1]) + 
    data$x*(summary(BB1)$coefficients[2,1])
  
  t <- (summary(BB1)$coefficients[1,1] - alpha)/summary(BB1)$coefficients[1,2]
  if ((pt(abs(t), (N*TT-2), lower.tail = FALSE, log.p = FALSE)*2) < 0.05){
    tamano_BB1 <- 1
  } else { 
    tamano_BB1 <- 0
  }
  

  df[1, ] <- c("BB1", 
               summary(BB1)$coefficients[1,1],
               summary(BB1)$coefficients[1,2],
               summary(BB1)$coefficients[2,1],
               summary(BB1)$coefficients[2,2],
               sqrt(mean((data$y - pred)^2)), 
               tamano_BB1)
  return(df)
  
}

get_AH <- function(data, N, TT, alpha) {
  
  tamano_AH <- 0
  
  df <- setNames(data.frame(matrix(0, ncol = 7, nrow = 1)), 
                 c("estimador", "alpha", "st_alpha",
                   "beta", "st_beta", "RMSE", "tamano"))
  
  pdata <- pdata.frame(data, index = c("j", "t"))
  
  AH <- ivreg(dif_y ~ dif_yL1 + dif_x - 1 | . - dif_yL1 + yL2, 
              data = pdata)
  
  t <- (summary(AH)$coefficients[1,1] - alpha)/summary(AH)$coefficients[1,2]
  if ((pt(abs(t), (N*TT-2), lower.tail = FALSE, log.p = FALSE)*2) < 0.05){
    tamano_AH <- 1
  } else { 
    tamano_AH <- 0
  }
  
  df[1, ] <- c("AH",
               summary(AH)$coefficients[1,1],
               summary(AH)$coefficients[1,2],
               summary(AH)$coefficients[2,1],
               summary(AH)$coefficients[2,2],
               sqrt(mean((data$y - predict(AH))^2)), 
               tamano_AH)
  return(df)
  
}

get_kiviet <- function(data, N, TT, alpha){
  
  tamano_kiviet <- 0
  df <- setNames(data.frame(matrix(0, ncol = 7, nrow = 1)), 
                 c("estimador", "alpha", "st_alpha",
                   "beta", "st_beta", "RMSE", "tamano"))
  pdata <- pdata.frame(data, index = c("j", "t"))
  kiviet <-lm(y ~ yL1 + x - 1, data=pdata)
  alpha_hat <- kiviet$coefficients[1]
  beta_hat <- kiviet$coefficients[2]
  N <- nrow(data %>% group_by(j) %>% summarise())
  t <- nrow(data %>% group_by(t) %>% summarise())
  NT <- N*t
  k <- 2
  
  within_data <- data %>% group_by(j) %>%
    transmute(ddot_y = y - mean(y, na.rm = TRUE), 
              ddot_yL1 = yL1 - mean(yL1, na.rm = TRUE), 
              ddot_x = x - mean(x, na.rm = TRUE)) %>% 
    do(na.omit(.))
  Z <- append(as.vector(within_data$ddot_yL1), 
              as.vector(within_data$ddot_x))
  u_hat <- within_data$ddot_y - 
    within_data$ddot_yL1 * alpha_hat - 
    within_data$ddot_x * beta_hat
  SRC <- t(u_hat)%*%u_hat
  sigma2e <- SRC/(NT-N-t-k+1)
  se_a <- sqrt(solve(t(Z)%*%Z)*sigma2e)
  se_b <- sqrt(diag(vcov(kiviet)))[2]
  
  t <- as.numeric((alpha_hat-alpha)/se_a)
  if((pt(abs(t), (NT-k), lower.tail = FALSE, log.p = FALSE)*2) < 0.05){
    tamano_kiviet <- tamano_kiviet + 1
  }
  
  pred <- data$y*alpha_hat + data$x*beta_hat
  
  df[1, ] <- c("kiviet", 
               alpha_hat, 
               se_a, 
               beta_hat, 
               se_b, 
               sqrt(mean((data$y - pred)^2)), 
               tamano_kiviet)
  return(df)
} 

montecarlo <- function(simulations, N, TT, alpha, beta){
  
  temp <- list()
  
  for (n in seq(1:simulations)) {
    
    print(n)
    data <- get_data(N, TT, alpha, beta)
    df <- rbind(get_LSDV(data, N, TT, alpha), 
                get_AB1(data, N, TT, alpha), 
                get_AB2(data, N, TT, alpha),
                get_BB1(data, N, TT, alpha), 
                get_AH(data, N, TT, alpha), 
                get_kiviet(data, N, TT, alpha))
    temp[[n]] <- df 
    
  }
  
  res <- dplyr::bind_rows(temp)
  res <- res %>% mutate(across(!estimador, as.numeric)) %>%  
    group_by(estimador) %>% summarise(alpha_hat = mean(alpha), 
                                      st_alpha = mean(st_alpha), 
                                      beta_hat = mean(beta), 
                                      st_beta = mean(st_beta), 
                                      RMSE = mean(RMSE), 
                                      tamano = mean(tamano))
  
  return(res)

}

EJ1 <- montecarlo(1000, 30, 10, 0.5, 0)
EJ2 <- montecarlo(1000, 50, 10, 0.5, 0)
EJ3 <- montecarlo(1000, 30, 20, 0.8, 0)
EJ4 <- montecarlo(1000, 30, 20, 0.92, 0)
EJ5 <- montecarlo(1000, 50, 30, 0.5, 0)
EJ6 <- montecarlo(1000, 100, 7, 0.8, 1)
EJ7 <- montecarlo(1000, 100, 4, 0.8, 0)