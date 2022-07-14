rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/Econometría/ps4") 
library("haven"); library("plm"); library("dplyr"); library("pglm");
library("sampleSelection"); library("lmtest"); library("boot"); library("EnvStats")
set.seed(13)

get_data <- function(N, TT, model) {
  df <- df <- setNames(data.frame(matrix(0, ncol = 10, nrow = N*TT)), 
                       c("j", "t", "x", "z", "eps", "u", 
                         "psi_1", "psi_2", "psi_3", "psi_4"))
  aux <- 1
  for(j in 1:N){
    psi_2 <- rnorm(1,0,1)
    psi_3 <- rnorm(1,0,1)
    psi_4 <- rnorm(1,0,1)
    for(t in 1:TT){
      x <- rnorm(1,0,1)
      z <- rnorm(1,0,1)
      eps <- rnorm(1,0,1)
      psi_1 <- rnorm(1,0,1)
      u <- 0.6*eps + 0.8*psi_1
      df[aux, ] <- c(j, t, x, z, eps, u, psi_1, psi_2, psi_3, psi_4)
      aux <- aux + 1
    }
  }
  if (model == "A"){
    df <- df %>% mutate(alpha = psi_2 + psi_4, 
                        c = psi_3 + psi_4, 
                        s = case_when(x + z + alpha + eps > 0 ~ 1,
                                      TRUE ~ 0),
                        y = case_when(s == 1 ~ x + c + u,
                                      TRUE ~ NA_real_))
  } else if (model == "B"){
    df <- df %>% group_by(j) %>% mutate(alpha = psi_2 + sum(z)/2, 
                                        c = psi_3 + sum(x)/2) %>% 
      ungroup()
    df <- df %>% mutate(s = case_when(x + z + alpha + eps > 0 ~ 1,
                                      TRUE ~ 0),
                        y = case_when(s == 1 ~ x + c + u,
                                      TRUE ~ NA_real_))
  } else if (model == "C") { 
    df <- df %>% group_by(j) %>% mutate(alpha = psi_2 + sum(z)/2 + psi_4, 
                                        c = psi_3 + sum(x)/2 + psi_4) %>% 
      ungroup()
    df <- df %>% mutate(s = case_when(x + z + alpha + eps > 0 ~ 1,
                                      TRUE ~ 0),
                        y = case_when(s == 1 ~ x + c + u,
                                      TRUE ~ NA_real_))
    }
  df <- df[, c("j", "t", "y", "x", "s", "z", "alpha", "c")]
  return(df)
}
get_wooldridge <- function(df, boots){
  df <- df %>% group_by(j) %>% mutate(mean_x = mean(x), 
                                      mean_z = mean(z), 
                                      t1 = case_when(t == 1 ~ 1,
                                                     TRUE ~ 0), 
                                      t2 = case_when(t == 2 ~ 1, 
                                                     TRUE ~ 0)) %>% 
    ungroup()
  pdata <- pdata.frame(df, index = c("j", "t"))
  probit <- pglm(s ~ x + mean_x + z + mean_z -1, 
                 family = binomial("probit"), 
                 model = "pooling", 
                 method = "bfgs",
                 data = pdata)
  df <- df %>% mutate(pred = probit$estimate[1]*x + 
                        probit$estimate[2]*mean_x + 
                        probit$estimate[3]*z + 
                        probit$estimate[4]*mean_z, 
                      lambda = dnorm(pred)/pnorm(pred), 
                      lambda_1 = t1*lambda,
                      lambda_2 = t2*lambda)
  pdata <- pdata.frame(df, index = c("j", "t"))
  pOls <- plm(y ~ x + mean_x + lambda_1 + lambda_2 -1,
              fixed = c("j", "t"), 
              effect = "individual", 
              model = "pooling", 
              data = pdata)
  if (boots){
    return(c(pOls$coefficients[1], probit$estimate[1], probit$estimate[3],(df$s-df$pred)))
  } else {
    return(c(pOls$coefficients[1], probit$estimate[1], probit$estimate[3]))
  }
}
montecarlo <- function(S) {
  models <- c("A", "B", "C")
  Ns <- c(20,40,100)
  df <- df <- setNames(data.frame(matrix(0, ncol = 6, nrow = length(Ns)*S)),
                       c("N", "T", "model", "beta", "gamma_1", "gamma_2"))
  aux <- 1
  for(s in 1:S) {
    for(N in Ns){
      for(model in models){
        data <- get_data(N, 2, model)
        df[aux, ] <- c(N, 2, model, get_wooldridge(data, boots = FALSE))
        aux <- aux + 1
      }
    }
  }
  resultados <- df %>% mutate(across(!model, as.numeric)) %>% 
    group_by(N, model) %>% 
    summarise(sesgo_medio_beta = mean(beta) - 1, 
              sesgo_medio_gamma1 = mean(gamma_1) - 1, 
              sesgo_medio_gamma2 = mean(gamma_2) - 1, 
              sesgo_mediano_beta = median(beta) - 1,
              sesgo_mediano_gamma1 = median(gamma_1) - 1,
              sesgo_mediano_gamma2 = median(gamma_2) - 1,
              desvio_beta = sqrt((sum(beta - mean(beta))^2)/S), 
              desvio_gamma1 = sqrt((sum(gamma_1 - mean(gamma_1))^2)/S),
              desvio_gamma2 = sqrt((sum(gamma_2 - mean(gamma_2))^2)/S), 
              rmse_beta = sqrt(((sum(mean(beta)-1))^2)/S), 
              rmse_gamma_1 = sqrt(((sum(mean(gamma_1)-1))^2)/S), 
              rmse_gamma_2= sqrt(((sum(mean(gamma_2)-1))^2)/S), 
              desvio_medio_abs_beta = abs(sum(beta-mean(beta)))/S)
  return(resultados)
}
get_bt_iteration <- function(N, TT, model){
  df <- get_data(N, TT, model)
  wold <- get_wooldridge(df, boots = TRUE)
  beta_hat <- wold[1]
  gamma1_hat <- wold[2]
  gamma2_hat <- wold[3]
  eps_hat <- wold[4:(N*TT+3)]
  muestra <- eps_hat[sample.int(N*TT, N*TT, replace = TRUE)]
  df["eps_hat"] <- muestra
  df <- df %>% mutate(s = case_when(gamma1_hat*x + gamma2_hat*z + alpha + eps_hat > 0 ~ 1,
                                    TRUE ~ 0),
                      y = case_when(s == 1 ~ beta_hat*x + c,
                                    TRUE ~ NA_real_))
  return(get_wooldridge(df, boots = FALSE))
}
bootstrap <- function(B){
  models <- c("A", "B", "C")
  Ns <- c(20,40,100)
  res <- setNames(data.frame(matrix(0, ncol = 6, nrow = B*length(Ns))), 
                       c("N", "T", "model", "beta", "gamma1", "gamma2"))
  aux <- 1
  for (b in 1:B){
    print(b)
    for (N in Ns){
      for(model in models){
        res[aux, ] <- c(N, 10, model, get_bt_iteration(N, 10, model))
        aux <- aux + 1
      }
    }
  }
  res <- res %>% mutate(across(!model, as.numeric)) %>% 
    group_by(N, model) %>% 
    summarise(beta = paste(t.test(beta, conf.level = 0.95)$conf.int[1], 
                           t.test(beta, conf.level = 0.95)$conf.int[2], 
                                                    sep = " - "), 
              gamma1 = paste(t.test(gamma1, conf.level = 0.95)$conf.int[1], 
                             t.test(gamma1, conf.level = 0.95)$conf.int[2], 
                             sep = " - "), 
              gamma2 = paste(t.test(gamma2, conf.level = 0.95)$conf.int[1], 
                             t.test(gamma2, conf.level = 0.95)$conf.int[2], 
                             sep = " - "))
  return(res)
}

boot <- bootstrap(B = 5)
monte <- montecarlo(S = 5)

