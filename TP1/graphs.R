rm(list = ls())
library("tidyverse")
library("ggthemes")


N <- c(5, 10, 30, 100, 200, 500)
tamaño_1p <- c(0.3428, 0.105, 0.0258, 0.013, 0.0106, 0.014)
tamaño_5p <- c(0.473, 0.2154, 0.0908, 0.0608, 0.0522, 0.0526)
poder_b0 <- c(0.3708, 0.2276, 0.3806, 0.9178, 0.9992, 1)
poder_b1 <- c(0.5676, 0.6368, 0.9846, 1, 1, 1)
se_b0 <- c(2.4007, 2.2241, 1.3556, 0.7436, 0.5257, 0.3321)
se_b1 <- c(0.1329, 0.1251, 0.0767, 0.0421, 0.0298, 0.0188)


df <- data.frame(N, tamaño_1p, tamaño_5p, poder_b0, poder_b1, se_b0, se_b1)


#-------------------------------------------------------------------------------
aux <- df %>% select(N, se_b0, se_b1) %>% 
  rename('DE \u03B20' = se_b0, 'DE \u03B21' = se_b1) %>% 
  gather(key = "variable", value = "value", -N)


ggplot(aux, aes(x = N, y = value)) + 
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("#004c69", "#72737e")) +
  theme_economist() +
  labs(
    title='Propiedades de muestra finita en FGSL: DE',
    x = '\nN',
    y = 'Desvio estándar\n')+
  theme(legend.title = element_blank()) 
ggsave("DP1.png", units="in", width=10, height=6, dpi=300)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
aux <- df %>% select(N, tamaño_1p, tamaño_5p) %>% 
  rename('Tamaño al 1%' = tamaño_1p, 'Tamaño al 5%' = tamaño_5p) %>% 
  gather(key = "variable", value = "value", -N)


ggplot(aux, aes(x = N, y = value)) + 
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("#004c69", "#72737e")) +
  theme_economist() +
  labs(
    title='Propiedades de muestra finita en FGSL: Tamaño del test (H0: \u03B21 = 0.8)',
    x = '\nN',
    y = 'Probabilidad\n')+
  theme(legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.05))
ggsave("DP2.png", units="in", width=10, height=6, dpi=300)
#--------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
aux <- df %>% select(N, poder_b0, poder_b1) %>% 
  rename('Poder H0: \u03B20 = 0' = poder_b0, 'Poder H0: \u03B21 = 0.4' = poder_b1) %>% 
  gather(key = "variable", value = "value", -N)


ggplot(aux, aes(x = N, y = value)) + 
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("#004c69", "#72737e")) +
  theme_economist() +
  labs(
    title='Propiedades de muestra finita en FGSL: Poder del test',
    x = '\nN',
    y = 'Probabilidad\n')+
  theme(legend.title = element_blank()) 
ggsave("DP3.png", units="in", width=10, height=6, dpi=300)
#--------------------------------------------------------------------------------

