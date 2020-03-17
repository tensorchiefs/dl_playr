library(ggplot2)
library(ggpubr)
library(tidyr)
load('1D_Toy_sin.rdata')
dat = dat_sin

filter = function(df, x_filter, scale_factor) {
  r = df[abs(df$x - x_filter) < 0.1,]
  return (data.frame(x = r$x + scale_factor * r$p_y, y = r$y, method = r$method, x_base=r$x))
}

# Colors from https://rpkgs.datanovia.com/ggpubr/
cols <- c("mlt" = "#00AFBB", "dl_mlt" = 'blue', "gen" = "darkred")
ls = c("mlt" = "dotdash", "dl_mlt" = "solid", "gen" = "dashed")

ggplot(dat) + 
  geom_point(aes(x = x, y = y), col='grey') + 
  geom_path(data = filter(ret_models_sin, 0.0, 0.078), aes(x=x, y=y, col=method,linetype=method)) +  
  geom_vline(data = filter(ret_models_sin, 0.0, 0.078), aes(xintercept = x_base), alpha = 0.5)+
  geom_path(data = filter(ret_models_sin, 2.5, 0.18), aes(x=x, y=y, col=method,linetype=method,)) +  
  geom_vline(data = filter(ret_models_sin, 2.5, 0.18), aes(xintercept = x_base), alpha = 0.5)+
  geom_path(data = filter(ret_models_sin,  4.755, 0.13), aes(x=x, y=y, col=method,linetype=method)) +  
  geom_vline(data = filter(ret_models_sin, 4.765, 0.13), aes(xintercept = x_base), alpha = 0.5)+
  coord_cartesian(xlim=c(0,6.5)) +
  scale_color_manual(values=cols) +
  scale_linetype_manual(values=ls) + 
  theme_pubr()

ggsave('1D_Toy_sin.pdf', width = 6, height = 3.2)

load('1D_Toy_2l.rdata')
dat = dat_2l

ggplot(dat) + 
  geom_point(aes(x = x, y = y), col='grey') + 
  geom_path(data = filter(ret_models_2l, 0.0, 0.23), aes(x=x, y=y, col=method,linetype=method)) +  
  geom_vline(data = filter(ret_models_2l, 0.0, 0.23), aes(xintercept = x_base), alpha = 0.5)+
  geom_path(data = filter(ret_models_2l, 2.5, 0.4), aes(x=x, y=y, col=method,linetype=method)) +  
  geom_vline(data = filter(ret_models_2l, 2.5, 0.4), aes(xintercept = x_base), alpha = 0.5)+
  geom_path(data = filter(ret_models_2l,  5, 0.55), aes(x=x, y=y, col=method,linetype=method)) +  
  geom_vline(data = filter(ret_models_2l, 5, 0.55), aes(xintercept = x_base), alpha = 0.5)+
  coord_cartesian(xlim=c(0,6.5)) +
  scale_color_manual(values=cols) +
  scale_linetype_manual(values=ls) +
  theme_pubr(legend = 'top')

ggsave('1D_Toy_2l.pdf', width = 6, height = 3.2)
