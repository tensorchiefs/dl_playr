if (FALSE){
  #hist_1 = history
  hist_select = "/Users/oli/Dropbox/__ZHAW/__Projekte_Post_ZHAH/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/2020-03-0627.291_test_run/history_12000.Rdata"
  path_result = "/Users/oli/Dropbox/__ZHAW/__Projekte_Post_ZHAH/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/2020-03-0627.291_test_run/"
}

load(hist_select)
library(ggplot2)

history = hist_grid
str(history)
summary(history)
  


library(tidyr)
h = gather(history, 'sample', 'loss', nll_train:nll_test)
h$sample = as.factor(h$sample)  # test or validation/test

summary(h)
str(h)


  # i=1
  # j=2
  # k=3
  h1 = h
  
  x_scale_pick = levels(h1$x_scale)[i]
  h1= h1[h1$x_scale==x_scale_pick,]

  spatz_pick = levels(h1$spatz)[j]
  h1= h1[h1$spatz==spatz_pick,]
  
  regularization_pick = levels(h1$regularization)[k]
  h1= h1[h1$regularization==regularization_pick,]
  
  main = h1$method[1] 
  

  p = ggplot(data=h1, aes(x = step, y = loss, color=fold)) + 
    geom_line(lwd=1.5) +
    geom_hline(yintercept=2.48) +
    ylim(2,3) +
    ggtitle(main) +
    facet_wrap(. ~ sample, ncol=2) +
    theme_bw() + 
    theme(plot.title = element_text(size=28),
          plot.subtitle = element_text(size=26),
          axis.text = element_text( size = 20 ),
          axis.text.x = element_text( size = 24 ),
          axis.title = element_text( size = 26, face = "bold" ),
          strip.text.x = element_text(size = 26, face = "bold"),
          strip.text.y= element_text(size = 26, face = "bold"))
  
  # ggsave(paste0(path_result, "loss_",main,sub,".png"), 
  #        plot = last_plot(), width = 18, height = 14)
  p
  #gridplot = c(gridplot, list(p))
    

library(dplyr)
max_step = max(h$step)
hmax_test = filter(h, step == max_step, sample=='nll_test')
mean(hmax_test$loss) 
sd(hmax_test$loss)
sd(hmax_test$loss)/sqrt(nrow(hmax_test))
t.test(hmax_test$loss)




