if (TRUE){
  hist_1 = history
  hist_select = "c:/Users/sick/dl Dropbox/beate sick/IDP_Projekte/DL_Projekte/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/2020-03-0436.797_grid_search/history_6000.Rdata"
  path_result = "c:/Users/sick/dl Dropbox/beate sick/IDP_Projekte/DL_Projekte/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/2020-03-0436.797_grid_search/"
  load(hist_select)
}

library(ggplot2)

history = hist_grid
str(history)
summary(history)
  

library(tidyr)
h = gather(history, 'sample', 'loss', nll_train:nll_test)
h$sample = as.factor(h$sample)  # test or validation/test

summary(h)
str(h)
xtabs(~spatz+x_scale+regularization, data=h)

gridplot = list()
for(i in 1:length(levels(h$x_scale)) ){
  for( j in 1:length(levels(h$spatz))){
    for( k in 1:length(levels(h$regularization))){
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
      sub = paste("--x_scale_", x_scale_pick,
                  "--spatz_", spatz_pick ,
                  "--regularization_", regularization_pick, sep=""  ) 
 
      p = ggplot(data=h1, aes(x = step, y = loss, color=fold)) + 
        geom_line() +
        geom_hline(yintercept=2.48) +
        ylim(2.0,4.5) +
        ggtitle(main, subtitle=sub) +
        facet_wrap(. ~ sample, ncol=2) +
        theme_bw()
      
      # ggsave(paste0(path_result, "loss_",main,sub,".png"), 
      #        plot = last_plot(), width = 18, height = 14)
      
      gridplot = c(gridplot, list(p))
    }
  }
}

for(i in levels(h$x_scale)){
  idx = sapply(gridplot, function(x) x$data$x_scale[1])
  idx = which(idx==i)
  p=cowplot::plot_grid(plotlist = gridplot[idx], ncol=1)
  ggsave(paste0(path_result, "loss--x_scale_", i, ".png"),
         plot = p, width = 18, height = 14*6, limitsize = F)
}

# # get x_scale value for each plot
# idx = sapply(gridplot, function(x) x$data$x_scale[1])
# # reorder plots such that x_scale=FALSE is in the left column and x_scale=TRUE in the right.
# id = c(1,7,2,8,3,9,4,10,5,11,6,12)
# p=cowplot::plot_grid(plotlist = gridplot[idx], ncol=2)
# ggsave(paste0(path_result, "loss.png"),
#        plot = p, width = 18*2, height = 14*6, limitsize = F)
  
# library(dplyr)
# #dd = hh %>% filter(step == 12000) %>% filter(sample == 'nll_test') 
# ddd = history %>% filter(step == 71000)
# mean(ddd$nll_test) 
# sd(ddd$nll_test) / sqrt(5)


