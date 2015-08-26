foo1 <- data.frame(cov=rep("BW",10),coval=11:20,eta=rep(c("ETA1","ETA2"),each=5), etaval=rnorm(10))
foo2 <- data.frame(cov=rep("CV",10),coval=11:20,eta=rep(c("ETA1","ETA2"),each=5), etaval=rnorm(10))
foo <- rbind(foo1, foo2)
library(dplyr)
library(ggplot2)
library(lazyeval)
ebe_plots <- function(df){
  # requires dplyr, ggplot2, and lazyeval packages
  p1 <- df %>%
    ggplot(aes(x=etaval,y=coval))+
    geom_point()+
    facet_wrap(~cov, scales = "free")+
    scale_x_continuous(breaks=-1.5:1.5)+
    stat_smooth(method = "lm")+
    geom_vline(xintercept=0,col="red",linetype="dashed")
  return(p1)
}

foo %>% split(.$eta) %>% lapply(ebe_plots)
