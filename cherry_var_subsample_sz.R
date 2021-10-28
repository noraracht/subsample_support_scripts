require(ggplot2); require(scales); require(reshape2); 
#install.packages("dplyr")
require(dplyr)
#require(Hmisc)
library("readxl")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#dir1 = 'd0.01'
#dir2 = 'd0.05'
#dir3 = 'd0.25'

#dir1 = '16x_d0.01'
#dir2 = '16x_d0.05'
#dir3 = '16x_d0.25'

#d1=read.csv(paste0(dir1,'/combined_all_dist_removed_outliers.csv'),sep=",",h=T)
#d2=read.csv(paste0(dir2,'/combined_all_dist_removed_outliers.csv'),sep=",",h=T)
#d3=read.csv(paste0(dir3,'/combined_all_dist_removed_outliers.csv'),sep=",",h=T)
#d <- rbind(d1, d2, d3)
#write.csv(d,"combined_all_dist_removed_outliers_threeD_16x.csv", row.names = FALSE)

d <- read.csv("combined_all_dist_removed_outliers_threeD.csv",sep=",",h=T)
#d <- read.csv("combined_all_dist_removed_outliers_threeD_16x.csv",sep=",",h=T)


tail(d)


b1 = subset(d, d['condition']=="subsample_no_strap" | d['condition']=="subsample_uncor")
b = subset(b1, b1['exp_coverage']>0.06)
head(b)
d=b
#b=d

library(RColorBrewer)
my_palette = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])
#values=my_palette




d$bb=as.factor(round(d$b,3))
levels(d$bb)=list("4/5"=0.8,"6/7"=0.857,"9/10*"=9/10,"13/14"=0.929,"20/21"=0.952)
#plit by color but all conditions together
myplt <- ggplot(aes(
  x=reorder(paste(bb,sprintf("%0.2f", round(exp_coverage, digits = 2)),sep="\n"),b),
              y=dist, groupy(exp_coverage), color=condition),
       data=d[d$condition %in% c("subsample_no_strap","subsample_uncor"),])+
  #geom_point()+
  #stat_summary(geom="point",fun.y = mean)+
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  geom_boxplot()+
  #stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  #geom_violin()+
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1), size = 0.1)+
  
  #geom_boxplot(aes(y=sqrt(en(exp_coverage)/en(2))*(dist-mean)+main),color="black",
  #             data=d[d$condition %in% c("subsample_uncor"),])+
  #theme_bw()+
  theme_classic()+
  #facet_wrap(facets = vars(condition))+
  facet_wrap(~true_dist, scales = "free")+
  #geom_hline(yintercept=0.05, color='black', linetype="dashed", size = 0.4)+
  labs(y= "Distance", x = "Subsample coverage")+
  scale_color_manual(name="", values = c("#ca0020", "#0571b0"), 
                     labels = c("Corrected", "Uncorrected"))+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")
  theme(legend.position = c(.92,.11), 
      legend.margin=margin(t = -0.5, unit='cm'))
  #coord_cartesian(ylim=c(0.239,0.271))+
  myplt
  ggsave("cherry_var_subsample_all_2x.pdf", width=8.0,height = 3.5, myplt)
  #ggsave("cherry_var_subsample_all_16x.pdf", width=8.0,height = 4)




# 16x plot
d <- read.csv("combined_all_dist_removed_outliers_threeD_16x.csv",sep=",",h=T)


b1 = subset(d, d['condition']=="subsample_no_strap" | d['condition']=="subsample_uncor")
b = subset(b1, b1['exp_coverage']>0.26)
head(b)
d=b
#b=d

library(RColorBrewer)
my_palette = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])
#values=my_palette


d$bb
d$bb=as.factor(round(d$b,3))
levels(d$bb)=list("3/4"=0.75, "4/5"=0.8,"6/7"=0.857,"9/10*"=9/10,"13/14"=0.929,"20/21"=0.952)

#plit by color but all conditions together
ggplot(aes(
  x=reorder(paste(bb,sprintf("%0.2f", round(exp_coverage, digits = 2)),sep="\n"),b),
  y=dist, groupy(exp_coverage), color=condition),
  data=d[d$condition %in% c("subsample_no_strap","subsample_uncor"),])+

#ggplot(aes(x=as.factor(sprintf("%0.2f", round(exp_coverage, digits = 2))),y=dist, groupy(exp_coverage), color=condition),
#       data=d[d$condition %in% c("subsample_no_strap","subsample_uncor"),])+
  
  
  #geom_point()+
  #stat_summary(geom="point",fun.y = mean)+
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  geom_boxplot()+
  #geom_violin()+
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1), size = 0.1)+
  
  #geom_boxplot(aes(y=sqrt(en(exp_coverage)/en(2))*(dist-mean)+main),color="black",
  #             data=d[d$condition %in% c("subsample_uncor"),])+
  #theme_bw()+
  theme_classic()+
  #facet_wrap(facets = vars(condition))+
  facet_wrap(~true_dist, scales = "free")+
  #geom_hline(yintercept=0.05, color='black', linetype="dashed", size = 0.4)+
  labs(y= "Distance", x = "Subsample coverage")+
  scale_color_manual(name="", values = c("#ca0020", "#0571b0"), 
                     labels = c("Corrected", "Uncorrected"))+
  geom_vline(xintercept=3.5, linetype="dashed", 
             color = "grey", size=0.4)+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  theme(legend.position = 'bottom', legend.direction="horizontal")
  #coord_cartesian(ylim=c(0.239,0.271))
  #ggsave("cherry_var_subsample_all_2x.pdf", width=7.0,height = 4)
  ggsave("cherry_var_subsample_all_16x.pdf", width=8.0,height = 3.5)



############################################################################

head(d)

#(2000000^0.75)/2000000
#en = function(cvg) sum((dpois(1:max(10,cvg*10),lambda=cvg)))
#en = function(cvg) (1-(dpois(0,lambda=cvg)))

#sqrt(en(0.7)/en(2))
#sqrt(0.7/2)


#sqrt(en(1/4)/en(16))
#sqrt(0.25/16)

#sqrt(en(0.25)/en(16))
#sqrt(0.25/16)

#sqrt(en(7)/en(16))
#sqrt(7/16)

#en(16)
#############


#individual plots
ggplot(aes(x=as.factor(round(exp_coverage, digits = 2) ),y=dist, groupy(exp_coverage)), data=d)+
  #geom_point()+
  #stat_summary(geom="point",fun.y = mean)+
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  geom_boxplot()+
  #theme_bw()+
  theme_classic()+
  facet_wrap(facets = vars(condition))+
  #facet_wrap(~true_dist, scales = "free")+
  #geom_hline(yintercept=0.05, color='red', linetype="dashed", size = 0.4)+
  labs(y= "Distance", x = "Coverage")+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  theme(legend.position = 'bottom', legend.direction="horizontal")+
  #geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
  ggsave("cherry_var_subsample_d0.01.pdf", width=5.2,height = 4)






b=read.csv('/Users/admin/Documents/support/cherry_subsample_sim_var_subsample_sz/d0.05/combined_all_dist_removed_outliers.csv')
b=b[b$condition=="subsample_uncor",]
head(b)
b=merge(b,dcast(sample+variable+true_dist+b+prct_reads~.,data=b,value.var = "dist",fun.aggregate = mean))
names(b)[10]="mean"
head(b)
me=0.04981580886482962
ggplot(aes(x=prct_reads,y=(prct_reads)^(2/3)*(dist-mean)+me,group=prct_reads),data=b)+
  stat_summary(geom="point",fun.y = var)+
  theme_bw()+
  geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")


d[d$V1<33,]
