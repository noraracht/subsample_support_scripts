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

#d1=read.csv(paste0(dir1,'/combined_all_dist_removed_outliers.csv'),sep=",",h=T)
#d2=read.csv(paste0(dir2,'/combined_all_dist_removed_outliers.csv'),sep=",",h=T)
#d3=read.csv(paste0(dir3,'/combined_all_dist_removed_outliers.csv'),sep=",",h=T)
#d <- rbind(d1, d2, d3)
#write.csv(d,"combined_all_dist_removed_outliers_threeD.csv", row.names = FALSE)


d <- read.csv("combined_all_dist_removed_outliers_threeD.csv",sep=",",h=T)

head(d)

d$exp_coverage = round(d$exp_coverage, digits = 2) 

b1 = subset(d, d['condition']=="subsample_no_strap" | d['condition']=="subsample_uncor")
b = subset(b1, b1['exp_coverage']>0.06)
head(b)
#b=d

library(RColorBrewer)
my_palette = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])
#values=my_palette



#individual plots
ggplot(aes(x=as.factor(exp_coverage),y=dist, groupy(exp_coverage)), data=b[b$true_dist==0.01,])+
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


#plit by color but all conditions together
ggplot(aes(x=as.factor(exp_coverage),y=dist, groupy(exp_coverage), color=condition), data=b)+
  #geom_point()+
  #stat_summary(geom="point",fun.y = mean)+
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  geom_boxplot()+
  #theme_bw()+
  theme_classic()+
  #facet_wrap(facets = vars(condition))+
  facet_wrap(~true_dist, scales = "free")+
  #geom_hline(yintercept=0.05, color='black', linetype="dashed", size = 0.4)+
  labs(y= "Distance", x = "Coverage")+
  scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  theme(legend.position = 'bottom', legend.direction="horizontal")+
  ggsave("cherry_var_subsample_all.pdf", width=8.0,height = 4)



#############

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
