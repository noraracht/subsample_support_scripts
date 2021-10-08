require(ggplot2); require(scales); require(reshape2); 
#install.packages("dplyr")
require(dplyr)
require(Hmisc)
library("readxl")
library(ggpubr)
#install.packages("ggpubr")

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


d <- read.csv("combined_all_dist_new.csv",sep=",",h=T)

head(d)




#d$exp_coverage = round(d$exp_coverage, digits = 2) 

#b1 = subset(d, d['condition']!="subsample_main_main" & d['condition']!="subsample_mean_mean")
#b = subset(b1, b1['exp_coverage']>0.06)
#head(b1)
#b=d

library(RColorBrewer)
my_palette = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])
values=my_palette

#groupy(true_dist, condition)

# scaled dist
dp <-ggplot(aes(x=as.factor(true_dist),y=scaled_dist, groupy(condition), color=condition ), 
               data=d[d$condition %in% c("subsample_no_strap","sim", "subsample_uncor") & d$true_dist %in% c(0.01, 0.02, 0.05, 0.10, 0.15,  0.25), ], )+
  #geom_point()+
  geom_rect(xmin = 1.5,xmax = 2.5,
            ymin = -Inf,ymax = Inf,alpha = 1.0, fill="#f7f7f7", color = NA) +
  geom_rect(xmin = 3.5,xmax = 4.5,
            ymin = -Inf,ymax = Inf,alpha = 1.0, fill="#f7f7f7", color = NA) +
  geom_rect(xmin = 5.5,xmax = 6.5,
            ymin = -Inf,ymax = Inf,alpha = 1.0, fill="#f7f7f7", color = NA) +
  #geom_violin()+
  geom_boxplot()+
  #geom_point(aes(x=0.75, y=0.009956196351708286/0.01), colour="blue")+
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1))+
  
  #stat_summary(geom="point",fun.y = mean)+
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  #theme_bw()+
  theme_classic()+
  
  
  #annotate("rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf,alpha=0.1,fill="#fddbc7")+
  #annotate("rect", xmin=3.5, xmax=4.5, ymin=-Inf, ymax=Inf,alpha=0.1,fill="#fddbc7")+
  #annotate("rect", xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf,alpha=0.1,fill="#fddbc7")+
 

  #facet_wrap(facets = vars(true_dist), ncol = 5)+
  geom_hline(yintercept=1.0, color='grey', linetype="dashed", size = 0.4)+
  labs(y= "Estimated / true distance", x = "True distance")+
  #theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1.0))+
  scale_color_manual(name="", values = c("#252525", "#ca0020", "#0571b0" ), 
                     labels = c("Simulated", "Corrected", "Uncorrected"))+
  #theme(axis.text.x = element_text(angle = 45))+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")+
  theme(legend.position = c(.46,.05), 
      legend.margin=margin(t = 0.0, unit='cm'), legend.direction="horizontal" )+
  scale_y_continuous(limits = c(0.965, NA))
  #geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
  ggsave("cherry_scaled_dist.pdf", width=4.5,height = 3.5, dp)


head(d)
library(dplyr)
# summarising all non-grouping variables
df2 <- d %>% group_by(condition, true_dist) %>% summarise(Variance=var(dist))
df2


# scaled dist
bp<-ggplot(aes(x=as.factor(true_dist), y=Variance, color=condition, group = condition), data=df2[df2$condition %in% c("subsample_no_strap","sim", "subsample_uncor") & df2$true_dist %in% c(0.01, 0.02, 0.05, 0.10, 0.15, 0.25), ], )+
  geom_point()+
  geom_line()+
  scale_y_continuous(trans='log10')+
  #stat_summary(geom="point",fun.y = mean)+
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  #geom_boxplot()+
  #theme_bw()+
  theme_classic()+
  scale_color_manual(name="", values = c("#252525", "#ca0020", "#0571b0" ), 
                    labels = c("Simulated", "Corrected", "Uncorrected"))+
  #facet_wrap(facets = vars(true_dist), ncol = 8)+
  #geom_hline(yintercept=1.0, color='red', linetype="dashed", size = 0.4)+
  labs(y= "Log(variance of distance)", x = "True distance")+
  theme(legend.position = c(.83,.20), 
        legend.margin=margin(t = 0.0, unit='cm') )
  #theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1.0))+
  #theme(axis.text.x = element_text(angle = 45))+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")
  #geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
  ggsave("cherry_var.pdf", width=4,height = 3.5, bp)




p1 <- ggarrange(dp, bp, labels = c("A", "B"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)

arrange <-ggarrange(p1, NULL, myplt, labels = c("", "", "C"), ncol = 1, heights=c(1, 0.05, 1), nrow = 3)
arrange
ggsave("cherry_three_plots.pdf", width=8.0,height = 7.0, arrange)







#sd dev
ggplot(aes(x=as.factor(condition),y=dist, groupy(true_dist)), data=d)+
    #geom_point()+
    #geom_violin() +
    #stat_summary(fun.y = "mean", geom = "col") +
    stat_summary(geom="point", fun.data = mean_sdl, colour='red', fun.args = list(mult = 1))+
    #geom_boxplot(aes(factor(exp_coverage), dist))+
    theme_classic()+
    facet_wrap(facets = vars(true_dist), ncol = 8, scales = "free")+
    labs(y= "Scaled distance", x = "Condition")+
    scale_y_continuous(trans='log10')+
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1.0))+
    #theme(axis.text.x = element_text(angle = 45))+
    #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
    #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
    #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
    theme(legend.position = 'bottom', legend.direction="horizontal")+
    #geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
    ggsave("cherry_sd_dev.pdf", width=7.2,height = 4)
  
  
#

#############




d[d$V1<33,]
