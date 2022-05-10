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


d1<- read.csv("combined_resample_all_dist.csv",sep=",",h=T)
d2 <- read.csv("combined_subsample_all_dist.csv",sep=",",h=T) #Main correction
d1$test = "resample"
d2$test = "subsample"

d <- rbind(d1, d2)
head(d)


d=d[d$variable %in% c("ucseq_ucseq_merged_SRR6425991_2") & !d$cond %in% c("main"),]

d=d[!d$sample %in% c("ucseq_ucseq_merged_SRR6425991_2"),]

d
#d$bb=as.factor(round(d$b,3))

levels(d$sample)
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425989_2"] <- "Drosophila bipectinata"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425990_2"] <- "Drosophila erecta"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425991_2"] <- "Drosophila ananassae"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425992_2"] <- "Drosophila biarmipes"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425993_2"] <- "Drosophila mauritiana"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425995_2"] <- "Drosophila eugracilis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425997_2"] <- "Drosophila mojavensis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425998_2"] <- "Drosophila persimilis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425999_2"] <- "Drosophila simulans"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426000_2"] <- "Drosophila virilis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426001_2"] <- "Drosophila pseudoobscura"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426002_2"] <- "Drosophila sechellia"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426003_2"] <- "Drosophila willistoni"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426004_2"] <- "Drosophila yakuba"




#levels(d$sample)=list("3/4"=0.75, "4/5"=0.8,"6/7"=0.857,"9/10*"=9/10,"13/14"=0.929,"20/21"=0.952)



head(d)
#d$exp_coverage = round(d$exp_coverage, digits = 2) 

#b1 = subset(d, d['condition']!="subsample_main_main" & d['condition']!="subsample_mean_mean")
#b = subset(b1, b1['exp_coverage']>0.06)
#head(b1)
#b=d

library(RColorBrewer)
my_palette = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])
my_colors <- RColorBrewer::brewer.pal(8, "Dark2")
values=my_palette

#groupy(true_dist, condition)

head(d)

theme_grey()$plot.margin

dp<-ggplot(aes(x=sample,y=est_true, color=test), data=d)+
  #geom_point(alpha=0.5)+
  #stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom="pointrange", alpha=0.5)+
  stat_summary(aes(group=test),fun = mean, fun.min = min, fun.max = max, geom="pointrange")+
  #stat_summary(fun = mean, fun.min = min, fun.max = max, colour = test, size = 2) +
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1))+
  
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  #theme_bw()+
  theme_classic()+
  geom_hline(yintercept=1.0, linetype="dashed", 
             color = "grey", size=0.4)+
  
  #facet_wrap(facets = vars(true_dist), ncol = 5)+
  #geom_hline(yintercept=1.0, color='grey', linetype="dashed", size = 0.4)+
  labs(y= "Estimated/expected distance", x = NA)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1.0, face='italic'), 
        axis.title.x=element_blank(),)+
  scale_color_manual(name="", values = c(my_colors[4], my_colors[3]), 
                     labels = c("Resampling", "Subsampling" ))+
  #theme(axis.text.x = element_text(angle = 45))+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")+
  theme(legend.position = c(.89,.95),
        legend.margin=margin(t = 0.0, unit='cm'), legend.direction="vertical" )+
  theme(plot.margin=unit(c(5.5,5.5,5.5,20.0),"pt"))
  #scale_y_continuous(limits = c(0.965, NA))+
#theme(legend.position = "none")
#geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
dp
ggsave("resample.pdf", width=6.5,height = 3.5, dp)






###### combined corrected, uncorrected and bootstrap
d3<- read.csv("_summary.csv",sep=",",h=T)
d3
names(d3)[names(d3) == 'sample_2'] <- 'variable'
names(d3)[names(d3) == 'uncorrected_dist'] <- 'value_x'
names(d3)[names(d3) == 'no_strapped_dist'] <- 'value_y'
d3$test = "subsample_raw"
d3$est_true = d3$value_x/d3$value_y






common_cols <- intersect(colnames(d), colnames(d3))
d_all<-rbind(
  subset(d, select = common_cols), 
  subset(d3, select = common_cols)
)
d_all

d
d_all=d_all[d_all$variable %in% c("ucseq_ucseq_merged_SRR6425991_2"),]

d_all=d_all[!d_all$sample %in% c("ucseq_ucseq_merged_SRR6425991_2"),]



d_all
#d$bb=as.factor(round(d$b,3))

levels(d_all$sample)
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425989_2"] <- "Drosophila bipectinata"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425990_2"] <- "Drosophila erecta"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425991_2"] <- "Drosophila ananassae"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425992_2"] <- "Drosophila biarmipes"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425993_2"] <- "Drosophila mauritiana"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425995_2"] <- "Drosophila eugracilis"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425997_2"] <- "Drosophila mojavensis"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425998_2"] <- "Drosophila persimilis"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6425999_2"] <- "Drosophila simulans"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6426000_2"] <- "Drosophila virilis"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6426001_2"] <- "Drosophila pseudoobscura"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6426002_2"] <- "Drosophila sechellia"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6426003_2"] <- "Drosophila willistoni"
levels(d_all$sample)[levels(d_all$sample)=="ucseq_ucseq_merged_SRR6426004_2"] <- "Drosophila yakuba"




#levels(d$sample)=list("3/4"=0.75, "4/5"=0.8,"6/7"=0.857,"9/10*"=9/10,"13/14"=0.929,"20/21"=0.952)



head(d_all)
#d$exp_coverage = round(d$exp_coverage, digits = 2) 

#b1 = subset(d, d['condition']!="subsample_main_main" & d['condition']!="subsample_mean_mean")
#b = subset(b1, b1['exp_coverage']>0.06)
#head(b1)
#b=d

library(RColorBrewer)
my_palette = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])
my_colors <- RColorBrewer::brewer.pal(8, "Dark2")
values=my_palette
my_colors
#groupy(true_dist, condition)

head(d_all)

theme_grey()$plot.margin

dp<-ggplot(aes(x=reorder(sub("Drosophila ","D. ",sample),-est_true),y=est_true, color=test), data=d_all)+
  #geom_point(alpha=0.5)+
  #stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom="pointrange", alpha=0.5)+
  #stat_summary(aes(group=test),fun = mean, fun.min = min, fun.max = max, geom="pointrange", alpha=0.5)+
  #stat_summary(fun = mean, fun.min = min, fun.max = max, colour = test, size = 2) +
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1))+
  
  geom_violin(draw_quantiles = c(1/4,1/2,3/4))+
  #theme_bw()+
  theme_classic()+
  geom_hline(yintercept=1.0, linetype="dashed", 
             color = "grey", size=0.4)+
  
  #facet_wrap(facets = vars(true_dist), ncol = 5)+
  #geom_hline(yintercept=1.0, color='grey', linetype="dashed", size = 0.4)+
  labs(y= "Estimated/expected distance", x = NA)+
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust=1.0, face='italic'), 
        axis.title.x=element_blank(),)+
  scale_color_manual(name="", values = c(my_colors[4], my_colors[5], my_colors[3]), 
                     labels = c("Resampling (bootstrapping)", "Subsampling corrected ","Subsampling uncorrected"))+
  #theme(axis.text.x = element_text(angle = 45))+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")+
  theme(legend.position = c(.84,.92),
        legend.margin=margin(t = 0.0, unit='cm'), legend.direction="vertical" )+
  theme(plot.margin=unit(c(5.5,5.5,5.5,20.0),"pt"))
#scale_y_continuous(limits = c(0.965, NA))+
#theme(legend.position = "none")
#geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
dp
ggsave("resample_all.pdf", width=6.0,height = 3.5, dp)


write.table(d_all, "combined_all_three.csv",row.names = FALSE, sep=',')





##############################################################################
##############################################################################
#graph in ppt

dp<-ggplot(aes(x=reorder(sub("Drosophila ","D. ",sample),-est_true),y=est_true, color=test), data=d_all[d_all$test !='subsample', ])+
  #geom_point(alpha=0.5)+
  #stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom="pointrange", alpha=0.5)+
  #stat_summary(aes(group=test),fun = mean, fun.min = min, fun.max = max, geom="pointrange", alpha=0.5)+
  #stat_summary(fun = mean, fun.min = min, fun.max = max, colour = test, size = 2) +
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1))+
  
  geom_violin(draw_quantiles = c(1/4,1/2,3/4))+
  #theme_bw()+
  theme_classic()+
  geom_hline(yintercept=1.0, linetype="dashed", 
             color = "grey", size=0.4)+
  
  #facet_wrap(facets = vars(true_dist), ncol = 5)+
  #geom_hline(yintercept=1.0, color='grey', linetype="dashed", size = 0.4)+
  labs(y= "Estimated/expected distance", x = NA)+
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust=1.0, face='italic'), 
        axis.title.x=element_blank(),)+
  scale_color_manual(name="", values = c(my_colors[4], my_colors[3]), 
                     labels = c("Resampling (bootstrapping)", "Subsampling"))+
  #theme(axis.text.x = element_text(angle = 45))+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")+
  theme(legend.position = c(.82,.92),
        legend.margin=margin(t = 0.0, unit='cm'), legend.direction="vertical" )+
  theme(plot.margin=unit(c(5.5,5.5,5.5,20.0),"pt"))
#scale_y_continuous(limits = c(0.965, NA))+
#theme(legend.position = "none")
#geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
dp
ggsave("resample_corrected_uncorrected.pdf", width=6.0,height = 3.5, dp)



#my_colors <- RColorBrewer::brewer.pal(8, "Dark2")

dp<-ggplot(aes(x=reorder(sub("Drosophila ","D. ",sample),-est_true),y=est_true, color=test), data=d_all[d_all$test !='subsample', ])+
  #geom_point(alpha=0.5)+
  #stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom="pointrange", alpha=0.5)+
  #stat_summary(aes(group=test),fun = mean, fun.min = min, fun.max = max, geom="pointrange", alpha=0.5)+
  #stat_summary(fun = mean, fun.min = min, fun.max = max, colour = test, size = 2) +
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1))+
  
  geom_violin(draw_quantiles = c(1/4,1/2,3/4), key_glyph = draw_key_path)+
  #theme_bw()+
  theme_classic()+
  geom_hline(yintercept=1.0, linetype="dashed", 
             color = "grey", size=0.4)+
  
  #facet_wrap(facets = vars(true_dist), ncol = 5)+
  #geom_hline(yintercept=1.0, color='grey', linetype="dashed", size = 0.4)+
  labs(y= "Estimated/expected distance", x = NA, size = 10)+
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust=1.0, face='italic'), 
        axis.title.x=element_blank(),)+
  scale_color_manual(name="", values = c(my_colors[4], my_colors[1]), 
                     labels = c("Resampling reads with replacement (bootstrapping)", "Subsampling reads without replacement"))+
  #theme(axis.text.x = element_text(angle = 45))+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")+
  theme(legend.position = c(.66,.92),
        legend.margin=margin(t = 0.0, unit='cm'), legend.direction="vertical" )+
  theme(plot.margin=unit(c(5.5,5.5,5.5,20.0),"pt"))+
  theme(legend.text=element_text(size=10))
#scale_y_continuous(limits = c(0.965, NA))+
#theme(legend.position = "none")
#geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
dp
ggsave("resample_corrected_uncorrected_v2.pdf", width=6.0,height = 3.5, dp)

# Show only colorblind-friendly brewer palettes
display.brewer.all(colorblindFriendly = TRUE)
# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 8, name = 'Dark2')
# Hexadecimal color specification 
brewer.pal(n = 8, name = "Dark2")

d_all

##############################################################################
##############################################################################

###### raw resample distances with raw subsample distances
d1<- read.csv("_summary.csv",sep=",",h=T)
d1
names(d1)[names(d1) == 'sample_2'] <- 'variable'
names(d1)[names(d1) == 'uncorrected_dist'] <- 'value_x'
names(d1)[names(d1) == 'no_strapped_dist'] <- 'value_y'
d1$test = "resample"
d1$est_true = d1$value_x/d1$value_y

d1

d2 <- read.csv("combined_resample_all_dist.csv",sep=",",h=T) #Main correction
d2$test = "subsample"
d2=d2[!d2$cond %in% c("main"),]


common_cols <- intersect(colnames(d1), colnames(d2))
d<-rbind(
  subset(d1, select = common_cols), 
  subset(d2, select = common_cols)
)

d
d=d[d$variable %in% c("ucseq_ucseq_merged_SRR6425991_2"),]

d=d[!d$sample %in% c("ucseq_ucseq_merged_SRR6425991_2"),]



d
#d$bb=as.factor(round(d$b,3))

levels(d$sample)
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425989_2"] <- "Drosophila bipectinata"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425990_2"] <- "Drosophila erecta"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425991_2"] <- "Drosophila ananassae"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425992_2"] <- "Drosophila biarmipes"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425993_2"] <- "Drosophila mauritiana"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425995_2"] <- "Drosophila eugracilis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425997_2"] <- "Drosophila mojavensis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425998_2"] <- "Drosophila persimilis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6425999_2"] <- "Drosophila simulans"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426000_2"] <- "Drosophila virilis"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426001_2"] <- "Drosophila pseudoobscura"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426002_2"] <- "Drosophila sechellia"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426003_2"] <- "Drosophila willistoni"
levels(d$sample)[levels(d$sample)=="ucseq_ucseq_merged_SRR6426004_2"] <- "Drosophila yakuba"


d

#levels(d$sample)=list("3/4"=0.75, "4/5"=0.8,"6/7"=0.857,"9/10*"=9/10,"13/14"=0.929,"20/21"=0.952)



head(d)
#d$exp_coverage = round(d$exp_coverage, digits = 2) 

#b1 = subset(d, d['condition']!="subsample_main_main" & d['condition']!="subsample_mean_mean")
#b = subset(b1, b1['exp_coverage']>0.06)
#head(b1)
#b=d

library(RColorBrewer)
my_palette = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])
my_colors <- RColorBrewer::brewer.pal(8, "Dark2")
values=my_palette

#groupy(true_dist, condition)

head(d)

theme_grey()$plot.margin

dp<-ggplot(aes(x=sample,y=est_true, color=test), data=d)+
  #geom_point(alpha=0.5)+
  #stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom="pointrange", alpha=0.5)+
  stat_summary(aes(group=test),fun = mean, fun.min = min, fun.max = max, geom="pointrange")+
  #stat_summary(fun = mean, fun.min = min, fun.max = max, colour = test, size = 2) +
  #stat_summary(geom="pointrange", aes(group=condition), position=position_dodge(.9),
  #             fun.data = mean_sdl, colour='black', fun.args = list(mult = 1))+
  
  #geom_boxplot(aes(factor(exp_coverage), dist))+
  #theme_bw()+
  theme_classic()+
  geom_hline(yintercept=1.0, linetype="dashed", 
             color = "grey", size=0.4)+
  
  #facet_wrap(facets = vars(true_dist), ncol = 5)+
  #geom_hline(yintercept=1.0, color='grey', linetype="dashed", size = 0.4)+
  labs(y= "Estimated/expected distance", x = NA)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1.0, face='italic'), 
        axis.title.x=element_blank(),)+
  scale_color_manual(name="", values = c(my_colors[3], my_colors[4]), 
                     labels = c("Subsampling", "Resampling"), guide = guide_legend(reverse = TRUE))+
  #theme(axis.text.x = element_text(angle = 45))+
  #scale_color_manual(name="", values = c("#ca0020", "#0571b0"), )+
  #theme(legend.text=element_text(size=10), legend.title=element_text(size=10))+
  #theme(legend.position = c(0.5, -0.1), legend.direction="horizontal")+
  #theme(legend.position = 'bottom', legend.direction="horizontal")+
  theme(legend.position = c(.89,.95),
        legend.margin=margin(t = 0.0, unit='cm'), legend.direction="vertical" )+
  theme(plot.margin=unit(c(5.5,5.5,5.5,20.0),"pt"))
#scale_y_continuous(limits = c(0.965, NA))+
#theme(legend.position = "none")
#geom_boxplot(aes(y=sqrt(prct_reads)*(dist-mean)+me),color="red")
dp
ggsave("resample_raw.pdf", width=6.5,height = 3.5, dp)





##############################################################################
##############################################################################






# scaled dist
dp <-ggplot(aes(x=as.factor(true_dist),y=scaled_dist, groupy(condition), color=condition ), 
               data=d[d$condition %in% c("subsample_no_strap","sim", "subsample_uncor") & d$true_dist %in% c(0.01, 0.02, 0.05, 0.10, 0.15,  0.25), ], )+
  


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
  labs(y= expression(Log[10](variance~of~distance)), x = "True distance")+
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

bp


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
