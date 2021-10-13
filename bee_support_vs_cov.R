require(ggplot2); require(scales); require(reshape2); 
#install.packages("dplyr")
require(dplyr)
#require(Hmisc)
library("readxl")
library(RColorBrewer)
library("ggsci")
#install.packages("ggrepel")
library("ggrepel")
library(ggpubr)
display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 8, name = "Dark2")
#library(ape)


my_colors <- RColorBrewer::brewer.pal(8, "Dark2")

my_colors2 <- RColorBrewer::brewer.pal(8, "Paired")
my_colors3 <- RColorBrewer::brewer.pal(8, "RdBu")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


d=read.csv('bee_support_vs_cov.csv',sep=",",h=T)

head(d)
ggplot(aes(x=as.factor(coverage), y=support/100, group=branch, color=as.factor(sp), linetype=as.factor(cluster),), data=d[d$coverage!=0,])+
  #geom_bar(position="dodge", stat="identity")+
  geom_point()+
  geom_line()+
  geom_text_repel(aes(label=ifelse(support!=100, support, "")),
                  force = 10, nudge_x = -0.01,nudge_y = 0.004, show.legend = F )+
  #geom_text_repel(aes(label=support),hjust=0, vjust=0)+
  scale_x_discrete(name="Coverage")+
  scale_y_continuous(name="Support" , labels=percent)+
  theme_classic()+
  theme(legend.text.align = 0)+
  #scale_fill_grey()+
  scale_linetype_manual(name="", values = c(1, 2), 
                        labels=c((expression(italic("B. haemor.")~"clade")), expression(italic("B. brev.")~"clade") ))+
  scale_colour_manual(name="", values = c( my_colors[1], my_colors[2]), 
                    labels=c("Parent", "Child"))+
  theme(legend.position = c(.86,.14), 
        legend.margin=margin(t = -0.5, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ), 
         linetype = guide_legend(title = NULL, order = 2, reverse=FALSE,))
  


ggsave("bee_support_vs_coverage.pdf",width=5,height = 4)



# branch length vs support on real data

t1=read.csv('dist_bees.txt',sep=",",h=F, skip = 1)
t2=read.csv('dist_whales.txt',sep=",",h=F, skip = 1)
t3=read.csv('dist_lice.txt',sep=",",h=F, skip = 1)
t4=read.csv('dist_dros.txt',sep=",",h=F, skip = 1)
t5=read.csv('dist_dros_200.txt',sep=",",h=F, skip = 1)

t <- rbind(t1, t2, t3, t5)


ggplot(aes(x=log10(as.numeric(V1)), y=as.numeric(V2)/100), data=t)+
  geom_point(aes(color=V3), alpha=0.8)+
  geom_smooth(se=F, color=my_colors[8], size=0.5, linetype="dashed")+

  #stat_summary_bin(fun = "mean", geom="point", bins=100, alpha=0.2) +
  #stat_summary_bin(fun.data = "mean_sdl", fun.args = list(mult = 1), geom="pointrange", alpha=0.5, bins=20)+
  #stat_summary_bin(fun = "mean", bins= 20, size=0.8, geom= "line") +
  #stat_density2d()+
  #stat_ellipse(geom="polygon", aes(fill = variable), alpha = 0.2,show.legend = FALSE, level = 0.8)+
  #geom_smooth(se=F, fullrange = FALSE, level = 0.95,linetype=1)+
  #scale_x_continuous()+
  scale_y_continuous(labels=percent, limits=c(NA, 1.05))+
  #geom_smooth(method=lm,linetype=2,colour="black",se=F, size=0.4,formula=y~x, 
  #            fullrange = FALSE, level = 0.95,) +
  theme_classic()+
  labs(y= "Support", x = "Log(branch length)")+
  scale_color_manual(name="", values = c(my_colors[1], my_colors [2], my_colors[3], my_colors[4] ), 
                     labels=c("Bee", "Whale","Lice", "Drosophila"))+
  theme(legend.position = c(.85,.15), 
        legend.margin=margin(t = 0.0, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=FALSE, ))


ggsave("branch_len_vs_support_real.pdf",width=5,height = 4)


