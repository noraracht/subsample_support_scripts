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
my_colors3  = c(brewer.pal(9, "RdBu")[c(1,2, 3, 7, 9)])

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


d=read.csv('summary_main.csv',sep=",",h=T)


head(d)
ggplot(aes(x=as.factor(reps), y=support/100, group=branch, color=as.factor(sp), linetype=as.factor(cluster),), data=d)+
  #geom_bar(position="dodge", stat="identity")+
  geom_point()+
  geom_line(alpha = 0.6, size=1)+
  #geom_text_repel(aes(label=ifelse(support!=100, support, "")),
  #                force = 10, nudge_x = -0.001,nudge_y = 0.004, show.legend = F, size=3 )+
  #geom_text_repel(aes(label=support),hjust=0, vjust=0)+
  scale_x_discrete(name="Number of replicates")+
  scale_y_continuous(name="Support" , labels=percent, limits = c(0.2, 1))+
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

ggsave("bee_support_vs_reps_main_no_label.pdf",width=5,height = 4)



#consensus

d=read.csv('summary_cons.csv',sep=",",h=T)
ggplot(aes(x=as.factor(reps), y=support/100, group=branch, color=as.factor(sp), linetype=as.factor(cluster),), data=d)+
  #geom_bar(position="dodge", stat="identity")+
  geom_point()+
  geom_line(alpha = 0.6, size=1)+
  #geom_text_repel(aes(label=ifelse(support!=100, support, "")),
  #                force = 10, nudge_x = -0.001,nudge_y = 0.004, show.legend = F, size = 3 )+
  #geom_text_repel(aes(label=support),hjust=0, vjust=0)+
  scale_x_discrete(name="Number of replicates")+
  scale_y_continuous(name="Support" , labels=percent, limits = c(0.2, 1))+
  theme_classic()+
  theme(legend.text.align = 0)+
  #scale_fill_grey()+
  scale_linetype_manual(name="", values = c(1, 2), 
                        labels=c((expression(italic("B. haemor.")~"clade")), expression(italic("B. brev.")~"clade") ))+
  scale_colour_manual(name="", values = c( my_colors[1], my_colors[2]), 
                      labels=c("Parent", "Child"))+
  theme(legend.position = c(.16,.14), 
        legend.margin=margin(t = -0.5, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ), 
         linetype = guide_legend(title = NULL, order = 2, reverse=FALSE,))


ggsave("bee_support_vs_reps_cons_no_label.pdf",width=5,height = 4)



#fbot <- ggarrange(f3, f4, labels = c("C", "D"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
#arrange <-ggarrange(ftop, NULL, fbot, labels = c(NA, NA, NA), ncol = 1, heights=c(1, 0.05, 1), nrow = 3)
fright

ggsave("fel_8taxa_three_plots.pdf", width=5.0, height = 12.0, fright)

