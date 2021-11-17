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

my_colors <- RColorBrewer::brewer.pal(8, "Dark2")

my_colors2 <- RColorBrewer::brewer.pal(8, "Paired")

#my_colors = c("#bf812d", "#35978f", "#666666")
#my_colors = c("#9970ab", "#5aae61", "#666666")
#my_colors = c("#d6604d", "#4393c3", "#666666")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#paste <( cat Fel_try1.txt |nw_labels -L - ) <( cat Fel_try1.txt|nw_topology -I - ) > 90reps

#Updated:
#paste <( cat Fel_try2.txt |nw_labels -L - ) <( cat Fel_try2.txt |nw_reroot - n6 |nw_topology -I - ) > 500reps



d2=read.csv('8leaves.txt',sep=" ",h=F)
d1=read.csv('8leaves_CONS.txt',sep=" ",h=F)
head(d2)



#nrow(d)
#correction labes are swapped but graphs are manually corrected
d1$correction = "cons"
d2$correction = "mean_main"

###################################### Fig2 ######################################
r=rbind(d1,d2)
r$correction <- factor(r$correction, levels=c("cons", "mean_main"))
r

r$correct=!grepl("0.0",r$V6)
head(r)
head(r)

f6<-ggplot(aes(x=V5/100,color=correction,linetype=correct),data=r)+stat_ecdf()+coord_cartesian(xlim=c(0,1))+theme_classic()+
  geom_vline(xintercept = 0.75, linetype=3,color="gray50")+
  theme_classic()+
  scale_x_continuous(name="Support",labels=percent)+
  scale_y_continuous(name="ECDF")+
  scale_color_manual(name="", values = c(my_colors[2], my_colors [1], my_colors[8] ), 
                     labels=c("Consensus", "Main"))+
  scale_linetype_manual(name="", values = c(1, 2), 
                     labels=c("Incorrect", "Correct"))+
  theme(legend.position = c(.20,.80), 
        legend.margin=margin(t = 0.0, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ), 
         linetype = guide_legend(title = NULL, order = 2, reverse=FALSE,))
f6
ggsave("correct_v2fixed_cons.pdf",width=5,height = 4)
  

#################################################################################

quants = c(10,50,70,80,90,100)


t1 = rename_with(dcast(V2+cut(V5,quants)~!is.na(V6),data=d2),function(x) {"bootstrap"}, starts_with("cut"))
t1
summary(d2$V5)
t1$correction = "mean_main"

t2 = rename_with(dcast(V2+cut(V5,quants)~!is.na(V6),data=d1),function(x) {"bootstrap"}, starts_with("cut"))
t2
t2$correction = "cons"

med_t <-data.frame(t1)


med_t$correction = "median"
dt = rbind(t1, t2)
#dt=t1


dt$ratio = dt$`TRUE`/(dt$`TRUE`+dt$`FALSE`)*100


ratio = vector()
for (i in 2:length(quants)){
  num = seq(quants[i-1]+1, quants[i], by=1)
  #print(num)
  ratio <- c(ratio, median(num))
}
#med_labels


med_t <- cbind(med_t, ratio)
med_t
#names(med_t)[names(med_t) == 'med_labels'] <- 'ratio'

colnames(med_t) <- colnames(dt)
dt = rbind(dt, med_t)




###################################### Fig1 ######################################
d=dt
d$correction <- factor(d$correction, levels=c("mean_main", "cons","median"))

head(d)
#"#ef8a62", "#67a9cf"

f5<-ggplot(aes(x=bootstrap,y=ratio/100, color = correction, group=correction, linetype = correction ), 
       data=d[d$V2=="unif_0.00001" | d$V2=="unif_0.00001_CONS",])+
  #geom_line(linetype = "dashed", color = "black", data=d[d$correction %in% c("Median")])+
  geom_point(
    aes(size=(`FALSE`+`TRUE`)),alpha=0.5
    )+
  geom_line(show.legend = F)+
  theme_classic()+
  scale_y_continuous(labels=percent)+
  scale_linetype_manual(values=c(1,1,3)) +
  #scale_linetype_manual(values=c("dashed","dotted","dashed"))+
  #scale_color_brewer(palette = "Dark2", name="",labels=c("Mean_main","Consensus", "Bin median"))+
  scale_color_manual(name="Method", values = c(my_colors [1], my_colors[2], my_colors[8] ), 
                     labels=c( "Main   ", "Consensus", "Ideal support"))+
  theme(legend.position = c(.72,.25),
        legend.box = "horizontal",
      legend.margin=margin(t = 0.0, unit='cm'))+
  scale_size_binned_area(name="# branches",max_size = 4)+
  labs(y= "Correct / (Correct+Incorrect)", x = "Support bin")
  #geom_line(aes(y=(`TRUE`+`FALSE`)/nrow(d)),color="red")
  #geom_line(aes(y=(`FALSE`)/nrow(d)),color="blue")
f5
#ggsave("correct_mean_mean.pdf")
ggsave("correct_v1_cons.pdf",width=5,height = 4)



##################################################################################

###################################### Fig3 ######################################



ths=1:100
head(r)
m=merge(r,data.frame(ths=ths),by=c())
m$c = as.factor(with(m,ifelse(V5<ths&correct, "FN",ifelse(V5<ths&!correct, "TN", ifelse(V5>=ths&correct, "TP", ifelse(V5>=ths&!correct, "FP",NA))))))
cm = dcast(correction+ths~c,data=m[,c(13,15,16)])

f7<-ggplot(aes(x=FP/(FP+TN),y=TP/(TP+FN),color=correction),data=cm)+
  #geom_point()+
  geom_path()+
  theme_classic()+
  labs(y= "Recall", x = "FPR")+
  geom_text_repel(aes(label=ths),data=cm[cm$ths %% 10==0,], show.legend = F )+
  geom_point(data=cm[cm$ths %% 10==0,], )+
  #geom_text_repel(aes(label=ths),data=cm[cm$ths %% 10==0,], nudge_x = -0.015,nudge_y = 0.01, show.legend = F )+
  scale_color_manual(name="", values = c(my_colors[2], my_colors [1], my_colors[8] ), 
                     labels=c("Consensus", "Main","Bin median"))+
  theme(legend.position = c(.86,.10), 
        legend.margin=margin(t = 0.0, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ))
f7
ggsave("correct_v3fixed_cons.pdf",width=5,height = 4)

#max.overlaps=Inf
############################################################################


fright <- ggarrange(f5, NULL, f6, NULL, f7, labels = c("B", NA, "D", NA, "F"), heights=c(1, 0.05, 1, 0.05, 1), ncol = 1, nrow = 5)
#fbot <- ggarrange(f3, f4, labels = c("C", "D"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
#arrange <-ggarrange(ftop, NULL, fbot, labels = c(NA, NA, NA), ncol = 1, heights=c(1, 0.05, 1), nrow = 3)
fright

ggsave("fel_8taxa_three_plots.pdf", width=5.0, height = 12.0, fright)






# fbot <- ggarrange(f5, f6, f7, labels = c("D", "E", "F"), widths=c(0.5, 0.5, 0.5), ncol = 3, nrow = 1)
# ggsave("fel_8taxa.pdf", width=10.0,height = 3.5, fbot)



tail(r)


######### branch support vs branch length Fig4 #########


#data=subset(ts, ratio < 400), 
#labels = scales::number_format(accuracy = 1.0,
#                               decimal.mark = ','),
f8<-ggplot(aes(x=log10(as.numeric(V4)), y=as.numeric(V5)/100), data=r[r$V2=="unif_0.00001",])+
  #geom_point(alpha=0.2)+
  #stat_summary_bin(fun = "mean", geom="point", bins=100, alpha=0.2) +
  stat_summary_bin(fun.data = "mean_sdl", fun.args = list(mult = 1), geom="pointrange", alpha=0.5, bins=20, color=my_colors[8])+
  stat_summary_bin(fun = "mean", bins= 20, size=0.8, geom= "line", color=my_colors[8]) +
  #stat_density2d()
  #stat_ellipse(geom="polygon", aes(fill = variable), alpha = 0.2,show.legend = FALSE, level = 0.8)+
  #geom_smooth(se=F, fullrange = FALSE, level = 0.95,linetype=1)+
  #scale_x_continuous()+
  scale_x_reverse()+
  scale_y_continuous(labels=percent, limits=c(NA, NA), breaks=c(0.4, 0.6, 0.8, 1))+
  #geom_smooth(method=lm,linetype=2,colour="black",se=F, size=0.4,formula=y~x, 
  #            fullrange = FALSE, level = 0.95,) +
  theme_classic()+
  labs(y= "Support", x = expression(Log[10](branch~length)))
  #scale_color_manual(name="", values = c(my_colors[2], my_colors [1], my_colors[8] ), 
  #                   labels=c("Consensus", "Main","Bin median"))+
  #theme(legend.position = c(.13,.10), 
  #      legend.margin=margin(t = 0.0, unit='cm') )
  #guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ))
f8
  ggsave("correct_v4_3_8taxa.pdf",width=5,height = 4)
  
  
  
  
  
ftop <- ggarrange(f1, f2, labels = c("A", "B"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
fbot <- ggarrange(f3, f4, labels = c("C", "D"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
arrange <-ggarrange(ftop, NULL, fbot, labels = c(NA, NA, NA), ncol = 1, heights=c(1, 0.05, 1), nrow = 3)
arrange

ggsave("fel_four_plots_v2.pdf", width=10.0,height = 8.0, arrange)




######## Histograms for supplement


hist(bs$short)
hist(bs$long)


h1 <-ggplot(data=ts, aes(short)) + 
  geom_histogram(bins=100, color=my_colors[8], fill="white")+
  theme_classic()+
  labs(x=expression("Short branch length"), y="Frequency")+
  scale_color_grey()+
  geom_vline(aes(xintercept=mean(short)),
          color="red", linetype="dashed", size=0.4)
ggsave("hist_short_len.pdf",width=5,height = 4)


h2 <-ggplot(data=ts, aes(long)) + 
  geom_histogram(bins=100, color=my_colors[8], fill="white")+
  theme_classic()+labs(x=expression("Long branch length"), y="Frequency")+
  scale_color_grey()+
  geom_vline(aes(xintercept=mean(long)),
            color="red", linetype="dashed", size=0.4)
h2
ggsave("hist_long_len.pdf",width=5,height = 4)

ch <- ggarrange(h1, h2, labels = c("A", "B"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
ch

ggsave("fel_hist_plots.pdf", width=10.0,height = 4.0, ch)








##################old version##################


  geom_line(aes(x=bootstrap, y=(`TRUE`+`FALSE`)/1000),color="red", )+
  geom_line(aes(x=bootstrap, y=(`FALSE`)/1000),color="blue")
  #geom_line(linetype = "dashed", color = "black", data=d[d$correction %in% c("Median")])+
  geom_point()+
  geom_line(show.legend = F)+
  theme_bw()+
  scale_linetype_manual(values=c(1,1,3)) +
  #scale_linetype_manual(values=c("dashed","dotted","dashed"))+
  #scale_color_brewer(palette = "Dark2", name="",labels=c("Mean_main","Consensus", "Bin median"))+
  scale_color_manual(name="", values = c(my_colors[1], my_colors [2], my_colors[8] ), 
                     labels=c("Mean_main","Consensus", "Bin median"))+
  theme(legend.position = c(.86,.15), 
        legend.margin=margin(t = 0.0, unit='cm'))+
  labs(y= "True / True+False", x = "Support bin")
#geom_line(aes(y=(`TRUE`+`FALSE`)/nrow(d)),color="red")
#geom_line(aes(y=(`FALSE`)/nrow(d)),color="blue")

ggsave("correct_v3.pdf",width=5,height = 4)










##################previous version##################


d=read.csv('true-stats.txt',sep="\t",h=F)
head(d)

ggplot(aes(x=cut(log10(V2),10),y=cut(V1,10),fill=V3),data=d[d$V1>0.05 & d$V2 < 0.004 & d$V2 > 0.00015,])+
  geom_tile()+scale_fill_gradient2(high="black",low = "yellow",mid = "blue",midpoint = 90)

qplot(d[,]$V3)
qplot(d[d$V1<0.12& d$V1>0.05 & d$V2 < 10^(-2.4) & d$V2 > 10^(-3.8),]$V3,binwidth=15)

((3*((1+0.01)^3)+2)-(3*((1-0.01)^3)+2))/(2*0.01)



#d=read.csv('constensus6all.txt',sep="\t",h=F)
d=read.csv('500reps_try6all',sep="\t",h=F)
#d=read.csv('500reps_try6_main_mainall',sep="\t",h=F)
#d=read.csv('1000reps_try6_med_main',sep="\t",h=F)
#nrow(d)


ggplot(aes(x=bootstrap,y=(`TRUE`/(`TRUE`+`FALSE`)), group="1"), 
       data=rename_with(dcast(cut(V1,c(20,50,60,70,80,90,99,100))~grepl("(n4,n3)|(n3,n4)",V2),data=d),function(x) {"bootstrap"}, starts_with("cut")))+
  geom_line()+
  theme_bw()+
  geom_line(aes(y=(`TRUE`+`FALSE`)/nrow(d)),color="red")+
  geom_line(aes(y=(`FALSE`)/nrow(d)),color="blue")
 #ggsave("correct_mean_mean.pdf")
# ggsave("correct.pdf")
 ggsave("correct_main_main.pdf")

nrow(d)

sum(dcast(cut(V1,c(0,25,50,60,70,80,90,99,100))~V2=="(n6,((n4,n3),n5));",
           data=read.csv('500reps_try6_main_mainall',sep="\t",h=F))$`TRUE`)



d[d$V1<33,]




##################previous version##################
ggplot(aes(x=bootstrap,y=`TRUE`/(`TRUE`+`FALSE`),group="1"),data=rename_with(dcast(cut(V1,c(0,33,50,70,80,90,95,99,100))~V2=="(n6,((n4,n3),n5));",data=read.csv('500reps_try4',sep="\t",h=F)),function(x) {"bootstrap"},starts_with("cut")))+
  #geom_step()+
  geom_line()+
  geom_line(aes(y=(`TRUE`+`FALSE`)/nrow(d)),color="red")+
  theme_bw()+
  ggsave("correct.pdf")


