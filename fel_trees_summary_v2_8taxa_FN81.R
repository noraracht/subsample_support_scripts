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



names <- list(
  'JC'="JC69",
  'FN81'="F81"
)

hospital_labeller <- function(variable,value){
  return(names[value])
}

d2=read.csv('8leaves_FN81.txt',sep=" ",h=F)
d1=read.csv('8leaves_CONS_FN81.txt',sep=" ",h=F)
head(d2)


head(r)
#nrow(d)
#correction labes are swapped but graphs are manually corrected
d1$correction = "cons"
d2$correction = "mean_main"
d1$model = "FN81"
d2$model = "FN81"


d3=read.csv('8leaves.txt',sep=" ",h=F)
d4=read.csv('8leaves_CONS.txt',sep=" ",h=F)
d4$correction = "cons"
d3$correction = "mean_main"
d3$model = "JC"
d4$model = "JC"


###################################### Fig2 ######################################
r=rbind(d1,d2, d3, d4)
r$correction <- factor(r$correction, levels=c("cons", "mean_main"))
r$model <- factor(r$model, levels=c("JC", "FN81"))
r = r[r$V2=="unif_0.00001" | r$V2=="unif_0.00001_CONS",]

r$correct=!is.na(r$V6)
head(r)


f6<-ggplot(aes(x=V5/100,color=correction,linetype=correct),data=r)+stat_ecdf()+coord_cartesian(xlim=c(0,1))+theme_classic()+
  geom_vline(xintercept = 0.75, linetype=3,color="gray50")+
  facet_wrap(~model, labeller=hospital_labeller)+
  theme_classic()+
  scale_x_continuous(name="Support",labels=percent)+
  scale_y_continuous(name="ECDF")+
  scale_color_manual(name="", values = c(my_colors[2], my_colors [1], my_colors[8] ), 
                     labels=c("Consensus", "Main"))+
  scale_linetype_manual(name="", values = c(1, 2), 
                     labels=c("Incorrect", "Correct"))+
  theme(legend.position = c(.08,.79), 
        legend.margin=margin(t = 0.0, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ), 
         linetype = guide_legend(title = NULL, order = 2, reverse=FALSE,))
f6
ggsave("correct_v2fixed_cons_FN81vsJC.pdf",width=8,height = 4)
  

#################################################################################

quants = c(10,50,70,80,90,99, 100)

#d2
t1 = rename_with(dcast(V2+cut(V5,quants)~!is.na(V6),data=d2),function(x) {"bootstrap"}, starts_with("cut"))
#sum(is.na(d2$V6))
#t1
summary(d2$V5)
t1$correction = "mean_main"

t2 = rename_with(dcast(V2+cut(V5,quants)~!is.na(V6),data=d1),function(x) {"bootstrap"}, starts_with("cut"))
t2
t2$correction = "cons"

t1$model = "FN81"
t2$model = "FN81"



t3 = rename_with(dcast(V2+cut(V5,quants)~!is.na(V6),data=d3),function(x) {"bootstrap"}, starts_with("cut"))
t3$correction = "mean_main"

t4 = rename_with(dcast(V2+cut(V5,quants)~!is.na(V6),data=d4),function(x) {"bootstrap"}, starts_with("cut"))
t4$correction = "cons"

t3$model = "JC"
t4$model = "JC"


med_t <-data.frame(t1)
med_t3 <-data.frame(t3)


med_t$correction = "median"
med_t3$correction = "median"
dt = rbind(t1, t2, t3, t4)
#dt=t1
dt

dt$ratio = dt$`TRUE`/(dt$`TRUE`+dt$`FALSE`)*100


ratio = vector()
for (i in 2:length(quants)){
  num = seq(quants[i-1]+1, quants[i], by=1)
  #print(num)
  ratio <- c(ratio, median(num))
}
#med_labels

ratio
med_t <- cbind(med_t, ratio)
med_t
med_t3 <- cbind(med_t3, ratio)
med_t3
#names(med_t)[names(med_t) == 'med_labels'] <- 'ratio'

colnames(med_t) <- colnames(dt)
colnames(med_t3) <- colnames(dt)
dt = rbind(dt, med_t, med_t3)



dt
###################################### Fig1 ######################################
d=dt
d$correction <- factor(d$correction, levels=c("mean_main", "cons","median"))
d$model <- factor(d$model, levels=c("JC", "FN81"))
d
head(d)
#"#ef8a62", "#67a9cf"

f5<-ggplot(aes(x=bootstrap,y=ratio/100, color = correction, group=correction, linetype = correction ), 
       data=d[d$V2=="unif_0.00001" | d$V2=="unif_0.00001_CONS",])+
  facet_wrap(~model, labeller=hospital_labeller)+
  #geom_line(linetype = "dashed", color = "black", data=d[d$correction %in% c("Median")])+
  geom_point(
    aes(size=(`FALSE`+`TRUE`)),data=d[d$correction!="median" & (d$V2=="unif_0.00001" | d$V2=="unif_0.00001_CONS"),],alpha=0.5
    )+
  geom_line(show.legend = F)+
  theme_classic()+
  scale_y_continuous(labels=percent)+
  scale_linetype_manual(values=c(1,1,3)) +
  #scale_linetype_manual(values=c("dashed","dotted","dashed"))+
  #scale_color_brewer(palette = "Dark2", name="",labels=c("Mean_main","Consensus", "Bin median"))+
  scale_color_manual(name="Method", values = c(my_colors [1], my_colors[2], my_colors[8] ), 
                     labels=c( "Main   ", "Consensus", "Ideal support"))+
  theme(legend.position = c(.35,.231),
        legend.box = "horizontal",
      legend.margin=margin(t = 0.0, unit='cm'))+
  scale_size_binned_area(name="# branches",max_size = 4)+
  labs(y= "Correct / (Correct+Incorrect)", x = "Support bin")
  #geom_line(aes(y=(`TRUE`+`FALSE`)/nrow(d)),color="red")
  #geom_line(aes(y=(`FALSE`)/nrow(d)),color="blue")
f5
#ggsave("correct_mean_mean.pdf")
ggsave("correct_v1_cons_FN81vsJC.pdf",width=8,height = 4)



ggplot(aes(x=bootstrap,y=ratio/100, color = correction, group=correction, linetype = correction ), 
           data=d[d$V2=="unif_0.00001" | d$V2=="unif_0.00001_CONS",])+
  facet_wrap(~model)+
  #geom_line(linetype = "dashed", color = "black", data=d[d$correction %in% c("Median")])+
  geom_point(
    aes(size=(`FALSE`+`TRUE`)),data=d[d$correction!="median" & (d$V2=="unif_0.00001" | d$V2=="unif_0.00001_CONS"),],alpha=0.5
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



##################################################################################

###################################### Fig3 ######################################



ths=1:100
head(r)
m=merge(r,data.frame(ths=ths),by=c())
m
m$c = as.factor(with(m,ifelse(V5<ths&correct, "FN",ifelse(V5<ths&!correct, "TN", ifelse(V5>=ths&correct, "TP", ifelse(V5>=ths&!correct, "FP",NA))))))
cm = dcast(correction+model+ths~c,data=m[,c(13,14, 16,17)])

head(m)


cm
f7<-ggplot(aes(x=FP/(FP+TN),y=TP/(TP+FN),color=correction),data=cm)+
  #geom_point()+
  geom_path()+
  facet_wrap(~model, labeller=hospital_labeller)+
  theme_classic()+
  labs(y= "Recall", x = "FPR")+
  geom_text_repel(aes(label=ths),data=cm[cm$ths %% 10==0,], show.legend = F )+
  geom_point(data=cm[cm$ths %% 10==0,], )+
  #geom_text_repel(aes(label=ths),data=cm[cm$ths %% 10==0,], nudge_x = -0.015,nudge_y = 0.01, show.legend = F )+
  scale_color_manual(name="", values = c(my_colors[2], my_colors [1], my_colors[8] ), 
                     labels=c("Consensus", "Main","Bin median"))+
  theme(legend.position = c(.08,.10), 
        legend.margin=margin(t = 0.0, unit='cm') )+
  coord_cartesian(xlim = c(0,1),ylim = c(0.20,1))+
  scale_y_continuous(breaks=c(0.25,0.5,0.75,1))+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ))
f7
ggsave("correct_v3fixed_cons_FN81vsJC.pdf",width=8,height = 4)



#max.overlaps=Inf
#####################################################################################
#####################################################################################


fright <- ggarrange(f5, NULL, f6, NULL, f7, labels = c("A", NA, "B", NA, "C"), heights=c(1, 0.05, 1, 0.05, 1), ncol = 1, nrow = 5)
#fbot <- ggarrange(f3, f4, labels = c("C", "D"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
#arrange <-ggarrange(ftop, NULL, fbot, labels = c(NA, NA, NA), ncol = 1, heights=c(1, 0.05, 1), nrow = 3)
fright

ggsave("fel_FN81vsJC_three_plots.pdf", width=8.0, height = 12.1, fright)






# fbot <- ggarrange(f5, f6, f7, labels = c("D", "E", "F"), widths=c(0.5, 0.5, 0.5), ncol = 3, nrow = 1)
# ggsave("fel_8taxa.pdf", width=10.0,height = 3.5, fbot)



tail(r)


########################################################################


#data=subset(ts, ratio < 400), 
#labels = scales::number_format(accuracy = 1.0,
#                               decimal.mark = ','),

  
  
  
  
ftop <- ggarrange(f1, f2, labels = c("A", "B"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
fbot <- ggarrange(f3, f4, labels = c("C", "D"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
arrange <-ggarrange(ftop, NULL, fbot, labels = c(NA, NA, NA), ncol = 1, heights=c(1, 0.05, 1), nrow = 3)
arrange

ggsave("fel_four_plots_v2.pdf", width=10.0,height = 8.0, arrange)




