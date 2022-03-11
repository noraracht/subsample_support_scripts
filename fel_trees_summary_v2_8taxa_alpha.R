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


# Read support
d1=read.csv('8leaves_CONS.txt',sep=" ",h=F)
d2=read.csv('8leaves.txt',sep=" ",h=F)
head(d2)



#nrow(d)
#correction labes are swapped but graphs are manually corrected
d1$correction = "cons"
d2$correction = "mean_main"


# Read alphas
s2=read.csv('alpha_caterpillar_unif_0.00001_form.txt',sep=" ",h=F)
s1=read.csv('alpha_balanced_unif_0.00001_form.txt',sep=" ",h=F)
names(s1)[names(s1) == 'V1'] <- 'V3'
names(s1)[names(s1) == 'V2'] <- 'V13'
names(s2)[names(s2) == 'V1'] <- 'V3'
names(s2)[names(s2) == 'V2'] <- 'V13'


# Append alpha values d1
d1_bal <- d1[d1$V1 == "balanced", ]
d1_cat <- d1[d1$V1 == "cat", ]

out1 <- merge(d1_bal, s1, by.x = c("V3"), by.y = c("V3"))
out2 <- merge(d1_cat, s2, by.x = c("V3"), by.y = c("V3"))

d1 <- rbind(out1, out2)



# Append alpha values d2
d2_bal <- d2[d2$V1 == "balanced", ]
d2_cat <- d2[d2$V1 == "cat", ]

out1 <- merge(d2_bal, s1, by.x = c("V3"), by.y = c("V3"))
out2 <- merge(d2_cat, s2, by.x = c("V3"), by.y = c("V3"))

d2 <- rbind(out1, out2)


tail(d2)

#d1[d1$V1 %in% c("balanced")]
###################################### Fig1 ######################################
r=rbind(d1,d2)
r$correction <- factor(r$correction, levels=c("cons", "mean_main"))
r = r[r$V2=="unif_0.00001" | r$V2=="unif_0.00001_CONS",]

r$correct=!is.na(r$V6)
head(r)

r$correction_sort = factor(r$correction, levels=c('mean_main','cons'))
names <- list(
  'mean_main'="Main",
  'cons'="Consensus"
)

hospital_labeller <- function(variable,value){
  return(names[value])
}



r$bins <- cut(r$V13,breaks = 10)
# Points:
ggplot(r, aes(x = bins, y = V5, color=correction)) +
  #stat_summary(fun = "mean", geom = "linerange")+
  geom_point()


r

a1 <-ggplot(data = r,aes( color = correct,
  x = cut(V13,c(min(V13)-0.001,quantile(V13,probs=(1:10)/10))), 
                             y = V5/100)) +
  geom_violin(draw_quantiles = c(1/4,1/2,3/4))+
  #geom_pointrange(stat = "summary",
  #                fun.ymin = min,
  #                fun.ymax = max,
  #                fun.y = "median",
  #                alpha = 0.6)+
  theme_classic()+
  facet_wrap(~correction, labeller=hospital_labeller,ncol=2)+
  scale_x_discrete(name="Heterogeneity rate")+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1))+
  scale_y_continuous(name="Support", label=percent)+
  stat_summary(position = position_dodge(width = 0.8))+
  scale_color_brewer(palette = "Dark2",labels=c("Incorrect","Correct"),name="")+
  theme(legend.position = c(.18,.14), 
        legend.margin=margin(t = 0.0, unit='cm') )+

a1


ggsave("alpha_vs_support.pdf",width=5,height = 4)

#################################################################################
r
ux <- unique(r$V3)
length(ux)

new_r <- data.frame(matrix(ncol = ncol(r)+2, nrow = 0))

for(i in ux){
  
  tmp <- r[r$V3 == i, ]
  
  tmp_bal <- tmp[tmp$V1 == "balanced", ]
  tmp_cat <- tmp[tmp$V1 == "cat", ]
  
  tmp_bal_cons <- tmp_bal[tmp_bal$correction == "cons", ]
  tmp_bal_mean <- tmp_bal[tmp_bal$correction == "mean_main", ]
  
  tmp_cat_cons <- tmp_cat[tmp_cat$correction == "cons", ]
  tmp_cat_mean <- tmp_cat[tmp_cat$correction == "mean_main", ]
  
  tmp_cat_mean$errors = sum(is.na(tmp_cat_mean$V6))
  tmp_cat_mean$correct_br=sum(!is.na(tmp_cat_mean$V6))
  
  tmp_bal_mean$errors = sum(is.na(tmp_bal_mean$V6))
  tmp_bal_mean$correct_br=sum(!is.na(tmp_bal_mean$V6))
  
  tmp_cat_cons$errors = sum(is.na(tmp_cat_cons$V6))
  tmp_cat_cons$correct_br=sum(!is.na(tmp_cat_cons$V6))
  
  tmp_bal_cons$errors = sum(is.na(tmp_bal_cons$V6))
  tmp_bal_cons$correct_br=sum(!is.na(tmp_bal_cons$V6))
  
  combo = rbind(tmp_cat_mean, tmp_bal_mean, tmp_cat_cons, tmp_bal_cons)
  {print(combo)}
  new_r <- rbind(new_r,combo)
  #print(nrow(new_r))
  
 
  }
    

new_r

#write.csv(new_r, "data.csv", row.names=FALSE)

new_r$correction_sort = factor(new_r$correction, levels=c('mean_main','cons'))
names <- list(
  'mean_main'="Main",
  'cons'="Consensus"
)

hospital_labeller <- function(variable,value){
  return(names[value])
}



# updated version
a2 <-ggplot(data = new_r,aes(x = cut(V13,c(min(V13)-0.001,quantile(V13,probs=(1:10)/10))), 
                        y = correct_br/5)) +
  geom_violin(draw_quantiles = c(1/4,1/2,3/4))+
  #geom_pointrange(stat = "summary",
  #                #fun.ymin = min,
  #                #fun.ymax = max,
  #                #fun.y = "median",
  #                alpha = 0.6)+
  theme_classic()+
  facet_wrap(~correction_sort, labeller=hospital_labeller)+
  scale_x_discrete(name="Heterogeneity rate")+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1))+
  scale_y_continuous(name="Correct branches")
  #scale_color_manual(name="", values = c(my_colors[2], my_colors [1] ), 
  #                   labels=c("Consensus", "Main" ))+
  #theme(legend.position = c(.88,.90), 
  #      legend.margin=margin(t = 0.0, unit='cm') )+
  #guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ), )
a2

ggsave("alpha_vs_correct_br.pdf",width=5,height = 4)


#################################################################################

a1 <-ggplot(data = r) +
  geom_pointrange(mapping = aes(x = bins, y = V5/100, color=correction),
                  stat = "summary",
                  #fun.ymin = min,
                  #fun.ymax = max,
                  #fun.y = "median",
                  alpha = 0.6)+
  theme_classic()+
  scale_x_discrete(name="Alpha")+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1))+
  scale_y_continuous(name="Support", label=percent)+
  scale_color_manual(name="", values = c(my_colors[2], my_colors [1] ), 
                     labels=c("Consensus", "Main" ))+
  theme(legend.position = c(.88,.90), 
        legend.margin=margin(t = 0.0, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ), )
a1




a2 <-ggplot(data = new_r) +
  geom_pointrange(mapping = aes(x = bins, y = correct_br, color=correction),
                  stat = "summary",
                  #fun.ymin = min,
                  #fun.ymax = max,
                  #fun.y = "median",
                  alpha = 0.6)+
  theme_classic()+
  scale_x_discrete(name="Alpha")+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1))+
  scale_y_continuous(name="Correct branches")+
  scale_color_manual(name="", values = c(my_colors[2], my_colors [1] ), 
                     labels=c("Consensus", "Main" ))+
  theme(legend.position = c(.88,.90), 
        legend.margin=margin(t = 0.0, unit='cm') )+
  guides(colour = guide_legend(title = NULL, order = 1, reverse=TRUE, ), )
a2

ggsave("alpha_vs_correct_br.pdf",width=5,height = 4)



#################################################################################

  
  
  
  
  
ftop <- ggarrange(f1, f2, labels = c("A", "B"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
fbot <- ggarrange(f3, f4, labels = c("C", "D"), widths=c(0.5, 0.5), ncol = 2, nrow = 1)
arrange <-ggarrange(ftop, NULL, fbot, labels = c(NA, NA, NA), ncol = 1, heights=c(1, 0.05, 1), nrow = 3)
arrange

ggsave("fel_four_plots_v2.pdf", width=10.0,height = 8.0, arrange)


