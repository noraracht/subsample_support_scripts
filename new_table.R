require(ggplot2); require(scales); require(reshape2); require(Hmisc)
library("readxl")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#d = read.csv('total_var.csv')
d = read.csv('dros_masking_summary.csv')

require(Hmisc)

latex(format(read.csv('dros_masking_summary.csv')),file="dros_sum.tex",rowname=NULL)

###################
ld = d[,c(3,8:11,2,17)]
ld
names(ld) <- c("sample", "ScientificName", "prct_plastid_reads",	"getorg_beffilt_contig_len",	"getorg_affilt_contig_len",	"stdspades_affilt_contig_len")

#latex(round(ld,3),file = "parameter-setting.tex", rowname = NULL)

latex(file = "parameter-setting.tex", rowname = NULL)

#compute Dirichlet distribution
#install.packages("DirichletReg")
library("DirichletReg")
rdirichlet(1, alpha = c(36, 26, 28, 32))
#A, C, G, T

round(rdirichlet(10, alpha = c(16,3,5,5,6,15)), 2)
#Dirichlet(16,3,5,5,6,15)
#CT, AT, GT, AC, GC, GA

((4000000^(9/10))/(4000000))^0.5

1288972/4

