require(ggplot2); require(scales); require(reshape2); require(Hmisc)
library("readxl")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



require(Hmisc)


###################


#compute Dirichlet distribution
#install.packages("DirichletReg")
library("DirichletReg")
N <- 1000
stationary <-round(rdirichlet(1, alpha = c(36, 26, 28, 32)), 2)
#A, C, G, T


#write.csv(stationary, file = "stationary.csv", row.names = FALSE)

#transition <-round(rdirichlet(1, alpha = c(16,3,5,5,6,15)), 2)
#Dirichlet(16,3,5,5,6,15)
#CT, AT, GT, AC, GC, GA
#a, b, c, d, e, f


#vals <- round(rexp(N, rate=1.2 ), 2)
#vals
#with(vals> 0.1)


#[submodel]  GTR 0.2 0.4 0.6 0.8 1.2	//  GTR: a=0.2, b=0.4, c=0.6, d=0.8, e=1.2, f=1
#[statefreq] 0.1 0.2 0.3 0.4        	//  pi_T=0.1, pi_C=0.2, pi_A=0.3, pi_G=0.4

#file_list <- list.files(path=getwd(), pattern='control_.*.txt')
#file_list
#length(file_list)
#i=1
#i in 1:(length(file_list)-499)

#file_list = c()
#file_list <-list.files(path="/Users/admin/Documents/support/fel_simulations_indelible_v2_0.0001_0.001/original_good_500", pattern='control_.*.txt', full.names = TRUE)
#file_list


#### balanced ####

i=1
count = 120
leaves = 15
step=0


#vals = round(rexp(15, rate=40), 8)
#try 1
vals = round(rexp(leaves*count, rate=50), 8) 

#try 2
#vals = round(exp(runif(leaves*count,log(0.0001),log(0.12))), 8) 
vals
#qplot(round(rexp(15*200, rate=50),7),log="x")
#qplot(exp(runif(15*200,log(0.0001),log(0.12))),log="x") # modify 1/3-1/2 branches are challending

#summary((exp(runif(15*200,log(0.0001),log(0.12)))))
#hist((exp(runif(15*200,log(0.0001),log(0.12)))))







for (i in 1:count) {
  print(i)
  
  fi="control.txt"
  con <- file(fi)
  #print(con)
  txx=readLines(con)
  
  
  #(((A:0.1,B:0.1):0.1,(C:0.1,D:0.1):0.1):0.1,((E:0.1,F:0.1):0.1,(G:0.1,H:0.1):0.1):0.1);
  
  tmp = sprintf("(((n1:%.8f,n2:%.8f):%.8f,(n3:%.8f,n4:%.8f):%.8f):%.8f,((n5:%.8f,n6:%.8f):%.8f,(n7:%.8f,n8:%.8f):%.8f):%.8f);", 
                vals[1+step], vals[2+step], vals[3+step], vals[4+step], vals[5+step], vals[6+step], vals[7+step], vals[8+step],vals[9+step], vals[10+step], vals[11+step], vals[12+step], vals[13+step], vals[14+step])
  print(tmp)
  
  tx  = gsub(pattern="n3 n4 n5 n6", replace = tmp , x=txx)
  
  #tx2 <- paste(seq_along(tx), sub("n3:n4:n5:n6", tmp, tx))
  #tx2
  #print(tx2)
  
  
  #stationary probabilities
  N <- 1
  #stationary <-round(rdirichlet(1, alpha = c(36, 26, 28, 32)), 2) #A, C, G, T
  stationary <-round(rdirichlet(1, alpha = c(19, 14, 14, 19)), 2)  # ruminants
  stationary
  #pi(A): pi(C): pi(G): pi(T): 
  #  19     14     14     19
  
  pi_T = stationary[4]
  pi_C = stationary[2]
  pi_A = stationary[1]
  pi_G = stationary[3]
  pi_G = 1 - (pi_T+pi_C+pi_A)
  
  tmp = sprintf("%s %s %s %s", pi_T, pi_C, pi_A, pi_G)
  #tmp
  tx2 <- gsub(pattern="0.26 0.21 0.24 0.29", replace = tmp , x=tx)
  #print(tx2)
  
  
  # transition probabilities
  
  #transition <-rdirichlet(1, alpha = c(16,3,5,5,6,15))
  #transition <-rdirichlet(1, alpha = c(116,20,28,35,47,129)) #simphy paper switch to this one
  transition <-rdirichlet(1, alpha = c(50,7,12,12,14,50)) # ruminants
  #A C: A G: A T: C G: C T: G T: 
  #  12   50    7   14   50   12
  
  #Dirichlet(16,3,5,5,6,15)
  #CT, AT, GT, AC, GC, GA
  #a, b, c, d, e, f
  f=transition[6]             # G>A
  a=round(transition[1]/f, 2) # C>T
  b=round(transition[2]/f, 2) # A>T
  c=round(transition[3]/f, 2) # G>T
  d=round(transition[4]/f, 2) # A>C
  e=round(transition[5]/f, 2) # G>C
  
  tmp = sprintf("GTR %s %s %s %s %s", a, b, c, d, e)
  
  tx3  = gsub(pattern="GTR 1.0 1.0 1.0 1.0 1.0", replace = tmp , x=tx2)
  
  
  
  #alpha
  alpha = 0.0
  while (alpha < 0.2){
    #vals <- round(rexp(N, rate=1.2 ), 2)
    #b<-runif(1, min = 0, max = 1)
    #b2 <- (-log(b))/1.2
    #b2 = rexp(n = 1,rate=log(2)/9.2)
    #b2 = rlnorm(n = 11,meanlog= log(12.86715), sdlog=log(1.3))
    
    #Alpha: 
    b2 = rlnorm(n=1,meanlog=log(12.86715-(log(1.2)^2)/2),sdlog =log(1.2))
    
    
    alpha = round(b2, 2)
  }
  
  #1/var(rgamma(n=1000,shape=9.6,rate=5.5)/(9.6/5.5)) Fig.3 of https://genome.cshlp.org/content/17/12/1932/F3.expansion.html
  #qplot(rexp(n = 1000,rate=log(2)/9.2))
  #summary(rexp(n = 1000,rate=log(2)/9.2))
  
  tmp = sprintf("f=1\n  [rates]     0 %s 0", alpha)
  #print(tmp)
  tx4  = gsub(pattern="f=1", replace = tmp , x=tx3)
  
  rnum <- sample(1:2147483647, 1)
  tmp = sprintf("%s", rnum)
  tx5  = gsub(pattern="5410", replace = tmp , x=tx4)
  
  
  #write to file
  fname = sprintf("balanced_dif/control_%s.txt", i)
  #print(fname)
  con2 = file(fname)
  writeLines(tx5, con2)
  
  
  close(con)
  close(con2)
  i=i+1
  step=step+leaves
  
  
  
}







# caterpillar dif
i=1
count = 120
leaves = 14
step=0


#vals = round(rexp(15, rate=40), 8)
#try 1
vals = round(rexp(leaves*count, rate=50), 8) 

#try 2
#vals = round(exp(runif(leaves*count,log(0.0001),log(0.12))), 8) 
vals
#qplot(round(rexp(15*200, rate=50),7),log="x")
#qplot(exp(runif(15*200,log(0.0001),log(0.12))),log="x") # modify 1/3-1/2 branches are challending

#summary((exp(runif(15*200,log(0.0001),log(0.12)))))
#hist((exp(runif(15*200,log(0.0001),log(0.12)))))


for (i in 1:count) {
  print(i)
  fi="control.txt"
  con <- file(fi)
  #print(con)
  txx=readLines(con)
  
  
  #(A:0.1,(B:0.1,(C:0.1,(D:0.1,(E:0.1,(F:0.1,(G:0.1,H:0.1):0.1):0.1):0.1):0.1):0.1):0.1);
  
  tmp = sprintf("(n1:%.8f,(n2:%.8f,(n3:%.8f,(n4:%.8f,(n5:%.8f,(n6:%.8f,(n7:%.8f,n8:%.8f):%.8f):%.8f):%.8f):%.8f):%.8f):%.8f);", 
                vals[1+step], vals[2+step], vals[3+step], vals[4+step], vals[5+step], vals[6+step], vals[7+step], vals[8+step],vals[9+step], vals[10+step], vals[11+step], vals[12+step], vals[13+step], vals[14+step])
  print(tmp)
  
  tx  = gsub(pattern="n3 n4 n5 n6", replace = tmp , x=txx)
  
  #tx2 <- paste(seq_along(tx), sub("n3:n4:n5:n6", tmp, tx))
  #tx2
  #print(tx2)
  
  
  #stationary probabilities
  N <- 1
  #stationary <-round(rdirichlet(1, alpha = c(36, 26, 28, 32)), 2) #A, C, G, T
  stationary <-round(rdirichlet(1, alpha = c(19, 14, 14, 19)), 2)  # ruminants
  stationary
  #pi(A): pi(C): pi(G): pi(T): 
  #  19     14     14     19
  
  pi_T = stationary[4]
  pi_C = stationary[2]
  pi_A = stationary[1]
  pi_G = stationary[3]
  pi_G = 1 - (pi_T+pi_C+pi_A)
  
  tmp = sprintf("%s %s %s %s", pi_T, pi_C, pi_A, pi_G)
  #tmp
  tx2 <- gsub(pattern="0.26 0.21 0.24 0.29", replace = tmp , x=tx)
  #print(tx2)
  
  
  # transition probabilities
  
  #transition <-rdirichlet(1, alpha = c(16,3,5,5,6,15))
  #transition <-rdirichlet(1, alpha = c(116,20,28,35,47,129)) #simphy paper switch to this one
  transition <-rdirichlet(1, alpha = c(50,7,12,12,14,50)) # ruminants
  #A C: A G: A T: C G: C T: G T: 
  #  12   50    7   14   50   12
  
  #Dirichlet(16,3,5,5,6,15)
  #CT, AT, GT, AC, GC, GA
  #a, b, c, d, e, f
  f=transition[6]             # G>A
  a=round(transition[1]/f, 2) # C>T
  b=round(transition[2]/f, 2) # A>T
  c=round(transition[3]/f, 2) # G>T
  d=round(transition[4]/f, 2) # A>C
  e=round(transition[5]/f, 2) # G>C
  
  tmp = sprintf("GTR %s %s %s %s %s", a, b, c, d, e)
  
  tx3  = gsub(pattern="GTR 1.0 1.0 1.0 1.0 1.0", replace = tmp , x=tx2)
  
  
  
  #alpha
  alpha = 0.0
  while (alpha < 0.2){
    #vals <- round(rexp(N, rate=1.2 ), 2)
    #b<-runif(1, min = 0, max = 1)
    #b2 <- (-log(b))/1.2
    #b2 = rexp(n = 1,rate=log(2)/9.2)
    #b2 = rlnorm(n = 11,meanlog= log(12.86715), sdlog=log(1.3))
    
    #Alpha: 
    b2 = rlnorm(n=1,meanlog=log(12.86715-(log(1.2)^2)/2),sdlog =log(1.2))
    
    
    alpha = round(b2, 2)
  }
  
  #1/var(rgamma(n=1000,shape=9.6,rate=5.5)/(9.6/5.5)) Fig.3 of https://genome.cshlp.org/content/17/12/1932/F3.expansion.html
  #qplot(rexp(n = 1000,rate=log(2)/9.2))
  #summary(rexp(n = 1000,rate=log(2)/9.2))
  
  tmp = sprintf("f=1\n  [rates]     0 %s 0", alpha)
  #print(tmp)
  tx4  = gsub(pattern="f=1", replace = tmp , x=tx3)
  
  
  rnum <- sample(1:2147483647, 1)
  tmp = sprintf("%s", rnum)
  tx5  = gsub(pattern="5410", replace = tmp , x=tx4)
  
  
  
  #write to file
  fname = sprintf("caterpillar_dif/control_%s.txt", i)
  #print(fname)
  con2 = file(fname)
  writeLines(tx5, con2)
  
  
  close(con)
  close(con2)
  i=i+1
  step=step+leaves
  
  
}


 

  
 qplot(round(rexp(15*200, rate=50),7),log="x")

 qplot(exp(runif(15*200,log(0.0001),log(0.12))),log="x") # modify 1/3-1/2 branches are challending

 qplot(exp(runif(15*200,log(0.0001),log(0.12))))
summary(exp(runif(15*200,log(0.0001),log(0.12))))
         
         