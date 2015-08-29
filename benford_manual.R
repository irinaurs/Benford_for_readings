##inporting library
library(hash)

##load data created in create_hash.R
h<-readRDS("hash2.rds")

##save random
set.seed=1

##create a uniform distribution of last digits
unif_distr<-ceiling(runif(5000,-1,9))

##import a benford distrib of first digit
##that file contains numbers that have a Benford distribution of the first digits
bd<-read.csv("x.csv")
n=nrow(bd)

benf_distr<-array(n)
for( i in 1:n)
{
        benf_distr[i]<-as.numeric(bd[i,1])
}

k<-keys(h)

##test our data for both distributions

#generate first digit
first_digits<-function(x)
{
        N<-nrow(x)
        aux_benf<-array(N)
        counter<-0
        for(i in 1:N)
        {
                if(x[i,1]>1)
                {
                        aux<-as.numeric(x[i,1])
                        while(aux>=1)
                        {
                                if(aux<10) 
                                        break
                                else aux<-aux/10
                        }
                        counter<-counter+1
                        aux_benf[counter]=floor(aux)
                }        
        }
        return(aux_benf)
}

#generate last digit
last_digits<-function(x)
{
        N<-nrow(x)
        aux_unif<-array(N)
        counter<-0
        for(i in 1:N)
        {
                if(x[i,1]>1)
                {
                        aux<-as.numeric(x[i,1])%%10
                        counter<-counter+1
                        aux_unif[counter]=floor(aux)
                }        
        }
        return(aux_unif)
}

##Test if the distributions are the same
results_benf<-array(list(6))
results_unif<-array(list(6))
for( i in 1:N)
{
      readings<-h[[k[i]]]  
      fd<-first_digits(readings)
      ld<-last_digits(readings)
      
      results_benf[[i]]<-ks.test(fd,benf_distr)
      results_benf[[i]][[6]]<-k[[i]]
      
      results_unif[[i]]<-ks.test(ld,unif_distr)
      results_unif[[i]][[6]]<-k[[i]]
}

min<-1
for(i in 1:N)
{
      if(results_benf[[i]][[2]]<min && results_benf[[i]][[2]]>0 )
      {
              min<-results_benf[[i]][[2]] 
              name<-results_benf[[i]][[6]]
      }
}


#save results
saveRDS(results_benf,"results_benf.rds")
saveRDS(results_unif,"results_unif.rds")

##save results in a readible format

results1<-matrix(N,2)
name1<-matrix(N,2)

for( i in 1:N)
{
        results1[i][1]<-as.numeric(results_benf[[i]][[2]])
        name1[i]<-results_benf[[i]][[6]]      
}

results2<-matrix(N,2)
name2<-matrix(N,2)

for( i in 1:N)
{
        results2[i][1]<-as.numeric(results_unif[[i]][[2]])
        name2[i]<-results_unif[[i]][[6]]      
}
