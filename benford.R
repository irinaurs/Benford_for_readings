##load data saved in create_hash.R
h<-readRDS("hash.rds")

## Benford
library(hash)
library(benford.analysis)
k<-keys(h)
N<-nrow(k)

## we save the TOP 2 that are not ok
max_benf1<-benford(as.numeric(h[[k[1]]]),number.of.digits=1)
max_name1<-k[1]

max_benf2<-benford(as.numeric(h[[k[2]]]),number.of.digits=1)
max_name2<-k[2]

for( i in 3:N)
{
        aux<-benford(as.numeric(h[[k[i]]]),number.of.digits=1)   
        if(aux$MAD>max_benf2$MAD)
        {
                if(aux$MAD>max_benf1$MAD)
                {
                        max_benf2<-max_benf1
                        max_name2<-max_name1
                        max_benf1<-aux
                        max_name1<-k[i]
                }
                else 
                {
                        max_benf2<-aux
                        max_name2<-k[i]       
                }
        }
}

##Keep benford results
benford_results<-array(list(9))

for( i in 1:N)
{
        #columns 1-8  keep benford object
        aux<-benford(as.numeric(h[[k[i]]]),number.of.digits=1)
        benford_results[[i]]<-aux
        #column 9 keeps the name
        benford_results[[i]][[9]]<-k[i]
}

##save the results
saveRDS(benford_results,"benford_results.rds")

## calculate a distribution for the last digit

#generate a random uniform distribution
a<-ceiling(runif(5000,-1,9))

distrib<-function(x)
{
        n<-nrow(x)
        last_digit<-array(n)
        k<-0
        for(i in 1:n)
        {
                y<-as.numeric(x[i])
                if(y>10) 
                {
                        u<-as.numeric(y%%10) 
                        k<-k+1
                        last_digit[k]<-u
                }
                
        }
        return(ks.test(last_digit,a))
}


##Keep benford results and last digit results
all_results<-array(list(10))

for( i in 1:N)
{
        aux<-benford(as.numeric(h[[k[i]]]),number.of.digits=1)
        all_results[[i]]<-aux
        #9 - tine numele cititorului
        all_results[[i]][[9]]<-k[i]
        aux2<-as.array(h[[k[i]]])
        all_results[[i]][[10]]<-distrib(aux2)
}

saveRDS(all_results,"all_results.rds")


## save triggers of fraud
for( i in 1:222)
{
        benf[i]<-all_results[[i]][[6]]
        name[i]<-all_results[[i]][[9]]
        Dunif[i]<-all_results[[i]][[10]][1]
}

