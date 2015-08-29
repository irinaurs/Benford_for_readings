## Import libraries
library(hash)
library(stringr)

## Read data
all_data<-read.csv("x.csv")
all_data<-all_data[,]

## initialize hash
h<-hash()
n=nrow(all_data)


##create hash
for(i in 1:n)
{
        aux<-paste(str_trim(all_data[i,1]))
        if(has.key(aux,h)==TRUE)
        {
                aux_table<-h[[aux]]
                aux_table<-rbind(aux_table,all_data[i,2])
                h[[aux]]<-aux_table
        }
        else 
        {
                h[[aux]]<-all_data[i,2]
        }    
        if(i %% 1000==0)print(i)
}

saveRDS(h,"hash2.rds")

keys(h)




