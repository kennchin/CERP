partition= function(data,sample_size,column_size,n_partition)
{
#shuffle columns
shuffle = lapply(list(data),function(x) x[,c(1,sample(2:sample_size))])  #need to make list
splitting =split_train_test(shuffle[[1]])  #list 1 splitting$train and splitting$test

counter=1	 #counter to keep track of group
a = vector(mode="list",length=n_partition)  #empty list to hold partitions for training
a_test = vector(mode="list",length=n_partition) #empty list to hold partition for test
i=1
start=2
end=37
remaining = sample_size-column_size
while(counter<=n_partition)
{
  a[[i]] = lapply(splitting$train,function(elt) elt[,c(1,start:end)]) #first 36 variables of trainset with response for each list
  a_test[[i]] = lapply(splitting$test,function(elt) elt[,c(1,start:end)])  #first 36 variables of test set
  remaining=remaining-column_size
  start=start+column_size
  end=end+column_size
  counter= counter+1
  i=i+1
}
fill_start = start
remainder = (sample_size-1)%%n_partition
while(fill_start<=sample_size)  #fill_start=3026
{
  	for (j in 1:length(splitting$train))
  	{
    	fill_start = start
    	for(l in 1:remainder)
    	{  
    		a[[l]][[j]]= cbind(a[[l]][[j]],splitting$train[[j]][fill_start])
    		a_test[[l]][[j]]= cbind(a_test[[l]][[j]],splitting$test[[j]][fill_start])
    		fill_start = fill_start + 1
    	}
  	}  
}

#swap
i <- 1:length(a)
j <- 1:length(a[[1]])
swap<-lapply(j, function(j) lapply(i, function(i) a[[i]][[j]]))

#swap_test
i_test <- 1:length(a_test)
j_test <- 1:length(a_test[[1]])
swap_test<-lapply(j_test, function(j_test) lapply(i_test, function(i_test) a_test[[i_test]][[j_test]]))

part = list(part_train=swap,part_test=swap_test)

}