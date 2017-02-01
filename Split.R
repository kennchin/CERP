split_train_test=function(x)
{
  obs=dim(x)[1]     #number of observations
  #obs=30
  train_list = vector(mode="list",length=obs)  #initialize empty list for training set
  test_list = vector(mode="list",length=obs)   #initialize empty list for test set
  for (i in 1:obs)
  {
    train_list[[i]]=x[-i,]       #train set is all the observations but one
    test_list[[i]]=x[i,]		  #test set is one observations from dataset
  }
  split = list(train=train_list,test=test_list)
} 