Confusion_part = function(data,train,test)  
{
	n_obs = dim(data)[1]
	part_tab_ctree=numeric(n_obs)
	part_tab_rpart=numeric(n_obs)
	for (i in 1:n_obs)
	{
		q = Prediction(train[[i]],test[[i]])
		if (sum(q[,1]=="1")>sum(q[,1]=="0"))
		{
			part_tab_ctree[i]=1
		}
		else if (sum(q[,1]=="1")<sum(q[,1]=="0"))
		{
			part_tab_ctree[i]=0
		}
		if (sum(q[,2]=="1")>sum(q[,2]=="0"))
		{
			part_tab_rpart[i]=1
		}
		if (sum(q[,2]=="1")<sum(q[,2]=="0"))
		{
			part_tab_rpart[i]=0
		}
		as.numeric.factor <- function(m) {as.numeric(levels(m))[m]} 
		true = as.numeric.factor(data[,1])
		
	}
	all_table = cbind(part_tab_ctree,part_tab_rpart,true)
	Confusion(all_table)
}
