Prediction=function(x,validation)   #input as list
{
	obs = length(validation)
	o = vector(mode="list",length=obs)
	for(i in 1:obs)
	{
		o[[i]]=optimal_tree(x[[i]])
	}
	
	as.numeric.factor <- function(m) {as.numeric(levels(m))[m]}   #function to convert factor to numeric
	#obs = 30
	#obs=dim(x)[1] 
	fit.predsc=numeric(length=obs)   #initialize array to store prediction from ctree
	fit.predsr=numeric(length=obs)   #initialize array to store prediction from rpart
	actual=numeric(length=obs)		  #initialize array to store original response value from testing set
	for (i in 1:obs)
	{
 		fit.predsc[i]= as.numeric.factor(predict(o[[i]]$c.tree,newdata=validation[[i]],type="response")) #prediction for ctree
 		fit.predsr[i] = as.numeric.factor(predict(o[[i]]$r.part,newdata=validation[[i]],type="class")) #prediction for rpart
    		actual = as.numeric.factor(sapply(validation,"[[",1 ))
	}
	print(o)  #list of rpart and ctree trees
	print(cbind(fit.predsc,fit.predsr,actual))
}