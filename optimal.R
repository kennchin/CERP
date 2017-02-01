optimal_tree=function(x)
{
	mycontrol = rpart.control(minsplit=5, xval = 10)  #control for rpart
	compute_model = list(
  	c.tree = function(x) ctree(x$y~.,data=x),
  	r.part = function(x) {
  				fit = rpart(x$y~.,data=x,cp=0,control=mycontrol)
  				minimum_xerror = fit$cptable[which.min(fit$cptable[,"xerror"]),"xerror"] #find absolute minimum xerror
    				minimum_xerror_sd = fit$cptable[which.min(fit$cptable[,"xerror"]),"xstd"] #find absolute minimum xerror std
    				minimum_xerror_split = fit$cptable[which.min(fit$cptable[,"xerror"]),"nsplit"] #find the number split associated with minimum xerror
    				target_error= minimum_xerror+minimum_xerror_sd   #calculate the target error
   			    #if goes to root node, use absolute minimim xerror as optimal tree
   				if (target_error>=fit$cptable[which.min(fit$cptable[,"nsplit"]),"xerror"])
    				{ 
   						alpha = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
    					prunedtree = prune(fit,cp=alpha)   					
    				}
				else
    				{
    					
    					#finding the subset of trees within one standard deviation of target error if it doesn't go to root node
    					subtrees=fit$cptable[fit$cptable[,"nsplit"]>0&fit$cptable[,"nsplit"]<=minimum_xerror_split&fit$cptable[,"xerror"]<target_error,,drop=FALSE] 		
    					
    					#if the xerror falls within an interval and upper limit doesn't go to zero root, find the geometic mean
    					
    					if((fit$cptable[which(fit$cptable[,"nsplit"]==subtrees[1,"nsplit"])-1,"nsplit"]!=0))
  						{
  							lower_sub = as.numeric(subtrees[1,1]) #lower 
  							upper_index = as.numeric(which(fit$cptable[,"CP"]==subtrees[1,"CP"])-1)
  							upper_sub = as.numeric(fit$cptable[upper_index,"CP"]) #upper limit
    						alpha = as.numeric(sqrt(as.numeric(lower_sub)*as.numeric(upper_sub)))
    						#print(alpha)
    						prunedtree = prune(fit,cp=alpha) 
    						#print(alpha)	
    					}
    					#if none of the above condition is met, use the largest xerror below the target error
    					else
    					{
    						
    						alpha = subtrees[1,"CP"]
    						prunedtree = prune(fit,cp=alpha) 

    					}
    				}

  			}
		)
	lapply(compute_model,function(f) f(x))  #compute both ctree and rpart simultaneously
} 

