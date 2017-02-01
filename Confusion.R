Confusion = function(x)  #input matrix table
{
	confu_ctree = as.matrix(table(x[,1],x[,3],dnn=c("predicted","actual")))   #confusion matrix for ctree
	acc.ctree = (confu_ctree[1]+confu_ctree[4])/(dim(x)[1])
	sn.ctree = confu_ctree[4]/(confu_ctree[3]+confu_ctree[4])
	sp.ctree = confu_ctree[1]/(confu_ctree[1]+confu_ctree[2])
	ppv.ctree = confu_ctree[4]/(confu_ctree[2]+confu_ctree[4])
	npv.ctree = confu_ctree[1]/(confu_ctree[1]+confu_ctree[3])

	confu_rpart = as.matrix(table(x[,2],x[,3],dnn=c("predicted","actual")))   #confusion matrix for rpart
	acc.rpart = (confu_rpart[1]+confu_rpart[4])/(dim(x)[1])
	sn.rpart = confu_rpart[4]/(confu_rpart[3]+confu_rpart[4])
	sp.rpart = confu_rpart[1]/(confu_rpart[1]+confu_rpart[2])
	ppv.rpart = confu_rpart[4]/(confu_rpart[2]+confu_rpart[4])
	npv.rpart = confu_rpart[1]/(confu_rpart[1]+confu_rpart[3])
	sum_stat = matrix(c(acc.ctree,acc.rpart,sn.ctree,sn.rpart,sp.ctree,sp.rpart,ppv.ctree,ppv.rpart,npv.ctree,npv.rpart),nrow=2,ncol=5)
	colnames(sum_stat)=c("accuracy","sensitivity","specificity","ppv","npv")
	rownames(sum_stat)=c("ctree","rpart")
	b = cbind(confu_ctree,confu_rpart)
	row_names=c("predicted0","predicted1")
	col_names=c("ctree.actual0","ctree.actual1","r.part.actual0","rpart.actual1")
	dimnames(b)=list(row_names,col_names)
	print(b)
	print(sum_stat)
}