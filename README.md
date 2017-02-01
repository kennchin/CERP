# CERP
Classification by Ensemble Random Partition

Classifying high genomic data using CART tree.
Two decision tree models are used:
- CART tree in which it uses greedy algorithm to fully grows tree and then prune based on cross validation criteria
- Ctree is a conditional regression tree in which it performs recursive partition based on hypothesis testing.
