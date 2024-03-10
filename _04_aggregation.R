#' Fit agregation of previous models
#' Agregation of a boosting model and three gam models

experts = cbind(expert1,expert2,expert3,expert4)
colnames(experts) = c("gbm","gam1","gam2","gam3")
or = oracle(Data0$Net_demand[sel_b],experts,model="convex",loss.type="square")
pinball_loss(Data0$Net_demand[sel_b],or$prediction,quant = 0.95)

# Return a pinball loss of 107, the same score as our best gam model on the validation test, all the weigth is on this model

# Agregation removing the best gam and the boosting model and whithout quantile prediction

experts = cbind(expert2,expert4)
colnames(experts) = c("gam1","gam3")
or = oracle(Data0$Net_demand[sel_b],experts,model="convex",loss.type="square")
pinball_loss(Data0$Net_demand[sel_b],or$prediction,quant = 0.95)

# Return a pinball loss 115 on the validation test, 0.1 weight on gam1 and 0.9 on gam3
