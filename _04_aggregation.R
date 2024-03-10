#' Fit agregation of previous models
#' Agregation of a boosting model and three gam models

#1st expert
equation <- Net_demand~  Time + toy + Temp + Net_demand.1 + Net_demand.7 + Temp_s99 + as.factor(WeekDays3) + 
            Wind_power.1 + Solar_power.1 + Wind_power.7 + Solar_power.7 + Wind_weighted + Nebulosity_weighted + 
            confinement1 + confinement2 + confinement3

Ntree <- 1000
gbm1 <- gbm(equation,distribution = "gaussian" , data = Data0[sel_a,],n.trees = Ntree, interaction.depth = 10,
            n.minobsinnode = 5, shrinkage = 0.02, bag.fraction = 0.5, train.fraction = 1,
            keep.data = FALSE, n.cores = 8)

best.iter=gbm.perf(gbm1,method="OOB", plot.it = TRUE,oobag.curve = TRUE)    
best.iter    
NTreeOpt <- which.min(-cumsum(gbm1$oobag.improve))
gbm1.forecast <- predict(gbm1,n.trees=NTreeOpt,single.tree=FALSE,newdata=Data0[sel_b,])

#2nd expert
equation <- Net_demand ~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + 
  s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr')  +
  s(Temp_s99,k=10, bs='cr') + WeekDays + s(Wind_weighted) +
  te(as.numeric(Date), Nebulosity, k=c(4,10)) + 
  s(Wind_power.1, k=10, bs='cr') + s(Solar_power.1, k=10,  bs='cr')


md.gam.1 <- gam(equation,  data=Data0[sel_a,])
md.gam.1.forecast <- predict(md.gam.1,  newdata = Data0[sel_b, ])

#3rd expert
equation = Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') +
  s(toy, k = 30, bs = 'cc') + s(Temp, k = 10, bs = 'cr') +
  s(Net_demand.1, bs = 'cr',  by = WeekDays) +
  s(Net_demand.7, bs = 'cr') +
  s(Load.1, bs = 'cr',  by = WeekDays) +
  s(Load.7, bs = 'cr') +
  s(Temp_s99, k = 10, bs = 'cr') + 
  as.factor(WeekDays) +
  as.factor(BH) +
  s(Wind_weighted) + 
  te(as.numeric(Date), Nebulosity_weighted, k = c(4, 10)) +
  s(Wind_power.1, k = 10, bs = 'cr') +
  s(Solar_power.1, k = 10,  bs = 'cr')

md.gam.2 <- gam(equation,  data=Data0[sel_a,])
md.gam.2.forecast <- predict(md.gam.2,  newdata = Data0[sel_b, ])

#4th expert
equation = Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') +
  s(toy, k = 30, bs = 'cc') + s(Temp, k = 10, bs = 'cr') +
  s(Net_demand.1, bs = 'cr',  by = WeekDays) +
  s(Net_demand.7, bs = 'cr') +
  s(Load.1, bs = 'cr',  by = WeekDays) +
  s(Load.7, bs = 'cr') +
  s(Temp_s99, k = 10, bs = 'cr') +
  WeekDays +
  s(Wind_weighted) +
  te(as.numeric(Date), Nebulosity_weighted, k = c(4, 10)) 

md.gam.3 <- gam(equation,  data=Data0[sel_a,])
md.gam.3.forecast <- predict(md.gam.3,  newdata = Data0[sel_b, ])

# Agregation

experts = cbind(gbm1.forecast,md.gam.1.forecast,md.gam.2.forecast,md.gam.3.forecast)
colnames(experts) = c("gbm","gam1","gam2","gam3")
or = oracle(Data0$Net_demand[sel_b],experts,model="convex",loss.type="square")
pinball_loss(Data0$Net_demand[sel_b],or$prediction,quant = 0.95)

# Return a pinball loss of 107, the same score as our best gam model on the validation test, all the weigth is on this model (gam2)

# Agregation removing the best gam and the boosting model and whithout quantile prediction

equation1 <- Net_demand ~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + 
  s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr')  +
  s(Temp_s99,k=10, bs='cr') + WeekDays + s(Wind_weighted) +
  te(as.numeric(Date), Nebulosity, k=c(4,10)) + 
  s(Wind_power.1, k=10, bs='cr') + s(Solar_power.1, k=10,  bs='cr')


md.gam.1 <- gam(equation1,  data=Data0[sel_a,])
md.gam.1.forecast <- predict(md.gam.1,  newdata = Data0[sel_b, ])


equation3 = Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') +
  s(toy, k = 30, bs = 'cc') + s(Temp, k = 10, bs = 'cr') +
  s(Net_demand.1, bs = 'cr',  by = WeekDays) +
  s(Net_demand.7, bs = 'cr') +
  s(Load.1, bs = 'cr',  by = WeekDays) +
  s(Load.7, bs = 'cr') +
  s(Temp_s99, k = 10, bs = 'cr') +
  WeekDays +
  s(Wind_weighted) +
  te(as.numeric(Date), Nebulosity_weighted, k = c(4, 10)) 

md.gam.3 <- gam(equation3,  data=Data0[sel_a,])
md.gam.3.forecast <- predict(md.gam.3,  newdata = Data0[sel_b, ])


experts = cbind(md.gam.1.forecast,md.gam.3.forecast)
colnames(experts) = c("gam1","gam3")
or = oracle(Data0$Net_demand[sel_b],experts,model="convex",loss.type="square")
pinball_loss(Data0$Net_demand[sel_b],or$prediction,quant = 0.95)

# Return a pinball loss 115 on the validation test, 0.1 weight on gam1 and 0.9 on gam3
# Using this agregation model to predict the private set data1

md.gam.1.data1 = gam(equation1,  data=Data0)
md.gam.1.data1.forecast = predict(md.gam.1.data1, newdata=Data1)

md.gam.3.data1 = gam(equation3,  data=Data0)
md.gam.3.data1.forecast = predict(md.gam.3.data1, newdata=Data1)

experts = cbind(md.gam.1.data1.forecast,md.gam.3.data1.forecast)

agreg_gam1_gam3 = predict(or,newexpert = experts)







