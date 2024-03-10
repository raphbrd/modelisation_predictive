#' Fit agregation of previous models
#' Agregation of a boosting model and three gam models

experts = cbind(expert1,expert2,expert3,expert4)
colnames(experts) = c("gbm","gam1","gam2","gam3")
or = oracle(Data0$Net_demand[sel_b],experts,model="convex",loss.type="square")
pinball_loss(Data0$Net_demand[sel_b],or$prediction,quant = 0.95)

# Return a pinball loss of 107, the same score as our best gam model on the validation test, all the weigth is on this model

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







