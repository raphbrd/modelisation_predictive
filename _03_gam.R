#' Fit generalized additive model (GAM)
#' 
#' 

## First models ####

equation <- Net_demand ~ WeekDays + 
  s(Temp, k = 10, bs="cr") + 
  s(toy, k = 30, bs="cc") + 
  s(Load.1, bs='cr', k = 15)+ 
  s(Load.7, bs='cr') + 
  s(Wind_weighted, k = 10, bs = "cr") + # BH + 
  te(as.numeric(Date), Nebulosity_weighted, k=c(4,15)) +
  s(Temp_s99,k=10, bs='cr') + 
  s(Solar_power.1, bs='cr', k = 5) + + s(Wind_power.1, bs='cr', k = 5) # + tempo + stringency_index
md.gam.0 <- gam(equation, data = Data0[sel_a,])
summary(md.gam.1)
md.gam.0.forecast <- predict(md.gam.0, newdata = Data0[sel_b, ])

rmse.old(Data0[sel_b, ]$Net_demand - md.gam.0.forecast)

res <- Data0[sel_a, "Net_demand"] - md.gam.0$fitted.values
pred.quant <- md.gam.1.forecast + qnorm(p=0.95, mean=mean(res), sd=sd(res))
pinball_loss(
  y = Data0$Net_demand[sel_b],
  pred.quant,
  quant = 0.95,
  output.vect = FALSE
) # 134.67

# getting a more robust estimate of the pinball loss with cross validation
fitmod.gam <- function(eq, block, tau=0.5)
{
  mod <- gam(eq, data = Data0[-block, ])
  mod.cvpred <- predict(mod, newdata=Data0[block,])
  return(mod.cvpred)
}
block_list_D0 <- get_cv_blocks(nrow(Data0))
md.gam.1.cvpred <- lapply(block_list_D0, fitmod.gam, eq = equation) %>% unlist
rmse(y = Data0$Net_demand, ychap = md.gam.1.cvpred)
res <- Data0$Net_demand - md.gam.1.cvpred
quant <- qnorm(0.95, mean = mean(res, na.rm = TRUE), sd = sd(res, na.rm = TRUE))

pinball_loss(
  y = Data0$Net_demand[sel_b],
  md.gam.1.forecast + quant,
  quant = 0.95,
  output.vect = FALSE
) # 134.9

## Interaction analysis ####
# is the interaction Nebulosity_weighted x significant ? 
equation <- Net_demand ~ WeekDays + 
  s(Temp, k = 10, bs="cr") + 
  s(toy, k = 30, bs="cc")+ 
  s(Load.1, bs='cr', k = 15) + 
  s(Load.7, bs='cr') + 
  s(Wind_weighted, k = 10, bs = "cr") +
  s(Time, k = 4, bs = "cr")  +
  s(Nebulosity_weighted, k = 10, bs = "cr")  +
  s(Temp_s99,k=10, bs='cr') + 
  s(Solar_power.1, bs='cr', k = 5) +
  s(Wind_power.1, bs='cr', k = 5)
md.gam.1 <- gam(equation, data = rbind(train_data, val_data))
summary(md.gam.1)

equation <- Net_demand ~ WeekDays + 
  s(Temp, k = 10, bs="cr") + 
  s(toy, k = 30, bs="cc")+ 
  s(Load.1, bs='cr', k = 15) + 
  s(Load.7, bs='cr') + 
  s(Wind_weighted, k = 10, bs = "cr") +
  te(Time, Nebulosity_weighted, k=c(4,10)) +
  s(Temp_s99,k=10, bs='cr') + 
  s(Solar_power.1, bs='cr', k = 5) +
  s(Wind_power.1, bs='cr', k = 5)
md.gam.1_bis <- gam(equation, data = rbind(train_data, val_data))
summary(md.gam.1_bis)

# Adding the interaction lower a bit the GCV score
md.gam.1$gcv.ubre%>%sqrt
md.gam.1_bis$gcv.ubre%>%sqrt

# Nebulosity weighted has a significant interaction with the numerical Date
gam1.forecast <- predict(gam1, newdata = Data0[sel_b, ])

# is the interaction Wind_weighted x significant ? 
equation <- Net_demand ~ WeekDays + 
  s(Temp, k = 10, bs="cr") + 
  s(toy, k = 30, bs="cc")+ 
  s(Load.1, bs='cr', k = 15) + 
  s(Load.7, bs='cr') + 
  s(Wind_weighted, k = 10, bs = "cr") +
  s(Time, k = 4, bs = "cr")  +
  te(Time, Nebulosity_weighted, k=c(4,10)) +
  s(Temp_s99,k=10, bs='cr') + 
  s(Solar_power.1, bs='cr', k = 5) +
  s(Wind_power.1, bs='cr', k = 5)
md.gam.2 <- gam(equation, data = rbind(train_data, val_data))
summary(md.gam.1)

equation <- Net_demand ~ WeekDays + 
  s(Temp, k = 10, bs="cr") + 
  s(toy, k = 30, bs="cc")+ 
  s(Load.1, bs='cr', k = 15) + 
  s(Load.7, bs='cr') + 
  te(Time, Wind_weighted, k=c(4,10)) +
  te(Time, Nebulosity_weighted, k=c(4,10)) +
  s(Temp_s99,k=10, bs='cr') + 
  s(Solar_power.1, bs='cr', k = 5) +
  s(Wind_power.1, bs='cr', k = 5)
md.gam.2_bis <- gam(equation, data = rbind(train_data, val_data))
summary(md.gam.2_bis)

# Adding the interaction considerably lowers the GCV score
md.gam.2$gcv.ubre%>%sqrt
md.gam.2_bis$gcv.ubre%>%sqrt

# Wind weighted has a significant interaction with the numerical Date
anova(md.gam.2, md.gam.2_bis, test = "Chisq")

