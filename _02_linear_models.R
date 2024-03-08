### First models : linear regression ####

# Main steps of a model evaluation for forecasting this time series
# 1. Fit the model on the train data (sel_a)
# 2. Compute RMSE on the validation data (sel_b)
# 3. Perform 8-fold cross-validation on the whole train-validation dataset
# 4. Compute 0.95 quantiles of the residuals
# 5. Compute the pinball loss, the lower the better !

mod0 <- lm(Net_demand ~ WeekDays, data = train_data)
summary(mod0)

mod0.forecast <- predict(mod0, newdata = val_data)
rmse(y = val_data$Net_demand, ychap = mod0.forecast)

# mod0.cvpred <-
#   lapply(block_list, fit.lm, eq = "Net_demand ~ WeekDays") %>% unlist

res <- train_data$Net_demand - mod0$fitted.values
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pinball_loss(
  val_data$Net_demand,
  mod0.forecast + quant,
  quant = 0.95,
  output.vect = FALSE
)

fitmod.rq <- function(eq, block, tau = 0.5)
{
  mod <- rq(eq, data = Data0[-block,], tau)
  mod.cvpred <- predict(mod, newdata = Data0[block,])
  return(mod.cvpred)
}

mod5.rq <- rq(Net_demand ~ WeekDays + Temp + stringency_index,
              data = train_data,
              tau = 0.95)
summary(mod5.rq)
