#' Fit various linear model on the training data
#' Then, evaluate the performance with respect to a set of criteria : RMSE,
#' pinball loss, CV, etc.

## First model : linear regression ####

# Main steps of a model evaluation for forecasting this time series
# 1. Fit the model on the train data (train_data)
# 2. Compute RMSE on the validation data (val_data)
# 3. Perform 8-fold cross-validation on the whole train-validation dataset
# 4. Compute 0.95 quantiles of the residuals
# 5. Compute the pinball loss, the lower the better !

mod0 <- lm(Net_demand ~ WeekDays + Temp, data = train_data2)
summary(mod0)

mod0.forecast <- predict(mod0, newdata = val_data2)
rmse(y = val_data2$Net_demand, ychap = mod0.forecast)  # 5238

mod0.cvpred <- lapply(
  block_list, 
  fit.lm, 
  eq = "Net_demand ~ WeekDays",
  df = rbind(train_data, val_data)
) %>% unlist

res <- train_data2$Net_demand - mod0$fitted.values
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pinball_loss(
  val_data2$Net_demand,
  mod0.forecast + quant,
  quant = 0.95,
  output.vect = FALSE
)

## Second model : quantile regression ####

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

## Third model : using sin/cosine cycles ####

