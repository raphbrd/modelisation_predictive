#' Fit various linear model on the training data
#' Then, evaluate the performance with respect to a set of criteria : RMSE,
#' pinball loss, CV, etc.
#' In this script, we are working with Data0[sel_a, ] as the training data
#' and Data0[sel_b, ] as the validation data

## First model : linear regression ####

# Main steps of a model evaluation for forecasting this time series
# 1. Fit the model on the train data (train_data)
# 2. Compute RMSE on the validation data (val_data)
# 3. Perform 8-fold cross-validation on the whole train-validation dataset
# 4. Compute 0.95 quantiles of the residuals
# 5. Compute the pinball loss, the lower the better !

mod1 <- lm(Net_demand ~ WeekDays3 + Temp + Temp_trunc1 + Temp_trunc2 + stringency_index, data=Data0[sel_a,])
summary(mod1)

mod1.forecast <- predict(mod1, newdata = Data0[sel_b,])
rmse(y = Data0[sel_b,]$Net_demand, ychap = mod1.forecast)  # 3836

res <- Data0[sel_a,]$Net_demand - mod1$fitted.values
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))

pinball_loss(
  Data0[sel_b,]$Net_demand,
  mod1.forecast + quant,
  quant = 0.95,
  output.vect = FALSE
) # 391


# Cross validation on the first linear model
block_list_D0 <- get_cv_blocks(nrow(Data0))
mod1.cvpred <- lapply(
  block_list_D0, 
  fit.lm, 
  eq = "Net_demand ~ WeekDays + Temp + Temp_trunc1 + Temp_trunc2 + stringency_index + stringency_index",
  df = Data0
) %>% unlist

res <-  Data0$Net_demand - mod1.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))

pinball_loss(
  Data0$Net_demand,
  mod1.cvpred + quant,
  quant = 0.95,
  output.vect = FALSE
)

plot(Data0$Temp, Data0$Net_demand, pch=20)
Data0$Temp_trunc1 <- pmax(Data0$Temp-280,0)
Data0$Temp_trunc2 <- pmax(Data0$Temp-290,0) # plutôt 290 que 295 => améliore le modele.

plot(Data0$Temp, Data0$Temp_trunc1 , pch=20)
plot(Data0$Temp, Data0$Temp_trunc2 , pch=20)

## Second model : quantile regression ####

rq.cvpred <- lapply(
  block_list, 
  fit.mdl.cv, 
  mdl = rq,
  eq = "Net_demand ~ WeekDays + Temp + stringency_index",
  df = Data0
) %>% unlist

mod.rq <- rq(Net_demand ~ WeekDays3 + Temp + Temp_trunc1 + Temp_trunc2 + stringency_index,
              data = Data0[sel_a, ],
              tau = 0.95)
summary(mod.rq)

rq.pred <- predict(mod.rq, newdata = Data0[sel_b, ])

pinball_loss(
  Data0[sel_b, ]$Net_demand,
  rq.pred,
  quant = 0.95,
  output.vect = FALSE
) # 322

## Third model : using sin/cosine cycles ####

Nfourier <- 30
lm.fourier <- list()
eq <- list()
for (i in c(1:Nfourier))
{
  cos <- paste(c('cos'), c(1:i), sep = "")
  sin <- paste(c('sin'), c(1:i), sep = "")
  fourier <- paste(c(cos, sin), collapse = "+")
  eq[[i]] <-
    as.formula(paste(
      "Net_demand~ WeekDays3 + Temp + Temp_trunc1 + Temp_trunc2 + stringency_index +",
      fourier,
      sep = ""
    ))
  lm.fourier[[i]] <- lm(eq[[i]], data = Data0[sel_a, ])
}

fit.rmse <-
  lapply(lm.fourier, function(x) {
    rmse(Data0$Net_demand[sel_a], x$fitted)
  }) %>% unlist

forecast.rmse <-
  lapply(lm.fourier, function(x) {
    rmse(Data0$Net_demand[sel_b], predict(x, newdata = Data0[sel_b, ]))
  }) %>% unlist

par(mfrow = c(1, 1))
plot(
  fit.rmse,
  type = 'b',
  pch = 20,
  ylim = range(fit.rmse, forecast.rmse),
  col = 'royalblue2'
)
lines(
  forecast.rmse,
  type = 'b',
  pch = 20,
  col = 'orangered2'
)
legend(
  'top',
  c("fit", "forecast"),
  col = c('royalblue2', 'orangered2'),
  lty = 1
)
title(main = paste(c(
  "Min forecast RMSE is achieved by model ", which.min(forecast.rmse)
), sep = " "))

mod3 <- lm(formula(lm.fourier[[15]]), data = Data0[sel_a, ])
mod3.cvpred <-
  lapply(block_list_D0, fit.lm, eq = formula(lm.fourier[[15]]), df = Data0) %>% unlist
mod3.forecast <- predict(mod3, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand, ychap = mod3.cvpred)
rmse(y = Data0$Net_demand[sel_b], ychap = mod3.forecast) # 3196
mod4.rmse_bloc <-
  lapply(block_list_D0, function(x) {
    rmse(y = Data0$Net_demand[x], ychap = mod3.cvpred[x])
  }) %>% unlist

res <- Data0$Net_demand - mod3.cvpred
quant <- qnorm(0.95, mean= mean(res), sd= sd(res))
pinball_loss(
  y = Data0$Net_demand[sel_b],
  mod3.forecast + quant,
  quant = 0.95,
  output.vect = FALSE
) # 340


