#' Fit generalized additive model (GAM)
#' 
#' 

## Example of some of the first models ####

equation <- Net_demand ~ WeekDays + 
  s(Temp, k = 10, bs="cr") + 
  s(toy, k = 30, bs="cc") + 
  s(Load.1, bs='cr', k = 15)+ 
  s(Load.7, bs='cr') + 
  s(Wind_weighted, k = 10, bs = "cr") +
  te(as.numeric(Date), Nebulosity_weighted, k=c(4,15)) +
  s(Temp_s99,k=10, bs='cr') + 
  s(Solar_power.1, bs='cr', k = 5) + 
  s(Wind_power.1, bs='cr', k = 5)
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

## Second models ####

equation <- Net_demand ~ s(Time, k = 3, bs = 'cr') +
  s(toy, k = 30, bs = 'cc') + 
  s(Temp, k = 10, bs = 'cr') +
  # s(Net_demand.1, bs = 'cr',  by = WeekDays) +
  # s(Net_demand.7, bs = 'cr') +
  s(Load.1, bs = 'cr',  by = WeekDays) +
  s(Load.7, bs = 'cr') +
  s(Temp_s99, k = 10, bs = 'cr') + 
  as.factor(WeekDays) +
  as.factor(BH) +
  s(Wind_weighted) + 
  te(as.numeric(Date), Nebulosity_weighted, k = c(4, 10)) +
  s(Wind_power.1, k = 10, bs = 'cr') +
  s(Solar_power.1, k = 10,  bs = 'cr')
# md.gam.11 <- gam(equation, data = Data0[sel_a,])
md.gam.11 <- gam(equation, data = train_data2)
summary(md.gam.11)
# md.gam.11.forecast <- predict(md.gam.11, newdata = Data0[sel_b,])
md.gam.11.forecast <- predict(md.gam.11, newdata = val_data2)
res <- Data0[sel_b, "Net_demand"] - md.gam.11.forecast
block_list_D0 <- get_cv_blocks(nrow(Data0))
md.gam.1.cvpred <- lapply(block_list_D0, fitmod.gam, eq = equation) %>% unlist
rmse(y = Data0$Net_demand, ychap = md.gam.1.cvpred) # 1513
res <- Data0$Net_demand - md.gam.1.cvpred
md.gam.11$gcv.ubre%>%sqrt # 1055.3

res <- val_data2$Net_demand - md.gam.11.forecast
quant <- qnorm(0.95, mean = mean(res, na.rm = TRUE), sd = sd(res, na.rm = TRUE))

pinball_loss(
  y = val_data2$Net_demand,
  md.gam.11.forecast + quant,
  quant = 0.95,
  output.vect = FALSE
) # 132

pinball_loss(
  y = val_data2$Net_demand,
  md.gam.11.forecast,
  quant = 0.95,
  output.vect = FALSE
) # 442 without the VC

# soumission
md.gam.11 <- gam(equation, data = Data0_clean)
gam.forecast <- predict(md.gam.11, newdata = Data1)
block_list_D0 <- get_cv_blocks(nrow(Data0_clean))
md.gam.11.cvpred <- lapply(block_list_D0, fitmod.gam, eq = equation) %>% unlist
res <- Data0_clean$Net_demand - md.gam.11.cvpred
quant <- qnorm(0.95,
               mean = mean(res, na.rm = TRUE),
               sd = sd(res, na.rm = TRUE))
hist(res)
submit <- read_delim( file="data/sample_submission.csv", delim=",")
submit$Net_demand <- gam.forecast + quant
write.table(
  submit,
  file = "data/submission_gam_report_gam_quant.csv",
  quote = F,
  sep = ",",
  dec = '.',
  row.names = F
) # 559

submit$Net_demand <- gam.forecast
write.table(
  submit,
  file = "data/submission_gam_report_gam_without_quant.csv",
  quote = F,
  sep = ",",
  dec = '.',
  row.names = F
) # 139 on the private

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


## GAM followed by quantile regression ####

# removing the lagged Net demand as it induces non-singular
# matrices in the quantile regression
equation <- Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') +
  s(toy, k = 30, bs = 'cc') + 
  s(Temp, k = 10, bs = 'cr') +
  # s(Net_demand.1, bs = 'cr',  by = WeekDays) +
  # s(Net_demand.7, bs = 'cr') +
  s(Load.1, bs = 'cr',  by = WeekDays) +
  s(Load.7, bs = 'cr') +
  s(Temp_s99, k = 10, bs = 'cr') + 
  as.factor(WeekDays) +
  as.factor(BH) +
  s(Wind_weighted) + 
  te(as.numeric(Date), Nebulosity_weighted, k = c(4, 10)) +
  s(Wind_power.1, k = 10, bs = 'cr') +
  s(Solar_power.1, k = 10,  bs = 'cr')
gam.rq.1 <- gam(equation, data = train_data2)
summary(gam.rq.1)

gam.rq.1.forecast <- predict(gam.rq.1, newdata = val_data2)
res <-gam.rq.1$residuals
par(mfrow = c(1, 1))
plot(res)
gam.rq.1$gcv.ubre%>%sqrt # 1055.3
rmse(val_data2$Net_demand, gam.rq.1.forecast) # 1184

quantile_data <- data.frame(
  residuals = gam.rq.1$residuals,
  Date = as.numeric(train_data2$Date),
  toy = train_data2$toy,
  Temp = train_data2$Temp,
  # Net_demand.1 = train_data2$Net_demand.1,
  # Net_demand.7 = train_data2$Net_demand.7,
  Load.1 = train_data2$Load.1,
  Load.7 = train_data2$Load.7,
  BH = train_data2$BH,
  Temp_s99 = train_data2$Temp_s99,
  WeekDays = train_data2$WeekDays,
  Wind_weighted = train_data2$Wind_weighted,
  Nebulosity_weighted = train_data2$Nebulosity_weighted,
  Wind_power.1 = train_data2$Wind_power.1,
  Solar_power.1 = train_data2$Solar_power.1
)

quantile_newdata <- data.frame(
  Date = as.numeric(val_data2$Date),
  toy = val_data2$toy,
  Temp = val_data2$Temp,
  Load.1 = val_data2$Load.1,
  Load.7 = val_data2$Load.7,
  # Net_demand.1 = val_data2$Net_demand.1,
  # Net_demand.7 = val_data2$Net_demand.7,
  BH = val_data2$BH,
  Temp_s99 = val_data2$Temp_s99,
  WeekDays = val_data2$WeekDays,
  Wind_weighted = val_data2$Wind_weighted,
  Nebulosity_weighted = val_data2$Nebulosity_weighted,
  Wind_power.1 = val_data2$Wind_power.1,
  Solar_power.1 = val_data2$Solar_power.1
)

res.mod.rq <- rq(
  residuals ~ Date + toy + Temp +
    BH + Load.1 + Load.7 + WeekDays + Wind_weighted +
    Nebulosity_weighted + Wind_power.1 +
    Solar_power.1,
  data = quantile_data,
  tau = 0.95,
  method = "fn"
)
summary(res.mod.rq)

quant <- qnorm(0.95, mean = mean(res, na.rm=TRUE), sd = sd(res, na.rm=TRUE))
quantnew <- predict(res.mod.rq, newdata = quantile_newdata)
pinball_loss(
  y = val_data2$Net_demand,
  gam.rq.1.forecast + quantnew, #quantile(mod.rq$fitted.values, 0.95),
  quant = 0.95,
  output.vect = FALSE
) # 107.6

pinball_loss(
  y = val_data2$Net_demand,
  gam.rq.1.forecast + quant,
  quant = 0.95,
  output.vect = FALSE
) # 133.8

# Soumission
gam1 <- gam(equation, data = Data0_clean)

quantile_data <- data.frame(
  residuals = gam1$residuals,
  Date = as.numeric(Data0_clean$Date),
  toy = Data0_clean$toy,
  Temp = Data0_clean$Temp,
  Load.1 = Data0_clean$Load.1,
  Load.7 = Data0_clean$Load.7,
  BH = as.factor(Data0_clean$BH),
  Temp_s99 = Data0_clean$Temp_s99,
  WeekDays = Data0_clean$WeekDays,
  Wind_weighted = Data0_clean$Wind_weighted,
  Nebulosity_weighted = Data0_clean$Nebulosity_weighted,
  Wind_power.1 = Data0_clean$Wind_power.1,
  Solar_power.1 = Data0_clean$Solar_power.1
)


quantile_newdata <- data.frame(
  Date = as.numeric(Data1$Date),
  toy = Data1$toy,
  Temp = Data1$Temp,
  Load.1 = Data1$Load.1,
  Load.7 = Data1$Load.7,
  BH = as.factor(Data1$BH),
  Temp_s99 = Data1$Temp_s99,
  WeekDays = Data1$WeekDays,
  Wind_weighted = Data1$Wind_weighted,
  Nebulosity_weighted = Data1$Nebulosity_weighted,
  Wind_power.1 = Data1$Wind_power.1,
  Solar_power.1 = Data1$Solar_power.1
)

gam1.forecast <- predict(gam1, newdata = Data1)
mod.rq <- rq(residuals ~ ., data = quantile_data, tau=0.95)
quantnew <- predict(mod.rq, newdata = quantile_newdata)

submit <- read_delim( file="data/sample_submission.csv", delim=",")
submit$Net_demand <- gam1.forecast + quantnew
write.table(
  submit,
  file = "data/submission_gam_rq_report_1.csv",
  quote = F,
  sep = ",",
  dec = '.',
  row.names = F
)

## Quantile GAM ####

equation <- Net_demand ~ s(as.numeric(Date), k = 3, bs = 'cr') +
  s(toy, k = 30, bs = 'cc') +
  s(Temp_s99, k = 10, bs = "cr") +
  WeekDays +
  # s(Net_demand.1, bs = 'cr',  by = as.factor(WeekDays)) +
  # s(Net_demand.7, bs = 'cr') + 
  s(Load.1, bs = 'cr',  by = as.factor(WeekDays)) +
  s(Load.7, bs = 'cr') +
  s(Wind_weighted) + 
  te(as.numeric(Date), Nebulosity_weighted, k = c(4, 10)) +
  s(Temp, k = 10, bs = 'cr') 

# This might take a few minutes to run
gqgam <- qgam(
  equation, 
  data=train_data2, 
  qu=0.95,
  multicore = TRUE,
  ncores = 8
) # , discrete=TRUE)
summary(gqgam)
sqrt(gqgam$gcv.ubre) # 103.6969

gqgam.forecast <- predict(gqgam, newdata = val_data2)

pinball_loss(
  y = val_data2$Net_demand,
  gqgam.forecast,
  quant = 0.95,
  output.vect = FALSE
) # 105

# looking at the prediction
plot(val_data2$Net_demand, type='l')
lines(gqgam.forecast, col='red')

# soumission
gqgam.sub <- qgam(
  equation, 
  data=Data0_clean, 
  qu=0.95,
  multicore = TRUE,
  ncores = 8
)
gqam.forecast <- predict(gqgam.sub, newdata = Data1)

submit <- read_delim( file="data/sample_submission.csv", delim=",")
submit$Net_demand <- gqam.forecast
write.table(
  submit,
  file = "data/submission_gqgam_late_sub_report.csv",
  quote = F,
  sep = ",",
  dec = '.',
  row.names = F
)


