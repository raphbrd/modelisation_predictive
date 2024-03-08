#' Fit a linear model by performing one fold of a cross-validation
#'
#' @param eq The equation of the model to evaluate
#' @param block The indexes used in the test block of the CV
#' @param df The dataframe used to fit the model
#' @returns The prediction on the test block of the CV
fit.lm <- function(eq, block)
{
  mod <- lm(eq, data = df[-block, ])
  mod.cvpred <- predict(mod, newdata = df[block, ])
  return(mod.cvpred)
}


#' Fit a model by performing one fold of a cross-validation
#'
#' Usage: mdl.cvpred <- fit.mdl.cv(gam, equation, Data0, block)
#' 
#' @param eq The equation of the model to evaluate
#' @param block The indexes used in the test block of the CV
#' @param df The dataframe used to fit the model
#' @returns The prediction on the test block of the CV
fit.mdl.cv <- function(mdl, eq, df, block)
{
  mod <- mdl(eq, data = df[-block,])
  mod.cvpred <- predict(mod, newdata = df[block,])
  res <- residuals(mod)
  quant <- qnorm(0.95,
                 mean = mean(res, na.rm = TRUE),
                 sd = sd(res, na.rm = TRUE))
  # df$residuals = NA
  # df[-block, "residuals"] = res
  # mod.rq <- rq(eq_rq, data = df[-block, ], tau = 0.95)
   #quant.rq <- predict(mod.rq, newdata = df[block,])
  print(c(
    pinball_loss(
      y = df$Net_demand[block],
      mod.cvpred + quant,
      quant = 0.95,
      output.vect = FALSE
    )
    # ),
    # pinball_loss(
    #   y = df$Net_demand[block],
    #   # mod.cvpred + quant,
    #   quant.rq,
    #   quant = 0.95,
    #   output.vect = FALSE
    # )
  ))
  return(mod.cvpred)
}


#' Get the indexes for a K-fold cross-validation
#' @param K the number of block in the cross-validation
#' @returns a list of blocks
get_cv_blocks <- function(n_datapoints, K = 8) {
  bound_indexes <- seq(1, n_datapoints, length = K + 1) %>% floor
  block_list <- list()
  l <- length(bound_indexes)
  for (i in c(2:(l - 1)))
  {
    block_list[[i - 1]] <- c(bound_indexes[i - 1]:(bound_indexes[i] - 1))
  }
  block_list[[l - 1]] <- c(bound_indexes[l - 1]:(bound_indexes[l]))
  return(block_list)
}
