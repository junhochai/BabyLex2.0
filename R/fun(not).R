# Comparison of Web-CDI Data Collected from Amazon Mechanical Turk against Data from Aggregated CDI studies
# 2017-01-13
# Retrieved from https://rstudio-pubs-static.s3.amazonaws.com/241626_01b8451f756c4f298447a0a47e28c21a.html
# On Saturday, 14 September, 2019, 12:37PM, Malaysia

# Edited for usage purpose #

fit_gcrq <- function(x,xmean) {
  mod <- try(gcrq(formula = xmean ~ ps(age, monotone = 1,
                                      lambda = 1000),
                  tau = taus, data = x))

  if(inherits(mod, "try-error"))
  {
    return(NA)
  }

  return(mod)
}

pred_gcrq <- function(x, mods) {
  mod <- mods#[[x$language[1]]]

  if (is.na(mod[1])) {
    return(expand.grid(age = x$age,
                       #language = x$language[1],
                       percentile = as.character(taus*100),
                       pred = NA))
  } else {
    preds <- predictQR_fixed(mod,
                             newdata = x) %>%
      data.frame %>%
      mutate(age = x$age#,language = x$language
             ) %>%
      gather(percentile, pred, starts_with("X")) %>%
      mutate(percentile = as.character(as.numeric(str_replace(percentile,
                                                              "X", ""))
                                       * 100))
    return(preds)
  }
}

fit_sex <- function(x) {
  robustbase::glmrob(cbind(production, no_production) ~ age * sex - sex,
                     family = "binomial",
                     data = x)
}

pred_sex_language <- function(x, model) {
  x$pred <- predict(model[[x$language[1]]],
                    newdata = x, type = "response")
  return(x)
}

pred_sex_source <- function(x, model) {
  x$pred <- predict(model[[x$source[1]]],
                    newdata = x, type = "response")
  return(x)
}

fit_ses <- function(x) {
  robustbase::glmrob(cbind(production, no_production) ~
                       age * mom_ed  - mom_ed,
                     family = "binomial",
                     data = x)
}

pred_ses_language <- function(x, model) {
  x$pred <- predict(model[[x$language[1]]],
                    newdata = x, type = "response")
  return(x)
}

pred_ses_source <- function(x, model) {
  x$pred <- predict(model[[x$source[1]]],
                    newdata = x, type = "response")
  return(x)
}
