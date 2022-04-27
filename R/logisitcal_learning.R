get_data_for_prediction <- function(data_set, dependent, regressors, experience_levels) {
  coding_experience <- c(Uerfaren = 1, Grunnleggende = 2,
                         Videregående = 3, Avansert = 4)
  data_short <- data_set %>% dplyr::select(dependent, regressors)
  all_vars <- c(dependent, regressors)
  data_short <- tibble::as_tibble(lapply(data_short[all_vars], as.factor))
  if("leder_c" %in% regressors && isTRUE(!is.factor(data_set$leder_c))) {
    new_leder <- (as.integer(data_short$leder_c) - 2) * (-1) + 1
    new_leder <- c("Nei", "Ja")[new_leder]
    new_leder <- factor(new_leder, levels = c("Nei", "Ja"))
    data_short[, "leder_c"] <- new_leder
  }

  data_out <- data_short %>%
    dplyr::filter(as.integer(data_short[[dependent]]) %in% coding_experience[experience_levels])

  return(data_out)
}
logistic_learn <- function(data_set,
                           model) {

  dependent         <- model$dependent
  regressors        <- model$regressors
  experience_levels <- model$experience_levels

  data_short_selected <- get_data_for_prediction(data_set,
                                                 dependent,
                                                 regressors,
                                                 experience_levels)
  model_formula <- as.formula(paste(dependent, paste(regressors, collapse=" + "), sep=" ~ "))

  logistic_model <- glm(model_formula,
                        data = data_short_selected,
                        family = binomial(link="logit"))
  summary_logistic_model <- summary(logistic_model)
  summary_odds <- exp(summary_logistic_model$coefficients[, 1, drop = FALSE])
  colnames(summary_odds) <- "Odds"
  summary_logistic_model[["coefficients"]] <- cbind(summary_logistic_model[["coefficients"]], summary_odds)

  output <- list(logistic_model = logistic_model,
                 summary_logistic_model = summary_logistic_model,
                 data_set = data_short_selected)
  return(output)
}
