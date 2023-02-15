#' Transforms data set into a format suitable for logit regressions.
#'
#' The transformation on the input \code{data_set} are:
#'   \itemize{
#'     \item compute a short data set that only has dependent and regressor vars
#'     \item transform these vars into \code{factor}s
#'     \item re-code, the \code{leder_c} into "Nei"/"Ja", if the variable is
#'     used
#'     \item reduce short data set to a comparison between experience levels
#'     (which is 0/1) as defined via argument \code{experience}; e.g. if
#'     \code{experience = c("Uerfaren", "Avansert")} then 'Uerfaren'=0
#'     and 'Avansert'=1 which are then the possible realizations of the
#'     dependent variable
#'   }
#'
#' @param data_set a data set as \code{data.frame} or \code{tibble}
#' @param dependent the name of the dependent variable as a character string
#' @param regressors the names of the regressor variables as a character vector
#' @param experience the experience levels as a two-dimensional character
#'   vector with values of either
#'   \itemize{
#'     \item Uerfaren
#'     \item Grunnleggende
#'     \item Videregende
#'     \item Avansert
#'   }
#'
#' @return a data set (as a \code{data.frame}) shortened and thus suitable for
#'   prediction via logit regressions; the output data has dependent variable
#'   and regressors only
#' @export
get_data_for_prediction <- function(data_set,
                                    dependent,
                                    regressors,
                                    experience) {
  coding_experience <- c(Uerfaren = 1, Grunnleggende = 2,
                         "Videreg\u00e5ende" = 3, Avansert = 4)
  data_short <- data_set %>% dplyr::select(dependent,
                                           dplyr::any_of(regressors))
  all_vars <- names(data_short)
  data_short <- tibble::as_tibble(lapply(data_short[all_vars], as.factor))
  if("leder_c" %in% regressors && isTRUE(!is.factor(data_set$leder_c))) {
    new_leder <- (as.integer(data_short$leder_c) - 2) * (-1) + 1
    new_leder <- c("Nei", "Ja")[new_leder]
    new_leder <- factor(new_leder, levels = c("Nei", "Ja"))
    data_short[, "leder_c"] <- new_leder
  }

  data_out <- data_short %>%
    dplyr::filter(as.integer(data_short[[dependent]]) %in% coding_experience[experience])

  return(data_out)
}
#' Function wrapper for logit regressions adjusted to the SSV setting.
#'
#' The wrapper is araound \link[stats]{glm} and a suitable expression to be
#' passed to \code{as.formula} i.e. of the form "y~x1+x2+...+xn"
#'
#' @param data_set the data set; will be passed to get_data_for_prediction
#'   internally
#' @param model a model specification as a list containing named elements of
#'   the form
#' \itemize{
#'   \item 'dependent': the name of the dependent variable as a character string
#'   \item 'regressors': a character vector of regressor names
#'   \item 'experience': a two element vector giving the dependent
#'   variable levels (values coded to zero and one) that are being compared
#'  }
#'
#' @return output as generated via \link[stats]{glm} but flavored with some
#'   nicer printing structure and additional information; in the
#'   \code{type = "default"} version a list of three elements, in the shiny
#'   version a list of two
#' @export
logistic_learn <- function(data_set, model, type = "default") {
  if (type == "default") {
    out <- logistic_learn_def(data_set, model)
  } else if (type == "shinyDB") {
    out <- logistic_learn_shy(data_set, model)
    out <- list(model_summary = out$summary_logistic_model,
                odds_info = out$summary_odds)
  } else {
    stop("Unknown argument value for argument 'type'.")
  }
  return(out)
}
logistic_predict <- function(data_set, model,
                             sample_train_seq) {
  num_obs   <- nrow(data_set)
  num_train <- sample_train_seq[length(sample_train_seq)]
  out1    <- logistic_learn(data_set[sample_train_seq, ],
                            model = model, type = "shinyDB")

  sample_pred_seq <- (num_train + 1):num_obs
  data_pred <- get_data_for_prediction(data_set[sample_pred_seq, ],
                                       dependent = model$dependent,
                                       regressors =  model$regressors,
                                       experience =  model$experience)

  truePRED <- as.integer(data_pred[[model$dependent]]) - 1

  predictions <- predict(out1$logistic_model,
                         newdata = data_pred[, -c(1)],
                         type = "response")
  # optCutOff21 <- InformationValue::optimalCutoff(truePRED, predictions)
  # InformationValue::misClassError(truePRED, predictions, threshold = optCutOff21)
  # InformationValue::plotROC(truePRED, predictions,  Show.labels = TRUE)
  # InformationValue::Concordance(truePRED, predictions)
  # InformationValue::sensitivity(truePRED, predictions, threshold = optCutOff21)
  # InformationValue::specificity(truePRED, predictions, threshold = optCutOff21)
}
logistic_learn_def <- function(data_set, model) {
  dependent         <- model$dependent
  regressors        <- model$regressors
  experience <- model$experience

  data_short <- get_data_for_prediction(data_set,
                                        dependent,
                                        regressors,
                                        experience)

  model_formula <- stats::as.formula(paste(dependent,
                                           paste(regressors,
                                                 collapse=" + "),
                                           sep=" ~ "))

  logistic_model <- stats::glm(model_formula,
                               data = data_short,
                               family = stats::binomial(link = "logit"))
  summary_logistic_model <- summary(logistic_model)
  summary_odds <- exp(summary_logistic_model$coefficients[, 1, drop = FALSE])
  colnames(summary_odds) <- "Odds"
  summary_logistic_model[["coefficients"]] <- cbind(summary_logistic_model[["coefficients"]], summary_odds)

  output <- list(logistic_model = logistic_model,
                 summary_logistic_model = summary_logistic_model,
                 data_set = data_short)
  return(output)
}
logistic_learn_shy <- function(data_set, model) {
  model_formula <- parse_model_to_formula(model)
  data_short    <- get_data_for_prediction(data_set,
                                           model$dependent,
                                           model$regressors,
                                           model$experience)
  logistic_out <- stats::glm(model_formula,
                             data = data_short,
                             family = stats::binomial(link = "logit"))
  logistic_out_sum <- summary(logistic_out)
  logistic_out_odd <- exp(logistic_out_sum$coefficients[, 1, drop = FALSE])
  return(list(summary_logistic_model = logistic_out_sum,
              summary_odds = logistic_out_odd))
}
parse_model_to_formula <- function(model) {
  dependent           <- model$dependent
  regressors          <- get_regs(model$regressors)

  moduel_formula_char <- get_model_forumula_char(dependent, regressors)
  model_formula <- stats::as.formula(moduel_formula_char)

  return(model_formula)
}
get_regs <- function(regs) {
  if(is.null(regs)) return(" 1 ")
  paste(regs, collapse = " + ")
}
get_model_forumula_char <- function(dep, regs) {
  paste(dep, regs, sep = "~")
}
