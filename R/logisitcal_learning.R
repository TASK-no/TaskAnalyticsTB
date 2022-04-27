#' Transforms data set into a format suitable for logit regressions.
#'
#' The transformation on the input data set \code{data_set} are:
#'   \itemize{
#'     \item compute a short data set that only has dependent and regressor vars
#'     \item transform these vars into \code{factor}
#'     \item recode, if the variable is used, the \code{leder_c} into "Nei"/"Ja"
#'     \item reduce short data set to a comparison between experience levels
#'       (which is 0/1) as defined via argument \code{experience_levels}; e.g.
#'       if \code{experience_levels = c("Uerfaren", "Avansert")} then
#'       'Uerfaren'=0 and 'Avansert'=1 which are then the possible realizations
#'       of the dependent variable
#'   }
#'
#' @param data_set a data set as \code{data.frame} or \code{tibble}
#' @param dependent the name of the dependent variable as a character string
#' @param regressors the names of the regressor variables as a character vector
#' @param experience_levels the experience levels as a two-dimensional character
#'   vector with values of either
#'   \itemize{
#'     \item Uerfaren
#'     \item Grunnleggende
#'     \item `Videreg\u00e5ende`
#'     \item Avansert
#'   }
#'
#' @return a data set (as a \code{data.frame}) shortened and thus suitable for
#'   prediction via logit regressions; the output data has dependent variable
#'   and regressors only
get_data_for_prediction <- function(data_set,
                                    dependent,
                                    regressors,
                                    experience_levels) {
  coding_experience <- c(Uerfaren = 1, Grunnleggende = 2,
                         "Videreg\u00e5ende" = 3, Avansert = 4)
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
#' Function wrapper for logit regressions adjusted to the SSV setting.
#'
#' The wrapper is araound \link[stats]{glm} and a suitable expression to be
#' passed to \code{as.formula} i.e. of the form "y~x1+x2+...+xn"
#'
#' @param data_set the data set; will be passed to get_data_for_prediction
#'   internally
#' @param model a model specification of the form ....
#'
#' @return output as generated via \link[stats]{glm} but flavoured with some
#'   nicer printing structure and addtional information
#' @export
logistic_learn <- function(data_set,
                           model) {

  dependent         <- model$dependent
  regressors        <- model$regressors
  experience_levels <- model$experience_levels

  data_short_selected <- get_data_for_prediction(data_set,
                                                 dependent,
                                                 regressors,
                                                 experience_levels)
  model_formula <- stats::as.formula(paste(dependent,
                                           paste(regressors,
                                                 collapse=" + "),
                                           sep=" ~ "))

  logistic_model <- stats::glm(model_formula,
                               data = data_short_selected,
                               family = stats::binomial(link="logit"))
  summary_logistic_model <- summary(logistic_model)
  summary_odds <- exp(summary_logistic_model$coefficients[, 1, drop = FALSE])
  colnames(summary_odds) <- "Odds"
  summary_logistic_model[["coefficients"]] <- cbind(summary_logistic_model[["coefficients"]], summary_odds)

  output <- list(logistic_model = logistic_model,
                 summary_logistic_model = summary_logistic_model,
                 data_set = data_short_selected)
  return(output)
}
