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
#' @param model a list with the following items
#' \itemize{
#' \item `dependent` the name of the dependent variable as a character string
#' \item `regressors` the names of the regressor variables as a character vector
#' \item `experience` the experience levels as a two-dimensional character
#'   of which vector with values of either
#'   \itemize{
#'     \item Uerfaren
#'     \item Grunnleggende
#'     \item Videregende
#'     \item Avansert
#'   }
#' }
#'
#' @return a data set (as a \code{data.frame}) shortened and thus suitable for
#'   prediction via logit regressions; the output data has dependent variable
#'   and regressors only
#' @export
get_data_for_prediction <- function(data_set, model) {
  if (is.null(data_set)) return(NULL)
  dependent <- model$dependent
  regressors <- model$regressors
  experience <- model$experience
  coding_experience <- c(Uerfaren = 1, Grunnleggende = 2,
                         "Videreg\u00e5ende" = 3, Avansert = 4)
  data_short <- data_set %>% dplyr::select(dependent,
                                           dplyr::any_of(regressors))

  tmp_cd_exp <- coding_experience[experience]
  data_out <- data_short %>%
    dplyr::filter(as.integer(data_short[[dependent]]) %in% tmp_cd_exp)

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
#' @param type Character string specifying the type of logistic regression
#'   execution. "default" for a standard logistic regression model fitting and
#'   summary, "shinyDB" for a Shiny app-specific version that includes
#'   additional summaries like pseudo R-squared and odds ratios. The function
#'   will stop with an error if an unrecognized type is provided.

#'
#' @return output as generated via \link[stats]{glm} but flavored with some
#'   nicer printing structure and additional information; in the
#'   \code{type = "default"} version a list of three elements, in the shiny
#'   version a list of two
#' @export
logistic_learn <- function(data_set = NULL, model = NULL, type = "default") {
  if (is.null(data_set) || is.null(model)) {return(NULL);}
  if (type == "default") {
    out <- logistic_learn_def(data_set, model)
  } else if (type == "shinyDB") {
    out <- logistic_learn_shy(data_set, model)
    out <- list(model_run = out$logistic_model,
                model_summary = out$summary_logistic_model,
                psR2_info = out$summary_psR2,
                odds_info = out$summary_odds,
                fail_conv = out$summary_converged,
                fail_num = out$summary_numerical)
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
                                       model)

  truePRED <- as.integer(data_pred[[model$dependent]]) - 1
  predictions <- stats::predict(out1$logistic_model,
                                newdata = data_pred[, -c(1)],
                                type = "response")
}
logistic_learn_def <- function(data_set, model) {
  dependent  <- model$dependent
  regressors <- model$regressors
  # experience <- model$experience

  data_short <- get_data_for_prediction(data_set, model)

  model_formula <- stats::as.formula(paste(dependent,
                                           paste(regressors,
                                                 collapse = " + "),
                                           sep = " ~ "))

  logistic_model <- stats::glm(model_formula,
                               data = data_short,
                               family = stats::binomial(link = "logit"))
  summary_logistic_model <- summary(logistic_model)
  summary_odds <- exp(summary_logistic_model$coefficients[, 1, drop = FALSE])
  colnames(summary_odds) <- "Odds"
  tmp_coef_bind <- cbind(summary_logistic_model[["coefficients"]], summary_odds)
  summary_logistic_model[["coefficients"]] <- tmp_coef_bind

  output <- list(logistic_model = logistic_model,
                 summary_logistic_model = summary_logistic_model,
                 data_set = data_short)
  return(output)
}
logistic_learn_shy <- function(data_set, model) {
  logistic_out_sum <- NULL
  logistic_out_pR2 <- NULL
  logistic_out_odd <- NULL
  logistic_out_cnv <- NULL
  logistic_out_num <- NULL

  model_formula <- parse_model_to_formula(model)
  data_short    <- get_data_for_prediction(data_set, model)
  check_match   <- check_forumula_data_match(data_short, model_formula)

  if (isTRUE(check_match)) {
    logistic_out <- tryCatch(stats::glm(
      model_formula,
      data = data_short,
      family = stats::binomial(link = "logit")),
      warning = function(w) {
        return(list(out = stats::glm(model_formula,
                                     data = data_short,
                                     family = stats::binomial(link = "logit")),
                    mywarn = w))
        }
    )
    if (names(logistic_out)[2] == "mywarn") {
      logistic_out_cnv <- grepl("converge", logistic_out[[2]][[1]])
      logistic_out_num <- grepl("numerically", logistic_out[[2]][[1]])
      logistic_out     <- logistic_out[[1]]
      logistic_out_sum <- summary(logistic_out)
      logistic_out_mod <- logistic_out
    } else {
      logistic_out_mod <- logistic_out
      logistic_out_sum <- summary(logistic_out)
    }
    logistic_out_pR2 <- get_logistic_pseudoR2(log_out = logistic_out,
                                              data_set = data_short)
    logistic_out_odd <- get_logistic_odds_probs(logistic_out)
  } else if (isFALSE(check_match)) {
    msg <- paste0("Datasett for det", paste0("\u00e5" ,"r"),
                  "et har ikke de spesifiserte regressorene!")
    logistic_out_mod <- msg
    logistic_out_sum <- msg
    logistic_out_pR2 <- msg
    logistic_out_odd <- msg
  }
  return(list(logistic_model = logistic_out_mod,
              summary_logistic_model = logistic_out_sum,
              summary_psR2 = logistic_out_pR2,
              summary_odds = logistic_out_odd,
              summary_converged = logistic_out_cnv,
              summary_numerical = logistic_out_num))
}
get_logistic_pseudoR2 <- function(log_out = NULL,
                                  data_set = NULL) {
  ################################### SOURCES###################################
  # https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
  # https://de.wikipedia.org/wiki/Pseudo-Bestimmtheitsma%C3%9F
  ##############################################################################
  if (is.null(log_out)) stop("Cannot compute pseudo R^2 without model arg.")
  if ( is.null(data_set)) {
    pR2_McFD <-  1 - log_out$deviance / log_out$null.deviance # works for glm
  } else if (!is.null(data_set)) {
    log_outNULL <- stats::glm(
      stats::as.formula(paste0(names(data_set)[1], " ~ 1")),
      data = data_set,
      family = stats::binomial(link = "logit"))
    LLfull <- unclass(stats::logLik(log_out))[[1]]
    LLnull <- unclass(stats::logLik(log_outNULL))[[1]]
    numreg <- ncol(data_set) - 1
    pR2_McFD      <-  1 - LLfull / LLnull
    pR2_McFD_corr <-  1 - ((LLfull -  numreg) / LLnull)
  }
  out <- matrix(c(pR2_McFD, pR2_McFD_corr), nrow = 1)
  colnames(out) <- c("McFadden R2", "korrigert McFadden R2")
  rownames(out) <- "->"
  return(out)
}
get_logistic_odds_probs <- function(log_out) {
  tmp_out <- tryCatch(comput_odds(log_out, WITH_CI = TRUE),
                      warning = function(w) {
                        return(
                          list(comput_odds(log_out, WITH_CI = FALSE),
                               mywarn = w))},
                      error = function(e) {
                        return(
                          list(comput_odds(log_out, WITH_CI = FALSE),
                               myerr = e))})
  if (!is.null(names(tmp_out))) {
    tmp_fill <- rep(NA_real_, times = length(tmp_out[[1]]))
    tmp_out  <- cbind(tmp_out[[1]], tmp_fill, tmp_fill)
  }
  colnames(tmp_out) <- c("Oddsratio",
                         "KI nedre grense",
                         "KI \u00f8vre grense")
  tmp_out
}
comput_odds <- function(log_model_output, WITH_CI) {
  if (isTRUE(WITH_CI)) {
    tmp <- exp(cbind(stats::coef(log_model_output),
                     stats::confint(log_model_output)))
  } else if (isFALSE(WITH_CI)) {
    tmp <- exp(stats::coef(log_model_output))
  }
  round(tmp, digits = 2)
}
parse_model_to_formula <- function(model) {
  dependent           <- model$dependent
  regressors          <- get_regs(model$regressors)

  moduel_formula_char <- get_model_forumula_char(dependent, regressors)
  model_formula <- stats::as.formula(moduel_formula_char)

  return(model_formula)
}
get_regs <- function(regs) {
  if (is.null(regs)) return(" 1 ")
  paste(regs, collapse = " + ")
}
get_model_forumula_char <- function(dep, regs) {
  paste(dep, regs, sep = "~")
}
check_forumula_data_match <- function(data_set, formula_taken) {
  col_to_check <- colnames(data_set)[-1]
  formula_to_check <- gsub(" ", "", strsplit(as.character(formula_taken[3]),
                                             "\\+")[[1]])
  setequal(col_to_check, formula_to_check)
}
#' Extract True Class Labels from Prediction Data Set
#'
#' This function extracts the true class labels as binary outcomes (0 or 1)
#' from a given prediction data set. It is assumed that the first column of the
#' data set contains the dependent variable with values coded accordingly.
#'
#' @param data_set_predictions A data frame containing the predictions, with the
#' first column being the dependent variable.
#'
#' @return A numeric vector containing the true class labels as 0 or 1.
#' @export
get_true_ones <- function(data_set_predictions) {
  if (is.null(data_set_predictions)) return(NULL)
  as.integer(data_set_predictions[[1]]) - 1
}
#' Generate Predictions Using a Logistic Model
#'
#' This function generates predictions from a logistic regression model for a
#' new dataset. It assumes the model is already fitted using logistic regression
#' and the new data is prepared in a similar format as the training data. The
#' function is designed to work with binary classification problems.
#'
#' @param logistic_model A logistic regression model object, typically created
#'   and trained using \code{\link[stats]{glm}} with a binomial family. This
#'   model is used to generate predictions on the new data.
#' @param new_data A data frame or matrix containing the new observations for
#'   which predictions are to be made. The structure of this data should match
#'   the model's training data, excluding the dependent variable.
#' @param type Character string specifying the type of prediction required.
#'   Default is "response", which returns predicted probabilities. Other options
#'   depend on the \code{\link[stats]{predict.glm}} method, such as "link" for
#'   the linear predictors.
#'
#' @return A numeric vector of predicted probabilities for the positive class
#'   (assuming a binary classification problem) if \code{type = "response"} is
#'   used. The length of the vector matches the number of rows in
#'   \code{new_data}. For other values of \code{type}, the return value follows
#'   the specification of \code{\link[stats]{predict.glm}}.
#'
#' @export
generate_predictions <- function(logistic_model,
                                 new_data,
                                 type = "response") {
  if (is.null(logistic_model)) return(NULL)
  if (is.null(new_data)) return(NULL)
  stats::predict(logistic_model, newdata = new_data[, -c(1)], type = "response")
}
#' Calculate Classification Metrics and Optimal Cutoff
#'
#' Computes various classification metrics including the optimal cutoff for
#' classification, misclassification error, concordance, sensitivity, and
#' specificity based on true outcomes and predicted probabilities.
#'
#' @param true_ones Numeric vector of true class labels (0 or 1).
#' @param preds Numeric vector of predicted probabilities for the positive class
#' @param thrsh Numeric value specifying the threshold for classification.
#'
#' @return A list containing the optimal cutoff, concordance, misclassification
#'   error, sensitivity, and one minus specificity.
#' @export
get_cls_infos <- function(true_ones, preds, thrsh) {
  if (is.null(true_ones) || is.null(preds)) return(NULL)
  opt_cut_off <- InformationValue::optimalCutoff(
    true_ones,
    preds)
  miss_class_error <- InformationValue::misClassError(
    true_ones,
    preds,
    threshold = thrsh)
  concordance <- InformationValue::Concordance(
    true_ones,
    preds)
  sensitivity <- InformationValue::sensitivity(
    true_ones,
    preds,
    threshold = thrsh)
  specificity <- InformationValue::specificity(
    true_ones,
    preds,
    threshold = thrsh)
  list(opt_cut_off = opt_cut_off,
       concordance = concordance[1],
       miss_class_error = miss_class_error,
       sensitivity = sensitivity,
       minus_specificity = 1 - specificity)
}
