#' Generate data for a learning curve plot
#'
#' @author Adam Acosta
#'
#' @param data data.frame Contains training examples
#' @param learner A learner object
#'
#' @return A data frame with columns
#'   \item{Examples}{integer Sample size used}
#'   \item{Type}{factor with levels 'training' and 'testing'}
#'   \item{Error}{numeric Mean misclassification error across repeated CV}
#'
#' @importFrom mlr makeLearner
#' @importFrom mlr makeClassifTask
#' @importFrom mlr makeResampleDesc
#' @importFrom mlr resample
#' @importFrom parallelMap parallelStart
#' @importFrom parallelMap parallelMap
#' @importFrom parallelMap parallelStop
#'
#' @export
learningCurve <- function(data, lrn) {
    nr <- nrow(data)
    percs <- seq(0.1, 1.0, by = 0.1)

    generateSeries <- function(perc) {
        new.task <- makeClassifTask(id = 'new.task',
                                    data = data[sample(nr, perc * nr), ],
                                    target = names(data)[ncol(data)])
        rdesc <- makeResampleDesc(method = 'RepCV', predict = 'both')
        r <- resample(lrn, new.task, rdesc)
        out <- list(training = mean(r$measures.train$mmce),
                    testing = mean(r$measures.test$mmce))
        out
    }

    parallelStart(mode = 'multicore')
    tmp <- unlist(parallelMap(generateSeries, percs))
    parallelStop()

    out <- data.frame(Examples = percs * nr,
                      Type = as.factor(c(rep('Training', length(tmp) / 2),
                                         rep('Testing', length(tmp) / 2))),
                      Error = c(unname(tmp)[c(TRUE, FALSE)],
                                unname(tmp)[c(FALSE, TRUE)]))
    out
}

#' Generate a learning curve plot
#'
#' @author Adam Acosta
#'
#' @param data A data.frame containing learning curve data
#' @param ggvis Logical Should ggvis be used; default FALSE
#'
#' @return A ggplot object if ggvis = TRUE, otherwise no return value
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggvis ggvis
#' @importFrom ggvis layer_lines
#' @importFrom magrittr %>%
#'
#' @export
learningCurvePlot <- function(data, ggvis = FALSE) {
    if (ggvis) {
        data %>%
            ggvis(~Examples, ~Error, stroke = ~Type) %>%
            layer_lines()
    } else {
        data %>%
            ggplot(aes(Examples, Error, color = Type)) +
            geom_line()
    }
}
