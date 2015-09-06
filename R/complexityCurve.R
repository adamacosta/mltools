#' Generate complexity curve data
#'
#' @author Adam Acosta
#'
#' @param task A mlr task
#' @param learner A mlr learner
#' @param ps A parameter set over which to optimize model
#'
#' @importFrom mlr makeAggregation
#' @importFrom mlr setAggregation
#' @importFrom mlr makeTuneMultiCritControlRandom
#' @importFrom mlr makeResampleDesc
#' @importFrom mlr tuneParamsMultiCrit
#' @importFrom parallelMap parallelStart
#' @importFrom parallelMap parallelStop
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#'
#' @export
complexityCurve <- function(task, learner, ps) {
    train.mean <- makeAggregation(id = 'train.mean', name = 'Mean Training Error',
                      fun = function(task, perf.test, perf.train,
                                     measure, group, pred) {
                                         mean(perf.train)
                            }
                  )

    train.mmce <- setAggregation(mmce, train.mean)

    ctrl = makeTuneMultiCritControlRandom(maxit = 30L)
    rdesc = makeResampleDesc("RepCV", predict = 'both')
    parallelStart(mode = 'multicore')
    res = tuneParamsMultiCrit(learner,
                              task = task,
                              resampling = rdesc,
                              par.set = ps,
                              measures = list(mmce, train.mmce),
                              control = ctrl,
                              show.info = FALSE)
    parallelStop()
    out <- as.data.frame(res$opt.path)[, -(0:-4)]
    out %>% gather(Type, Error, mmce.test.mean:mmce.train.mean)
}

#' Plot complexity curve
#'
#' @author Adam Acosta
#'
#' @param data A data.frame with training and testing error to plot
#' @param x Name of the complexity parameter to plot on the X axis
#' @param ggvis logical Whether or not to use ggvis; default FALSE
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
complexityCurvePlot <- function(data, x, ggvis = FALSE) {
    if (ggvis) {
        data %>% ggvis(~x, ~Error, stroke = ~Type) %>% layer_lines()
    } else {
        data %>% ggplot(aes(x, Error, color = Type)) + geom_line()
    }
}
