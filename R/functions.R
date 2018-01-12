#' Add Features to Time Series
#'
#' This is a function from Quintuitive that adds some features to a time series.
#' Actually the automatisms have been completely re-engineered and now all the
#' statistics refers to the actual day except for the \code{NEXTDAY} features
#' which indicates if the close price in the next trading day has been higher or
#' lower than the current price.
#' @param data is an \code{xts} object to which the features have to be added.
#' @importFrom PerformanceAnalytics apply.rolling kurtosis skewness
#' @importFrom TTR ROC
#' @importFrom quantmod Cl Vo
#' @importFrom stats lag mad sd
#' @importFrom zoo na.trim rollmean rollmedian
#' @export
add_features <- function(data) {

    # Working with close prices
    close <- Cl(data)

    # Computing rate-of-changes (ROCs) for different periods
    periods <- c(1, 2, 3, 5, 10, 20, 50, 100, 150, 200)
    res_roc <- lapply(periods,
                      function(x) ROC(close,
                                      type = "discrete",
                                      n = x))
    res_roc <- do.call("merge", res_roc)
    res_roc <- na.trim(res_roc)
    names(res_roc) <- paste("ROC", periods, sep = ".")

    # Computing rolling averages and other statistics
    returns <- na.trim(ROC(close, type = "discrete"))
    res_stat <- merge(rollmean(returns,
                               k = 21,
                               align = "right"),
                      rollmedian(returns,
                                 k = 21,
                                 align = "right"),
                      apply.rolling(returns,
                                    width = 21,
                                    FUN = sd),
                      apply.rolling(returns,
                                    width = 21,
                                    FUN = mad),
                      apply.rolling(returns,
                                    width = 21,
                                    FUN = skewness,
                                    align = "right"),
                      apply.rolling(returns,
                                    width = 21,
                                    FUN = kurtosis,
                                    align = "right"),
                      Vo(data),
                      lag(returns, k = -1),
                      lag(ifelse(returns >= 0, 1, -1), k = -1))
    res_stat <- na.trim(res_stat)
    names(res_stat) <- c("MEAN", "MEDIAN", "SD", "MAD", "SKEW", "KURTOSIS",
                         "VOLUME", "RESULT", "NEXTDAY")

    # Union of the two pieces and return the result
    res <- merge(res_roc, res_stat)
    res <- na.trim(res)
    return(res)

}

#' Add Future Linear Trend
#'
#' This function adds to each point of a time series object the actual linear
#' trend in the immediate future.
#' @param data is an \code{xts} object to which the linear trend has to be
#' added.
#' @param n is the number of points for which the linear trend is suspected to
#' exist.
#' @export
add_linear_trend <- function(data, n = 10) {

    # Working with close prices
    close <- Cl(data)

    # Bulding the rolling linear regression
    n_max <- nrow(close) - n - 1
    slopes <- lapply(1:n_max,
                     function(i) {
                         close_ss <- data.frame(id = 1:n,
                                                value = as.vector(close[(i+1):(i+n)]))
                         fit <- lm(value ~ id,
                                   data = close_ss)
                         return(as.vector(fit$coefficients[2]))
                     })
    slopes <- xts(x = unlist(slopes),
                  order.by = time(close[1:n_max]))
    names(slopes) <- "Slopes"

    # Adding the linear regression slopes to the original dataset
    res <- merge(data, slopes)
    res <- na.trim(res)
    return(res)

}
