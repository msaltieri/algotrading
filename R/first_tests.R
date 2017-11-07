#' Test #1
#'
#' This is a test function that launches some test code to test if this project
#' has any sense.
#'
#' Jokes aside, it is the SVM example taken from Järjestelmällinen sijoittaja's
#' \href{https://jarjestelmallinensijoittaja.wordpress.com/tag/svm/}{article} on
#' Machine Learning in Time Series Prediction.
#' @importFrom quantmod getSymbols
#' @export
test1 <- function() {

    # This is to make the check() clean
    SPY <- NULL

    # Load the ETF on the S&P 500 symbol
    getSymbols("SPY")
    data <- SPY
    dataset <- add_features(data)

}

#' Add Features to Time Series
#'
#' This is a function from Quintuitive that adds some features to a time series.
#' @param data is an \code{xts} object to which the features have to be added.
#' @importFrom PerformanceAnalytics apply.rolling kurtosis skewness
#' @importFrom TTR ROC
#' @importFrom quantmod Cl Vo
#' @importFrom stats lag mad sd
#' @importFrom xts xts
#' @importFrom zoo na.trim rollmean rollmedian
#' @export
add_features <- function(data) {

    close <- Cl(data)
    returns <- na.trim(ROC(close, type = "discrete"))

    # n-day returns
    res <- merge(na.trim(lag(returns, 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 2), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 3), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 5), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 10), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 20), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 50), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 100), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 150), 1)),
                 na.trim(lag(ROC(close, type = "discrete", n = 200), 1)),
                 all = FALSE)

    # other features
    res <- merge(res,
                 xts(na.trim(lag(rollmean(returns, k = 21, align = "right"), 1))),
                 xts(na.trim(lag(rollmedian(returns, k = 21, align = "right"), 1))),
                 xts(na.trim(lag(apply.rolling(returns, width = 21, FUN = sd), 1))),
                 xts(na.trim(lag(apply.rolling(returns, width = 21, FUN = mad), 1))),
                 xts(na.trim(lag(apply.rolling(returns, width = 21, align = "right", FUN = skewness), 1))),
                 xts(na.trim(lag(apply.rolling(returns, width = 21, align = "right", FUN = kurtosis), 1))),
                 all = FALSE)

    # add volume
    res <- merge(res, xts(na.trim(lag(Vo(data), 2))), all = FALSE)

    # add result column
    nextday <- ifelse(returns >= 0, 1, -1) # 1 if next day higher, 0 otherwise
    res <- merge(res, nextday, all = FALSE)

    colnames(res) <- c("ROC.1", "ROC.2", "ROC.3", "ROC.5", "ROC.10", "ROC.20",
                       "ROC.50", "ROC.100", "ROC.150", "ROC.200", "MEAN",
                       "MEDIAN", "SD", "MAD", "SKEW", "KURTOSIS", "VOLUME",
                       "NEXTDAY")
    return(res)

}
