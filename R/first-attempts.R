#' Test #1
#'
#' This is a test function that launches some test code to test if this project
#' has any sense.
#'
#' Jokes aside, it is the SVM example taken from Järjestelmällinen sijoittaja's
#' \href{https://jarjestelmallinensijoittaja.wordpress.com/tag/svm/}{article} on
#' Machine Learning in Time Series Prediction.
#' @importFrom caret createDataPartition
#' @importFrom e1071 classAgreement svm tune.svm
#' @importFrom quantmod getSymbols
#' @importFrom stats predict
#' @export
test1 <- function() {

    # This is to make the check() clean
    SPY <- NULL

    # Load the ETF on the S&P 500 symbol
    getSymbols("SPY")
    dataset <- add_features(SPY)

    # Split data into training and test set
    idx <- 1:nrow(dataset)
    in_training <- createDataPartition(idx, p = 0.75, list = FALSE)
    training_orig <- dataset[in_training, ]
    testing_orig <- dataset[-in_training, ]

    # Change objects type into data.frame
    training <- as.data.frame(training_orig)
    testing <- as.data.frame(testing_orig)
    rownames(training) <- rownames(testing) <- NULL

    # Convert result variable into factor
    training$NEXTDAY <- as.factor(training$NEXTDAY)
    testing$NEXTDAY <- as.factor(testing$NEXTDAY)

    # Change levels into up and down
    levels(training$NEXTDAY) <- levels(testing$NEXTDAY) <-
        list(down = "-1", up = "1")

    # Model tuning
    obj <- tune.svm(NEXTDAY ~ .,
                    data = training,
                    cost = 2^(2:4),
                    gamma = 2^(-1:1))

    # Training model
    svm_model <- svm(NEXTDAY ~ .,
                     data = training,
                     kernel = "sigmoid",
                     cost = 10)

    # Prediction step
    svm_pred <- predict(svm_model, testing[, -ncol(testing)])

    # Evalute results
    table(pred = svm_pred, true = testing[, ncol(testing)])
    classAgreement(table(pred = svm_pred, true = testing[, ncol(testing)]))

}

#' Test #2
#'
#' This is a test on the gold historical price
#' @importFrom PerformanceAnalytics charts.PerformanceSummary
#' @importFrom e1071 svm
#' @importFrom quantmod getSymbols
#' @importFrom stats predict
#' @export
test2 <- function() {

    # This is to silent the check()
    GLD <- NULL

    # Ottengo i dati
    getSymbols("GLD", src = "yahoo", from = "1990-01-01")

    # Agggiungo le statistiche
    data <- add_features(GLD)
    daily <- data$RESULT
    data$RESULT <- NULL

    # Costruisco la strategia
    learning_period <- 200
    result <- c()

    for (i in (learning_period+1):(nrow(data) - 2)) {
        ef_train <- data[(i - learning_period):i, ]
        r1 <- svm(factor(NEXTDAY) ~ .,
                  data = ef_train,
                  cost = 100,
                  gamma = 0.1)
        r1_pred <- predict(r1, data[i+1, 1:17])
        r1_pred <- data.frame(r1_pred)

        if (as.numeric(as.character(r1_pred[1, ])) == data$NEXTDAY[i+1]) {
            result <- rbind(result, abs(daily[i+1, 1]))
        } else {
            result <- rbind(result, -abs(daily[i+1, 1]))
        }

        if (i %% 200 == 0) {
            charts.PerformanceSummary(result, ylog = TRUE)
        }
    }

}

#' Test #3
#'
#' Testing svm with more interesting features
#' @importFrom PerformanceAnalytics charts.PerformanceSummary
#' @importFrom TTR RSI
#' @importFrom e1071 svm
#' @importFrom quantmod Cl getSymbols
#' @importFrom splines ns
#' @importFrom stats lm
#' @export
test3 <- function() {

    # This to silent the check()
    GLD <- NULL

    # Retrieve data from Yahoo Finance
    getSymbols("GLD", src = "yahoo", from = "2016-01-01")
    data <- GLD

    # Aggancio l'RSI
    data <- na.trim(merge(data, RSI(Cl(data))))
    names(data)[7] <- "RSI"

    # Aggancio l'RSI^2
    # data <- na.trim(merge(data, RSI(data$RSI)))
    # names(data)[8] <- "RSI2"

    # Aggancio l'RSI^3
    # data <- na.trim(merge(data, RSI(data$RSI2)))
    # names(data)[9] <- "RSI3"

    # Aggiuno la spline sull'RSI (sto barando!!)
    fit <- lm(data$RSI ~ ns(1:nrow(data), df = floor(nrow(data) / 4)))
    data <- merge(data, predict(fit, data.frame(p = 1:nrow(data))))
    names(data)[8] <- "Spline"

    # Aggiungo il ritorno del giorno dopo
    data <- na.trim(merge(data, lag(ROC(Cl(data), type = "discrete"), -1)))
    names(data)[9] <- "NextDay"

    # Costruisco la strategia
    learning_period <- 200
    threshold <- 0.1 / 100
    result <- NULL

    for (i in (learning_period+1):(nrow(data) - 2)) {

        ef_train <- data[(i - learning_period):i, ]

        r1 <- svm(NextDay ~ .,
                  data = ef_train,
                  type = "nu-regression",
                  cost = 100,
                  gamma = 0.1)
        r1_pred <- predict(r1, data[i+1, -ncol(data)])

        trigger <- ifelse(abs(r1_pred) > threshold, "go", "dont")

        value <- abs(data$NextDay[i+1])
        if (sign(r1_pred) != sign(data$NextDay[i+1]))
            value <- -value
        if (trigger == "dont")
            value$NextDay <- 0

        result <- rbind(result, value)

    }

    charts.PerformanceSummary(result)

}

#' RSI Trading Signal
#'
#' This is a strategy for long/short positioning according to RSI cross of
#' pre-fixed thresholds subject to parameterization.
#' @param series is a \code{xts} object representing the OHLC object as
#' retrieved using \code{getSymbols}.
#' @param lower is the RSI level requested to trigger the short trade.
#' @param upper is the RSI level requested to trigger the long trade.
#' @param epsilon is the tolerance to avoid closing a position too early.
#' @importFrom TTR RSI
#' @importFrom quantmod Cl Op
#' @importFrom zoo na.trim
#' @export
trade_rsi <- function(series, lower = 30, upper = 70, epsilon = 5/100) {

    # Adding the RSI to the original time series
    series <- na.trim(merge(series, RSI(Cl(series))))
    names(series)[ncol(series)] <- "RSI"

    # Initializing the trade repository
    trade_repo <- NULL
    actual_deal <- NULL

    # Cycling on each available trade day
    n <- nrow(series)
    for (i in 1:(n - 1)) {

        # Check if the upper limit has been up violated
        if (is.null(actual_deal) & series$RSI[[i]] >= upper) {

            # Creating a new deal at the open price of the following day
            actual_deal <- data.frame(side = "buy",
                                      open_date = time(series)[i + 1],
                                      open_price = Op(series)[[i + 1]],
                                      close_date = as.Date(NA),
                                      close_price = as.numeric(NA))
            next
        }

        # Check if the upper limit has been down violated
        if (!is.null(actual_deal) &&
            actual_deal$side == "buy" &&
            series$RSI[[i]] <= upper * (1 - epsilon)) {

            # Closing the open deal at the close price of the day
            actual_deal$close_date <- time(series)[i]
            actual_deal$close_price <- Cl(series)[[i]]
            trade_repo <- rbind(trade_repo, actual_deal)
            actual_deal <- NULL
            next
        }

        # Check if the lower limit has been down violated
        if (is.null(actual_deal) & series$RSI[[i]] <= lower) {

            # Creating a new deal at the open price of the following day
            actual_deal <- data.frame(side = "sell",
                                      open_date = time(series)[i + 1],
                                      open_price = Op(series)[[i + 1]],
                                      close_date = as.Date(NA),
                                      close_price = as.numeric(NA))
            next
        }

        # Check if the lower limit has been up violated
        if (!is.null(actual_deal) &&
            actual_deal$side == "sell" &&
            series$RSI[[i]] <= lower) {

            # Closing the open deal at the close price of the day
            actual_deal$close_date <- time(series)[i]
            actual_deal$close_price <- Cl(series)[[i]]
            trade_repo <- rbind(trade_repo, actual_deal)
            actual_deal <- NULL
            next
        }

    }

    # If I still have an open deal, I put it in the repo
    trade_repo <- rbind(trade_repo, actual_deal)

}
