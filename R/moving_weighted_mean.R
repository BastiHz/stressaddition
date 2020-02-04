moving_weighted_mean <- function(x) {
    # Helper function to calculate the moving weighted mean. This is used to
    # smooth out points which are jumping up and down.
    count <- rep(1, length(x))
    x_diff <- diff(x) * -1
    while (any(x_diff < 0, na.rm = TRUE)) {
        i <- which(x_diff < 0)[1]
        j <- i + 1
        x[i] <- weighted.mean(x[i:j], count[i:j])
        x <- x[-j]
        count[i] <- count[i] + count[j]
        count <- count[-j]
        x_diff <- diff(x) * -1
    }
    rep(x, count)
}
