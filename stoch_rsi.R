# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Calculate the lowest and highest RSI values
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  
  # Calculate the StochRSI values
  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(rsi_values))
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }
  
  # Calculate the %K line
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line
  d_line <- sma(k_line, d_period)
  
  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}