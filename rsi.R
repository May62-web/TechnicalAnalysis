rsi <- function(data, period) {
  # Calculate price changes
  changes <- diff(data)
  
  # Separate gains and losses
  gains <- pmax(changes, 0)
  losses <- pmax(-changes, 0)
  
  # Calculate average gain and average loss
  avg_gain <- numeric(length(changes))
  avg_loss <- numeric(length(changes))
  
  avg_gain[1] <- gains[1]
  avg_loss[1] <- losses[1]
  
  for (i in 2:length(changes)) {
    avg_gain[i] <- ((avg_gain[i - 1] * (period - 1)) + gains[i]) / period
    avg_loss[i] <- ((avg_loss[i - 1] * (period - 1)) + losses[i]) / period
  }
  
  # Calculate Relative Strength
  rs <- avg_gain / avg_loss
  
  # Calculate RSI
  rsi_values <- 100 - (100 / (1 + rs))
  
  # Handle cases where average loss is zero
  rsi_values[avg_loss == 0] <- 100
  
  return(rsi_values)
}