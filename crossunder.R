crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("None", length(arr1))
  
  # Check for crossunders at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "Down"
    } else {
      crossunder_signals[i] <- "None"
    }
  }
  
  return(crossunder_signals)
}