library(ggplot2)

# Function to find peaks in a signal vector with given intensity and length thresholds
find_peaks_with_threshold <- function(signal_vector, min_int_threshold, min_points) {
  peaks <- list() # Initialize list to store detected peaks
  peak_start <- NULL # Variable to mark the start of a peak
  peak_end <- NULL # Variable to mark the end of a peak
  peak_length <- 0 # Variable to track the length of the current peak
  
  # Loop through each value in the signal vector
  for (index in seq_along(signal_vector)) {
    value <- signal_vector[index]
    if (value > min_int_threshold) { # Check if the value exceeds the minimum intensity threshold
      if (is.null(peak_start)) { # If peak_start is NULL, mark the start of a new peak
        peak_start <- index
      }
      peak_end <- index # Update the end of the current peak
      peak_length <- peak_length + 1 # Increment the length of the current peak
    } else if (!is.null(peak_start)) { # If the value does not exceed the threshold and a peak was started
      if (peak_length >= min_points) { # Check if the peak length meets the minimum points requirement
        peaks <- append(peaks, list(c(peak_start, peak_end))) # Append the peak to the list of peaks
      }
      peak_start <- NULL # Reset peak_start
      peak_end <- NULL # Reset peak_end
      peak_length <- 0 # Reset peak_length
    }
  }
  
  # Check if the last peak was not appended
  if (!is.null(peak_start) && peak_length >= min_points) {
    peaks <- append(peaks, list(c(peak_start, peak_end))) # Append the last peak if it meets the criteria
  }
  
  return(peaks) # Return the list of detected peaks
}

# Example usage
ints <- c(1.3, 1, 2, 4, 1.5, 1.8, 3, 5, 5.5, 4.3, 3, 1.5, 1.8, 1.4, 1.9, 1, 2, 3.2, 4.5, 5.3, 5.8, 5.1, 4, 3, 2, 1, 1.1)
min_int_threshold <- 2.2 # Set minimum intensity threshold
min_points <- 4 # Set minimum points threshold
peaks <- find_peaks_with_threshold(ints, min_int_threshold, min_points) # Find peaks in the signal vector

# Create a data frame for plotting
df <- data.frame(index = seq_along(ints), intensity = ints)
# Generate a plot of the signal vector
p <- ggplot(df, aes(x = index, y = intensity)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = min_int_threshold, color = 'green') + # Add a horizontal line at the intensity threshold
  labs(title = "Detected Peaks", x = "Index", y = "Intensity")

# Add the detected peaks to the plot
for (peak in peaks) {
  p <- p + annotate("rect", xmin = peak[1], xmax = peak[2], ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "red") # Highlight peaks with a rectangle
}

# Display the plot
p
