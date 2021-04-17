# This function takes a number of successes and failuers coded as a TRUE/FALSE
# or 0/1 vector. This should be given as the data argument.
# The result is a visualization of the how a Beta-Binomial
# model gradualy learns the underlying proportion of successes 
# using this data. The function also returns a sample from the
# posterior distribution that can be further manipulated and inspected.
# The default prior is a Beta(1,1) distribution, but this can be set using the
# prior_prop argument.

# Make sure the packages tidyverse and ggridges are installed, otherwise run:
# install.packages(c("tidyverse", "ggridges"))

# Example usage:
# data <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
# prop_model(data)
prop_model <- function(data = c(), prior_prop = c(1, 1), n_draws = 10000) {
  library(tidyverse)
  data <- as.logical(data)
  # data_indices decides what densities to plot between the prior and the posterior
  # For 20 datapoints and less we're plotting all of them.
  data_indices <- round(seq(0, length(data), length.out = min(length(data) + 1, 20)))
  
  # dens_curves will be a data frame with the x & y coordinates for the 
  # denities to plot where x = proportion_success and y = probability
  proportion_success <- c(0, seq(0, 1, length.out = 100), 1)
  dens_curves <- map_dfr(data_indices, function(i) {
    value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", "Failure"))
    label <- paste0("n=", i)
    probability <- dbeta(proportion_success,
                         prior_prop[1] + sum(data[seq_len(i)]),
                         prior_prop[2] + sum(!data[seq_len(i)]))
    probability <- probability / max(probability)
    data_frame(value, label, proportion_success, probability)
  })
  # Turning label and value into factors with the right ordering for the plot
  dens_curves$label <- fct_rev(factor(dens_curves$label, levels =  paste0("n=", data_indices )))
  dens_curves$value <- factor(dens_curves$value, levels = c("Prior", "Success", "Failure"))
  
  p <- ggplot(dens_curves, aes(x = proportion_success, y = label,
                               height = probability, fill = value)) +
    ggridges::geom_density_ridges(stat="identity", color = "white", alpha = 0.8,
                                  panel_scaling = TRUE, size = 1) +
    scale_y_discrete("", expand = c(0.01, 0)) +
    scale_x_continuous("Underlying proportion of success") +
    scale_fill_manual(values = hcl(120 * 2:0 + 15, 100, 65), name = "", drop = FALSE,
                      labels =  c("Prior   ", "Success   ", "Failure   ")) +
    ggtitle(paste0(
      "Binomial model - Data: ", sum(data),  " successes, " , sum(!data), " failures")) +
    theme_light() +
    theme(legend.position = "top")
  print(p)
  
  # Returning a sample from the posterior distribution that can be further 
  # manipulated and inspected
  posterior_sample <- rbeta(n_draws, prior_prop[1] + sum(data), prior_prop[2] + sum(!data))
  invisible(posterior_sample)
}