library(AccSamplingDesign)
library(ggplot2)

# Function to estimate mu corresponding to a given proportion nonconforming (p)
mu_limit_estimate <- function(p, limit, theta, limtype = c("lower", "upper")) {
  limtype <- match.arg(limtype)
  USL = LSL = NULL
  if(limtype == "lower"){
    LSL = limit
  } else if(limtype == "upper") {
    USL = limit
  }
  
  mu <- muEst(p, USL = USL, LSL = LSL, theta = theta, dist = "beta")  
  return(mu)
}

# Function to estimate probability of acceptance (Pa)
Pa_estimate <- function(p, theta, k, m, limit, limtype = c("lower", "upper")) {
  limtype <- match.arg(limtype)
  USL = LSL = NULL
  if(limtype == "lower"){
    LSL = limit
  } else if(limtype == "upper") {
    USL = limit
  }
  
  planObj <- structure(list(m = m, k = k, USL = USL, LSL = LSL,
                            theta = theta, theta_type = "known",
                            distribution = "beta"), 
                       class = "VarPlan")
  
  pa <- accProb(planObj, p)
  return(pa)
}

# Function to plot OC curves #uom
plot_OC_curve_ggplot <- function(m, k, theta, limit, 
                                 limtype = c("lower", "upper"), 
                                 uom, uom_mapping,
                                 p1, alpha, p2, beta, 
                                 nsim = 5000) {
  limtype <- match.arg(limtype)
  # Get reference mu values
  mu1 <- mu_limit_estimate(p1, limit, theta, limtype)
  mu2 <- mu_limit_estimate(p2, limit, theta, limtype)
  
  # Generate p values and corresponding mu values
  p_seq <- seq(0.005, 0.35, by = 0.005)
  mu_seq <- sapply(p_seq, function(p) mu_limit_estimate(p, limit, theta, limtype))
  
  # Calculate acceptance probabilities
  pa_seq <- sapply(p_seq, function(p) 
    Pa_estimate(p, theta, k, m, limit, limtype))
  
  # Create data frame for ggplot
  oc_data <- data.frame(
    p_nonconforming = 100 * p_seq,  # Convert to percentage
    #mu_level = 1e6 * mu_seq,        # Convert to ppm
    mu_level = uom_mapping*mu_seq, 
    pa = 100 * pa_seq               # Convert to percentage
  )
  
  # First OC Curve: Percentage Nonconforming vs Probability of Acceptance
  plot1 <- ggplot(oc_data, aes(x = p_nonconforming, y = pa)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = 100 * c(p1, p2), linetype = "dashed", color = "gray60") +
    geom_hline(yintercept = 100 * c(1 - alpha, beta), linetype = "dashed", color = "gray60") +
    labs(title = paste("OC Curve by Nonconforming Proportion (m =", m, "k =", round(k,3), ")"),
         x = "Percentage Nonconforming (%)", 
         y = "Probability of Acceptance (%)") +
    theme_minimal()
  
  #uom
  # Second OC Curve: Average Level (ppm) vs Probability of Acceptance
  plot2 <- ggplot(oc_data, aes(x = mu_level, y = pa)) +
    geom_line(color = "red", linewidth = 1) + 
    geom_hline(yintercept = 100 * c(1 - alpha, beta), linetype = "dashed", color = "gray60") +
    geom_vline(xintercept = c(mu1, mu2), linetype = "dashed", color = "gray60") +
    labs(title = paste("OC Curve by Mean Levels (m =", m, "k =", round(k,3), ")"),
         x = paste("Mean Level -", uom),
         y = "Probability of Acceptance (%)") +
    theme_minimal()
  
  # Third chart
  #plot3 <- plot_beta_distributions(p1, p2, limit, theta, limtype) 
  
  return(list(p1 = plot1, p2 = plot2))#, p3 = plot3))
}


# plot_beta_distributions <- function(p1, p2, limit, theta, limtype = c("lower", "upper")) {
#   limtype <- match.arg(limtype)
#   # Get corresponding mean levels
#   mu1 <- mu_limit_estimate(p1, limit, theta, limtype)
#   mu2 <- mu_limit_estimate(p2, limit, theta, limtype)
#   
#   # Define x-range for plotting
#   x_min <- max(0, min(mu1, mu2) * 0.8)
#   x_max <- min(1, max(mu1, mu2) * 1.2)
#   x_vals <- seq(x_min, x_max, length.out = 1000)
#   
#   # Beta distribution parameters
#   alphaU <- mu1 * theta
#   betaU <- (1 - mu1) * theta
#   alphaL <- mu2 * theta
#   betaL <- (1 - mu2) * theta
#   
#   # Compute densities
#   densityU <- dbeta(x_vals, alphaU, betaU)
#   densityL <- dbeta(x_vals, alphaL, betaL)
#   
#   # Create data frame for plotting
#   data <- data.frame(
#     x = rep(x_vals, 2), 
#     density = c(densityU, densityL), 
#     Distribution = rep(c("PRQ (µ1)", "CRQ (µ2)"), each = length(x_vals))
#   )
#   
#   # Define shading areas
#   shaded_data_U <- subset(data, x >= limit & Distribution == "PRQ (µ1)")
#   shaded_data_L <- subset(data, x >= limit & Distribution == "CRQ (µ2)")
#   
#   if (limtype == "upper") {
#     shaded_data_U <- subset(data, x <= limit & Distribution == "PRQ (µ1)")
#     shaded_data_L <- subset(data, x <= limit & Distribution == "CRQ (µ2)")
#   }
#   
#   # Create ggplot with shading and mu lines
#   plot <- ggplot(data, aes(x = x, y = density, color = Distribution, fill = Distribution)) +
#     geom_line(linewidth = 1) +
#     #geom_ribbon(data = shaded_data_U, aes(ymin = 0, ymax = density), alpha = 0.2) +
#     #geom_ribbon(data = shaded_data_L, aes(ymin = 0, ymax = density), alpha = 0.2) +
#     
#     # Add vertical lines for mu1 and mu2
#     geom_vline(xintercept = mu1, linetype = "dashed", color = "blue", linewidth = 1) +
#     geom_vline(xintercept = mu2, linetype = "dashed", color = "red", linewidth = 1) +
#     
#     # Add vertical line for the acceptance limit
#     geom_vline(xintercept = limit, linetype = "dashed", color = "black", linewidth = 1) +
#     
#     # Add labels for mu1 and mu2
#     annotate("text", x = mu1, y = 0, label = expression(mu[1]), color = "blue", hjust = -0.1) +
#     annotate("text", x = mu2, y = 0, label = expression(mu[2]), color = "red", hjust = -0.1) +
#     annotate("text", x = limit, y = 0, label = "SL", color = "black", hjust = -0.1) +
#     
#     labs(x = "Mean Levels", y = "Density", 
#          title = paste0("Beta Distributions for 2 Points Estimation")) +
#     scale_color_manual(values = c("PRQ (µ1)" = "blue", "CRQ (µ2)" = "red", "SL" = "black")) +
#     scale_fill_manual(values = c("PRQ (µ1)" = "blue", "CRQ (µ2)" = "red", "SL" = "black")) +
#     theme_minimal() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     xlim(x_min, x_max)  # Focus on the relevant x-range
#   
#   return(plot)
# }