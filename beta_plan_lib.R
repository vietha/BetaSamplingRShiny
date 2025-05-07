require(AccSamplingDesign)
require(ggplot2)

# Function to plot OC curves, could define custom p_seq
plot_OC_curve <- function(plan, uom, uom_mapping, p_seq = NULL) {
  
  p1 <- plan$PRQ
  p2 <- plan$CRQ
  alpha <- plan$PR
  beta <- plan$CR
  
  # estimate mu from PRQ/CRQ
  mu1 <- muEst(plan$PRQ, USL = plan$USL, LSL = plan$LSL, 
               theta = plan$theta, dist = plan$distribution)
  mu2 <- muEst(plan$CRQ, USL = plan$USL, LSL = plan$LSL,  
               theta = plan$theta, dist = plan$distribution)   
  
  plan_data <- OCdata(plan, pd = p_seq)
  p_seq <- plan_data@pd
  mu_seq <- plan_data@process_means
  pa_seq <- plan_data@paccept
  
  
  # Create data frame for ggplot
  plot_data <- data.frame(
    p_nonconforming = 100 * p_seq,  # Convert to percentage
    #mu_level = 1e6 * mu_seq,        # Convert to ppm
    mu_level = uom_mapping*mu_seq, 
    pa = 100 * pa_seq               # Convert to percentage
  )
  
  # First OC Curve: Percentage Nonconforming vs Probability of Acceptance
  plot1 <- ggplot(plot_data, aes(x = p_nonconforming, y = pa)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = 100 * c(p1, p2), linetype = "dashed", color = "gray60") +
    geom_hline(yintercept = 100 * c(1 - alpha, beta), linetype = "dashed", color = "gray60") +
    labs(title = paste("OC Curve by Nonconforming Proportion (m =", plan$m, "k =", round(plan$k,3), ")"),
         x = "Percentage Nonconforming (%)", 
         y = "Probability of Acceptance (%)") +
    theme_minimal()
  
  #uom
  # Second OC Curve: Average Level (ppm) vs Probability of Acceptance
  plot2 <- ggplot(plot_data, aes(x = mu_level, y = pa)) +
    geom_line(color = "red", linewidth = 1) + 
    geom_hline(yintercept = 100 * c(1 - alpha, beta), linetype = "dashed", color = "gray60") +
    geom_vline(xintercept = c(mu1, mu2), linetype = "dashed", color = "gray60") +
    labs(title = paste("OC Curve by Mean Levels (m =", plan$m, "k =", round(plan$k,3), ")"),
         x = paste("Mean Level -", uom),
         y = "Probability of Acceptance (%)") +
    theme_minimal()
  
  # Third chart
  #plot3 <- plot_beta_distributions(p1, p2, limit, theta, limtype) 
  
  return(list(p1 = plot1, p2 = plot2))#, p3 = plot3))
}


# # Function to estimate mu corresponding to a given proportion nonconforming (p)
# mu_limit_estimate <- function(p, limit, theta, limtype = c("lower", "upper")) {
#   limtype <- match.arg(limtype)
#   USL = LSL = NULL
#   if(limtype == "lower"){
#     LSL = limit
#   } else if(limtype == "upper") {
#     USL = limit
#   }
#   
#   mu <- muEst(p, USL = USL, LSL = LSL, theta = theta, dist = "beta")  
#   return(mu)
# }
# 
# # Function to estimate probability of acceptance (Pa)
# Pa_estimate <- function(p, theta, k, m, limit, limtype = c("lower", "upper")) {
#   limtype <- match.arg(limtype)
#   USL = LSL = NULL
#   if(limtype == "lower"){
#     LSL = limit
#   } else if(limtype == "upper") {
#     USL = limit
#   }
#   
#   planObj <- structure(list(m = m, k = k, USL = USL, LSL = LSL,
#                             theta = theta, theta_type = "known",
#                             distribution = "beta"), 
#                        class = "VarPlan")
#   
#   pa <- accProb(planObj, p)
#   return(pa)
# }
