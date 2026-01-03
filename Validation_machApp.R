# ===============================================================================================
# MODULE: MONTE CARLO SIMULATION & PSYCHOMETRIC VALIDATION (DATA ANALYSIS)
# ===============================================================================================
# NOTE: This section is for validation purposes (offline analysis). It implements the logic 
# found in the Data Analysis section (Han, 2018b).
# Author:      Álvaro González Sánchez
# Affiliation: Universidad Autónoma de Madrid (UAM)
# Description: Validation purposes for the `machApp.R` found at \alvaroanalytics in GITHUB REPOSITORIES.
# LinkedIn: https://www.linkedin.com/in/alvarogonzalezsanchezpsicologia/overlay/background-image/
# Contact: alvarogonzalezsanchz@gmail.com 
# ===============================================================================================

# 1. Simulation Setup
# We simulate N virtual examinees to test the precision of the current CAT configuration.
n_simulaciones <- 10000
true_thetas <- matrix(rnorm(n_simulaciones, mean = 0, sd = 1), ncol = 1) # True Theta (Population)

# We use the same design object defined in the production app (`machApp.R`) to ensure ecological validity.
# The 'generate_pattern' argument creates response patterns based on the calibrated mod_grm model.
results_sim <- mirtCAT(
  mo = mod_grm, 
  local_pattern = generate_pattern(mod_grm, Theta = true_thetas), 
  start_item = "random",
  criteria = "MI", 
  design = design, # Uses your specific stopping rules (min_SEM = 0.30, max_items = 15), found at `machApp.R` 
  method = "MAP"
)

# 2. Data Extraction
# We extract the Estimated Theta (Theta_hat) produced by the CAT algorithm for each simulant:
estimated_thetas <- sapply(results_sim, function(x) x$thetas)
diffs <- estimated_thetas - as.vector(true_thetas) # (Theta_hat - Theta_true)

# 3. Calculation of Psychometric Indices (As per Data Analysis Section)

# --- BIAS ---
# "The average difference between estimated and true theta across all candidates."
# Bias = Sum(theta_hat - theta_true) / I
bias_stat <- mean(diffs)

# --- Mean Absolute Error [MAE] ---
# "A statistic that depicts the overall measurement error... computed as the means absolute values."
# MAE = Sum(|theta_hat - theta_true|) / I
mae_stat <- mean(abs(diffs))

# --- Root Mean Square Error ---
# "The square of the bias and the square root of the result."
# RMSE = Sqrt( Sum((theta_hat - theta_true)^2) / I )
rmse_stat <- sqrt(mean(diffs^2))

# --- Conditional Analysis (CBIAS, CMAE, CRMSE) ---
# As noted in the text: "Characteristics of the tests may differ considerably across the range of theta."
# We categorize the true thetas into the bins suggested: < -2, -2 to -1, -1 to 0, etc.
results_df <- data.frame(
  True_Theta = as.vector(true_thetas),
  Est_Theta = estimated_thetas,
  Diff = diffs,
  Abs_Diff = abs(diffs),
  Sq_Diff = diffs^2
)

results_df$Condition <- cut(results_df$True_Theta, 
                            breaks = c(-Inf, -2, -1, 0, 1, 2, Inf),
                            labels = c("< -2", "-2 to -1", "-1 to 0", "0 to 1", "1 to 2", "> 2"))

conditional_stats <- results_df %>%
  group_by(Condition) %>%
  summarise(
    N = n(),
    CBIAS = mean(Diff),           # Conditional Bias
    CMAE = mean(Abs_Diff),        # Conditional MAE
    CRMSE = sqrt(mean(Sq_Diff)),  # Conditional RMSE
    Avg_Items = mean(sapply(results_sim, function(x) length(x$items_answered))) # Efficiency check
  )

# 4. Reporting
cat("\n MACH-IV CAT VALIDATION REPORT \n")
cat("Overall BIAS: ", round(bias_stat, 4), "\n")
cat("Overall MAE: ", round(mae_stat, 4), "\n")
cat("Overall RMSE: ", round(rmse_stat, 4), "\n\n")
print(conditional_stats)

# ===============================================================================================
# MODULE: VISUALIZATION OF PSYCHOMETRIC PROPERTIES (PLOTS)
# ===============================================================================================
# Description: Generates plots to visualize the validation results calculated above.
# ===============================================================================================

library(ggplot2)

# 1. SCATTER PLOT: Parameter Recovery (True Theta vs. Estimated Theta)
# Visualizes the global precision

plot_recovery <- ggplot(results_df, aes(x = True_Theta, y = Est_Theta)) +
  geom_point(alpha = 0.4, color = "#2c3e50") +  # Semi-transparent points to see density
  geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed", size = 1) + # Perfect correlation line
  geom_smooth(method = "loess", color = "#3498db", se = FALSE, size = 0.8) + # Observed trend line
  labs(
    title = "Parameter Recovery: True vs. Estimated Theta",
    subtitle = paste0("N = ", n_simulaciones, " | Red Dashed Line = Perfect Estimation"),
    x = "True Theta (θ)",
    y = "Estimated Theta (θ̂)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# 2. CONDITIONAL RMSE PLOT
# Visualizes where the test is most accurate. Lower bars = Higher precision.

plot_crmse <- ggplot(conditional_stats, aes(x = Condition, y = CRMSE)) +
  geom_col(fill = "#2c3e50", width = 0.7) +
  geom_text(aes(label = round(CRMSE, 3)), vjust = -0.5, fontface = "bold", color = "#2c3e50") +
  labs(
    title = "Conditional RMSE (Precision by Ability Level)",
    subtitle = "Root Mean Square Error across Theta ranges (Lower is better)",
    x = "Theta Range (Ability Level)",
    y = "RMSE (Measurement Error)"
  ) +
  ylim(0, max(conditional_stats$CRMSE) * 1.2) + # Add headroom for labels
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# --- EXECUTE PLOTS ---
print(plot_recovery)
print(plot_crmse)

# Optional: Save plots to disk for the report
# ggsave("machApp_recovery.png", plot_recovery, width = 8, height = 6)
# ggsave("machApp_crmse.png", plot_crmse, width = 8, height = 6)