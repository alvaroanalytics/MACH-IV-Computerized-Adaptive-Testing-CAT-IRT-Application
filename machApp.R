# ===============================================================================================
# SCRIPT: MACH-IV COMPUTERIZED ADAPTIVE TEST (WEB APP)
# ===============================================================================================
# Author:      Álvaro González Sánchez
# Affiliation: Universidad Autónoma de Madrid (UAM)
# Description: Production script for ShinyApps.io deployment. GITHUB REPOSITORIES.
#              Includes IRT logic, Executive CSS design, and dynamic reporting.
# LinkedIn: https://www.linkedin.com/in/alvarogonzalezsanchezpsicologia/overlay/background-image/
# Contact: alvarogonzalezsanchz@gmail.com 
# ===============================================================================================


library(shiny)
library(mirt)
library(mirtCAT)
library(base64enc)
library(dplyr)

# We load the item bank and the pre-calibrated IRT model. These are the brain and heart of the application: 

# df_cat <- readRDS("banco_items.rds") # Here you see the Liker-type (1-5) items and their statements.

# df_cat$Forced <- TRUE # This is critical for the CAT algorithm stability, as it requires a valid input at every step to update the Theta estimate iteratively.

# mod_grm <- readRDS("mod_grm.rds") # This is the Graded Response Model object. It will be used all the way to find the estimated thetas in real moment while every participant is responding.

# This is a critical psychometric patch. The original model expects inverted scores for reverse items,
# but our UI sends raw inputs (1-5). Instead of messing with the UI, we mathematically flip the 
# discrimination parameter (a1) to negative for these items. Now, a raw "1" correctly increases 
# the Machiavellianism estimate.
items_inversos <- c(3, 4, 6, 7, 9, 10, 11, 14, 16, 17)

for(i in items_inversos) {
  pars_originales <- mod_grm@ParObjects$pars[[i]]@par
  pars_originales[1] <- -1 * pars_originales[1]
  mod_grm@ParObjects$pars[[i]]@par <- pars_originales
}

# Defining item clusters here helps us calculate and display specific sub-dimension means in the 
# final report, giving the user more granular feedback.
dim_1_items <- c(1, 2, 12, 15)       
dim_2_items <- c(3, 6, 7, 9, 10, 16) 
dim_3_items <- c(5, 8, 13, 17, 18, 20) 
dim_4_items <- c(4, 11, 14, 19)      

# This function generates the final report. It grabs the scientific Theta estimate and translates it into a user-friendly visual experience.
final_fun_pro <- function(person) {
  
  # --- 1. EXTRACTION OF PSYCHOMETRIC DATA ---
  # We extract the final Theta (trait level)
  theta_est <- as.numeric(person$thetas[1])
  
  # NEW: We extract the final Standard Error (SEM) from the history
  # mirtCAT stores the SE evolution; we take the last value (the one at the moment of stopping).
  final_sem <- as.numeric(tail(person$thetas_SE_history, 1))
  
  # NEW: We calculate the approximate Reliability (r = 1 - SEM^2)
  # This gives us a 0-1 index of how trustworthy this specific result is.
  reliability_est <- max(0, 1 - (final_sem^2))
  
  # --- 2. DATA LOGGING (SAVING RESULTS) ---
  # This block saves the psychometric properties of this session to a CSV file.
  # "append = TRUE" ensures we add a new row for each participant without deleting previous ones.
  
  log_data <- data.frame(
    Date = as.character(Sys.time()),
    Theta = round(theta_est, 3),
    SEM = round(final_sem, 3),
    Reliability = round(reliability_est, 3),
    Items_Answered = length(person$responses) # Tracks if they stopped early
  )
  
  # Check if file exists to write headers only once
  if (!file.exists("registro_participantes.csv")) {
    write.table(log_data, "registro_participantes.csv", sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    write.table(log_data, "registro_participantes.csv", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
  # --- 3. VISUAL SCORING LOGIC (EXISTING) ---
  # Transformation to T-Score (0-100)
  visual_score <- round((theta_est * 15) + 50, 0)
  if (visual_score > 99) visual_score <- 99
  if (visual_score < 1) visual_score <- 1
  
  # Text feedback logic
  if (theta_est >= 0.8) {
    accent_color <- "#e74c3c"; level_label <- "High Machiavellian Orientation"
    level_desc <- "Profile characterized by a distinct pragmatic detachment, strategic calculation in interpersonal management, and an instrumental view of social structures."
  } else if (theta_est <= -0.8) {
    accent_color <- "#27ae60"; level_label <- "Low Machiavellian Orientation"
    level_desc <- "Profile exhibiting a strong inclination towards cooperative engagement, empathetic transparency, and a reliance on normative ethical standards."
  } else {
    accent_color <- "#f39c12"; level_label <- "Moderate Machiavellian Orientation"
    level_desc <- "Balanced profile reflecting a situational adaptability between strategic self-interest and collaborative trust, contingent upon environmental cues."
  }
  
  # --- 4. SUB-DIMENSION CALCULATION (EXISTING) ---
  responses <- person$responses
  reverse_items <- c(3, 4, 6, 7, 9, 10, 11, 14, 16, 17)
  processed_values <- numeric()
  answered_items <- integer()
  
  for (i in seq_along(responses)) {
    val <- responses[i]
    if (!is.na(val)) {
      val_1to5 <- val + 1
      val_final <- if (i %in% reverse_items) 6 - val_1to5 else val_1to5
      processed_values <- c(processed_values, val_final)
      answered_items   <- c(answered_items, i)
    }
  }
  
  calc_dimension <- function(item_indices) {
    valid_items <- intersect(item_indices, answered_items)
    if (length(valid_items) == 0) return(NULL)
    vals <- processed_values[match(valid_items, answered_items)]
    mean(vals)
  }
  
  # --- 5. PLOT GENERATION (EXISTING) ---
  tf <- tempfile(fileext = ".png")
  png(tf, width = 800, height = 400, res = 140)
  par(mar = c(2, 0, 1, 0), bg = NA) 
  x <- seq(-3.5, 3.5, length = 300)
  y <- dnorm(x)
  plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-3.5, 3.5), ylim = c(0, 0.48))
  polygon(c(min(x), x, max(x)), c(0, y, 0), col = "#d4e6f1", border = "#a9cce3", lwd = 1.5)
  abline(v = theta_est, col = "#2c3e50", lwd = 1.5, lty = 2)
  y_point <- dnorm(theta_est)
  points(theta_est, y_point, pch = 21, bg = "#34495e", col = "white", cex = 2, lwd = 2)
  text(x = theta_est, y = y_point, labels = "You", pos = 3, offset = 0.9, col = "#2c3e50", font = 2, cex = 1.1, family = "sans")
  axis(1, at = c(-3, -1.5, 0, 1.5, 3), labels = c("Low", "", "Average", "", "High"), col = NA, col.ticks = NA, col.axis = "#7f8c8d", cex.axis = 0.8, font = 2)
  dev.off()
  plot_base64 <- base64enc::dataURI(file = tf, mime = "image/png")
  
  # --- 6. HTML REPORT GENERATION ---
  # Included CSS for the report layout
  css_report <- paste0("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600&display=swap');
    .main-container { font-family: 'Inter', sans-serif; background-color: #f8f9fa; padding: 40px; max-width: 900px; margin: 0 auto; border-radius: 16px; color: #2c3e50; }
    .header { text-align: center; margin-bottom: 40px; }
    .header h2 { font-weight: 600; letter-spacing: -0.5px; margin: 0; color: #2c3e50; }
    .grid-box { display: flex; gap: 24px; flex-wrap: wrap; justify-content: center; align-items: stretch; }
    .info-card { background: #ffffff; border-radius: 12px; padding: 26px; flex: 1; min-width: 300px; box-shadow: 0 4px 20px rgba(0,0,0,0.05); text-align: center; transition: transform 0.2s ease; display: flex; flex-direction: column; justify-content: center; }
    .info-card:hover { transform: translateY(-3px); }
    .info-card h4 { text-transform: uppercase; font-size: 0.75em; letter-spacing: 1px; font-weight: 600; color: #95a5a6; margin: 0 0 15px 0; }
    .score-ring { width: 120px; height: 120px; border-radius: 50%; border: 5px solid ", accent_color, "; display: flex; align-items: center; justify-content: center; margin: 10px auto 20px auto; font-size: 2.5em; font-weight: 700; color: ", accent_color, "; }
    .chart-container { margin-top: 5px; width: 100%; border-radius: 8px; overflow: hidden; }
    .chart-img { width: 100%; height: auto; display: block; }
    .dim-row { display: flex; justify-content: space-between; padding: 14px 0; border-bottom: 1px solid #f1f2f6; font-size: 0.95em; }
    .dim-row:last-child { border-bottom: none; }
    .pill { background: #f1f2f6; padding: 4px 12px; border-radius: 20px; font-weight: 600; font-size: 0.85em; color: #2c3e50; }
    /* NEW CSS FOR TECHNICAL FOOTER */
    .tech-footer { margin-top: 30px; background-color: #e9ecef; padding: 15px; border-radius: 8px; font-size: 0.85em; color: #636e72; display: flex; justify-content: space-between; align-items: center; }
  ")
  
  div(
    tags$style(HTML(css_report)),
    div(class = "main-container",
        div(class = "header", h2("Executive Assessment Report"), p(style = "color:#95a5a6; font-size:0.85em; margin-top:5px;", paste("Generated on:", format(Sys.Date(), "%B %d, %Y")))),
        div(class = "grid-box",
            div(class = "info-card", h4("Standardized Score"), div(class = "score-ring", visual_score), h3(level_label, style = paste0("color:", accent_color, "; margin:5px 0; font-size:1.1em;")), p(level_desc, style = "font-size:0.9em; color:#7f8c8d; line-height:1.5;")),
            div(class = "info-card", h4("Latent Trait Distribution"), div(style="display:flex; justify-content:space-around; width:100%; margin-bottom:5px;", div(p("Theta (θ)", style="margin:0; font-size:0.8em; color:#95a5a6;"), h2(sprintf("%.2f", theta_est), style="margin:0; color:#2c3e50;")), div(p("Percentile", style="margin:0; font-size:0.8em; color:#95a5a6;"), h2(paste0(round(pnorm(theta_est) * 100, 0), "%"), style="margin:0; color:#2c3e50;"))), div(class = "chart-container", tags$img(src = plot_base64, class = "chart-img")), p("Position relative to the normative population curve.", style="font-size:0.75em; color:#bdc3c7; margin-top:10px;"))
        ),
        br(),
        div(class = "info-card", style = "text-align:left; max-width:100%; align-items: stretch;", h4("Dimensional Breakdown (Likert Mean 1–5)"),
            div(lapply(list(list("Negative Tactics", calc_dimension(dim_1_items)), list("Positive Tactics", calc_dimension(dim_2_items)), list("Cynical Worldview", calc_dimension(dim_3_items)), list("Positive Worldview",calc_dimension(dim_4_items))), function(x) { if (is.null(x[[2]])) return(NULL); div(class = "dim-row", span(x[[1]], style="color:#57606f;"), span(class = "pill", sprintf("%.1f", x[[2]])))}))),
        
        # --- NEW TECHNICAL DATASHEET SECTION ---
        div(class = "tech-footer",
            div(strong("Psychometric Precision:")),
            div(paste0("SEM: ", round(final_sem, 2), " | Reliability: ", round(reliability_est, 2))),
            div(style = "font-size: 0.9em; font-style: italic;", 
                if(reliability_est > 0.90) "Excellent Accuracy" else if(reliability_est > 0.80) "Good Accuracy" else "Moderate Accuracy")
        ),
        
        br(),
        div(style="text-align:center;",
            tags$button(class="exit-btn", onclick="window.location.href='https://www.uam.es';", "Exit Questionnaire")
        )
    )
  )
}

# This CSS ensures a polished, professional look for the testing interface itself (the questions), overriding the default Shiny styles with something cleaner.
css_global_pro <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;800&display=swap');
  body { background-color: #f0f2f5; font-family: 'Inter', sans-serif; color: #2c3e50; }
  .well { background-color: transparent; border: none; box-shadow: none; padding: 0; }
  .shiny-text-output, #Question {; font-size: 3.5em !important; font-weight: 800 !important; color: #1a252f; margin-bottom: 40px; line-height: 1.3; letter-spacing: -0.8px; text-align: center; max-width: 900px; margin-left: auto; margin-right: auto; }
  .radio label { background-color: #ffffff; border: 2px solid #e1e8ed; border-radius: 16px; padding: 22px 30px; margin-bottom: 18px; width: 100%; font-size: 1.25em; font-weight: 500; color: #4b5563; cursor: pointer; transition: all 0.2s cubic-bezier(0.25, 0.8, 0.25, 1); display: flex; align-items: center; box-shadow: 0 4px 6px rgba(0,0,0,0.01); }
  .radio label:hover { border-color: #2c3e50; background-color: #ffffff; transform: translateY(-3px); box-shadow: 0 10px 25px rgba(44, 62, 80, 0.12); color: #111827; }
  .radio input[type='radio'] { margin-top: 0; margin-right: 15px; transform: scale(1.25); accent-color: navy; }
  .shiny-output-error-validation { color: #c0392b !important; font-weight: 700; font-size: 1.1em; background-color: #fadbd8; border-left: 5px solid #c0392b; padding: 15px; border-radius: 4px; margin-top: 20px; text-transform: uppercase; letter-spacing: 0.5px; box-shadow: 0 2px 10px rgba(192, 57, 43, 0.1); }
  .shiny-output-error-validation:before { content: 'REQUIRED: '; margin-right: 5px; }
  .action-button { background-color: #2c3e50 !important; border: none !important; color: white !important; font-weight: 500; padding: 15px 40px; font-size: 1.1em; border-radius: 7px; transition: all 0.3s ease; letter-spacing: 1px; box-shadow: 0 4px 6px rgba(44, 62, 80, 0.2); margin-top: 30px; }
  .action-button:hover { transform: translateY(-2px); box-shadow: 0 8px 15px rgba(44, 62, 80, 0.3); background-color: #1a252f !important; }
  .progress { height: 10px; border-radius: 5px; background-color: #e1e8ed; margin-bottom: 40px; }
  .progress-bar { background-color: #27ae60; }
  .welcome-container { background: white; padding: 60px; border-radius: 24px; box-shadow: 0 20px 60px rgba(0,0,0,0.08); text-align: center; max-width: 900px; margin: 50px auto; }
  .brand-logo { font-weight: 800; color: #9ca3af; letter-spacing: 3px; font-size: 0.85rem; margin-bottom: 25px; }
  .welcome-container h1 { font-weight: 900; color: #111827; letter-spacing: -2px; margin-top: 0; font-size: 4rem; line-height: 1.1; }
  .welcome-container h3 { font-weight: 400; color: #6b7280; margin-bottom: 50px; font-size: 1.4rem; margin-top: 10px; }
  .intro-box { background: #f9fafb; padding: 35px; border-radius: 12px; text-align: left; margin-bottom: 40px; border-left: 6px solid #2c3e50; }
"

first_page_content <- list(
  div(class = "welcome-container",
      div(class = "brand-logo", "UAM PSYCHOMETRIC ASSESSMENT SERIES"),
      h1("MACH-IV: CAT Application"),
      h3("Computerized Adaptive Testing (CAT) Environment in RStudio"),
      div(class = "intro-box",
          p("Welcome. This instrument is designed to evaluate interpersonal Machiavellianism orientation using a probabilistic item response model (IRT)."),
          tags$ul(class = "instructions-list",
                  tags$li("The algorithm adjusts difficulty in real-time based on your responses."),
                  tags$li("Estimated completion time: 1-3 minutes."),
                  tags$li("All data is processed anonymously for psychometric calibration."),
                  tags$li("Individuals with a minimum English proficiency level of B2 are expected to perform well.")
          )
      ),
      p(class = "footer-note", "Please click 'Next' to proceed."),
      p(
        style = "margin-top:20px; font-size:0.85em; color:#7f8c8d;",
        "Source code and documentation: ",
        tags$a(
          href = "https://github.com/alvaroanalytics/MACH-IV-Computerized-Adaptive-Testing-CAT-IRT-Application",
          target = "_blank",
          "González, A. (2026): GitHub repository"
        )
      ),
      p(
        style = "margin-top:30px; font-size:0.8em; color:#7f8c8d; text-align:center;",
        HTML("Reference:<br/>Christie, R., &amp; Geis, F. L. (1970). <em>Studies in Machiavellianism</em>. New York: Academic Press.")
      )
  )
)

gui_props <- list(
  title     = "MACH-IV CAT Assessment",
  authors   = "Original by Christie & Geis (1970) | CAT application by Álvaro González Sánchez (UAM, 2026).",
  firstpage = first_page_content,
  lastpage  = final_fun_pro,
  css       = css_global_pro
)

# We increase the minimum items to 10. This gives the MAP algorithm enough data points to 
# confidently estimate the trait before stopping, ensuring higher reliability.

n_items <- nrow(df_cat)
vec_exposure <- rep(3, n_items)

design <- list(min_items = 8, 
               max_items = 15, 
               min_SEM = 0.30,
               exposure = vec_exposure # Applying Randomesque (3)
)

# Finally, we launch the test. Using 'MAP' (Maximum A Posteriori) is crucial here because it allows 
# the score to move to extremes (high/low Machiavellianism) faster than the default method.

mirtCAT(
  df = df_cat,
  mo = mod_grm,   
  method = "MAP", 
  criteria = "MI",
  start_item = "random",
  design = design,
  shinyGUI = gui_props
)
