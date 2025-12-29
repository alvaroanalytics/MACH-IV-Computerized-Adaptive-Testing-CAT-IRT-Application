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

# We start by loading the 'brain' of the test from the files you uploaded. 
# The server needs these to know the questions (df_cat) and the math behind them (mod_grm).
df_cat <- readRDS("banco_items.rds")
df_cat$Forced <- TRUE 

mod_grm <- readRDS("mod_grm.rds")

# Here I'm defining which items belong to which dimension so the final report can calculate sub-scores easily.
dim_1_items <- c(1, 2, 12, 15)       
dim_2_items <- c(3, 6, 7, 9, 10, 16) 
dim_3_items <- c(5, 8, 13, 17, 18, 20) 
dim_4_items <- c(4, 11, 14, 19)      

# This helper function converts the simple 0-100 score into a statistical Theta (-3 to +3).
# We define it here so the server knows how to map the user on the bell curve later.
obtener_theta_realista <- function(raw_score_0_100) {
  theta <- (raw_score_0_100 - 50) / 15
  
  # I'm adding safety limits to keep the dot inside the graph boundaries.
  if(theta > 3.5) theta <- 3.5
  if(theta < -3.5) theta <- -3.5
  
  return(theta)
}

# This is the main function that runs the moment the user finishes the test.
final_fun_pro <- function(person) {
  
  # --- PROCESAMIENTO ---
  responses <- person$responses
  reverse_items <- c(3, 4, 6, 7, 9, 10, 11, 14, 16, 17)
  
  answered_items <- integer()
  processed_values <- numeric()
  
  for (i in seq_along(responses)) {
    val <- responses[i]
    if (!is.na(val)) {
      val_1to5 <- val + 1
      val_final <- if (i %in% reverse_items) 6 - val_1to5 else val_1to5
      processed_values <- c(processed_values, val_final)
      answered_items   <- c(answered_items, i)
    }
  }
  
  # Scores
  raw_sum <- sum(processed_values)
  n_answered <- length(processed_values)
  raw_score <- 0
  if (n_answered > 0) {
    max_possible <- n_answered * 5
    raw_score <- round((raw_sum / max_possible) * 100, 0)
  }
  
  # Theta
  theta_est <- tryCatch(obtener_theta_realista(raw_score), error = function(e) person$thetas[1])
  if (is.na(theta_est)) theta_est <- person$thetas[1]
  
  # Helper Dimensiones
  calc_dimension <- function(item_index) {
    v <- processed_values[match(item_index, answered_items)]
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NULL)
    mean(v)
  }
  
  # --- GRAPHIC ---
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
  
  # --- LOGIC ---
  if (raw_score >= 75) {
    accent_color <- "#e74c3c"; level_label <- "High Machiavellian Orientation"
    level_desc <- "Profile characterized by a distinct pragmatic detachment, strategic calculation in interpersonal management, and an instrumental view of social structures."
  } else if (raw_score <= 45) {
    accent_color <- "#27ae60"; level_label <- "Low Machiavellian Orientation"
    level_desc <- "Profile exhibiting a strong inclination towards cooperative engagement, empathetic transparency, and a reliance on normative ethical standards."
  } else {
    accent_color <- "#f39c12"; level_label <- "Moderate Machiavellian Orientation"
    level_desc <- "Balanced profile reflecting a situational adaptability between strategic self-interest and collaborative trust, contingent upon environmental cues."
  }
  
  # --- CSS ---
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
  ")
  
  # --- HTML ---
  div(
    tags$style(HTML(css_report)),
    div(class = "main-container",
        div(class = "header", h2("Executive Assessment Report"), p(style = "color:#95a5a6; font-size:0.85em; margin-top:5px;", paste("Generated on:", format(Sys.Date(), "%B %d, %Y")))),
        div(class = "grid-box",
            div(class = "info-card", h4("Standardized Score"), div(class = "score-ring", raw_score), h3(level_label, style = paste0("color:", accent_color, "; margin:5px 0; font-size:1.1em;")), p(level_desc, style = "font-size:0.9em; color:#7f8c8d; line-height:1.5;")),
            div(class = "info-card", h4("Latent Trait Distribution"), div(style="display:flex; justify-content:space-around; width:100%; margin-bottom:5px;", div(p("Theta (θ)", style="margin:0; font-size:0.8em; color:#95a5a6;"), h2(sprintf("%.2f", theta_est), style="margin:0; color:#2c3e50;")), div(p("Percentile", style="margin:0; font-size:0.8em; color:#95a5a6;"), h2(paste0(round(pnorm(theta_est) * 100, 0), "%"), style="margin:0; color:#2c3e50;"))), div(class = "chart-container", tags$img(src = plot_base64, class = "chart-img")), p("Position relative to the normative population curve.", style="font-size:0.75em; color:#bdc3c7; margin-top:10px;"))
        ),
        br(),
        div(class = "info-card", style = "text-align:left; max-width:100%; align-items: stretch;", h4("Dimensional Breakdown (Likert Mean 1–5)"),
            div(lapply(list(list("Negative Tactics", calc_dimension(dim_1_items)), list("Positive Tactics", calc_dimension(dim_2_items)), list("Cynical Worldview", calc_dimension(dim_3_items)), list("Positive Worldview",calc_dimension(dim_4_items))), function(x) { if (is.null(x[[2]])) return(NULL); div(class = "dim-row", span(x[[1]], style="color:#57606f;"), span(class = "pill", sprintf("%.1f", x[[2]])))})))
    )
  )
}

# This giant block of CSS styling makes the actual testing interface (questions and buttons)
# It overrides the default look to provide large text, card-style options, and better error messages. Some text might be in Spanish:
css_global_pro <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;800&display=swap');

  /* --- GLOBAL SETUP --- */
  body { 
    background-color: #f0f2f5; /*  */
    font-family: 'Inter', sans-serif; 
    color: #2c3e50; 
  }

  /* --- LIMPIEZA DE SHINY --- */
  .well { 
    background-color: transparent; 
    border: none; 
    box-shadow: none; 
    padding: 0; 
  }
  
  /* --- ESTÉTICA DE LA PREGUNTA (MUY IMPORTANTE) --- */
  /* Hacemos la pregunta grande y clara */
  .shiny-text-output {
    font-size: 2.2em;      /* Mucho más grande */
    font-weight: 700;      /* Negrita sólida */
    color: #1a252f;        /* Gris casi negro para contraste máximo */
    margin-bottom: 40px;   
    line-height: 1.3;      /* Más espacio entre líneas si la frase es larga */
    letter-spacing: -0.8px; /* Un pelín más juntas las letras */
    text-align: center;    /* Centramos la pregunta */
    max-width: 900px;      /* Evitamos que se estire demasiado en pantallas anchas */
    margin-left: auto;
    margin-right: auto;
  }

  /* --- TRANSFORMACIÓN DE LAS RESPUESTAS (De radio buttons a TARJETAS) --- */
  /* Esto convierte las opciones en cajas clicables */
  .radio label { 
    background-color: #ffffff;
    border: 2px solid #e1e8ed; 
    border-radius: 16px;       /* Bordes un poco más redondeados */
    padding: 22px 30px;        /* Más relleno interno */
    margin-bottom: 18px;       
    width: 100%;               
    font-size: 1.25em;         /* Texto de opciones más grande */
    font-weight: 500;
    color: #4b5563;
    cursor: pointer;
    transition: all 0.2s cubic-bezier(0.25, 0.8, 0.25, 1);
    display: flex;             
    align-items: center;
    box-shadow: 0 4px 6px rgba(0,0,0,0.01); /* Sombra muy sutil por defecto */
  }

  /* Efecto al pasar el ratón por encima (Hover) */
  .radio label:hover {
    border-color: #2c3e50;     /* Borde oscuro al pasar ratón */
    background-color: #ffffff; 
    transform: translateY(-3px); 
    box-shadow: 0 10px 25px rgba(44, 62, 80, 0.12); /* Sombra difusa de calidad */
    color: #111827;
  }

  /* Estilo del circulito del radio button */
  .radio input[type='radio'] {
    margin-top: 0;
    margin-right: 15px; /* Separar el círculo del texto */
    transform: scale(1.25); /* Hacer el círculo un poco más grande */
    accent-color: navy; /* Color del punto cuando se selecciona */
  }

  /* --- MENSAJE DE ERROR (OBLIGATORIO CONTESTAR) --- */
  /* Transformamos el error aburrido en una alerta roja profesional */
  .shiny-output-error-validation {
    color: #c0392b !important;
    font-weight: 700;
    font-size: 1.1em;
    background-color: #fadbd8;
    border-left: 5px solid #c0392b;
    padding: 15px;
    border-radius: 4px;
    margin-top: 20px;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    box-shadow: 0 2px 10px rgba(192, 57, 43, 0.1);
  }
  
  /* Texto personalizado para el error (usando pseudo-elemento CSS) */
  /* Esto pone 'ATTENTION:' antes del mensaje */
  .shiny-output-error-validation:before {
    content: 'REQUIRED: ';
    margin-right: 5px;
  }

  /* --- BOTONES DE NAVEGACIÓN --- */
  .action-button { 
    background-color: #2c3e50 !important; 
    border: none !important; 
    color: white !important; 
    font-weight: 500; 
    padding: 15px 40px; 
    font-size: 1.1em;
    border-radius: 7px; 
    transition: all 0.3s ease; 
    letter-spacing: 1px;
    box-shadow: 0 4px 6px rgba(44, 62, 80, 0.2);
    margin-top: 30px;
  }
  
  .action-button:hover { 
    transform: translateY(-2px); 
    box-shadow: 0 8px 15px rgba(44, 62, 80, 0.3);
    background-color: #1a252f !important;
  }

  /* --- BARRA DE PROGRESO --- */
  .progress {
    height: 10px;
    border-radius: 5px;
    background-color: #e1e8ed;
    margin-bottom: 40px;
  }
  .progress-bar {
    background-color: #27ae60; /*  */
  }

  /* --- PANTALLA DE BIENVENIDA --- */
  ..welcome-container {
    background: white; padding: 60px; border-radius: 24px;
    box-shadow: 0 20px 60px rgba(0,0,0,0.08); text-align: center; max-width: 900px; margin: 50px auto;
  }
  .brand-logo { 
    font-weight: 800; color: #9ca3af; letter-spacing: 3px; font-size: 0.85rem; margin-bottom: 25px; 
  }
  
  /* Aquí hacemos el título gigante */
  .welcome-container h1 { 
    font-weight: 900;       /* Ultra Bold */
    color: #111827;         /* Negro casi puro */
    letter-spacing: -2px;   /* Letras más pegadas (estilo Apple/Nike) */
    margin-top: 0; 
    font-size: 4rem;        /* TAMAÑO GIGANTE (antes era 2.5rem) */
    line-height: 1.1;
  }
  
  .welcome-container h3 { 
    font-weight: 400; color: #6b7280; margin-bottom: 50px; font-size: 1.4rem; margin-top: 10px;
  }
  
  .intro-box { 
    background: #f9fafb; padding: 35px; border-radius: 12px; 
    text-align: left; margin-bottom: 40px; border-left: 6px solid #2c3e50; 
  }
"

# Defining the Welcome Page with clear instructions that back-navigation is disabled.
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

# Bundling everything together into the GUI properties list. 
# 'forced_choice' ensures the user cannot skip questions.
gui_props <- list(
  title     = "MACH-IV CAT Assessment",
  authors   = "Original by Christie & Geis (1970) | CAT application by González (2026). 
  See more at 'Source code and documentation'",
  firstpage = first_page_content,
  lastpage  = final_fun_pro,
  css       = css_global_pro
)

# Setting the rules for the Adaptive Test: length of test and precision requirements.
design <- list(min_items = 5, max_items = 10, min_SEM = 0.35)

# Finally, we launch the application. This is the command that starts the web server.
mirtCAT(
  df = df_cat,
  mo = mod_grm,
  method = "EAP",
  criteria = "MI",
  start_item = "random",
  design = design,
  shinyGUI = gui_props
)
