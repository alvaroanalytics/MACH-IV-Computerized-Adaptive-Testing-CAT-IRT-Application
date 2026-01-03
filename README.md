# MACH-IV: Computerized Adaptive Testing (CAT) IRT Application

This web application implements a Computerized Adaptive Test (CAT) for the Mach-IV scale. By dynamically selecting items based on the user's previous responses, the algorithm maximizes measurement precision while reducing test length, providing real-time scoring and trait estimation of the user's Machiavellian orientation.

You can execute this application directly from your RStudio console without manually downloading files. The code includes an auto-installation script for required packages.

### Option 1: Command (Recommended)
Run the following command in your RStudio Console:

```r
shiny::runGitHub("MACH-IV-Computerized-Adaptive-Testing-CAT-IRT-Application", "alvaroanalytics")
```

### Option 2: Manual Execution
1. **Download** this repository as a ZIP file.
2. **Unzip** the folder.
3. Open the file `machApp.R` in RStudio.
4. Click the **"Run App"** button (green play icon) located at the top right of the script editor.

---

## File Structure & Description

This repository contains three essential files required for the CAT algorithms to function.

### 1. `machApp.R` (CORE FILE)
This is the main execution script. It contains:
* **Auto-installer:** Checks and installs missing libraries (i.e `mirt`, `shiny`, `mirtCAT`).
* **User Interface (UI):** The CSS styling and layout of the web application.
* **Server Logic:** The adaptive algorithm that selects the next question based on current estimation (`MI` + `MAP`).
* **Reporting:** The function that generates the final plot and interpretation text (for a friendly description and view😎).

### 2. `banco_items.rds`
Contains the **Item Bank**. It holds the text of the questions (Mach-IV scale items) and metadata required for the display.

### 3. `mod_grm.rds`
Contains the **Graded Response Model** applied to the Mach-IV file of raw puntuation/score (Likert-Type (1-5) responses).

---

## What to Expect

1. **Adaptive Logic:** The test starts with a random item. Subsequent items are selected to maximize information based on your provisional trait level.
2. **Randomesque Item Selection:** When multiple items provide similar information, the algorithm randomly selects among the top candidates. This reduces item overexposure and increases test security.
3. **No Backtracking:** Due to the adaptive nature of the test, navigation is strictly forward-only. Once an answer is submitted, it cannot be changed; therefore, participants are encouraged to click the "Next" buttom only when confident that their answer best reflects their position.
4. **Real-Time Estimation:** The engine updates your latent trait estimate (Theta, θ) after each response using Bayesian estimation.
5. **Precision-Based Stopping Rule:** The test continues until a predefined level of measurement precision is reached (SEM ≤ 0.30), with a minimum of 8 and a maximum of 15 items to ensure reliable estimation.
6. **Executive Report:** Upon completion, the app generates a concise dashboard including:
* A standardized score (0–100), derived from a proportional transformation of the observed responses, following the original test scoring approach.
* Machiavellian orientation level (Low, Moderate, High), accompanied by a brief interpretive description.
* A normal distribution plot positioning the individual relative to the normative population (point estimate only; no confidence interval).
* A dimensional breakdown of the underlying trait, reporting the mean score (1–5) for each dimension, based on the four dimensions defined in the original Christie & Geis form.


---

## Technologies Used

* **R Language:** Core programming language (plus, It's open-source😜).
* **Shiny-friendly:** Web application framework.
* **mirt & mirtCAT:** Packages for Multidimensional Item Response Theory and Computerized Adaptive Testing.
* **CSS/HTML**

---

### Author
**Álvaro González Sánchez** | *BSc **Psychology*** | *MSc **Methods, Psychometrics & Data Science** (Health & Behavioral Sciences)* | UCM & UAM

