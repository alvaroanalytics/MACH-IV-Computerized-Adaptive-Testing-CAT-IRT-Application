# MACH-IV: Computerized Adaptive Testing (CAT) IRT Application

This web application implements a Computerized Adaptive Test (CAT) for the Mach-IV scale. By dynamically selecting items based on the user's previous responses, the algorithm maximizes measurement precision while significantly reducing test length: providing real-time scoring and trait estimation of the user's Machiavellian orientation.

You can execute this application directly from your RStudio console without manually downloading files. The code includes an auto-installation script for required packages.

### Option 1: Command (Recommended)
Run the following command in your RStudio Console:

```r
shiny::runGitHub("MACH-IV-Computerized-Adaptive-Testing-CAT-IRT-Application", "alvaroanalytics")
```

### Option 2: Manual Execution
1. **Download** this repository as a ZIP file.
2. **Unzip** the folder.
3. Open the file `app.R` in RStudio.
4. Click the **"Run App"** button (green play icon) located at the top right of the script editor.

---

## File Structure & Description

This repository contains three essential files required for the CAT algorithms to function.

### 1. `app.R` (CORE FILE)
This is the main execution script. It contains:
* **Auto-installer:** Checks and installs missing libraries (`mirt`, `shiny`, `mirtCAT`).
* **User Interface (UI):** The CSS styling and layout of the web application.
* **Server Logic:** The adaptive algorithm that selects the next question based on current estimation.
* **Reporting:** The function that generates the final plot and interpretation text.

### 2. `banco_items.rds`
Contains the **Item Bank**. It holds the text of the questions (Mach-IV scale items) and metadata required for the display.

### 3. `mod_grm.rds`
Contains the **Graded Response Model** applied to the Mach-IV file of raw puntuation/score (Likert-Type (1-5) responses).

---

## What to Expect

1. **Adaptive Logic:** The test will start with a random item. Subsequent items are selected to maximize information based on your provisional trait level.
2. **No Backtracking:** Due to the adaptive nature of the test, navigation is strictly forward-only.
3. **Real-Time Estimation:** The underlying engine recalculates your Theta (θ) after every response.
4. **Executive Report:** Upon completion, the app generates a dashboard showing:
    * Standardized Score (0-100).
    * Machiavellian Orientation Level (Low, Moderate, High).
    * A Normal Distribution Plot placing the user relative to the population.
    * Dimensional breakdown of the trait.

---

## Technologies Used

* **R Language:** Core programming language.
* **Shiny-friendly:** Web application framework.
* **mirt & mirtCAT:** Packages for Multidimensional Item Response Theory and Computerized Adaptive Testing.
* **CSS/HTML:** Custom styling for a professional "Executive Assessment" look.

---

### Author
**Álvaro González Sánchez**
*Universidad Autónoma de Madrid (UAM)*
*2026*
