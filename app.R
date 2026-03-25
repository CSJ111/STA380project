# Ensure these packages are installed: install.packages(c("shiny", "bslib", "dplyr", "ggplot2"))
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# --- UI Definition ---
ui <- page_sidebar(
  title = "STA380 Project: Permutation Test on ECG Age Gap (by Group GG Bond)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar: Input Controls
  sidebar = sidebar(
    numericInput("seed", "1. Random Seed", value = 123),
    selectInput("B", "2. Number of Permutations (B)", 
                choices = c(500, 1000, 5000), selected = 1000),
    sliderInput("alpha", "3. Significance Level (\u03B1)", 
                min = 0.01, max = 0.10, value = 0.05, step = 0.01),
    selectInput("data_handling", "4. Repeated Exams Handling", 
                choices = c("One exam per patient (Random)", "One exam per patient (Earliest)")),
    hr(),
    actionButton("run", "Run Permutation Test", class = "btn-primary"),
    # Moved the warning out of the main title to a subtle help text here
    helpText("Note: Computations may take a moment depending on the number of permutations (B).")
  ),
  
  # Main Panel: Tabbed Layout to solve scrolling/thinness issues
  navset_card_underline(
    
    # --- Tab 1: Project Description ---
    nav_panel("1. Project Intro & Data", 
      card(
        card_header("Project Background & Objectives"),
        # ！！！在这里加了 withMathJax() ！！！
        withMathJax(markdown("
        #### Dataset Description
        This application utilizes the publicly available **CODE-15% ECG dataset** (Zenodo record 4916206). It consists of large-scale clinical electrocardiograms (ECGs) along with corresponding metadata.
        
        From the patient metadata, we primarily focus on:
        * `AF`: A binary label indicating the presence of Atrial Fibrillation.
        * `age`: The chronological age of the patient.
        * `nn_predicted_age`: An 'ECG age' derived from a deep neural network analyzing the patient's heart signals.
        
        #### What We Are Analyzing
        We define our continuous outcome variable as the **Age Gap** (\\(g\\)):
        
        $$ g = \\text{nn\\_predicted\\_age} - \\text{age} $$
        
        A positive Age Gap suggests that the physiological ECG age appears older than the patient's actual age.
        
        #### General Goal of This Application
        The objective of this app is to perform a **two-sample Permutation Test** to evaluate whether the *entire distribution* of the Age Gap is identical between patients with Atrial Fibrillation (AF) and those without (non-AF). 
        
        Instead of merely comparing the means (which could be done with a simple t-test), we compute the **Kolmogorov-Smirnov (KS) statistic** entirely from scratch. The app randomly shuffles the group labels \\(B\\) times to construct an empirical null distribution, calculating a reliable p-value to test the exchangeability of the two groups.
        "))
      )
    ),
    
    # --- Tab 2: Main Results & Plot ---
    nav_panel("2. Permutation Results",
      card(
        card_header("Test Conclusion & Statistics"),
        uiOutput("result_text")
      ),
      card(
        card_header("Permutation Null Distribution (KS Statistic)"),
        plotOutput("perm_plot", height = "500px"),
        full_screen = TRUE
      )
    ),
    
    # --- Tab 3: Summary Statistics & Flow ---
    nav_panel("3. Group Summary Statistics",
      card(
        card_header("Observed Data Summary"),
        tableOutput("summary_table"),
        uiOutput("data_flow_text")
      ),
      card(
        card_header("Statistical Caveats & Assumptions"),
        HTML("
          <ul>
            <li><strong>Independence:</strong> We restrict the dataset to one exam per patient before permutation to satisfy the independence assumption of the test.</li>
            <li><strong>KS Statistic:</strong> We use the maximum absolute difference between the empirical cumulative distribution functions (ECDFs) of the two groups.</li>
            <li><strong>Interpretation of P-value:</strong> The p-value is a Monte Carlo estimate. It will fluctuate slightly if you change the random seed or the number of permutations (B).</li>
          </ul>
        ")
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # 1. Data Processing Pipeline
  my_data <- reactive({
    set.seed(input$seed)
    
    if (file.exists("exams.csv")) {
      df <- read.csv("exams.csv")
    } else {
      # Mock data fallback for testing
      df <- data.frame(
        patient_id = sample(1:500, 800, replace = TRUE),
        AF = sample(c(TRUE, FALSE), 800, replace = TRUE, prob = c(0.15, 0.85)),
        age = rnorm(800, mean = 60, sd = 15),
        nn_predicted_age = rnorm(800, mean = 62, sd = 16)
      )
    }
    
    raw_n <- nrow(df)
    
    df$age <- as.numeric(as.character(df$age))
    df$nn_predicted_age <- as.numeric(as.character(df$nn_predicted_age))
    
    if (is.numeric(df$AF) || all(na.omit(df$AF) %in% c(0, 1, "0", "1"))) {
      df$AF <- as.logical(as.numeric(as.character(df$AF)))
    } else {
      df$AF <- as.logical(toupper(as.character(df$AF)))
    }
    
    df$g <- df$nn_predicted_age - df$age
    df_clean <- df[!is.na(df$g) & !is.na(df$AF), ]
    removed_na <- raw_n - nrow(df_clean)
    
    if (input$data_handling == "One exam per patient (Random)") {
      df_final <- df_clean %>% group_by(patient_id) %>% slice_sample(n = 1) %>% ungroup()
    } else {
      df_final <- df_clean %>% group_by(patient_id) %>% slice(1) %>% ungroup()
    }
    removed_dups <- nrow(df_clean) - nrow(df_final)
    
    return(list(data = df_final, raw_n = raw_n, removed_na = removed_na, removed_dups = removed_dups))
  })
  
  # 2. Permutation Engine (HARDCODED KS STATISTIC)
  perm_results <- eventReactive(input$run, {
    dataset <- my_data()
    df <- dataset$data
    
    validate(
      need(nrow(df) > 0, "Error: Dataset is empty after processing."),
      need(sum(df$AF == TRUE) > 0, "Error: No AF==TRUE patients found."),
      need(sum(df$AF == FALSE) > 0, "Error: No AF==FALSE patients found.")
    )
    
    set.seed(input$seed) 
    B <- as.numeric(input$B)
    
    af_labels <- df$AF
    n_af <- sum(af_labels == TRUE)
    n_nonaf <- sum(af_labels == FALSE)
    
    # ---------------------------------------------------------
    # CUSTOM FAST KS-STATISTIC IMPLEMENTATION
    # We pre-sort the data indices ONCE to optimize the loop.
    # The actual numerical values never change, only their group labels do!
    # ---------------------------------------------------------
    sort_idx <- order(df$g)
    
    # Calculate Observed KS Statistic
    sorted_labels_obs <- af_labels[sort_idx]
    cdf_a_obs <- cumsum(sorted_labels_obs == TRUE) / n_af
    cdf_b_obs <- cumsum(sorted_labels_obs == FALSE) / n_nonaf
    obs_stat <- max(abs(cdf_a_obs - cdf_b_obs))
    
    # Permutation Loop
    perm_stats <- numeric(B)
    withProgress(message = 'Running Permutations...', value = 0, {
      for (i in 1:B) {
        # Randomly shuffle the labels
        shuffled_labels <- sample(af_labels)
        
        # Apply the pre-calculated sorting order to the new labels
        sorted_labels_sim <- shuffled_labels[sort_idx]
        
        # Compute empirical CDFs dynamically
        cdf_a_sim <- cumsum(sorted_labels_sim == TRUE) / n_af
        cdf_b_sim <- cumsum(sorted_labels_sim == FALSE) / n_nonaf
        
        # Calculate max distance (KS Statistic)
        perm_stats[i] <- max(abs(cdf_a_sim - cdf_b_sim))
        
        if (i %% 100 == 0) incProgress(100/B) 
      }
    })
    
    p_val <- (1 + sum(abs(perm_stats) >= abs(obs_stat), na.rm=TRUE)) / (B + 1)
    
    return(list(obs = obs_stat, perms = perm_stats, p_val = p_val, data = df, B = B, dataset_meta = dataset))
  })
  
  # --- Outputs ---
  
  output$result_text <- renderUI({
    if (input$run == 0) return(HTML("<p style='color: gray; font-style: italic;'>Please click 'Run Permutation Test' in the sidebar to view the results.</p>"))
    
    res <- perm_results()
    p_display <- ifelse(res$p_val < (1/res$B), paste0("< ", 1/res$B), round(res$p_val, 4))
    
    decision <- if(res$p_val < input$alpha) {
      tags$span("REJECT the null hypothesis", style="color:red; font-weight:bold;")
    } else {
      tags$span("FAIL TO REJECT the null hypothesis", style="color:green; font-weight:bold;")
    }
    
    HTML(paste0(
      "<h4>Estimated Permutation P-value: <strong>", p_display, "</strong></h4>",
      "<p style='font-size: 0.9em; color: gray;'><em>Monte Carlo estimate based on B = ", res$B, " permutations.</em></p>",
      "<h4>Conclusion: ", as.character(decision), "</h4>",
      "<hr>",
      "<p><strong>Observed KS Statistic:</strong> ", round(res$obs, 4), "</p>",
      "<p style='font-size: 0.9em;'><em>Note: The KS statistic measures the maximum distance between the empirical cumulative distribution functions (ECDF) of the two groups.</em></p>"
    ))
  })
  
  output$perm_plot <- renderPlot({
    # req("Permutation Plot" %in% input$display)
    if (input$run == 0) return(NULL)
    res <- perm_results()
    
    ggplot(data.frame(x = res$perms), aes(x = x)) +
      geom_histogram(bins = 30, fill = "#3498db", color = "black", alpha = 0.7) +
      geom_vline(xintercept = res$obs, color = "#e74c3c", linetype = "dashed", linewidth = 1.2) +
      theme_minimal() +
      labs(title = "Permutation Distribution of the KS Statistic",
           subtitle = "Null hypothesis: The Age Gap distributions are identical (exchangeable)",
           x = "KS Statistic under H0",
           y = "Frequency")
  })
  
  output$summary_table <- renderTable({
    # req("Summary Table" %in% input$display)
    if (input$run == 0) return(NULL)
    df <- perm_results()$data
    
    df %>%
      group_by(AF) %>%
      summarise(
        N = n(),
        Mean_Gap = mean(g, na.rm=TRUE),
        Median_Gap = median(g, na.rm=TRUE),
        SD_Gap = sd(g, na.rm=TRUE),
        IQR_Gap = IQR(g, na.rm=TRUE) 
      ) %>%
      rename(`Atrial Fibrillation (AF)` = AF)
  })
  
  output$data_flow_text <- renderUI({
    # req("Summary Table" %in% input$display)
    if (input$run == 0) return(NULL)
    meta <- perm_results()$dataset_meta
    HTML(paste0(
      "<div style='font-size: 0.85em; color: #555; background-color: #f8f9fa; padding: 10px; border-radius: 5px;'>",
      "<strong>Data Flow:</strong> Started with ", meta$raw_n, " records. ",
      "Removed ", meta$removed_na, " rows due to missing values. ",
      "Removed ", meta$removed_dups, " repeated exams to preserve patient independence. ",
      "Final sample size: ", nrow(meta$data), " unique patients.",
      "</div>"
    ))
  })
}

shinyApp(ui, server)