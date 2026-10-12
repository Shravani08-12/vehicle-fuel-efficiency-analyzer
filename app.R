# Vehicle Fuel Efficiency Analyzer
# SDS Mini Project | R + Shiny
# Note: bundled data is simulated; see generate_data.R and README.md.

library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(DT)

DATA_PATH <- file.path("data", "vehicles.csv")
if (!file.exists(DATA_PATH)) DATA_PATH <- "vehicles.csv"
if (!file.exists(DATA_PATH)) stop("Dataset not found. Run generate_data.R and keep vehicles.csv in data/.")

vehicles <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
required_cols <- c("vehicle_id", "mileage", "engine_cc", "speed_kmph", "fuel_type", "mileage_category")
if (!all(required_cols %in% names(vehicles))) stop("Dataset is missing one or more required columns.")
vehicles <- vehicles |>
  mutate(
    fuel_type = factor(fuel_type, levels = c("Petrol", "Diesel")),
    mileage_category = factor(mileage_category, levels = c("Low", "High"))
  )

theme_app <- bs_theme(
  version = 5, bootswatch = "flatly",
  primary = "#145DA0", secondary = "#5B6B7A"
)

ui <- page_navbar(
  title = "Vehicle Fuel Efficiency Analyzer",
  theme = theme_app,
  header = tagList(
    h5("SDS mini project | Explore vehicle mileage using statistical methods"),
    p("Dataset: simulated educational data. Findings illustrate methods and are not manufacturer evidence.")
  ),
  nav_panel("Overview",
    layout_columns(
      value_box(title = "Vehicles in dataset", value = textOutput("n_vehicles", inline = TRUE), showcase = bsicons::bs_icon("car-front")),
      value_box(title = "Mean mileage", value = textOutput("mean_mileage", inline = TRUE), showcase = bsicons::bs_icon("speedometer2")),
      value_box(title = "Fuel types", value = textOutput("fuel_count", inline = TRUE), showcase = bsicons::bs_icon("fuel-pump"))
    ),
    layout_columns(
      card(card_header("Mileage distribution"), plotOutput("overview_plot", height = 320)),
      card(card_header("Dataset preview"), DTOutput("data_preview"))
    ),
    card(card_header("How to use this app"),
      p("Choose any analysis tab. Each module reports a plot, test statistic, p-value where applicable, and a plain-English interpretation."),
      p("Use the default reference value of 18 km/L for the manufacturer's claim and median test. Results are based on simulated data.")
    )
  ),
  nav_panel("P4 · Z-test",
    sidebarLayout(
      sidebarPanel(numericInput("z_mu0", "Claimed mileage (km/L)", 18, min = 0),
        helpText("One-sided test: H1 is that the population mean is greater than the claim.")),
      mainPanel(h3("Does mean mileage exceed the claim?"), plotOutput("z_plot", height = 300), verbatimTextOutput("z_results"))
    )
  ),
  nav_panel("P8 · Sign test",
    sidebarLayout(
      sidebarPanel(numericInput("sign_m0", "Reference median (km/L)", 18, min = 0),
        helpText("Two-sided sign test. Values exactly equal to the reference are omitted.")),
      mainPanel(h3("Is the population median different?"), plotOutput("sign_plot", height = 300), verbatimTextOutput("sign_results"))
    )
  ),
  nav_panel("P6 · Regression",
    sidebarLayout(
      sidebarPanel(sliderInput("engine_range", "Engine capacity range (cc)", min = min(vehicles$engine_cc), max = max(vehicles$engine_cc),
        value = range(vehicles$engine_cc), step = 50)),
      mainPanel(h3("Predict mileage from engine capacity"), plotOutput("reg_plot", height = 350), verbatimTextOutput("reg_results"))
    )
  ),
  nav_panel("P3 · 95% CI",
    sidebarLayout(
      sidebarPanel(selectInput("ci_method", "Interval method", choices = c("t interval (unknown population SD)" = "t", "z interval (known population SD)" = "z")),
        conditionalPanel("input.ci_method == 'z'", numericInput("ci_sigma", "Known population SD (km/L)", 4, min = 0.01))),
      mainPanel(h3("95% confidence interval for mean mileage"), plotOutput("ci_plot", height = 300), verbatimTextOutput("ci_results"))
    )
  ),
  nav_panel("P9 · Mann–Whitney",
    sidebarLayout(
      sidebarPanel(helpText("Compares mileage distributions for petrol and diesel vehicles.")),
      mainPanel(h3("Petrol versus diesel"), plotOutput("mw_plot", height = 320), verbatimTextOutput("mw_results"))
    )
  ),
  nav_panel("P7 · Chi-square",
    sidebarLayout(
      sidebarPanel(numericInput("category_cutoff", "High mileage if above (km/L)", 18, min = 0)),
      mainPanel(h3("Are fuel type and mileage category independent?"), plotOutput("chi_plot", height = 320), tableOutput("chi_table"), verbatimTextOutput("chi_results"))
    )
  ),
  nav_panel("Data",
    p("Download the dataset or inspect the complete table."),
    downloadButton("download_csv", "Download CSV"),
    br(), br(), DTOutput("full_data")
  )
)

server <- function(input, output, session) {
  output$n_vehicles <- renderText(nrow(vehicles))
  output$mean_mileage <- renderText(sprintf("%.2f km/L", mean(vehicles$mileage, na.rm = TRUE)))
  output$fuel_count <- renderText(paste(levels(droplevels(vehicles$fuel_type)), collapse = " / "))
  output$overview_plot <- renderPlot({
    ggplot(vehicles, aes(x = mileage, fill = fuel_type)) +
      geom_histogram(bins = 20, alpha = 0.75, position = "identity") +
      labs(x = "Mileage (km/L)", y = "Number of vehicles", fill = "Fuel type") + theme_minimal()
  })
  output$data_preview <- renderDT(datatable(head(vehicles, 8), options = list(dom = "tip", scrollX = TRUE)))
  output$full_data <- renderDT(datatable(vehicles, options = list(pageLength = 10, scrollX = TRUE)))
  output$download_csv <- downloadHandler(
    filename = function() "vehicles.csv",
    content = function(file) write.csv(vehicles, file, row.names = FALSE)
  )

  # P4: Approximate one-sample z-test using sample SD as an estimate.
  # A textbook z-test strictly assumes known population SD. State this limitation.
  output$z_plot <- renderPlot({
    ggplot(vehicles, aes(x = mileage)) + geom_histogram(bins = 22, fill = "#2C7FB8", color = "white") +
      geom_vline(xintercept = input$z_mu0, linetype = "dashed", linewidth = 1, color = "#D95F0E") +
      labs(x = "Mileage (km/L)", y = "Count", subtitle = "Dashed line = claimed mileage") + theme_minimal()
  })
  output$z_results <- renderPrint({
    x <- vehicles$mileage
    n <- length(x); xbar <- mean(x); s <- sd(x); mu0 <- input$z_mu0
    z <- (xbar - mu0) / (s / sqrt(n))
    p <- pnorm(z, lower.tail = FALSE)
    cat("H0: population mean mileage = ", mu0, "\nH1: population mean mileage > ", mu0, "\n\n", sep = "")
    cat(sprintf("Sample size: %d\nSample mean: %.3f km/L\nSample SD: %.3f\nApprox. z statistic: %.3f\nOne-sided p-value: %.6f\n\n", n, xbar, s, z, p))
    cat(if (p < 0.05) "Conclusion at 5%: Reject H0. The data provide evidence that mean mileage exceeds the claim." else "Conclusion at 5%: Fail to reject H0. The data do not provide sufficient evidence that mean mileage exceeds the claim.")
    cat("\nCaution: because population SD is not supplied, this uses sample SD as an approximation. A one-sample t-test is more appropriate when SD is unknown.")
  })

  # P8: exact two-sided sign test
  output$sign_plot <- renderPlot({
    ggplot(vehicles, aes(x = mileage)) + geom_histogram(bins = 22, fill = "#41AB5D", color = "white") +
      geom_vline(xintercept = input$sign_m0, linetype = "dashed", color = "#D95F0E") +
      labs(x = "Mileage (km/L)", y = "Count", subtitle = "Dashed line = reference median") + theme_minimal()
  })
  output$sign_results <- renderPrint({
    d <- vehicles$mileage - input$sign_m0
    d <- d[d != 0]
    n <- length(d); positives <- sum(d > 0)
    p <- if (n == 0) NA_real_ else binom.test(positives, n, p = 0.5, alternative = "two.sided")$p.value
    cat(sprintf("H0: population median = %.3f km/L\nH1: population median differs from %.3f km/L\n\n", input$sign_m0, input$sign_m0))
    cat(sprintf("Non-tied observations: %d\nPositive signs: %d\nNegative signs: %d\nExact two-sided p-value: %s\n\n", n, positives, n - positives, ifelse(is.na(p), "undefined", format.pval(p, digits = 5))))
    if (is.na(p)) cat("No non-tied observations; the test cannot be computed.")
    else cat(if (p < 0.05) "Conclusion at 5%: Reject H0; evidence suggests the median differs from the reference." else "Conclusion at 5%: Fail to reject H0; insufficient evidence that the median differs from the reference.")
  })

  # P6: linear regression
  reg_data <- reactive({
    filter(vehicles, engine_cc >= input$engine_range[1], engine_cc <= input$engine_range[2])
  })
  output$reg_plot <- renderPlot({
    ggplot(reg_data(), aes(x = engine_cc, y = mileage, color = fuel_type)) +
      geom_point(alpha = 0.7) + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#D95F0E") +
      labs(x = "Engine capacity (cc)", y = "Mileage (km/L)", color = "Fuel type") + theme_minimal()
  })
  output$reg_results <- renderPrint({
    d <- reg_data()
    validate(need(nrow(d) >= 3, "Select a wider engine capacity range to include at least three vehicles."))
    model <- lm(mileage ~ engine_cc, data = d)
    sm <- summary(model)
    cf <- coef(model)
    cat("Model: mileage = intercept + slope × engine capacity\n\n")
    cat(sprintf("Intercept: %.4f\nSlope: %.6f km/L per cc\nR-squared: %.4f\nAdjusted R-squared: %.4f\nSlope p-value: %.6g\n\n",
      cf[1], cf[2], sm$r.squared, sm$adj.r.squared, sm$coefficients["engine_cc", "Pr(>|t|)"]))
    cat(sprintf("Fitted equation: mileage = %.4f + (%.6f × engine_cc)\n\n", cf[1], cf[2]))
    cat("Interpretation: the slope estimates the average change in mileage associated with a 1 cc increase in engine capacity. This is an association, not proof of causation.")
  })

  # P3: 95% CI
  output$ci_plot <- renderPlot({
    x <- vehicles$mileage; n <- length(x); m <- mean(x)
    if (input$ci_method == "t") {
      se <- sd(x) / sqrt(n); crit <- qt(0.975, df = n - 1)
    } else {
      se <- input$ci_sigma / sqrt(n); crit <- qnorm(0.975)
    }
    lo <- m - crit * se; hi <- m + crit * se
    ggplot(data.frame(mean = m, lower = lo, upper = hi), aes(y = "Mean mileage", x = mean)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.18, linewidth = 1.2, color = "#2C7FB8") +
      geom_point(size = 4, color = "#D95F0E") + labs(x = "Mileage (km/L)", y = NULL) + theme_minimal()
  })
  output$ci_results <- renderPrint({
    x <- vehicles$mileage; n <- length(x); m <- mean(x)
    if (input$ci_method == "t") {
      se <- sd(x) / sqrt(n); crit <- qt(0.975, df = n - 1)
      method <- "Student's t interval (population SD unknown)"
    } else {
      validate(need(input$ci_sigma > 0, "Population SD must be greater than zero."))
      se <- input$ci_sigma / sqrt(n); crit <- qnorm(0.975)
      method <- "z interval (population SD supplied as known)"
    }
    lo <- m - crit * se; hi <- m + crit * se
    cat(sprintf("Method: %s\nSample size: %d\nSample mean: %.3f km/L\nStandard error: %.4f\n95%% confidence interval: [%.3f, %.3f] km/L\n\n", method, n, m, se, lo, hi))
    cat("Interpretation: using this method, the interval is an estimate of plausible values for the population mean mileage.")
    if (input$ci_method == "z") cat("\nImportant: use the z interval only when the population SD is genuinely known.")
  })

  # P9: Mann-Whitney/Wilcoxon rank-sum test
  output$mw_plot <- renderPlot({
    ggplot(vehicles, aes(x = fuel_type, y = mileage, fill = fuel_type)) +
      geom_boxplot(alpha = 0.75) + labs(x = "Fuel type", y = "Mileage (km/L)") + theme_minimal() + theme(legend.position = "none")
  })
  output$mw_results <- renderPrint({
    petrol <- vehicles$mileage[vehicles$fuel_type == "Petrol"]
    diesel <- vehicles$mileage[vehicles$fuel_type == "Diesel"]
    res <- wilcox.test(petrol, diesel, alternative = "two.sided", exact = FALSE, correct = TRUE)
    cat("H0: petrol and diesel mileage distributions are equal\nH1: their distributions differ\n\n")
    cat(sprintf("Petrol n = %d; median = %.3f km/L\nDiesel n = %d; median = %.3f km/L\nWilcoxon rank-sum W statistic: %.3f\nTwo-sided p-value: %.6g\n\n",
      length(petrol), median(petrol), length(diesel), median(diesel), unname(res$statistic), res$p.value))
    cat(if (res$p.value < 0.05) "Conclusion at 5%: Reject H0; evidence indicates a difference between the distributions." else "Conclusion at 5%: Fail to reject H0; insufficient evidence of a distributional difference.")
    cat("\nNote: the Mann–Whitney/Wilcoxon rank-sum test compares distributions; it is not inherently a test of means.")
  })

  # P7: chi-square test of independence
  chi_data <- reactive({
    vehicles |>
      mutate(category_dynamic = factor(ifelse(mileage > input$category_cutoff, "High", "Low"), levels = c("Low", "High")))
  })
  output$chi_plot <- renderPlot({
    d <- chi_data()
    ggplot(d, aes(x = fuel_type, fill = category_dynamic)) +
      geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent) +
      labs(x = "Fuel type", y = "Proportion", fill = "Mileage category") + theme_minimal()
  })
  output$chi_table <- renderTable({
    table(chi_data()$fuel_type, chi_data()$category_dynamic)
  }, rownames = TRUE)
  output$chi_results <- renderPrint({
    tab <- table(chi_data()$fuel_type, chi_data()$category_dynamic)
    res <- suppressWarnings(chisq.test(tab, correct = FALSE))
    cat("H0: fuel type and mileage category are independent\nH1: they are associated\n\n")
    cat("Observed contingency table:\n"); print(tab)
    cat(sprintf("\nChi-square statistic: %.4f\nDegrees of freedom: %d\np-value: %.6g\n\n", unname(res$statistic), unname(res$parameter), res$p.value))
    cat("Expected counts:\n"); print(round(res$expected, 2))
    if (any(res$expected < 5)) cat("\nWarning: at least one expected cell count is below 5. The chi-square approximation may be unreliable; consider Fisher's exact test if appropriate.")
    cat("\n")
    cat(if (res$p.value < 0.05) "Conclusion at 5%: Reject H0; evidence suggests fuel type and mileage category are associated." else "Conclusion at 5%: Fail to reject H0; insufficient evidence of an association.")
  })
}

shinyApp(ui = ui, server = server)
