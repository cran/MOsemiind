ui <- shiny::fluidPage(
  shiny::titlePanel("Interactive Copula with Expanded Marginal Distributions"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h4("Marginal Distribution Selection and Parameters"),

      # Inputs for V1
      shiny::selectInput("dist_v1", "Distribution for V1:",
                         choices = c("Exponential", "Weibull", "Gamma", "Pareto", "Log-Normal")),
      shiny::uiOutput("v1_params"),

      # Inputs for V2
      shiny:: selectInput("dist_v2", "Distribution for V2:",
                          choices = c("Exponential", "Weibull", "Gamma", "Pareto", "Log-Normal")),
      shiny::uiOutput("v2_params"),

      # Inputs for V3
      shiny::selectInput("dist_v3", "Distribution for V3:",
                         choices = c("Exponential", "Weibull", "Gamma", "Pareto", "Log-Normal")),
      shiny::uiOutput("v3_params"),

      shiny::h4("Copula Parameters"),
      shiny::selectInput("copula_type", "Copula Type:", choices = c("Clayton", "Gumbel")),
      shiny:: numericInput("theta", "Theta (Dependence Parameter):", value = 0, min = 0, step = 0.1),

      shiny::actionButton("update", "Show results")
    ),
    shiny:: mainPanel(
      shiny::plotOutput("perspPlot"),
      shiny::plotOutput("contourPlot"),
      shiny::h4("Dependence Measures"),
      shiny::verbatimTextOutput("dependenceMeasures")
    )

  )
)

server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()  # Stops the Shiny app
  })
  # ReactiveValues za cuvanje stanja i kontrolu generisanja grafikona
  currentValues <- shiny::reactiveValues(
    dist_v1 = NULL,
    dist_v2 = NULL,
    dist_v3 = NULL,
    params = list(),
    theta = NULL,
    copula_type = NULL,
    renderPlots = FALSE # Indikator za crtanje grafikona
  )


  # Dinamicko generisanje UI za V1
  output$v1_params <-shiny:: renderUI({
    currentValues$renderPlots <- FALSE # Resetovanje grafikona kada se promeni raspodela
    switch(input$dist_v1,
           "Exponential" = numericInput("lambda1", "Rate (lambda) for V1:", value = 0, min = 0, step = 0.1),
           "Weibull" = tagList(
             numericInput("weibull_shape1", "Shape (k) for V1:", value = 0, min = 0, step = 0.1),
             numericInput("weibull_scale1", "Scale (lambda) for V1:", value = 0, min = 0, step = 0.1)
           ),
           "Gamma" = tagList(
             numericInput("gamma_shape1", "Shape (alpha) for V1:", value = 0, min = 0, step = 0.1),
             numericInput("gamma_rate1", "Rate (beta) for V1:", value = 0, min = 0, step = 0.1)
           ),
           "Pareto" = tagList(
             numericInput("pareto_shape1", "Shape (alpha) for V1:", value = 0, min = 0, step = 0.1),
             numericInput("pareto_scale1", "Scale (sigma) for V1:", value = 0, min = 0, step = 0.1)
           ),
           "Log-Normal" = tagList(
             numericInput("lnorm_meanlog1", "Mean (log-scale) for V1:", value = 0, step = 0.1),
             numericInput("lnorm_sdlog1", "SD (log-scale) for V1:", value = 0, min = 0, step = 0.1)
           ))
  })

  # Dinamicko generisanje UI za V2
  output$v2_params <- shiny:: renderUI({
    currentValues$renderPlots <- FALSE # Resetovanje grafikona kada se promeni raspodela
    switch(input$dist_v2,
           "Exponential" = numericInput("lambda2", "Rate (lambda) for V2:", value = 0, min = 0, step = 0.1),
           "Weibull" = tagList(
             numericInput("weibull_shape2", "Shape (k) for V2:", value = 0, min = 0, step = 0.1),
             numericInput("weibull_scale2", "Scale (lambda) for V2:", value = 0, min = 0, step = 0.1)
           ),
           "Gamma" = tagList(
             numericInput("gamma_shape2", "Shape (alpha) for V2:", value = 0, min = 0, step = 0.1),
             numericInput("gamma_rate2", "Rate (beta) for V2:", value = 0, min = 0, step = 0.1)
           ),
           "Pareto" = tagList(
             numericInput("pareto_shape2", "Shape (alpha) for V2:", value = 0, min = 0, step = 0.1),
             numericInput("pareto_scale2", "Scale (sigma) for V2:", value = 0, min = 0, step = 0.1)
           ),
           "Log-Normal" = tagList(
             numericInput("lnorm_meanlog2", "Mean (log-scale) for V2:", value = 0, step = 0.1),
             numericInput("lnorm_sdlog2", "SD (log-scale) for V2:", value = 0, min = 0, step = 0.1)
           ))
  })

  # Dinamicko generisanje UI za V3
  output$v3_params <- shiny::renderUI({
    currentValues$renderPlots <- FALSE # Resetovanje grafikona kada se promeni raspodela
    switch(input$dist_v3,
           "Exponential" = numericInput("lambda3", "Rate (lambda) for V3:", value = 0, min = 0, step = 0.1),
           "Weibull" = tagList(
             numericInput("weibull_shape3", "Shape (k) for V3:", value = 0, min = 0, step = 0.1),
             numericInput("weibull_scale3", "Scale (lambda) for V3:", value = 0, min = 0, step = 0.1)
           ),
           "Gamma" = tagList(
             numericInput("gamma_shape3", "Shape (alpha) for V3:", value = 0, min = 0, step = 0.1),
             numericInput("gamma_rate3", "Rate (beta) for V3:", value = 0, min = 0, step = 0.1)
           ),
           "Pareto" = tagList(
             numericInput("pareto_shape3", "Shape (alpha) for V3:", value = 0, min = 0, step = 0.1),
             numericInput("pareto_scale3", "Scale (sigma) for V3:", value = 0, min = 0, step = 0.1)
           ),
           "Log-Normal" = tagList(
             numericInput("lnorm_meanlog3", "Mean (log-scale) for V3:", value = 0, step = 0.1),
             numericInput("lnorm_sdlog3", "SD (log-scale) for V3:", value = 0, min = 0, step = 0.1)
           ))
  })

  # Azuriranje vrednosti nakon klika na dugme
  shiny::observeEvent(input$update, {
    currentValues$dist_v1 <- input$dist_v1
    currentValues$dist_v2 <- input$dist_v2
    currentValues$dist_v3 <- input$dist_v3
    currentValues$theta <- input$theta
    currentValues$copula_type <- input$copula_type

    # Validacija parametara
    shiny:: validate(
      need(input$theta > 0, "Theta mora biti veca od 0"),
      need(!is.null(input$dist_v1), "Odaberite raspodelu za V1"),
      need(!is.null(input$dist_v2), "Odaberite raspodelu za V2"),
      need(!is.null(input$dist_v3), "Odaberite raspodelu za V3")
    )

    # Postavi indikator na TRUE da omoguci crtanje grafikona
    currentValues$renderPlots <- TRUE
  })

  computeMarginalCDFs <- shiny::reactive({
    list(
      F_V1 = switch(input$dist_v1,
                    "Exponential" = function(t) { 1 - exp(-as.numeric(input$lambda1) * t) },
                    "Weibull" = function(t) { 1 - exp(-(t / as.numeric(input$weibull_scale1))^as.numeric(input$weibull_shape1)) },
                    "Gamma" = function(t) { stats::pgamma(t, shape = as.numeric(input$gamma_shape1), rate = as.numeric(input$gamma_rate1)) },
                    "Pareto" = function(t) { 1 - (as.numeric(input$pareto_scale1) / (as.numeric(input$pareto_scale1) + t))^as.numeric(input$pareto_shape1) },
                    "Log-Normal" = function(t) {stats:: plnorm(t, meanlog = as.numeric(input$lnorm_meanlog1), sdlog = as.numeric(input$lnorm_sdlog1)) }),
      F_V2 = switch(input$dist_v2,
                    "Exponential" = function(t) { 1 - exp(-as.numeric(input$lambda2) * t) },
                    "Weibull" = function(t) { 1 - exp(-(t / as.numeric(input$weibull_scale2))^as.numeric(input$weibull_shape2)) },
                    "Gamma" = function(t) { stats::pgamma(t, shape = as.numeric(input$gamma_shape2), rate = as.numeric(input$gamma_rate2)) },
                    "Pareto" = function(t) { 1 - (as.numeric(input$pareto_scale2) / (as.numeric(input$pareto_scale2) + t))^as.numeric(input$pareto_shape2) },
                    "Log-Normal" = function(t) { stats::plnorm(t, meanlog = as.numeric(input$lnorm_meanlog2), sdlog = as.numeric(input$lnorm_sdlog2)) }),
      F_V3 = switch(input$dist_v3,
                    "Exponential" = function(t) { 1 - exp(-as.numeric(input$lambda3) * t) },
                    "Weibull" = function(t) { 1 - exp(-(t / as.numeric(input$weibull_scale3))^as.numeric(input$weibull_shape3)) },
                    "Gamma" = function(t) { stats::pgamma(t, shape = as.numeric(input$gamma_shape3), rate = as.numeric(input$gamma_rate3)) },
                    "Pareto" = function(t) { 1 - (as.numeric(input$pareto_scale3) / (as.numeric(input$pareto_scale3) + t))^as.numeric(input$pareto_shape3) },
                    "Log-Normal" = function(t) { stats::plnorm(t, meanlog = as.numeric(input$lnorm_meanlog3), sdlog = as.numeric(input$lnorm_sdlog3)) })
    )
  })


  output$copula_result <- shiny::renderPrint({
    shiny::req(input$copula_type, input$theta)  # Ensure inputs are not NULL

    copula_type <- input$copula_type
    theta_value <- input$theta

    computeCopula(copula_type, theta_value)
  })

  computeKMatrix <- shiny::reactive({
    shiny::req(input$copula_type, input$theta)  # Ensure inputs exist

    marginals <- computeMarginalCDFs()
    copula1 <- computeCopula(input$copula_type, input$theta)
    # Define the survival copula of copula1
    survival_copula1 <- function(u, v) {
      # Flip the inputs
      return(u+v-1+copula1(1 - u, 1 - v,1))
    }


    F_V1 <- marginals$F_V1
    F_V2 <- marginals$F_V2
    F_V3 <- marginals$F_V3
    survF1 <- function(t) {

      (1- F_V1(t))*(1-F_V3(t))

    }

    # Funkcija za CDF od X2: F2(t)
    survF2 <- function(t) {
      (1- F_V2(t))*(1-F_V3(t))

    }



    inverse_survF1 <- function(y, lower = 0, upper = 50, step_size = 10, max_iterations = 20) {
      if (y < 0 || y > 1) stop("Error: y must be between 0 and 1.")

      f_lower <- survF1(lower) - y
      f_upper <- survF1(upper) - y
      iteration <- 0

      # Automatically adjust bounds if necessary
      while (f_lower * f_upper > 0 && iteration < max_iterations) {
        iteration <- iteration + 1
        lower <- lower - step_size
        upper <- upper + step_size

        # Recalculate function values at new bounds
        f_lower <- survF1(lower) - y
        f_upper <- survF1(upper) - y

        # Check if the function crosses zero within the expanded bounds
        if (f_lower * f_upper <= 0) break
      }

      if (f_lower * f_upper > 0) {
        stop("survF1 function does not cross y in the adjusted interval. Please check survF1 or extend bounds further.")
      }

      # Perform uniroot to find the root within the valid bounds
      tryCatch({
        root <- stats::uniroot(function(t) survF1(t) - y, lower = lower, upper = upper)
        return(root$root)
      }, error = function(e) {
        stop("Error in inverse_survF1: ", e$message)
      })
    }



    inverse_survF2 <- function(y, lower = 0, upper = 50, step_size = 10, max_iterations = 20) {
      if (y < 0 || y > 1) stop("Error: y must be between 0 and 1.")

      f_lower <- survF2(lower) - y
      f_upper <- survF2(upper) - y
      iteration <- 0

      # Automatically adjust bounds if necessary
      while (f_lower * f_upper > 0 && iteration < max_iterations) {
        iteration <- iteration + 1
        lower <- lower - step_size
        upper <- upper + step_size

        # Recalculate function values at new bounds
        f_lower <- survF2(lower) - y
        f_upper <- survF2(upper) - y

        # Check if the function crosses zero within the expanded bounds
        if (f_lower * f_upper <= 0) break
      }

      if (f_lower * f_upper > 0) {
        stop("survF1 function does not cross y in the adjusted interval. Please check survF1 or extend bounds further.")
      }

      # Perform uniroot to find the root within the valid bounds
      tryCatch({
        root <-stats:: uniroot(function(t) survF2(t) - y, lower = lower, upper = upper)
        return(root$root)
      }, error = function(e) {
        stop("Error in inverse_survF1: ", e$message)
      })
    }


    Surv_C_v1_v2 <- function(v1, v2) {
      u1 <- 1 - v1
      u2 <- 1 - v2
      C_val <- copula::pCopula(cbind(u1, u2, 1),copula1)
      surv_val <- v1 + v2 - 1 + C_val
      return(surv_val)
    }
    Surv_F_V3 <- function(t) { 1 - F_V3(t) }
    K1 <- function(v1, v2) {
      t1 <- inverse_survF1(v1)
      t2 <- inverse_survF2(v2)
      t_max <- max(t1, t2)
      surv_v1 <- 1 - F_V1(t1)
      surv_v2 <- 1 - F_V2(t2)
      Surv_C_v1_v2(surv_v1, surv_v2)*Surv_F_V3(t_max)


    }

    K <- function(u, v) {
      if (u ==0 | v==0){ return(0)}
      K_hat <-K1(1-u, 1-v)
      u + v - 1 + K_hat
    }
    u_values <- seq(0.1, 0.9, by = 0.1)
    v_values <- seq(0.1, 0.9, by = 0.1)

    K_function1 <- Vectorize(function(u, v) {
      K(u,v)
    })
    K_matrix<- outer(u_values, v_values, K_function1)
    return(K_matrix)
  })

  computeDependenceMeasures <- shiny:: reactive({
    K_matrix <- computeKMatrix()  # Compute K-matrix from copula
    u_values <- seq(0.1, 0.9, by = 0.1)
    v_values <- seq(0.1, 0.9, by = 0.1)

    #  Convert K_matrix into an interpolated function
    convertKMatrixToFunction <- function(K_matrix, u_values, v_values) {
      return(function(u, v) {
        # Ensure (u, v) values are within bounds
        u <- pmin(pmax(u, min(u_values)), max(u_values))
        v <- pmin(pmax(v, min(v_values)), max(v_values))

        # Perform bilinear interpolation
        pracma::interp2(u_values, v_values, K_matrix, u, v)
      })
    }

    # Convert matrix to function
    K_function <- convertKMatrixToFunction(K_matrix, u_values, v_values)

    computeSpearmanRho <- function(K_function) {
      integral_result <- pracma::integral2(
        function(u, v) K_function(u,v),
        xmin = 0, xmax = 1,
        ymin = 0, ymax = 1
      )$Q  # Extract numerical integration result

      return(12 * integral_result -3)  # Spearman's Rho formula
    }

    computeKendallTau <- function(K_function) {
      # Generate random samples
      u_samples <- stats::runif(1e5)
      v_samples <- stats::runif(1e5)

      # Evaluate interpolated K-values at sampled points
      K_values <- K_function(u_samples, v_samples)

      # Compute Kendall's Tau
      tau <- 4 * mean(K_values) - 1
      return(tau)
    }
    # Calculate both measures
    spearman_rho <- computeSpearmanRho(K_function)
    kendall_tau <- computeKendallTau(K_function)

    # Return as a list
    return(list(kendall = kendall_tau, spearman = spearman_rho))
  })

  output$dependenceMeasures <- shiny::renderPrint({
    shiny:: req(currentValues$renderPlots)
    measures <- computeDependenceMeasures()
    cat("Kendall's Tau:", measures$kendall, "\n")
    cat("Spearman's Rho:", measures$spearman, "\n")
  })


  # Prikaz grafikona
  output$perspPlot <- shiny::renderPlot({
    shiny::req(currentValues$renderPlots)
    K_matrix <- computeKMatrix()


    graphics::persp(
      seq(0.1, 0.9, by = 0.1), seq(0.1, 0.9, by = 0.1), K_matrix,
      main = "Perspective Plot of K(u, v)",
      xlab = "u", ylab = "v", zlab = "K(u, v)",
      theta = 30, phi = 20, col = "lightblue"
    )
  })

  output$contourPlot <-shiny:: renderPlot({
    shiny::req(currentValues$renderPlots)
    K_matrix <- computeKMatrix()


    graphics:: contour(
      seq(0.1, 0.9, by = 0.1), seq(0.1, 0.9, by = 0.1), K_matrix,
      nlevels = 15, col = "blue",
      main = "Contour Plot of K(u, v)",
      xlab = "u", ylab = "v"
    )
  })

  # Aktivacija grafikona nakon klika na dugme
  shiny::observeEvent(input$update, {
    currentValues$renderPlots <- TRUE
  })
}

shiny::shinyApp(ui = ui, server = server)
