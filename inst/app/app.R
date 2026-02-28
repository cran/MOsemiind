# app.R


# ============================================================
# 1) Marginals: surv + quant + rng
# ============================================================

make_marginal <- function(dist, params = list()) {
  dist <- tolower(dist)
  need <- function(name) {
    if (is.null(params[[name]])) stop(sprintf("Missing parameter '%s' for %s.", name, dist), call. = FALSE)
    params[[name]]
  }

  if (dist %in% c("exponential","exp")) {
    rate <- need("rate")
    list(
      dist="exponential", params=list(rate=rate),
      surv=function(t) exp(-rate * pmax(t,0)),
      quant=function(p) qexp(p, rate=rate),
      rng=function(n) rexp(n, rate=rate)
    )

  } else if (dist %in% c("weibull","wei")) {
    shape <- need("shape"); scale <- need("scale")
    list(
      dist="weibull", params=list(shape=shape, scale=scale),
      surv=function(t) exp(- (pmax(t,0)/scale)^shape),
      quant=function(p) qweibull(p, shape=shape, scale=scale),
      rng=function(n) rweibull(n, shape=shape, scale=scale)
    )

  } else if (dist %in% c("gamma","gam")) {
    shape <- need("shape"); rate <- need("rate")
    list(
      dist="gamma", params=list(shape=shape, rate=rate),
      surv=function(t) 1 - pgamma(t, shape=shape, rate=rate),
      quant=function(p) qgamma(p, shape=shape, rate=rate),
      rng=function(n) rgamma(n, shape=shape, rate=rate)
    )

  } else if (dist %in% c("pareto")) {
    xm <- need("xm"); alpha <- need("alpha")
    list(
      dist="pareto", params=list(xm=xm, alpha=alpha),
      surv=function(t){
        t <- as.numeric(t)
        out <- rep(1, length(t))
        idx <- t >= xm
        out[idx] <- (xm / t[idx])^alpha
        out[t < xm] <- 1
        out
      },
      quant=function(p){
        p <- as.numeric(p)
        if (any(p <= 0 | p >= 1)) stop("Pareto quantile requires p in (0,1).", call. = FALSE)
        xm / (1 - p)^(1/alpha)
      },
      rng=function(n){
        u <- runif(n); xm / (1 - u)^(1/alpha)
      }
    )

  } else if (dist %in% c("lognormal","log-normal","lnorm")) {
    meanlog <- need("meanlog"); sdlog <- need("sdlog")
    list(
      dist="lognormal", params=list(meanlog=meanlog, sdlog=sdlog),
      surv=function(t) 1 - plnorm(t, meanlog=meanlog, sdlog=sdlog),
      quant=function(p) qlnorm(p, meanlog=meanlog, sdlog=sdlog),
      rng=function(n) rlnorm(n, meanlog=meanlog, sdlog=sdlog)
    )

  } else stop("Unknown distribution.", call. = FALSE)
}

# ============================================================
# 2) Copula for (V1,V2): need survival copula C-hat_12
#    C-hat(u,v) = u + v - 1 + C(1-u, 1-v)
# ============================================================

make_copula12 <- function(family = c("clayton","gumbel"), theta) {
  family <- match.arg(tolower(family), c("clayton","gumbel"))
  clip01 <- function(x) pmin(pmax(x, 1e-12), 1 - 1e-12)

  if (family == "clayton") {
    if (theta <= 0) stop("Clayton requires theta > 0.", call. = FALSE)
    C <- function(u,v){
      u <- clip01(u); v <- clip01(v)
      A <- u^(-theta) + v^(-theta) - 1
      A^(-1/theta)
    }
  } else { # gumbel
    if (theta < 1) stop("Gumbel requires theta >= 1.", call. = FALSE)
    C <- function(u,v){
      u <- clip01(u); v <- clip01(v)
      a <- (-log(u))^theta + (-log(v))^theta
      exp(-a^(1/theta))
    }
  }

  Chat <- function(u,v) {
    u <- pmin(pmax(u, 0), 1)
    v <- pmin(pmax(v, 0), 1)
    u + v - 1 + C(1-u, 1-v)
  }

  list(family=family, theta=theta, C=C, Chat=Chat)
}

# ============================================================
# 3) Model object
# ============================================================

make_model <- function(dist_v1, par_v1, dist_v2, par_v2, dist_v3, par_v3,
                       copula_family, theta) {
  V1 <- make_marginal(dist_v1, par_v1)
  V2 <- make_marginal(dist_v2, par_v2)
  V3 <- make_marginal(dist_v3, par_v3)
  cop <- make_copula12(copula_family, theta)
  list(V1=V1, V2=V2, V3=V3, copula12=cop)
}

# ============================================================
# 4) Survival quantile inversion: S^{-1}(u) via uniroot
#    Needed for SFinv_{X1}, SFinv_{X2}
# ============================================================

qsurv_numeric <- function(u, surv_fun, lower = 0, upper_init = 1, max_upper = 1e10) {
  u <- as.numeric(u)
  out <- numeric(length(u))

  for (i in seq_along(u)) {
    ui <- u[i]
    if (ui >= 1) { out[i] <- lower; next }
    if (ui <= 0) { out[i] <- Inf; next }

    f <- function(t) surv_fun(t) - ui

    upper <- upper_init
    while (upper < max_upper && f(upper) > 0) upper <- upper * 2
    if (upper >= max_upper) stop("Failed to bracket S^{-1}(u). Try different parameters.", call. = FALSE)

    out[i] <- uniroot(f, lower=lower, upper=upper)$root
  }
  out
}

# S_X1(t) = S_V1(t) * S_V3(t), S_X2(t) = S_V2(t) * S_V3(t)
qSX1 <- function(u, model) qsurv_numeric(u, function(t) model$V1$surv(t) * model$V3$surv(t))
qSX2 <- function(u, model) qsurv_numeric(u, function(t) model$V2$surv(t) * model$V3$surv(t))

# ============================================================
# 5) Manuscript formula for survival copula K-hat of (X1,X2)
#   Khat(u,v) = Chat_12( S_V1(SX1^{-1}(u)), S_V2(SX2^{-1}(v)) ) * S_V3(max(...))
# and K(u,v) = u+v-1 + Khat(1-u,1-v)
# ============================================================

Khat_surface <- function(grid_u, grid_v, model) {
  # precompute t1(u), t2(v)
  t1 <- qSX1(grid_u, model)
  t2 <- qSX2(grid_v, model)

  # compute S_V1(t1) and S_V2(t2) as vectors
  s1 <- model$V1$surv(t1)  # length m
  s2 <- model$V2$surv(t2)  # length m

  # Chat on grid as matrix using outer
  Chat_mat <- outer(s1, s2, model$copula12$Chat)  # m x m

  # S_V3(max(t1,t2)) matrix via outer+ pmax
  max_mat <- outer(t1, t2, pmax)
  s3_mat  <- model$V3$surv(max_mat)

  Chat_mat * s3_mat
}

K_surface <- function(grid, model) {
  # K(u,v) = u+v-1 + Khat(1-u, 1-v)
  u_rev <- 1 - grid
  v_rev <- 1 - grid
  Khat_rev <- Khat_surface(u_rev, v_rev, model)

  # build u+v-1 matrix
  uv_mat <- outer(grid, grid, function(u,v) u + v - 1)

  uv_mat + Khat_rev
}

# ============================================================
# 6) (Optional) simulate X for dependence measures only
# ============================================================

rposstable <- function(n, alpha) {
  if (alpha <= 0 || alpha > 1) stop("alpha must be in (0,1].", call. = FALSE)
  if (alpha == 1) return(rep(1, n))
  U <- runif(n, 0, pi)
  W <- rexp(n, 1)
  part1 <- sin(alpha * U) / (sin(U))^alpha
  part2 <- (sin((1 - alpha) * U) / W)^((1 - alpha))
  (part1 * part2)^(1 / alpha)
}

rCopula12 <- function(n, family = c("clayton","gumbel"), theta) {
  family <- match.arg(tolower(family), c("clayton","gumbel"))

  if (family == "clayton") {
    W  <- rgamma(n, shape = 1/theta, rate = 1)
    E1 <- rexp(n, 1); E2 <- rexp(n, 1)
    U1 <- (1 + E1 / W)^(-1/theta)
    U2 <- (1 + E2 / W)^(-1/theta)
    cbind(U1, U2)
  } else {
    alpha <- 1/theta
    S <- rposstable(n, alpha)
    E1 <- rexp(n, 1); E2 <- rexp(n, 1)
    U1 <- exp(-(E1 / S)^alpha)
    U2 <- exp(-(E2 / S)^alpha)
    cbind(U1, U2)
  }
}

simulate_X <- function(n, model) {
  U12 <- rCopula12(n, family=model$copula12$family, theta=model$copula12$theta)
  V1 <- model$V1$quant(U12[,1])
  V2 <- model$V2$quant(U12[,2])
  V3 <- model$V3$rng(n)
  cbind(X1=pmin(V1,V3), X2=pmin(V2,V3))
}

# ============================================================
# 7) UI helpers for parameters
# ============================================================

dist_choices <- c("Exponential","Weibull","Gamma","Pareto","Log-Normal")

param_ui <- function(prefix, dist) {
  dist <- tolower(dist)
  ns <- function(id) paste0(prefix, "_", id)

  if (dist %in% c("exponential","exp")) {
    tagList(numericInput(ns("rate"), "rate", value=1, min=1e-6, step=0.1))
  } else if (dist %in% c("weibull","wei")) {
    tagList(
      numericInput(ns("shape"), "shape", value=1.5, min=1e-6, step=0.1),
      numericInput(ns("scale"), "scale", value=1,   min=1e-6, step=0.1)
    )
  } else if (dist %in% c("gamma","gam")) {
    tagList(
      numericInput(ns("shape"), "shape", value=2, min=1e-6, step=0.1),
      numericInput(ns("rate"),  "rate",  value=1, min=1e-6, step=0.1)
    )
  } else if (dist %in% c("pareto")) {
    tagList(
      numericInput(ns("xm"), "xm", value=1, min=1e-6, step=0.1),
      numericInput(ns("alpha"), "alpha", value=2, min=1e-6, step=0.1)
    )
  } else { # lognormal
    tagList(
      numericInput(ns("meanlog"), "meanlog", value=0, step=0.1),
      numericInput(ns("sdlog"),   "sdlog", value=0.5, min=1e-6, step=0.05)
    )
  }
}

get_params <- function(input, prefix, dist) {
  dist <- tolower(dist)
  g <- function(id) input[[paste0(prefix, "_", id)]]

  if (dist %in% c("exponential","exp")) list(rate=g("rate"))
  else if (dist %in% c("weibull","wei")) list(shape=g("shape"), scale=g("scale"))
  else if (dist %in% c("gamma","gam")) list(shape=g("shape"), rate=g("rate"))
  else if (dist %in% c("pareto")) list(xm=g("xm"), alpha=g("alpha"))
  else list(meanlog=g("meanlog"), sdlog=g("sdlog"))
}

# ============================================================
# 8) Shiny app
# ============================================================

ui <- fluidPage(
  titlePanel("Copula K(u,v) of (X1,X2) using manuscript formulas"),
  sidebarLayout(
    sidebarPanel(
      h4("Marginals for V1, V2, V3"),
      selectInput("dist_v1", "Distribution for V1:", choices=dist_choices, selected="Exponential"),
      uiOutput("v1_params"),
      selectInput("dist_v2", "Distribution for V2:", choices=dist_choices, selected="Exponential"),
      uiOutput("v2_params"),
      selectInput("dist_v3", "Distribution for V3:", choices=dist_choices, selected="Exponential"),
      uiOutput("v3_params"),

      hr(),
      h4("Copula for (V1,V2)"),
      selectInput("copula_type", "Copula Type:", choices=c("Clayton","Gumbel")),
      numericInput("theta", "Theta:", value=1, min=0.001, step=0.1),

      hr(),
      h4("Grid / speed"),
      sliderInput("grid_n", "Grid size:", min=15, max=80, value=35, step=5),

      hr(),
      h4("Dependence measures (optional, via simulation)"),
      sliderInput("n_dep", "Simulation size:", min=1000, max=20000, value=5000, step=1000),

      hr(),
      h4("Sensitivity analysis (formula-based)"),
      checkboxInput("do_sensitivity", "Perform sensitivity analysis on θ", value = FALSE),

      conditionalPanel(
        condition = "input.do_sensitivity == true",
        sliderInput("theta_range", "Range for θ:",
                    min = 0.1, max = 10, value = c(0.5, 3), step = 0.1),
        numericInput("theta_step", "Step for θ:", value = 0.5, min = 0.05, step = 0.05),
        sliderInput("grid_sens_n", "Grid size for sensitivity:", min = 11, max = 41, value = 21, step = 2),
        actionButton("run_sens", "Run Sensitivity Analysis")
      ),

      actionButton("update", "Show results")
    ),

    mainPanel(
      plotOutput("perspPlot", height=350),
      plotOutput("contourPlot", height=350),
      h4("Dependence measures"),
      verbatimTextOutput("dependenceMeasures"),
      conditionalPanel(
        condition = "input.do_sensitivity == true",
        h4("Sensitivity results"),
        tableOutput("sensitivityTable"),
        plotOutput("sensitivityPlot", height = 300)
      )

    )
  )
)
summarize_K_surface <- function(grid, Kmat) {
  # Independence copula matrix uv
  uv <- outer(grid, grid, function(u,v) u*v)
  list(
    K_05_05 = Kmat[which.min(abs(grid - 0.5)), which.min(abs(grid - 0.5))],
    mean_abs_dev = mean(abs(Kmat - uv))
  )
}

server <- function(input, output, session) {

  output$v1_params <- renderUI(param_ui("v1", input$dist_v1))
  output$v2_params <- renderUI(param_ui("v2", input$dist_v2))
  output$v3_params <- renderUI(param_ui("v3", input$dist_v3))

  observeEvent(input$copula_type, {
    if (tolower(input$copula_type) == "gumbel") {
      updateNumericInput(session, "theta", min=1, value=max(1, input$theta))
    } else {
      updateNumericInput(session, "theta", min=0.001, value=max(0.001, input$theta))
    }
  }, ignoreInit = TRUE)

  res <- eventReactive(input$update, {
    p1 <- get_params(input, "v1", input$dist_v1)
    p2 <- get_params(input, "v2", input$dist_v2)
    p3 <- get_params(input, "v3", input$dist_v3)

    copfam <- tolower(input$copula_type)
    theta  <- input$theta
    if (copfam=="clayton" && theta <= 0) stop("Clayton requires theta>0.", call.=FALSE)
    if (copfam=="gumbel"  && theta <  1) stop("Gumbel requires theta>=1.", call.=FALSE)

    model <- make_model(input$dist_v1, p1, input$dist_v2, p2, input$dist_v3, p3,
                        copula_family=copfam, theta=theta)

    grid <- seq(0.01, 0.99, length.out=input$grid_n)

    # Compute K(u,v) using manuscript equations
    Kmat <- K_surface(grid, model)

    # Dependence measures (quick, via simulation of X -> pseudo-observations)
    X <- simulate_X(input$n_dep, model)

    # Pseudo-observations U=F1(X1), V=F2(X2); F1(t)=1-SV1(t)SV3(t), similarly F2
    F1 <- function(t) 1 - model$V1$surv(t) * model$V3$surv(t)
    F2 <- function(t) 1 - model$V2$surv(t) * model$V3$surv(t)
    U <- pmin(pmax(F1(X[,"X1"]), 0), 1)
    V <- pmin(pmax(F2(X[,"X2"]), 0), 1)

    tau <- suppressWarnings(cor(U, V, method="kendall"))
    rho <- suppressWarnings(cor(U, V, method="spearman"))

    list(model=model, grid=grid, Kmat=Kmat, tau=tau, rho=rho)
  })

  output$perspPlot <- renderPlot({
    r <- res()
    persp(r$grid, r$grid, r$Kmat,
          theta=30, phi=25, expand=0.65,
          col="lightblue",
          xlab="u", ylab="v", zlab="K(u,v)",
          main="Copula surface K(u,v) (formula-based)")
  })

  output$contourPlot <- renderPlot({
    r <- res()
    contour(r$grid, r$grid, r$Kmat,
            nlevels=10,
            xlab="u", ylab="v",
            main="Copula contours K(u,v) (formula-based)")
  })

  output$dependenceMeasures <- renderPrint({
    r <- res()
    cat("Kendall tau (simulated from model):  ", sprintf("%.4f", r$tau), "\n", sep="")
    cat("Spearman rho (simulated from model): ", sprintf("%.4f", r$rho), "\n", sep="")
    cat("\nNote: Copula surface is computed from manuscript formulas.\n")
    cat("Dependence measures are estimated by simulation for speed.\n")
  })

  sens_res <- eventReactive(input$run_sens, {
    req(input$do_sensitivity)

    # Read current marginals (fixed during sensitivity run)
    p1 <- get_params(input, "v1", input$dist_v1)
    p2 <- get_params(input, "v2", input$dist_v2)
    p3 <- get_params(input, "v3", input$dist_v3)

    copfam <- tolower(input$copula_type)

    # Respect theta constraints
    th_min_allowed <- if (copfam == "gumbel") 1 else 1e-3

    th_seq <- seq(input$theta_range[1], input$theta_range[2], by = input$theta_step)
    th_seq <- th_seq[th_seq >= th_min_allowed]

    grid <- seq(0.01, 0.99, length.out = input$grid_sens_n)

    out <- data.frame(
      theta = th_seq,
      K_05_05 = NA_real_,
      mean_abs_dev = NA_real_
    )

    for (i in seq_along(th_seq)) {
      model_i <- make_model(input$dist_v1, p1, input$dist_v2, p2, input$dist_v3, p3,
                            copula_family = copfam, theta = th_seq[i])

      Kmat_i <- K_surface(grid, model_i)  # <-- SAME manuscript formula as main plots
      s <- summarize_K_surface(grid, Kmat_i)

      out$K_05_05[i] <- s$K_05_05
      out$mean_abs_dev[i] <- s$mean_abs_dev
    }

    list(grid = grid, table = out)
  })
  output$sensitivityTable <- renderTable({
    req(input$do_sensitivity)
    sens_res()$table
  }, digits = 5)

  output$sensitivityPlot <- renderPlot({
    req(input$do_sensitivity)
    d <- sens_res()$table
    if (nrow(d) < 2) return(NULL)

    plot(d$theta, d$mean_abs_dev, type = "b",
         xlab = expression(theta), ylab = "Sensitivity metric",
         main = "Sensitivity (formula-based)")
    lines(d$theta, d$K_05_05, type = "b")
    legend("topleft",
           legend = c("mean |K - uv|", "K(0.5,0.5)"),
           bty = "n", lty = 1, pch = 1)
  })

}

shiny::shinyApp(ui = ui, server = server)
