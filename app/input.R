# ─── Utilities ─────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

read_delim_auto <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("tsv", "txt")) {
        fread(path, sep="\t")
    } else {
        fread(path, sep=",")
    }
}

# ─── Demo Data Generator ──────────────────────────────────────────────────────

DemoDataCache <- reactiveVal(NULL)

generate_demo_data <- function() {
    set.seed(42)
    n <- 200

    patient_id <- paste0("PT", sprintf("%03d", 1:n))

    # Treatment groups with different survival profiles
    group <- sample(c("Treatment_A", "Treatment_B", "Control"), n,
                    replace=TRUE, prob=c(0.35, 0.35, 0.30))

    sex   <- sample(c("Male", "Female"), n, replace=TRUE)
    age   <- round(rnorm(n, mean=60, sd=12))
    age   <- pmax(25, pmin(85, age))
    stage <- sample(c("Stage_I", "Stage_II", "Stage_III", "Stage_IV"), n,
                    replace=TRUE, prob=c(0.25, 0.30, 0.25, 0.20))

    # Weibull-distributed event times with group- and covariate-specific hazards
    base_rate <- ifelse(group == "Treatment_A", 0.002,
                 ifelse(group == "Treatment_B", 0.004, 0.008))
    stage_mult <- ifelse(stage == "Stage_I",   0.5,
                  ifelse(stage == "Stage_II",  1.0,
                  ifelse(stage == "Stage_III", 1.5, 2.5)))
    age_mult   <- exp(0.02 * (age - 60))
    hazard     <- base_rate * stage_mult * age_mult

    time_event  <- rweibull(n, shape=1.2, scale=1/hazard)
    time_censor <- runif(n, min=100, max=2000)

    time   <- pmin(time_event, time_censor)
    time   <- round(pmax(time, 1))
    status <- as.integer(time_event <= time_censor)

    clinical <- data.table(
        patient_id = patient_id,
        time       = time,
        status     = status,
        group      = group,
        sex        = sex,
        age        = age,
        stage      = stage
    )

    # Expression matrix: 50 genes x 200 samples
    gene_names <- c(paste0("SURV_GENE_", 1:10), paste0("NOISE_GENE_", 1:40))
    n_genes    <- length(gene_names)
    expr_mat   <- matrix(rnorm(n_genes * n, mean=8, sd=2), nrow=n_genes, ncol=n)

    # Make SURV_GENE_1..5 protective (high expr = longer survival)
    # Make SURV_GENE_6..10 risk genes (high expr = shorter survival)
    for (i in 1:10) {
        noise <- rnorm(n, sd=1.5)
        expr_mat[i, ] <- 5 + 3 * as.numeric(scale(log(time + 1))) + noise
        if (i > 5) {
            expr_mat[i, ] <- 14 - expr_mat[i, ]
        }
    }

    expr_df <- data.table(Gene = gene_names)
    expr_df <- cbind(expr_df, as.data.table(round(expr_mat, 3)))
    colnames(expr_df)[-1] <- patient_id

    list(clinical = clinical, expression = expr_df)
}

# ─── Data Loading Reactives ───────────────────────────────────────────────────

ClinicalReactive <- reactive({
    if (input$DemoData == TRUE) {
        shiny::validate(need(!is.null(input$clinical_file),
            "Please upload a clinical data file."))
        tryCatch(
            read_delim_auto(input$clinical_file$datapath),
            error = function(e) {
                showNotification(paste("Clinical file error:", e$message),
                    type='error', duration=NULL)
                NULL
            }
        )
    } else {
        cached <- DemoDataCache()
        if (is.null(cached)) {
            cached <- generate_demo_data()
            DemoDataCache(cached)
        }
        cached$clinical
    }
})

ExprReactive <- reactive({
    if (input$DemoData == TRUE) {
        req(input$expr_file)
        tryCatch(
            read_delim_auto(input$expr_file$datapath),
            error = function(e) {
                showNotification(paste("Expression matrix error:", e$message),
                    type='error', duration=NULL)
                NULL
            }
        )
    } else {
        cached <- DemoDataCache()
        if (is.null(cached)) {
            cached <- generate_demo_data()
            DemoDataCache(cached)
        }
        cached$expression
    }
})

HasExpression <- reactive({
    expr <- tryCatch(ExprReactive(), error=function(e) NULL)
    !is.null(expr) && ncol(expr) > 1
})

output$has_expression <- reactive({ HasExpression() })
outputOptions(output, "has_expression", suspendWhenHidden=FALSE)

# ─── Column Selectors (custom upload only) ────────────────────────────────────

output$column_selectors <- renderUI({
    req(input$clinical_file)
    dat <- tryCatch(read_delim_auto(input$clinical_file$datapath), error=function(e) NULL)
    req(dat)
    cols <- colnames(dat)

    # Smart guessing for common survival column names
    guess_time <- cols[min(2, length(cols))]
    time_match <- grep("time|surv.*time|os_time|dfs_time|pfs_time|days|months",
                       cols, ignore.case=TRUE, value=TRUE)
    if (length(time_match) > 0) guess_time <- time_match[1]

    guess_event <- cols[min(3, length(cols))]
    event_match <- grep("status|event|dead|vital|os_status|censor",
                        cols, ignore.case=TRUE, value=TRUE)
    if (length(event_match) > 0) guess_event <- event_match[1]

    tagList(
        h4("Map Columns", style="color:#0F344C;"),
        selectInput("time_col", "Time-to-event column:", choices=cols, selected=guess_time),
        selectInput("event_col", "Event/Status column:", choices=cols, selected=guess_event),
        selectInput("time_unit", "Time unit (for axis label):",
            choices=c("Days"="days", "Months"="months", "Years"="years"),
            selected="days"),
        hr()
    )
})

# ─── Processed Survival Data ──────────────────────────────────────────────────

ProcessedSurvData <- reactive({
    clinical <- ClinicalReactive()
    req(clinical)

    # Determine column mapping
    if (input$DemoData == TRUE) {
        time_col  <- input$time_col %||% "time"
        event_col <- input$event_col %||% "status"
    } else {
        time_col  <- "time"
        event_col <- "status"
    }

    shiny::validate(
        need(time_col %in% colnames(clinical),
            paste("Time column '", time_col, "' not found in data.")),
        need(event_col %in% colnames(clinical),
            paste("Event column '", event_col, "' not found in data."))
    )

    # Build data.frame with standardized time/status + all other columns
    df <- data.frame(
        time   = as.numeric(clinical[[time_col]]),
        status = clinical[[event_col]],
        stringsAsFactors = FALSE
    )

    # Handle event status encoding
    if (is.logical(df$status)) {
        df$status <- as.integer(df$status)
    } else if (is.character(df$status) || is.factor(df$status)) {
        s <- tolower(as.character(df$status))
        df$status <- ifelse(s %in% c("dead", "deceased", "event", "yes", "1", "true",
                                      "relapse", "recurrence", "progressed"), 1,
                     ifelse(s %in% c("alive", "living", "censored", "no", "0", "false",
                                      "no_event", "stable"), 0, NA_integer_))
    } else {
        df$status <- as.integer(df$status)
        # Handle 1/2 encoding (common in some clinical datasets)
        if (all(df$status %in% c(1, 2), na.rm=TRUE)) {
            df$status <- df$status - 1L
        }
    }

    # Attach all other clinical columns
    other_cols <- setdiff(colnames(clinical), c(time_col, event_col))
    for (col in other_cols) {
        df[[col]] <- as.character(clinical[[col]])
        # Try to preserve numeric columns
        num_vals <- suppressWarnings(as.numeric(df[[col]]))
        if (!all(is.na(num_vals)) && sum(is.na(num_vals)) == sum(is.na(df[[col]]))) {
            df[[col]] <- num_vals
        }
    }

    # Clean: remove NA time/status, require time > 0
    df <- df[!is.na(df$time) & !is.na(df$status), ]
    df <- df[df$time > 0, ]

    shiny::validate(need(nrow(df) >= 2, "Not enough valid observations in the data."))
    df
})

# ─── Time Unit Label ──────────────────────────────────────────────────────────

TimeUnitLabel <- reactive({
    if (input$DemoData == TRUE) {
        unit <- input$time_unit %||% "days"
    } else {
        unit <- "days"
    }
    paste0("Time (", unit, ")")
})

# ─── Preview Tables ───────────────────────────────────────────────────────────

output$clinical_table <- DT::renderDataTable({
    dat <- ClinicalReactive()
    req(dat)
    DT::datatable(dat, style='bootstrap', options=list(pageLength=15, scrollX=TRUE))
})

output$expr_table <- DT::renderDataTable({
    expr <- tryCatch(ExprReactive(), error=function(e) NULL)
    req(expr)
    DT::datatable(expr, style='bootstrap', options=list(pageLength=15, scrollX=TRUE))
})

# ─── Populate Stratification Dropdowns ────────────────────────────────────────

observe({
    df <- tryCatch(ProcessedSurvData(), error=function(e) NULL)
    req(df)
    other_cols <- setdiff(colnames(df), c("time", "status"))

    # Categorical variables: character/factor OR numeric with <= 10 unique values
    cat_cols <- other_cols[sapply(other_cols, function(x) {
        is.character(df[[x]]) || is.factor(df[[x]]) ||
        (is.numeric(df[[x]]) && length(unique(df[[x]])) <= 10)
    })]
    updateSelectInput(session, "strat_clinical_var", choices=cat_cols)

    # Numeric columns with > 10 unique values
    num_cols <- other_cols[sapply(other_cols, function(x) {
        is.numeric(df[[x]]) && length(unique(df[[x]])) > 10
    })]
    updateSelectInput(session, "strat_cont_var", choices=num_cols)
})

observe({
    expr <- tryCatch(ExprReactive(), error=function(e) NULL)
    if (!is.null(expr) && ncol(expr) > 1) {
        gene_list <- as.character(expr[[1]])
        updateSelectizeInput(session, "strat_gene", choices=gene_list, server=TRUE,
            options=list(maxOptions=100, placeholder='Type gene name...'))
    }
})
