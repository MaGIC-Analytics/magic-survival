# ─── Cutoff Helper ─────────────────────────────────────────────────────────────

apply_cutoff <- function(values, method, custom_val=NULL, surv_df=NULL) {
    if (method == "median") {
        med <- median(values, na.rm=TRUE)
        ifelse(values > med, "High", "Low")

    } else if (method == "tertile") {
        q <- quantile(values, probs=c(1/3, 2/3), na.rm=TRUE)
        ifelse(values <= q[1], "Low", ifelse(values <= q[2], "Medium", "High"))

    } else if (method == "quartile") {
        q <- quantile(values, probs=c(0.25, 0.5, 0.75), na.rm=TRUE)
        as.character(cut(values, breaks=c(-Inf, q, Inf),
                         labels=c("Q1","Q2","Q3","Q4"), include.lowest=TRUE))

    } else if (method == "optimal") {
        # surv_cutpoint from survminer (uses maxstat internally)
        tmp <- data.frame(time=surv_df$time, status=surv_df$status,
                          variable=values, stringsAsFactors=FALSE)
        tmp <- tmp[complete.cases(tmp), ]

        tryCatch({
            cp <- surv_cutpoint(tmp, time="time", event="status", variables="variable")
            cat_data <- surv_categorize(cp)
            as.character(cat_data$variable)
        }, error=function(e) {
            showNotification(
                paste("Optimal cutpoint failed:", e$message, "-- falling back to median split."),
                type='warning', duration=6)
            med <- median(values, na.rm=TRUE)
            ifelse(values > med, "High", "Low")
        })

    } else if (method == "custom") {
        ifelse(values > custom_val, "High", "Low")
    }
}

# ─── Stratified Data Reactive ─────────────────────────────────────────────────

StratifiedData <- reactive({
    df <- ProcessedSurvData()
    req(df)

    mode <- input$strat_mode

    if (mode == "clinical") {
        var <- input$strat_clinical_var
        req(var)
        shiny::validate(need(var %in% colnames(df),
            "Selected variable not found in data."))
        df$strata <- as.character(df[[var]])

    } else if (mode == "gene") {
        expr <- ExprReactive()
        req(expr)
        gene <- input$strat_gene
        req(gene)

        gene_idx <- which(as.character(expr[[1]]) == gene)
        shiny::validate(need(length(gene_idx) > 0,
            paste("Gene", gene, "not found in expression matrix.")))

        gene_vals  <- as.numeric(expr[gene_idx, -1, with=FALSE])
        sample_ids <- colnames(expr)[-1]

        clinical <- ClinicalReactive()
        id_col   <- colnames(clinical)[1]

        # Build lookup from expression data
        gene_lookup <- setNames(gene_vals, sample_ids)

        # Match expression values to processed data rows
        clinical_ids <- as.character(clinical[[id_col]])
        # The processed df rows correspond to clinical rows (after cleaning)
        # We need to align by tracking which clinical rows survived processing
        df$expr_val <- NA_real_
        for (i in seq_len(nrow(df))) {
            # Find matching sample ID
            sid <- clinical_ids[i]
            if (!is.na(sid) && sid %in% names(gene_lookup)) {
                df$expr_val[i] <- gene_lookup[[sid]]
            }
        }

        df <- df[!is.na(df$expr_val), ]
        shiny::validate(need(nrow(df) >= 10,
            "Too few samples matched between expression matrix and clinical data."))

        method <- input$gene_cutoff_method
        custom_val <- if (method == "custom") input$gene_cutoff_value else NULL
        df$strata <- apply_cutoff(df$expr_val, method, custom_val, df)

    } else if (mode == "continuous") {
        var <- input$strat_cont_var
        req(var)
        shiny::validate(need(var %in% colnames(df),
            "Selected variable not found in data."))

        vals <- as.numeric(df[[var]])
        method <- input$cont_cutoff_method
        custom_val <- if (method == "custom") input$cont_cutoff_value else NULL
        df$strata <- apply_cutoff(vals, method, custom_val, df)
    }

    df <- df[!is.na(df$strata), ]
    shiny::validate(need(nrow(df) >= 2, "Not enough observations after stratification."))
    shiny::validate(need(length(unique(df$strata)) >= 2,
        "Need at least 2 groups for comparison. Try a different stratification."))
    # Ensure all groups have at least 1 patient
    grp_counts <- table(df$strata)
    shiny::validate(need(all(grp_counts >= 1),
        "One or more groups has no patients. Adjust the cutoff."))

    df
})

# ─── Number of Groups Reactive ────────────────────────────────────────────────

NumGroups <- reactive({
    df <- tryCatch(StratifiedData(), error=function(e) NULL)
    if (is.null(df)) return(0)
    length(unique(df$strata))
})

# ─── KM Plot Reactive ─────────────────────────────────────────────────────────

KMPlotter <- reactive({
    df <- StratifiedData()
    req(df)

    fit <- survfit(Surv(time, status) ~ strata, data=df)

    n_groups <- length(unique(df$strata))
    time_label <- tryCatch(TimeUnitLabel(), error=function(e) "Time")

    # Build color palette
    pal <- input$color_palette %||% "default"
    palette_arg <- NULL
    if (pal == "custom") {
        groups <- sort(unique(df$strata))
        colors <- sapply(groups, function(grp) {
            id <- paste0("custom_color_", gsub("[^A-Za-z0-9]", "_", grp))
            input[[id]] %||% "#000000"
        })
        palette_arg <- unname(colors)
    } else if (pal != "default") {
        palette_arg <- pal
    }

    # Build ggsurvplot arguments
    args <- list(
        fit        = fit,
        data       = df,
        pval       = isTRUE(input$show_pval),
        conf.int   = isTRUE(input$show_ci),
        risk.table = isTRUE(input$show_risk_table),
        censor     = isTRUE(input$show_censor),
        xlab       = time_label,
        ylab       = "Survival Probability",
        legend     = input$legend_pos %||% "top",
        legend.title = if (isTRUE(input$show_legend_title))
                           (input$legend_title_text %||% "Strata") else "",
        font.main    = c(input$font_title %||% 16, "bold"),
        font.x       = c(input$font_axis %||% 12),
        font.y       = c(input$font_axis %||% 12),
        font.legend  = c(input$font_legend %||% 10),
        font.tickslab = c(input$font_axis %||% 12),
        risk.table.fontsize = (input$font_risk %||% 10) / 3,
        ggtheme = theme_bw()
    )

    if (!is.null(palette_arg)) {
        args$palette <- palette_arg
    }

    # Axes
    xmax_val  <- input$xmax
    xbreak_val <- input$xbreak
    if (!is.null(xmax_val) && !is.na(xmax_val) && xmax_val > 0) {
        args$xlim <- c(0, xmax_val)
    }
    if (!is.null(xbreak_val) && !is.na(xbreak_val) && xbreak_val > 0) {
        args$break.x.by <- xbreak_val
    }

    if (isTRUE(input$log_yaxis)) {
        args$fun <- "log"
    }

    if (isTRUE(input$yaxis_zero) && !isTRUE(input$log_yaxis)) {
        args$ylim <- c(0, 1)
    }

    # Generate the plot
    p <- do.call(ggsurvplot, args)

    # Add HR annotation for 2-group comparisons
    if (n_groups == 2 && isTRUE(input$show_hr)) {
        tryCatch({
            cox_fit <- coxph(Surv(time, status) ~ strata, data=df)
            hr    <- exp(coef(cox_fit))
            hr_ci <- exp(confint(cox_fit))
            hr_text <- sprintf("HR = %.2f (95%% CI: %.2f - %.2f)",
                               hr[1], hr_ci[1,1], hr_ci[1,2])
            p$plot <- p$plot +
                ggplot2::annotate("text", x=Inf, y=0.1, label=hr_text,
                    hjust=1.1, vjust=0, size=3.5, fontface="italic")
        }, error=function(e) {
            # Silently skip HR annotation on error
        })
    }

    p
})

# ─── Render KM Plot ───────────────────────────────────────────────────────────

output$km_plot_out <- renderPlot({
    p <- KMPlotter()
    req(p)
    print(p)
}, height = function() input$km_height %||% 600,
   width  = function() input$km_width %||% 800)

outputOptions(output, "km_plot_out", suspendWhenHidden=FALSE)

# ─── Summary Statistics Table ─────────────────────────────────────────────────

KMSummary <- reactive({
    df <- StratifiedData()
    req(df)

    fit <- survfit(Surv(time, status) ~ strata, data=df)

    summ <- summary(fit)$table
    # Handle single-group edge case
    if (is.null(dim(summ))) {
        summ <- t(as.matrix(summ))
    }

    result <- data.frame(
        Group           = gsub("strata=", "", rownames(summ)),
        N               = as.integer(summ[, "records"]),
        Events          = as.integer(summ[, "events"]),
        Median_Survival = round(summ[, "median"], 1),
        CI_Lower_95     = round(summ[, "0.95LCL"], 1),
        CI_Upper_95     = round(summ[, "0.95UCL"], 1),
        stringsAsFactors = FALSE
    )
    colnames(result) <- c("Group", "N", "Events", "Median Survival",
                           "95% CI Lower", "95% CI Upper")
    result
})

output$km_summary_table <- DT::renderDataTable({
    summ <- KMSummary()
    req(summ)
    DT::datatable(summ, style='bootstrap',
        options=list(pageLength=10, scrollX=TRUE, dom='t'),
        rownames=FALSE)
})

# ─── Log-Rank Test & Hazard Ratio Display ─────────────────────────────────────

output$logrank_hr_display <- renderUI({
    df <- StratifiedData()
    req(df)

    # Log-rank test
    lr   <- survdiff(Surv(time, status) ~ strata, data=df)
    pval <- 1 - pchisq(lr$chisq, df=length(lr$n) - 1)

    pval_text <- if (pval < 0.001)
        sprintf("p < 0.001 (chi-sq = %.2f, df = %d)", lr$chisq, length(lr$n) - 1)
    else
        sprintf("p = %.4f (chi-sq = %.2f, df = %d)", pval, lr$chisq, length(lr$n) - 1)

    n_groups <- length(unique(df$strata))

    elements <- tagList(
        h4("Log-Rank Test", style="color:#0F344C;"),
        p(strong("Result: "), pval_text)
    )

    if (n_groups == 2) {
        tryCatch({
            cox_fit <- coxph(Surv(time, status) ~ strata, data=df)
            hr    <- exp(coef(cox_fit))
            hr_ci <- exp(confint(cox_fit))
            hr_text <- sprintf("%.3f (95%% CI: %.3f - %.3f)",
                               hr[1], hr_ci[1,1], hr_ci[1,2])
            ref_name <- gsub("strata", "", names(coef(cox_fit))[1])

            elements <- tagList(elements,
                hr(),
                h4("Hazard Ratio (Cox Proportional Hazards)", style="color:#0F344C;"),
                p(strong("HR: "), hr_text),
                p(em("Comparison: "), ref_name)
            )
        }, error=function(e) {
            elements <- tagList(elements,
                hr(),
                p(em("Could not compute hazard ratio: ", e$message), style="color:#999;")
            )
        })
    } else {
        elements <- tagList(elements,
            hr(),
            p(em("Hazard ratio is displayed only for two-group comparisons."),
              style="color:#999;")
        )
    }

    div(class="well", elements)
})

# ─── Dynamic Custom Color Pickers ─────────────────────────────────────────────

output$custom_color_pickers <- renderUI({
    df <- tryCatch(StratifiedData(), error=function(e) NULL)
    req(df)
    groups <- sort(unique(df$strata))
    default_colors <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3",
                        "#FF7F00","#FFFF33","#A65628","#F781BF")

    color_inputs <- lapply(seq_along(groups), function(i) {
        colourInput(
            inputId = paste0("custom_color_", gsub("[^A-Za-z0-9]", "_", groups[i])),
            label   = groups[i],
            value   = default_colors[((i - 1) %% length(default_colors)) + 1]
        )
    })
    do.call(tagList, color_inputs)
})

# ─── Download Handlers ────────────────────────────────────────────────────────

output$download_km <- downloadHandler(
    filename = function() {
        paste0("kaplan_meier.", input$km_download_format)
    },
    content = function(file) {
        p    <- KMPlotter()
        fmt  <- input$km_download_format
        h_px <- input$km_height %||% 600
        w_px <- input$km_width  %||% 800

        if (fmt %in% c("pdf", "svg", "eps")) {
            h_in <- h_px / 96
            w_in <- w_px / 96
            if (fmt == "pdf")      pdf(file, height=h_in, width=w_in)
            else if (fmt == "svg") svg(file, height=h_in, width=w_in)
            else { setEPS(); postscript(file, height=h_in, width=w_in) }
        } else {
            if (fmt == "png")       png(file, height=h_px, width=w_px, res=96)
            else if (fmt == "jpeg") jpeg(file, height=h_px, width=w_px, res=96)
            else if (fmt == "tiff") tiff(file, height=h_px, width=w_px, res=96)
        }
        print(p)
        dev.off()
    }
)

output$download_stats <- downloadHandler(
    filename = function() "survival_summary.csv",
    content = function(file) {
        write.csv(KMSummary(), file, row.names=FALSE)
    }
)

# ─── Help Modal ───────────────────────────────────────────────────────────────

show_surv_help_ui <- function() {
    showModal(modalDialog(
        title     = tagList(icon("circle-question"), " Survival Analysis Tool Help"),
        size      = "l",
        easyClose = TRUE,
        footer    = modalButton("Close"),
        tabsetPanel(
            tabPanel("Overview",
                br(),
                h4("What is a Kaplan-Meier Plot?"),
                p("A Kaplan-Meier (KM) plot visualizes the estimated survival function as a step function over time.",
                  "The x-axis is time, the y-axis is the probability of surviving beyond that time.",
                  "Each step down corresponds to an event (e.g., death). Censored observations are shown as tick marks."),
                h4("Interpreting the Plot"),
                tags$ul(
                    tags$li(strong("Curves higher up:"), " better survival (more patients alive for longer)."),
                    tags$li(strong("Curves dropping steeply:"), " rapid occurrence of events."),
                    tags$li(strong("Separated curves:"), " potential survival difference between groups."),
                    tags$li(strong("Crossing curves:"), " may violate proportional hazards assumption; interpret HR with caution.")
                ),
                h4("Statistical Tests"),
                p("The log-rank test compares curves between groups. The hazard ratio (HR) from Cox regression quantifies
                  the relative risk between exactly two groups.")
            ),
            tabPanel("Input Data",
                br(),
                h4("Clinical Data (Required)"),
                tags$ul(
                    tags$li(strong("Time column:"), " numeric time-to-event (days, months, or years)."),
                    tags$li(strong("Event/Status column:"), " 0/1 (0 = censored, 1 = event) or TRUE/FALSE."),
                    tags$li("Additional categorical or continuous columns can be used for stratification.")
                ),
                h4("Expression Matrix (Optional)"),
                tags$ul(
                    tags$li("Genes as rows, samples as columns."),
                    tags$li("First column = gene identifiers."),
                    tags$li("Sample IDs must match patient IDs in the clinical data."),
                    tags$li("Enables gene expression-based stratification.")
                )
            ),
            tabPanel("Controls",
                br(),
                h4("Stratification"),
                tags$ul(
                    tags$li(strong("Clinical variable:"), " split by a categorical column (e.g., treatment group, stage)."),
                    tags$li(strong("Gene expression:"), " split by expression of a selected gene using median, tertiles, quartiles, optimal cutpoint, or a custom threshold."),
                    tags$li(strong("Continuous clinical:"), " split a numeric variable (e.g., age) using the same cutoff methods.")
                ),
                h4("Statistics"),
                p("Toggle p-value (log-rank test), hazard ratio (2 groups only), number-at-risk table, confidence intervals, and censoring marks."),
                h4("Color, Fonts, Axes, Legend"),
                p("Choose from journal-standard palettes (NEJM, JCO, Lancet, JAMA) or define custom colors.",
                  " Adjust font sizes for title, axes, legend, and risk table.",
                  " Set axis limits, break intervals, and log-transform the y-axis.",
                  " Reposition the legend or hide it entirely."),
                h4("Resize & Download"),
                p("Use the Resize panel to set pixel dimensions. Download in PDF, PNG, TIFF, JPEG, SVG, or EPS.")
            )
        )
    ))
}

observeEvent(input$show_help_float, { show_surv_help_ui() })

# ─── Code Modal ───────────────────────────────────────────────────────────────

observeEvent(input$show_code_modal, {
    df <- isolate(tryCatch(StratifiedData(), error=function(e) NULL))
    if (is.null(df)) {
        code <- "# Generate a KM plot first, then click here to get the reproducible code."
    } else {
        n_groups <- length(unique(df$strata))
        pal <- isolate(input$color_palette %||% "default")

        palette_line <- if (pal == "custom") {
            groups <- sort(unique(df$strata))
            colors <- sapply(groups, function(grp) {
                id <- paste0("custom_color_", gsub("[^A-Za-z0-9]", "_", grp))
                isolate(input[[id]] %||% "#000000")
            })
            sprintf('    palette = c(%s),',
                paste0('"', unname(colors), '"', collapse=", "))
        } else if (pal != "default") {
            sprintf('    palette = "%s",', pal)
        } else {
            ""
        }

        xlim_line <- ""
        xmax_val <- isolate(input$xmax)
        if (!is.null(xmax_val) && !is.na(xmax_val) && xmax_val > 0) {
            xlim_line <- sprintf('    xlim = c(0, %s),', xmax_val)
        }

        xbreak_line <- ""
        xbreak_val <- isolate(input$xbreak)
        if (!is.null(xbreak_val) && !is.na(xbreak_val) && xbreak_val > 0) {
            xbreak_line <- sprintf('    break.x.by = %s,', xbreak_val)
        }

        fun_line <- ""
        if (isolate(isTRUE(input$log_yaxis))) {
            fun_line <- '    fun = "log",'
        }

        legend_title <- if (isolate(isTRUE(input$show_legend_title)))
            sprintf('    legend.title = "%s",', isolate(input$legend_title_text %||% "Strata"))
        else '    legend.title = "",'

        code <- paste0(
            "library(survival)\nlibrary(survminer)\n\n",
            "# data <- read.csv('your_clinical_data.csv')\n",
            "# Ensure columns: time (numeric), status (0/1), strata (grouping variable)\n\n",
            "fit <- survfit(Surv(time, status) ~ strata, data = data)\n\n",
            "ggsurvplot(\n",
            "    fit,\n",
            "    data = data,\n",
            sprintf('    pval = %s,\n', tolower(as.character(isolate(isTRUE(input$show_pval))))),
            sprintf('    conf.int = %s,\n', tolower(as.character(isolate(isTRUE(input$show_ci))))),
            sprintf('    risk.table = %s,\n', tolower(as.character(isolate(isTRUE(input$show_risk_table))))),
            sprintf('    censor = %s,\n', tolower(as.character(isolate(isTRUE(input$show_censor))))),
            if (nchar(palette_line) > 0) paste0(palette_line, "\n") else "",
            sprintf('    legend = "%s",\n', isolate(input$legend_pos %||% "top")),
            legend_title, "\n",
            if (nchar(xlim_line) > 0) paste0(xlim_line, "\n") else "",
            if (nchar(xbreak_line) > 0) paste0(xbreak_line, "\n") else "",
            if (nchar(fun_line) > 0) paste0(fun_line, "\n") else "",
            '    ggtheme = theme_bw()\n',
            ")\n"
        )
    }

    showModal(modalDialog(
        title     = tagList(icon("file-code"), " Reproducible R Code"),
        size      = "l",
        easyClose = TRUE,
        footer    = modalButton("Close"),
        p("Copy this code to reproduce your current Kaplan-Meier plot in an offline R session.",
          style="color:#555; margin-bottom:12px;"),
        tags$pre(
            style = paste(
                "background:#1e1e1e; color:#d4d4d4; border-radius:6px;",
                "padding:16px; font-size:12px; max-height:520px; overflow-y:auto;",
                "white-space:pre; font-family:'Courier New', monospace;"
            ),
            code
        )
    ))
})
