library(shiny)
require(shinyjs)
library(shinythemes)
require(shinycssloaders)
library(shinyWidgets)

library(DT)
library(tidyverse)
library(data.table)
library(colourpicker)
library(RColorBrewer)
library(survival)
library(survminer)

tagList(
    tags$head(
        #includeHTML(("www/GA.html")),
        tags$style(type = 'text/css','.navbar-brand{display:none;}'),
        tags$style(HTML("
            .control-group-panel {
                border: 1px solid #ddd;
                border-radius: 6px;
                padding: 10px 12px;
                margin-bottom: 10px;
                background-color: #f9f9f9;
            }
            .control-group-title {
                font-weight: bold;
                font-size: 14px;
                color: #0F344C;
                margin-bottom: 8px;
            }
            #show_help_float {
                position: fixed;
                bottom: 28px;
                right: 28px;
                z-index: 9999;
                border-radius: 50%;
                width: 46px;
                height: 46px;
                font-size: 20px;
                padding: 0;
                box-shadow: 0 3px 8px rgba(0,0,0,0.25);
            }
        "))
    ),
    ## Global always-visible help button (fixed bottom-right)
    actionButton("show_help_float", label=NULL,
        icon=icon("circle-question"),
        title="Help & documentation",
        class="btn btn-info"
    ),
    fluidPage(theme = shinytheme('yeti'),
            windowTitle = "MaGIC Survival Analysis Tool",
            useShinyjs(),
            titlePanel(
                fluidRow(
                column(2, tags$a(href='http://www.bioinformagic.io/', tags$img(height=75, src="MaGIC_Icon_0f344c.svg")), align='center'),
                column(10, fluidRow(
                    column(10, h1(strong('MaGIC Survival Analysis Tool'), align='center', style="color:#0F344C;"))
                ))
                ),
                windowTitle = "MaGIC Survival Analysis Tool"),
                tags$style(type='text/css', '.navbar{font-size:20px;}'),
                tags$style(type='text/css', '.nav-tabs{padding-bottom:20px;}'),
                tags$style(type='text/css', '.navbar-default{background-color:#0F344C;}'),
                tags$style(type='text/css', HTML('.navbar { background-color: #0F344C;}
                          .tab-panel{ background-color: #0F344C;}
                          .navbar-default .navbar-nav > .active > a,
                           .navbar-default .navbar-nav > .active > a:focus,
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: white;
                                background-color: #008cba;
                            }')
                          ),
                tags$head(tags$style(".modal-dialog{ width:1300px}")),

        navbarPage(title="", id='NAVTABS',

        ## Intro Page
##########################################################################################################################################################
            tabPanel('Introduction',
                fluidRow(
                    column(2),
                    column(8,
                        column(12, align="center", style="margin-bottom:25px;",
                            h3(markdown("Welcome to the Survival Analysis Tool by the
                            [Molecular and Genomics Informatics Core (MaGIC)](http://www.bioinformagic.io)."))),
                        hr(),
                        h4("How to Use This Tool", style="color:#0F344C;"),
                        tags$ol(
                            tags$li(strong("Navigate to the Data Input tab."),
                                " Upload your clinical data file and (optionally) an expression matrix, or click 'Load Demo Data' to explore with a synthetic example."),
                            tags$li(strong("Map your columns."),
                                " Select the time-to-event and event/status columns from your data. Choose the appropriate time unit for axis labeling."),
                            tags$li(strong("Submit your data."),
                                " Click Submit. The Kaplan-Meier Analysis tab will appear once data is loaded."),
                            tags$li(strong("Stratify and visualize."),
                                " Choose how to split patients into groups: by a clinical variable, gene expression level, or a continuous clinical variable with a cutoff method."),
                            tags$li(strong("Customize and download."),
                                " Fine-tune colors, fonts, axes, and legend. Download publication-quality plots and summary statistics.")
                        ),
                        hr(),
                        h4("Survival Analysis Concepts", style="color:#0F344C;"),
                        fluidRow(
                            column(6,
                                div(class="control-group-panel",
                                    h5(strong("Time-to-Event & Events"), style="color:#0F344C;"),
                                    p("Survival analysis studies the time until a specific event occurs (e.g., death, disease recurrence, progression).
                                      The", strong("time-to-event"), "is the duration from a defined starting point (diagnosis, treatment start) to the event of interest."),
                                    p("An", strong("event"), "(status = 1) means the outcome was observed. If the patient was lost to follow-up or the study ended before the event,
                                      they are", strong("censored"), "(status = 0). Censored observations still contribute information about survival up to the point of censoring.")
                                )
                            ),
                            column(6,
                                div(class="control-group-panel",
                                    h5(strong("Kaplan-Meier Estimator"), style="color:#0F344C;"),
                                    p("The", strong("Kaplan-Meier (KM) estimator"), "is a non-parametric method that estimates the survival function S(t) as a step function.
                                      At each time an event occurs, the survival probability is recalculated. The resulting KM curve shows the probability of surviving beyond each time point."),
                                    p("Censored patients are marked with tick marks on the curve, indicating they were still event-free when last observed.")
                                )
                            )
                        ),
                        fluidRow(
                            column(6,
                                div(class="control-group-panel",
                                    h5(strong("Log-Rank Test"), style="color:#0F344C;"),
                                    p("The", strong("log-rank test"), "compares survival distributions between two or more groups. It tests the null hypothesis that there is no difference
                                      in survival between groups. A small p-value (e.g., < 0.05) suggests a statistically significant difference."),
                                    p("The test compares observed vs. expected events at each time point across groups, giving equal weight to all time points.")
                                )
                            ),
                            column(6,
                                div(class="control-group-panel",
                                    h5(strong("Hazard Ratio"), style="color:#0F344C;"),
                                    p("The", strong("hazard ratio (HR)"), "from a Cox proportional hazards model quantifies the relative risk between two groups."),
                                    tags$ul(
                                        tags$li("HR = 1: no difference in hazard between groups"),
                                        tags$li("HR > 1: increased hazard (worse survival) in the test group"),
                                        tags$li("HR < 1: decreased hazard (better survival) in the test group")
                                    ),
                                    p("A 95% confidence interval that does not cross 1 indicates statistical significance. HR is only meaningful for two-group comparisons.")
                                )
                            )
                        ),
                        hr(),
                        h4("Required Input Data", style="color:#0F344C;"),
                        fluidRow(
                            column(6,
                                div(class="control-group-panel",
                                    h5(strong("Clinical Data (required)"), style="color:#0F344C;"),
                                    tags$ul(
                                        tags$li("File format: CSV or TSV"),
                                        tags$li("Rows: Patients / samples (one per row)"),
                                        tags$li("Must include a", strong("time-to-event column"), "(numeric: days, months, or years)"),
                                        tags$li("Must include an", strong("event/status column"), "(0/1 or FALSE/TRUE, where 1 = event occurred)"),
                                        tags$li("Optional: grouping columns (treatment arm, stage, etc.) for stratification")
                                    ),
                                    tags$pre("patient_id, time, status, group,    stage\nPT001,      450,  1,      Treatment, Stage_II\nPT002,      820,  0,      Control,   Stage_I")
                                )
                            ),
                            column(6,
                                div(class="control-group-panel",
                                    h5(strong("Expression Matrix (optional)"), style="color:#0F344C;"),
                                    tags$ul(
                                        tags$li("File format: CSV or TSV"),
                                        tags$li("Rows: Genes (one gene per row)"),
                                        tags$li("Columns: Samples (must match patient IDs in clinical data)"),
                                        tags$li("First column: Gene identifiers"),
                                        tags$li("Required only for gene expression-based stratification")
                                    ),
                                    tags$pre("Gene,   PT001, PT002, PT003\nTP53,   8.2,   7.9,   9.1\nBRCA1,  6.5,   7.1,   5.8")
                                )
                            )
                        ),
                        hr()
                    ),
                    column(2)
                )
            ),


        ## Data Input Page
##########################################################################################################################################################
            tabPanel('Data Input',
                fluidRow(
                    column(3,
                        wellPanel(
                            h2('Input Data', align='center'),
                            hr(),
                            materialSwitch("DemoData", label="Upload custom data", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.DemoData",
                                h4("Clinical Data (required)", style="color:#0F344C;"),
                                fileInput('clinical_file', 'Upload Clinical Data (CSV/TSV)',
                                    accept=c('text/csv', 'text/comma-separated-values, text/plain', '.csv',
                                             'text/tsv', 'text/tab-separated-values, text/plain', '.tsv'),
                                    multiple=FALSE
                                ),
                                h4("Expression Matrix (optional)", style="color:#0F344C;"),
                                fileInput('expr_file', 'Upload Expression Matrix (CSV/TSV)',
                                    accept=c('text/csv', 'text/comma-separated-values, text/plain', '.csv',
                                             'text/tsv', 'text/tab-separated-values, text/plain', '.tsv'),
                                    multiple=FALSE
                                ),
                                hr(),
                                uiOutput('column_selectors'),
                                actionButton('submit', "Submit Data", class='btn btn-info btn-block')
                            ),
                            conditionalPanel("input.DemoData==false",
                                p("Use dynamically generated demo data to explore survival analysis features."),
                                p(em("Demo: ~200 patients with time-to-event, censoring, clinical grouping variables, and a matching 50-gene expression matrix.")),
                                hr(),
                                actionButton('demo_submit', "Load Demo Data", class='btn btn-success btn-block')
                            )
                        )
                    ),
                    column(9,
                        tabsetPanel(id='InputTables',
                            tabPanel(title='Clinical Data', hr(),
                                withSpinner(type=6, color='#5bc0de',
                                    dataTableOutput('clinical_table')
                                )
                            ),
                            tabPanel(title='Expression Matrix', hr(),
                                withSpinner(type=6, color='#5bc0de',
                                    dataTableOutput('expr_table')
                                )
                            )
                        )
                    )
                )
            ),


        ## Kaplan-Meier Analysis Page (hidden until data submitted)
##########################################################################################################################################################
            tabPanel('Kaplan-Meier Analysis',
                fluidRow(
                    column(3,
                        wellPanel(

                            ## Stratification
                            h5(strong("Stratification"), style="color:#0F344C; margin-top:4px;"),
                            hr(),
                            radioButtons("strat_mode", label=NULL,
                                choices=c(
                                    "By Clinical Variable"="clinical",
                                    "By Gene Expression"="gene",
                                    "By Continuous Clinical Variable"="continuous"
                                ),
                                selected="clinical"
                            ),
                            conditionalPanel("input.strat_mode == 'clinical'",
                                selectInput("strat_clinical_var", "Select grouping variable:", choices=NULL)
                            ),
                            conditionalPanel("input.strat_mode == 'gene'",
                                conditionalPanel("output.has_expression",
                                    selectizeInput("strat_gene", "Select gene:", choices=NULL,
                                        options=list(placeholder='Type gene name...', maxOptions=100)),
                                    selectInput("gene_cutoff_method", "Cutoff method:",
                                        choices=c("Median split"="median", "Tertiles"="tertile",
                                                  "Quartiles"="quartile", "Optimal (surv_cutpoint)"="optimal",
                                                  "Custom threshold"="custom")),
                                    conditionalPanel("input.gene_cutoff_method == 'custom'",
                                        numericInput("gene_cutoff_value", "Custom cutoff value:", value=0, step=0.1)
                                    )
                                ),
                                conditionalPanel("!output.has_expression",
                                    p(em("No expression matrix loaded. Upload an expression matrix on the Data Input tab to enable gene-based stratification."),
                                      style="color:#999;")
                                )
                            ),
                            conditionalPanel("input.strat_mode == 'continuous'",
                                selectInput("strat_cont_var", "Select numeric variable:", choices=NULL),
                                selectInput("cont_cutoff_method", "Cutoff method:",
                                    choices=c("Median split"="median", "Tertiles"="tertile",
                                              "Quartiles"="quartile", "Optimal (surv_cutpoint)"="optimal",
                                              "Custom threshold"="custom")),
                                conditionalPanel("input.cont_cutoff_method == 'custom'",
                                    numericInput("cont_cutoff_value", "Custom cutoff value:", value=0, step=0.1)
                                )
                            ),
                            hr(),

                            ## Statistics
                            materialSwitch("show_stats", label="Statistics Options", value=TRUE, right=TRUE, status='info'),
                            conditionalPanel("input.show_stats",
                                hr(),
                                materialSwitch("show_pval", label="Show p-value (log-rank)", value=TRUE, right=TRUE, status='info'),
                                materialSwitch("show_hr", label="Show hazard ratio (95% CI)", value=TRUE, right=TRUE, status='info'),
                                materialSwitch("show_risk_table", label="Number at risk table", value=TRUE, right=TRUE, status='info'),
                                materialSwitch("show_ci", label="Confidence intervals", value=FALSE, right=TRUE, status='info'),
                                materialSwitch("show_censor", label="Censoring tick marks", value=TRUE, right=TRUE, status='info')
                            ),

                            ## Color
                            materialSwitch("show_color_opts", label="Color Options", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.show_color_opts",
                                hr(),
                                selectInput("color_palette", "Color Palette:",
                                    choices=c("Default"="default", "NEJM"="nejm", "JCO"="jco",
                                              "Lancet"="lancet", "JAMA"="jama", "Custom"="custom")),
                                conditionalPanel("input.color_palette == 'custom'",
                                    uiOutput('custom_color_pickers')
                                )
                            ),

                            ## Fonts
                            materialSwitch("show_fonts", label="Font Options", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.show_fonts",
                                hr(),
                                sliderInput("font_title", "Title font size:", min=8, max=30, step=1, value=16),
                                sliderInput("font_axis", "Axis label font size:", min=8, max=24, step=1, value=12),
                                sliderInput("font_legend", "Legend font size:", min=6, max=20, step=1, value=10),
                                sliderInput("font_risk", "Risk table font size:", min=6, max=16, step=1, value=10)
                            ),

                            ## Axes
                            materialSwitch("show_axes", label="Axis Options", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.show_axes",
                                hr(),
                                numericInput("xmax", "X-axis maximum (time):", value=NA, min=0, step=10),
                                numericInput("xbreak", "X-axis break interval:", value=NA, min=1, step=10),
                                materialSwitch("log_yaxis", label="Log-transform Y axis", value=FALSE, right=TRUE, status='info'),
                                materialSwitch("yaxis_zero", label="Y-axis start at 0", value=TRUE, right=TRUE, status='info')
                            ),

                            ## Legend
                            materialSwitch("show_legend_opts", label="Legend Options", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.show_legend_opts",
                                hr(),
                                radioButtons("legend_pos", "Position:", inline=TRUE,
                                    choices=c("Top"="top", "Bottom"="bottom", "Right"="right", "None"="none"),
                                    selected="top"),
                                materialSwitch("show_legend_title", label="Show legend title", value=TRUE, right=TRUE, status='info'),
                                conditionalPanel("input.show_legend_title",
                                    textInput("legend_title_text", "Legend title:", value="Strata")
                                )
                            ),

                            ## Resize
                            materialSwitch("show_resize", label="Resize Plot", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.show_resize",
                                hr(),
                                sliderInput("km_height", "Plot height (px):", min=300, max=1500, step=50, value=600),
                                sliderInput("km_width", "Plot width (px):", min=300, max=1500, step=50, value=800)
                            )

                        )# end wellPanel sidebar
                    ),
                    column(9,
                        tabsetPanel(id='KMTabs',
                            tabPanel(title='Kaplan-Meier Plot', hr(),
                                fluidRow(style="margin: 0 8px 4px 0;",
                                    column(12, align="right",
                                        actionButton("show_code_modal", label=NULL,
                                            icon=icon("file-code"),
                                            title="View R code to reproduce this plot",
                                            class="btn btn-default btn-sm",
                                            style="border-radius:6px; font-size:16px; padding:4px 8px;"
                                        )
                                    )
                                ),
                                hr(),
                                div(style="overflow-x:auto; width:100%;",
                                    withSpinner(type=6, color='#5bc0de',
                                        plotOutput("km_plot_out", height='100%')
                                    )
                                ),
                                div(style="margin-top:30px; text-align:center; padding-bottom:50px;",
                                    div(style="display:inline-block; width:250px; margin-bottom:10px;",
                                        selectInput("km_download_format", "Download format:",
                                            choices=c('pdf','png','tiff','jpeg','svg','eps'))
                                    ),
                                    br(),
                                    downloadButton('download_km', 'Download KM Plot')
                                )
                            ),
                            tabPanel(title='Summary Statistics', hr(),
                                withSpinner(type=6, color='#5bc0de',
                                    DT::dataTableOutput('km_summary_table')
                                ),
                                hr(),
                                uiOutput('logrank_hr_display'),
                                hr(),
                                div(style="text-align:center; padding-bottom:30px;",
                                    downloadButton('download_stats', 'Download Summary (CSV)')
                                )
                            )
                        )
                    )
                )
            )


        ),# Ends navbarPage

        ## Footer (outside navbarPage to avoid bslib nav container error)
        tags$footer(
            wellPanel(
                fluidRow(
                    column(4, align='center',
                    tags$a(href="https://github.com/MaGIC-Analytics/magic-survival", icon("github", "fa-3x")),
                    tags$h4('GitHub to submit issues/requests')
                    ),
                    column(4, align='center',
                    tags$a(href="http://www.bioinformagic.io/", icon("magic", "fa-3x")),
                    tags$h4('MaGIC Home Page')
                    ),
                    column(4, align='center',
                    tags$a(href="https://github.com/MaGIC-Analytics", icon("address-card", "fa-3x")),
                    tags$h4("Developer's Page")
                    )
                ),
                fluidRow(
                    column(12, align='center',
                        HTML('<a href="https://www.youtube.com/watch?v=dQw4w9WgXcQ">
                        <p>&copy;
                            <script language="javascript" type="text/javascript">
                            var today = new Date()
                            var year = today.getFullYear()
                            document.write(year)
                            </script>
                        </p>
                        </a>
                        ')
                    )
                )
            )
        )
    )# Ends fluidPage
)# Ends tagList
