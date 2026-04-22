# ─── Tab Visibility Management ─────────────────────────────────────────────────
# The "Kaplan-Meier Analysis" tab is hidden by default and only shown after data is submitted.

# Hide on initial load
observe({
    hideTab(inputId="NAVTABS", target="Kaplan-Meier Analysis")
})

# Show after custom data submit
observeEvent(input$submit, {
    clinical <- ClinicalReactive()
    if (!is.null(clinical) && nrow(clinical) > 0) {
        showTab(inputId="NAVTABS", target="Kaplan-Meier Analysis")
        updateTabsetPanel(session, inputId="NAVTABS", selected="Kaplan-Meier Analysis")
        shinyjs::delay(300, shinyjs::runjs("$(window).trigger('resize');"))
    }
})

# Show after demo data submit
observeEvent(input$demo_submit, {
    showTab(inputId="NAVTABS", target="Kaplan-Meier Analysis")
    updateTabsetPanel(session, inputId="NAVTABS", selected="Kaplan-Meier Analysis")
    shinyjs::delay(300, shinyjs::runjs("$(window).trigger('resize');"))
})
