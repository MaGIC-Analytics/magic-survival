function(input, output, session) {
    options(shiny.maxRequestSize=50*1024^2)
    options(shiny.sanitize.errors=FALSE)
    source('ui.R',            local=TRUE)
    source('input.R',         local=TRUE)
    source('tabmanagement.R', local=TRUE)
    source('survival.R',      local=TRUE)
}
