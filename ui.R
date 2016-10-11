# BSD_2_clause

shinyUI(fluidPage(
  useShinyjs(),
  column(12,
    h3("FWS section 7 consultations through time",
       style = "text-align:center"),
    htmlOutput("consult_by_time"),
    h3("FWS section 7 consultations by work type",
       style = "text-align:center"),
    htmlOutput("consult_by_work"),
    h3("FWS section 7 consultations by agency",
       style = "text-align:center"),
    htmlOutput("consult_by_agency"),
    br(),br(),
    tags$div(
      style = "font-size: large; text-align: center;",
      tags$a(href = "https://defend-esc-dev.org/shiny/open/section7_explorer/",
             target = "_blank",
             "Visit the Section 7 Explorer for more detail")
    )
  )
))
