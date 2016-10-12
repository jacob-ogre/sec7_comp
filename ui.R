# BSD_2_clause

shinyUI(fluidPage(
  useShinyjs(),
  column(12,
    fluidRow(
      column(9,
        h3("FWS section 7 consultations through time",
           style = "text-align:center"),
        htmlOutput("consult_by_time")
      ),
      column(3,
        br(), br(), br(),
        tags$blockquote(textOutput("mod1_text"))
      )
    ),
    fluidRow(
      column(9,
        h3("FWS section 7 consultations by work type",
           style = "text-align:center"),
        htmlOutput("consult_by_work")
      ),
      column(3,
        br(), br(), br(),
        tags$blockquote(textOutput("mod2_text"))
      )
    ),
    fluidRow(
      column(9,
        h3("FWS section 7 consultations by agency",
           style = "text-align:center"),
        htmlOutput("consult_by_agency")
      ),
      column(3,
        br(), br(), br(),
        tags$blockquote(textOutput("mod3_text"))
      )
    ),
    fluidRow(
      br(),br(),
      tags$div(
        style = "font-size: large; text-align: center;",
        tags$a(href = "https://defend-esc-dev.org/shiny/open/section7_explorer/",
               target = "_blank",
               "Visit the Section 7 Explorer for more detail")
      )
    )
  )
))
