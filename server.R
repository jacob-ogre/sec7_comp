# BSD_2_clause

shinyServer(function(input, output, session) {
  
  dat <- reactive({
    q <- parseQueryString(session$clientData$url_search)
    if(length(q) == 0) q[[1]] <- "All"
    subd <- full
    if(q[[1]] != "All") {
      idx <- filter(spp_lookup, spp == q[["species"]])$idx
      subd <- subd[idx, ]
    }
    return(subd)
  })
  
  make_consult_year_summary_df <- function(x) {
    cons_yr <- table(x$FY)
    form_yr <- table(x[x$formal_consult=="Yes",]$FY)
    years <- names(cons_yr)
    cur_dat <- data_frame(years, as.vector(cons_yr), as.vector(form_yr))
    names(cur_dat) <- c("years", "all", "formal")
    return(cur_dat)
  }
  
  output$consult_by_time <- renderGvis({
    cur_dat <- make_consult_year_summary_df(dat())
    chart <- gvisColumnChart(
               cur_dat, 
               xvar="years", 
               yvar=c("all", "formal"),
               options = list(
                 legend="{ position: 'top', maxLines: 2 }",
                 height="500px",
                 colors="['#0A4783', '#f49831']",
                 bar="{groupWidth:'90%'}",
                 vAxis="{title: '# Consultations'}",
                 chartArea="{left: 80, top: 50, width:'90%', height:'75%'}")
    )
    chart
  })
  
  # output$consult_by_work <- renderGvis({
  #   make_work_cat_plot(cur_s7)
  # })
  # 
  # output$consult_by_agency <- renderGvis({
  #   make_agency_figure(cur_s7)
  # })

})
