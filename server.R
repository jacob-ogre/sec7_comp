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
    cur_dat <- data_frame(years = years, 
                          all = as.vector(cons_yr), 
                          formal = as.vector(form_yr))
    return(cur_dat)
  }
  
  output$consult_by_time <- renderGvis({
    cur_dat <- make_consult_year_summary_df(dat())
    output$mod1_text <- renderText({ make_outtext_1(cur_dat) })
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
    return(chart)
  })
  
  make_outtext_1 <- function(df) {
    df$years <- as.numeric(df$years)
    amod <- lm(all ~ years, data = df)
    s_amod <- summary(amod)
    all_trnd <- ifelse(s_amod$coefficients[2, 1] < 0,
                       "declined",
                       "increased")
    all_tr_sig <- ifelse(s_amod$coefficients[2, 4] < 0.05,
                         sprintf("significantly (p = %s).", 
                                 round(s_amod$coefficients[2, 4], 3)),
                         sprintf("but not significantly (%s)",
                                 round(s_amod$coefficients[2, 4], 3)))
    all_tr_noise <- ifelse(s_amod$r.squared > 0.5,
                           "generally steady",
                           "highly variable")
    all_tr_r2 <- round(s_amod$r.squared, 3)
    txt1 <- sprintf("The number of consultations %s through time, %s", 
                    all_trnd,
                    all_tr_sig)
    txt2 <- sprintf("That change was %s through these years (R-sq. = %s).", 
                    all_tr_noise,
                    all_tr_r2)
    txt <- paste(txt1, txt2)
    return(txt)
  }
  
  make_state_work_cat_df <- function(x) {
    categories <- table(x$work_category)
    sorted <- -sort(-categories)
    if (length(sorted) <= 25) {
      dat <- data_frame(work_cat=names(sorted), 
                        consultations=as.vector(sorted))
    } else {
      dat <- data_frame(work_cat=names(sorted)[1:25], 
                        consultations=as.vector(sorted[1:25]))
    }
    return(dat)
  }

  output$consult_by_work <- renderGvis({
    cur_dat2 <- make_state_work_cat_df(dat())
    output$mod2_text <- renderText({ make_outtext_2(cur_dat2) })
    left <- nchar(as.character(cur_dat2$work_cat[1])) * 5
    if (left < 80) {
      left <- 80
    }
    chartArea <- paste0("{left: ", left, ", top: 50, width: '90%', height: '",
                       "500px", "'}")
    chart3 <- gvisColumnChart(
                cur_dat2,
                xvar="work_cat",
                yvar="consultations",
                options = list(height="500px",
                               colors="['#0A4783']",
                               legend="{position: 'none'}",
                               vAxis="{title: '# Consultations'}",
                               chartArea=chartArea)
    )
    chart3
  })

  make_outtext_2 <- function(df) {
    cv <- round(sd(df$consultations) / mean(df$consultations), 2)
    fold_df <- round(df$consultations[1] / df$consultations[2], 2)
    top_wk <- df$work_cat[1]
    sec_wk <- df$work_cat[2]
    if(cv > 2 | fold_df > 2) {
      var_lev <- "high"
    } else {
      var_lev <- "low"
    }
    txt <- sprintf("There has been relatively %s variation in consultations
                   rates among work categories (CV = %s). For example, the work 
                   category most consulted on (%s) accounts for %s times as many
                   consultations the second-highest (%s).", 
                   var_lev, cv, top_wk, fold_df, sec_wk)
    return(txt)
  }
  
  make_top_25_agencies_df <- function(sub) {
    sub_agency <- table(sub$lead_agency)
    sorted <- -sort(-sub_agency)
    sorted <- sorted[sorted > 0]
    if (length(sorted) <= 25) {
      dat <- data_frame(agency=names(sorted), 
                        consultations=as.vector(sorted))
    } else {
      dat <- data_frame(agency=names(sorted)[1:25], 
                        consultations=as.vector(sorted[1:25]))
    }
    return(dat)
  }
  
  output$consult_by_agency <- renderGvis({
    cur_dat3 <- make_top_25_agencies_df(dat())
    output$mod3_text <- renderText({ make_outtext_3(cur_dat3) })
    left <- nchar(as.character(cur_dat3$agency[1])) * 6
    if (left < 80) {
      left <- 80
    }
    if (left > 200) {
      left <- 200
    }
    chartArea <- paste0("{left: ", left, 
                        ", top: 50, width: '85%', height: '500px'}")
    chart4 <- gvisColumnChart(
                cur_dat3,
                xvar="agency",
                yvar="consultations",
                options = list(height="500px",
                               colors="['#0A4783']",
                               legend="{position: 'none'}",
                               vAxis="{title: '# Consultations'}",
                               chartArea=chartArea)
    )
    chart4
  })

  make_outtext_3 <- function(df) {
    cv <- round(sd(df$consultations) / mean(df$consultations), 2)
    fold_df <- round(df$consultations[1] / df$consultations[2], 2)
    top_ag <- df$agency[1]
    sec_ag <- df$agency[2]
    if(cv > 2 | fold_df > 2) {
      var_lev <- "high"
    } else {
      var_lev <- "low"
    }
    txt <- sprintf("There has been relatively %s variation in consultations
                   rates among consulting agencies (CV = %s). For example, the
                   agency with the highest rate of consultation (%s) did so
                   at %s times than the rate of the second-highest (%s).", 
                   var_lev, cv, top_ag, fold_df, sec_ag)
    return(txt)
  }
  
})
