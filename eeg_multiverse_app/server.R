# Developed in R version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"
# Daniel Kristanto (University of Oldenburg)
# adapted by Nadine Nadine Jacobsen (University of Oldenburg) and Cosku Inceler (University of Oldenburg)
# January 2024, last revision: 23-January-2024
library(shiny)        # v1.7.5
library(networkD3)    # v0.4
library(dplyr)        # v1.1.3
library(igraph)       # v1.5.1
library(visNetwork)   # v2.1.2
library(geomnet)      # v0.3.1, not on CRAN anymore, download from https://cran.r-project.org/src/contrib/Archive/geomnet/?C=M;O=A
library(stringr)      # v1.5.0
library(png)          # v0.1-8
library(shinyjs)      # v2.1.0
library(DT)           # v0.29
library(rintrojs)     # v0.3.2
library(ggplot2)      # v3.4.3
library(qdapTools)    # v2.4.6
library(RColorBrewer) # v1.1-3
library(shinyWidgets) # v0.8.0
library(tibble)       # v3.2.1
library(htmlwidgets)  # v1.6.2
library(ggtext)       # v0.1.2
library(sna)          # v2.7-1
library(leaflet)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)

library(patchwork)
library(shinyBS)


callback <- c(
  "table.on('row-reorder', function(e, details, edit){",
  "  var oldRows = [], newRows = [];",
  "  for(let i=0; i < details.length; ++i){",
  "    oldRows.push(details[i].oldData);",
  "    newRows.push(details[i].newData);",
  "  }",
  "  Shiny.setInputValue('rowreorder', {old: oldRows, new: newRows});",
  "});"
)

server <- function(input, output, session){
  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  # DT Options --------------------------------------------------------------
  options(DT.options = list( lengthMenu = c(10, 20),
                             dom = 'tl'
  ))  # table and lengthMenu options
  
  ### Introduction page
  observeEvent(input$btn_MA, {
    shinyjs::runjs('fakeClick("MA")')
  })
  observeEvent(input$btn_MD, {
    shinyjs::runjs('fakeClick("MD")')
  })
  observeEvent(input$btn_WH, {
    shinyjs::runjs('fakeClick("WH")')
  })
  observeEvent(input$btn_fa, {
    shinyjs::runjs('fakeClick("fa")')
  })
  observeEvent(input$btn_IP, {
    shinyjs::runjs('fakeClick("IP")')
  })
  observeEvent(input$btn_EP, {
    shinyjs::runjs('fakeClick("EP")')
  })
  observeEvent(input$btn_DIY, {
    shinyjs::runjs('fakeClick("DIY")')
  })
  
  #exemplary pipeline
  observeEvent(input$btn_IPexp, {
    shinyjs::runjs('fakeClick("IP")')
  })

  # your pipeline -------------------------------------------------------
  output$selectStep_DIY <- renderUI({
    # gr_sel_DIY <- input$selectGroup_DIY
    # opts <- which(nodes_op$Groups_vis==gr_sel_DIY)
    #opts_ <- nodes_op[opts, ]
    selectInput("selectStep_DIY",
                label   = "Select the step",
                choices =  c(nodes$Names_vis[nodes$Groups==input$selectGroup_DIY]),
                selected = c(nodes$Names_vis[nodes$Groups==input$selectGroup_DIY])[1],
                selectize = FALSE,
                size = paste(length(c(nodes$Names_vis[nodes$Groups==input$selectGroup_DIY])))
    )
  })
  
  output$selectDecision_DIY <- renderUI({
    st_sel_DIY <- input$selectStep_DIY
    opts <- which(nodes_op$Groups_vis==st_sel_DIY)
    opts_ <- nodes_op[opts, ]
    selectInput("selectDecision_DIY2",
                label   = "Select the option",
                choices =  c("Any", opts_$Names),
                selected = "Any",
    )
  })
  
  output$selectedStep_DIY <- renderText({
    dec <- input$selectStep_DIY
    opts <- which(ListSteps$Step==dec)
    if (!is.na(ListSteps$Info[opts])){
      url <- a(ListSteps$Info[opts], href = ListSteps$Link[opts])}
    else{
      url <- NA}
    HTML(paste("<i>Definition:</i> ", ListSteps$Definiton[opts],
               "<br><i>Further information:</i>", url, "<br><br>" ))
  })
  
  
  tableValues <- reactiveValues(df = data.frame(Names = as.character(), Options = as.character(), 
                                                Comment = as.character(), check.names = FALSE))
  
  
  observeEvent(input$add, {
    if (is.null(input$selectStep_DIY)){
      showNotification("Please select a step",
                       type = "error")
      return()
    }
    
    selected_row <- input$table_DIY_rows_selected
    temp <- tableValues$m
    if (length(selected_row) == 0) {
      # If no row is selected, append the new row to the end
      newRow <- data.frame(Names = input$selectStep_DIY, Options = input$selectDecision_DIY2, 
                           Comment = input$selectComment_DIY, check.names = FALSE)
      temp <- rbind(temp, newRow)
    } else {
      # If a row is selected, insert the new row after the selected row
      newRow <- data.frame(Names = input$selectStep_DIY, Options = input$selectDecision_DIY2, 
                           Comment = input$selectComment_DIY, check.names = FALSE)
      temp <- rbind(temp[1:selected_row, ], newRow, temp[(selected_row + 1):nrow(temp), ])
    }
    rownames(temp) <- NULL
    tableValues$m <- temp
    updateTextInput(session, "selectComment_DIY", value = "")
  })
  
  observeEvent(input$delete,{
    selected_row <- input$table_DIY_rows_selected
    temp <- tableValues$m
    
    if (length(selected_row) == 0) {
      temp <- head(temp,-1)
      
    } else {
      temp  <- temp [-as.numeric(input$table_DIY_rows_selected),]
    }
    rownames(temp) <- NULL
    tableValues$m <- temp
  })
  
  
  observeEvent(input$count, {
    
    table_DIY <- tableValues$m
    step_DIY1 <- c(table_DIY$Names)
    step_DIY <- nodes$Names[match(step_DIY1, nodes$Names_vis, nomatch = 0)] 
    #step_DIY <- nodes$Names[sapply(nodes$Names_vis, function(x) x %in% step_DIY1)]
    option_DIY <- c(table_DIY$Options)
    option_DIY <- option_DIY[option_DIY!=""]
    
    ###Find only step first
    if (input$order == "Same order"){
      row_stepDIY <- which(apply(dat, 1, function(x1) {
        if (length(x1) < length(step_DIY)) {
          return(FALSE)
        }
        idx <- match(step_DIY, x1)
        all(!is.na(idx)) && all(diff(idx) == 1)
      }))
    }
    else if (input$order == "Consecutive"){
      row_stepDIY <- which(apply(dat, 1, function(x3) {
        idx <- match(step_DIY, x3)
        all(!is.na(idx)) && all(diff(idx) > 0)
      }))
    }
    else if (input$order == "Any order"){
      row_stepDIY <- which(apply(dat, 1, function(x2) all(step_DIY %in% x2)))
    }
    
    
    paper_opt <- dat_op[row_stepDIY, ]
    
    ###Find also with option
    id_D_all <- list()
    for (na in 1:length(option_DIY)){
      opt_D <- option_DIY[na]
      if (opt_D == "Any"){
        id_D_all[[na]] <- 1:nrow(dat_op_or)
      }
      
      else {
        st_D1 <- table_DIY$Names[table_DIY$Options == opt_D]
        st_D <- nodes$Names[nodes$Names_vis %in% st_D1]
        id_D <- which(dat_op_or[,st_D] == opt_D)
        id_D_all[[na]] <- id_D
      }
    }
    
    vals <- unlist(id_D_all)
    row_optDIY <- which(tabulate(vals) >= length(id_D_all))
    row_finDIY <- intersect(row_stepDIY, row_optDIY)
    count <- length(row_finDIY)
    
    output$counted_paper <- renderText({
      paste(c("Your selected pipeline is used by", count, "articles (out of 27 articles):<br>"), collapse = " ")
    })
    
    
    
    output$table_DIY2 <- DT::renderDataTable({
      table_DIYfin <- p_inf[row_finDIY, ]
      datatable(table_DIYfin, escape = FALSE,
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip', #does not remder ("argument empty" so commented out)
                  pageLength = 20,
                  scrollX = TRUE,
                  autoWidth = TRUE,
                  paging = TRUE,
                  searching = T,
                  ordering = T,
                  fixedHeader = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf')
                )
      )
    })
    
    # output$table_DIY2 <- renderTable({
    #   table_DIYfin <- p_inf[row_finDIY, ]
    #   table_DIYfin
    # })
  })
  
  observeEvent(input$count, {
    
    table_DIY <- tableValues$m
    step_DIY1 <- c(table_DIY$Names)
    step_DIY <- step_DIY1
    #step_DIY <- nodes_crowd$Names[match(step_DIY1, nodes_crowd$Names_vis, nomatch = 0)]
    #step_DIY <- nodes$Names[sapply(nodes$Names_vis, function(x) x %in% step_DIY1)]
    option_DIY <- c(table_DIY$Options)
    option_DIY <- option_DIY[option_DIY!=""]
    
    ###Find only step first
    if (input$order == "Same order"){
      row_stepDIY <- which(apply(dat_crowd, 1, function(x1) {
        if (length(x1) < length(step_DIY)) {
          return(FALSE)
        }
        idx <- match(step_DIY, x1)
        all(!is.na(idx)) && all(diff(idx) == 1)
      }))
    }
    else if (input$order == "Consecutive"){
      row_stepDIY <- which(apply(dat_crowd, 1, function(x3) {
        if (length(x3) < length(step_DIY)) {
          return(FALSE)
        }
        idx <- match(step_DIY, x3)
        all(!is.na(idx)) && all(diff(idx) > 0)
      }))
    }
    else if (input$order == "Any order"){
      row_stepDIY <- which(apply(dat_crowd, 1, function(x2) all(step_DIY %in% x2)))
    }
    
    paper_opt <- dat_op_crowd[row_stepDIY, ]
    
    ##Find also with option
    table_DIY$Names <- gsub(" ", "_",table_DIY$Names)
    table_DIY$Names <- gsub("-", "_", table_DIY$Names)
    table_DIY$Names <- gsub("/", "_", table_DIY$Names)
    
    id_D_all <- list()
    for (na in 1:length(option_DIY)){
      opt_D <- option_DIY[na]
      if (opt_D == "Any"){
        id_D_all[[na]] <- 1:nrow(dat_op_or_crowd)
      }
      
      else {
        st_D1 <- table_DIY$Names[table_DIY$Options == opt_D]
        st_D <- st_D1
        #st_D <- nodes_crowd$Names[nodes_crowd$Names_vis %in% st_D1]
        id_D <- which(dat_op_or_crowd[,st_D] == opt_D)
        id_D_all[[na]] <- id_D
      }
    }
    
    vals <- unlist(id_D_all)
    row_optDIY <- which(tabulate(vals) >= length(id_D_all))
    row_finDIY <- intersect(row_stepDIY, row_optDIY)
    count <- length(row_finDIY)
    
    output$counted_paper_crowd <- renderText({
      paste(c("<b>Similar pipelines</b></br>Your selected pipeline is used by", count, "experts:", seenIDs_df[row_finDIY, ]), collapse = " ")
    })
    
  }) #count experts using same pipeline
  
  
  # output$table_DIY <- DT::renderDataTable({
  #   table_data <- tableValues$m
  #   datatable(table_data,#escape = FALSE,
  #             extensions = 'RowReorder', callback = JS(callback), 
  #             options = list(
  #               # dom = 'Bfrtip', #does not remder ("argument empty" so commented out)
  #               # searching = F,
  #               # 
  #                 ordering = F,
  #               # fixedHeader = TRUE,
  #               rowReorder = T,
  #               targets = 0,
  #               visible = FALSE,
  #             selection = "none"
  #             )
  #    )
  # })
  
  
  output[["table_DIY"]] <- renderDT({
    dat <- tableValues$m 
    datatable(dat, extensions = "RowReorder", callback = JS(callback), 
              selection = "none", 
              options = list(
                dom = 'Bfrtip', #does not remder ("argument empty" so commented out)
                searching = F,
                ordering = F,
                rowReorder = TRUE
              ))
  })
  
  proxy <- dataTableProxy("table_DIY")
  
  observeEvent(input[["rowreorder"]], {
    dat <- tableValues$m 
    old <- unlist(input[["rowreorder"]]$old)
    new <- unlist(input[["rowreorder"]]$new)
    dat[new, ] <- dat[old, ]
    tableValues$m <- dat
    replaceData(proxy, dat, resetPaging = FALSE)
  })
  
  output$download <- downloadHandler(
    filename = function(){"my_pipeline.csv"},
    content = function(fname){
      table_data <- tableValues$m
      write.csv(table_data, fname)
    })
  
  # output$download_lit <- downloadHandler( 'table need to ba availabe glovbally to do so
  #   filename = function(){"articles.csv"},
  #   content = function(fname){
  #     #table_DIYfin <- p_inf[row_finDIY, ]
  #     #table_data <- tableValues$m
  #     write.csv(table_DIYfin, fname)
  #   })
  
  ### i.Graph of individual paper #####
  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  # DT Options --------------------------------------------------------------
  options(DT.options = list( lengthMenu = c(10, 20),
                             dom = 'tl'
  ))  # table and lengthMenu options
  
  observe({
    if (!is.null(input$selectA)) {
      # Update choices for selectB based on the value of selectA
      updateSelectInput(session, "selectB", choices = c("All Options", input$selectA))
    } else {
      # If selectA is not specified, provide all options for selectB
      updateSelectInput(session, "selectB", choices = c("Option 1", "Option 2", "Option 3"))
    }
  })
  
  output$selected_paper <- renderText({ 
    sel_pap_st <- input$selectPapers
    pap_st <- p_inf[p_inf$Key == sel_pap_st, ]
    paste("You have selected this article:", "Author:", pap_st$Author,
          "Title:", pap_st$Title, "Year:", pap_st$Year,
          "Publisher:", pap_st$Publisher)
  })
  
  output$selected_paper_cv <- renderText({ 
    sel_pap_cv <- input$selectPapers_cv
    pap_cv <- p_inf[p_inf$Key == sel_pap_cv, ]
    paste("You have selected this article:", "Author:", pap_cv$Author,
          "Title:", pap_cv$Title, "Year:", pap_cv$Year,
          "Publisher:", pap_cv$Publisher)
  })
  
  output$list_paper <- DT::renderDataTable(
    p_inf,  escape=FALSE,
    #extensions = "FixedHeader",
    style="bootstrap",
    options = list(
      dom = 'Bfrtip',
      pageLength = 20,
      scrollX=TRUE,
      autoWidth = TRUE,
      paging=TRUE,
      searching=FALSE,
      ordering=TRUE
      #fixedHeader = TRUE,
    )
  )
  
  output$list_steps <- DT::renderDataTable(
    ListSteps,
    #extensions = "FixedHeader",
    style="bootstrap",
    options = list(
      dom = 'Bfrtip',
      pageLength = 20,
      scrollX=TRUE,
      autoWidth = TRUE,
      paging=TRUE,
      searching=FALSE,
      ordering=TRUE
      #fixedHeader = TRUE,
    )
  )
  
  output$list_decisions <- DT::renderDataTable(
    ListOptions,
    #extensions = "FixedHeader",
    style="bootstrap",
    options = list(
      dom = 'Bfrtip',
      pageLength = 20,
      scrollX=TRUE,
      autoWidth = TRUE,
      paging=TRUE,
      searching=FALSE,
      ordering=TRUE
      #fixedHeader = TRUE,
    )
  )
  
  # Plot pipeline individual article ---------------------------------------- 
  output$plot <- renderPlot(
    width = 1000, height = 700, res = 100,
    {
      input$newplot
      sel_p <- input$selectPapers
      id_p <- which(dat$Key == sel_p)
      links_ind <- list_df[[id_p]]
      g <- graph_from_data_frame(d=links_ind, vertices=nodes, directed=T)
      first_nd <- links_ind$source[1]
      id_fn <- which(nodes$Names == first_nd)
      V(g)$color <- nodes$col
      V(g)$label <- nodes2$Names_vis
      V(g)$color[id_fn] <- "grey"
      cdi <- readRDS("coordinates_steps_example.RDS")
      cdi <- cdi[1:nrow(cdi)-1, ]
      plot(g, 
           layout = cdi,
           vertex.frame.color = "white",                 # Node border color
           vertex.shape="circle",                        # One of ???none???, ???circle???, ???square???, ???csquare???, ???rectangle??? ???crectangle???, ???vrectangle???, ???pie???, ???raster???, or ???sphere???
           vertex.size=10,                               # Size of the node (default is 15)
           
           # === vertex label
           vertex.label.color="black",
           vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
           vertex.label.cex=0.7,                         # Font size (multiplication factor, device-dependent)
           vertex.label.dist=0,                          # Distance between the label and the vertex
           vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
           
           # === Edge
           edge.color="darkgrey",                            # Edge color
           edge.width=1,                                 # Edge width, defaults to 1
           edge.arrow.size=0.6,                          # Arrow size, defaults to 1
           edge.arrow.width=3,                           # Arrow width, defaults to 1
           edge.lty="solid",                             # Line type, could be 0 or ???blank???, 1 or ???solid???, 2 or ???dashed???, 3 or ???dotted???, 4 or ???dotdash???, 5 or ???longdash???, 6 or ???twodash???
           edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
           asp = 0.7
      )
      
      legend(x="topleft", c(unique(nodes$Groups)),      # add legend w/ Group names x=-1.35, y=1.18,
             pch=21, col="#777777", pt.bg=clrs, pt.cex=2, cex=1, bty="n", ncol=1)
      
    })
  
  # Plot pipeline individual expert ---------------------------------------- 
  output$plot_crowd <- renderPlot(
    width = 1000, height = 800, res = 100,
    {
      input$newplot
      sel_p <- input$selectExpert
      id_p <- which(dat_crowd$Key == sel_p)
      links_ind <- list_df_crowd[[id_p]]
      g <- graph_from_data_frame(d=links_ind, vertices=nodes_crowd$Names_vis, directed=T)
      first_nd <- links_ind$source[1]
      id_fn <- which(nodes_crowd$Names_vis == first_nd)
      V(g)$color <- nodes_crowd$col
      V(g)$label <- nodes_crowd$Names_vis
      V(g)$color[id_fn] <- "white"
      V(g)$size <- 10
      cdi <- readRDS("coordinates_steps_example.RDS")
      cdi <- cdi[1:nrow(cdi)-1, ]
      plot(g, 
           layout = cdi,
           vertex.frame.color = "white",                 # Node border color
           vertex.shape="circle",                        # One of ???none???, ???circle???, ???square???, ???csquare???, ???rectangle??? ???crectangle???, ???vrectangle???, ???pie???, ???raster???, or ???sphere???
           vertex.size=10,                               # Size of the node (default is 15)
           
           # === vertex label
           vertex.label.color="black",
           vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
           vertex.label.cex=0.7,                         # Font size (multiplication factor, device-dependent)
           vertex.label.dist=0,                          # Distance between the label and the vertex
           vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
           
           # === Edge
           edge.color="darkgrey",                            # Edge color
           edge.width=1,                                 # Edge width, defaults to 1
           edge.arrow.size=0.6,                          # Arrow size, defaults to 1
           edge.arrow.width=3,                           # Arrow width, defaults to 1
           edge.lty="solid",                             # Line type, could be 0 or ???blank???, 1 or ???solid???, 2 or ???dashed???, 3 or ???dotted???, 4 or ???dotdash???, 5 or ???longdash???, 6 or ???twodash???
           edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
           asp = 0.7
      )
      
      legend(x="topleft", unique(nodes$Groups), pch=21,
             col="#777777", pt.bg=clrs, pt.cex=2, cex=1, bty="n", ncol=1)
    })
  
  
  # Table single article ----------------------------------------------------
  output$table_step <- DT::renderDataTable({
    sel_p <- input$selectPapers
    pipes_p <- dat_vis[which(dat_vis$Key == sel_p),2:ncol(dat_vis) ]#select steps
    opts_p <- dat_op[which(dat_vis$Key == sel_p),2:ncol(dat_vis) ]#select options
    comments_p <- dat_comment[which(dat_comment$Key == sel_p),2:ncol(dat_comment) ]#select steps
    colnames(opts_p) <- colnames(pipes_p) #get toget '
    all_p <- rbind(pipes_p, opts_p, comments_p) #append
    all_p <-  transpose(all_p) #transpose
    colnames(all_p) <- c("Step", "Option", "Comment") #rename cols'
    all_p <- all_p[!is.na(all_p$Step),]#delete all NA rows (steps)
    all_p$Option[all_p$Option==0] <- "Not reported"#substitute opition 0 with "not_reported"
    datatable(all_p,
              options = list(
                dom = 'Bfrtip',
                pageLength = 20,
                scrollX = TRUE,
                #autoWidth = TRUE,
                paging = F,
                searching = FALSE,
                ordering = F
                # fixedHeader = TRUE
              ))
  })
  
  # Table single expert ----------------------------------------------------
  output$table_step_crowd <- DT::renderDataTable({
    sel_p <- input$selectExpert
    pipes_p <- dat_vis_crowd[which(dat_vis_crowd$Key == sel_p),2:ncol(dat_vis_crowd) ]#select steps
    #needs dat_op crowd which is not read in correctls!
    # opts_p <- dat_op_crowd[which(dat_op_crowd$Key == sel_p),2:ncol(dat_op_crowd) ]#select options
    # colnames(opts_p) <- colnames(pipes_p) #get toget '
    # all_p <- rbind(pipes_p, opts_p) #append
    # all_p <-  transpose(all_p) #transpose
    # all_p <- all_p[complete.cases(all_p), ] #delete all NA rows (steps)
    # colnames(all_p) <- c("Step", "Option") #rename cols'
    # all_p$Option[all_p$Option==0] <- "Not reported"#substitute opition 0 with "not_reported"
    datatable(pipes_p,
              options = list(
                dom = 'Bfrtip',
                pageLength = 20,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = FALSE,
                ordering = TRUE
                # fixedHeader = TRUE
              )
    )
  })
  
  # Steps: Aggregated: Literature ----------------------------------------------------
  output$WP <- renderForceNetwork({
    thr <- input$Thr
    ndWP <- input$Node_WP
    nodes2$size2 <- nodes2$size/(max(nodes2$size)/100) #prev 28, maybe change back?
    # thr_nd <- input$Thr_nd
    # nodes2$Groups[nodes2$size < thr_nd] <- "Below threshold" #set nodes below threshold to Group "below threshold"
    links2$color <- "gray"
    links2$color[links2$value>5] <- "blue"
    links2$color[links2$value>20] <- "red"
    links2 <- links2[links2$value > thr, ]
    links2$value2 <- links2$value/(max(links2$value)/10)
    
    links3 <- data.frame(source = match(links2$source, nodes2$Names) - 1,
                         target = match(links2$target, nodes2$Names) - 1,
                         value = links2$value,
                         value2 = links2$value2)
    
    
    if (ndWP == "All"){
      links_vis <- links3
    }
    else {
      ndWP <- nodes$Names[nodes$Names_vis==ndWP]
      id_ndWP <- which(nodes2$Names == ndWP)-1
      links_vis <- links3[links3$source == id_ndWP | links3$target == id_ndWP, ]
    }
    # error message if no links can be displayed
    # observeEvent(input$add, {
    if (dim(links_vis)[1] == 0){
      showNotification("Please select a lower threshold or a different step.",
                       type = "error")
      return()
    }
    else {
      script <- 'alert("Name: " + d.Names + "\\n" +
              "Definiton: " + d.Definition + "\\n" + "Used by: " + d.size + " articles out of 27 articles");'
      
      fn = forceNetwork(Links = links_vis, Nodes = nodes2,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "Names_vis", linkWidth = JS("function(d) { return Math.sqrt(d.value)/3; }"),
                        Nodesize = "size2", Group = "Groups", radiusCalculation = JS("Math.sqrt(d.nodesize)+6"), 
                        opacityNoHover = 0.5, opacity = 0.9, charge = -50,
                        zoom = TRUE, fontSize = 8, fontFamily = "sans-serif", arrows = TRUE, legend = TRUE, bounded = TRUE, clickAction = script 
                        # colourScale = JS('d3.scaleOrdinal().domain(["Offline filter", "Downsampling","Re-refrencing",
                        #  "Reject data", "Reject channels", "Interpolate channels","Multi-step automated approach",
                        #  "Data decomposition","Source estimation","Line noise correction","Epoching",""Other"]).
                        #             range([#1E90FF", "#D55E00", "#008B8B", "#9370DB", "#FF4500", "#56B4E9",
                        #                   "#32CD32", "#0072B2", "#A0522D", "#FFD700", "#CC79A7", "#009E73", "#FF69B4"])')
      )
      #To-Do: javascript should be dynamic!!!!
      
      fn$x$nodes$Names <- nodes2$Names_vis
      fn$x$nodes$Definition <- nodes2$Definition
      fn$x$nodes$size <- nodes2$size
      #radiusCalculation: "Math.sqrt(d.nodesize)+6"
      
      
      htmlwidgets::onRender(fn, jsCode = '
    function (el, x) {
          d3.select("svg").append("g").attr("id", "legend-layer");
          var legend_layer = d3.select("#legend-layer");
          d3.selectAll(".legend")
            .each(function() { legend_layer.append(() => this); });
          d3.select(el)
            .selectAll(".link")
            .append("title")
            .text(d => d.value);
          var link = d3.selectAll(".link")
          var node = d3.selectAll(".node")
      
          var options = { opacity: 1,
                          clickTextSize: 10,
                          opacityNoHover: 0.1,
                        }
      
          var unfocusDivisor = 4;
      
          var links = HTMLWidgets.dataframeToD3(x.links);
          var linkedByIndex = {};
      
          links.forEach(function(d) {
            linkedByIndex[d.source + "," + d.target] = 1;
            linkedByIndex[d.target + "," + d.source] = 1;
          });
      
          function neighboring(a, b) {
            return linkedByIndex[a.index + "," + b.index];
          }
      
          function mouseover(d) {
            var unfocusDivisor = 4;
      
            link.transition().duration(200)
              .style("opacity", function(l) { return d != l.source && d != l.target ? +options.opacity / unfocusDivisor : +options.opacity });
      
            node.transition().duration(200)
              .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });
      
            node.select("text").transition()
              .duration(750)
              .attr("x", 13)
              .style("stroke-width", ".5px")
              .style("font", 5 + "px ")
              .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
          }
      
          function mouseout() {
            node.style("opacity", +options.opacity);
            link.style("opacity", +options.opacity);
      
            d3.select(this).select("circle").transition()
              .duration(750)
              .attr("r", function(d){return nodeSize(d);});
            node.select("text").transition()
              .duration(1250)
              .attr("x", 0)
              .style("font", options.fontSize + "px ")
              .style("opacity", 0);
          }
      
          d3.selectAll(".node").on("mouseover", mouseover).on("mouseout", mouseout);
            
      }')
      
    }
  })
  

# Steps: Aggregated -------------------------------------------------------
  output$plot_combined <- renderPlot(
    width = 1300, height = 500, res = 100,
    {
      input$newplot
  df1 <- subset(nodes, select=c(Names, Names_vis, Groups, ID, col)) # get literature data
  df1$sizeLit <- nodes2$size
  df2 <- subset(nodes_crowd, select= c(Names,size)) # gat expert data
  
  merged_df <- merge(df1, df2, by = "Names", all.x = TRUE) #merge
  merged_df[is.na(merged_df)] <-0 # add 0 for unused steps (NA before)
  
  merged_df <- merged_df %>%
    mutate(Names_vis = as.factor(Names_vis),  # Convert Names_vis to a factor
           ID = as.numeric(ID)) %>%  # Convert ID to numeric
    mutate(Names_or = fct_reorder(Names_vis, ID)) %>%  # Reorder the levels of Names_vis
    mutate(Names_or = fct_rev(Names_or))  # Reverse the order of the levels
  # Limit y-axis labels to 5 characters
  # st_dat$Names_or <- stringr::str_sub(st_dat$Names_or, 1, 20)
  par(mar=c(10,4,4,1)+.1)
  
  p1<-ggplot(merged_df, aes(x = Names_or, y = sizeLit, color = Groups)) +
    geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
    geom_point(size = 4, alpha = 0.6) +
    geom_text(aes(label = sizeLit), vjust = 0.5, hjust = -1, size = 4, color = "black") +
    scale_color_manual(values = unique(merged_df$col)) +
    #scale_y_discrete(labels = st_dat$Names_or, breaks = st_dat$Names_or, limits = st_dat$Names_or) +
    theme_light() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      text = element_text(size = 12, family = "Arial"),
      axis.text.y = element_text(),
      legend.position ="none",
    ) +
    coord_flip() +
    labs(
      x = "Step",
      y = "Number of articles",
      color = "Groups"
    )
  p2<-ggplot(merged_df, aes(x = Names_or, y = size, color = Groups)) +
    geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
    geom_point(size = 4, alpha = 0.6) +
    geom_text(aes(label = size), vjust = 0.5, hjust = -1, size = 4, color = "black") +
    scale_color_manual(values = unique(merged_df$col)) +
    #scale_y_discrete(labels = st_dat$Names_or, breaks = st_dat$Names_or, limits = st_dat$Names_or) +
    theme_light() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      text = element_text(size = 12, family = "Arial"),
      axis.text.y = element_text(),
    ) +
    coord_flip() +
    labs(
      x = "Step",
      y = "Number of experts",
      color = "Groups"
    )
  p1 + p2  
    })
  
  # Steps: Network: Experts ----------------------------------------------------
  output$WP_crowd <- renderForceNetwork({
    thr <- input$Thr
    ndWP <- input$Node_WP
    nodes2 <- nodes2_crowd
    nodes2$size2 <- nodes2$size/(max(nodes2$size)/200)
    # thr_nd <- input$Thr_nd
    # nodes2$Groups[nodes2$size < thr_nd] <- "Below threshold"
    links2 <- links2_crowd
    links2$color <- "#080808"
    links2$color[links2$value>1] <- "blue"
    links2$color[links2$value>2] <- "red"
    links2 <- links2[links2$value > thr, ]
    links2$value2 <- links2$value/(max(links2$value)/5)
    
    links3 <- data.frame(source = match(links2$source, nodes2$Names_vis) - 1,
                         target = match(links2$target, nodes2$Names_vis) - 1,
                         value = links2$value,
                         value2 = links2$value2)
    if (ndWP == "All"){
      links_vis <- links3
    }
    else {
      ndWP <- nodes_crowd$Names[nodes_crowd$Names_vis==ndWP]
      id_ndWP <- which(nodes2$Names == ndWP)-1
      links_vis <- links3[links3$source == id_ndWP | links3$target == id_ndWP, ]
    }
    
    script <- 'alert("Name: " + d.Names + "\\n" +
              "Used by: " + d.size + " experts");'
    
    fn = forceNetwork(Links = links_vis, Nodes = nodes2,
                      Source = "source", Target = "target",
                      Value = "value", NodeID = "Names_vis", linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
                      Nodesize = "size2", Group = "Groups", radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
                      opacityNoHover = 0.5, opacity = 0.9, charge = -50,
                      zoom = TRUE, fontSize = 8, fontFamily = "sans-serif",
                      arrows = TRUE, legend = TRUE, bounded = TRUE, clickAction = script, 
                      #      colourScale = JS('d3.scaleOrdinal().domain(["Offline filter", "Downsampling", "Re-refrencing", "Reject data", 
                      # "Reject channels", "Interpolate channels", "Multi-step automated approach", "Add_on"]).
                      #                  range([#6FB7FF", "#A8D582", "#FFE06F", "#FFB36F", "#FF6F6F", "#B07FFF", "#4287f5", 
                      #                  "#56b870", "#e6d066", "#ff943e", "#e84747", "#7c58a5","#808080", "#4FB5A7"])')
    )
    
    fn$x$nodes$Names <- nodes2$Names_vis
    fn$x$nodes$size <- nodes2$size
    #radiusCalculation: "Math.sqrt(d.nodesize)+6"
    
    htmlwidgets::onRender(fn, jsCode = '
    function (el, x) {
          d3.select("svg").append("g").attr("id", "legend-layer");
          var legend_layer = d3.select("#legend-layer");
          d3.selectAll(".legend")
            .each(function() { legend_layer.append(() => this); });
          d3.select(el)
            .selectAll(".link")
            .append("title")
            .text(d => d.value);
          var link = d3.selectAll(".link")
          var node = d3.selectAll(".node")
      
          var options = { opacity: 1,
                          clickTextSize: 10,
                          opacityNoHover: 0.1,
                        }
      
          var unfocusDivisor = 4;
      
          var links = HTMLWidgets.dataframeToD3(x.links);
          var linkedByIndex = {};
      
          links.forEach(function(d) {
            linkedByIndex[d.source + "," + d.target] = 1;
            linkedByIndex[d.target + "," + d.source] = 1;
          });
      
          function neighboring(a, b) {
            return linkedByIndex[a.index + "," + b.index];
          }
      
      
          function mouseover(d) {
            var unfocusDivisor = 4;
      
            link.transition().duration(200)
              .style("opacity", function(l) { return d != l.source && d != l.target ? +options.opacity / unfocusDivisor : +options.opacity });
      
            node.transition().duration(200)
              .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });
      
      
            node.select("text").transition()
              .duration(750)
              .attr("x", 13)
              .style("stroke-width", ".5px")
              .style("font", 5 + "px ")
              .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
          }
      
          function mouseout() {
            node.style("opacity", +options.opacity);
            link.style("opacity", +options.opacity);
      
            node.select("text").transition()
              .duration(1250)
              .attr("x", 0)
              .style("font", options.fontSize + "px ")
              .style("opacity", 0);
          }
      
          d3.selectAll(".node").on("mouseover", mouseover).on("mouseout", mouseout);
            
      }')
  })
  
  # not used?
  # output$plot_cv <- renderPlot(
  #   width = 1000, height = 800, res = 100,
  #   {
  #     input$newplot
  #     sel_p_op <- input$selectPapers_cv
  #     id_p_op <- which(dat_op$Key == sel_p_op)
  #     links_op <- list_df_op[[id_p_op]]
  #     g_op <- graph_from_data_frame(d=links_op, vertices=nodes_op, directed=T)
  #     first_nd_op <- links_op$source[1]
  #     id_fn_op <- which(nodes_op$Names == first_nd_op)
  #     V(g_op)$color <- clrs_op[V(g_op)$Groups.type]
  #     V(g_op)$color[id_fn_op] <- "grey"
  #     cdi_op <- readRDS("coordinates_op.RDS")
  #     cdi_op <- cdi_op[1:nrow(cdi_op)-1, ]
  #     plot(g_op, 
  #          layout = cdi_op,
  #          vertex.frame.color = "white",                 # Node border color
  #          vertex.shape="circle",                        # One of ???none???, ???circle???, ???square???, ???csquare???, ???rectangle??? ???crectangle???, ???vrectangle???, ???pie???, ???raster???, or ???sphere???
  #          vertex.size=10,                               # Size of the node (default is 15)
  #          
  #          # === vertex label
  #          vertex.label.color="black",
  #          vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
  #          vertex.label.cex=0.7,                         # Font size (multiplication factor, device-dependent)
  #          vertex.label.dist=0,                          # Distance between the label and the vertex
  #          vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
  #          
  #          # === Edge
  #          edge.color="darkgrey",                        # Edge color
  #          edge.width=1,                                 # Edge width, defaults to 1
  #          edge.arrow.size=0.6,                          # Arrow size, defaults to 1
  #          edge.arrow.width=3,                           # Arrow width, defaults to 1
  #          edge.lty="solid",                             # Line type, could be 0 or ???blank???, 1 or ???solid???, 2 or ???dashed???, 3 or ???dotted???, 4 or ???dotdash???, 5 or ???longdash???, 6 or ???twodash???
  #          edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
  #          asp = 0.7
  #     )
  #     
  #     legend(x=-1.1, y=1.1, c(nodes$Names_vis), pch=21,
  #            col="#777777", pt.bg=clrs_op, pt.cex=2, cex=.8, bty="n", ncol=3)
  #     
  #   })
  # 
  # output$table_option <- DT::renderDataTable({
  #   sel_p_op <- input$selectPapers_cv
  #   pipes_p_op <- dat_op_or_vis[which(dat_op_or_vis$Key == sel_p_op), ]
  #   pipes_p_op <- pipes_p_op %>%
  #     select(where(~ all(!is.na(.))))
  #   datatable(pipes_p_op,
  #             options = list(
  #               dom = 'Bfrtip',
  #               pageLength = 1,
  #               scrollX = TRUE,
  #               autoWidth = TRUE,
  #               paging = TRUE,
  #               searching = FALSE,
  #               ordering = TRUE
  #               # fixedHeader = TRUE
  #             )
  #   )
  # })
  
  
  # Plot Steps> Combination Literature -------------------------------------------------
  output$plot_YN <- renderPlot(
    width = 800, height = 800, res = 100,
    {
      input$newplot
      st_sel <- input$selectDecisionYN
      st_dat <- mat_yn[st_sel, ]
      st_dat <- data.frame(st_dat)
      st_dat$name <- row.names(st_dat)
      colnames(st_dat) <- c("value", "name")
      st_dat$Groups <- nodes$Groups
      st_dat$Groups <- factor(st_dat$Groups, levels = unique(st_dat$Groups))
      st_dat$col <- nodes$col
      st_dat <- st_dat %>%
        mutate(Names_or = fct_reorder(name, desc(nodes$ID)))
      label_colors <- ifelse(st_dat$Names_or == st_sel, "red", "black")
      names(label_colors) <- st_dat$Names_or
      
      par(mar=c(10,4,4,1)+.1)
      # Create the ggplot
      ggplot(st_dat, aes(x = Names_or, y = value, color = Groups)) +
        geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
        geom_point(size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = unique(st_dat$col)) +
        #scale_y_discrete(labels = st_dat$Names_or, breaks = st_dat$Names_or, limits = st_dat$Names_or) +
        theme_light() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial"),
          axis.text.y = element_text(color = label_colors[st_dat$Names_or]),
        ) +
        coord_flip() +
        labs(
          x = "Step",
          y = paste("Number of articles (out of 27 articles) used it together with", st_sel),
          color = "Group"
        )
    })
  
  # Plot Steps> Combination Crowd -------------------------------------------------
  output$plot_YN_crowd <- renderPlot(
    width = 800, height = 800, res = 100,
    {
      input$newplot
      st_sel <- input$selectDecisionYN
      st_dat <- mat_yn_crowd[st_sel, ]
      st_dat <- data.frame(st_dat)
      st_dat$name <- row.names(mat_yn_crowd)#prev row.names
      colnames(st_dat) <- c("value", "name")
      st_dat$Groups <- nodes_crowd$Groups
      st_dat$Groups <- factor(st_dat$Groups, levels = unique(st_dat$Groups))
      st_dat$col <- nodes$col[match(nodes_crowd$Names, nodes$Names)]#get color from literature
      st_dat <- st_dat %>%
        mutate(Names_or = fct_reorder(name, desc(nodes_crowd$ID)))
      # Limit y-axis labels to 5 characters
      #st_dat$Names_or <- stringr::str_sub(st_dat$Names_or, 1, 20)
      label_colors <- ifelse(st_dat$Names_or == st_sel, "red", "black")
      names(label_colors) <- st_dat$Names_or
      par(mar=c(10,4,4,1)+.1)
      
      # Create the ggplot
      ggplot(st_dat, aes(x = Names_or, y = value, color = Groups)) +
        geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
        geom_point(size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = unique(st_dat$col)) +
        #scale_y_discrete(labels = st_dat$Names_or, breaks = st_dat$Names_or, limits = st_dat$Names_or) +
        theme_light() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial"),
          axis.text.y = element_text(color = label_colors[st_dat$Names_or]),
        ) +
        coord_flip() +
        labs(
          x = "Steps",
          y = paste("Number experts used it together with", st_sel),
          color = "Groups"
        )
    })
  
  # Plot Steps> Order Literature -------------------------------------------------
  output$plot_OR <- renderPlot(
    width =800, height = 800, res = 100,
    {
      input$newplot
      st_sel_OR <- input$selectDecisionOR
      st_dat_OR <- mat_or[st_sel_OR, ]
      st_dat_OR <- data.frame(st_dat_OR)
      st_dat_OR$name <- row.names(st_dat_OR)
      colnames(st_dat_OR) <- c("value", "name")
      st_dat_OR$Groups <- nodes$Groups
      st_dat_OR$Groups <- factor(st_dat_OR$Groups, levels = unique(st_dat_OR$Groups))
      st_dat_OR$col <- nodes$col
      st_dat_OR <- st_dat_OR %>%
        mutate(Names_or = fct_reorder(name, desc(nodes$ID)))
      label_colors <- ifelse(st_dat_OR$Names_or == st_sel_OR, "red", "black")
      names(label_colors) <- st_dat_OR$Names_or
      par(mar=c(10,4,4,1)+.1)
      ggplot(st_dat_OR, aes(x = Names_or, y = value, color = Groups)) +
        geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
        geom_point(size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = unique(st_dat_OR$col)) +
        #scale_y_discrete(labels = st_dat_OR$Names_or, breaks = st_dat_OR$Names_or, limits = st_dat_OR$Names_or) +
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial"),
          axis.text.y = element_text(color = label_colors[st_dat_OR$Names_or]),
        ) +
        labs(
          x = "Step",
          y = paste("Number of articles (out of 27 articles) used it after", st_sel_OR),
          color = "Group"
        )
    })
  
  # Plot Steps> Order Crowd -------------------------------------------------
  output$plot_OR_crowd <- renderPlot(
    width =800, height = 800, res = 100,
    {
      input$newplot
      st_sel_OR <- input$selectDecisionOR
      st_dat_OR <- mat_or_crowd[st_sel_OR, ]
      st_dat_OR <- data.frame(st_dat_OR)
      st_dat_OR$name <- row.names(st_dat_OR)
      colnames(st_dat_OR) <- c("value", "name")
      st_dat_OR$Groups <- nodes_crowd$Groups
      st_dat_OR$Groups <- factor(st_dat_OR$Groups, levels = unique(st_dat_OR$Groups))
      st_dat_OR$col <- nodes$col[match(nodes_crowd$Names, nodes$Names)]#get color from literature
      st_dat_OR <- st_dat_OR %>%
        mutate(Names_or = fct_reorder(name, desc(nodes_crowd$ID)))
      label_colors <- ifelse(st_dat_OR$Names_or == st_sel_OR, "red", "black")
      names(label_colors) <- st_dat_OR$Names_or
      par(mar=c(10,4,4,1)+.1)
      ggplot(st_dat_OR, aes(x = Names_or, y = value, color = Groups)) +
        geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
        geom_point(size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = unique(st_dat_OR$col)) +
        #scale_y_discrete(labels = st_dat_OR$Names_or, breaks = st_dat_OR$Names_or, limits = st_dat_OR$Names_or) +
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial"),
          axis.text.y = element_text(color = label_colors[st_dat_OR$Names_or]),
        ) +
        labs(
          x = "Steps",
          y = paste("Number of experts used it after", st_sel_OR),
          color = "Groups"
        )
    })
  
  # Steps:Options: Literature ----------------------------------------------------
  output$plot_group_decision <- renderPlot(
    width = 450, height = 450, res = 100,
    {
      input$newplot
      gr_dec <- input$selectGroup
      gr_dec <- nodes$Names[nodes$Names_vis == gr_dec]
      gr_dat <- dat_op_or[ ,c(gr_dec)]
      gr_ds <- colSums(mtabulate(gr_dat))
      gr_ds <- data.frame(gr_ds)
      gr_ds$name <- row.names(gr_ds)
      colnames(gr_ds) <- c("value", "name")
      gr_ds$name <- fct_relevel(gr_ds$name, "Not_reported", "Not_used")
      custom_colors <- c("Not_reported" = "red", "Not_used" = "blue")
      
      
      par(mar=c(10,4,4,1)+.1)
      ggplot(gr_ds, aes(x = name, y = value)) +
        geom_segment(aes(xend = name, yend = 0, color = name), size = 1) +
        geom_point(aes(color = name), size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = custom_colors, guide = "none") +  # Remove the legend
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial")
        ) +
        labs(x = "Option", y = "Number of articles out of 27 articles")
    })
  
  output$selectDecision <- renderUI({
    st_sel <- input$selectGroup
    opts2 <- which(nodes_op$Groups_vis==st_sel)
    opts_2 <- nodes_op[opts2, ]
    selectInput("selectDecision2",
                label   = "Select an option to generate a table of articles using it:",
                choices =  c(opts_2$Names, "Not_used", "Not_reported"),
                selected = opts_2$Names[1]
    )
  })
  
  output$selected_decision <- renderText({
    dec <- input$selectDecision2
    id_dec <- which(dat_op == dec, arr.ind = T)
    
    dec1 <- input$selectGroup #interim solution as longa as table does not work
    dec1 <- nodes$Names[which(nodes$Names_vis == dec1)]
    dat_op_or_sel <- dat_op_or[, dec1]
    id_dec <- which(dat_op_or_sel == dec, arr.ind = TRUE)
    
    paste("You have selected option of ", input$selectDecision2, " which was used by articles:", paste(p_inf$Key[id_dec], collapse = ", "))
  })
  
  output$table_op <- DT::renderDataTable({
    dec <- input$selectDecision2
    dec1 <- input$selectGroup
    dec1 <- nodes$Names[which(nodes$Names_vis == dec1)]
    dat_op_or_sel <- dat_op_or[, dec1]
    id_dec <- which(dat_op_or_sel == dec, arr.ind = TRUE)
    tmp<- p_inf[id_dec[, 1], ]#try to work out why table does not render
    
    datatable(tmp,
              escape = FALSE,
              options = list(
                dom = 'Bfrtip',
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = FALSE,
                ordering = TRUE,
                columnDefs = list(
                  list(width = '150px', targets = c(2,3))
                ),
                fixedHeader = TRUE
              )
    )
  })
  
  # Steps: Options: Experts ----------------------------------------------------
  output$plot_group_decision_crowd <- renderPlot(
    
    width = 450, height = 450, res = 100,
    {
      input$newplot
      gr_dec <- input$selectGroup
      gr_dec <- gsub(" ", "_", gr_dec)
      gr_dec <- gsub("-", "_", gr_dec)
      gr_dec <- gsub("/", "_", gr_dec)#      nodes_crowd$Names[nodes_crowd$Names_vis == gr_dec]
      
      # error message if no data can be displayed
      if (all(is.na(match(colnames(dat_op_or_crowd), c(gr_dec))))){
        showNotification("This step was not used. Please select a different step.",
                         type = "error")
        return()
      }
      
      gr_dat <- dat_op_or_crowd[ ,c(gr_dec)]
      gr_ds <- colSums(mtabulate(gr_dat))
      gr_ds <- data.frame(gr_ds)
      gr_ds$name <- row.names(gr_ds)
      colnames(gr_ds) <- c("value", "name")
      gr_ds$name <- fct_relevel(gr_ds$name, "Not_reported", "Not_used")
      custom_colors <- c("Not_reported" = "red", "Not_used" = "blue")
      
      par(mar=c(10,4,4,1)+.1)
      ggplot(gr_ds, aes(x = name, y = value)) +
        geom_segment(aes(xend = name, yend = 0, color = name), size = 1) +
        geom_point(aes(color = name), size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = custom_colors, guide = "none") +  # Remove the legend
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial")
        ) +
        labs(x = "Options", y = "Number of experts")
      
    })
  
  output$selectDecision_crowd <- renderUI({#check!
    st_sel <- input$selectGroup
    st_sel <- gsub(" ", "_", st_sel)
    st_sel <- gsub("-", "_", st_sel)
    st_sel <- gsub("/", "_", st_sel)
    opts2 <- which(nodes_op_crowd$Groups_vis==st_sel)
    opts_2 <- nodes_op_crowd[opts2, ]
    selectInput("selectDecision_crowd",
                label   = "Select an option to generate a list of experts using it:",
                choices =  c(opts_2$Names, "Not_used", "Not_reported"),
                selected = opts_2$Names[1]
    )
  })
  output$selected_decision_crowd <- renderText({
    dec <- input$selectDecision_crowd
    dec1 <- input$selectGroup
    dec1 <- gsub(" ", "_", dec1)
    dec1 <- gsub("-", "_", dec1)
    dec1 <- gsub("/", "_", dec1)
    #dec1 <- nodes_crowd$Names[which(nodes_crowd$Names_vis == dec1)]
  

    dat_op_or_sel <- dat_op_or_crowd[, dec1]
    id_dec <- which(dat_op_or_sel == dec, arr.ind = TRUE)
    id_crowd<- seenIDs_df[id_dec, ]
    paste("You have selected option of ", dec, " which was used by experts:", paste(id_crowd, collapse = ", "))
  })
  
  
  #################Metadata##########################
  #######Experiment########################
  output$Selected_decisionExperiment <- renderText({
    decExp <- input$SelectExp
    paste("Lollipop plot of the number of studies using various options of experiment methods")
  })
  
  output$selected_OptionExperiment <- renderText({
    dec <- input$SelectExp
    paste("You have selected the option of ", input$SelectExp, "which was used by papers:")
  })
  
  
  output$tableExperiment <- DT::renderDataTable({
    decExp <- input$SelectExp
    dec1Exp <- "task_name"
    #unique_df <- distinct(MetaData, Key, .keep_all = TRUE)
    dat_op_or_selExp <- MetaData[, dec1Exp]
    id_decExp <- which(dat_op_or_selExp == decExp, arr.ind = TRUE)
    new_tabExp <- p_inf2[id_decExp[, 1], ]
    
    datatable(new_tabExp,
              escape = FALSE,
              options = list(
                dom = 'Bfrtip',
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = FALSE,
                ordering = TRUE,
                columnDefs = list(
                  list(width = '300px', targets = c(2,3))
                )
                # fixedHeader = TRUE
              )
    )
  })
  #######EEG Acquistion####################
  # Select Input Part -------------------------------------------------------
  output$selectDecisionAcq <- renderUI({
    st_sel <- input$SelectAcq
    opts2Acq <- which(EEGAcq$EEG_Acq_Steps==st_sel)
    opts_2Acq <- EEGAcq[opts2Acq, ]
    selectInput("selectDecision2Acq",
                label   = "Select the option",
                choices =  c(opts_2Acq$Options),
                selected = opts_2Acq[1]
    )
  })
  
  output$selected_decisionAcq <- renderText({
    dec <- input$selectDecision2Acq
    #id_dec <- which(dat_op == dec, arr.ind = T)
    paste("Lollipop plot of the number of studies using various options of", input$SelectAcq)
  })
  
  # Lollipop Plot Part ------------------------------------------------------
  output$plot_group_decisionAcq <- renderPlot(
    width = 450, height = 450, res = 100,
    {
      input$newplotAcq
      gr_decAcq <- input$SelectAcq
      #gr_decAcq <- EEGAcq$EEG_Acq_Steps[EEGAcq$EEG_Acq_Steps == gr_decAcq]
      #MetaDataUnique <- unique(MetaData$Key)
      #unique_df <- distinct(MetaData, Key, .keep_all = TRUE)
      gr_datAcq <- MetaData[ ,c(gr_decAcq)]
      gr_dsAcq <- colSums(mtabulate(gr_datAcq))
      gr_dsAcq <- data.frame(gr_dsAcq)
      gr_dsAcq$name <- row.names(gr_dsAcq)
      colnames(gr_dsAcq) <- c("value", "name")
      gr_dsAcq$name <- fct_relevel(gr_dsAcq$name)
      custom_colors <- c("Not_reported" = "gray")
      # gr_dsAcq$color[gr_dsAcq$name ~= "Not_reported"] <- "Other_Values"
      # gr_dsAcq$color[gr_dsAcq$name == "Not_reported"] <- "Not_reported"
      
      par(mar=c(10,4,4,1)+.1)
      ggplot(gr_dsAcq, aes(x = name, y = value)) +
        geom_segment(aes(xend = name, yend = 0, color = name), size = 1) +
        geom_point(aes(color = name), size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = custom_colors, guide = "none") +  # Remove the legend
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial")
        ) +
        labs(x = "Option", y = "Number of articles")
      
    })
  # Paper Table Part --------------------------------------------------------
  output$selected_optionAcq <- renderText({
    dec <- input$selectDecision2Acq
    #id_dec <- which(dat_op == dec, arr.ind = T)
    paste("You have selected the option of ", input$selectDecision2Acq, "which was used by papers:")
  })
  
  
  output$tableAcq <- DT::renderDataTable({
    decAcq <- input$selectDecision2Acq
    dec1Acq <- input$SelectAcq
    #unique_df <- distinct(MetaData, Key, .keep_all = TRUE)
    dat_op_or_selAcq <- MetaData[, dec1Acq]
    id_decAcq <- which(dat_op_or_selAcq == decAcq, arr.ind = TRUE)
    new_tabAcq <- p_inf2[id_decAcq[, 1], ]
    
    datatable(new_tabAcq,
              escape = FALSE,
              options = list(
                dom = 'Bfrtip',
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = FALSE,
                ordering = TRUE,
                columnDefs = list(
                  list(width = '300px', targets = c(2,3))
                )
                # fixedHeader = TRUE
              )
    )
  })
  
  
  ########Software##############################
  # Select Input Part -------------------------------------------------------
  output$SelectSoftwareMeta <- renderUI({
    st_selSoft <- input$SelectSoftware
    opts2Soft <- which(SoftwareMeta$Environment==st_selSoft)
    opts_2Soft <- SoftwareMeta[opts2Soft, ]
    selectInput("SelectSoftwareMeta2",
                label   = "Select the option",
                choices =  c(opts_2Soft$Options),
                selected = opts_2Soft[1]
    )
  })
  
  output$selected_decisionSoft <- renderText({
    dec <- input$SelectSoftwareMeta2
    #id_dec <- which(dat_op == dec, arr.ind = T)
    paste("Lollipop plot of the number of studies using various options of", input$SelectSoftware)
  })
  
  
  # Lollipop Plot Part ------------------------------------------------------
  output$plot_group_decisionSoft <- renderPlot(
    width = 450, height = 450, res = 100,
    {
      input$newplotSoft
      gr_decSoft <- input$SelectSoftware
      #gr_decAcq <- EEGAcq$EEG_Acq_Steps[EEGAcq$EEG_Acq_Steps == gr_decAcq]
      gr_datSoft <- MetaData[ ,c(gr_decSoft)]
      gr_dsSoft <- colSums(mtabulate(gr_datSoft))
      gr_dsSoft <- data.frame(gr_dsSoft)
      gr_dsSoft$name <- row.names(gr_dsSoft)
      colnames(gr_dsSoft) <- c("value", "name")
      gr_dsSoft$name <- fct_relevel(gr_dsSoft$name)
      custom_colors <- c("Not_reported" = "gray")
      
      
      par(mar=c(10,4,4,1)+.1)
      ggplot(gr_dsSoft, aes(x = name, y = value)) +
        geom_segment(aes(xend = name, yend = 0, color = name), size = 1) +
        geom_point(aes(color = name), size = 4, alpha = 0.6) +
        geom_text(aes(label = value), vjust = 0.5, hjust = -1, size = 4, color = "black") +
        scale_color_manual(values = custom_colors, guide = "none") +  # Remove the legend
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 12, family = "Arial")
        ) +
        labs(x = "Option", y = "Number of articles")
      
    })
  
  # Paper Table Part --------------------------------------------------------
  output$selected_optionSoft <- renderText({
    dec <- input$SelectSoftwareMeta2
    #id_dec <- which(dat_op == dec, arr.ind = T)
    paste("You have selected the option of ", input$SelectSoftwareMeta2, "which was used by papers:")
  })
  
  output$tableSoft <- DT::renderDataTable({
    decSoft <- input$SelectSoftwareMeta2
    dec1Soft <- input$SelectSoftware
    #dec1Acq <- EEGAcq$EEG_Acq_Steps[which(EEGAcq$EEG_Acq_Steps == dec1Acq)]
    dat_op_or_selSoft <- MetaData[, dec1Soft]
    id_decSoft <- which(dat_op_or_selSoft == decSoft, arr.ind = TRUE)
    new_tabSoft <- p_inf2[id_decSoft[, 1], ]
    
    datatable(new_tabSoft,
              escape = FALSE,
              options = list(
                dom = 'Bfrtip',
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = FALSE,
                ordering = TRUE,
                columnDefs = list(
                  list(width = '300px', targets = c(2,3))
                )
                # fixedHeader = TRUE
              )
    )
  })
  ###########Open Scholarship#####################
  # Select Input Part -------------------------------------------------------
  output$SelectOpenScience <- renderUI({
    st_selOpenSci <- input$SelectOpenSci
    opts2OpenSci <- which(OpenScienceMeta$`Open Science`==st_selOpenSci)
    opts_2OpenSci <- OpenScienceMeta[opts2OpenSci, ]
    selectInput("SelectOpenScienceMeta2",
                label   = "Select the option",
                choices =  c(opts_2OpenSci$Options),
                selected = opts_2OpenSci[1]
    )
  })
  
  output$selected_decisionOpenSci <- renderText({
    dec <- input$SelectOpenScienceMeta2
    #id_dec <- which(dat_op == dec, arr.ind = T)
    paste("Bar plot of the number of studies performing various open science practices of", input$SelectOpenSci)
  })
  
  
  # Bar Graph Part ----------------------------------------------------------
  
  
  # Paper Table Part --------------------------------------------------------
  output$selected_optionOpenSci <- renderText({
    dec <- input$SelectOpenScienceMeta2
    #id_dec <- which(dat_op == dec, arr.ind = T)
    paste("You have selected the option of ", input$SelectOpenScienceMeta2, "which was used by papers:")
  })
  
  output$tableOpenSci <- DT::renderDataTable({
    decOpenSci <- input$SelectOpenScienceMeta2
    dec1OpenSci <- input$SelectOpenSci
    #dec1Acq <- EEGAcq$EEG_Acq_Steps[which(EEGAcq$EEG_Acq_Steps == dec1Acq)]
    dat_op_or_selOpenSci <- MetaData[, dec1OpenSci]
    id_decOpenSci <- which(dat_op_or_selOpenSci == decOpenSci, arr.ind = TRUE)
    new_tabOpenSci <- p_inf2[id_decOpenSci[, 1], ]
    
    datatable(new_tabOpenSci,
              escape = FALSE,
              options = list(
                dom = 'Bfrtip',
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = FALSE,
                ordering = TRUE,
                columnDefs = list(
                  list(width = '300px', targets = c(2,3))
                )
                # fixedHeader = TRUE
              )
    )
  })
  
  
  # Database ----------------------------------------------------------------
  
  # Expert Survey -----------------------------------------------------------
  
  #Output text expert information
  output$selected_var <- renderText({
    # Find the row in the Questions data frame where Code equals input$variable
    row_var <- match(input$variable, Questions$Question)
    
    # Extract the response options
    response_options <- Questions$Options[row_var]
    if (is.na(response_options)){ # not indicated for free text so instert
      response_options <- "Free text"}
    
    HTML(paste("<b>Question: </b>", input$variable, "<br><b>Response options: </b>", response_options))
  })
  
  
  ##Output plot expert information
  output$hist <- renderUI({
    if (!is.null(input$variable)) {
      if (input$variable == 'What is the basis of your pre-processing decisions?'){
        plotOutput("barPlotAgreement")
      } else if(input$variable == 'If you were to follow the pipeline recommendations of your peer community, which peer characteristics would contribute to you agreeing to make pipeline choices that would not at first glance be your personal preferred choice?'){
        plotOutput("barPlotAgreement2")
        
      } else {
        row_var <- match(input$variable, Questions$Question)
        code_var <- Questions$Code[row_var] # Extract the Code from that row
        if (code_var %in% c('yearsfconn','yeearsgraph')) {
          # Create a histogram if the column is numeric
          plotOutput("histPlot")
        } else if (code_var == 'country') {
          # Render a leaflet map if the column is 'country'
          leafletOutput("map")
        } else if (code_var %in% c('field','topics', 'codesharelink', 'EEGsoftw', 'choicesource8', 'peeraccept8')) {
          # Create a table if the column is 'topics', 'codesharelink', or 'EEGsoftw'
          tableOutput("table")
        } else {
          # Create a bar plot if the column is not numeric and not 'topics', 'codesharelink', or 'fmrisoftw'
          plotOutput("barPlot")
        }
      }
    }
  })
  
  output$histPlot <- renderPlot({
    row_var <- match(input$variable, Questions$Question)
    code_var <- Questions$Code[row_var] # Extract the Code from that row
    hist(as.numeric(Survey[[code_var]]), main = "",ylab = "Number of Experts", xlab = "Years", col = "lightblue")
  }, height = 400, width = 600)
  
  output$barPlotAgreement <- renderPlot({
    data_choicesource <- subset(Survey, 
                                select=c( 'choicesource1','choicesource2','choicesource3',
                                          'choicesource4','choicesource5','choicesource6', 
                                          'choicesource7'))
    colnames(data_choicesource) <- c("Previous literature on the topic",
                                     "Methodological literature",
                                     "Published standard pipelines",
                                     "Procedures at your current lab",
                                     "Procedures at previous labs",
                                     "Ease of implementation",
                                     "Existing personal skills")
    counts <- sapply(data_choicesource,as.numeric)
    par(mar=c(4,14,4,4) + 0.1, cex.lab = 1)
    bp <- barplot(colMeans(data.matrix(counts)),  col = "lightblue", 
                  horiz = T, las = 2,
                  xlab = "Agreement (%)")
  }, height = 400, width = 600)
  
  output$barPlotAgreement2 <- renderPlot({
    data_peeraccept <- subset(Survey,
                              select=c( 'peeraccept1','peeraccept2','peeraccept3',
                                        'peeraccept4','peeraccept5','peeraccept6',
                                        'peeraccept7'))
    colnames(data_peeraccept ) <- c("Graeter Duration Work EEG",
                                    "More Pubications EEG",
                                    "More Expertise EEG",
                                    "Researchs EEG Analyses",
                                    "Researchs Scientific Practices",
                                    "Greater Likelihood of Publication",
                                    "Greater Likelihood of sig Results")
    counts <- sapply(data_peeraccept,as.numeric)
    par(mar=c(4,14,4,4) + 0.1, cex.lab = 1)
    bp <- barplot(colMeans(data.matrix(counts)),  col = "lightblue", 
                  horiz = T, las = 2,
                  xlab = "Agreement (%)")
  }, height = 400, width = 600)
  
  output$barPlot <- renderPlot({
    row_var <- match(input$variable, Questions$Question)
    code_var <- Questions$Code[row_var] # Extract the Code from that row
    counts <- table(Survey[[code_var]])
    par(mar=c(4,14,4,4) + 0.1, cex.lab = 1)
    bp <- barplot(counts,  col = "lightblue", 
                  horiz = T, las = 2,
                  xlab = "Number of Experts") #add title: main = code_var,xlab = ""
    
  }, height = 400, width = 600)
  
  output$table <- renderTable({
    row_var <- match(input$variable, Questions$Question)
    code_var <- Questions$Code[row_var] # Extract the Code from that row
    
    # Check if input$variable is 'topics', 'codesharelink', or 'fmrisoftw'
    if (code_var %in% c('field','topics', 'codesharelink', 'EEGsoftw', 'choicesource8', 'peeraccept8')) {
      # Count the number of occurrences of each value in the selected column
      counts <- table(Survey[[code_var]])
      
      # Convert the table to a data frame
      df <- as.data.frame.table(counts)
      
      # Rename the columns
      colnames(df) <- c("Values", "Count")
      
      df
    }
  })
  
  output$map <- renderLeaflet({
    # Convert the country names to ISO 3166-1 alpha-2 country codes
    Survey2 <- Survey
    Survey2$country <- countrycode(Survey2$country, "country.name", "iso2c")
    
    # Count the number of experts in each country
    country_counts <- Survey2 %>%
      group_by(country) %>%
      summarise(n = n(), .groups = "drop")
    
    # Get the world countries spatial data
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Join the spatial data with your data
    world$iso_a2 <- tolower(world$iso_a2)  # Make sure the country codes are in the same case
    country_counts <- left_join(world, country_counts, by = c("iso_a2_eh" = "country"))
    
    # Create a color palette for the counts
    pal <- colorNumeric("viridis", domain = country_counts$n)
    
    # Render a leaflet map
    leaflet(country_counts) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(n), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, color = "#FFFFFF", dashArray = "3", fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Number of Experts")
  })

}
