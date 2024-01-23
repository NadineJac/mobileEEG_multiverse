# Developed in R version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"
# Daniel Kristanto (University of Oldenburg)
# adapted by Nadine Nadine Jacobsen (University of Oldenburg) and Cosku Inceler (University of Oldenburg)
# January 2024, last revision: 23-January-2024
library(shiny)        # v1.7.5
library(networkD3)    # v0.4
library(dplyr)        # v1.1.3
library(igraph)       # v1.5.1
library(visNetwork)   # v2.1.2
library(geomnet)      # v0.3.1
library(stringr)      # v1.5.0
library(png)          # v0.1-8
library(shinyjs)      # v2.1.0
library(DT)           # v0.29
library(rintrojs)     # v0.3.2
library(ggplot2)      # v3.4.3
library(qdapTools)    # v2.4.6
library(RColorBrewer) # v1.1-3
library(shinyWidgets) # v0.8.0

shinyWidgets::shinyWidgetsGallery()

source("helper_function.R")

ui <- shinyUI(navbarPage(title = div(img(src="metarep.jpg", height = "80px"), img(src="uol.jpg", height = "80px") ), id = "navBar",
                         theme = "bootstrap.css",
                         collapsible = TRUE,
                         inverse = TRUE,
                         windowTitle = "Forking Path of fMRI and mobile EEG analyses",
                         position = "fixed-top",
                         header = tags$style(
                           ".navbar-right {
                       float: right !important;
                       }",
                           "body {padding-top: 150px;}"),
                         
                         # Tab: Introduction -------------------------------------------------------
                         
                         tabPanel("Introduction", value = "home",
                                  
                                  shinyjs::useShinyjs(),
                                  
                                  tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           shiny::HTML("<br><h1><center>METEOR</h1><br>
                                           <h2><center> <b>M</b>ast<b>E</b>ring <b>T</b>h<b>E O</b>pp<b>R</b>essive number of forking paths 
                                                        unfolded by noisy and complex neural data</h2></center><br>
                                                       <h4>This interactive shiny app allows you to investigate the 
                                                       multiverse of mobile electroencephalography (EEG) acquisition and 
                                                       preprocessing to investigate the P3 during walking and rest. You can 
                                                       explore the preprocessing choices of published articles. You can also 
                                                       construct your own preprocessing pipeline and compare it to the ones 
                                                       in the literature.<br><br>
                                                       <b>Database:</b> The multiverse has been identified by a literature review. 
                                                       All information on the literature review, the included articles, and 
                                                       the coded preprocessing steps and their respective options can be 
                                                       found here.<br><br>
                                                       <b>Steps:</b> Explore which preprocessing steps have been used and 
                                                       which combinations and orders are common.<br><br>
                                                       <b>Steps: Options:</b> Explore which options for the respective 
                                                       preprocessing steps have been used by which articles.<br><br>
                                                       <b>Individual article:</b> Check out preprocessing pipelines and their 
                                                       chosen options for individual articles.<br><br>
                                                       <b>Your Own Pipeline:</b> Construct your own pipeline and compare it to 
                                                       the ones in the literature.<br><br>
                                                       <center> <h4>Last updated: 2024-01-23</h4><br>
                                                       <h4>Ready to get started?</h4></center><br>")
                                    ),
                                  ),
                         ), # Closes the first tabPanel called "Home"
                         
                         # Tab: Background  -------------------------------------------------------
                         
                         tabPanel("Background", value = "background",
                                  
                                  #shinyjs::useShinyjs(),
                                  
                                  fluidRow(
                                    column(1),
                                    column(4,
                                           shiny::HTML("<br><h3><b>The P3 and its reduction during motion</b></h3><br>
                                                       <p style = 'text-align: justify;'>The P3, or P300, is a well-known event-related potential (ERP) 
                                                       component observed in electroencephalography (EEG) recordings of the 
                                                       brain's electrical activity. It is a positive deflection in the EEG 
                                                       waveform that typically occurs around 300 milliseconds (hence the name 
                                                       'P300'') after the presentation of a stimulus, such as an auditory or 
                                                       visual cue. The P3 component is associated with various cognitive 
                                                       processes, including attention, memory, and decision-making.<br><br>
                                                       
                                                       When healthy humans engage in motion, such as walking or running, 
                                                       the P3 component may be altered. Firstly, the amplitude of the P3 
                                                       component may decrease during motion. It has been hypothesized that 
                                                       this may indicate a re-allocation of attention and cognitive resources 
                                                       to the motor tasks involved in motion. Secondly, the P3 latency, the 
                                                       time it takes for the P3 component to peak after stimulus presentation, 
                                                       may be prolonged during motion. This delay has been attributed to the 
                                                       diversion of cognitive resources toward motor control and the need for 
                                                       the brain to reorient its focus to the cognitive task at hand. Factors 
                                                       such as the type and intensity of motion, individual differences, and 
                                                       the specific cognitive task being performed can influence how the P3 
                                                       component changes during motion. <br><br>
                                                       
                                                       Moreover, motion can introduce artifacts in EEG recordings, which are 
                                                       disturbances in the signal caused by movement-related electrical 
                                                       activity. These artifacts can obscure the P3 component and make it 
                                                       challenging to analyze accurately. Researchers often use techniques 
                                                       like artifact correction and filtering to mitigate these issues.</p>"
                                           )
                                    ),
                                    column(2),
                                    column(4,
                                           shiny::HTML("<br><h3><b>Researchers<span>&#39;</span>  degrees of freedom</b></h3><br>
                                                       <p style = 'text-align: justify;'>While there is a considerable number of preprocessing steps 
                                                       available, there is no standard approach for analyzing (mobile) EEG. 
                                                       Hence, each researcher faces a vast number of decisions and reserachers
                                                       <span>&#39;</span> degrees of freedom when deciding on their 
                                                       preprocessing pipeline. They not only have to decide which steps to 
                                                       employ but also with which parameters and in which order. This is a 
                                                       problem as the chosen preprocessing might influence the obtained results,
                                                       but the impact is so far unknown for mobile EEG. <br><br> 
                                                       Here, we compiled the garden of forking paths of mobile EEG preprocessing  
                                                       strategies in peer-reviewed publications on the P3 reduction during walking 
                                                       compared to standing using a systematic literature review. Defining chosen 
                                                       preprocessing approaches is a prerequisite to systematically assess their 
                                                       impact. Furthermore, this identified common, debated, and unique preprocessing 
                                                       choices and may help researchers to navigate the garden of forking paths.")
                                    ),
                                    
                                  ),
                                  
                         ), # Closes tab calles "Background"
                         
                         
                         # Tab: Database -----------------------------------------------------------
                         
                         tabPanel("Database", value = "MA",
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel(
                                                  "Literature review",
                                                  fluidRow(
                                                    column(5, 
                                                           shiny::HTML("<h5><b>Literature review of articles using mobile EEG to investigate 
                                                  the P3 during walking and rest.</b></h5> 
                                                  <b><p style = 'text-align:justify;'>Literature search:</b>Three databases were searched: Scopus, Web of Science, 
                                                  and PubMed for publications containing the terms <i>EEG or ERP and P3 or P300 and 
                                                  gait</i> in the summer of 2022. <br><br> 
                                                  <b>Study inclusion:</b> We only included empirical studies in which both the P3 
                                                  during walking as well as standing or a similar non-motion condition was analyzed 
                                                  as we targeted the P3 reduction with motion. Moreover, we only included studies 
                                                  with healthy participants as some conditions prompt an adapted EEG preprocessing 
                                                  such as an altered muscular tonus. Details can be found in the Preferred Reporting 
                                                  Items for Systematic Reviews and Meta-Analyses (PRISMA) flowchart on the right. <br><br>
                                                  <b>Data extraction:</b> All data was coded by two independent reviewers. 
                                                  Discrepancies were resolved by both reviewers together. Information on the recording 
                                                  setup and task (here called <i>EEG metadata</i>) and the preprocessing steps and 
                                                  their respective options were extracted. <br><br></h5>"),
                                                    ),
                                                    column(7, 
                                                           img(src='prisma1.jpg', align = "right", width = "700px"),
                                                    ),
                                                  ),
                                                ),
                                                tabPanel(
                                                  "List of included articles",
                                                  shiny::HTML("<h5><b>List of included articles mobile EEG to investigate the P3 during walking and rest. </b></h3>"),
                                                  DT::dataTableOutput("list_paper")
                                                ),
                                                tabPanel(
                                                  "List of Steps",
                                                  shiny::HTML("<h5><b>List of identivied steps</b></h5>"),
                                                  DT::dataTableOutput("list_steps")
                                                ),
                                                tabPanel(
                                                  "List of Options",
                                                  shiny::HTML("<h5><b>List of identified options</b></h5>"),
                                                  DT::dataTableOutput("list_decisions")
                                                )
                                    )
                                  )  # Closes the mainPanel
                         ),  # Closes the second tabPanel called "Database"
                         
                         # Tab: Metadata ----------------------------------------------------------
                         tabPanel("Metadata", value = "MD",
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Experiment",
                                                       sidebarLayout(
                                                         sidebarPanel(width = 3,
                                                                      selectInput("SelectExp",
                                                                                  label = "Explore the experimental tasks chosen by different articles. Select a task from the dropdown below.",
                                                                                  choices = c(ExperimentModels)
                                                                                  )
                                                                      ),
                                                         mainPanel(width =9,
                                                                   fluidRow(
                                                                     column(5, textOutput("Selected_decisionExperiment"), img(src='ExperimentModels.png', align = "right", width = "550px")),
                                                                     column(7, textOutput("selected_OptionExperiment"), DT::dataTableOutput("tableExperiment"))
                                                                   )
                                                                   )
                                                       )),
                                              tabPanel("EEG acquisition",
                                                       sidebarLayout(
                                                         sidebarPanel(width = 3,
                                                                      selectInput("SelectAcq",
                                                                                  label = "Explore the EEG acquisition parameters chosen by different articles. Select a parameter from the dropdown below.",
                                                                                  choices = c(unique(EEGAcq$EEG_Acq_Steps)),
                                                                      ),
                                                                      uiOutput("selectDecisionAcq")
                                                         ),
                                                         mainPanel(width = 9,
                                                                   fluidRow(
                                                                     column(5, textOutput("selected_decisionAcq"), plotOutput("plot_group_decisionAcq", height = 600, width = "100%"), 
                                                                            shiny::HTML("<h4><b>Caution:</b> Some studies compared multiple 
                                                                                 EEG acquisition systems. Within the 27 included articles in total 32 systems were 
                                                                                 compared. If you are interested in a specific system, 
                                                                                 please check for additional parameters or code within 
                                                                                 the publication by clicking on it.</h4>")),
                                                                     column(6,textOutput("selected_optionAcq"),  DT::dataTableOutput("tableAcq")),
                                                                   )
                                                         )
                                                       )
                                              ),
                                              tabPanel("Software",
                                                       sidebarLayout(
                                                         sidebarPanel(width = 3,
                                                                      selectInput("SelectSoftware",
                                                                                  label = "Explore the EEG analysis environments chosen by different articles. Select environment or toolbox below.",
                                                                                  choices = c(unique(SoftwareMeta$Environment))
                                                                      ),
                                                                      uiOutput("SelectSoftwareMeta")
                                                         ),
                                                         mainPanel(width = 9,
                                                                   fluidRow(
                                                                     column(5, textOutput("selected_decisionSoft"), plotOutput("plot_group_decisionSoft", height = 600, width = "100%")),
                                                                     column(6, textOutput("selected_optionSoft"), DT::dataTableOutput("tableSoft"))
                                                                   )
                                                         )
                                                       )
                                              ),
                                              tabPanel("Open Scholarship",
                                                       sidebarLayout(
                                                         sidebarPanel(width = 3,
                                                                      selectInput("SelectOpenSci",
                                                                                  label = "Which practice do 
                                                                                    you want to access the publications for?",
                                                                                  choices = c(unique(OpenScienceMeta$`Open Science`))
                                                                      ),
                                                                      uiOutput("SelectOpenScience")
                                                         ),
                                                         mainPanel(width = 9,
                                                                   fluidRow(
                                                                     column(5, img(src='OpenScienceBar.jpg', width = "500px")),
                                                                     column(6, textOutput("selected_optionOpenSci"), DT::dataTableOutput("tableOpenSci"))
                                                                   ))
                                                       ))
                                  )
                         ), # Closes the tab called "Metadata"
                         
                         
                         # Tab: Steps --------------------------------------------------------------
                         
                         tabPanel("Steps", value = "WH",
                                  tabsetPanel(type = "tabs",
                                              tabPanel(width = 12,
                                                       "Aggregated Steps",
                                                       sidebarLayout(
                                                         sidebarPanel( width = 3,
                                                                       shiny::HTML("<h5><b>Explore the aggregated preprocessing 
                                                                       pipelines of all articles in a network fashion.</b><br><br>
                                                                       <p style = 'text-align: justify;'>Each node (circle) represents 
                                                                       a pre-processing step. The color of a node indicates the processing 
                                                                       group the step belongs to. <br><br>
                                                                       Steps performed in succession are connected by arrows, 
                                                                       these are edges. The wider the arrow, the higher the number
                                                                       of papers using this edge. The arrow points in the direction 
                                                                       of the step that is performed afterward. You can hover over 
                                                                       an arrow to see how many articles used this edge.<br><br>
                                                                       By hovering over a node, you can get its name. If you click on it, 
                                                                       you get its definition and how many articles used it. You can zoom 
                                                                       into the figure by hovering your cursor over the figure and scrolling.<br><br>
                                                                       To identify the edges of a specific preprocessing step, please 
                                                                       select it in the dropdown below. If you choose all, 
                                                                       you will see the edges of all preprocessing steps<br><br>"
                                                                       ),
                                                                       
                                                                       selectInput("Node_WP",
                                                                                   label   = "Explore edges of step:",
                                                                                   choices =  list('All' = list('All'),
                                                                                                   'Channel Rejection' = (c(nodes$Names_vis[nodes$Groups=='Channel_rejection'])),
                                                                                                   'Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                                                                   'Frequency Filter' = (c(nodes$Names_vis[nodes$Groups=='Frequency_filter'])),
                                                                                                   'Other' = (c(nodes$Names_vis[nodes$Groups=='Other'])),
                                                                                                   'Rejection Continous Data' = (c(nodes$Names_vis[nodes$Groups=='Rejection_continous_data'])),
                                                                                                   'Rejection Epoch' = (c(nodes$Names_vis[nodes$Groups=='Rejection_epoch'])),
                                                                                                   'Spatial Filter' = (c(nodes$Names_vis[nodes$Groups=='Spatial_filter']))),
                                                                                   selected = "All"
                                                                       ),
                                                                       
                                                                       sliderInput("Thr", "Threshold paper",
                                                                                   min = 0, max = 10,
                                                                                   value = 0
                                                                       ),
                                                                       shiny::HTML("<h5>Move the threshold to only see edges used by more papers than the threshold.</h5>"),
                                                         ),  # Closes sidebarPanel
                                                         mainPanel( width = 9,
                                                                    forceNetworkOutput(outputId = "WP", width = "100%", height = "700px")
                                                         )  # Closes the mainPanel
                                                       )  # Closes the sidebarLayout
                                              ),
                                              
                                              tabPanel(
                                                "Combination",
                                                sidebarPanel( width = 3,
                                                              shiny::HTML("<h5><b>Explore pairs of preprocessing steps used in 
                                                              combination. </b><br><br>
                                                              Select a step from the dropdown below to see which steps were 
                                                              used in conjunction with this step. <br><br></h5>"),
                                                              selectInput("selectDecisionYN",
                                                                          label   = "Explore combinations of step:",
                                                                          choices =  list('Channel Rejection' = (c(nodes$Names_vis[nodes$Groups=='Channel_rejection'])),
                                                                                          'Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                                                          'Frequency Filter' = (c(nodes$Names_vis[nodes$Groups=='Frequency_filter'])),
                                                                                          'Other' = (c(nodes$Names_vis[nodes$Groups=='Other'])),
                                                                                          'Rejection Continous Data' = (c(nodes$Names_vis[nodes$Groups=='Rejection_continous_data'])),
                                                                                          'Rejection Epoch' = (c(nodes$Names_vis[nodes$Groups=='Rejection_epoch'])),
                                                                                          'Spatial Filter' = (c(nodes$Names_vis[nodes$Groups=='Spatial_filter']))),
                                                                          selected = "RmChanManual"
                                                              ),
                                                              shiny::HTML("<h5>Lollipop plot of the number of processing steps use together with 
                                                              the step selected above. Selected step in red font. Color 
                                                                          indicates the preprocessing group.</h5>"),
                                                ),
                                                mainPanel(
                                                  fluidRow(
                                                    column(2),
                                                    column(6, plotOutput("plot_YN", height = 600, width = "100%")),
                                                    column(3)
                                                  )
                                                )
                                              ),
                                              
                                              tabPanel(
                                                "Order",
                                                sidebarPanel( width = 3,
                                                              shiny::HTML("<h5><b>Investigate the order of preprocessing steps.</b><br><br>
                                                              Select a preprocessing step from the dropdown below to see which 
                                                              preprocessing steps were performed AFTER the selected one.<br><br></h5>"),
                                                              selectInput("selectDecisionOR",
                                                                          label   = "Explore steps performed after step:",
                                                                          choices =  list('Channel Rejection' = (c(nodes$Names_vis[nodes$Groups=='Channel_rejection'])),
                                                                                          'Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                                                          'Frequency Filter' = (c(nodes$Names_vis[nodes$Groups=='Frequency_filter'])),
                                                                                          'Other' = (c(nodes$Names_vis[nodes$Groups=='Other'])),
                                                                                          'Rejection Continous Data' = (c(nodes$Names_vis[nodes$Groups=='Rejection_continous_data'])),
                                                                                          'Rejection Epoch' = (c(nodes$Names_vis[nodes$Groups=='Rejection_epoch'])),
                                                                                          'Spatial Filter' = (c(nodes$Names_vis[nodes$Groups=='Spatial_filter']))),
                                                                          selected = "RmChanManual"
                                                              ),
                                                              shiny::HTML("<h5>Lollipop plot of the number of processing steps 
                                                                          used after the selected step. Selected step in red font. 
                                                                          Color indicates the preprocessing group.</h5>"),
                                                ),
                                                mainPanel(
                                                  fluidRow(
                                                    column(2),
                                                    column(6, plotOutput("plot_OR", height = 600, width = "100%")),
                                                    column(3)
                                                  )
                                                )
                                              )
                                  )# closes the tabsetpanel
                         ),  # Closes tab calles "Steps"
                         
                         
                         
                         # Tab: Step:Options ---------------------------------------------------
                         tabPanel("Steps: Options", value = "fa",
                                  sidebarPanel( width = 3,
                                                shiny::HTML("<h5><b>Explore the distribution of options chosen by
                                                                     different articles.</b><br></h5>"),
                                                selectInput("selectGroup",
                                                            label   = "Explore the chosen options of step:",
                                                            choices =  c(unique(nodes_op$Groups_vis)),
                                                            selected = "RmChanManual"),
                                                shiny::HTML("<h5>Lollipop plot of the number of articles using
                                                         various options of the selected step.</h5>"),
                                                shiny::HTML("<h5><b>Caution:</b>If steps were performed more than
                                                                     once only the option of the last time the step was
                                                                     performed is displayed here. To see all chosen options,
                                                                     explore the <i>Individual Paper</i>. Moreover, each step
                                                                     is only characterized by one parameter, this is in many
                                                                     cases not sufficient to replicate the pipeline (e.g. we
                                                                     only visualize filter cut-offs in Hz). If you are interested
                                                                     in a specific study, please check for additional parameters
                                                                     or code within the publication.<br><br></h5>"),
                                                uiOutput("selectDecision"),
                                                
                                  ),
                                  mainPanel(
                                    fluidRow(
                                      column(5, plotOutput("plot_group_decision", height = 600, width = "100%")),
                                      column(7, textOutput("selected_decision")),
                                      column(7, DT::dataTableOutput("table"))
                                    ),
                                  )
                         ),
                         
                         
                         # Tab: Individual Paper ---------------------------------------------------
                         
                         tabPanel("Individual Paper", value = "IP",
                                  tabsetPanel(type = "tabs",
                                              tabPanel(
                                                "Step Visualisation",
                                                sidebarLayout(
                                                  sidebarPanel( width = 3,
                                                                shiny::HTML("<h5><b>Visualize the processing steps taken by a 
                                                                specific article.</b><br><br>
                                                                Select the code of the article in the dropdown below to get 
                                                                a visualization of the processing steps and to generate a 
                                                                table of their respective options that were employed by the 
                                                                selected article in the order they were performed. You can 
                                                                find the code of each study in the Database tab.<br></h5>"),
                                                                selectInput("selectPapers",
                                                                            label   = "Select article:",
                                                                            choices =  c(dat$Key),
                                                                            selected = c(dat$Key)[2], # 2nd, as first paper displayed incorrectly
                                                                ),
                                                                
                                                  ),  # Closes sidebarPanel
                                                  mainPanel( width =9,
                                                             fluidRow(
                                                               column(12, textOutput("selected_paper"), DT::dataTableOutput("table_step")),
                                                               column(12, plotOutput("plot", width = "100%"))
                                                             )
                                                  )  # Closes the mainPanel
                                                )
                                              ),
                                              
                                  )
                         ),  # Closes the second
                         # 
                         # 
                         # Tab: Your own pipeline --------------------------------------------------
                         
                         tabPanel("Your Own Pipeline", value = "DIY",
                                  sidebarLayout(
                                    sidebarPanel( width = 3,
                                                  shiny::HTML("<h5><b>Construct your preferred pipeline for mobile EEG data 
                                                  pre-processing</b><br><br>
                                                  Select the step you want to include with the dropdown below. You may also 
                                                  select a specific option or any of the available ones. You can add and 
                                                  delete steps with the respective buttons. <br><br>
                                                  You can count the number of studies that have used the same pipeline and 
                                                  obtain a table by clicking count. You may specify whether the order of 
                                                  their pipeline should be considered. If this option is disabled, the a
                                                  lgorithm will identify all articles that have employed the selected steps 
                                                  regardless of the order. <br></h5>"),
                                                  selectInput("selectStep_DIY",
                                                              label   = "Select the step you want to include",
                                                              choices =  list('Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                                              'Frequency Filter' = (c(nodes$Names_vis[nodes$Groups=='Frequency_filter'])),
                                                                              'Other' = (c(nodes$Names_vis[nodes$Groups=='Other'])),
                                                                              'Rejection Continous Data' = (c(nodes$Names_vis[nodes$Groups=='Rejection_continous_data'])),
                                                                              'Rejection Epoch' = (c(nodes$Names_vis[nodes$Groups=='Rejection_epoch'])),
                                                                              'Spatial Filter' = (c(nodes$Names_vis[nodes$Groups=='Spatial_filter']))),
                                                              selected = "RmChanManual"
                                                  ),
                                                  uiOutput("selectDecision_DIY"),
                                                  actionButton("add",
                                                               label = "Add",
                                                               icon = icon("arrow-circle-right", class = "fa-2x"),
                                                               width= "100px", height= "40px"
                                                  ),
                                                  actionButton("delete",
                                                               label = "Delete",
                                                               icon = icon("arrow-circle-left", class = "fa-2x"),
                                                               width= "100px", height= "40px"
                                                  ),
                                                  shiny::HTML("<h6>Click this Order button if you want to take the order 
                                                  of the steps into consideration.</h6>"),
                                                  materialSwitch(inputId = "order",
                                                                 label = "order",
                                                                 status = "success",
                                                                 right = T,
                                                                 value = T
                                                  ),
                                                  actionButton("count",
                                                               label = "Count",
                                                               icon = icon("arrow-circle-right", class = "fa-2x"),
                                                               width= "100px", height= "40px"
                                                  ),
                                                  downloadButton('download',"Download the table",
                                                                 width= "100px", height= "40px"
                                                  ),
                                                  shiny::HTML("<h5>Information here</h5>"),
                                    ),  # Closes sidebarPanel
                                    mainPanel(
                                      DT::DTOutput("table_DIY"),
                                      fluidRow(style = "height:100px;"),
                                      fluidRow(
                                        column(12, textOutput("counted_paper"))
                                      ),
                                      DT::DTOutput("table_DIY2"),
                                      #plotOutput("plot_DIY", width = "100%")
                                    ),  # Closes the mainPanel
                                  ),  # Closes the sidebarLayout
                         ),
                         
                         # Tab: About --------------------------------------------------------------
                         
                         tabPanel("About", value = "about",
                                  
                                  column(1),
                                  column(10,
                                         shiny::HTML("<h4><center>The METEOR project is based at the University of Oldenburg funded 
                                         by priority program <a href='https://www.meta-rep.uni-muenchen.de'> META-REP</a> 
                                         (SPP 2317). Meta-REP involves 15 individual projects with 50+ scholars analyzing 
                                         and optimizing replicability in the Behavioral, Social, and Cognitive Sciences.</h4><br>
                                          <h3><center>Our team</center></h3><br>")
                                  ),
                                  
                                  # TEAM BIO
                                  fluidRow(
                                    
                                    style = "height:50px;"),
                                  
                                  fluidRow(
                                    column(2),
                                    
                                    # Andrea
                                    column(2,
                                           div(class="panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "andrea2.jpg",
                                                              height = "70px")
                                                   ),
                                                   div(
                                                     tags$h5("Hildebrandt, Andrea, Prof. Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Professor for Psychological Methods and Statistics, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    # Stefan
                                    column(2,
                                           div(class="panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "stefan.jpg", 
                                                              height = "70px")
                                                   ),
                                                   div(
                                                     tags$h5("Debener, Stefan, Prof. Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Professor for Neuropsychology, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    # Carsten
                                    column(2,
                                           div(class="panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "carsten.jpg",
                                                              height = "70px")),
                                                   div(
                                                     tags$h5("Giessing, Carsten, Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Senior Scientist in Biological Psychology, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    
                                    # Christiane
                                    column(2,
                                           div(class="panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "christiane2.jpg",
                                                              height = "70px")),
                                                   div(
                                                     tags$h5("Thiel, Christiane, Prof. Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Professor for Biological Psychology, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    column(2)
                                    
                                  ),
                                  
                                  fluidRow(
                                    column(3),
                                    # Nadine
                                    column(2,
                                           div(class="panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "nadine.jpg",
                                                              height = "70px")),
                                                   div(
                                                     tags$h5("Jacobsen, Nadine, Dr. rer. nat."),
                                                     tags$h6( tags$i("Postdoctoral Fellow"))
                                                   ),
                                                   div(
                                                     "Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    # Daniel
                                    column(2,
                                           div(class="panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "Daniel.jpg",
                                                              height = "70px")),
                                                   div(
                                                     tags$h5("Kristanto, Daniel, PhD."),
                                                     tags$h6( tags$i("Postdoctoral Fellow"))
                                                   ),
                                                   div(
                                                     "Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    column(2,
                                           div(class="panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "cassie.jpg",
                                                              height = "70px")),
                                                   div(
                                                     tags$h5("Short, Cassie, PhD."),
                                                     tags$h6( tags$i("Postdoctoral Fellow"))
                                                   ),
                                                   div(
                                                     "Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    column(3)
                                  ),
                                  fluidRow(
                                    column(1),
                                    column(10,
                                         shiny::HTML("<h4><center><br>We would like to thank our student assistants 
                                         for their indispensable work: Suong Welp for her work coding all the articles and 
                                         Cosku Inceler for his work on the Shiny App. </h4><br>"),
                                          ),
                                          )
                                  
                         )  # Closes About tab
)
)