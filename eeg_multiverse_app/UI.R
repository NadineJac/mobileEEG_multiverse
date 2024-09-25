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
library(bslib)
library(shinyBS)

shinyWidgets::shinyWidgetsGallery()

source("helper_function.R")


ui <- shinyUI(navbarPage(title = div(img(src="metarep.jpg", height = "80px"), img(src="uol.jpg", height = "80px") ), id = "navBar",
                         theme = "bootstrap.css",
                         collapsible = TRUE,
                         inverse = TRUE,
                         windowTitle = "Forking Path of mobile EEG analyses",
                         position = "fixed-top",
                         header = tags$style(
                           ".navbar-right { float: right !important; }",
                           "body {padding-top: 150px;}",
                           ".shiny-notification {
                               position:fixed;
                               top: calc(30%);
                               left: calc(30%);
                               height: calc(10%);
                               width: calc(10%);
                              font-size: 18px}"
                         ),
                         
                         # Tab: Info -------------------------------------------------------
                         tabPanel("Info >", value = "info",
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
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
                                                         column(12,
                                                                align = "left",
                                                                shiny::tags$h3("METEOR - Mastering The Oppressive number of forking paths unfolded by noisy and complex neural data",
                                                                               style = "color: #333; font-weight: normal;"),
                                                                shiny::tags$h4("This interactive shiny app allows you to investigate the multiverse of mobile EEG data preprocessing. 
                                                                               You can explore the preprocessing choices of experts. You can also construct your own preprocessing 
                                                                               pipeline and compare it to the ones in the database. We provide an exemplary preprocessing pipeline 
                                                                               from our lab as starting point.",
                                                                               style = "color: #333; font-weight: normal;"),
                                                                shiny::HTML("<br>"),
                                                                #shiny::tags$h3("Navigate to tab:", style = "color: #333;"),
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(6,  align = "left",
                                                                
                                                                div(shiny::actionButton("btn_MA", "Database", 
                                                                                        class = "btn-primary", width= '100%', style = "font-size: 20px")),
                                                                shiny::tags$h4("The multiverse has been identified by a literature review and expert survey. All information,
                                                             the preprocessing steps and their respective
                                                             options can be found here."), br(),
                                                             
                                                             div(shiny::actionButton("btn_MD", "Metadata", class = "btn-primary",  
                                                                                     class = "btn-primary", width= '100%', style = "font-size: 20px")),
                                                             shiny::tags$h4("Here you can find information on the EEG metadata of the articles."),br(),
                                                             
                                                             div(shiny::actionButton("btn_WH", "Steps", class = "btn-primary",  
                                                                                     class = "btn-primary", width= '100%', style = "font-size: 20px")),
                                                             shiny::tags$h4("Explore which preprocessing steps have been used and which
                                                             combinations and orders are common."), br(),
                                                             
                                                             div(shiny::actionButton("btn_fa", "Steps: Options", class = "btn-primary",  
                                                                                     class = "btn-primary", width= '100%', style = "font-size: 20px")),
                                                             shiny::tags$h4("Explore which options for the respective preprocessing
                                                             steps have been used by experts."), br(),
                                                             
                                                         ),
                                                         column(6, align = "left",
                                                                
                                                                div(shiny::actionButton("btn_IP", "Individual Pipeline", class = "btn-primary",  
                                                                                        class = "btn-primary", width= '100%', style = "font-size: 20px")),
                                                                shiny::tags$h4("Check out preprocessing pipelines and their chosen options
                                                             for individual articles and experts."), br(),br(),
                                                             
                                                             div(shiny::actionButton("btn_EP", "Exemplary Pipeline", class = "btn-primary",  
                                                                                     class = "btn-primary", width= '100%', style = "font-size: 20px")),
                                                             shiny::tags$h4("Check out an exemplary preprocessing pipeline from the Debener lab."), br(),
                                                             
                                                             div(shiny::actionButton("btn_DIY", "Your Pipeline", class = "btn-primary",  
                                                                                     class = "btn-primary", width= '100%', style = "font-size: 20px")),
                                                             shiny::tags$h4("Construct your own pipeline and compare it to the ones in the database."), br(),
                                                         ),
                                                           column(12, align = "center",
                                                                  shiny::tags$h4("Last updated: 2024-09-12"),
                                                         )
                                                       )
                                                ),
                                                
                                                # Tab: Background  -------------------------------------------------------
                                                
                                                tabPanel("Background", value = "background",
                                                         
                                                         #shinyjs::useShinyjs(),
                                                         
                                                         fluidRow(
                                                           column(12,
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
                                                                  ),
                                                       
                                                       
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
                         ),
                         # Tab: Database -----------------------------------------------------------
                         
                         tabPanel("Database >", value = "MA",
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
                                                tabPanel("Expert Survey", value = 'EI',
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             selectInput("variable", "Select a variable:", 
                                                                         choices = c(Questions$Question[c(3,5,7:13,16:20)],
                                                                                     "What is the basis of your pre-processing decisions?",
                                                                                     "If you were to follow the pipeline recommendations of your peer community, which peer characteristics would contribute to you agreeing to make pipeline choices that would not at first glance be your personal preferred choice?")
                                                             )
                                                           ),
                                                           mainPanel(
                                                             htmlOutput("selected_var"),
                                                             uiOutput("hist")
                                                           )
                                                         )
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
                         tabPanel("Metadata >", value = "MD",
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
                         tabPanel("Steps >", value = "WH",
                                  tabsetPanel(type = "tabs",
                                              tabPanel(
                                                "Aggregated",
                                                sidebarPanel( width = 3,
                                                              shiny::HTML("<h5><b>Compare steps used within the literature 
                                                              and by experienced users. </b><br><br>"),
                                                              shiny::HTML("<h5>Lollipop plot of the number of articles/experts
                                                              using a preprocessing step. Color indicates the preprocessing group.</h5>"),
                                                ),
                                                mainPanel(column(7, plotOutput("plot_combined", height = 300, width = "100%")),
                                                )
                                              ),
                                              
                                              tabPanel(width = 12,
                                                       "Network",
                                                       sidebarLayout(
                                                         sidebarPanel( width = 3,
                                                                       tags$div(
                                                                         class = "pull-right",
                                                                         shiny::HTML("<h5><b>Explore the aggregated preprocessing 
                                                                       pipelines of all articles/experts in a network fashion.</b>"),
                                                                       dropdownButton("Info", status = 'success', icon = icon('info'),
                                                                                      size = 'xs', inline = T, width = 400,
                                                                                      h5(strong('How to explore the aggregated pipelines')),
                                                                                      h5('Each node (circle) represents a pre-processing step. 
                                                                                        The color of a node indicates the processing  group the step belongs to.'),
                                                                                      h5('Steps performed in succession are connected by arrows, 
                                                                                         these are edges. The wider the arrow, the higher the number
                                                                                         of papers using this edge. The arrow points in the direction 
                                                                                         of the step that is performed afterward. You can hover over 
                                                                                         an arrow to see how many articles used this edge.'),
                                                                                      h5('By hovering over a node, you can get its name. If you click on it, 
                                                                                        you get its definition and how many articles/experts used it. You can zoom 
                                                                                        into the figure by hovering your cursor over the figure and scrolling.'),
                                                                                      h5(' To identify the edges of a specific preprocessing step, please 
                                                                                         select it in the dropdown below. If you choose all, 
                                                                                         you will see the edges of all preprocessing steps.') 
                                                                       )
                                                                       ),
                                                                       
                                                                       selectInput("Node_WP",
                                                                                   label   = "Explore edges of step:",
                                                                                   choices =  list('All' = list('All'),
                                                                                                   'Offline filter' = (c(nodes$Names_vis[nodes$Groups=='Offline filter'])),
                                                                                                   'Downsampling' = (c(nodes$Names_vis[nodes$Groups=='Downsampling'])),
                                                                                                   'Re-referencing' = (c(nodes$Names_vis[nodes$Groups=='Re-referencing'])),
                                                                                                   'Reject data' = (c(nodes$Names_vis[nodes$Groups=='Reject data'])),
                                                                                                   'Reject channels' = (c(nodes$Names_vis[nodes$Groups=='Reject channels'])),
                                                                                                   'Interpolate channels' = (c(nodes$Names_vis[nodes$Groups=='Interpolate channels'])),
                                                                                                   'Multi-step automated approach' = (c(nodes$Names_vis[nodes$Groups=='Multi-step automated approach'])),
                                                                                                   'Data decomposition' = (c(nodes$Names_vis[nodes$Groups=='Data decomposition'])),
                                                                                                   'Source estimation' = (c(nodes$Names_vis[nodes$Groups=='Source estimation'])),
                                                                                                   'Line noise correction' = (c(nodes$Names_vis[nodes$Groups=='Line noise correction'])),
                                                                                                   'Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                                                                   'Baseline Correction' = (c(nodes$Names_vis[nodes$Groups=='Baseline Correction'])),
                                                                                                   'Other' = (c(nodes$Names_vis[nodes$Groups=='Other']))),
                                                                                   selected = "All"
                                                                       ),
                                                                       # sliderInput("Thr_nd", "Threshold publications (step)",
                                                                       #             min = 0, max = 27,
                                                                       #             value = 0
                                                                       # ),
                                                                       # shiny::HTML("<h5>Move the threshold to increse size of steps used by more publications than the threshold.</h5>"),
                                                                       sliderInput("Thr", "Threshold publications (edge)",
                                                                                   min = 0, max = 10,
                                                                                   value = 0
                                                                       ),
                                                                       shiny::HTML("<h5>Move the threshold to only see edges used by more publications than the threshold.</h5>"),
                                                                       
                                                         ),  # Closes sidebarPanel
                                                         mainPanel( width = 9,
                                                                    tabsetPanel(type = "tabs",
                                                                                tabPanel("Literature review",
                                                                                         forceNetworkOutput(outputId = "WP", width = "100%", height = "700px")
                                                                                ),
                                                                                tabPanel("Experts",
                                                                                         forceNetworkOutput(outputId = "WP_crowd", width = "100%", height = "700px")
                                                                                ),
                                                                    )
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
                                                                          choices =  list(
                                                                            'Downsampling' = (c(nodes$Names_vis[nodes$Groups=='Downsampling'])),
                                                                            'Re-referencing' = (c(nodes$Names_vis[nodes$Groups=='Re-referencing'])),
                                                                            'Interpolate channels' = (c(nodes$Names_vis[nodes$Groups=='Interpolate channels'])),
                                                                            'Multi-step automated approach' = (c(nodes$Names_vis[nodes$Groups=='Multi-step automated approach'])),
                                                                            'Offline filter' = (c(nodes$Names_vis[nodes$Groups=='Offline filter'])),
                                                                            'Reject data' = (c(nodes$Names_vis[nodes$Groups=='Reject data'])),
                                                                            'Reject channels' = (c(nodes$Names_vis[nodes$Groups=='Reject channels'])),
                                                                            'Data decomposition' = (c(nodes$Names_vis[nodes$Groups=='Data decomposition'])),
                                                                            'Source estimation' = (c(nodes$Names_vis[nodes$Groups=='Source estimation'])),
                                                                            'Line noise correction' = (c(nodes$Names_vis[nodes$Groups=='Line noise correction'])),
                                                                            'Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                                            'Baseline Correction' = (c(nodes$Names_vis[nodes$Groups=='Baseline Correction'])),
                                                                            'Other' = (c(nodes$Names_vis[nodes$Groups=='Other']))),
                                                                          selected = ""
                                                              ),
                                                              shiny::HTML("<h5>Lollipop plot of the number of processing steps use together with 
                                                              the step selected above. Selected step in red font. Color 
                                                                          indicates the preprocessing group.</h5>"),
                                                ),
                                                mainPanel(
                                                  tabsetPanel(type = "tabs",
                                                              tabPanel("Literature review",
                                                                       fluidRow(
                                                                         column(2),
                                                                         column(6, plotOutput("plot_YN", height = 600, width = "100%")),
                                                                         column(3)
                                                                       )
                                                              ),
                                                              tabPanel("Experts",
                                                                       fluidRow(
                                                                         column(2),
                                                                         column(6, plotOutput("plot_YN_crowd", height = 600, width = "100%")),
                                                                         column(3)
                                                                       )
                                                              ),
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
                                                                          choices =  list(
                                                                            'Downsampling' = (c(nodes$Names_vis[nodes$Groups=='Downsampling'])),
                                                                            'Re-referencing' = (c(nodes$Names_vis[nodes$Groups=='Re-referencing'])),
                                                                            'Interpolate channels' = (c(nodes$Names_vis[nodes$Groups=='Interpolate channels'])),
                                                                            'Multi-step automated approach' = (c(nodes$Names_vis[nodes$Groups=='Multi-step automated approach'])),
                                                                            'Offline filter' = (c(nodes$Names_vis[nodes$Groups=='Offline filter'])),
                                                                            'Reject data' = (c(nodes$Names_vis[nodes$Groups=='Reject data'])),
                                                                            'Reject channels' = (c(nodes$Names_vis[nodes$Groups=='Reject channels'])),
                                                                            'Data decomposition' = (c(nodes$Names_vis[nodes$Groups=='Data decomposition'])),
                                                                            'Source estimation' = (c(nodes$Names_vis[nodes$Groups=='Source estimation'])),
                                                                            'Line noise correction' = (c(nodes$Names_vis[nodes$Groups=='Line noise correction'])),
                                                                            'Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                                            'Baseline Correction' = (c(nodes$Names_vis[nodes$Groups=='Baseline Correction'])),
                                                                            'Other' = (c(nodes$Names_vis[nodes$Groups=='Other']))),
                                                                          selected = ""
                                                              ),
                                                              shiny::HTML("<h5>Lollipop plot of the number of processing steps 
                                                                          used after the selected step. Selected step in red font. 
                                                                          Color indicates the preprocessing group.</h5>"),
                                                ),
                                                mainPanel(
                                                  tabsetPanel(type = "tabs",
                                                              tabPanel("Literature review",
                                                                       fluidRow(
                                                                         column(2),
                                                                         column(6, plotOutput("plot_OR", height = 600, width = "100%")),
                                                                         column(3)
                                                                       )
                                                              ),
                                                              tabPanel("Experts",
                                                                       fluidRow(
                                                                         column(2),
                                                                         column(6, plotOutput("plot_OR_crowd", height = 600, width = "100%")),
                                                                         column(3)
                                                                       )
                                                              ),
                                                  )
                                                )
                                              )
                                  )# closes the tabsetpanel
                         ),  # Closes tab calles "Steps"
                         
                         
                         
                         # Tab: Step:Options ---------------------------------------------------
                         tabPanel("Steps: Options >", value = "fa",
                                  sidebarPanel( width = 3,
                                                shiny::HTML("<h5><b>Explore the distribution of options chosen by
                                                                     different articles.</b><br></h5>"),
                                                selectInput("selectGroup",
                                                            label   = "Explore the chosen options of step:",
                                                            choices =  list(
                                                              'Offline filter' = (c(nodes$Names_vis[nodes$Groups=='Offline filter'])),
                                                              'Downsampling' = (c(nodes$Names_vis[nodes$Groups=='Downsampling'])),
                                                              'Re-referencing' = (c(nodes$Names_vis[nodes$Groups=='Re-referencing'])),
                                                              'Reject data' = (c(nodes$Names_vis[nodes$Groups=='Reject data'])),
                                                              'Reject channels' = (c(nodes$Names_vis[nodes$Groups=='Reject channels'])),
                                                              'Interpolate channels' = (c(nodes$Names_vis[nodes$Groups=='Interpolate channels'])),
                                                              'Multi-step automated approach' = (c(nodes$Names_vis[nodes$Groups=='Multi-step automated approach'])),
                                                              'Data decomposition' = (c(nodes$Names_vis[nodes$Groups=='Data decomposition'])),
                                                              'Source estimation' = (c(nodes$Names_vis[nodes$Groups=='Source estimation'])),
                                                              'Line noise correction' = (c(nodes$Names_vis[nodes$Groups=='Line noise correction'])),
                                                              'Epoching' = (c(nodes$Names_vis[nodes$Groups=='Epoching'])),
                                                              'Baseline Correction' = (c(nodes$Names_vis[nodes$Groups=='Baseline Correction'])),
                                                              'Other' = (c(nodes$Names_vis[nodes$Groups=='Other']))),
                                                            selected = nodes$Names_vis[1]),
                                                shiny::HTML("<h5>Lollipop plot of the number of articles using
                                                         various options of the selected step.</h5>"),
                                                shiny::HTML("<h5><b>Caution:</b> If steps were performed more than
                                                                     once only the option of the last time the step was
                                                                     performed is displayed here. To see all chosen options,
                                                                     explore the <i>Individual Pipeline</i>. Moreover, each step
                                                                     is only characterized by one parameter, this is in many
                                                                     cases not sufficient to replicate the pipeline (e.g. we
                                                                     only visualize filter cut-offs in Hz). If you are interested
                                                                     in a specific study, please check for additional parameters
                                                                     or code within the publication.<br><br></h5>"),
                                                
                                                
                                  ),
                                  mainPanel(#
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Literature review",
                                                         fluidRow(
                                                           column(5, plotOutput("plot_group_decision", height = 600, width = "100%")),
                                                           column(7, uiOutput("selectDecision")),
                                                           column(7, textOutput("selected_decision")),
                                                           column(7, DT::dataTableOutput("table_op"))
                                                         ),
                                                ),
                                                tabPanel("Experts",
                                                         column(5, plotOutput("plot_group_decision_crowd", height = 600, width = "100%")),
                                                         column(7,uiOutput("selectDecision_crowd")),
                                                         column(5, textOutput("selected_decision_crowd")),
                                                         #column(3, DT::dataTableOutput("table_crowd"))
                                                ),
                                    )
                                  )
                         ),
                         
                         # Tab: Individual Pipeline -----------------------------------------------------
                         tabPanel("Individual Pipeline >", value = "IP",
                                  tabsetPanel(type = "tabs",
                                              tabPanel(width = 12,
                                                       "Literature review",
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
                                                                            selected = "AFFTQ2LH",
                                                                            #hardcoded tp select Scanlon, because used as exemplary pipeline, di just display first entry, use: c(dat$Key)[1], 
                                                                ),
                                                                
                                                         ),  # Closes sidebarPanel
                                                         mainPanel( width =9,
                                                                    fluidRow(
                                                                      column(12, textOutput("selected_paper"), DT::dataTableOutput("table_step")),
                                                                      column(12, plotOutput("plot", width = "100%"))
                                                                    )
                                                         )  # Closes the mainPanel
                                                       ),
                                              ),
                                              tabPanel(width = 12,
                                                       "Experts",
                                                       sidebarLayout(
                                                         sidebarPanel( width = 3,
                                                                       shiny::HTML("<h5><b>Visualize the processing steps taken by a 
                                                                specific expert.</b><br><br>
                                                                Select the code of the expert in the dropdown below to get 
                                                                a visualization of the processing steps and to generate a 
                                                                table of their respective options that were employed by the 
                                                                selected expert in the order they were performed. You can 
                                                                find the code of each expert in the Database tab.<br></h5>"),
                                                                selectInput("selectExpert",
                                                                            label   = "Select expert:",
                                                                            choices =  c(dat_crowd$Key),
                                                                            selected = c(dat_crowd$Key)[1], 
                                                                ),
                                                                
                                                         ),  # Closes sidebarPanel
                                                         mainPanel( width =9,
                                                                    fluidRow(
                                                                      column(12, textOutput("selected_paper_crowd"), DT::dataTableOutput("table_step_crowd")),
                                                                      column(12, plotOutput("plot_crowd", width = "100%"))
                                                                    )
                                                         )  # Closes the mainPanel
                                                       ),
                                              )
                                  )
                         ),  # Closes the second
                         
                         # Tab: Exemplary Pipeline ---------------------------------------------------
                         tabPanel("Exemplary Pipeline >", value = "EP",
                                  sidebarLayout(
                                    sidebarPanel( width = 3,
                                                  shiny::HTML("<h5><b>Exemplary preprocessing pipeline.</b><br><br>
                                                                Exemplary EEG preprocessing pipeline to investigate 
                                                              a P3 during walking from the Debener Lab as used in 
                                                              <a href='https://doi.org/10.1111/ejn.15037'>Scanlon 
                                                              et al. (2021)</a>.<br><br>"),
                                                  div(shiny::actionButton("btn_IPexp", "Check individual pipeline Scanlon et al. (2021)", class = "btn-primary",  
                                                                          class = "btn-primary", width= '100%', style = "font-size: 14px")),
                                                  shiny::HTML("<br>Assuming continuous, multichannel EEG, the following steps may be performed 
                                                              in succession to attenuate artifacts. This exemplary pipeline is a starting point
                                                              and not necessarily the optimal preprocessing pipeline for your dataset. 
                                                              It reflects our experiences and opinions with passive mobile EEG data
                                                              captured during motion. This is no substitute for the empirical investigation of 
                                                              preprocessing choices on ERP results. We propose evaluating the preprocessing 
                                                              pipeline on a dataset with similar EEG data characteristics, such as a pilot dataset
                                                              but not the dataset that will be subject to statistical evalution.<br><br>
                                                              "),
                                                  shiny::HTML("<b>Legend:</b></h5>"),
                                                  bsCollapse(id = "LegendICA", #open = c("Panel 1", "Panel 3"),
                                                             bsCollapsePanel("Recommended step for interim data", style = "success"),
                                                             bsCollapsePanel("Optional step for interim data", style = "warning"),
                                                             bsCollapsePanel("Recommended step", style = "primary"),
                                                             bsCollapsePanel("Optional step", style = "info")
                                                  )
                                    ),  # Closes sidebarPanel
                                    mainPanel( width =8,
                                               img(src='EEGraw.png', align = "left", width = "80%"),
                                               fluidRow(#add flowchart here
                                                 column(5,
                                                        shiny::HTML("<h5 style='text-align:center'><b>Preprocessing for ICA </b></h5>"), # 
                                                        
                                                         bsCollapse(id = "collapse_EP_ICA", #open = c("Panel 1", "Panel 3"),
                                                                   bsCollapsePanel("High-Pass filter, interim data", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b>Offline filter > High-pass filter > 1Hz <br>
                                                                            <b>After:</b> Channel removal<br>
                                                                            <b>Before:</b> NA<br><br>
                                                                            Removal of DC component and slow drifts. By choosing separate high and low-pass filters, 
                                                                            you filter order can be optimized according to your cut-off frequency.
                                                                            "),
                                                                            style = "success"),
                                                                   bsCollapsePanel("Low Pass filter, interim data", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Offline filter > Low-pass filter > 40 Hz<br>
                                                                            <b>After:</b> Downsampling (only if no anti-aliasing is incorporated)<br>
                                                                            <b>Before:</b> NA<br><br>
                                                                            Removal of high frequency power can reduce their impact on the ICA decomposition. 
                                                                            If ICA is used to identify muscular artifacts a sufficiently high cut-off is advised, 
                                                                            otherwise muscular artifacts will be difficult to identify.
                                                                            "),
                                                                            style = "success"),
                                                                   bsCollapsePanel("Channel rejection, interim data", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Reject Channels > Numerical criterion<br>
                                                                            <b>After:</b> ICA, re-referencing<br>
                                                                            <b>Before:</b> High-pass filter<br><br>
                                                                            Bad channels, i.e. channels with abnormal signal quality, can have strong negative impact 
                                                                            on multichannel computations like ICA.
                                                                            High-pass filtering before bad channel rejection is important because it removes slow drifts.
                                                                            "),
                                                                            style = "success"),
                                                                   bsCollapsePanel("Downsampling (optional), interim data", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Downsampling > 250Hz<br>
                                                                            <b>After:</b> Low-pass filter (only if no anti-aliasing is incorporated)<br>
                                                                            <b>Before:</b> Computationally intensive preprocessing, e.g. ICA<br><br>
                                                                            Downsampling decreases computational costs and may be performed before ICA. 
                                                                            If your downsampling algorithm has no anti-aliasing, low-pass filter below Nyquist frequency (half of the new sampling rate). Anti-aliasing prevents higher frequencies, which cannot be presented accurately with a lower sampling rate, from distorting your signal.
                                                                            "),
                                                                            style = "warning"),
                                                                   bsCollapsePanel("Data segmentation (optional), interim data", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Epoching > 0 to 1000 ms<br>
                                                                            <b>After:</b> NA<br>
                                                                            <b>Before:</b> rejection of data points<br><br>
                                                                            Extract consecutive epochs or event-related epochs, so epochs containing abnormal, 
                                                                            non-sterotypical values can be discarded later and will not influence ICA decomposition. 
                                                                            Consecutive epochs will contain more data points and may lead to better decomposition 
                                                                            but will also contain activity/artifacts unrelated to the task-performance.
                                                                            "),
                                                                            style = "warning"),
                                                                   bsCollapsePanel("Artifact rejection: Rejection of time points, interim data", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Reject data > numerical criterion<br>
                                                                            <b>After:</b> NA<br>
                                                                            <b>Before:</b> ICA<br><br>
                                                                            Abnormal, non-sterotypical signal may impede ICA decomposition and should be removed beforehand. 
                                                                            You can either epoch your data and reject epoch or use rejection of timepoints 
                                                                            as incorporated into AMICA. Ensure to keep sufficient timepoints for ICA decomposition 
                                                                            (rule of thumb: at least 20*(number of ICs)^2). 
                                                                            "),
                                                                            style = "success"),
                                                                   bsCollapsePanel("Artifact correction: ICA decomposition, interim data", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Data decomposition > ICA decomposition <br>
                                                                            <b>After:</b> High-pass filter (around 1 Hz), rejection of time-points<br>
                                                                            <b>Before:</b> Back-projection, dipole fitting and IC selection<br><br>
                                                                            Both AMICA and extended Infomax may achieve good source separation which may be 
                                                                            helpful to disentangle brain signal from artifacts and brain signal from each other. 
                                                                            "),
                                                                            style = "success")
                                                        )
                                                 ),
                                                 column(5,
                                                        shiny::HTML("<h5 style='text-align:center'><b>&rarr; Back-projection of ICA weights to continous EEG</b></h5>"),
                                                        bsCollapse(id = "collapse_EP", #open = c("Panel 1", "Panel 3"),
                                                                   bsCollapsePanel("Artifact correction: IC selection ", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Data decomposition > IC selection > automatic: IC label<br>
                                                                            <b>After:</b> ICA<br>
                                                                            <b>Before:</b> NA<br><br>
                                                                            We recommend using automatic IC selection mathods but strongly advise to check their performance. 
                                                                            Experienced users may disagree with automatic classification. 
                                                                            If you deviate from the automatic classification document your choices and the reason so results 
                                                                            remain reproducible. 
                                                                            "),
                                                                                   style = "primary"),
                                                                   bsCollapsePanel("Temporal filtering", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b>  Offline filter > High-pass filter > 0.3 Hz & Offline filter > Low-pass filter > 40 Hz<br>
                                                                            <b>After:</b> NA<br>
                                                                            <b>Before:</b> NA<br><br>
                                                                             Use offline filters to attenuate the power of frequencies not generating your 
                                                                            ERP. Higher high-pass filter cut-offs can remove drifts but may affect your 
                                                                            morphology. In any case the impact of temporal filter on ERP morphology should be 
                                                                            carefully evaluated (<a href='http://dx.doi.org/10.1016/j.jneumeth.2014.08.002'>Widman et al., 2015</a>). 
                                                                            "),
                                                                                   style = "primary"),
                                                                   bsCollapsePanel("Channel Interpolation (optional)",
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Interpolate channels > spherical<br>
                                                                            <b>After:</b> Channel removal<br>
                                                                            <b>Before:</b> Re-referencing (if removed channels may distort results,
                                                                            e.g., common average re-reference); Aggregation of data across datasets.<br><br>
                                                                            Replacement of rejected channels greatly facilitates aggregation actross individuals. 
                                                                            "),
                                                                                   style = "info"),
                                                                   bsCollapsePanel("Re-referencing (optional)",
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Re-referencing > TP9 & Tp10/average mastoids<br>
                                                                            <b>After:</b> Channel removal<br>
                                                                            <b>Before:</b> NA <br><br>
                                                                            Re-referencing to average mastoids can make P3 morphologies easier to identify.
                                                                            However, ths may not neccesarily be the best reference for your data.
                                                                            "),
                                                                            style = "info"),
                                                                   bsCollapsePanel("Data segmentation", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Epoching > -200 to 800 ms<br>
                                                                            <b>After: </b>NA <br>
                                                                            <b>Before: </b>NA <br><br>
                                                                            Epoch duration should capture ar reasonalble of prestiumulus data and 
                                                                            allow evaluation ERP morphologies and a return of the signal to baseline.
                                                                             "),
                                                                                   style = "primary"),#primary, danger, warning, info, or success)
                                                                   bsCollapsePanel("Baseline correction (optional)", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Baseline correction > subtract mean > -200 to 0 ms<br>
                                                                            <b>After:</b> Epoching<br>
                                                                            <b>Before:</b> NA<br><br>
                                                                            Baseline correction is common but not necessary depending of the high-pass filter. 
                                                                            There are hints that it can even be detrimental 
                                                                            (<a href='https://www.nature.com/articles/s41598-023-27528-0'>Delorme, 2023</a>).
                                                                            Ensure that your baseline does not contain task-related activity. 
                                                                            A baseline of 200 ms should provide a stable estimate of the baseline voltage.
                                                                            "),
                                                                            style = "info"),#primary, danger, warning, info, or success)
                                                                   bsCollapsePanel("Artifact removal: Rejection of time points ", 
                                                                                   shiny::HTML(
                                                                                     "<b>App:</b> Reject data > numerical criterion > standard deviation threshold data<br>
                                                                            <b>After:</b> NA<br>
                                                                            <b>Before:</b> NA<br><br>
                                                                            Very likely artifact attenuation is not perfect, therefore it can be beneficial to reject 
                                                                            epochs with residual artifacts, for instance by searching for abnormal values. 
                                                                        "),
                                                                            style = "primary")#primary, danger, warning, info, or success)
                                                        )
                                                        
                                                        # img(src='preprocessing_table.png', align = "left", width = "1000px")
                                                        
                                                 )
                                               )
                                    )  # Closes the mainPanel
                                    
                                  ),
                         ),  # Closes the second
                         
                         # Tab: Your own pipeline --------------------------------------------------
                         
                         tabPanel("Your Pipeline", value = "DIY",
                                  sidebarLayout(
                                    sidebarPanel( width = 3,
                                                  tags$div(
                                                    class = "pull-right",
                                                    # tags$li(
                                                    #   class = "dropdown",
                                                    #tags$style(".main-header {min-height: 75px}"),
                                                    shiny::HTML("<h5><b>Construct your preferred pipeline for mobile EEG data 
                                                  pre-processing</b>"),
                                                  dropdownButton("Info", status = 'success', icon = icon('info'),
                                                                 size = 'xs', inline = T, width = 400,
                                                                 h5(strong('How to construct your preprocessing pipeline')),
                                                                 h5('Select the step you want to include with the dropdown below. You may also 
                                                                    select a specific option or any of the available ones. 
                                                                    In the comment field you can highlight whether a step was only performed 
                                                                    on a dataset optimized for ICA decomposition (interim data) or any other information, 
                                                                    such as version numbers, chosen parameters, to make steps reproducibe. 
                                                                    The comment field in not considered when searching for similar pipelines.You can add and 
                                                                    delete steps to the end of the preprocessing pipeline with the respective buttons.
                                                                    You can download your table as CSV with the download button. 
                                                                    You can reorder the steps via drag-and-drop if you click in the number of a step.'), 
                                                                 h5('Find studies or experts using similar pipelines by clicking search. 
                                                                   You can either search for pipelines using the exact same order, consecutively
                                                                   (with the possibility of other steps in-between) or without regarding the order of steps.
                                                                   Publications using the specified pipeline can be downloaded with the button on top of the table.'),
                                                                 h5('The order of the preprocessing steps is extended from the ', 
                                                                    em('Agreed Reprting Template for ERP Methodologyh (ARTEM-IS)'), '. Visit to get more information'),
                                                                 tags$a("Visit ARTEM-IS webapp",
                                                                        target="_blank",
                                                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                                        href = "https://artemis.incf.org/"),
                                                                 h5(strong('How to evaluate your preprocessing pipeline')),
                                                                 tags$a("Download our primer [pdf]",
                                                                        target="_blank",
                                                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                                        href = "primer_evaluation_EEGpreprocessing.pdf")
                                                                 
                                                  )
                                                  ),
                                                  selectInput("selectGroup_DIY",
                                                              label   = "Select the group of the step you want to include",
                                                              choices =  unique(nodes$Groups),
                                                              selected = unique(nodes$Groups)[1],
                                                              selectize = FALSE,
                                                              size = "13"
                                                  ),
                                                  
                                                  uiOutput("selectStep_DIY"),
                                                  htmlOutput("selectedStep_DIY"),
                                                  
                                                  uiOutput("selectDecision_DIY"),
                                                  
                                                  textInput("selectComment_DIY", 
                                                            label = "Add a comment", 
                                                            # value = "", 
                                                            # width = NULL, 
                                                            placeholder = "Add another option; 'interim data' for ICA-only steps; info needed for reproduction"
                                                  ),
                                                  actionButton("add",
                                                               label = "Add",
                                                               icon = icon("arrow-circle-right", class = "fa-2x"),
                                                               width= "120px", height= "50px"
                                                  ),
                                                  actionButton("delete",
                                                               label = "Delete",
                                                               icon = icon("arrow-circle-left", class = "fa-2x"),
                                                               width= "120px", height= "50px"
                                                  ),
                                                  downloadButton('download',"Download table",
                                                                 icon = icon("download", class = "fa-2x"),
                                                                 width= "120px", height= "50px" 
                                                  ),
                                                  shiny::HTML("<h6><br></h6>"),
                                                  radioButtons("order", 
                                                               label = "Search for similar pipelines: Order of steps", 
                                                               choices = c("Same order","Consecutive",  "Any order"), 
                                                               selected = "Any order", inline = FALSE,
                                                               width = NULL),
                                                  actionButton("count",
                                                               label = "Search pipelines",
                                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                               width= "180px", height= "100%"
                                                  ),
                                                  
                                    ),  # Closes sidebarPanel
                                    mainPanel(
                                      #DT::
                                      DTOutput("table_DIY"),
                                      
                                      fluidRow(
                                        column(12, htmlOutput("counted_paper_crowd")),
                                        column(12, htmlOutput("counted_paper"))
                                      ),
                                      DT::DTOutput("table_DIY2"),
                                      
                                    ),  # Closes the mainPanel
                                  ),  # Closes the sidebarLayout
                         )
                         
                         
))