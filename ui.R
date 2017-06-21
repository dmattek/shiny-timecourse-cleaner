library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/

shinyUI(fluidPage(useShinyjs(), # Include shinyjs
                        
                        # Application title
                        titlePanel = "Timecourse Inspector",
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            h4("Load data files"),
                            
                            fileInput(
                              'inFileLoadNuc',
                              'Select file (e.g. tCoursesSelected.csv) and press "Load Data"',
                              accept = c('text/csv', 'text/comma-separated-values,text/plain')
                            ),
                            actionButton("inButLoadNuc",  'Load Data'),
                            
                            # fileInput(
                            #   'inFileLoadStim',
                            #   'Select Stimulation File (e.g. stimT.csv)',
                            #   accept = c('text/csv', 'text/comma-separated-values,text/plain')
                            # ),
                            # actionButton("inButLoadStim", 'Load Stimulation'),
                            
                            actionButton('inDataGen1', 'Generate artificial dataset'),
                            actionButton("inButReset", "Reset file input"),
                            br(),
                            tags$hr(),
                            uiOutput('varSelSite'),
                            uiOutput('varSelTrackLabel'),
                            uiOutput('varSelTime'),
                            checkboxInput('chBhighlightTraj', 'Highlight trajectories?', FALSE),
                            uiOutput('varSelHighlight'),
                            tags$hr(),
                            checkboxInput('inRobustFit', 'Robust Linear Regression', value = TRUE, width = '100px'),
                            numericInput('inBandWidth', 'Width of selection band', min = 0, step = 0.1, value = 1, width = '100px'),
                            textOutput('outTextCellRatio'),
                            tags$hr(),
                            downloadButton('downloadDataClean', 'Download cleaned data'),
                            downloadButton('downloadDataMarked', 'Download original data with mid.in column')
                            
                          ),
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Scatter plot", 
                                       br(),
                                       fluidRow(
                                         column(5,
                                                uiOutput('varSelMeas11'),
                                                radioButtons(
                                                  'inSelMath1',
                                                  'Operation:',
                                                  c(
                                                    'None' = '',
                                                    'Divide' = " / ",
                                                    'Sum' = " + ",
                                                    'Multiply' = " * ",
                                                    'Subtract' = ' - ',
                                                    '1/x' = "1 / "
                                                  )
                                                ),
                                                uiOutput('varSelMeas12'),
                                                dataTableOutput('outTabStats')
                                         ),
                                         
                                         column(5,
                                                uiOutput('varSelMeas21'),
                                                radioButtons(
                                                  'inSelMath2',
                                                  'Operation:',
                                                  c(
                                                    'None' = '',
                                                    'Divide' = " / ",
                                                    'Sum' = " + ",
                                                    'Multiply' = " * ",
                                                    'Subtract' = ' - ',
                                                    '1/x' = "1 / "
                                                  )
                                                ),
                                                uiOutput('varSelMeas22')
                                         )
                                       ),
                                       
                                       h4("Plot format"),
                                       fluidRow(
                                         column(4,
                                                numericInput('inPlotScatterHeight', 'Height [px]:', value = 1000, min = 100, width = '100px', step = 50),
                                                actionButton('butGoScatter', 'Plot!')),
                                         column(4,
                                                numericInput('inPlotScatterWidth', 'Width [%]:', value = 100, min = 10, max = 100, width = '100px', step = 10))
                                       ),
                                       
                                       br(),
                                       checkboxInput('chBplotScatterInt', 'Interactive Plot?'),
                                       uiOutput("plotInt_ui"),
                                       downPlotUI('downPlotScatter', "Download PDF")
                                       
                              ), 
                              tabPanel("Time courses", 
                                       br(),
                                       
                                       uiOutput('varSelGroup'),
                                       br(),
                                       
                                       uiOutput('varSelMeas31'),
                                       radioButtons(
                                         'inSelMath3',
                                         'Operation:',
                                         c(
                                           'None' = '',
                                           'Divide' = " / ",
                                           'Sum' = " + ",
                                           'Multiply' = " * ",
                                           'Subtract' = ' - ',
                                           '1/x' = "1 / "
                                         )
                                       ),
                                       uiOutput('varSelMeas32'),
                                       
                                       h4("Plot format"),
                                       fluidRow(
                                         column(4,
                                                numericInput('inPlotTrajFacetNcol', '#Columns:', value = 4, min = 1, width = '100px', step = 1)),
                                         column(4,
                                                numericInput('inPlotTrajHeight', 'Height [px]:', value = 600, min = 100, width = '100px', step = 50)),
                                         column(4,
                                                numericInput('inPlotTrajWidth', 'Width [%]:', value = 100, min = 10, max = 100, width = '100px', step = 10))
                                       ),
                                       actionButton('butGoTraj', 'Plot!'),
                                       
                                       br(),
                                       uiOutput('uiPlotTraj')
                              )
                            )
                          )
                        )
                        
))
