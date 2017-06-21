require(plotly) # interactive plot
require(robust)

# UI
tabScatterPlotUI <- function(id, label = "Comparing t-points") {
  ns <- NS(id)
  
  tagList(
    tagList(
      h4('Scatter plot')
    ),
    br(),
    
    fluidRow(
      column(5,
             uiOutput(ns('varSelMeas11')),
             radioButtons(
               ns('inSelMath1'),
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
             uiOutput(ns('varSelMeas12')),
             dataTableOutput(ns('outTabStats'))
      ),
      
      column(5,
             uiOutput(ns('varSelMeas21')),
             radioButtons(
               ns('inSelMath2'),
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
             uiOutput(ns('varSelMeas22'))
      )
    ),
    
    h4("Plot format"),
    fluidRow(
      column(4,
             numericInput(ns('inPlotScatterHeight'), 'Height [px]:', value = 1000, min = 100, width = '100px', step = 50),
             actionButton(ns('butGoScatter'), 'Plot!')),
      column(4,
             numericInput(ns('inPlotScatterWidth'), 'Width [%]:', value = 100, min = 10, max = 100, width = '100px', step = 10))
    ),
    
    br(),
    uiOutput(ns("plotInt_ui")),
    downPlotUI(ns('downPlotScatter'), "Download PDF")
  )
}


# SERVER
tabScatterPlot <- function(input, output, session, in.data) {
 
  # return column names of the main dt
  getDataNucCols <- reactive({
    cat(file = stderr(), 'getDataNucCols: in\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(colnames(loc.dt))
  })
  
  
  output$varSelMeas11 = renderUI({
    cat(file = stderr(), 'UI varSelMeas11\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'imNucCorrBg'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        ns('inSelMeas11'),
        'Select X-axis:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  output$varSelMeas12 = renderUI({
    cat(file = stderr(), 'UI varSelMeas12\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols) &&
        !(input$inSelMath1 %in% c('', '1 / '))) {
      locColSel = locCols[locCols %like% 'Intensity'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        ns('inSelMeas12'),
        'Select 2nd operand for X-axis:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  output$varSelMeas21 = renderUI({
    cat(file = stderr(), 'UI varSelMeas21\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'objCell_Intensity_MeanIntensity_imErkCor.*'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        ns('inSelMeas21'),
        'Select Y-axis:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  output$varSelMeas22 = renderUI({
    cat(file = stderr(), 'UI varSelMeas22\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols) &&
        !(input$inSelMath2 %in% c('', '1 / '))) {
      locColSel = locCols[locCols %like% 'objNuc_Intensity_MeanIntensity_imErkCor.*'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        ns('inSelMeas22'),
        'Select 2nd perand for Y-axis:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
}