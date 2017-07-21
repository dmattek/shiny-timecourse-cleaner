# 2017-02-26
# This is a follow-up to Stephen McDaniel's response to Shiny Google Groups thread.
#
# Main issue:
#   By and large my Shiny apps process data loaded from a file. However, for
#   testing and/or demonstration purposes it is often convenient to include
#   an option that would generate an equivalent dataset.
#
# In this example, we have two buttons that generate random numbers from
# Gaussian and Poisson distributions, respectively. Additionally, we have
# an option to load a single-column text file with numbers. A histogram
# will be plotted for any of these datasets.
#
# The code relies on:
# 1. Stephen McDaniel's solution to track usage of the action buttons
#    and determine which data to utilize.
#
# 2. ShinyJS library by Dean Attali to reset the state of fileInput
#    such that the file with the same name can be re-loaded consecutively.
#
# Original problem posted by me on Google Groups:
# https://groups.google.com/d/msg/shiny-discuss/9yG7hPMT5Qc/Ujdh6L87FQAJ
#
# Discussion on the topic initiated by Stephen McDaniel
# https://groups.google.com/d/msg/shiny-discuss/gkeuyPAZndM/rLV_L-cvFgAJ
#
# Steven's code:
# https://github.com/Stephen-McDaniel/reactives-used-if-else-logic-shiny
#
#
# Program: plotTwoSources.R
#    Data: randomly generated or loaded from a text file with ONE column (with or without header)
#
# License: MIT License
# Attribution, package authors for shiny on CRAN.

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(plotly)
library(DT)

require(data.table)
require(dplyr)
require(ggplot2)
require(robust)
require(Hmisc)

options(shiny.maxRequestSize=30*1024^2)

###### SERVER
shinyServer(function(input, output, session) {
  
  # this is to delay renderUI display
  # from: http://stackoverflow.com/questions/20490619/delayed-execution-in-r-shiny-app
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
  # This is only set at session start
  # we use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of inDataGen1,2 actionButton is the number of times they were pressed
    dataGen1     = isolate(input$inDataGen1),
    dataLoadNuc  = isolate(input$inButLoadNuc)
    #dataLoadStim = isolate(input$inButLoadStim)
  )
  
  # This button will reset the inFileLoad
  observeEvent(input$inButReset, {
    reset("inFileLoadNuc")  # reset is a shinyjs function
    #reset("inButLoadStim")  # reset is a shinyjs function
  })
  
  # generate random dataset 1
  dataGen1 <- eventReactive(input$inDataGen1, {
    cat("dataGen1\n")
    
    return(userDataGen())
  })
  
  # load main data file
  dataLoadNuc <- eventReactive(input$inButLoadNuc, {
    cat("dataLoadNuc\n")
    locFilePath = input$inFileLoadNuc$datapath

    counter$dataLoadNuc <- input$inButLoadNuc - 1
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(fread(locFilePath))
    }
  })
  
  # load stimulation pattern
  # dataLoadStim <- eventReactive(input$inButLoadStim, {
  #   cat("dataLoadStim\n")
  #   locFilePath = input$inFileLoadStim$datapath
  #   counter$dataLoadStim <- input$inButLoadStim - 1
  #   
  #   return(fread(locFilePath))
  # })
  
  dataInBoth <- reactive({
    # Without direct references to inDataGen1,2 and inFileLoad, inDataGen2
    #    does not trigger running this reactive once inDataGen1 is used.
    # This is one of the more nuanced areas of reactive programming in shiny
    #    due to the if else logic, it isn't fetched once inDataGen1 is available
    # The morale is use direct retrieval of inputs to guarantee they are available
    #    for if else logic checks!
    
    locInGen1 = input$inDataGen1
    locInLoadNuc = input$inButLoadNuc
    #locInLoadStim = input$inButLoadStim
    
    cat(
      "dataInBoth\ninGen1: ",
      locInGen1,
      "   prev=",
      isolate(counter$dataGen1),
      "\ninDataNuc: ",
      locInLoadNuc,
      "   prev=",
      isolate(counter$dataLoadNuc),
#      "\ninDataStim: ",
#      locInLoadStim,
#      "   prev=",
#      isolate(counter$dataLoadStim),
      "\n"
    )
    
    # isolate the checks of counter reactiveValues
    # as we set the values in this same reactive
    if (locInGen1 != isolate(counter$dataGen1)) {
      cat("dataInBoth if inDataGen1\n")
      dm = dataGen1()
      # no need to isolate updating the counter reactive values!
      counter$dataGen1 <- locInGen1
    } else if (locInLoadNuc != isolate(counter$dataLoadNuc)) {
      cat("dataInBoth if inDataLoadNuc\n")
      dm = dataLoadNuc()
      # no need to isolate updating the counter reactive values!
      counter$dataLoadNuc <- locInLoadNuc
    } else {
      cat("dataInBoth else\n")
      dm = NULL
    }
    return(dm)
  })
  
  # return column names of the main dt
  getDataNucCols <- reactive({
    cat(file = stderr(), 'getDataNucCols: in\n')
    loc.dt = dataInBoth()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(colnames(loc.dt))
  })
  
  # return dt with an added column with unique track object label
  dataMod <- reactive({
    cat(file=stderr(), 'dataMod\n')
    loc.dt = dataInBoth()
    
    if(is.null(loc.dt))
      return(NULL)
    
    loc.dt[, trackObjectsLabelUni := paste(sprintf("%03d", get(input$inSelSite)),
                                           sprintf("%04d", get(input$inSelTrackLabel)),
                                           sep = "_")]

    return(loc.dt)
  })
  
  # return all unique track object labels (created in dataMod)
  # This will be used to display in UI for trajectory highlighting
  getDataTrackObjLabUni <- reactive({
    cat(file = stderr(), 'getDataTrackObjLabUni\n')
    loc.dt = dataMod()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt$trackObjectsLabelUni))
  })
  
  
  getMeas1Selection = reactive({
    if(input$inSelMath1 == '')
      loc.s.x = input$inSelMeas11
    else if (input$inSelMath1 == '1 / ')
      loc.s.x = paste0(input$inSelMath1, input$inSelMeas11)
    else
      loc.s.x = paste0(input$inSelMeas11, input$inSelMath1, input$inSelMeas12)
  })
  
  getMeas2Selection = reactive({
    if(input$inSelMath2 == '')
      loc.s.y = input$inSelMeas21
    else if (input$inSelMath2 == '1 / ')
      loc.s.y = paste0(input$inSelMath2, input$inSelMeas21)
    else
      loc.s.y = paste0(input$inSelMeas21, input$inSelMath2, input$inSelMeas22)
  })
  
  # prepare dt for fitting
  # returns dt with x, y, and id columns
  # x & y selected based on input
  data4Fit <- reactive({
    cat(file=stderr(), 'data4Fit\n')
    
    loc.dt = dataMod()
    if(is.null(loc.dt))
      return(NULL)
    
    loc.s.x = getMeas1Selection()
    loc.s.y = getMeas2Selection()

    loc.out = loc.dt[, .(
      x = eval(parse(text = loc.s.x)),
      y = eval(parse(text = loc.s.y)),
      t = get(input$inSelTime),
      id = trackObjectsLabelUni
    )]
    
    # remove rows with NA
    loc.out = loc.out[complete.cases(loc.out)]

    setkey(loc.out, id, t)
    print(loc.out)
    return(loc.out)
  })
  
  # fit linear model (robust or standard lm)
  # returns named list with slope, intercept and rsquared 
  dataFit <- reactive({
    cat(file=stderr(), 'dataFit\n')
    loc.dt = data4Fit()
    
    if(is.null(loc.dt))
      return(NULL)
    
    if (input$inRobustFit)
      loc.fit = lmRob(y ~ x, loc.dt)
    else
      loc.fit = lm(y ~ x, loc.dt)
    
    
    loc.fit.out = list()
    loc.fit.out$coeff.a = summary(loc.fit)$coefficients[2]
    loc.fit.out$coeff.b = summary(loc.fit)$coefficients[1]
    loc.fit.out$r.squared = summary(loc.fit)$r.squared
    
    return(loc.fit.out)
  })
  
  # select trajectories within a band around linear regression line
  # returns original dt with mid.in column added whether trajectory is in or out of the selection band
  data4scatterPlot <- reactive({
    cat(file=stderr(), 'data4scatterPlot\n')
    loc.dt = data4Fit()
    
    if(is.null(loc.dt))
      return(NULL)
    
    # Assign logical variable whether the cell is within te band
    loc.tracks = dataSelTracksIn()
    loc.dt[, mid.in := ifelse(id %in% loc.tracks, TRUE, FALSE)]
    
    # Assign tracks selected for highlighting in UI
    # Same column, mid.in is used to ad 3rd level - SELECTED
    loc.tracks.highlight = input$inSelHighlight
    locBut = input$chBhighlightTraj
    if (locBut) {
      loc.dt[, mid.in := ifelse(id %in% loc.tracks.highlight, 'SELECTED', mid.in)]
    }

    return(loc.dt)
  })
  
  # select trajectories within a band around linear regression line
  # returns vector of trackIDs that lie within selection band
  dataSelTracksIn <- reactive({
    cat(file=stderr(), 'dataSelTracksIn\n')

    loc.dt = data4Fit()
    if(is.null(loc.dt))
      return(NULL)
    
    loc.fit = dataFit()
    
    # add Boolean column: TRUE if a point lies within selection band obtained from fitting
    loc.dt[, mid.in := ifelse(y > loc.fit$coeff.a * x + loc.fit$coeff.b - abs(loc.fit$coeff.b) * input$inBandWidth &
                                y < loc.fit$coeff.a * x + loc.fit$coeff.b + abs(loc.fit$coeff.b) * input$inBandWidth, TRUE, FALSE)]
    
    # calculate number of time points and the number of time points within the band per track id 
    loc.dt.n = loc.dt[, .(n.tpoints = .N, n.tpoints.in = sum(mid.in)), by = id]
    
    # Select cells of which entire trajectories are within the band
    return(loc.dt.n[n.tpoints - n.tpoints.in == 0, id])
  })
  
  
  # prepare data for plotting time courses
  # returns dt with these columns:
  # realtime - selected from input
  # y - measurement selected from input (can be a single column or result of an operation on two cols)
  # id - trackObjectsLabelUni (created in dataMod)
  # group - grouping variable from input
  data4trajPlot <- reactive({
    cat(file=stderr(), 'data4trajPlot\n')
    
    loc.dt = dataMod()
    if(is.null(loc.dt))
      return(NULL)
    
    
    if(input$inSelMath3 == '')
      loc.s.y = input$inSelMeas31
    else if (input$inSelMath3 == '1 / ')
      loc.s.y = paste0(input$inSelMath3, input$inSelMeas31)
    else
      loc.s.y = paste0(input$inSelMeas31, input$inSelMath3, input$inSelMeas32)
    
    # create expression for parsing
    # creates a merged column based on other columns from input
    # used for grouping of plot facets
    loc.s.gr = sprintf("paste(%s, sep=';')", paste(input$inSelGroup, sep = '', collapse = ','))
    
    loc.s.rt = input$inSelTime
    
    loc.out = loc.dt[, .(
      y = eval(parse(text = loc.s.y)),
      id = trackObjectsLabelUni,
      group = eval(parse(text = loc.s.gr)),
      realtime = eval(parse(text = loc.s.rt))
    )]
    
    
    # add a column that indicates whether entire track is in or out of the selection band
    loc.tracks = dataSelTracksIn()
    loc.out[, mid.in := ifelse(id %in% loc.tracks, TRUE, FALSE)]
    
    # Assign tracks selected for highlighting in UI
    loc.tracks.highlight = input$inSelHighlight
    locBut = input$chBhighlightTraj
    if (locBut) {
      loc.out[, mid.in := ifelse(id %in% loc.tracks.highlight, 'SELECTED', mid.in)]
    }
    
    # remove rows with NA
    return(loc.out[complete.cases(loc.out)])
  })
  
  # return dt WITHOUT tracks that aren't within selection band
  data4saveClean <- reactive({
    cat(file = stderr(), 'data4saveClean\n')
    loc.dt = dataMod()
    if(is.null(loc.dt))
      return(NULL)
    
    loc.dt = loc.dt[trackObjectsLabelUni %in% dataSelTracksIn()]
    
    return(loc.dt[, names(loc.dt)[!(names(loc.dt) %in% 'trackObjectsLabelUni')], with = FALSE])
  })
  
  # return original dataset with and additional column
  # signifying whether a track is within selection band
  data4saveMarked <- reactive({
    cat(file = stderr(), 'data4saveMarked\n')
    loc.dt = dataMod()
    if(is.null(loc.dt))
      return(NULL)
    
    loc.tracks = dataSelTracksIn()
    if(is.null(loc.tracks))
      return(NULL)
    
    loc.dt[, mid.in := ifelse(trackObjectsLabelUni %in% loc.tracks, TRUE, FALSE)]
    return(loc.dt)
  })
  
  
  # calculate fraction of tracks within selection band
  dataCalcMidInFrac <- reactive({
    cat(file=stderr(), 'dataCalcMidInFrac\n')
    
    loc.tracks = dataSelTracksIn()
    loc.dt = data4Fit()
    if(is.null(loc.dt) || is.null(loc.tracks))
      return(NULL)
    
    loc.l = list()
    loc.l$nTracksTot = length(unique(loc.dt$id))
    loc.l$nTracksMid = length(loc.tracks)
    return(loc.l)
  })
  
  output$varSelSite = renderUI({
    cat(file = stderr(), 'UI varSelSite\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'ite'][1] # index 1 at the end in case more matches; select 1st
      # cat(locColSel, '\n')
      selectInput(
        'inSelSite',
        'Select FOV (e.g. Metadata_Site or Metadata_Series):',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  output$varSelTrackLabel = renderUI({
    cat(file = stderr(), 'UI varSelTrackLabel\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'rack'][1] # index 1 at the end in case more matches; select 1st
      # cat(locColSel, '\n')
      selectInput(
        'inSelTrackLabel',
        'Select Track Label (e.g. objNuc_Track_ObjectsLabel):',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  output$varSelTime = renderUI({
    cat(file = stderr(), 'UI varSelTime\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'RealTime'][1] # index 1 at the end in case more matches; select 1st
      # cat(locColSel, '\n')
      selectInput(
        'inSelTime',
        'Select time (e.g. RealTime):',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  

  # This is main field to select plot facet grouping
  # It's typically a column with the entire experimental description,
  # e.g. in Yannick's case it's Stim_All_Ch or Stim_All_S.
  # In Coralie's case it's a combination of 3 columns called Stimulation_...
  output$varSelGroup = renderUI({
    cat(file=stderr(), 'UI varSelGroup\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'ite']
      if (length(locColSel) == 0)
        locColSel = locCols[locCols %like% 'eries'][1] # index 1 at the end in case more matches; select 1st
      else if (length(locColSel) > 1) {
        locColSel = locColSel[1]
      }
      #    cat('UI varSelGroup::locColSel ', locColSel, '\n')
      selectInput('inSelGroup', 
                  'Select one or more facet groupings (e.g. Site, Well, Channel):', 
                  locCols, width = '100%', selected = locColSel, multiple = TRUE)
    }
    
  })
  
  output$varSelMeas11 = renderUI({
    cat(file = stderr(), 'UI varSelMeas11\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'imNucCorrBg'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        'inSelMeas11',
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
        'inSelMeas12',
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
        'inSelMeas21',
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
        'inSelMeas22',
        'Select 2nd perand for Y-axis:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  output$varSelHighlight = renderUI({
    cat(file = stderr(), 'UI varSelHighlight\n')

    locBut = input$chBhighlightTraj
    if (!locBut)
      return(NULL)
    
    loc.v = getDataTrackObjLabUni()
    if(!is.null(loc.v)) {
      selectInput(
        'inSelHighlight',
        'Select one or more rajectories:',
        loc.v,
        width = '100%',
        multiple = TRUE
      )
    }
  })
  
  output$varSelMeas31 = renderUI({
    cat(file = stderr(), 'UI varSelMeas31\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'objCyto_Intensity_MeanIntensity_imErkCor.*'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        'inSelMeas31',
        'Select Y-axis:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  output$varSelMeas32 = renderUI({
    cat(file = stderr(), 'UI varSelMeas32\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols) &&
        !(input$inSelMath3 %in% c('', '1 / '))) {
      locColSel = locCols[locCols %like% 'objNuc_Intensity_MeanIntensity_imErkCor.*'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        'inSelMeas32',
        'Select 2nd opernad for Y-axis',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  calcStats = reactive({
    cat(file = stderr(), 'calcStats\n')

    loc.dt = data4scatterPlot()
    
    if (is.null(loc.dt))
      return(NULL)

    loc.dt.aggr = as.data.table(loc.dt[, sapply(.SD, function(x) c('N' = .N,
                                                     'Mean' = mean(x), 
                                                     'CV' = sd(x)/mean(x), 
                                                     'Median' = median(x), 
                                                     'rCV (IQR)' = IQR(x)/median(x), 
                                                     'rCV (MAD)'= mad(x)/median(x))), .SDcols = c('x', 'y')])
    loc.dt.aggr[, stat := c('N', 'Mean', 'CV', 'Median', 'rCV (IQR)', 'rCV (MAD)')]
    
    return(loc.dt.aggr)
  })
  
  output$outTabStats = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'outTabStats\n')
    loc.dt = calcStats()
    
    if (is.null(loc.dt))
      return(NULL)
    
    #datatable(loc.dt, filter = 'none') %>% formatRound(1:2, 3)
    
    datatable(loc.dt, 
              rownames = FALSE,
              extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 
                               'print', 
                               list(extend = 'collection',
                                    buttons = list(list(extend='csv',
                                                        filename = 'hitStats'),
                                                   list(extend='excel',
                                                        filename = 'hitStats'),
                                                   list(extend='pdf',
                                                        filename= 'hitStats')),
                                    text = 'Download')))) %>% formatRound(1:2, 3)
  })
  
  output$outTextCellRatio = renderText({
    cat(file = stderr(), 'outTextCellRatio\n')
    
    locBut = input$butGoScatter
    if (locBut == 0) {
      cat(file=stderr(), 'outTextCellRatio: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.res = dataCalcMidInFrac()

    paste0('Selected: ', loc.res$nTracksMid, '/', loc.res$nTracksTot, ' tracks')
  })
  
  plotScatter <- function() {
    cat(file=stderr(), "plotScatter\n")
    
    # isolate because calculations & plotting take a while
    # re-plotting done upon button press
    loc.dt = isolate(data4scatterPlot())
    loc.fit = isolate(dataFit())
    loc.res = isolate(dataCalcMidInFrac())
    
    cat("plotScatter on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file=stderr(), 'plotScatter: dt is NULL\n')
      return(NULL)
    }
    
    cat(file=stderr(), 'plotScatter:dt not NULL\n')

    # select every other point for plotting
    loc.dt = loc.dt[, .SD[seq(1, .N, input$sliPlotScatterSkip)], by = id]
    
    
    
    ## FIX: r.squared is unavailable for lm  
    
    #     loc.fit.rsq = ifelse(input$inRobustFit, loc.fit$r.squared, )

    p.out = myGgplotScat(xlab.arg = getMeas1Selection(), ylab.arg = getMeas2Selection(),
      dt.arg = loc.dt,
      band.arg = list(a = loc.fit$coeff.a, b = loc.fit$coeff.b, width = input$inBandWidth),
      group.col.arg = 'mid.in',
      plotlab.arg = sprintf(
        "%s%.2f\n%s%.2f x %.2f\nSelected: %d / %d tracks",
        ifelse(input$inRobustFit, "lmRob, entire dataset R2=", "lm, entire dataset R2="),
        loc.fit$r.squared,
        'bandwidth=',
        input$inBandWidth,
        loc.fit$coeff.b,
        loc.res$nTracksMid, 
        loc.res$nTracksTot
      ),
      alpha.arg = 0.5
    )
    
    return(p.out)
  }
  
  output$outPlotScatter <- renderPlot({
    locBut = input$butGoScatter
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotScatter: Go button not pressed\n')
      return(NULL)
    }
    
    plotScatter()
  })

  
  output$outPlotScatterInt = renderPlotly({
    locBut = input$butGoScatter
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotScatter: Go button not pressed\n')
      
      return(NULL)
    }
    
    # This is required to avoid 
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device") dev.off()
    pdf(NULL)
    
    return( plotly_build(plotScatter()))
  })
  
  # download pdf
  callModule(downPlot, "downPlotScatter", "scatter.pdf", plotScatter, TRUE)
  callModule(downPlot, "downPlotTraj", "tcourses.pdf", plotTraj, TRUE)
  
  # 
  output$uiPlotScatter <- renderUI({
    if (input$chBplotScatterInt)
      plotlyOutput("outPlotScatterInt", height = paste0(input$inPlotScatterHeight, "px"))
    else
      plotOutput('outPlotScatter', height = paste0(input$inPlotScatterHeight, "px"))
  })

  
    
  plotTraj <- function() {
    
    cat(file=stderr(), 'plotTraj: in\n')
    locBut = input$butGoTraj
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotTraj: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.dt = isolate(data4trajPlot())
    
    cat("plotScatter on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file=stderr(), 'plotTraj: dt is NULL\n')
      return(NULL)
    }
    
    cat(file=stderr(), 'plotTraj:dt not NULL\n')
    
    
    p.out = myGgplotTraj(
      dt.arg = loc.dt,
      x.arg = 'realtime',
      y.arg = 'y',
      group.arg = "id",
      facet.arg = 'group',
      line.col.arg = 'mid.in',
      facet.ncol.arg = input$inPlotTrajFacetNcol,
      xlab.arg = 'Time (min)'
    )
    
    return(p.out)
    # This is required to avoid 
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device") dev.off()
    pdf(NULL)
    
    p.out.ly = plotly_build(p.out)
    return(p.out.ly)
  }
  
  output$outPlotTraj <- renderPlot({
    locBut = input$butGoTraj
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotTraj: Go button not pressed\n')
      return(NULL)
    }
    
    plotTraj()
  })
  
  output$outPlotTrajInt <- renderPlotly({
    locBut = input$butGoTraj
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotTraj: Go button not pressed\n')
      return(NULL)
    }
    
    # This is required to avoid 
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device") dev.off()
    pdf(NULL)
    
    return( plotly_build(plotTraj()))
    
  })

  output$uiPlotTraj <- renderUI({
    if (input$chBplotTrajInt)
      plotlyOutput("outPlotTrajInt", height = paste0(input$inPlotTrajHeight, "px"))
    else
      plotOutput('outPlotTraj', height = paste0(input$inPlotTrajHeight, "px"))
  })
  
  
  output$downloadDataClean <- downloadHandler(
    filename = 'tCoursesSelected_clean.csv',
    content = function(file) {
      write.csv(data4saveClean(), file, row.names = FALSE)
    }
  )

  output$downloadDataMarked <- downloadHandler(
    filename = 'tCoursesSelected_midin.csv',
    content = function(file) {
      write.csv(data4saveMarked(), file, row.names = FALSE)
    }
  )
  
})
