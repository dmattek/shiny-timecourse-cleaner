rhg_cols <- c(
  "#771C19",
  "#AA3929",
  "#E25033",
  "#F27314",
  "#F8A31B",
  "#E2C59F",
  "#B6C5CC",
  "#8E9CA3",
  "#556670",
  "#000000"
)

md_cols <- c(
  "#FFFFFF",
  "#F8A31B",
  "#F27314",
  "#E25033",
  "#AA3929",
  "#FFFFCC",
  "#C2E699",
  "#78C679",
  "#238443"
)

#####
## Custom functions

myCheckDigits <- function(x) {
  grepl('^[-]?[0-9]+[.]?[0-9]*$' , x)
}

myCheckLogical <- function(x) {
  grepl('^TRUE$|^FALSE$' , x)
}

myConvertStringListToTypes <- function(in.l) {
  # convert strings with digits to numeric
  # uses logical indexing: http://stackoverflow.com/questions/42207235/replace-list-elements-by-name-with-another-list
  loc.l = myCheckDigits(in.l)
  in.l[loc.l] = lapply(in.l[loc.l], as.numeric)
  
  # convert strings with TRUE/FALSE to logical
  loc.l = myCheckLogical(in.l)
  in.l[loc.l] = lapply(in.l[loc.l], as.logical)
  
  return(in.l)
}

# Read a two column xlsx file with parameters
# 1st column contains parameter names
# 2nd column contais parameter values
# Returns a list with parameters and ther values
myParRead = function(in.fname) {
  require(xlsx)
  
  loc.df.par = read.xlsx(
    s.par.plot,
    sheetIndex = 1,
    header = FALSE,
    as.data.frame = TRUE,
    colIndex = 1:2,
    colClasses = rep("character", 2),
    stringsAsFactors = FALSE
  )
  
  # convert data frame with parameters to a list
  l.par = split(loc.df.par[, 2], loc.df.par[, 1])
  
  # convert strings with digits to numeric and strings with TRUE/FALSE to logical
  l.par = myConvertStringListToTypes(l.par)
  
  return(l.par)
}


# f-n to read experimental description
# returns data table with entire experimental data
myExpRead = function(inFname,
                     inCleanRowCol = TRUE,
                     inCleanMissing = TRUE,
                     inStartRow = 1,
                     inSheetName = 1,
                     inRowIndex = NULL) {
  # read the file
  loc.dt.exp = as.data.table(
    read.xlsx(
      file = inFname,
      sheetName = inSheetName,
      rowIndex = inRowIndex,
      startRow = inStartRow
    )
  )
  
  if (inCleanRowCol) {
    # sometimes an NA column appears at the end; remove
    loc.dt.exp = loc.dt.exp[, names(loc.dt.exp)[!(names(loc.dt.exp) %like% 'NA')], with = FALSE]
    
    # sometimes an NA row appears at the end; remove
    loc.dt.exp = loc.dt.exp[loc.dt.exp[,!Reduce(`&`, lapply(.SD, is.na))]]
  }
  
  if (inCleanMissing) {
    # replace missing values with ''
    for (i in seq_along(loc.dt.exp))
      set(loc.dt.exp,
          i = which(is.na(loc.dt.exp[[i]])),
          j = i,
          value = '')
  }
  
  return(loc.dt.exp)
}


# Plots a scatter plot with marginal histograms
# Points are connected by a line (grouping by cellID)
#
# Assumes an input of data.table with
# x, y - columns with x and y coordinates
# id - a unique point identifier (here corresponds to cellID)
# mid - a (0,1) column by which points are coloured (here corresponds to whether cells are within bounds)

myGgplotScat = function(dt.arg,
                        band.arg = NULL,
                        facet.arg = NULL,
                        facet.ncol.arg = 2,
                        xlab.arg = NULL,
                        ylab.arg = NULL,
                        plotlab.arg = NULL,
                        alpha.arg = 1,
                        group.col.arg = NULL) {
  p.tmp = ggplot(dt.arg, aes(x = x, y = y))
  
  if (is.null(group.col.arg)) {
    p.tmp = p.tmp +
      geom_point(alpha = alpha.arg)
  } else {
    p.tmp = p.tmp +
      geom_point(aes(colour = as.factor(get(group.col.arg)), group = id), alpha = alpha.arg) +
      geom_path(aes(colour = as.factor(get(group.col.arg)), group = id), alpha = alpha.arg) +
      scale_color_manual(name = group.col.arg, values =c("FALSE" = rhg_cols[7], "TRUE" = rhg_cols[3], "SELECTED" = 'green'))
  }
  
  if (is.null(band.arg))
    p.tmp = p.tmp +
      stat_smooth(
        method = function(formula, data, weights = weight)
          rlm(formula, data, weights = weight, method = 'MM'),
        fullrange = FALSE,
        level = 0.95,
        colour = 'blue'
      )
  else {
    p.tmp = p.tmp +
      geom_abline(slope = band.arg$a, intercept = band.arg$b) +
      geom_abline(
        slope = band.arg$a,
        intercept =  band.arg$b + abs(band.arg$b)*band.arg$width,
        linetype = 'dashed'
      ) +
      geom_abline(
        slope = band.arg$a,
        intercept = band.arg$b - abs(band.arg$b)*band.arg$width,
        linetype = 'dashed'
      )
  }
  
  if (!is.null(facet.arg)) {
    p.tmp = p.tmp +
      facet_wrap(as.formula(paste("~", facet.arg)),
                 ncol = facet.ncol.arg)
    
  }
  
  
  if (!is.null(xlab.arg))
    p.tmp = p.tmp +
      xlab(paste0(xlab.arg, "\n"))
  
  if (!is.null(ylab.arg))
    p.tmp = p.tmp +
      ylab(paste0("\n", ylab.arg))
  
  if (!is.null(plotlab.arg))
    p.tmp = p.tmp +
      ggtitle(paste0(plotlab.arg, "\n"))
  
  
  
  p.tmp = p.tmp +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.25),
      axis.line.y = element_line(color = "black", size = 0.25),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      strip.text.x = element_text(size = 14, face = "bold"),
      strip.text.y = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines"),
      legend.position = "none"
    )
  
  # Marginal distributions don;t work with plotly...
  # if (is.null(facet.arg))
  #   ggExtra::ggMarginal(p.scat, type = "histogram",  bins = 100)
  # else
  return(p.tmp)
}


myGgplotTraj = function(dt.arg,
                        x.arg,
                        y.arg,
                        group.arg,
                        facet.arg,
                        facet.ncol.arg = 2,
                        line.col.arg = NULL,
                        xlab.arg = NULL,
                        ylab.arg = NULL,
                        plotlab.arg = NULL,
                        dt.stim.arg = NULL,
                        ylim.arg = c(0, 1),
                        stim.bar.height.arg = 0.1,
                        stim.bar.width.arg = 0.5) {
  p.tmp = ggplot(dt.arg,
                 aes_string(x = x.arg,
                            y = y.arg,
                            group = group.arg))
  
  if (is.null(line.col.arg))
    p.tmp = p.tmp + 
      geom_line(alpha = 0.25, size = 0.25)
  else {
    p.tmp = p.tmp + 
      geom_line(aes_string(colour = line.col.arg),
                alpha = 0.5,
                size = 0.5) +
      scale_color_manual(name = line.col.arg, values =c("FALSE" = rhg_cols[7], "TRUE" = rhg_cols[3], "SELECTED" = 'green'))
    
  }
  
  
  p.tmp = p.tmp +
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.y = mean,
      colour = 'blue',
      linetype = 'solid',
      size = 1,
      geom = "line",
      group = 1
    )
  
  if (!is.null(facet.arg)) {
    p.tmp = p.tmp +
      facet_wrap(as.formula(paste("~", facet.arg)),
                 ncol = facet.ncol.arg)
  }
  
  
  if (!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_segment(
      data = dt.stim.arg,
      aes(
        x = Stimulation_time,
        xend = Stimulation_time,
        y = ylim.arg[1],
        yend = ylim.arg[1] + abs(ylim.arg[2] - ylim.arg[1]) * stim.bar.height.arg
      ),
      colour = rhg_cols[[3]],
      size = stim.bar.width.arg,
      group = 1
    )
  }
  
  if (!is.null(xlab.arg))
    p.tmp = p.tmp +
    xlab(paste0(xlab.arg, "\n"))
  
  if (!is.null(ylab.arg))
    p.tmp = p.tmp +
    ylab(paste0("\n", ylab.arg))
  
  if (!is.null(plotlab.arg))
    p.tmp = p.tmp +
    ggtitle(paste0(plotlab.arg, "\n"))
  
  p.tmp = p.tmp +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.25),
      axis.line.y = element_line(color = "black", size = 0.25),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      strip.text.x = element_text(size = 14, face = "bold"),
      strip.text.y = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines"),
      legend.position = "top"
    )
  
  p.tmp
}

# generate artificial dataset
userDataGen <- function() {
  cat(file = stderr(), 'userDataGen: in\n')
  
  locNtp = 13
  locNtracks = 5
  locNsites = 4
  locNwells = 2
  
  loc.dt = data.table(
    Metadata_Site = rep(1:locNsites, each = locNtp * locNtracks),
    Metadata_Well = rep(1:locNwells, each = locNtp * locNsites * locNtracks / locNwells),
    RealTime = rep(1:locNtp, locNsites * locNtracks),
    objCyto_Intensity_MeanIntensity_imErkCor = rnorm(locNtp * locNtracks * locNsites, 1, 0.5),
    objNuc_Intensity_MeanIntensity_imErkCor  = rnorm(locNtp * locNtracks * locNsites, .5, 0.2),
    objCell_Intensity_MeanIntensity_imErkCor = rnorm(locNtp * locNtracks * locNsites, 1.5, 0.25),
    TrackLabel = rep(1:(locNtracks * locNsites), each = locNtp)
  )
  
  loc.dt[, meas_MeanIntensity_nuc_imNucCorrBg := objNuc_Intensity_MeanIntensity_imErkCor + rnorm(locNtp * locNtracks * locNsites, 0, 0.1)]
  
  return(loc.dt)
}
