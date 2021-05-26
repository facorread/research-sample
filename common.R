# This function loads a library; it stops the whole process if the library needs installation or corrections.
fabioLibrary <- function(...) {
  if(library(..., logical.return = TRUE) != TRUE) {
    stop("Did you install this library?\n")
  }
}

# # To deal with masking, load your most important packages last
fabioLibrary(conflicted)
# fabioLibrary(ssanv)
# fabioLibrary(dagitty)
fabioLibrary(data.table)
# fabioLibrary(ellipse)
# fabioLibrary(entropy)
fabioLibrary(ggplot2)
fabioLibrary(cowplot)
fabioLibrary(openxlsx) # Export to Excel: write.xlsx. Do not use read.xlsx because it does not preserve space characters in column names. Use read_excel instead.
fabioLibrary(readxl) # Read from Excel: read_excel
# fabioLibrary(rethinking)

conflict_prefer("%like%", "data.table", quiet = TRUE)
conflict_prefer("dbern", "rethinking", quiet = TRUE)
conflict_prefer("pairs", "rethinking", quiet = TRUE)
conflict_prefer("Position", "base", quiet = TRUE)
conflict_prefer("rbern", "rethinking", quiet = TRUE)
conflict_prefer("rstudent", "rethinking", quiet = TRUE)

# This function returns the (i-th)-before-the-last element of vector v. No bounds checking. Please make sure i >= 0.
fabioLastItem <- function(v, i) {
  v[length(v) - i]
}

# This function returns the (i-th)-before-the-last row of data frame df. No bounds checking. Please make sure i >= 0.
fabioLastRow <- function(df, i) {
  df[nrow(df) - i]
}

# This function sets the column names of a data.table; it is similar to setnames.data.table(x, old, new) but with a different syntax: fabioSetNames(x, "Old1", "New1", "Old2", "New2", ...). This prevents confusion. See the unit tests below for examples.
fabioSetNames <- function(x, ...) {
  namesMatrix <- matrix(c(...), nrow = 2)
#  print(namesMatrix[1,])
#  print(namesMatrix[2,])
  setnames(x, namesMatrix[1,], namesMatrix[2,])
}

# fabioApply takes the results from rethinking::link() or rethinking::sim(), applies statistical functions such as HPDI and formats the output for assignment into a data.table. The very first use case is at my homework02 Rmd. See the unit tests below.
fabioApply <- function(data, ...) {
  as.list(as.data.frame(t(apply(data, 2, ...))))
}

# Prints a precis object without the Unicode histogram. Histograms do not work on neither RStudio nor RMarkdown for Windows.
fabioPrecis <- function(...) {
  r <- precis(...)
  r$histogram <- NULL
  r
}

# The panel.* functions come from the Analysing Ecological Data AED library.
# Mixed effects models and extensions in ecology with R. (2009).
# Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.
# Copyright Highland Statistics LTD.

#Here are some functions that we took from the pairs help file and
#modified, or wrote ourselves. To cite these, use the r citation: citation()

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}

panel.smooth2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                        cex = 1, col.smooth = "black", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}

panel.lines2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                       cex = 1, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)){
    tmp=lm(y[ok]~x[ok])
    abline(tmp)}
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

fabioPairs <- function(...) {
  graphics::pairs(..., lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel = panel.hist)
}

# Returns the mode (most frequent value) in an object.
fabioMode <- function(v) {
  d <- as.data.table(v)
  counts <- d[, .N, by = v]
  counts[which.max(N), v]
}

# Replaces the NAs with the mode (most frequent value) in the vector.
# Useful for curating data.
fabioCheckFill <- function(v, strict = FALSE) {
  omitIdx <- is.na(v) | v == "?" | v == "TBD"
  if(sum(omitIdx) != length(v)) {
    v1 <- v[!omitIdx]
    sameVal <- fabioMode(v1) # The mode
    if(all(v1 == sameVal)) {
      v[omitIdx] = sameVal
    } else if (strict) {
      warning("To prevent data loss, fabioCheckFill() is not filling in blanks for ",
        paste(v, collapse = " "))
    }
  }
  v
}

# Returns a logical vector indicating the data that is valid.
fabioIsValid <- function(v) {
  (!is.na(v)) & (v != "?") & (v != "TBD")
}

# Omits invalid data.
fabioOmitInvalid <- function(v) {
  v[fabioIsValid(v)]
}

# Removes all non-numbers from v and coerces it to integer. Discards "." and "," too!
fabioInteger <- function(v) {
  as.integer(gsub("[^0-9]", "", v, perl = TRUE))
}

# Returns the maximum value of fabioInteger(v)
fabioMaxInteger <- function(v, ...) {
  max(fabioInteger(v), ...)
}

# Returns nMax unique values that are the maxima of vector v
#
# Examples
#
# stopifnot(fabioMaxima(c(1, 2, 2, 3, 4, 4, 5), 1) == 5)
# stopifnot(fabioMaxima(c(1, 2, 2, 3, 4, 4, 5), 2) == 4:5)
# stopifnot(fabioMaxima(c(1, 2, 2, 3, 4, 4, 5), 3) == 3:5)
# stopifnot(fabioMaxima(c(1, 2, 2, 3, 4, 4, 5), 4) == 2:5)
# stopifnot(fabioMaxima(c(1, 2, 2, 3, 4, 4, 5), 5) == 1:5)
# stopifnot(fabioMaxima(c(1, 2, 2, 3, 4, 4, NA), 5) == 1:4)
#
fabioMaxima <- function(v, nMax) {
  vunique <- data.table(values = na.omit(v))[, .BY, keyby = values]
  tail(vunique$values, nMax)
}

# Returns a logical vector indicating which elements are
# the nMax maxima of vector v.
#
# Examples
#
# stopifnot(fabioSelMax(c(1, 1, 2, 3, 3), 1) == c(FALSE, FALSE, FALSE, TRUE, TRUE))
# stopifnot(fabioSelMax(c(1, 1, 2, 3, 3), 2) == c(FALSE, FALSE, TRUE, TRUE, TRUE))
# stopifnot(fabioSelMax(c(1, NA, 3), 1) == c(FALSE, FALSE, TRUE))
fabioSelMax <- function(v, nMax) {
  v %in% fabioMaxima(v, nMax)
}

# Returns a logical vector indicating which elements are
# the maximum of vector v.
#
# Examples
#
# stopifnot(fabioMax(c(1, 2, 3, 3, NA)) == c(FALSE, FALSE, TRUE, TRUE, FALSE))
fabioMax <- function(v) {
  v %in% max(na.omit(v))
}
# Returns nMin unique values that are the minima of vector v
#
# Examples
#
# stopifnot(fabioMinima(c(1, 2, 2, 3, 4, 4, 5), 1) == 1)
# stopifnot(fabioMinima(c(1, 2, 2, 3, 4, 4, 5), 2) == 1:2)
# stopifnot(fabioMinima(c(1, 2, 2, 3, 4, 4, 5), 3) == 1:3)
# stopifnot(fabioMinima(c(1, 2, 2, 3, 4, 4, 5), 4) == 1:4)
# stopifnot(fabioMinima(c(1, 2, 2, 3, 4, 4, 5), 5) == 1:5)
# stopifnot(fabioMinima(c(1, NA, 2, 3, 4, 4, 5), 5) == 1:5)
#
fabioMinima <- function(v, nMin) {
  vunique <- data.table(values = na.omit(v))[, .BY, keyby = values]
  head(vunique$values, nMin)
}

# Returns a logical vector indicating which elements are
# the nMin minima of vector v.
#
# Examples
#
# stopifnot(fabioSelMin(c(1, 1, 2, 3, 3), 1) == c(TRUE, TRUE, FALSE, FALSE, FALSE))
# stopifnot(fabioSelMin(c(1, 1, 2, 3, 3), 2) == c(TRUE, TRUE, TRUE, FALSE, FALSE))
# stopifnot(fabioSelMin(c(1, NA, 3), 1) == c(TRUE, FALSE, FALSE))
fabioSelMin <- function(v, nMin) {
  v %in% fabioMinima(v, nMin)
}

# Returns a logical vector indicating which elements are
# the minimum of vector v.
#
# Examples
#
# stopifnot(fabioMin(c(1, 1, 2, 3, NA)) == c(TRUE, TRUE, FALSE, FALSE, FALSE))
fabioMin <- function(v) {
  v %in% min(na.omit(v))
}

# Returns a logical vector indicating which elements are at the extremes of
# the range of vector v.
#
# Examples
#
# stopifnot(fabioSelRange(c(1, 1, 2, 3)) == c(TRUE, TRUE, FALSE, TRUE))
# stopifnot(fabioSelRange(c(1, 1, 2, 3)) == c(TRUE, TRUE, FALSE, TRUE))
# stopifnot(fabioSelRange(c(1, NA, 3)) == c(TRUE, FALSE, TRUE))
fabioSelRange <- function(v) {
  v %in% range(v, na.rm = TRUE)
}

# Do not treat warnings as errors
fabioDoNotTreatWarningsAsErrors <- function() {
  options(warn = 1)
}

# Treat warnings as errors
fabioTreatWarningsAsErrors <- function() {
  options(warn = 2)
}

# Treat warnings as errors
fabioTreatWarningsAsErrors()

# ggplot theme
# Note: some ggplot elements have hardcoded defaults. For dark themes there is no way around setting aesthetics by hand:
# geom_path(colour = th$col0)
# geom_point(colour = th$col0)
{
  .GlobalEnv$th <- list()
  th$size0 <- 5
  th$size1 <- 6
  th$size2 <- 7
  th$size3 <- 8
  th$setDark <- function() {
    .GlobalEnv$th$col0 <- "white" # Foreground color
    .GlobalEnv$th$col1 <- "grey10"
    .GlobalEnv$th$col2 <- "black" # Background color
    .GlobalEnv$th$col3 <- "#00ff00" # For gradient
    .GlobalEnv$th$col4 <- "#ffff00" # For gradient
    .GlobalEnv$th$col5 <- "#ff0000" # For gradient
    .GlobalEnv$th$line0 <- element_line(colour = th$col0, size = 1)
    .GlobalEnv$th$line1 <- element_line(colour = th$col1, size = 1)
    .GlobalEnv$th$rect0 <- element_rect(fill = NA, colour = th$col0, size = 0, linetype = 0)
    .GlobalEnv$th$rect1 <- element_rect(fill = th$col2, colour = th$col1, size = 1, linetype = "solid")
    .GlobalEnv$th$rect2 <- element_rect(fill = NA, colour = th$col0, size = 1, linetype = "solid")
    .GlobalEnv$th$rect3 <- element_rect(fill = th$col2, colour = th$col2, size = 1, linetype = "solid")
    .GlobalEnv$th$text0 <- element_text(colour = th$col0, size = th$size0)
    .GlobalEnv$th$text1 <- element_text(colour = th$col0, size = th$size1)
    .GlobalEnv$th$text2 <- element_text(colour = th$col0, size = th$size2)
    .GlobalEnv$th$text3 <- element_text(colour = th$col0, size = th$size3)
    theme_set(theme_cowplot() + background_grid() + theme(
      axis.line = th$line1, axis.line.x = th$line0, axis.line.x.top = th$line1, axis.line.x.bottom = th$line0, axis.line.y.left = th$line0, axis.line.y.right = th$line1,
      axis.text = th$text0, axis.text.x = th$text0, axis.text.y = th$text0, axis.text.x.top = th$text0, axis.text.x.bottom = th$text0, axis.text.y.left = th$text0,
      axis.title = th$text1, axis.title.x = th$text1, axis.title.y = th$text1, axis.title.x.top = th$text1, axis.title.x.bottom = th$text1, axis.title.y.left = th$text1,
      axis.ticks = th$line0, axis.ticks.x = th$line0, axis.ticks.x.top = th$line0, axis.ticks.x.bottom = th$line0, axis.ticks.y = th$line0, axis.ticks.y.left = th$line0, legend.box = "horizontal", legend.background = th$rect1, legend.box.background = th$rect1, legend.text = th$text2, legend.title = th$text2,
      panel.background = th$rect3, panel.border = th$rect2, plot.background = th$rect3, panel.grid.major = th$line1, panel.grid.minor = element_blank()
      ))
    #theme_set(theme_dark())
  }
  th$setLight <- function() {
    .GlobalEnv$th$col0 <- "black" # Foreground color
    .GlobalEnv$th$col1 <- "grey90"
    .GlobalEnv$th$col2 <- "white" # Background color
    .GlobalEnv$th$col3 <- "#007f00" # For gradient
    .GlobalEnv$th$col4 <- "#7fff00" # For gradient
    .GlobalEnv$th$col5 <- "#ff0000" # For gradient
    .GlobalEnv$th$line0 <- element_line(colour = th$col0, size = 1)
    .GlobalEnv$th$line1 <- element_line(colour = th$col1, size = 1)
    .GlobalEnv$th$rect0 <- element_rect(fill = NA, colour = th$col0, size = 0, linetype = 0)
    .GlobalEnv$th$rect1 <- element_rect(fill = th$col2, colour = th$col1, size = 1, linetype = "solid")
    .GlobalEnv$th$rect2 <- element_rect(fill = NA, colour = th$col0, size = 1, linetype = "solid")
    .GlobalEnv$th$rect3 <- element_rect(fill = th$col2, colour = th$col2, size = 1, linetype = "solid")
    .GlobalEnv$th$text0 <- element_text(colour = th$col0, size = th$size2)
    .GlobalEnv$th$text1 <- element_text(colour = th$col0, size = th$size3)
    .GlobalEnv$th$text2 <- element_text(colour = th$col0, size = th$size1)
    theme_set(theme_cowplot() + background_grid() + theme(
      axis.text = th$text0, axis.text.x = th$text0, axis.text.y = th$text0, axis.text.x.top = th$text0, axis.text.x.bottom = th$text0, axis.text.y.left = th$text0,
      axis.title = th$text1, axis.title.x = th$text1, axis.title.y = th$text1, axis.title.x.top = th$text1, axis.title.x.bottom = th$text1, axis.title.y.left = th$text1,
      legend.text = th$text2, legend.title = th$text2, legend.box = "horizontal", legend.box.background = element_rect(fill = th$col2, colour = th$col1, size = 0.25) ) )
  }
  # activate a theme here:
  # th$setDark()
  th$setLight()
}
figLabel <- function(figX, figY, figWidth, figHeight, labelText) {
  annotation_custom(grobTree(rectGrob(x = figX, y = figY, width = figWidth, height = figHeight, gp = gpar(col = th$col1, fill = th$col2)),
    textGrob(labelText, x = figX, y = figY, gp = gpar(col = th$col0, fill = th$col2, fontsize = th$size0))))
}
figAuthors <- function(figX, figY, figGitCommit = figureGitCommit) {
  figLabel(figX, figY, 0.15, 0.09, paste0("F. Correa, S. Downey\n", figureDate, " (", figGitCommit, ")"))
}
figRect <- function(figX, figY, figWidth, figHeight) {
  annotation_custom(rectGrob(x = figX, y = figY, width = figWidth, height = figHeight, gp = gpar(col = th$col1, fill = th$col2)))
}
figIcon <- function(fileName, ...) {
  # The ellipsis enables rasterGrob defaults. Examples:
  # ggplot() + figIcon("AuxGraphics/Patterns/00000000.png", x = 0.5, y = 0.5, width = 0.1, height = 0.1)
  # ggplot() + figIcon("AuxGraphics/Patterns/00000000.png", x = 0.5, y = 0.5, width = 0.1)
  annotation_custom(rasterGrob(image = readPNG(fileName), ...) )
}
figIconKey <- function(fileNames, x, y, ydelta, ...) {
  # Example:
  # swstrat$selectStrategies <- c("11211212", "11112122", "11212112", "11112212", "11112222", "11111222", "11112112", "00000000")
  # ggplot() + figIconKey(fileNames = swstrat$selectStrategies, x = 0.22, y = 0.95, ydelta = -0.05)
  fullFileNames <- paste0("AuxGraphics/Patterns/", fileNames, ".png")
  yValues <- seq(y, by = ydelta, along.with = fullFileNames)
  mFunction <- function(filename, yvalue) {
    rasterGrob(image = readPNG(filename), x = x, y = yvalue, ...)
  }
  iconGrobs <- mapply(mFunction, fullFileNames, yValues, SIMPLIFY = FALSE)
  annotation_custom(do.call(grobTree, iconGrobs))
}

# Unit tests for fabioLastItem
stopifnot(fabioLastItem(letters, 3) == "w")

# Unit tests for fabioSetNames
ut1 <- list()
ut1$x <- data.table(old1 = c(11, 21), old2 = c(12, 22))
fabioSetNames(ut1$x, "old1", "new1", "old2", "new2")
stopifnot(names(ut1$x) == c("new1", "new2"))
ut1$y <- data.table(old1 = c(11, 21), old2 = c(12, 22))
fabioSetNames(ut1$y, c("old1", "new1"), c("old2", "new2"))
stopifnot(names(ut1$y) == c("new1", "new2"))

# Unit tests for fabioLastRow
stopifnot(all(fabioLastRow(ut1$x, 1) == data.table(new1 = 11, new2 = 12)))
stopifnot(all(fabioLastRow(ut1$x, 0) == data.table(new1 = 21, new2 = 22)))

# # Unit tests for fabioApply
# data(Howell1)
# set.seed(10000)
# ut2 <- list()
# ut2$d <- as.data.table(Howell1)
# ut2$d2 <- ut2$d[age >= 18,]
#
# # define the average weight, x-bar
# ut2$xbar <- mean(ut2$d2$weight)
#
# # fit model
# ut2$model <- quap(
#   alist(
#     height ~ dnorm( mu , sigma ) ,
#     mu <- a + b*( weight - ut2$xbar ) ,
#     a ~ dnorm( 178 , 20 ) ,
#     b ~ dlnorm( 0 , 1 ) ,
#     sigma ~ dunif( 0 , 50 )
#   ) ,
#   data=ut2$d2 )
#
# summary(ut2$model)
# ut2$newData <- data.table(Individual = 1:5, weight = c(45, 40, 65, 31, 53))
# ut2$newH <- sim(ut2$model, data = ut2$newData)
# ut2$newData[, `Expected height` := apply(ut2$newH, 2, mean)]
# ut2$newData[, c("|0.89", "0.89|") := fabioApply(ut2$newH, HPDI, prob=0.89)]
# # dput(ut2$newData)
# # dput(ut2$expectedData)
# ut2$expectedData = data.table(Individual = 1:5, weight = c(45, 40, 65, 31, 53
# ), `Expected height` = c(154.812789663582, 150.177585576336,
# 172.355970561305, 141.987645946323, 162.113464829334), `|0.89` = c(146.755844775977,
# 141.948919315438, 163.868766618236, 133.631600156691, 154.848486766838
# ), `0.89|` = c(163.452669899048, 157.508581867091, 179.903173954529,
# 149.065833707887, 170.394611966835))
# stopifnot(isTRUE(all.equal(ut2$newData, ut2$expectedData)))

# Unit tests for fabioInteger
ut3 <- list()
ut3$i <- fabioInteger(c("E1000", "E2F", "4.2"))
stopifnot(ut3$i == c(1000, 2, 42))

# Unit tests for fabioMode
stopifnot(fabioMode(c(1, 1, 0)) == 1)
stopifnot(is.na(fabioMode(c(NA, NA, 0))))
stopifnot(fabioMode(c(1, 1, 0, 0, 0)) == 0)
stopifnot(fabioMode(c(1, 1, 2, 2)) == 1)
stopifnot(identical(fabioCheckFill(c(1, 1, NA, NA)), c(1, 1, 1, 1)))
stopifnot(identical(fabioCheckFill(c(1, 2, NA, NA)), c(1, 2, NA, NA)))

set.seed(10000)

