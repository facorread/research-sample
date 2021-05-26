#!/usr/bin/R

# This script uses the "facd" name to prevent clashes and misunderstandings with R resources.

# Common variables and algorithms
{
  source("common.R")

  # Common variables
  facdCommon <- list(date = "2021-05-14")
  facdCommon$figureLabels <- paste0("F. Correa\n", facdCommon$date)
  facdCommon$figureSizes <- 3

  # Common algorithms and utilities
  facdCommon$printLevels <- function(param)
    cat(deparse(substitute(param)), levels(factor(param)), "\n")
  facdCommon$stopifnotall <- function(comparison) {
    stopifnot(length(comparison) > 0) # This prevents a false negative forstopifnot(logical(0)) or stopifnot(all(logical(0)))
    stopifnot(all(comparison))
  }
}

# Reading and curating data
{
  facdData <- list()
  # facdData$degrees <- as.data.table(read_excel("Coordinates From Daily Records.xlsx")) # Do not use read.xlsx because it does not preserve space characters in column names.
  facdData$degrees <- as.data.table(read.csv("CSV_5162021-609/CSV_5162021-609.csv"))
  # Note: unitid identifies each university, not each department.

  facdData$finance <- as.data.table(read.csv("CSV_5162021-1007/CSV_5162021-1007.csv"))
  # str(facdData$degrees)
  # dput(names(facdData$degrees))
  # Parsing, auditing, and transforming coordinates
  facdData$curate <- function(dt) { # dt: data.table
    classes <- sapply(dt, class)
    facdCommon$stopifnotall(classes %in% c("character", "integer"))
    removecols <- which(classes == "character")
    set(dt, , removecols, NULL)
    # Almost all names require manual fixes. Let's not automate name fixes.
    # n <- gsub('.', ' ', names(dt), fixed = TRUE)
    # n <- gsub('  +', ' ', n)
    # setnames(dt, n)
    facdCommon$stopifnotall(is.finite(dt$unitid))
    facdCommon$stopifnotall(dt$year == 2019)
    dt[, year := NULL]
  }
  facdData$curate(facdData$degrees)
  facdData$curate(facdData$finance)
  # dput(names(facdData$degrees))
  # dput(names(facdData$finance))
  tmp <- list()
  # tmp$nprogs <- facdData$degrees[, .N, by = unitid] # Multiple programs per unitid
  # tmp$nfinancials <- facdData$finance[, .N, by = unitid] # One line per unitid
  facdData$degrees[, IDX_C := NULL]
  facdData$degrees <- facdData$degrees[, lapply(.SD, sum), by = unitid]
  facdData$degrees <- facdData$degrees[, .(unitid, Graduations = C2019_A.Grand.total, `International graduations (%)` = 100 * C2019_A.Nonresident.alien.total / C2019_A.Grand.total)]
  facdData$degrees[is.na(`International graduations (%)`), `International graduations (%)` := 0]
  stopifnot(!anyNA(facdData$degrees))
  stopifnot(!anyNA(facdData$finance$unitid))
  stopifnot(!anyDuplicated(facdData$degrees$unitid))
  stopifnot(!anyDuplicated(facdData$finance$unitid))

  facdData$finance[, EFIA2019.Estimated.full.time.equivalent..FTE..undergraduate.enrollment..2018.19 := NULL] # The DF has a collinear, `Reported` version of this variable.
  facdData$finance[, `Tuition ($)` := IC2019_AY.In.state.average.tuition.for.full.time.undergraduates]
  facdData$finance[is.na(`Tuition ($)`), `Tuition ($)` := IC2019_AY.Published.in.state.tuition.and.fees.2018.19]
  facdData$finance[is.na(`Tuition ($)`), `Tuition ($)` := IC2019_PY.Published.tuition.and.fees.2018.19]
  facdData$finance[is.na(`Tuition ($)`), `Tuition ($)` := DRVIC2019.Total.price.for.in.state.students.living.on.campus.2019.20]
  facdData$finance[!(`Tuition ($)` > 0), `Tuition ($)` := NA]
  facdData$finance[!(`Tuition ($)` > 0), `Tuition ($)` := NA]
  facdData$finance[`Tuition ($)` > 0, `Grant aid per student (% of Tuition)` := SFA1819.Average.amount.of.federal..state..local..institutional.or.other.sources.of.grant.aid.awarded.to.undergraduate.students / `Tuition ($)`]
  # facdData$finance[, c(.N, sum(is.na(`Grant aid per student (% of Tuition)`)))]
  # Columns that have NA should not participate in step()
  facdData$finance[, IC2019_PY.Published.tuition.and.fees.2018.19 := NULL]
  facdData$finance[, IC2019_PY.Books.and.supplies.2018.19 := NULL] # Too few
  facdData$finance[, IC2019_PY.On.campus..room.and.board.2018.19 := NULL]
  facdData$finance[, IC2019_AY.In.state.average.tuition.for.full.time.undergraduates := NULL]
  facdData$finance[, DRVIC2019.Total.price.for.in.state.students.living.on.campus.2019.20 := NULL]
  facdData$finance[, IC2019_AY.Published.in.state.tuition.and.fees.2018.19 := NULL]
  facdData$finance[, DRVHR2019.Average.salary.equated.to.9.months.of.full.time.instructional.staff...all.ranks := NULL]
  facdData$finance[, ADM2019.Applicants.total := NULL]
  facdData$finance[, ADM2019.Applicants.men := NULL]
  facdData$finance[, ADM2019.Applicants.women := NULL]
  facdData$finance[, ADM2019.Admissions.total := NULL]
  facdData$finance[, ADM2019.Admissions.men := NULL]
  facdData$finance[, ADM2019.Admissions.women := NULL]
  facdData$finance[, ADM2019.Percent.of.first.time.degree.certificate.seeking.students.submitting.SAT.scores := NULL]
  facdData$finance[, ADM2019.Percent.of.first.time.degree.certificate.seeking.students.submitting.ACT.scores := NULL]
  facdData$finance[, SFA1819.Average.amount.of.federal..state..local..institutional.or.other.sources.of.grant.aid.awarded.to.undergraduate.students := NULL]
  # # Manual review of data columns
  # facdData$naCount <- function(x) {
  #     s <- sum(is.na(x))
  #     if (s > 0) {
  #       s
  #     } else {
  #       NA
  #     }
  #   }
  #   facdData$nacols <- sapply(facdData$finance, facdData$naCount)
  #   facdData$nacols <- na.omit(facdData$nacols)
  #   sort(facdData$nacols)
  #   nrow(facdData$finance)
  #   names(facdData$nacols[facdData$nacols])
  #   length(facdData$nacols)
  # c(nrow(facdData$finance), nrow(facdData$finance[complete.cases(facdData$finance),]))
  facdData$finance <- facdData$finance[complete.cases(facdData$finance),]
  facdData$d <- facdData$degrees[facdData$finance, on = "unitid", nomatch = NULL]
  # dput(names(facdData$d))
  stopifnot(!anyNA(facdData$d))
  # names(facdData$d)
  # Rename variables a posteriori
  tmp$names <- names(facdData$d)
  names(tmp$names) <- tmp$names
  tmp$names["SFA1819.Percent.of.undergraduate.students.awarded.federal..state..local..institutional.or.other.sources.of.grant.aid"] <- "Funded undergrads (%)"
  tmp$names["EFIA2019.Reported.full.time.equivalent..FTE..undergraduate.enrollment..2018.19"] <- "Undergrad enrollment (Credit hours)"
  setnames(facdData$d, tmp$names)
  tmp$scalenames <- tmp$names[tmp$names != "unitid"]
  facdData$dn <- copy(facdData$d)
  facdData$dn[, tmp$scalenames := lapply(.SD, scale, center = FALSE), .SDcols = tmp$scalenames]
  tmp$stepnames <- tmp$names[!(tmp$names %in% c("unitid", "Graduations"))]
  facdData$scope <- as.formula(paste0("~`", paste(tmp$stepnames, collapse="`+`"), "`"))
  facdData$glm <- glm(Graduations ~ `Tuition ($)`, data = facdData$dn)
  facdData$step <- step(facdData$glm, scope = facdData$scope)
}
{
  cat("Summary of original data:\n")
  print(summary(facdData$d))
  facdData$pct90f <- function(x) {
    q90 = quantile(x, 0.9)
    sum(x[x > q90]) / sum(x)
  }
  facdData$pct90 <- facdData$d[, lapply(.SD, facdData$pct90f)]

  # ncol(facdData$dn)
  # summary(facdData$d)
  # summary(facdData$dn)
  # boxplot(facdData$dn[,2:7])
  # fabioPairs(facdData$dn[,2:7])
  # summary(facdData$step)
  # plot(facdData$step)
}
{
  facdData$melt <- melt(facdData$dn[,2:7], measure.vars = 1:6, variable.name = "Unsorted variable", value.name = "Value, normalized by the variable's Root Mean Square (RMS)", variable.factor = FALSE)
  facdData$melt[, Variable := factor(`Unsorted variable`, levels = rev(names(facdData$d)[2:7]), ordered = TRUE)]
  ggsave(filename = "Fig01.png", device = "png", scale = 1.2, width = 6.5, height = 3,
    units = "in", dpi = 400, plot = (ggplot(facdData$melt, aes(x = Variable, y = `Value, normalized by the variable's Root Mean Square (RMS)`)) +
    geom_boxplot(fill = "#00804080", outlier.color = "#00000080", outlier.fill = "#00000000", outlier.shape = 21) + #, draw_quantiles = c(0.25, 0.5, 0.75)
    # geom_violin(fill = "#40800080") + #, draw_quantiles = c(0.25, 0.5, 0.75)
    # scale_fill_manual(values = c("black", "green")) +
    coord_flip(ylim = c(0, 2.5)) +
    theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1), panel.grid.major.y = element_blank()))
  )
  facdData$melt <- melt(facdData$dn[,2:7], measure.vars = 1:6, variable.name = "Unsorted variable", value.name = "Value, normalized by the variable's Root Mean Square (RMS)", variable.factor = FALSE)
  facdData$melt[, Variable := factor(`Unsorted variable`, levels = rev(names(facdData$d)[2:7]), ordered = TRUE)]
  ggsave(filename = "Fig02.png", device = "png", scale = 1.2, width = 6.5, height = 4,
    units = "in", dpi = 400, plot = fabioPairs(facdData$dn[,2:7])
  )
}

