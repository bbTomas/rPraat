#' tg.checkTierInd
#'
#' Returns tier index. Input can be either index (number) or tier name (character string).
#' It performs checks whether the tier exists.
#'
#' @param tg TextGrid object
#' @param tierInd Tier index or "name"
#'
#' @return Tier index
#'
#' @export
#' @seealso \code{\link{tg.getTierName}}, \code{\link{tg.isIntervalTier}}, \code{\link{tg.isPointTier}}, \code{\link{tg.plot}}, \code{\link{tg.getNumberOfTiers}}
#' @examples
#' tg <- tg.sample()
#' tg.checkTierInd(tg, 4)
#' tg.checkTierInd(tg, "word")
tg.checkTierInd <- function(tg, tierInd) {
    ntiers <- length(tg)

    if (is.numeric(tierInd) | is.integer(tierInd)) {      # tier index
        if ((length(tierInd) != 1)) {
            stop(paste0("tierInd must be one integer number [length(tierInd) = ", length(tierInd), "]"))
        }

        if (!tbTools::isInt(tierInd)) {
            stop(paste0("tierInd must be integer >= 1 [",  as.character(tierInd), "]"))
        }

        if (tierInd < 1 | tierInd > ntiers) {
            stop(paste0("tierInd out of range, tierInd = ", tierInd, ", ntiers = ", ntiers))
        }
    } else if (is.character(tierInd)) {       # tier name
        if ((length(tierInd) != 1)) {
            stop(paste0("tierInd must be one character string (tier name) [length(tierInd) = ", length(tierInd), "]"))
        }

        actNames <- names(tg)
        if (!(tierInd %in% actNames)) {
            stop(paste0("Tier name not found in TextGrid [character tierInd = '", tierInd, "']"))
        }

        if (sum(actNames == tierInd) != 1) {
            stop(paste0("TextGrid has a duplicate tier name [", tierInd, "]. You should not use the name for indexing to avoid ambiguity."))
        }

        tierInd <- which(actNames == tierInd)

    } else {
        stop(paste0("Wrong format of tierInd, should be integer or character [class(tierInd) = ", class(tierInd), "]"))
    }

    return(tierInd)
}



#' tg.read
#'
#' Loads TextGrid from Praat in Text or Short text format (UTF-8),
#' it handles both Interval and Point tiers.
#' Labels can may contain quotation marks and new lines.
#'
#' @param fileNameTextGrid Input file name
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.write}}, \code{\link{tg.plot}}, \code{\link{tg.repairContinuity}}, \code{\link{tg.createNewTextGrid}}, \code{\link{pt.read}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.read("demo/H.TextGrid")
#' tg.plot(tg)
#' }
tg.read <- function(fileNameTextGrid) {
    if (!tbTools::isString(fileNameTextGrid)) {
        stop("Invalid 'fileNameTextGrid' parameter.")
    }

    tg <- list()  # new textgrid

    # fid <- file(fileNameTextGrid, open = "r", encoding = "UTF-8")
    # flines <- readLines(fid)
    flines <- readr::read_lines(fileNameTextGrid, locale = readr::locale(encoding = "UTF-8"))
    # close(fid)
    find <- 4   # index of line to read, we ignore the first three

    xminStr <- flines[find]; find <- find + 1 # xmin
    xmaxStr <- flines[find]; find <- find + 1; # xmax

    r <- flines[find]; find <- find + 1; # either "<exists>" -> shorttext or "tiers? <exists> " -> full text format

    if (r == "<exists>") {
        shortFormat <- TRUE
    } else if (substr(r, 1, 6) == "tiers?") {
        shortFormat <- FALSE
    } else {
        stop("Unknown textgrid format.")
    }

    if (shortFormat) {
        xmin <- as.numeric(xminStr) # xmin
        xmax <- as.numeric(xmaxStr) # xmax
    } else {
        xmin <- readr::parse_number(xminStr) # xmin
        xmax <- readr::parse_number(xmaxStr) # xmax
    }

    if (shortFormat) {
        nTiers <- as.numeric(flines[find])
        find <- find + 1
    } else {
        r <- flines[find]
        if (substr(r, nchar(r), nchar(r)) != " ") {
            sppasFormat <- TRUE;
        } else {
            sppasFormat <- FALSE;
        }
        nTiers <- readr::parse_number(r)
        find <- find + 1
    }

    for (tier in tbTools::seqM(1, nTiers)) {

        if (shortFormat) {
            typ <- flines[find]; find <- find + 1
        } else {
            r <- tbTools::strTrim(flines[find]); find <- find + 1

            while (substr(r, 1, 4) == "item") {
                r <- tbTools::strTrim(flines[find]); find <- find + 1
            }

            if (substr(r, 1, 9) != 'class = "') {
                stop("Unknown textgrid format")
            }
            typ <- substr(r, 9, nchar(r))
        }

        if (typ == '"IntervalTier"') {  # IntervalTier
            r <- flines[find]; find <- find + 1  # names
            if (shortFormat) {
                tierName <- substr(r, 2, nchar(r)-1)
            } else {
                r <- tbTools::strTrim(r);
                tierName <- substr(r, 9, nchar(r)-1)
            }
            tierType <- "interval"

            find <- find + 2; # ignore xmin and xmax

            if (shortFormat) {
                nIntervals <- as.numeric(flines[find]); find <- find + 1
            } else {
                r <- tbTools::strTrim(flines[find]); find <- find + 1
                nIntervals <- readr::parse_number(r)
            }

            tierT1 <- numeric(0)
            tierT2 <- numeric(0)
            tierLabel <- character(0)

            for (I in tbTools::seqM(1, nIntervals)) {
                if (!shortFormat) {
                    r <- flines[find]; find <- find + 1 # ignore line intervals [..]:
                }

                if (shortFormat) {
                    t <- as.numeric(flines[find]); find <- find + 1
                    t2 <- as.numeric(flines[find]); find <- find + 1
                } else {
                    r1 <- tbTools::strTrim(flines[find]); find <- find + 1
                    r2 <- tbTools::strTrim(flines[find]); find <- find + 1
                    if ( (substr(r1, 1, 7) != "xmin = ")  ||  (substr(r2, 1, 7) != "xmax = ") ) {
                        stop("Unknown textgrid format");
                    }
                    t <- readr::parse_number(r1)
                    t2 <- readr::parse_number(r2)
                }

                r <- flines[find]; find <- find + 1;
                if (!shortFormat) {
                    if (!tbTools::str_contains(r, 'text = "')) {
                        stop("Unknown textgrid format");
                    }
                    rind <- tbTools::str_find1(r, '"')
                    nQuotationMarks <- length(tbTools::str_find(r, '"'))  # in Matlab: sum(r == '"')
                    if ((nQuotationMarks %% 2) != 1) { # remove whitespace at the end of line, it is only in the case of even number of quotation marks
                        if (sppasFormat != TRUE) {
                            r <- substr(r, rind, nchar(r)-1)
                        } else {
                            r <- substr(r, rind, nchar(r))
                        }
                    } else {
                        r <- substr(r, rind, nchar(r))
                    }
                }
                nQuotationMarks <- length(tbTools::str_find(r, '"'))
                label <- substr(r, 2, nchar(r))
                if ((nQuotationMarks %% 2) == 1) {
                    label <- paste0(label, "\n")

                    repeat {
                        r <- flines[find]; find <- find + 1
                        nQuotationMarks <- length(tbTools::str_find(r, '"'))
                        if (!shortFormat & (nQuotationMarks %% 2 == 1) & !sppasFormat) { # remove whitespace at the end of line, it is only in the case of odd number of quotation marks
                            r <- substr(r, 1, nchar(r)-1)
                        }

                        if (nQuotationMarks %% 2 == 1  &  stringr::str_sub(r, -1) == '"') {
                            label <- paste0(label, substr(r, 1, nchar(r)-1), '"')
                            break
                        } else {
                            label <- paste0(label, r, "\n")
                        }
                    }
                }
                label <- substr(label, 1, nchar(label)-1)

                tierT1 <- c(tierT1, t)
                tierT2 <- c(tierT2, t2)
                tierLabel <- c(tierLabel, label)

                xmin <- min(t, xmin); xmin <- min(t2, xmin);
                xmax <- max(t, xmax); xmax <- max(t2, xmax);
            }

            tg[[length(tg)+1]] <- list(name = tierName, type = tierType,
                                       t1 = tierT1, t2 = tierT2, label = tierLabel)
            actNames <- names(tg)
            proposedName <- tierName
#             while (proposedName %in% actNames) {
#                 proposedName <- paste0(proposedName, "2")  # modify already existing name by adding 2 at the end
#             }
            if (proposedName %in% actNames) {
                warning(paste0("TextGrid has a duplicate tier name [", proposedName, "]. You should not use the name for indexing to avoid ambiguity."))
            }
            names(tg)[length(tg)] <- proposedName



        } else if (typ == '"TextTier"') {  # PointTier
            r <- flines[find]; find <- find + 1  # name
            if (shortFormat) {
                tierName <- substr(r, 2, nchar(r)-1)
            } else {
                r <- tbTools::strTrim(r)
                tierName <- substr(r, 9, nchar(r)-1)
            }
            tierType <- "point"

            find <- find + 2 # ignore xmin and xmax

            if (shortFormat) {
                nIntervals <- as.numeric(flines[find]); find <- find + 1
            } else {
                r <- tbTools::strTrim(flines[find]); find <- find + 1
                nIntervals <- readr::parse_number(r)
            }

            tierT <- numeric(0)
            tierLabel <- character(0)

            for (I in tbTools::seqM(1, nIntervals)) {
                if (!shortFormat) {
                    r <- flines[find]; find <- find + 1 # ignore line points [..]:
                }

                if (shortFormat) {
                    t <- as.numeric(flines[find]); find <- find + 1
                } else {
                    r <- tbTools::strTrim(flines[find]); find <- find + 1
                    if (substr(r, 1, 9) != "number = ") {
                        stop("Unknown textgrid format");
                    }
                    t <- readr::parse_number(r)
                }

                r <- flines[find]; find <- find + 1
                if (!shortFormat) {
                    if (!tbTools::str_contains(r, 'mark = "')) {
                        stop("Unknown textgrid format");
                    }
                    rind <- tbTools::str_find1(r, '"')
                    nQuotationMarks <- length(tbTools::str_find(r, '"'))
                    if (nQuotationMarks %% 2 != 1) { # remove whitespace at the end of line, it is only in the case of even number of quotation marks
                        if (!sppasFormat) {
                            r <- substr(r, rind, nchar(r)-1)
                        } else {
                            r <- substr(r, rind, nchar(r))
                        }
                    } else {
                        r <- substr(r, rind, nchar(r))
                    }
                }
                nQuotationMarks <- length(tbTools::str_find(r, '"'))
                label <- substr(r, 2, nchar(r))
                if (nQuotationMarks %% 2 == 1) {
                    label <- paste0(label, "\n")
                    repeat {
                        r <- flines[find]; find <- find + 1
                        nQuotationMarks <- length(tbTools::str_find(r, '"'))
                        if (!shortFormat & (nQuotationMarks %% 2 == 1) & !sppasFormat) { # remove whitespace at the end of line, it is only in the case of odd number of quotation marks
                            r <- substr(r, 1, nchar(r)-1)
                        }

                        if ((nQuotationMarks %% 2 == 1) & (stringr::str_sub(r, -1) == '"')) {
                            label <- paste0(label, substr(r, 1, nchar(r)-1), '"')
                            break
                        } else {
                            label <- paste0(label, r, "\n")
                        }

                    }
                }
                label <- substr(label, 1, nchar(label)-1)

                tierT <- c(tierT, t)
                tierLabel <- c(tierLabel, label)

                xmin <- min(t, xmin)
                xmax <- max(t, xmax)
            }

            tg[[length(tg)+1]] <- list(name = tierName, type = tierType,
                                       t = tierT, label = tierLabel)
            actNames <- names(tg)
            proposedName <- tierName
            #             while (proposedName %in% actNames) {
            #                 proposedName <- paste0(proposedName, "2")  # modify already existing name by adding 2 at the end
            #             }
            if (proposedName %in% actNames) {
                warning(paste0("TextGrid has a duplicate tier name [", proposedName, "]. You should not use the name for indexing to avoid ambiguity."))
            }
            names(tg)[length(tg)] <- proposedName

        } else {  # neznamy typ tier
            stop(paste0("Unsupported tier type [tierInd = ", length(tg)+1, "]"))
        }

    }


    class(tg)["tmin"] <- xmin
    class(tg)["tmax"] <- xmax

    return(tg)
}




#' tg.write
#'
#' Saves TextGrid to the file. TextGrid may contain both interval and point
#' tiers (tg[[1]], tg[[2]], tg[[3]], etc.). If tier type is not specified in $type,
#' is is assumed to be "interval". If specified, $type have to be "interval" or "point".
#' If there is no class(tg)["tmin"] and class(tg)["tmax"], they are calculated as min and max of
#' all tiers. The file is saved in Short text file, UTF-8 format.
#'
#' @param tg TextGrid object
#' @param fileNameTextGrid Output file name
#'
#' @export
#' @seealso \code{\link{tg.read}}, \code{\link{pt.write}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.write(tg, "demo_output.TextGrid")
#' }
tg.write <- function(tg, fileNameTextGrid) {
    if (!tbTools::isString(fileNameTextGrid)) {
        stop("Invalid 'fileNameTextGrid' parameter.")
    }

    nTiers <- length(tg)  # number of Tiers

    minTimeTotal <-  NaN
    maxTimeTotal <-  NaN
    if ("tmin" %in% names(class(tg))  &  "tmax" %in% names(class(tg))) {
        minTimeTotal <- as.numeric(class(tg)["tmin"])
        maxTimeTotal <- as.numeric(class(tg)["tmax"])
    }

    for (I in tbTools::seqM(1, nTiers)) {
        if ("type" %in% names(tg[[I]])) {

            if (tg[[I]]$type == "interval") {
                typInt <- TRUE
            } else if (tg[[I]]$type == "point") {
                typInt <- FALSE
            } else {
                stop(paste0("Unknown tier type [", tg[[I]]$type, "]"))
            }
        } else {
            typInt <- TRUE
        }
        tg[[I]]$typInt <- typInt

        if (typInt == TRUE) {
            nInt <- length(tg[[I]]$t1) # number of intervals
            if (nInt > 0) {
                minTimeTotal <- min(tg[[I]]$t1[1], minTimeTotal)
                maxTimeTotal <- max(tg[[I]]$t2[length(tg[[I]]$t2)], maxTimeTotal)
            }
        } else {
            nInt <- length(tg[[I]]$t) # number of points
            if (nInt > 0) {
                minTimeTotal <- min(tg[[I]]$t[1], minTimeTotal)
                maxTimeTotal <- max(tg[[I]]$t[length(tg[[I]]$t)], maxTimeTotal)
            }
        }
    }

    fid <- file(fileNameTextGrid, open = "w", encoding = "UTF-8")
    if (!isOpen(fid)) {
        stop(paste0("cannot open file [", fileNameTextGrid, "]"))
    }

    writeLines('File type = "ooTextFile"', fid)
    writeLines('Object class = "TextGrid"', fid)
    writeLines("", fid)
    writeLines(as.character(tbTools::round2(minTimeTotal, -10)), fid)  # min time from all tiers
    writeLines(as.character(tbTools::round2(maxTimeTotal, -10)), fid)  # max time from all tiers
    writeLines("<exists>", fid)
    writeLines(as.character(nTiers), fid)

    for (N in tbTools::seqM(1, nTiers)) {
        if (tg[[N]]$typInt == TRUE) {  # interval tier
            writeLines('"IntervalTier"', fid)
            writeLines(paste0('"', tg[[N]]$name, '"'), fid)

            nInt <- length(tg[[N]]$t1)  # number of intervals
            if (nInt > 0) {
                writeLines(as.character(tbTools::round2(tg[[N]]$t1[1], -10)), fid)  # start time of the tier
                writeLines(as.character(tbTools::round2(tg[[N]]$t2[length(tg[[N]]$t2)], -10)), fid)  # end time of the tier
                writeLines(as.character(nInt), fid)  # pocet intervalu textgrid

                for (I in tbTools::seqM(1, nInt)) {
                    writeLines(as.character(tbTools::round2(tg[[N]]$t1[I], -10)), fid)
                    writeLines(as.character(tbTools::round2(tg[[N]]$t2[I], -10)), fid)
                    writeLines(paste0('"', tg[[N]]$label[I], '"'), fid)
                }
            } else {   # create one empty interval
                writeLines(as.character(tbTools::round2(minTimeTotal, -10)), fid)  # start time of the tier
                writeLines(as.character(tbTools::round2(maxTimeTotal, -10)), fid)  # end time of the tier
                writeLines("1", fid)  # number of intervals
                writeLines(as.character(tbTools::round2(minTimeTotal, -10)), fid)
                writeLines(as.character(tbTools::round2(maxTimeTotal, -10)), fid)
                writeLines('""', fid)
            }
        } else { # pointTier
            writeLines('"TextTier"', fid)
            writeLines(paste0('"', tg[[N]]$name, '"'), fid)

            nInt <- length(tg[[N]]$t)  # number of intervals
            if (nInt > 0) {
                writeLines(as.character(tbTools::round2(tg[[N]]$t[1], -10)), fid)  # start time of the tier
                writeLines(as.character(tbTools::round2(tg[[N]]$t[length(tg[[N]]$t)], -10)), fid)  # end time of the tier
                writeLines(as.character(nInt), fid)  # number of points

                for (I in tbTools::seqM(1, nInt)) {
                    writeLines(as.character(tbTools::round2(tg[[N]]$t[I], -10)), fid)
                    writeLines(paste0('"', tg[[N]]$label[I], '"'), fid)
                }
            } else { # prazdny pointtier
                writeLines(as.character(tbTools::round2(minTimeTotal, -10)), fid)  # start time of the tier
                writeLines(as.character(tbTools::round2(maxTimeTotal, -10)), fid)  # end time of the tier
                writeLines("0", fid)  # number of points
            }
        }

    }

    close(fid)
}


#' tg.plot
#'
#' Plots interactive TextGrid using dygraphs package.
#'
#' @param tg TextGrid object
#' @param group [optional] character string, name of group for dygraphs synchronization
#'
#' @export
#' @seealso \code{\link{tg.read}}, \code{\link{pt.plot}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.plot(tg)
#' }
tg.plot <- function(tg, group = "") {
    ntiers <- length(tg)

    if (ntiers == 0) {
        dygraphs::dygraph(list(x = 0, y = NA), main = "Empty TextGrid")
    }

    if (length(names(tg)) != length(unique(names(tg)))) {
        stop("Sorry, tg.plot cannot display TextGrids with duplicated tier names.")
    }

    if (!tbTools::isString(group)) {
        stop("group must be a character string.")
    }

    tAll <- as.numeric(c(class(tg)["tmin"], class(tg)["tmax"]))

    # find all time instances
    for (I in tbTools::seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            tAll <- c(tAll, tg[[I]]$t)

        } else if (tg[[I]]$type == "interval") {
            tAll <- c(tAll, tg[[I]]$t1, tg[[I]]$t1)

        } else {
            stop("Unknown tier type")
        }

    }
    tAll <- unique(sort(tAll))

    data <- list(t = tAll)

    # create tiers
    for (I in tbTools::seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            y <- rep(as.numeric(NA), length(tAll))

            y[tAll %in% tg[[I]]$t] <- ntiers + 1 - I  # y-value of graphic point according to tier index
            data[[length(data)+1]] <- y
            names(data)[length(data)] <- tg[[I]]$name

        } else if (tg[[I]]$type == "interval") {
            y <- rep(as.numeric(NA), length(tAll))

            # y[tAll %in% unique(c(tg[[I]]$t1, tg[[I]]$t2))] <- ntiers + 1 - I  # y-value of graphic point according to tier index
            y <- rep(ntiers + 1 - I, length(tAll))
            data[[length(data)+1]] <- y
            names(data)[length(data)] <- tg[[I]]$name

        } else {
            stop("Unknown tier type")
        }

    }

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
        g <- dygraphs::dyRangeSelector(g)
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
        g <- dygraphs::dyRangeSelector(g)
    }

    # Pridani popisku
    for (I in tbTools::seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            for (J in tbTools::seqM(1, length(tg[[I]]$label))) {
                g <- dygraphs::dyAnnotation(g, tg[[I]]$t[J], text = tg[[I]]$label[J], width = 10*max(1, nchar(tg[[I]]$label[J])), height = 25, series = tg[[I]]$name, tooltip = tg[[I]]$label[J])
            }

        } else if (tg[[I]]$type == "interval") {
            for (J in tbTools::seqM(1, length(tg[[I]]$label))) {
                g <- dygraphs::dyAnnotation(g, tg[[I]]$t1[J], text = tg[[I]]$label[J], series = tg[[I]]$name, tooltip = tg[[I]]$label[J], width = 10*max(1, nchar(tg[[I]]$label[J])), height = 25, tickHeight = 10)
            }

        } else {
            stop("Unknown tier type")
        }

    }

    # style of tiers
    for (I in tbTools::seqM(1, ntiers)) {
        if (tg[[I]]$type == "point") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, pointSize = 2, strokeWidth = 0)

        } else if (tg[[I]]$type == "interval") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, pointSize = 2, strokeWidth = 1)

        } else {
            stop("Unknown tier type")
        }
    }
    g <- dygraphs::dyAxis(g, "y", valueRange = c(0, length(tg)+2))
    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    g
}



#' tg.repairContinuity
#'
#' Repairs problem of continuity of T2 and T1 in interval tiers. This
#' problem is very rare and it should not appear. However, e.g.,
#' automatic segmentation tool Prague Labeller produces random numeric
#' round-up errors featuring, e.g., T2 of preceding interval is slightly
#' higher than the T1 of the current interval. Because of that, the boundary
#' cannot be manually moved in Praat edit window.
#'
#' @param tg TextGrid object
#' @param verbose [optional, default=FALSE] If TRUE, the function performs everything quietly.
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.sampleProblem}}
#'
#' @examples
#' \dontrun{
#' tgProblem <- tg.sampleProblem()
#' tgNew <- tg.repairContinuity(tgProblem)
#' tg.write(tgNew, "demo_problem_OK.TextGrid")
#' }
tg.repairContinuity <- function(tg, verbose = FALSE) {
    for (I in tbTools::seqM(1, length(tg))) {
        if (tg[[I]]$type == "interval") {
            for (J in tbTools::seqM(1, length(tg[[I]]$label)-1)) {
                if (tg[[I]]$t2[J] != tg[[I]]$t1[J+1]) {
                    newVal <- mean(c(tg[[I]]$t2[J], tg[[I]]$t1[J+1]))
                    if (!verbose) {
                        cat("Problem found [tier: ", I, ", int: ", J, ", ", J+1, "] t2 = ", as.character(tg[[I]]$t2[J]), ", t1 = ", as.character(tg[[I]]$t1[J+1]), ". New value: ", as.character(newVal), ".\n", sep = "")
                    }

                    tg[[I]]$t2[J] <- newVal
                    tg[[I]]$t1[J+1] <- newVal
                }
            }
        }
    }

    return(tg)
}



#' tg.createNewTextGrid
#'
#' Creates new and empty TextGrid. tStart and tEnd specify the total start
#' and end time for the TextGrid. If a new interval tier is added later
#' without specified start and end, they are set to TextGrid start and end.
#'
#' This empty TextGrid cannot be used for almost anything. At least one tier
#' should be inserted using tg.insertNewIntervalTier() or tg.insertNewPointTier().
#'
#' @param tMin Start time of TextGrid
#' @param tMax End time of TextGrid
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertNewIntervalTier}}, \code{\link{tg.insertNewPointTier}}
#'
#' @examples
#' tg <- tg.createNewTextGrid(0, 5)
#' tg <- tg.insertNewIntervalTier(tg, 1, "word")
#' tg <- tg.insertInterval(tg, "word", 1, 2, "hello")
#' tg.plot(tg)
tg.createNewTextGrid <- function(tMin, tMax) {
    if (!tbTools::isNum(tMin)) {
        stop("tMin must be a number")
    }

    if (!tbTools::isNum(tMax)) {
        stop("tMin must be a number")
    }

    if (tMin > tMax) {
        stop(paste0("Cannot be: tMin > tMax [", as.character(tMin), " > ", as.character(tMax), "]"))
    }

    tgNew <- list()
    class(tgNew)["tmin"] <- tMin
    class(tgNew)["tmax"] <- tMax

    return(tgNew)
}



#' tg.isIntervalTier
#'
#' Returns TRUE if the tier is IntervalTier, FALSE otherwise.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{tg.isPointTier}}, \code{\link{tg.getTierName}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.isIntervalTier(tg, 1)
#' tg.isIntervalTier(tg, "word")
tg.isIntervalTier <- function(tg, tierInd) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (tg[[tierInd]]$type == "interval") {
        b <- TRUE
    } else {
        b <- FALSE
    }


    return(b)
}


#' tg.isPointTier
#'
#' Returns TRUE if the tier is PointTier, FALSE otherwise.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{tg.isIntervalTier}}, \code{\link{tg.getTierName}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.isPointTier(tg, 1)
#' tg.isPointTier(tg, "word")
tg.isPointTier <- function(tg, tierInd) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (tg[[tierInd]]$type == "point") {
        b <- TRUE
    } else {
        b <- FALSE
    }


    return(b)
}



#' tg.getTierName
#'
#' Returns name of the tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return character string
#' @export
#' @seealso \code{\link{tg.setTierName}}, \code{\link{tg.isIntervalTier}}, \code{\link{tg.isPointTier}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getTierName(tg, 2)
tg.getTierName <- function(tg, tierInd) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    n <- tg[[tierInd]]$name

    return(n)
}




#' tg.setTierName
#'
#' Sets (changes) name of tier of the given index.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param name new "name" of the tier
#'
#' @export
#' @seealso \code{\link{tg.getTierName}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <- tg.setTierName(tg, "word", "WORDTIER")
#' tg.getTierName(tg2, 4)
tg.setTierName <- function(tg, tierInd, name) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isString(name)) {
        stop("name must be a character string")
    }

    tgNew <- tg
    tgNew[[tierInd]]$name <- name

    actNames <- names(tgNew)[-tierInd]
    if (name %in% actNames) {
        warning(paste0("TextGrid has a duplicate tier name [", name, "]. You should not use the name for indexing to avoid ambiguity."))
    }
    names(tgNew)[[tierInd]] <- name

    return(tgNew)
}



#' tg.countLabels
#'
#' Returns number of labels with the specified label.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param label character string: label to be counted
#'
#' @return integer number
#' @export
#' @seealso \code{\link{tg.getLabel}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.countLabels(tg, "phone", "a")
tg.countLabels <- function(tg, tierInd, label) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isString(label)) {
        stop("label must be a character string")
    }

    c <- 0  # count

    for (I in tbTools::seqM(1, length(tg[[tierInd]]$label))) {
        if (tg[[tierInd]]$label[I] == label) {
            c <- c + 1
        }
    }

    return(c)
}



#' tg.duplicateTier
#'
#' Duplicates tier originalInd to new tier with specified index newInd
#' (existing tiers are shifted).
#' It is highly recommended to set a name to the new tier
#' (this can also be done later by tg.setTierName). Otherwise, both original and new tiers have the
#' same name which is permitted but not recommended. In such a case, we
#' cannot use the comfort of using tier name instead of its index in other
#' functions.
#'
#' @param tg TextGrid object
#' @param originalInd tier index or "name"
#' @param newInd new tier index (1 = the first)
#' @param newTierName [optional but recommended] name of the new tier
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.setTierName}}, \code{\link{tg.removeTier}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <- tg.duplicateTier(tg, "word", 1, "NEW")
#' tg.plot(tg2)
tg.duplicateTier <- function(tg, originalInd, newInd, newTierName = "") {
    originalInd <- tg.checkTierInd(tg, originalInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(newInd)) {
        stop("newInd must be integer >= 1")
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!tbTools::isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    tgNew <- tg

    tOrig <- tg[[originalInd]]

    for (I in tbTools::seqM(ntiers+1, newInd+1, by = -1)) {
        tgNew[[I]] <- tgNew[[I-1]]
        names(tgNew)[I] <- names(tgNew)[I-1]
    }

    tgNew[[newInd]] <- tOrig

    if (newTierName == "") {
        newTierName <- tg[[originalInd]]$name
    }

    actNames <- names(tgNew)
    if (newTierName %in% actNames) {
        warning(paste0("TextGrid has a duplicate tier name [", newTierName, "]. You should not use the name for indexing to avoid ambiguity."))
    }
    tgNew[[newInd]]$name <- newTierName
    names(tgNew)[newInd] <- newTierName



    return(tgNew)
}



#' tg.getStartTime
#'
#' Returns start time. If tier index is specified, it returns start time
#' of the tier, if it is not specified, it returns start time of the whole
#' TextGrid.
#'
#' @param tg TextGrid object
#' @param tierInd [optional] tier index or "name"
#'
#' @return numeric
#' @export
#' @seealso \code{\link{tg.getEndTime}}, \code{\link{tg.getTotalDuration}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getStartTime(tg)
#' tg.getStartTime(tg, "phone")
tg.getStartTime <- function(tg, tierInd = 0) {
    if (tbTools::isInt(tierInd) & tierInd == 0) {
        t <- as.numeric(class(tg)["tmin"])
        return(t)
    }

    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (tg.isPointTier(tg, tierInd)) {
        if (length(tg[[tierInd]]$t) < 1) {
            t <- NA
        } else {
            t <- tg[[tierInd]]$t[1]
        }
    } else if (tg.isIntervalTier(tg, tierInd)) {
        if (length(tg[[tierInd]]$t1) < 1) {
            t < NA
        } else {
            t <- tg[[tierInd]]$t1[1]
        }
    } else {
        stop(paste0("Unknown tier type [tierInd = ", tierInd, "]"))
    }

    return(t)
}




#' tg.getEndTime
#'
#' Returns end time. If tier index is specified, it returns end time
#' of the tier, if it is not specified, it returns end time of the whole
#' TextGrid.
#'
#' @param tg TextGrid object
#' @param tierInd [optional] tier index or "name"
#'
#' @return numeric
#' @export
#' @seealso \code{\link{tg.getStartTime}}, \code{\link{tg.getTotalDuration}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getEndTime(tg)
#' tg.getEndTime(tg, "phone")
tg.getEndTime <- function(tg, tierInd = 0) {
    if (tbTools::isInt(tierInd) & tierInd == 0) {
        t <- as.numeric(class(tg)["tmax"])
        return(t)
    }

    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (tg.isPointTier(tg, tierInd)) {
        if (length(tg[[tierInd]]$t) < 1) {
            t <- NA
        } else {
            t <- tg[[tierInd]]$t[length(tg[[tierInd]]$t)]
        }
    } else if (tg.isIntervalTier(tg, tierInd)) {
        if (length(tg[[tierInd]]$t2) < 1) {
            t < NA
        } else {
            t <- tg[[tierInd]]$t2[length(tg[[tierInd]]$t2)]
        }
    } else {
        stop(paste0("Unknown tier type [tierInd = ", tierInd, "]"))
    }

    return(t)
}





#' tg.getTotalDuration
#'
#' Returns total duration. If tier index is specified, it returns duration
#' of the tier, if it is not specified, it returns total duration of the
#' TextGrid.
#'
#' @param tg TextGrid object
#' @param tierInd [optional] tier index or "name"
#'
#' @return numeric
#' @export
#' @seealso \code{\link{tg.getStartTime}}, \code{\link{tg.getEndTime}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getTotalDuration(tg)
#' tg.getTotalDuration(tg, "phone")
tg.getTotalDuration <- function(tg, tierInd = 0) {
    if (tbTools::isInt(tierInd) & tierInd == 0) {
        t <- tg.getEndTime(tg) - tg.getStartTime(tg)
        return(t)
    }

    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    t <- tg.getEndTime(tg, tierInd) - tg.getStartTime(tg, tierInd)
    return(t)
}



#' tg.getNumberOfTiers
#'
#' Returns number of tiers.
#'
#' @param tg TextGrid object
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getTierName}}, \code{\link{tg.isIntervalTier}}, \code{\link{tg.isPointTier}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getNumberOfTiers(tg)
tg.getNumberOfTiers <- function(tg) {
    ntiers <- length(tg)

    return(ntiers)
}




#' tg.getNumberOfPoints
#'
#' Returns number of points in the given point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getNumberOfIntervals}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getNumberOfPoints(tg, "phoneme")
tg.getNumberOfPoints <- function(tg, tierInd) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not PointTier."))
    }

    c <- length(tg[[tierInd]]$t)

    return(c)
}




#' tg.getNumberOfIntervals
#'
#' Returns number of intervals in the given interval tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getNumberOfPoints}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getNumberOfIntervals(tg, "phone")
tg.getNumberOfIntervals <- function(tg, tierInd) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not IntervalTier."))
    }

    c <- length(tg[[tierInd]]$t1)

    return(c)
}



#' tg.getLabel
#'
#' Return label of point or interval at the specified index.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of point or interval
#'
#' @return character string
#' @export
#' @seealso \code{\link{tg.setLabel}}, \code{\link{tg.countLabels}}
#'
#' @examples tg <- tg.sample()
#' tg.getLabel(tg, "phoneme", 4)
#' tg.getLabel(tg, "phone", 4)
tg.getLabel <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }


    if (tg.isIntervalTier(tg, tierInd)) {
        nint <- tg.getNumberOfIntervals(tg, tierInd)
        if (index < 1 | index > nint) {
            stop(paste0("Index out of range [index = ", index, ", nint = ", nint))
        }
    } else if (tg.isPointTier(tg, tierInd)) {
        npoints <- tg.getNumberOfPoints(tg, tierInd)
        if (index < 1 | index > npoints) {
            stop(paste0("Index out of range [index = ", index, ", npoints = ", npoints))
        }
    } else {
        stop("Unknown tier type")
    }

    lab <- tg[[tierInd]]$label[index]

    return(lab)
}



#' tg.setLabel
#'
#' Sets (changes) label of interval or point of the given index in the
#' interval or point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of interval or point
#' @param newLabel new "label"
#'
#' @export
#' @seealso \code{\link{tg.getLabel}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <- tg.setLabel(tg, "word", 3, "New Label")
#' tg.getLabel(tg2, "word", 3)
tg.setLabel <- function(tg, tierInd, index, newLabel) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tbTools::isString(newLabel)) {
        stop("newLabel must be a character string")
    }

    if (tg.isIntervalTier(tg, tierInd)) {
        nint <- tg.getNumberOfIntervals(tg, tierInd)
        if (index < 1 | index > nint) {
            stop(paste0("Index out of range [index = ", index, ", nint = ", nint))
        }
    } else if (tg.isPointTier(tg, tierInd)) {
        npoints <- tg.getNumberOfPoints(tg, tierInd)
        if (index < 1 | index > npoints) {
            stop(paste0("Index out of range [index = ", index, ", npoints = ", npoints))
        }
    } else {
        stop("Unknown tier type")
    }

    tgNew <- tg
    tgNew[[tierInd]]$label[index] <- newLabel

    return(tgNew)
}





#' tg.getIntervalStartTime
#'
#' Returns start time of interval in interval tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of interval
#'
#' @return numeric
#' @export
#' @seealso \code{\link{tg.getIntervalEndTime}}, \code{\link{tg.getIntervalDuration}}, \code{\link{tg.getIntervalIndexAtTime}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getIntervalStartTime(tg, "phone", 5)
tg.getIntervalStartTime <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("Tier ", tierInd, " is not IntervalTier."))
    }


    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index > nint) {
        stop(paste0("Index out of range [index = ", index, ", nint = ", nint, "]"))
    }

    t <- tg[[tierInd]]$t1[index]

    return(t)
}





#' tg.getIntervalEndTime
#'
#' Return end time of interval in interval tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of interval
#'
#' @return numeric
#' @export
#' @seealso \code{\link{tg.getIntervalStartTime}}, \code{\link{tg.getIntervalDuration}}, \code{\link{tg.getIntervalIndexAtTime}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getIntervalEndTime(tg, "phone", 5)
tg.getIntervalEndTime <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("Tier ", tierInd, " is not IntervalTier."))
    }


    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index > nint) {
        stop(paste0("Index out of range [index = ", index, ", nint = ", nint, "]"))
    }

    t <- tg[[tierInd]]$t2[index]

    return(t)
}




#' tg.getIntervalDuration
#'
#' Return duration (i.e., end - start time) of interval in interval tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of interval
#'
#' @return numeric
#' @export
#' @seealso \code{\link{tg.getIntervalStartTime}}, \code{\link{tg.getIntervalEndTime}}, \code{\link{tg.getIntervalIndexAtTime}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getIntervalDuration(tg, "phone", 5)
tg.getIntervalDuration <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("Tier ", tierInd, " is not IntervalTier."))
    }


    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index > nint) {
        stop(paste0("Index out of range [index = ", index, ", nint = ", nint, "]"))
    }

    t <- tg[[tierInd]]$t2[index] - tg[[tierInd]]$t1[index]

    return(t)
}




#' tg.getPointTime
#'
#' Return time of point at the specified index in point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of point
#'
#' @return numeric
#' @export
#' @seealso \code{\link{tg.getLabel}}, \code{\link{tg.getPointIndexNearestTime}}, \code{\link{tg.getPointIndexLowerThanTime}},
#' @seealso \code{\link{tg.getPointIndexHigherThanTime}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getPointTime(tg, "phoneme", 4)
tg.getPointTime <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0("Tier ", tierInd, " is not PointTier."))
    }

    npoints <- tg.getNumberOfPoints(tg, tierInd)
    if (index < 1 | index > npoints) {
        stop(paste0("Index out of range [index = ", index, ", npoints = ", npoints, "]"))
    }

    t <- tg[[tierInd]]$t[index]


    return(t)
}



#' tg.removeTier
#'
#' Removes tier of the given index.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertNewIntervalTier}}, \code{\link{tg.insertNewPointTier}}, \code{\link{tg.duplicateTier}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.plot(tg)
#' tg2 <- tg.removeTier(tg, "word")
#' tg.plot(tg2)
#' }
tg.removeTier <- function(tg, tierInd) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    tgNew <- tg

    for (I in tbTools::seqM(tierInd, ntiers - 1)) {
        tgNew[[I]] <- tgNew[[I+1]]
        names(tgNew)[I] <- names(tgNew)[I+1]
    }

    tgNew[[ntiers]] <- NULL


    return(tgNew)
}



#' tg.insertNewPointTier
#'
#' Inserts new point tier to the specified index (existing tiers are
#' shifted).
#'
#' @param tg TextGrid object
#' @param newInd new tier index (1 = the first)
#' @param newTierName new tier name
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertPoint}}, \code{\link{tg.insertNewIntervalTier}}, \code{\link{tg.duplicateTier}}, \code{\link{tg.removeTier}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg2 <- tg.insertNewPointTier(tg, 1, "POINTS")
#' tg2 <- tg.insertPoint(tg2, "POINTS", 3, "MY POINT")
#' tg.plot(tg2)
#' }
tg.insertNewPointTier <- function(tg, newInd, newTierName) {
    ntiers <- length(tg)

    if (!tbTools::isInt(newInd)) {
        stop("newInd must be integer >= 1")
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!tbTools::isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    tgNew <- tg

    tNew <- list(name = newTierName, type = "point", t = numeric(0), label = character(0))

    for (I in tbTools::seqM(ntiers+1, newInd+1, by = -1)) {
        tgNew[[I]] <- tgNew[[I-1]]
        names(tgNew)[I] <- names(tgNew)[I-1]
    }

    tgNew[[newInd]] <- tNew

    actNames <- names(tgNew)
    if (newTierName %in% actNames) {
        warning(paste0("TextGrid has a duplicate tier name [", newTierName, "]. You should not use the name for indexing to avoid ambiguity."))
    }
    names(tgNew)[newInd] <- newTierName

    return(tgNew)
}




#' tg.insertNewIntervalTier
#'
#' Inserts new interval tier to the specified index (existing tiers are
#' shifted). The new tier contains one empty interval from beginning to end.
#' Then, if we add new boundaries, this interval is divided to smaller
#' pieces.
#'
#' @param tg TextGrid object
#' @param newInd new tier index (1 = the first)
#' @param newTierName new tier name
#' @param tMin [optional] start time of the new tier
#' @param tMax [optional] end time of the new tier
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertInterval}}, \code{\link{tg.insertNewPointTier}}, \code{\link{tg.duplicateTier}}, \code{\link{tg.removeTier}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg2 <- tg.insertNewIntervalTier(tg, 1, "INTERVALS")
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.8)
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.1, "Interval A")
#' tg2 <- tg.insertInterval(tg2, "INTERVALS", 1.2, 2.5, "Interval B")
#' tg.plot(tg2)
#' }
tg.insertNewIntervalTier <- function(tg, newInd, newTierName, tMin=NA, tMax=NA) {
    ntiers <- length(tg)

    if (!tbTools::isInt(newInd)) {
        stop("newInd must be integer >= 1")
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!tbTools::isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    if (class(tMin) != "logical" & !tbTools::isNum(tMin)) {
        stop("tMin must be a number")
    }
    if (class(tMin) == "logical" & length(tMin) != 1) {
        stop("tMin must be a number")
    }
    if (!tbTools::isNum(tMin) & !is.na(tMin)) {
        stop("tMin must be a number")
    }

    if (class(tMax) != "logical" & !tbTools::isNum(tMax)) {
        stop("tMax must be a number")
    }
    if (class(tMax) == "logical" & length(tMax) != 1) {
        stop("tMax must be a number")
    }
    if (!tbTools::isNum(tMax) & !is.na(tMax)) {
        stop("tMax must be a number")
    }

    tgNew <- tg

    if (is.na(tMin)) {
        tMin <- as.numeric(class(tg)["tmin"])
        if (is.na(tMin)) {
            stop("TextGrid has undefined property tmin, you must set tMin parameter.")
        }
    } else {
       class(tgNew)["tmin"] <- min(as.numeric(class(tg)["tmin"]), tMin, na.rm = TRUE)
    }

    if (is.na(tMax)) {
        tMax <- as.numeric(class(tg)["tmax"])
        if (is.na(tMax)) {
            stop("TextGrid has undefined property tmax, you must set tMax parameter.")
        }
    } else {
        class(tgNew)["tmax"] <- max(as.numeric(class(tg)["tmax"]), tMax, na.rm = TRUE)
    }

    if (tMin > tMax) {
        stop(paste0("Cannot be: tMin > tMax [", as.character(tMin), " > ", as.character(tMax), "]"))
    }



    tNew <- list(name = newTierName, type = "interval", t1 = tMin, t2 = tMax, label = "")

    for (I in tbTools::seqM(ntiers+1, newInd+1, by = -1)) {
        tgNew[[I]] <- tgNew[[I-1]]
        names(tgNew)[I] <- names(tgNew)[I-1]
    }

    tgNew[[newInd]] <- tNew

    actNames <- names(tgNew)
    if (newTierName %in% actNames) {
        warning(paste0("TextGrid has a duplicate tier name [", newTierName, "]. You should not use the name for indexing to avoid ambiguity."))
    }
    names(tgNew)[newInd] <- newTierName

    return(tgNew)
}




#' tg.getIntervalIndexAtTime
#'
#' Returns index of interval which includes the given time, i.e.
#' tStart <= time < tEnd. Tier index must belong to interval tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in intervals
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getIntervalStartTime}}, \code{\link{tg.getIntervalEndTime}}, \code{\link{tg.getLabel}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getIntervalIndexAtTime(tg, "word", 0.5)
tg.getIntervalIndexAtTime <- function(tg, tierInd, time) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not IntervalTier."))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    nint <- length(tg[[tierInd]]$t1)
    for (I in tbTools::seqM(1, nint)) {
        if (tg[[tierInd]]$t1[I] <= time  &  time < tg[[tierInd]]$t2[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}



#' tg.getPointIndexHigherThanTime
#'
#' Returns index of point which is nearest the given time from right, i.e.
#' time <= pointTime. Tier index must belong to point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in points
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getPointIndexNearestTime}}, \code{\link{tg.getPointIndexLowerThanTime}}, \code{\link{tg.getLabel}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getPointIndexHigherThanTime(tg, "phoneme", 0.5)
tg.getPointIndexHigherThanTime <- function(tg, tierInd, time) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not PointTier."))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    for (I in tbTools::seqM(1, npoints)) {
        if (time <= tg[[tierInd]]$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}




#' tg.getPointIndexLowerThanTime
#'
#' Returns index of point which is nearest the given time from left, i.e.
#' pointTime <= time. Tier index must belong to point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in points
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getPointIndexNearestTime}}, \code{\link{tg.getPointIndexHigherThanTime}}, \code{\link{tg.getLabel}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getPointIndexLowerThanTime(tg, "phoneme", 0.5)
tg.getPointIndexLowerThanTime <- function(tg, tierInd, time) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not PointTier."))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    for (I in tbTools::seqM(npoints, 1, by = -1)) {
        if (time >= tg[[tierInd]]$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}




#' tg.getPointIndexNearestTime
#'
#' Returns index of point which is nearest the given time (from both sides).
#' Tier index must belong to point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in points
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getPointIndexLowerThanTime}}, \code{\link{tg.getPointIndexHigherThanTime}}, \code{\link{tg.getLabel}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getPointIndexNearestTime(tg, "phoneme", 0.5)
tg.getPointIndexNearestTime <- function(tg, tierInd, time) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not PointTier."))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    minDist <- Inf
    minInd <- NA

    for (I in tbTools::seqM(1, npoints)) {
        dist <- abs(tg[[tierInd]]$t[I] - time)
        if (dist < minDist) {
            minDist <- dist
            minInd <- I
        }
    }

    ind <- minInd


    return(ind)
}



#' tg.removePoint
#'
#' Remove point of the given index from the point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of point to be removed
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertPoint}}, \code{\link{tg.getNumberOfPoints}}, \code{\link{tg.removeIntervalBothBoundaries}}
#'
#' @examples
#' tg <- tg.sample()
#' tg$phoneme$label
#' tg2 <- tg.removePoint(tg, "phoneme", 1)
#' tg2$phoneme$label
tg.removePoint <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not PointTier."))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    npoints <- length(tg[[tierInd]]$t)

    if (index < 1 | index>npoints) {
        stop(paste0("index out of range [index = ", index, ", npoints = ", npoints, "]."))
    }


    tgNew <- tg
    for (I in tbTools::seqM(index, npoints - 1)) {
        tgNew[[tierInd]]$t[I] <- tgNew[[tierInd]]$t[I+1]
        tgNew[[tierInd]]$label[I] <- tgNew[[tierInd]]$label[I+1]
    }

    tgNew[[tierInd]]$t <- tgNew[[tierInd]]$t[-length(tgNew[[tierInd]]$t)]
    tgNew[[tierInd]]$label <- tgNew[[tierInd]]$label[-length(tgNew[[tierInd]]$label)]

    return(tgNew)
}




#' tg.insertPoint
#'
#' Inserts new point to point tier of the given index.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time of the new point
#' @param label time of the new point
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.removePoint}}, \code{\link{tg.insertInterval}}, \code{\link{tg.insertBoundary}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg2 <- tg.insertPoint(tg, "phoneme", 1.4, "NEW POINT")
#' tg.plot(tg2)
#' }
tg.insertPoint <- function(tg, tierInd, time, label) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not PointTier."))
    }

    if (!tbTools::isNum(time)) {
        stop("time must be a number.")
    }

    if (!tbTools::isString(label)) {
        stop("label must be a character string.")
    }


    tgNew <- tg

    indPosun <- tg.getPointIndexHigherThanTime(tg, tierInd, time)
    npoints <- length(tg[[tierInd]]$t)

    if (!is.na(indPosun)) {
        for (I in tbTools::seqM(npoints, indPosun, by = -1)) {
            tgNew[[tierInd]]$t[I+1] <- tgNew[[tierInd]]$t[I]
            tgNew[[tierInd]]$label[I+1] <- tgNew[[tierInd]]$label[I]
        }
    }


    if (is.na(indPosun)) {
        indPosun <- length(tgNew[[tierInd]]$t) + 1
    }

    tgNew[[tierInd]]$t[indPosun] <- time
    tgNew[[tierInd]]$label[indPosun] <- label

    class(tgNew)["tmin"] <- min(c(as.numeric(class(tgNew)["tmin"]), time), na.rm = TRUE)
    class(tgNew)["tmax"] <- max(c(as.numeric(class(tgNew)["tmax"]), time), na.rm = TRUE)

    return(tgNew)
}


#' tg.removeIntervalLeftBoundary
#'
#' Remove left boundary of the interval of the given index in Interval tier.
#' In fact, it concatenates two intervals into one (and their labels). It
#' cannot be applied to the first interval because it is the start boundary
#' of the tier.
#' E.g., we have interval 1-2-3, we remove the left boundary of the 2nd
#' interval, the result is two intervals 12-3.
#' If we do not want to concatenate labels, we have to set the label
#' to the empty string "" before this operation.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of the interval
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.removeIntervalRightBoundary}}, \code{\link{tg.removeIntervalBothBoundaries}}, \code{\link{tg.insertBoundary}}, \code{\link{tg.insertInterval}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.plot(tg)
#' tg2 <- tg.removeIntervalLeftBoundary(tg, "word", 3)
#' tg.plot(tg2)
#' }
tg.removeIntervalLeftBoundary <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not IntervalTier."))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index>nint) {
        stop(paste0("index out of range [index = ", index, ", nint = ", nint, "]."))
    }

    if (index == 1) {
        stop("Cannot remove left boundary of the first interval.")
    }

    t1 <- tg[[tierInd]]$t1[index-1]
    t2 <- tg[[tierInd]]$t2[index]
    lab <- paste0(tg[[tierInd]]$label[index-1], tg[[tierInd]]$label[index])

    tgNew <- tg
    for (I in tbTools::seqM(index, nint - 1)) {
        tgNew[[tierInd]]$t1[I] <- tgNew[[tierInd]]$t1[I+1]
        tgNew[[tierInd]]$t2[I] <- tgNew[[tierInd]]$t2[I+1]
        tgNew[[tierInd]]$label[I] <- tgNew[[tierInd]]$label[I+1]
    }

    tgNew[[tierInd]]$t1 <- tgNew[[tierInd]]$t1[-length(tgNew[[tierInd]]$t1)]
    tgNew[[tierInd]]$t2 <- tgNew[[tierInd]]$t2[-length(tgNew[[tierInd]]$t2)]
    tgNew[[tierInd]]$label <- tgNew[[tierInd]]$label[-length(tgNew[[tierInd]]$label)]

    tgNew[[tierInd]]$t1[index-1] <- t1
    tgNew[[tierInd]]$t2[index-1] <- t2
    tgNew[[tierInd]]$label[index-1] <- lab

    return(tgNew)
}






#' tg.removeIntervalRightBoundary
#'
#' Remove right boundary of the interval of the given index in Interval tier.
#' In fact, it concatenates two intervals into one (and their labels). It
#' cannot be applied to the last interval because it is the end boundary
#' of the tier.
#' E.g., we have interval 1-2-3, we remove the right boundary of the 2nd
#' interval, the result is two intervals 1-23.
#' If we do not want to concatenate labels, we have to set the label
#' to the empty string "" before this operation.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of the interval
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.removeIntervalLeftBoundary}}, \code{\link{tg.removeIntervalBothBoundaries}}, \code{\link{tg.insertBoundary}}, \code{\link{tg.insertInterval}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.plot(tg)
#' tg2 <- tg.removeIntervalRightBoundary(tg, "word", 3)
#' tg.plot(tg2)
#' }
tg.removeIntervalRightBoundary <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not IntervalTier."))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index>nint) {
        stop(paste0("index out of range [index = ", index, ", nint = ", nint, "]."))
    }

    if (index == nint) {
        stop("Cannot remove right boundary of the last interval.")
    }

    t1 <- tg[[tierInd]]$t1[index]
    t2 <- tg[[tierInd]]$t2[index+1]
    lab <- paste0(tg[[tierInd]]$label[index], tg[[tierInd]]$label[index+1])

    tgNew <- tg
    for (I in tbTools::seqM(index, nint - 1)) {
        tgNew[[tierInd]]$t1[I] <- tgNew[[tierInd]]$t1[I+1]
        tgNew[[tierInd]]$t2[I] <- tgNew[[tierInd]]$t2[I+1]
        tgNew[[tierInd]]$label[I] <- tgNew[[tierInd]]$label[I+1]
    }

    tgNew[[tierInd]]$t1 <- tgNew[[tierInd]]$t1[-length(tgNew[[tierInd]]$t1)]
    tgNew[[tierInd]]$t2 <- tgNew[[tierInd]]$t2[-length(tgNew[[tierInd]]$t2)]
    tgNew[[tierInd]]$label <- tgNew[[tierInd]]$label[-length(tgNew[[tierInd]]$label)]

    tgNew[[tierInd]]$t1[index] <- t1
    tgNew[[tierInd]]$t2[index] <- t2
    tgNew[[tierInd]]$label[index] <- lab

    return(tgNew)
}




#' tg.removeIntervalBothBoundaries
#'
#' Remove both left and right boundary of interval of the given index in
#' Interval tier. In fact, this operation concatenate three intervals into
#' one (and their labels). It cannot be applied to the first and the last
#' interval because they contain beginning or end boundary of the tier.
#' E.g., let's assume interval 1-2-3. We remove both boundaries of the
#' 2nd interval. The result is one interval 123.
#' If we do not want to concatenate labels (we wanted to remove the label
#' including its interval), we can set the label of the second interval
#' to the empty string "" before this operation.
#' If we only want to remove the label of interval "without concatenation",
#' i.e., the desired result is 1-empty-3, it is not this operation of
#' removing boundaries. Just set the label of the second interval to the
#' empty string "".
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param index index of the interval
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.removeIntervalLeftBoundary}}, \code{\link{tg.removeIntervalRightBoundary}}, \code{\link{tg.insertBoundary}}, \code{\link{tg.insertInterval}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.plot(tg)
#' tg2 <- tg.removeIntervalBothBoundaries(tg, "word", 3)
#' tg.plot(tg2)
#' }
tg.removeIntervalBothBoundaries <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not IntervalTier."))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index>nint) {
        stop(paste0("index out of range [index = ", index, ", nint = ", nint, "]."))
    }


    if (index == 1) {
        stop("Cannot remove left boundary of the first interval.")
    }
    if (index == nint) {
        stop("Cannot remove right boundary of the last interval.")
    }

    t1 <- tg[[tierInd]]$t1[index-1]
    t2 <- tg[[tierInd]]$t2[index+1]
    lab <- paste0(tg[[tierInd]]$label[index-1], tg[[tierInd]]$label[index], tg[[tierInd]]$label[index+1])

    tgNew <- tg
    for (I in tbTools::seqM(index, nint - 2)) {
        tgNew[[tierInd]]$t1[I] <- tgNew[[tierInd]]$t1[I+2]
        tgNew[[tierInd]]$t2[I] <- tgNew[[tierInd]]$t2[I+2]
        tgNew[[tierInd]]$label[I] <- tgNew[[tierInd]]$label[I+2]
    }

    tgNew[[tierInd]]$t1 <- tgNew[[tierInd]]$t1[-length(tgNew[[tierInd]]$t1)]
    tgNew[[tierInd]]$t2 <- tgNew[[tierInd]]$t2[-length(tgNew[[tierInd]]$t2)]
    tgNew[[tierInd]]$label <- tgNew[[tierInd]]$label[-length(tgNew[[tierInd]]$label)]
    tgNew[[tierInd]]$t1 <- tgNew[[tierInd]]$t1[-length(tgNew[[tierInd]]$t1)]
    tgNew[[tierInd]]$t2 <- tgNew[[tierInd]]$t2[-length(tgNew[[tierInd]]$t2)]
    tgNew[[tierInd]]$label <- tgNew[[tierInd]]$label[-length(tgNew[[tierInd]]$label)]

    tgNew[[tierInd]]$t1[index-1] <- t1
    tgNew[[tierInd]]$t2[index-1] <- t2
    tgNew[[tierInd]]$label[index-1] <- lab

    return(tgNew)
}



#' tg.insertBoundary
#'
#' Inserts new boundary into interval tier. This creates a new interval, to
#' which we can set the label (optional argument).
#'
#' There are more possible situations which influence where the new label
#' will be set.
#'
#' a) New boundary into the existing interval (the most common situation):
#'    The interval is splitted into two parts. The left preserves the label
#'    of the original interval, the right is set to the new (optional) label.
#'
#' b) On the left of existing interval (i.e., enlarging the tier size):
#'    The new interval starts with the new boundary and ends at the start
#'    of originally first existing interval. The label is set to the new
#'    interval.
#'
#' c) On the right of existing interval (i.e., enlarging the tier size):
#'    The new interval starts at the end of originally last existing
#'    interval and ends with the new boundary. The label is set to the new
#'    interval.
#'    This is somewhat different behaviour than in a) and b) where the new
#'    label is set to the interval which is on the right of the new
#'    boundary. In c), the new label is set on the left of the new boundary.
#'    But this is the only logical possibility.
#'
#' It is a nonsense to insert a boundary between existing intervals to a
#' position where there is no interval. This is against the basic logic of
#' Praat interval tiers where, at the beginning, there is one large empty
#' interval from beginning to the end. And then, it is divided to smaller
#' intervals by adding new boundaries. Nevertheless, if the TextGrid is
#' created by external programmes, you may rarely find such discontinuities.
#' In such a case, at first, use the tgRepairContinuity() function.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time of the new boundary
#' @param label [optional] label of the new interval
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertInterval}}, \code{\link{tg.removeIntervalLeftBoundary}}, \code{\link{tg.removeIntervalRightBoundary}}, \code{\link{tg.removeIntervalBothBoundaries}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <- tg.insertNewIntervalTier(tg, 1, "INTERVALS")
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.8)
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.1, "Interval A")
#' tg2 <- tg.insertInterval(tg2, "INTERVALS", 1.2, 2.5, "Interval B")
#' \dontrun{
#' tg.plot(tg2)
#' }
tg.insertBoundary <- function(tg, tierInd, time, label="") {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not IntervalTier."))
    }

    if (!tbTools::isNum(time)) {
        stop("time must be a number.")
    }

    if (!tbTools::isString(label)) {
        stop("label must be a character string.")
    }

    tgNew <- tg

    index <- tg.getIntervalIndexAtTime(tg, tierInd, time)
    nint <- tg.getNumberOfIntervals(tg, tierInd);

    if (nint == 0) {
        stop("Nonsense: tier [", tierInd, "] has 0 intervals.")
    }

    if (is.na(index)) {
        if (time > tg[[tierInd]]$t2[length(tg[[tierInd]]$t2)]) {  # situation c) On the right of existing interval
            tgNew[[tierInd]]$t1[nint+1] <- tg[[tierInd]]$t2[nint]
            tgNew[[tierInd]]$t2[nint+1] <- time
            tgNew[[tierInd]]$label[nint+1] <- label
            class(tgNew)["tmax"] <- max(c(as.numeric(class(tg)["tmax"]), time), na.rm = TRUE)
        } else if (time < tg[[tierInd]]$t1[1]) { # situation b) On the left of existing interval
            for (I in tbTools::seqM(nint, 1, by = -1)) {
                tgNew[[tierInd]]$t1[I+1] <- tgNew[[tierInd]]$t1[I]
                tgNew[[tierInd]]$t2[I+1] <- tgNew[[tierInd]]$t2[I]
                tgNew[[tierInd]]$label[I+1] <- tgNew[[tierInd]]$label[I]
            }
            tgNew[[tierInd]]$t1[1] <- time
            tgNew[[tierInd]]$t2[1] <- tgNew[[tierInd]]$t1[2]
            tgNew[[tierInd]]$label[1] <- label
            class(tgNew)["tmin"] <- min(c(as.numeric(class(tg)["tmin"]), time), na.rm = TRUE)
        } else if (time == tg[[tierInd]]$t2[length(tg[[tierInd]]$t2)]) {  # attempt to insert boundary exactly to the end of tier (nonsense)
            stop(paste0("Cannot insert boundary because one already exists at the same position [tierInd = ", tierInd, ", time = ", time, "]."))
        } else {
            stop("Nonsense: missing interval, even though time is between intervals. Please check continuity using tg.repairContinuity().")
        }
    } else { # situation a) New boundary into the existing interval
        for (I in tbTools::seqM(1, nint)) {
            if ((time %in% tgNew[[tierInd]]$t1) | (time %in% tgNew[[tierInd]]$t2)) {
                stop(paste0("Cannot insert boundary because one already exists at the same position [tierInd = ", tierInd, ", time = ", time, "]."))
            }
        }

        for (I in tbTools::seqM(nint, index+1, by = -1)) {
            tgNew[[tierInd]]$t1[I+1] <- tgNew[[tierInd]]$t1[I]
            tgNew[[tierInd]]$t2[I+1] <- tgNew[[tierInd]]$t2[I]
            tgNew[[tierInd]]$label[I+1] <- tgNew[[tierInd]]$label[I]
        }
        tgNew[[tierInd]]$t1[index] <- tg[[tierInd]]$t1[index]
        tgNew[[tierInd]]$t2[index] <- time
        tgNew[[tierInd]]$label[index] <- tg[[tierInd]]$label[index]
        tgNew[[tierInd]]$t1[index+1] <- time
        tgNew[[tierInd]]$t2[index+1] <- tg[[tierInd]]$t2[index]
        tgNew[[tierInd]]$label[index+1] <- label
    }


    return(tgNew)
}



#' tg.insertInterval
#'
#' Inserts new interval into an empty space in interval tier:
#' a) Into an already existing interval with empty label (most common
#' situation because, e.g., a new interval tier has one empty interval from
#' beginning to the end.
#' b) Outside og existing intervals (left or right), this may create another
#' empty interval between.
#'
#' In most cases, this function is the same as 1.) tgInsertBoundary(tEnd)
#' and 2.) tgInsertBoundary(tStart, "new label"). But, additional checks are
#' performed: a) tStart and tEnd belongs to the same empty interval, or
#' b) both times are outside of existings intervals (both left or both right).
#'
#' Intersection of the new interval with more already existing (even empty)
#' does not make a sense and is forbidden.
#'
#' In many situations, in fact, this function creates more than one interval.
#' E.g., let's assume an empty interval tier with one empty interval from 0 to 5 sec.
#' 1.) We insert a new interval from 1 to 2 with label "he".
#'     Result: three intervals, 0-1 "", 1-2 "he", 2-5 "".
#' 2.) Then, we insert an interval from 7 to 8 with label "lot".
#'     Result: five intervals, 0-1 "", 1-2 "he", 2-5 "", 5-7 "", 7-8 "lot"
#'     Note: the empty 5-7 "" interval is inserted because we are going
#'     outside of the existing tier.
#' 3.) Now, we insert a new interval exactly between 2 and 3 with label "said".
#'     Result: really only one interval is created (and only the right
#'     boundary is added because the left one already exists):
#'     0-1 "", 1-2 "he", 2-3 "said", 3-5 "", 5-7 "", 7-8 "lot".
#' 4.) After this, we want to insert another interval, 3 to 5: label "a".
#'     In fact, this does not create any new interval at all. Instead of
#'     that, it only sets the label to the already existing interval 3-5.
#'     Result: 0-1 "", 1-2 "he", 2-3 "said", 3-5 "a", 5-7 "", 7-8 "lot".
#'
#' This function is not implemented in Praat (6.0.14). And it is very useful
#' for adding separate intervals to an empty area in interval tier, e.g.,
#' result of voice activity detection algorithm.
#' On the other hand, if we want continuously add new consequential
#' intervals, tgInsertBoundary() may be more useful. Because, in the
#' tgInsertInterval() function, if we calculate both boundaries separately
#' for each interval, strange situations may happen due to numeric round-up
#' errors, like 3.14*5 != 15.7. In such cases, it may be hard to obtain
#' precisely consequential time instances. As 3.14*5 is slightly larger than
#' 15.7 (let's try to calculate 15.7 - 3.14*5), if you calculate tEnd of the
#' first interval as 3.14*5 and tStart of the second interval as 15.7, this
#' function refuse to create the second interval because it would be an
#' intersection. In the opposite case (tEnd of the 1st: 15.7, tStart of the
#' 2nd: 3.14*5), it would create another "micro" interval between these two
#' slightly different time instances. Instead of that, if you insert only
#' one boundary using the tgInsertBoundary() function, you are safe that
#' only one new interval is created. But, if you calculate the "15.7" (no
#' matter how) and store in the variable and then, use this variable in
#' the tgInsertInterval() function both for the tEnd of the 1st interval and
#' tStart of the 2nd interval, you are safe, it works fine.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param tStart start time of the new interval
#' @param tEnd end time of the new interval
#' @param label [optional] label of the new interval
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertBoundary}}, \code{\link{tg.removeIntervalLeftBoundary}}, \code{\link{tg.removeIntervalRightBoundary}}, \code{\link{tg.removeIntervalBothBoundaries}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <- tg.insertNewIntervalTier(tg, 1, "INTERVALS")
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.8)
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.1, "Interval A")
#' tg2 <- tg.insertInterval(tg2, "INTERVALS", 1.2, 2.5, "Interval B")
#' \dontrun{
#' tg.plot(tg2)
#' }
tg.insertInterval <- function(tg, tierInd, tStart, tEnd, label="") {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0("tier ", tierInd, " is not IntervalTier."))
    }

    if (!tbTools::isNum(tStart)) {
        stop("tStart must be a number.")
    }
    if (!tbTools::isNum(tEnd)) {
        stop("tEnd must be a number.")
    }
    if (tStart >= tEnd) {
        stop(paste0("tStart [", as.character(tStart), "] must be lower than tEnd [", as.character(tEnd), "]."))
    }
    # Note: thanks to this condition, some situations (which were solved below) cannot happen
    # (tStart == tEnd), thus it is easier. By the way, Praat does not allow to have 2 boundaries
    # in the same time instance, do it is fully compatible.

    if (!tbTools::isString(label)) {
        stop("label must be a character string.")
    }

    # tgNew <- tg

    nint <- length(tg[[tierInd]]$t1)
    if (nint == 0) {
        # Strange situation, tier does not have any single interval.
        tgNew <- tg
        tgNew[[tierInd]]$t1 <- tStart
        tgNew[[tierInd]]$t2 <- tEnd
        tgNew[[tierInd]]$label <- label
        class(tgNew)["tmin"] <- min(c(as.numeric(class(tgNew)["tmin"]), tStart), na.rm = TRUE)
        class(tgNew)["tmax"] <- max(c(as.numeric(class(tgNew)["tmax"]), tEnd), na.rm = TRUE)
        return(tgNew)
    }

    tgLeft <- tg[[tierInd]]$t1[1]
    tgRight <- tg[[tierInd]]$t2[length(tg[[tierInd]]$t2)]

    if (tStart < tgLeft & tEnd < tgLeft) {
        # cat("insert totally left + empty filling interval\n")
        tgNew <- tg.insertBoundary(tg, tierInd, tEnd)
        tgNew <- tg.insertBoundary(tgNew, tierInd, tStart, label)
        return(tgNew)
    } else if (tStart <= tgLeft & tEnd == tgLeft) {
        # cat("insert totally left, fluently connecting\n")
        tgNew <- tg.insertBoundary(tg, tierInd, tStart, label)
        return(tgNew)
    } else if (tStart < tgLeft & tEnd > tgLeft) {
        stop(paste0("Intersection of new interval (", as.character(tStart), " to ", as.character(tEnd), ' sec, "', label, '") and already existing intervals (region before "beginning" and also the first interval) is forbidden.'))
    } else if (tStart > tgRight & tEnd > tgRight) {
        # cat("insert totally right + empty filling interval\n")
        tgNew <- tg.insertBoundary(tg, tierInd, tEnd)
        tgNew <- tg.insertBoundary(tgNew, tierInd, tStart, label)
        return(tgNew)
    } else if (tStart == tgRight & tEnd >= tgRight) {
        # cat("insert totally right, fluently connecting\n")
        tgNew <- tg.insertBoundary(tg, tierInd, tEnd, label)
        return(tgNew)
    } else if (tStart < tgRight & tEnd > tgRight) {
        stop(paste0("Intersection of new interval (", as.character(tStart), " to ", as.character(tEnd), ' sec, "', label, '") and already existing intervals (the last interval and also the region after "end") is forbidden.'))
    } else if (tStart >= tgLeft & tEnd <= tgRight) {
        # cat("insert into an already existing area, we need a check: a) the same and b) empty interval\n")
        # Find all intervals, in which our times belongs - if we hit a boundary,
        # the time can belong to two intervals
        iStart <- integer(0)
        iEnd <- integer(0)
        for (I in tbTools::seqM(1, nint)) {
            if (dplyr::between(tStart, tg[[tierInd]]$t1[I], tg[[tierInd]]$t2[I])) {  # tStart >= tg[[tierInd]]$t1[I] & tStart <= tg[[tierInd]]$t2[I]
                iStart <- c(iStart, I)
            }
            if (dplyr::between(tEnd, tg[[tierInd]]$t1[I], tg[[tierInd]]$t2[I])) {  # tEnd >= tg[[tierInd]]$t1[I] & tEnd <= tg[[tierInd]]$t2[I]
                iEnd <- c(iEnd, I)
            }
        }

        if (!(length(iStart) == 1 & length(iEnd) == 1)) {
            inters <- intersect(iStart, iEnd) # nalezeni spolecneho intervalu z vice moznych variant
            if (length(inters) == 0) {
                # this is error but it is solved by the condition 'if (iStart == iEnd)' above
                iStart <- iStart[length(iStart)]
                iEnd <- iEnd[1]
            } else {
                iStart <- inters[1]
                iEnd <- inters[1]
                if (length(inters) > 1) { # attempt to find the first suitable candidate
                    for (I in tbTools::seqM(1, length(inters))) {
                        if (tg[[tierInd]]$label[inters[I]] == "") {
                            iStart <- inters[I]
                            iEnd <- inters[I]
                            break
                        }
                    }
                }
            }
        }

        if (iStart == iEnd) {
            if (tg[[tierInd]]$label[iStart] == "") {
                # cat("insert into an existing interval, the question is, concatenate or not?\n")
                t1 <- tg[[tierInd]]$t1[iStart]
                t2 <- tg[[tierInd]]$t2[iStart]
                if (tStart == t1 & tEnd == t2) {
                    # cat("only this: set label to existing empty interval\n");
                    tgNew <- tg
                    tgNew[[tierInd]]$label[iStart] <- label
                    return(tgNew)
                # } else if (tStart == t1 & tEnd == t1) {   # this cannot happen because of the condition 'if (iStart == iEnd)' above
                #    cat("set label to original interval and insert one boundary to t1, this creates a new zero-length interval at the start with a new label and the whole original interval will stay empty\n")
                # } else if (tStart == t2 & tEnd == t2) {   # this cannot happen because of the condition 'if (iStart == iEnd)' above
                #    cat("insert one boundary to t2 with new label, this ensures that the original empty interval stays as it is and it creates a new zero-length interval at the end with a new label\n")
                } else if (tStart == t1 & tEnd < t2) {
                    # cat("set a new label to the original interval and insert one new boundary to tEnd, it splits the original interval into two parts, the first will have new label, the second stays empty\n")
                    tgNew <- tg
                    tgNew[[tierInd]]$label[iStart] <- label
                    tgNew <- tg.insertBoundary(tgNew, tierInd, tEnd)
                    return(tgNew)
                } else if (tStart > t1 & tEnd == t2) {
                    # cat("insert one new boundary to tStart with a new label, it splits the original interval into two parts, the first stays empty and the second will have new label\n")
                    tgNew <- tg.insertBoundary(tg, tierInd, tStart, label)
                    return(tgNew)
                } else if (tStart > t1 & tEnd < t2) {
                    # cat("insert one boundary to tEnd with empty label and then insert another boundary to tStart with new label, it splits the original interval into three parts, the first and the third will be empty, the second will have new label\n")
                    tgNew <- tg.insertBoundary(tg, tierInd, tEnd)
                    tgNew <- tg.insertBoundary(tgNew, tierInd, tStart, label)
                } else {
                    stop("Error in author's logic. This cannot happen. Please, contact the author but be kind. He is really unhappy about this confusion.")
                }
            } else {
                stop(paste0("Insertion of new interval (", as.character(tStart), " to ", as.character(tEnd), ' sec, "', label, '") into the interval with unempty label (', as.character(tg[[tierInd]]$t1[iStart]), " to ", as.character(tg[[tierInd]]$t2[iStart]), ' sec, "', tg[[tierInd]]$label[iStart], '") is forbidden.'))
            }
        } else {
            stop(paste0("Intersection of new interval (", as.character(tStart), " to ", as.character(tEnd), ' sec, "', label, '") and more already existing (indexes ', iStart, " and ", iEnd, ") is forbidden."))
        }



    } else {
        stop("Error in author's logic. This cannot happen. Please, contact the author but be kind. He is really unhappy about this confusion.")
    }

    return(tgNew)
}






#' pt.read
#'
#' Reads PitchTier from Praat. Supported formats: text file, short text file,
#' spread sheet, headerless spread sheet (headerless not recommended,
#' it does not contain tmin and tmax info).
#'
#' @param fileNamePitchTier file name of PitchTier
#'
#' @return PitchTier object
#' @export
#' @seealso \code{\link{pt.write}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{pt.interpolate}}, \code{\link{tg.read}}
#'
#' @examples
#' \dontrun{
#' pt <- pt.read("demo/H.PitchTier")
#' pt.plot(pt)
#' }
pt.read <- function(fileNamePitchTier) {
    if (!tbTools::isString(fileNamePitchTier)) {
        stop("Invalid 'fileNamePitchTier' parameter.")
    }

    # fid <- file(fileNamePitchTier, open = "r", encoding = "UTF-8")
    # flines <- readLines(fid)
    flines <- readr::read_lines(fileNamePitchTier, locale = readr::locale(encoding = "UTF-8"))
    # close(fid)

    if (length(flines) < 1) {
        stop("Empty file.")
    }

    if (flines[1] == "\"ooTextFile\"") {    # spreadSheet
        if (length(flines) < 3) {
            stop("Unknown PitchTier format.")
        }

        if (flines[2] != "\"PitchTier\"") {
            stop("Unknown PitchTier format.")
        }

        fromToN <- stringr::str_split(flines[3], " ")
        if (length(fromToN[[1]]) != 3) {
            stop("Unknown PitchTier format.")
        }
        xmin <- as.numeric(fromToN[[1]][[1]])
        xmax <- as.numeric(fromToN[[1]][[2]])
        N <- as.integer(fromToN[[1]][[3]])

        if (N != (length(flines)-3)) {
            stop("Wrong number of points in PitchTier format.")
        }
        t <- numeric(N)
        f <- numeric(N)

        for (I in tbTools::seqM(1, N, by = 1)) {
            tf <- stringr::str_split(flines[I+3], "\\s")
            if (length(tf[[1]]) != 2) {
                stop("Unknown PitchTier format.")
            }
            t[I] <- as.numeric(tf[[1]][[1]])
            f[I] <- as.numeric(tf[[1]][[2]])
        }


    } else if (flines[1] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile
        if (length(flines) < 6) {
            stop("Unknown PitchTier format.")
        }

        if (flines[2] != "Object class = \"PitchTier\"") {
            stop("Unknown PitchTier format.")
        }

        if (flines[3] != "") {
            stop("Unknown PitchTier format.")
        }

        if (nchar(flines[4]) < 1) {
            stop("Unknown PitchTier format.")
        }

        if (stringr::str_sub(flines[4], 1, 1) == "x") {  # TextFile
            xmin <- readr::parse_number(flines[4])
            xmax <- readr::parse_number(flines[5])
            N <- readr::parse_number(flines[6])

            if (N != (length(flines)-6)/3) {
                stop("Wrong number of points in PitchTier format.")
            }
            t <- numeric(N)
            f <- numeric(N)

            for (I in tbTools::seqM(1, N, by = 1)) {
                t[I] <- readr::parse_number(flines[8 + (I-1)*3])
                f[I] <- readr::parse_number(flines[9 + (I-1)*3])
            }

        } else {   # shortTextFile

            xmin <- as.numeric(flines[4])
            xmax <- as.numeric(flines[5])
            N <- as.integer(flines[6])

            if (N != (length(flines)-6)/2) {
                stop("Wrong number of points in PitchTier format.")
            }
            t <- numeric(N)
            f <- numeric(N)

            for (I in tbTools::seqM(1, N, by = 1)) {
                t[I] <- as.numeric(flines[7 + (I-1)*2])
                f[I] <- as.numeric(flines[8 + (I-1)*2])
            }
        }


    } else {   # headerless SpreadSheet

        N <- length(flines)

        t <- numeric(N)
        f <- numeric(N)

        for (I in tbTools::seqM(1, N, by = 1)) {
            tf <- stringr::str_split(flines[I], "\\s")
            if (length(tf[[1]]) != 2) {
                stop("Unknown PitchTier format.")
            }
            t[I] <- as.numeric(tf[[1]][[1]])
            f[I] <- as.numeric(tf[[1]][[2]])
        }

        xmin <- min(t)
        xmax <- max(t)
    }


    pt <- list(t = t, f = f, tmin = xmin, tmax = xmax)

    return(pt)
}



#' pt.write
#'
#' Saves PitchTier to file (spread sheet file format).
#' pt is list with at least $t and $f vectors (of the same length).
#' If there are no $tmin and $tmax values, there are
#' set as min and max of $t vector.
#'
#' @param pt PitchTier object
#' @param fileNamePitchTier file name to be created
#'
#' @export
#' @seealso \code{\link{pt.read}}, \code{\link{tg.write}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.interpolate}}
#'
#' @examples
#' \dontrun{
#' pt <- pt.sample()
#' pt <- pt.Hz2ST(pt)    #  conversion of Hz to Semitones, reference 0 ST = 100 Hz.
#' pt.plot(pt)
#' pt.write(pt, "demo/H_st.PitchTier")
#' }
pt.write <- function(pt, fileNamePitchTier) {
    if (!tbTools::isString(fileNamePitchTier)) {
        stop("Invalid 'fileNamePitchTier' parameter.")
    }

    if (!("t" %in% names(pt))) {
        stop("pt must be a list with 't' and 'f' and optionally 'tmin' and 'tmax'")
    }
    if (!("f" %in% names(pt))) {
        stop("pt must be a list with 't' and 'f' and optionally 'tmin' and 'tmax'")
    }
    if (length(pt$t) != length(pt$f)) {
        stop("t and f lengths mismatched.")
    }
    N <- length(pt$t)


    if (!("tmin" %in% names(pt))) {
        xmin <- min(pt$t)
    } else {
        xmin <- pt$tmin
    }
    if (!("tmax" %in% names(pt))) {
        xmax <- max(pt$t)
    } else {
        xmax <- pt$tmax
    }


    fid <- file(fileNamePitchTier, open = "w", encoding = "UTF-8")
    if (!isOpen(fid)) {
        stop(paste0("cannot open file [", fileNamePitchTier, "]"))
    }

    writeLines('"ooTextFile"', fid)
    writeLines('"PitchTier"', fid)
    writeLines(paste0(as.character(tbTools::round2(xmin, -20)), " ", as.character(tbTools::round2(xmax, -20)), " ", as.character(N)), fid)

    for (n in tbTools::seqM(1, N)) {
        writeLines(paste0(as.character(tbTools::round2(pt$t[n], -20)), "\t", as.character(tbTools::round2(pt$f[n], -20))), fid)
    }

    close(fid)
}


#' pt.plot
#'
#' Plots interactive PitchTier using dygraphs package.
#'
#' @param pt PitchTier object
#' @param group [optional] character string, name of group for dygraphs synchronization
#'
#' @export
#' @seealso \code{\link{pt.read}}, \code{\link{tg.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{pt.interpolate}}, \code{\link{pt.write}}
#'
#' @examples
#' \dontrun{
#' pt <- pt.sample()
#' pt.plot(pt)
#' }
pt.plot <- function(pt, group = "") {
    data <- list(t = pt$t, f = pt$f)

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    g <- dygraphs::dyOptions(g, drawPoints = TRUE, pointSize = 2, strokeWidth = 0)
    g <- dygraphs::dyRangeSelector(g, dateWindow = c(pt$tmin, pt$tmax))

    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    g
}






#' pt.interpolate
#'
#' Interpolates PitchTier contour in given time instances.
#'
#'  a) If t < min(pt$t) (or t > max(pt$t)), returns the first (or the last) value of pt$f.
#'  b) If t is existing point in pt$t, returns the respective pt$f.
#'  c) If t is Between two existing points, returns linear interpolation of these two points.
#'
#' @param pt PitchTier object
#' @param t vector of time instances of interest
#'
#' @return PitchTier object
#' @export
#' @seealso \code{\link{pt.read}}, \code{\link{pt.write}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{pt.legendre}}
#'
#' @examples
#' pt <- pt.sample()
#' pt <- pt.Hz2ST(pt, ref = 100)  # conversion of Hz to Semitones, reference 0 ST = 100 Hz.
#' pt2 <- pt.interpolate(pt, seq(pt$t[1], pt$t[length(pt$t)], by = 0.001))
#' \dontrun{
#' pt.plot(pt)
#' pt.plot(pt2)
#' }
pt.interpolate <- function(pt, t) {
    if (class(t) != "numeric"  &  class(t) != "integer") {
        stop("t must be numeric vector")
    }

    if (length(pt$t) != length(pt$f))
        stop("PitchTier does not have equal length vectors $t and $f")

    if (length(pt$t) < 1)
        return(NA)

    if (!identical(sort(pt$t), pt$t)) {
        stop("time instances $t in PitchTier are not increasingly sorted")
    }

    if (!identical(unique(pt$t), pt$t)) {
        stop("duplicated time instances in $t vector of the PitchTier")
    }

    pt2 <- pt
    pt2$t <- t

    f <- numeric(length(t))
    for (I in seq_along(t)) {
        if (length(pt$t) == 1) {
            f[I] <- pt$f[1]
        } else if (t[I] < pt$t[1]) {   # a)
            f[I] <- pt$f[1]
        } else if (t[I] > pt$t[length(pt$t)]) {   # a)
            f[I] <- pt$f[length(pt$t)]
        } else {
            # b)
            ind <- which(pt$t == t[I])
            if (length(ind) == 1) {
                f[I] <- pt$f[ind]
            } else {
                # c)
                ind2 <- which(pt$t > t[I]); ind2 <- ind2[1]
                ind1 <- ind2 - 1
                # y = ax + b;  a = (y2-y1)/(x2-x1);  b = y1 - ax1
                a <- (pt$f[ind2] - pt$f[ind1]) / (pt$t[ind2] - pt$t[ind1])
                b <- pt$f[ind1] - a*pt$t[ind1]
                f[I] <- a*t[I] + b
            }
        }
    }

    pt2$f <- f
    return(pt2)
}




#' pt.Hz2ST
#'
#' Converts Hz to Semitones with given reference (default 0 ST = 100 Hz).
#'
#' @param pt PitchTier object
#' @param ref reference value (in Hz) for 0 ST. Default: 100 Hz.
#'
#' @return PitchTier object
#' @export
#' @seealso \code{\link{pt.read}}, \code{\link{pt.write}}, \code{\link{pt.plot}}, \code{\link{pt.interpolate}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}
#'
#' @examples
#' pt <- pt.sample()
#' pt2 <- pt.Hz2ST(pt, ref = 200)
#' \dontrun{
#' pt.plot(pt)  %>% dygraphs::dyAxis("y", label = "Frequency (Hz)")
#' pt.plot(pt2) %>% dygraphs::dyAxis("y", label = "Frequency (ST)")
#' }
pt.Hz2ST <- function(pt, ref=100) {
    if (!tbTools::isNum(ref) | ref <= 0) {
        stop("ref must be a positive number.")
    }

    pt$f <- 12*log(pt$f/ref) / log(2)
    return(pt)
}


#' pt.legendre
#'
#' Interpolate the PitchTier in 'npoints' equidistant points and approximate it by Legendre polynomials
#'
#' @param pt PitchTier object
#' @param npoints Number of points of PitchTier interpolation
#' @param npolynomials Number of polynomials to be used for Legendre modelling
#'
#' @return Vector of Legendre polynomials
#' @export
#' @seealso \code{\link{pt.legendreSynth}}, \code{\link{pt.legendreDemo}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{pt.read}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.interpolate}}
#'
#' @examples
#' pt <- pt.sample()
#' pt <- pt.Hz2ST(pt)
#' pt <- pt.cut(pt, tStart = 3)  # cut PitchTier from t = 3 sec and preserve time
#' c <- pt.legendre(pt)
#' print(c)
#' leg <- pt.legendreSynth(c)
#' ptLeg <- pt
#' ptLeg$t <- seq(ptLeg$tmin, ptLeg$tmax, length.out = length(leg))
#' ptLeg$f <- leg
#' \dontrun{
#' plot(pt$t, pt$f, xlab = "Time (sec)", ylab = "F0 (ST re 100 Hz)")
#' lines(ptLeg$t, ptLeg$f, col = "blue")
#' }
pt.legendre <- function(pt, npoints = 1000, npolynomials = 4) {
    if (!tbTools::isInt(npoints) | npoints < 0) {
        stop("npoints must be integer >= 0.")
    }

    if (!tbTools::isInt(npolynomials) | npolynomials <= 0) {
        stop("npolynomials must be integer > 0.")
    }

    pt <- pt.interpolate(pt, seq(pt$tmin, pt$tmax, length.out = npoints))

    y <- pt$f


    lP <- npoints # počet vzorků polynomu
    nP <- npolynomials

    B <- matrix(nrow = nP, ncol = lP)  # báze
    x <- seq(-1, 1, length.out = lP)

    for (i in tbTools::seqM(1, nP)) {
        n <- i - 1
        p <- numeric(lP)
        for (k in tbTools::seqM(0, n)) {
            p <- p + x^k*choose(n, k)*choose((n+k-1)/2, n)
        }
        p <- p*2^n

        B[i, ] <- p
    }

    c <- numeric(nP)
    for (I in 1: nP) {
        c[I] <- t(matrix(y)) %*% matrix(B[I, ], nrow = lP, ncol = 1) / lP * ((I-1)*2+1)
        # koeficient ((I-1)*2+1) odpovídá výkonům komponent, které lze spočítat i takto: mean((P.^2).')
    }

    return(c)
}

#' pt.legendreSynth
#'
#' Synthetize the contour from vector of Legendre polynomials 'c' in 'npoints' equidistant points
#'
#' @param c Vector of Legendre polynomials
#' @param npoints Number of points of PitchTier interpolation
#'
#' @return Vector of values of synthetized contour
#' @export
#' @seealso \code{\link{pt.legendre}}, \code{\link{pt.legendreDemo}}, \code{\link{pt.read}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.interpolate}}
#'
#' @examples
#' pt <- pt.sample()
#' pt <- pt.Hz2ST(pt)
#' pt <- pt.cut(pt, tStart = 3)  # cut PitchTier from t = 3 sec and preserve time
#' c <- pt.legendre(pt)
#' print(c)
#' leg <- pt.legendreSynth(c)
#' ptLeg <- pt
#' ptLeg$t <- seq(ptLeg$tmin, ptLeg$tmax, length.out = length(leg))
#' ptLeg$f <- leg
#' \dontrun{
#' plot(pt$t, pt$f, xlab = "Time (sec)", ylab = "F0 (ST re 100 Hz)")
#' lines(ptLeg$t, ptLeg$f, col = "blue")
#' }
pt.legendreSynth <- function(c, npoints = 1000) {
    if (class(c) != "numeric"  &  class(c) != "integer") {
        stop("c must be numeric vector")
    }

    if (!tbTools::isInt(npoints) | npoints < 0) {
        stop("npoints must be integer >= 0.")
    }

    lP <- npoints # počet vzorků polynomu
    nP <- length(c)

    B <- matrix(nrow = nP, ncol = lP)  # báze
    x <- seq(-1, 1, length.out = lP)

    for (i in tbTools::seqM(1, nP)) {
        n <- i - 1
        p <- numeric(lP)
        for (k in tbTools::seqM(0, n)) {
            p <- p + x^k*choose(n, k)*choose((n+k-1)/2, n)
        }
        p <- p*2^n

        B[i, ] <- p
    }

    if (nP > 0) {
        yModelovane <- t(matrix(c)) %*% B
    }
    else {
        yModelovane <- rep(NA, npoints)
    }

    return(as.numeric(yModelovane))
}

#' pt.legendreDemo
#'
#' Plots first four Legendre polynomials
#'
#' @export
#' @seealso \code{\link{pt.legendre}}, \code{\link{pt.legendreSynth}}, \code{\link{pt.read}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.interpolate}}
#'
#' @examples
#' \dontrun{
#' pt.legendreDemo()
#' }
pt.legendreDemo <- function() {
    graphics::par(mfrow = c(2, 2))
    graphics::plot(pt.legendreSynth(c(1, 0, 0, 0), 1024))
    graphics::plot(pt.legendreSynth(c(0, 1, 0, 0), 1024))
    graphics::plot(pt.legendreSynth(c(0, 0, 1, 0), 1024))
    graphics::plot(pt.legendreSynth(c(0, 0, 0, 1), 1024))
    graphics::par(mfrow = c(1, 1))
}


#' pt.cut
#'
#' Cut the specified interval from the PitchTier and preserve time
#'
#' @param pt PitchTier object
#' @param tStart beginning time of interval to be cut (default -Inf = cut from the tMin of the PitchTier)
#' @param tEnd final time of interval to be cut (default Inf = cut to the tMax of the PitchTier)
#'
#' @return PitchTier object
#' @export
#' @seealso \code{\link{pt.cut0}}, \code{\link{pt.read}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.interpolate}}, \code{\link{pt.legendre}}, \code{\link{pt.legendreSynth}}, \code{\link{pt.legendreDemo}}
#'
#' @examples
#' pt <- pt.sample()
#' pt2 <-   pt.cut(pt,  tStart = 3)
#' pt2_0 <- pt.cut0(pt, tStart = 3)
#' pt3 <-   pt.cut(pt,  tStart = 2, tEnd = 3)
#' pt3_0 <- pt.cut0(pt, tStart = 2, tEnd = 3)
#' pt4 <-   pt.cut(pt,  tEnd = 1)
#' pt4_0 <- pt.cut0(pt, tEnd = 1)
#' pt5 <-   pt.cut(pt,  tStart = -1, tEnd = 1)
#' pt5_0 <- pt.cut0(pt, tStart = -1, tEnd = 1)
#' \dontrun{
#' pt.plot(pt)
#' pt.plot(pt2)
#' pt.plot(pt2_0)
#' pt.plot(pt3)
#' pt.plot(pt3_0)
#' pt.plot(pt4)
#' pt.plot(pt4_0)
#' pt.plot(pt5)
#' pt.plot(pt5_0)
#' }
pt.cut <- function(pt, tStart = -Inf, tEnd = Inf) {
    if (!tbTools::isNum(tStart)) {
        stop("tStart must be a number.")
    }
    if (!tbTools::isNum(tEnd)) {
        stop("tEnd must be a number.")
    }
    if (is.infinite(tStart) & tStart>0) {
        stop("infinite tStart can be negative only")
    }
    if (is.infinite(tEnd) & tEnd<0) {
        stop("infinite tEnd can be positive only")
    }

    pt2 <- pt
    pt2$t <- pt$t[pt$t >= tStart  &  pt$t <= tEnd]
    pt2$f <- pt$f[pt$t >= tStart  &  pt$t <= tEnd]

    if (is.infinite(tStart)) {
        pt2$tmin <- pt$tmin
    } else {
        pt2$tmin <- tStart
    }

    if (is.infinite(tEnd)) {
        pt2$tmax <- pt$tmax
    } else {
        pt2$tmax <- tEnd
    }

    return(pt2)
}

#' pt.cut0
#'
#' Cut the specified interval from the PitchTier and shift time so that the new tmin = 0
#'
#' @param pt PitchTier object
#' @param tStart beginning time of interval to be cut (default -Inf = cut from the tMin of the PitchTier)
#' @param tEnd final time of interval to be cut (default Inf = cut to the tMax of the PitchTier)
#'
#' @return PitchTier object
#' @export
#' @seealso \code{\link{pt.cut}}, \code{\link{pt.read}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.interpolate}}, \code{\link{pt.legendre}}, \code{\link{pt.legendreSynth}}, \code{\link{pt.legendreDemo}}
#'
#' @examples
#' pt <- pt.sample()
#' pt2 <-   pt.cut(pt,  tStart = 3)
#' pt2_0 <- pt.cut0(pt, tStart = 3)
#' pt3 <-   pt.cut(pt,  tStart = 2, tEnd = 3)
#' pt3_0 <- pt.cut0(pt, tStart = 2, tEnd = 3)
#' pt4 <-   pt.cut(pt,  tEnd = 1)
#' pt4_0 <- pt.cut0(pt, tEnd = 1)
#' pt5 <-   pt.cut(pt,  tStart = -1, tEnd = 1)
#' pt5_0 <- pt.cut0(pt, tStart = -1, tEnd = 1)
#' \dontrun{
#' pt.plot(pt)
#' pt.plot(pt2)
#' pt.plot(pt2_0)
#' pt.plot(pt3)
#' pt.plot(pt3_0)
#' pt.plot(pt4)
#' pt.plot(pt4_0)
#' pt.plot(pt5)
#' pt.plot(pt5_0)
#' }
pt.cut0 <- function(pt, tStart = -Inf, tEnd = Inf) {
    if (!tbTools::isNum(tStart)) {
        stop("tStart must be a number.")
    }
    if (!tbTools::isNum(tEnd)) {
        stop("tEnd must be a number.")
    }
    if (is.infinite(tStart) & tStart>0) {
        stop("infinite tStart can be negative only")
    }
    if (is.infinite(tEnd) & tEnd<0) {
        stop("infinite tEnd can be positive only")
    }

    pt2 <- pt
    pt2$t <- pt$t[pt$t >= tStart  &  pt$t <= tEnd]
    pt2$f <- pt$f[pt$t >= tStart  &  pt$t <= tEnd]

    if (is.infinite(tStart)) {
        pt2$tmin <- pt$tmin
    } else {
        pt2$tmin <- tStart
    }

    if (is.infinite(tEnd)) {
        pt2$tmax <- pt$tmax
    } else {
        pt2$tmax <- tEnd
    }

    pt2$t <- pt2$t - pt2$tmin
    pt2$tmax <- pt2$tmax - pt2$tmin
    pt2$tmin <- 0

    return(pt2)
}




# tierInd <- tg.checkTierInd(tg, tierInd)
#     ntiers <- length(tg)



# if (!tbTools::isString(name)) {
#     stop("Name must be a character string.")
# }

# if (!tbTools::isNum(tMin)) {
#     stop("tMin must be a number.")
# }

# if (!tbTools::isInt(newInd)) {
#     stop("newInd must be integer >= 1.")
# }

