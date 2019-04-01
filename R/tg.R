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

        if (!isInt(tierInd)) {
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
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.write}}, \code{\link{tg.plot}}, \code{\link{tg.repairContinuity}}, \code{\link{tg.createNewTextGrid}}, \code{\link{tg.findLabels}}, \code{\link{tg.duplicateTierMergeSegments}}, \code{\link{pt.read}}, \code{\link{pitch.read}}, \code{\link{formant.read}}, \code{\link{it.read}}, \code{\link{col.read}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.read("demo/H.TextGrid")
#' tg.plot(tg)
#' }
tg.read <- function(fileNameTextGrid, encoding = "UTF-8") {
    if (!isString(fileNameTextGrid)) {
        stop("Invalid 'fileNameTextGrid' parameter.")
    }

    if (!isString(encoding)) {
        stop("Invalid 'encoding' parameter.")
    }

    if (encoding == "auto") {
        encoding <- detectEncoding(fileNameTextGrid)
    }

    if (encoding == "UTF-8") {
        flines <- readr::read_lines(fileNameTextGrid, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
    } else {
        fid <- file(fileNameTextGrid, open = "r", encoding = encoding)
        flines <- readLines(fid)   # does not work with tests/testthat/utf8.TextGrid  :-(
        close(fid)
    }

    flines <- enc2utf8(flines)

    find <- 4   # index of line to read, we ignore the first three

    tg_ind <- tg.read_lines(flines, find)
    class(tg_ind[[1]])["type"] <- "TextGrid"
    class(tg_ind[[1]])["name"] <- basename(fileNameTextGrid)
    return(tg_ind[[1]])
}


tg.read_lines <- function(flines, find) {
    tg <- list()  # new textgrid

    xminStr <- flines[find]; find <- find + 1 # xmin
    xmaxStr <- flines[find]; find <- find + 1; # xmax

    r <- flines[find]; find <- find + 1; # either "<exists>" -> shorttext or "tiers? <exists> " -> full text format

    if (str_contains(r, "tiers?")) {
        shortFormat <- FALSE
    } else if (str_contains(r, '<exists>')) {
        shortFormat <- TRUE
    } else {
        stop("Unknown textgrid format.")
    }

    if (shortFormat) {
        xmin <- as.numeric(xminStr) # xmin
        xmax <- as.numeric(xmaxStr) # xmax
    } else {
        xmin <- as.numeric(substr(strTrim(xminStr), 8, nchar(xminStr))) # xmin
        xmax <- as.numeric(substr(strTrim(xmaxStr), 8, nchar(xmaxStr))) # xmax
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

    for (tier in seqM(1, nTiers)) {

        if (shortFormat) {
            typ <- flines[find]; find <- find + 1
        } else {
            r <- strTrim(flines[find]); find <- find + 1

            while (substr(r, 1, 4) == "item") {
                r <- strTrim(flines[find]); find <- find + 1
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
                r <- strTrim(r);
                tierName <- substr(r, 9, nchar(r)-1)
            }
            tierType <- "interval"

            find <- find + 2; # ignore xmin and xmax

            if (shortFormat) {
                nIntervals <- as.numeric(flines[find]); find <- find + 1
            } else {
                r <- strTrim(flines[find]); find <- find + 1
                nIntervals <- as.numeric(substr(r, 19, nchar(r)))
            }

            tierT1 <- numeric(0)
            tierT2 <- numeric(0)
            tierLabel <- character(0)

            for (I in seqM(1, nIntervals)) {
                if (!shortFormat) {
                    r <- flines[find]; find <- find + 1 # ignore line intervals [..]:
                }

                if (shortFormat) {
                    t <- as.numeric(flines[find]); find <- find + 1
                    t2 <- as.numeric(flines[find]); find <- find + 1
                } else {
                    r1 <- strTrim(flines[find]); find <- find + 1
                    r2 <- strTrim(flines[find]); find <- find + 1
                    if ( (substr(r1, 1, 7) != "xmin = ")  ||  (substr(r2, 1, 7) != "xmax = ") ) {
                        stop("Unknown textgrid format");
                    }
                    t <-  as.numeric(substr(r1, 8, nchar(r1)))
                    t2 <- as.numeric(substr(r2, 8, nchar(r2)))
                }

                r <- flines[find]; find <- find + 1;
                if (!shortFormat) {
                    if (!str_contains(r, 'text = "')) {
                        stop("Unknown textgrid format");
                    }
                    rind <- str_find1(r, '"')
                    nQuotationMarks <- length(str_find(r, '"'))  # in Matlab: sum(r == '"')
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
                nQuotationMarks <- length(str_find(r, '"'))
                label <- substr(r, 2, nchar(r))
                if ((nQuotationMarks %% 2) == 1) {
                    label <- paste0(label, "\n")

                    repeat {
                        r <- flines[find]; find <- find + 1
                        nQuotationMarks <- length(str_find(r, '"'))
                        if (!shortFormat) {
                            if ((nQuotationMarks %% 2 == 1) & !sppasFormat) { # remove whitespace at the end of line, it is only in the case of odd number of quotation marks
                                r <- substr(r, 1, nchar(r)-1)
                            }
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
                r <- strTrim(r)
                tierName <- substr(r, 9, nchar(r)-1)
            }
            tierType <- "point"

            find <- find + 2 # ignore xmin and xmax

            if (shortFormat) {
                nIntervals <- as.numeric(flines[find]); find <- find + 1
            } else {
                r <- strTrim(flines[find]); find <- find + 1
                nIntervals <- as.numeric(substr(r, 16, nchar(r)))
            }

            tierT <- numeric(0)
            tierLabel <- character(0)

            for (I in seqM(1, nIntervals)) {
                if (!shortFormat) {
                    r <- flines[find]; find <- find + 1 # ignore line points [..]:
                }

                if (shortFormat) {
                    t <- as.numeric(flines[find]); find <- find + 1
                } else {
                    r <- strTrim(flines[find]); find <- find + 1
                    if (substr(r, 1, 9) != "number = ") {
                        stop("Unknown textgrid format");
                    }
                    t <- as.numeric(substr(r, 10, nchar(r)))
                }

                r <- flines[find]; find <- find + 1
                if (!shortFormat) {
                    if (!str_contains(r, 'mark = "')) {
                        stop("Unknown textgrid format");
                    }
                    rind <- str_find1(r, '"')
                    nQuotationMarks <- length(str_find(r, '"'))
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
                nQuotationMarks <- length(str_find(r, '"'))
                label <- substr(r, 2, nchar(r))
                if (nQuotationMarks %% 2 == 1) {
                    label <- paste0(label, "\n")
                    repeat {
                        r <- flines[find]; find <- find + 1
                        nQuotationMarks <- length(str_find(r, '"'))
                        if (!shortFormat) {
                            if ((nQuotationMarks %% 2 == 1) & !sppasFormat) { # remove whitespace at the end of line, it is only in the case of odd number of quotation marks
                                r <- substr(r, 1, nchar(r)-1)
                            }
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

    return(list(tg, find))
}

#' tg.write
#'
#' Saves TextGrid to the file. TextGrid may contain both interval and point
#' tiers (\code{tg[[1]]}, \code{tg[[2]]}, \code{tg[[3]]}, etc.). If tier type is not specified in \code{$type},
#' is is assumed to be \code{"interval"}. If specified, \code{$type} have to be \code{"interval"} or \code{"point"}.
#' If there is no \code{class(tg)["tmin"]} and \code{class(tg)["tmax"]}, they are calculated as min and max of
#' all tiers. The file is saved in UTF-8 encoding.
#'
#' @param tg TextGrid object
#' @param fileNameTextGrid Output file name
#' @param format Output file format (\code{"short"} (default, short text format) or \code{"text"} (a.k.a. full text format))
#'
#' @export
#' @seealso \code{\link{tg.read}}, \code{\link{pt.write}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.write(tg, "demo_output.TextGrid")
#' }
tg.write <- function(tg, fileNameTextGrid, format = "short") {
    tg.write0(tg, fileNameTextGrid, format)
}

tg.write0 <- function(tg, fileNameTextGrid, format = "short", fid = NULL, collection = FALSE) {
    if (!isString(fileNameTextGrid)) {
        stop("Invalid 'fileNameTextGrid' parameter.")
    }

    if (!isString(format)) {
        stop("Invalid 'format' parameter.")
    }
    if (format != "short" && format != "text") {
        stop("Unsupported format (supported: short [default], text)")
    }


    nTiers <- length(tg)  # number of Tiers

    minTimeTotal <-  NaN
    maxTimeTotal <-  NaN
    if ("tmin" %in% names(class(tg))  &  "tmax" %in% names(class(tg))) {
        minTimeTotal <- as.numeric(class(tg)["tmin"])
        maxTimeTotal <- as.numeric(class(tg)["tmax"])
    }

    for (I in seqM(1, nTiers)) {
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

    if (!collection) {
        fid <- file(fileNameTextGrid, open = "wb", encoding = "UTF-8")
        if (!isOpen(fid)) {
            stop(paste0("cannot open file [", fileNameTextGrid, "]"))
        }
    }

    if (!collection) {
        wrLine('File type = "ooTextFile"', fid)
        wrLine('Object class = "TextGrid"', fid)
        wrLine("", fid)
    }

    if (format == "short") {
        wrLine(as.character(round2(minTimeTotal, -15)), fid)  # min time from all tiers
        wrLine(as.character(round2(maxTimeTotal, -15)), fid)  # max time from all tiers
        wrLine("<exists>", fid)
        wrLine(as.character(nTiers), fid)
    } else if (format == "text") {
        wrLine(paste0("xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid, collection)  # min time from all tiers
        wrLine(paste0("xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid, collection)  # max time from all tiers
        wrLine("tiers? <exists> ", fid, collection)
        wrLine(paste0("size = ", as.character(nTiers), " "), fid, collection)
        wrLine("item []: ", fid, collection)
    }

    for (N in seqM(1, nTiers)) {
        if (format == "text") {
            wrLine(paste0("    item [", as.character(N), "]:"), fid, collection)
        }

        if (tg[[N]]$typInt == TRUE) {  # interval tier
            if (format == "short") {
                wrLine('"IntervalTier"', fid)
                wrLine(paste0('"', tg[[N]]$name, '"'), fid)
            } else if (format == "text") {
                wrLine('        class = "IntervalTier" ', fid, collection)
                wrLine(paste0('        name = "', tg[[N]]$name, '" '), fid, collection)
            }

            nInt <- length(tg[[N]]$t1)  # number of intervals
            if (nInt > 0) {
                if (format == "short") {
                    wrLine(as.character(round2(tg[[N]]$t1[1], -15)), fid)  # start time of the tier
                    wrLine(as.character(round2(tg[[N]]$t2[length(tg[[N]]$t2)], -15)), fid)  # end time of the tier
                    wrLine(as.character(nInt), fid)  # pocet intervalu textgrid
                } else if (format == "text") {
                    wrLine(paste0("        xmin = ", as.character(round2(tg[[N]]$t1[1], -15)), " "), fid, collection)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(tg[[N]]$t2[length(tg[[N]]$t2)], -15)), " "), fid, collection)  # end time of the tier
                    wrLine(paste0("        intervals: size = ", as.character(nInt), " "), fid, collection)  # pocet intervalu textgrid
                }

                for (I in seqM(1, nInt)) {
                    if (format == "short") {
                        wrLine(as.character(round2(tg[[N]]$t1[I], -15)), fid)
                        wrLine(as.character(round2(tg[[N]]$t2[I], -15)), fid)
                        wrLine(paste0('"', tg[[N]]$label[I], '"'), fid)
                    } else if (format == "text") {
                        wrLine(paste0("        intervals [", as.character(I), "]:"), fid, collection)
                        wrLine(paste0("            xmin = ", as.character(round2(tg[[N]]$t1[I], -15)), " "), fid, collection)
                        wrLine(paste0("            xmax = ", as.character(round2(tg[[N]]$t2[I], -15)), " "), fid, collection)
                        wrLine(paste0('            text = "', tg[[N]]$label[I], '" '), fid, collection)
                    }
                }
            } else {   # create one empty interval
                if (format == "short") {
                    wrLine(as.character(round2(minTimeTotal, -15)), fid)  # start time of the tier
                    wrLine(as.character(round2(maxTimeTotal, -15)), fid)  # end time of the tier
                    wrLine("1", fid)  # number of intervals
                    wrLine(as.character(round2(minTimeTotal, -15)), fid)
                    wrLine(as.character(round2(maxTimeTotal, -15)), fid)
                    wrLine('""', fid)
                } else if (format == "text") {
                    wrLine(paste0("        xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid, collection)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid, collection)  # end time of the tier
                    wrLine("        intervals: size = 1 ", fid, collection)  # number of intervals
                    wrLine("        intervals [1]:", fid, collection)
                    wrLine(paste0("            xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid, collection)
                    wrLine(paste0("            xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid, collection)
                    wrLine('            text = "" ', fid, collection)
                }
            }
        } else { # pointTier
            if (format == "short") {
                wrLine('"TextTier"', fid)
                wrLine(paste0('"', tg[[N]]$name, '"'), fid)
            } else if (format == "text") {
                wrLine('        class = "TextTier" ', fid, collection)
                wrLine(paste0('        name = "', tg[[N]]$name, '" '), fid, collection)
            }

            nInt <- length(tg[[N]]$t)  # number of points
            if (nInt > 0) {
                if (format == "short") {
                    wrLine(as.character(round2(tg[[N]]$t[1], -15)), fid)  # start time of the tier
                    wrLine(as.character(round2(tg[[N]]$t[length(tg[[N]]$t)], -15)), fid)  # end time of the tier
                    wrLine(as.character(nInt), fid)  # number of points
                } else if (format == "text") {
                    wrLine(paste0("        xmin = ", as.character(round2(tg[[N]]$t[1], -15)), " "), fid, collection)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(tg[[N]]$t[length(tg[[N]]$t)], -15)), " "), fid, collection)  # end time of the tier
                    wrLine(paste0("        points: size = ", as.character(nInt), " "), fid, collection)  # number of points
                }

                for (I in seqM(1, nInt)) {
                    if (format == "short") {
                        wrLine(as.character(round2(tg[[N]]$t[I], -15)), fid)
                        wrLine(paste0('"', tg[[N]]$label[I], '"'), fid)
                    } else if (format == "text") {
                        wrLine(paste0("        points [", as.character(I), "]:"), fid, collection)
                        wrLine(paste0("            number = ", as.character(round2(tg[[N]]$t[I], -15)), " "), fid, collection)
                        wrLine(paste0('            mark = "', tg[[N]]$label[I], '" '), fid, collection)
                    }
                }
            } else { # empty pointtier
                if (format == "short") {
                    wrLine(as.character(round2(minTimeTotal, -15)), fid)  # start time of the tier
                    wrLine(as.character(round2(maxTimeTotal, -15)), fid)  # end time of the tier
                    wrLine("0", fid)  # number of points
                } else if (format == "text") {
                    wrLine(paste0("        xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid, collection)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid, collection)  # end time of the tier
                    wrLine("        points: size = 0 ", fid, collection)  # number of points
                }
            }
        }

    }

    if (!collection) {
        close(fid)
    }
}


#' tg.plot
#'
#' Plots interactive TextGrid using \code{dygraphs} package.
#'
#' @param tg TextGrid object
#' @param group [optional] character string, name of group for dygraphs synchronization
#' @param pt [optional] PitchTier object
#' @param it [optional] IntensityTier object
#' @param formant [optional] Formant object
#' @param formantScaleIntensity [optional] Point size scaled according to relative intensity
#' @param formantDrawBandwidth [optional] Draw formant bandwidth
#' @param pitch [optional] Pitch object
#' @param pitchScaleIntensity [optional] Point size scaled according to relative intensity
#' @param pitchShowStrength [optional] Show strength annotation
#' @param snd [optional] Sound object
#'
#' @export
#' @seealso \code{\link{tg.read}}, \code{\link{pt.plot}}, \code{\link{it.plot}}, \code{\link{pitch.plot}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.plot(tg)
#' tg.plot(tg.sample(), pt = pt.sample())
#' }
tg.plot <- function(tg, group = "", pt = NULL, it = NULL, formant = NULL, formantScaleIntensity = TRUE, formantDrawBandwidth = TRUE, pitch = NULL, pitchScaleIntensity = TRUE, pitchShowStrength = FALSE, snd = NULL) {
    ntiers <- tg.getNumberOfTiers(tg)

    y2Axis <- !is.null(pt) | !is.null(it) | !is.null(formant) | !is.null(pitch)

    if (!is.null(formant) & (!is.null(pt) | !is.null(it) | !is.null(pitch))) {
        stop("Sorry, tg.plot cannot display Formant together with PitchTier, IntensityTier or Pitch.")
    }

    # if (!is.null(pitch) & (!is.null(pt) | !is.null(it) | !is.null(formant))) {
    #     stop("Sorry, tg.plot cannot display Pitch together with PitchTier, IntensityTier or Formant")
    # }

    if (ntiers == 0) {
        return(dygraphs::dygraph(list(x = 0, y = NA), main = "Empty TextGrid"))
    }

    if (length(names(tg)) != length(unique(names(tg)))) {
        stop("Sorry, tg.plot cannot display TextGrids with duplicated tier names.")
    }

    if (!isString(group)) {
        stop("group must be a character string.")
    }

    tAll <- as.numeric(c(class(tg)["tmin"], class(tg)["tmax"]))

    # find all time instances in tg
    for (I in seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            tAll <- c(tAll, tg[[I]]$t)

        } else if (tg[[I]]$type == "interval") {
            tAll <- c(tAll, tg[[I]]$t1, tg[[I]]$t2)

        } else {
            stop("Unknown tier type")
        }

    }

    if (!is.null(pt)) {
        tAll <- c(tAll, pt$t)
    }
    if (!is.null(pitch)) {
        tAll <- c(tAll, pitch$t)
    }
    if (!is.null(it)) {
        tAll <- c(tAll, it$t)
    }
    if (!is.null(formant)) {
        tAll <- c(tAll, formant$t)
    }
    if (!is.null(snd)) {
        if (is.null(nrow(snd$sig))) {
            nsamples <- length(snd$sig) # vector, not a matrix
        } else {
            nsamples <- nrow(snd$sig)   # matrix
        }

        if (!("nChannels" %in% names(snd))) {
            if (is.null(ncol(snd$sig))) {
                snd$nChannels <- 1  # probably a vector
            } else {
                snd$nChannels <- ncol(snd$sig)
            }
        }

        if (!("t" %in% names(snd))) {
            snd$t <- seqM(0, nsamples-1)/snd$fs
        }

        tAll <- c(tAll, snd$t)
    }

    tAll <- unique(sort(tAll))


    data <- list(t = tAll)


    if (!is.null(formant)) {
        fArray <- formant.toArray(formant)  ### formant

        if (formantScaleIntensity) {
            intensityNorm <- normIntensity(fArray$intensityVector, 1, 6)  # minimum, maximum radius
        } else {
            intensityNorm <- rep(2, formant$nx)
        }
        intensity2 <- rep(as.numeric(0), length(tAll))
        intensity2[tAll %in% formant$t] <- intensityNorm
        intensityNorm <- intensity2

        for (I in seqM(1, formant$maxnFormants)) {
            y2 <- rep(as.numeric(NA), length(tAll))
            y2[tAll %in% formant$t] <- fArray$frequencyArray[I, ]

            ### interpolation to make formant tracks not to be interrupted by labels in tg
            i <- is.na(y2) & (intensityNorm == 0)
            for (J in seqM(2, length(y2)-1)) {
                if (i[J] == TRUE & i[J-1] == FALSE & i[J+1] == FALSE) {
                    proportion <- (tAll[J] - tAll[J-1]) / (tAll[J+1] - tAll[J-1])  # where is my time point?
                    y2[J] <- y2[J-1] + (y2[J+1] - y2[J-1]) * proportion # mean value of neighbours; intensity == 0, so no point will be visible (it will look just like a normal line)
                }
            }
            ###
            data[[length(data)+1]] <- y2

            names(data)[length(data)] <- paste0("F", I)

            if (formantDrawBandwidth) {
                y2 <- rep(as.numeric(NA), length(tAll))
                y2[tAll %in% formant$t] <- fArray$frequencyArray[I, ] - fArray$bandwidthArray[I, ]/2
                ### interpolation to make formant tracks not to be interrupted by labels in tg
                i <- is.na(y2) & (intensityNorm == 0)
                for (J in seqM(2, length(y2)-1)) {
                    if (i[J] == TRUE & i[J-1] == FALSE & i[J+1] == FALSE) {
                        proportion <- (tAll[J] - tAll[J-1]) / (tAll[J+1] - tAll[J-1])  # where is my time point?
                        y2[J] <- y2[J-1] + (y2[J+1] - y2[J-1]) * proportion # mean value of neighbours; intensity == 0, so no point will be visible (it will look just like a normal line)
                    }
                }
                ###
                data[[length(data)+1]] <- y2
                names(data)[length(data)] <- paste0("lwr", I)


                y2 <- rep(as.numeric(NA), length(tAll))
                y2[tAll %in% formant$t] <- fArray$frequencyArray[I, ] + fArray$bandwidthArray[I, ]/2
                ### interpolation to make formant tracks not to be interrupted by labels in tg
                i <- is.na(y2) & (intensityNorm == 0)
                for (J in seqM(2, length(y2)-1)) {
                    if (i[J] == TRUE & i[J-1] == FALSE & i[J+1] == FALSE) {
                        proportion <- (tAll[J] - tAll[J-1]) / (tAll[J+1] - tAll[J-1])  # where is my time point?
                        y2[J] <- y2[J-1] + (y2[J+1] - y2[J-1]) * proportion # mean value of neighbours; intensity == 0, so no point will be visible (it will look just like a normal line)
                    }
                }
                ###
                data[[length(data)+1]] <- y2
                names(data)[length(data)] <- paste0("upr", I)
            }
        }
    }
    if (!is.null(pitch)) {
        pArray <- pitch.toArray(pitch)  ### pitch

        if (pitchScaleIntensity) {
            intensityNorm <- normIntensity(pArray$intensityVector, 0.5, 6)  # minimum, maximum radius
        } else {
            intensityNorm <- rep(1, pArray$nx)
        }
        pArray$frequencyArray[which(pArray$strengthArray == 0)] <- NA
        pArray$frequencyArray[which(pArray$frequencyArray > pArray$ceiling)] <- NA

        intensity2 <- rep(as.numeric(0), length(tAll))
        intensity2[tAll %in% pitch$t] <- intensityNorm
        intensityNorm <- intensity2

        for (I in seqM(1, pitch$maxnCandidates)) {
            y2 <- rep(as.numeric(NA), length(tAll))
            y2[tAll %in% pitch$t] <- pArray$frequencyArray[I, ]

            data[[length(data)+1]] <- y2

            names(data)[length(data)] <- paste0("c", I)
        }
    }
    if (!is.null(pt)) {
        y2 <- rep(as.numeric(NA), length(tAll))  ### pt
        y2[tAll %in% pt$t] <- pt$f
        data[[length(data)+1]] <- y2
        names(data)[length(data)] <- "PitchTier"
    }
    if (!is.null(it)) {
        y2 <- rep(as.numeric(NA), length(tAll))  ### it
        y2[tAll %in% it$t] <- it$i
        data[[length(data)+1]] <- y2
        names(data)[length(data)] <- "IntensityTier"
    }
    if (!is.null(snd)) {
        if (snd$nChannels == 1) {
            if (is.null(nrow(snd$sig))) {
                ch1 <- snd$sig  # probably a vector
            } else {
                ch1 <- snd$sig[, 1]
            }
            y2 <- rep(as.numeric(NA), length(tAll))
            y2[tAll %in% snd$t] <- ch1
            data[[length(data)+1]] <- y2
            names(data)[length(data)] <- "Sound"
        } else if (snd$nChannels == 2) {
            ch1 <- snd$sig[, 1]
            ch2 <-  snd$sig[, 2] - 2

            y2 <- rep(as.numeric(NA), length(tAll))
            y2[tAll %in% snd$t] <- ch1
            data[[length(data)+1]] <- y2
            names(data)[length(data)] <- "Sound1"
            y2 <- rep(as.numeric(NA), length(tAll))
            y2[tAll %in% snd$t] <- ch2
            data[[length(data)+1]] <- y2
            names(data)[length(data)] <- "Sound2"
        } else {
            stop("Only 1 or 2 channels are supported.")
        }
    }

    # create tg tiers
    for (I in seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            # y <- rep(as.numeric(NA), length(tAll))
            # y[tAll %in% tg[[I]]$t] <- ntiers + 1 - I  # y-value of graphic point according to tier index

            y <- rep(ntiers + 1 - I, length(tAll))  # this is to get invisible point (as all points are connected - no one is drawn)

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
        g <- dygraphs::dyRangeSelector(g, fillColor = "", strokeColor = "")
    }

    # tg Labels
    for (I in seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            for (J in seqM(1, length(tg[[I]]$label))) {
                g <- dygraphs::dyAnnotation(g, tg[[I]]$t[J], text = tg[[I]]$label[J], width = 10*max(1, nchar(tg[[I]]$label[J])), height = 25, series = tg[[I]]$name, tooltip = tg[[I]]$label[J])
            }

        } else if (tg[[I]]$type == "interval") {
            for (J in seqM(1, length(tg[[I]]$label))) {
                g <- dygraphs::dyAnnotation(g, tg[[I]]$t1[J], text = tg[[I]]$label[J], series = tg[[I]]$name, tooltip = tg[[I]]$label[J], width = -0.1, height = 25, tickHeight = 10)
                # width = -0.1: trick to get "right alignment". Original: width = 10*max(1, nchar(tg[[I]]$label[J]))
            }

        } else {
            stop("Unknown tier type")
        }

    }

    # tg style of tiers
    for (I in seqM(1, ntiers)) {
        if (tg[[I]]$type == "point") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, drawPoints = FALSE, strokeWidth = 0)

        } else if (tg[[I]]$type == "interval") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, drawPoints = FALSE, strokeWidth = 1)

        } else {
            stop("Unknown tier type")
        }
    }

    yMinAxis <- 0
    yMaxAxisCoef <- 0
    if (!is.null(snd)) {
        if (snd$nChannels == 1) {
            yMinAxis <- -1
            yMaxAxisCoef <- 1
        } else {
            yMinAxis <- -3
            yMaxAxisCoef <- 3
        }
    }

    if (!y2Axis) {
        g <- dygraphs::dyAxis(g, "y", label = "TextGrid", valueRange = c(yMinAxis, length(tg)+2+yMaxAxisCoef))
    } else {
        g <- dygraphs::dyAxis(g, "y", label = "TextGrid", valueRange = c(yMinAxis, length(tg)*2+2+yMaxAxisCoef), independentTicks = TRUE, drawGrid = TRUE, gridLineColor = "white")  # *2
        # Note: drawGrid = FALSE also turns off y2 grid :-(  Note2: gridLineWidth=0 does not work => the only solution is to set the white color

        if (is.null(pitch) & !is.null(pt) & !is.null(it)) {
            y_min <- min(c(pt$f, it$i))
            y_max <- max(c(pt$f, it$i))
            delta <- (y_max - y_min)*4/3
            yMin <- y_min - delta
            yMax <- y_min + delta
            g <- dygraphs::dyAxis(g, "y2", label = "PitchTier & IntensityTier", independentTicks = TRUE, valueRange = c(yMin, yMax), drawGrid = TRUE)
            g <- dygraphs::dySeries(g, "PitchTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0, color = "blue")
            g <- dygraphs::dySeries(g, "IntensityTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0, color = "green")
        } else if (is.null(pitch) & !is.null(pt)) {
            delta <- (max(pt$f)-min(pt$f))*4/3
            yMin <- min(pt$f) - delta
            yMax <- min(pt$f) + delta
            g <- dygraphs::dyAxis(g, "y2", label = "PitchTier", independentTicks = TRUE, valueRange = c(yMin, yMax), drawGrid = TRUE)
            g <- dygraphs::dySeries(g, "PitchTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0, color = "blue")
        } else if (is.null(pitch) & !is.null(it)) {
            delta <- (max(it$i)-min(it$i))*4/3
            yMin <- min(it$i) - delta
            yMax <- min(it$i) + delta
            g <- dygraphs::dyAxis(g, "y2", label = "IntensityTier", independentTicks = TRUE, valueRange = c(yMin, yMax), drawGrid = TRUE)
            g <- dygraphs::dySeries(g, "IntensityTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0, color = "green")
        } else if (!is.null(formant)) {
            if (formantDrawBandwidth) {
                max_f <- max(fArray$frequencyArray + fArray$bandwidthArray/2, na.rm = TRUE)
                min_f <- min(fArray$frequencyArray - fArray$bandwidthArray/2, na.rm = TRUE)
            } else {
                max_f <- max(fArray$frequencyArray, na.rm = TRUE)
                min_f <- min(fArray$frequencyArray, na.rm = TRUE)
            }
            delta <- (max_f - min_f)*4/3
            yMin <- min_f - delta
            yMax <- min_f + delta
            g <- dygraphs::dyAxis(g, "y2", label = "Formant", independentTicks = TRUE, valueRange = c(yMin, yMax), drawGrid = TRUE)

            for (I in seqM(1, formant$maxnFormants)) {
                if (!formantDrawBandwidth) {
                    g <- dygraphs::dySeries(g, paste0("F", I), axis = "y2", drawPoints = TRUE, pointSize = 2)
                } else {
                    g <- dygraphs::dySeries(g, axis = "y2", c(paste0("lwr", I), paste0("F", I), paste0("upr", I)), drawPoints = TRUE, pointSize = 2)
                }
            }

            g <- dygraphs::dyCallbacks(g, "drawPointCallback" = sprintf(
                "
                function(g, name, ctx, canvasx, canvasy, color, radius, index) {
                var radius_str = %s;
                radius = radius_str[index];
                return Dygraph.Circles.DEFAULT(g, name, ctx, canvasx, canvasy, color, radius)
                }
                ",
                paste0("[", paste0(intensityNorm, collapse = ","), "]") ))

        } else if (!is.null(pitch)) {
            max_f <- max(pArray$frequencyArray, na.rm = TRUE)
            min_f <- min(pArray$frequencyArray, na.rm = TRUE)

            if (!is.null(pt)) {
                max_f <- max(max_f, max(pt$f))
                min_f <- min(min_f, min(pt$f))
            }
            if (!is.null(it)) {
                max_f <- max(max_f, max(it$i))
                min_f <- min(min_f, min(it$i))
            }

            delta <- (max_f - min_f)*4/3
            yMin <- min_f - delta
            yMax <- min_f + delta

            lbl <- "Pitch"
            if (!is.null(pt)) {
                lbl <- paste0(lbl, " & PitchTier")
            }
            if (!is.null(it)) {
                lbl <- paste0(lbl, " & IntensityTier")
            }

            g <- dygraphs::dyAxis(g, "y2", label = lbl, independentTicks = TRUE, valueRange = c(yMin, yMax), drawGrid = TRUE)

            for (I in seqM(1, pitch$maxnCandidates)) {
                g <- dygraphs::dySeries(g, paste0("c", I), axis = "y2", drawPoints = TRUE, strokeWidth = 0, color = "black")
            }

            if (!is.null(pt)) {
                g <- dygraphs::dySeries(g, "PitchTier", axis = "y2", drawPoints = TRUE, pointSize = 3, strokeWidth = 0, color = "blue")
            }
            if (!is.null(it)) {
                g <- dygraphs::dySeries(g, "IntensityTier", axis = "y2", drawPoints = TRUE, pointSize = 3, strokeWidth = 0, color = "green")
            }

            if (!pitchShowStrength) {
                g <- dygraphs::dyCallbacks(g, "drawPointCallback" = sprintf(
                    "
                    function(g, name, ctx, canvasx, canvasy, color, radius, index) {
                    var radius_str = %s;
                    if (name != 'PitchTier' &&  name != 'IntensityTier') {
                    radius = radius_str[index];
                    }
                    return Dygraph.Circles.DEFAULT(g, name, ctx, canvasx, canvasy, color, radius)
                    }
                    ",
                    paste0("[", paste0(intensityNorm, collapse = ","), "]") ))
            } else {
                pArray$strengthArray[is.na(pArray$strengthArray)] <- 0 # or NaN, the value does not matter
                pArray$strengthArray <- round2(pArray$strengthArray*10)

                strength2 <- array(data = 0, dim = c(pitch$maxnCandidates, length(tAll)))
                for (I in seqM(1, pitch$maxnCandidates)) {
                    strength2[I, tAll %in% pitch$t] <- pArray$strengthArray[I, ]
                }

                g <- dygraphs::dyCallbacks(g, "drawPointCallback" = sprintf(
                    "
                    function(g, name, ctx, canvasx, canvasy, color, radius, index) {
                    var c_strength = %s;
                    var nc = %d;
                    var radius_str = %s;
                    if (name != 'PitchTier' &&  name != 'IntensityTier') {
                    var cs = c_strength[parseInt(name.substring(1))-1 + index*nc];
                    ctx.fillText(cs, canvasx+7, canvasy);
                    var grey = (10 - cs)*20;
                    var hex = grey.toString(16);
                    hex = (hex.length == 1 ? '0' + hex : hex);
                    color = '#' + hex + hex + hex;
                    radius = radius_str[index];
                    }
                    return Dygraph.Circles.DEFAULT(g, name, ctx, canvasx, canvasy, color, radius)
                    }
                    ",
                    paste0("[", paste0(strength2, collapse = ","), "]"),
                    pitch$maxnCandidates,
                    paste0("[", paste0(intensityNorm, collapse = ","), "]") ))
            }
            }

        }

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
#' @param verbose [optional, default=TRUE] If \code{FALSE}, the function performs everything quietly.
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
tg.repairContinuity <- function(tg, verbose = TRUE) {
    for (I in seqM(1, length(tg))) {
        if (tg[[I]]$type == "interval") {
            for (J in seqM(1, length(tg[[I]]$label)-1)) {
                if (tg[[I]]$t2[J] != tg[[I]]$t1[J+1]) {
                    newVal <- mean(c(tg[[I]]$t2[J], tg[[I]]$t1[J+1]))
                    if (verbose) {
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
#' Creates new and empty TextGrid. \code{tMin} and \code{tMax} specify the total start
#' and end time for the TextGrid. If a new interval tier is added later
#' without specified start and end, they are set to TextGrid start and end.
#'
#' This empty TextGrid cannot be used for almost anything. At least one tier
#' should be inserted using \code{tg.insertNewIntervalTier()} or \code{tg.insertNewPointTier()}.
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
    if (!isNum(tMin)) {
        stop("tMin must be a number")
    }

    if (!isNum(tMax)) {
        stop("tMin must be a number")
    }

    if (tMin > tMax) {
        stop(paste0("Cannot be: tMin > tMax [", as.character(tMin), " > ", as.character(tMax), "]"))
    }

    tgNew <- list()
    class(tgNew)["tmin"] <- tMin
    class(tgNew)["tmax"] <- tMax
    class(tgNew)["type"] <- "TextGrid"

    return(tgNew)
}



#' tg.isIntervalTier
#'
#' Returns \code{TRUE} if the tier is IntervalTier, \code{FALSE} otherwise.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return \code{TRUE} / \code{FALSE}
#' @export
#' @seealso \code{\link{tg.isPointTier}}, \code{\link{tg.getTierName}}, \code{\link{tg.findLabels}}
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
#' Returns \code{TRUE} if the tier is PointTier, \code{FALSE} otherwise.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return \code{TRUE} / \code{FALSE}
#' @export
#' @seealso \code{\link{tg.isIntervalTier}}, \code{\link{tg.getTierName}}, \code{\link{tg.findLabels}}
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

    if (!isString(name)) {
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
#' @seealso \code{\link{tg.findLabels}}, \code{\link{tg.getLabel}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.countLabels(tg, "phone", "a")
tg.countLabels <- function(tg, tierInd, label) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!isString(label)) {
        stop("label must be a character string")
    }

    c <- 0  # count

    for (I in seqM(1, length(tg[[tierInd]]$label))) {
        if (tg[[tierInd]]$label[I] == label) {
            c <- c + 1
        }
    }

    return(c)
}



#' tg.duplicateTier
#'
#' Duplicates tier \code{originalInd} to new tier with specified index \code{newInd}
#' (existing tiers are shifted).
#' It is highly recommended to set a name to the new tier
#' (this can also be done later by \code{tg.setTierName()}). Otherwise, both original and new tiers have the
#' same name which is permitted but not recommended. In such a case, we
#' cannot use the comfort of using tier name instead of its index in other
#' functions.
#'
#' @param tg TextGrid object
#' @param originalInd tier index or "name"
#' @param newInd new tier index (\code{1} = the first, \code{Inf} = the last [default])
#' @param newTierName [optional but recommended] name of the new tier
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.duplicateTierMergeSegments}}, \code{\link{tg.setTierName}}, \code{\link{tg.removeTier}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <- tg.duplicateTier(tg, "word", 1, "NEW")
#' tg.plot(tg2)
tg.duplicateTier <- function(tg, originalInd, newInd = Inf, newTierName = "") {
    originalInd <- tg.checkTierInd(tg, originalInd)
    ntiers <- length(tg)

    if (!isInt(newInd)) {
        stop("newInd must be integer >= 1 or +Inf")
    }

    if (is.infinite(newInd)) {
        if (newInd > 0) {
            newInd <- ntiers+1
        } else {
            stop("newInd must be integer >= 1 or +Inf")
        }
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    tgNew <- tg

    tOrig <- tg[[originalInd]]

    for (I in seqM(ntiers+1, newInd+1, by = -1)) {
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


#' tg.duplicateTierMergeSegments
#'
#' Duplicate tier \code{originalInd} and merge segments (according to the pattern) to the new tier with specified index \code{newInd}
#' (existing tiers are shifted).
#' Typical use: create new syllable tier from phone tier. It merges phones into syllables according to separators in pattern.
#'
#' Note 1: there can be segments with empty labels in the original tier (pause), do not specify them in the pattern
#'
#' Note 2: if there is an segment with empty label in the original tier in the place of separator in the pattern,
#'         the empty segment is duplicated into the new tier, i.e. at the position of the separator, there may or may not be
#'         an empty segment, if there is, it is duplicated. And they are not specified in the pattern.
#'
#' Note 3: if the segment with empty label is not at the position corresponding to separator, it leads to error
#'         - the part specified in the pattern between separators cannot be split by empty segments
#'
#' Note 4: beware of labels that appear empty but they are not (space, new line character etc.) - these segments are handled
#'         as classical non-empty labels. See example - one label is \code{" "}, therefore it must be specified in the pattern.
#'
#' @param tg TextGrid object
#' @param originalInd tier index or "name"
#' @param newInd new tier index (\code{1} = the first, \code{Inf} = the last [default])
#' @param newTierName name of the new tier
#' @param pattern merge segments pattern for the new tier (e.g., \code{"he-llo-world"})
#' @param sep separator in pattern (default: \code{"-"})
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.duplicateTier}}, \code{\link{tg.setTierName}}, \code{\link{tg.removeTier}}
#'
#' @examples
#' tg <- tg.sample()
#' tg <- tg.removeTier(tg, "syllable")
#' collapsed <- paste0(tg$phone$label, collapse = "")  # get actual labels
#' print(collapsed)  # all labels in collapsed form - copy the string, include separators -> pattern
#' pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
#' tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
#' \dontrun{
#' tg.plot(tg)
#' tg.plot(tg2)
#' }
tg.duplicateTierMergeSegments <- function(tg, originalInd, newInd = Inf, newTierName, pattern, sep = "-") {
    originalInd <- tg.checkTierInd(tg, originalInd)
    if (!tg.isIntervalTier(tg, originalInd)) {
        stop("originalInd must be interval tier")
    }

    ntiers <- length(tg)

    if (!isInt(newInd)) {
        stop("newInd must be integer >= 1 or +Inf")
    }

    if (is.infinite(newInd)) {
        if (newInd > 0) {
            newInd <- ntiers+1
        } else {
            stop("newInd must be integer >= 1 or +Inf")
        }
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    if (!isString(pattern)) {
        stop("pattern must be a character string")
    }


    tgNew <- tg

    tOrig <- tg[[originalInd]]
    ## process tOrig
    collapsed <- paste0(tOrig$label, collapse = "")
    patternCollapsed <- gsub(sep, "", pattern, fixed = TRUE)
    if (collapsed != patternCollapsed) {
        stop(paste0("pattern does not match actual labels in the tier\n", "pattern:       [", patternCollapsed, "]\n",
                    "actual labels: [", collapsed, "]"))
    }

    # parts <- unlist(strsplit(pattern, split = sep, fixed = TRUE))   #  ('-a--a-', '-')   ->  ""  "a" "" "a"
    parts <- unlist(stringr::str_split(pattern, stringr::coll(sep)))  #  ('-a--a-', '-')   ->  ""  "a" "" "a" ""

    t1 <- numeric(0)  #
    t2 <- numeric(0)  #
    label <- character(0)
    indPart <- 1
    labTemp <- ""

    t1Last <- NA

    # pozor, nejak se tez vyporadat s prazdnymi labely - idealne je zachovat a brat je tez jako oddelovac, tedy v ramci jedne "part" nemuze byt uvnitr prazdny label

    for (I in seqM(1, length(tOrig$label))) {
        if (labTemp == "") {
            t1Last <- tOrig$t1[I]
        }

        if (tOrig$label[I] == "") {  # empty label
            if (labTemp != "") {
                stop(paste0("unmatched labels [", labTemp, "] with the part [", parts[indPart], "], prematurely terminated by new segment with empty label"))
            }

            t1 <- c(t1, tOrig$t1[I])
            t2 <- c(t2, tOrig$t2[I])
            label <- c(label, tOrig$label[I])
        } else { # non-empty label
            labTemp <- paste0(labTemp, tOrig$label[I])
            if (indPart > length(parts)) {
                stop("more labels than parts")
            }
            if (nchar(labTemp) > nchar(parts[indPart])) {
                stop(paste0("unmatched label [", labTemp, "], the part should be [", parts[indPart], "]"))
            }

            if (labTemp == parts[indPart]) {  # match
                t1 <- c(t1, t1Last)
                t2 <- c(t2, tOrig$t2[I])
                label <- c(label, labTemp)
                labTemp <- ""
                indPart <- indPart + 1
            } else {  # not yet
                # nothing to do
            }
        }
    }

    if (indPart <= length(parts)) {
        stop("labels prematurely ended, not all parts found")
    }

    tOrig$t1 <- t1
    tOrig$t2 <- t2
    tOrig$label <- label

    ##

    for (I in seqM(ntiers+1, newInd+1, by = -1)) {
        tgNew[[I]] <- tgNew[[I-1]]
        names(tgNew)[I] <- names(tgNew)[I-1]
    }

    tgNew[[newInd]] <- tOrig


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
    if (isInt(tierInd) & tierInd == 0) {
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
    if (isInt(tierInd) & tierInd == 0) {
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
    if (isInt(tierInd) & tierInd == 0) {
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
#' @seealso \code{\link{tg.setLabel}}, \code{\link{tg.countLabels}}, \code{\link{tg.findLabels}}
#'
#' @examples tg <- tg.sample()
#' tg.getLabel(tg, "phoneme", 4)
#' tg.getLabel(tg, "phone", 4)
tg.getLabel <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!isInt(index)) {
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

    if (!isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!isString(newLabel)) {
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
#' @seealso \code{\link{tg.getIntervalEndTime}}, \code{\link{tg.getIntervalDuration}}, \code{\link{tg.getIntervalIndexAtTime}}, \code{\link{tg.findLabels}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getIntervalStartTime(tg, "phone", 5)
tg.getIntervalStartTime <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!isInt(index)) {
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
#' @seealso \code{\link{tg.getIntervalStartTime}}, \code{\link{tg.getIntervalDuration}}, \code{\link{tg.getIntervalIndexAtTime}}, \code{\link{tg.findLabels}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getIntervalEndTime(tg, "phone", 5)
tg.getIntervalEndTime <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!isInt(index)) {
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
#' @seealso \code{\link{tg.getIntervalStartTime}}, \code{\link{tg.getIntervalEndTime}}, \code{\link{tg.getIntervalIndexAtTime}}, \code{\link{tg.findLabels}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getIntervalDuration(tg, "phone", 5)
tg.getIntervalDuration <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!isInt(index)) {
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
#' @seealso \code{\link{tg.getPointIndexHigherThanTime}}, \code{\link{tg.findLabels}}
#'
#' @examples
#' tg <- tg.sample()
#' tg.getPointTime(tg, "phoneme", 4)
tg.getPointTime <- function(tg, tierInd, index) {
    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!isInt(index)) {
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

    for (I in seqM(tierInd, ntiers - 1)) {
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
#' @param newInd new tier index (\code{1} = the first, \code{Inf} = the last [default])
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
#' tg2 <- tg.insertNewPointTier(tg2, Inf, "POINTS2")  # the last tier
#' tg2 <- tg.insertPoint(tg2, "POINTS2", 2, "point in the last tier")
#' tg.plot(tg2)
#' }
tg.insertNewPointTier <- function(tg, newInd = Inf, newTierName) {
    ntiers <- length(tg)

    if (!isInt(newInd)) {
        stop("newInd must be integer >= 1 or +Inf")
    }

    if (is.infinite(newInd)) {
        if (newInd > 0) {
            newInd <- ntiers+1
        } else {
            stop("newInd must be integer >= 1 or +Inf")
        }
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    tgNew <- tg

    tNew <- list(name = newTierName, type = "point", t = numeric(0), label = character(0))

    for (I in seqM(ntiers+1, newInd+1, by = -1)) {
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
#' @param newInd new tier index (\code{1} = the first, \code{Inf} = the last [default])
#' @param newTierName new tier name
#' @param tMin [optional] start time of the new tier
#' @param tMax [optional] end time of the new tier
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertInterval}}, \code{\link{tg.insertNewPointTier}}, \code{\link{tg.duplicateTier}}, \code{\link{tg.duplicateTierMergeSegments}}, \code{\link{tg.removeTier}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg2 <- tg.insertNewIntervalTier(tg, 1, "INTERVALS")
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.8)
#' tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.1, "Interval A")
#' tg2 <- tg.insertInterval(tg2, "INTERVALS", 1.2, 2.5, "Interval B")
#' tg2 <- tg.insertNewIntervalTier(tg2, Inf, "LastTier")
#' tg2 <- tg.insertInterval(tg2, "LastTier", 1, 3, "This is the last tier")
#' tg.plot(tg2)
#' }
tg.insertNewIntervalTier <- function(tg, newInd = Inf, newTierName, tMin=NA, tMax=NA) {
    ntiers <- length(tg)

    if (!isInt(newInd)) {
        stop("newInd must be integer >= 1 or +Inf")
    }

    if (is.infinite(newInd)) {
        if (newInd > 0) {
            newInd <- ntiers+1
        } else {
            stop("newInd must be integer >= 1 or +Inf")
        }
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    if (class(tMin) != "logical" & !isNum(tMin)) {
        stop("tMin must be a number")
    }
    if (class(tMin) == "logical" & length(tMin) != 1) {
        stop("tMin must be a number")
    }
    if (!isNum(tMin) & !is.na(tMin)) {
        stop("tMin must be a number")
    }

    if (class(tMax) != "logical" & !isNum(tMax)) {
        stop("tMax must be a number")
    }
    if (class(tMax) == "logical" & length(tMax) != 1) {
        stop("tMax must be a number")
    }
    if (!isNum(tMax) & !is.na(tMax)) {
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

    for (I in seqM(ntiers+1, newInd+1, by = -1)) {
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
#' tStart <= \code{time} < tEnd. Tier index must belong to interval tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in intervals
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getIntervalStartTime}}, \code{\link{tg.getIntervalEndTime}}, \code{\link{tg.getLabel}}, \code{\link{tg.findLabels}}
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

    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    nint <- length(tg[[tierInd]]$t1)
    for (I in seqM(1, nint)) {
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
#' \code{time} <= pointTime. Tier index must belong to point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in points
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getPointIndexNearestTime}}, \code{\link{tg.getPointIndexLowerThanTime}}, \code{\link{tg.getLabel}}, \code{\link{tg.findLabels}}
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

    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    for (I in seqM(1, npoints)) {
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
#' pointTime <= \code{time}. Tier index must belong to point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in points
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getPointIndexNearestTime}}, \code{\link{tg.getPointIndexHigherThanTime}}, \code{\link{tg.getLabel}}, \code{\link{tg.findLabels}}
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

    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    for (I in seqM(npoints, 1, by = -1)) {
        if (time >= tg[[tierInd]]$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}




#' tg.getPointIndexNearestTime
#'
#' Returns index of point which is nearest the given \code{time} (from both sides).
#' Tier index must belong to point tier.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time which is going to be found in points
#'
#' @return integer
#' @export
#' @seealso \code{\link{tg.getPointIndexLowerThanTime}}, \code{\link{tg.getPointIndexHigherThanTime}}, \code{\link{tg.getLabel}}, \code{\link{tg.findLabels}}
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

    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    minDist <- Inf
    minInd <- NA

    for (I in seqM(1, npoints)) {
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
#' Remove point of the given \code{index} from the point tier.
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

    if (!isInt(index)) {
        stop("index must be an integer.")
    }

    npoints <- length(tg[[tierInd]]$t)

    if (index < 1 | index>npoints) {
        stop(paste0("index out of range [index = ", index, ", npoints = ", npoints, "]."))
    }


    tgNew <- tg
    for (I in seqM(index, npoints - 1)) {
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

    if (!isNum(time)) {
        stop("time must be a number.")
    }

    if (!isString(label)) {
        stop("label must be a character string.")
    }


    tgNew <- tg

    indPosun <- tg.getPointIndexHigherThanTime(tg, tierInd, time)
    npoints <- length(tg[[tierInd]]$t)

    if (!is.na(indPosun)) {
        for (I in seqM(npoints, indPosun, by = -1)) {
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
#' Remove left boundary of the interval of the given \code{index} in Interval tier.
#' In fact, it concatenates two intervals into one (and their labels). It
#' cannot be applied to the first interval because it is the start boundary
#' of the tier.
#' E.g., we have interval 1-2-3, we remove the left boundary of the 2nd
#' interval, the result is two intervals 12-3.
#' If we do not want to concatenate labels, we have to set the label
#' to the empty string \code{""} before this operation.
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

    if (!isInt(index)) {
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
    for (I in seqM(index, nint - 1)) {
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
#' Remove right boundary of the interval of the given \code{index} in Interval tier.
#' In fact, it concatenates two intervals into one (and their labels). It
#' cannot be applied to the last interval because it is the end boundary
#' of the tier.
#' E.g., we have interval 1-2-3, we remove the right boundary of the 2nd
#' interval, the result is two intervals 1-23.
#' If we do not want to concatenate labels, we have to set the label
#' to the empty string \code{""} before this operation.
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

    if (!isInt(index)) {
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
    for (I in seqM(index, nint - 1)) {
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
#' Remove both left and right boundary of interval of the given \code{index} in
#' Interval tier. In fact, this operation concatenate three intervals into
#' one (and their labels). It cannot be applied to the first and the last
#' interval because they contain beginning or end boundary of the tier.
#' E.g., let's assume interval 1-2-3. We remove both boundaries of the
#' 2nd interval. The result is one interval 123.
#' If we do not want to concatenate labels (we wanted to remove the label
#' including its interval), we can set the label of the second interval
#' to the empty string \code{""} before this operation.
#' If we only want to remove the label of interval "without concatenation",
#' i.e., the desired result is 1-empty-3, it is not this operation of
#' removing boundaries. Just set the label of the second interval to the
#' empty string \code{""}.
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

    if (!isInt(index)) {
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
    for (I in seqM(index, nint - 2)) {
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
#' which we can set the \code{label} (optional argument).
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
#' In such a case, at first, use the \code{tgRepairContinuity()} function.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param time time of the new boundary
#' @param label [optional] label of the new interval
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertInterval}}, \code{\link{tg.removeIntervalLeftBoundary}}, \code{\link{tg.removeIntervalRightBoundary}}, \code{\link{tg.removeIntervalBothBoundaries}}, \code{\link{tg.duplicateTierMergeSegments}}
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

    if (!isNum(time)) {
        stop("time must be a number.")
    }

    if (!isString(label)) {
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
            for (I in seqM(nint, 1, by = -1)) {
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
        for (I in seqM(1, nint)) {
            if ((time %in% tgNew[[tierInd]]$t1) | (time %in% tgNew[[tierInd]]$t2)) {
                stop(paste0("Cannot insert boundary because one already exists at the same position [tierInd = ", tierInd, ", time = ", time, "]."))
            }
        }

        for (I in seqM(nint, index+1, by = -1)) {
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
#' b) Outside of existing intervals (left or right), this may create another
#' empty interval between.
#'
#' In most cases, this function is the same as 1.) \code{tgInsertBoundary(tEnd)}
#' and 2.) \code{tgInsertBoundary(tStart, "new label")}. But, additional checks are
#' performed: a) \code{tStart} and \code{tEnd} belongs to the same empty interval, or
#' b) both times are outside of existings intervals (both left or both right).
#'
#' Intersection of the new interval with more already existing (even empty)
#' does not make a sense and is forbidden.
#'
#' In many situations, in fact, this function creates more than one interval.
#' E.g., let's assume an empty interval tier with one empty interval from 0 to 5 sec.
#' 1.) We insert a new interval from 1 to 2 with label \code{"he"}.
#'     Result: three intervals, 0-1 \code{""}, 1-2 \code{"he"}, 2-5 \code{""}.
#' 2.) Then, we insert an interval from 7 to 8 with label \code{"lot"}.
#'     Result: five intervals, 0-1 \code{""}, 1-2 \code{"he"}, 2-5 \code{""}, 5-7 \code{""}, 7-8 \code{"lot"}
#'     Note: the empty 5-7 \code{""} interval is inserted because we are going
#'     outside of the existing tier.
#' 3.) Now, we insert a new interval exactly between 2 and 3 with label \code{"said"}.
#'     Result: really only one interval is created (and only the right
#'     boundary is added because the left one already exists):
#'     0-1 \code{""}, 1-2 \code{"he"}, 2-3 \code{"said"}, 3-5 \code{""}, 5-7 \code{""}, 7-8 \code{"lot"}.
#' 4.) After this, we want to insert another interval, 3 to 5: label \code{"a"}.
#'     In fact, this does not create any new interval at all. Instead of
#'     that, it only sets the label to the already existing interval 3-5.
#'     Result: 0-1 \code{""}, 1-2 \code{"he"}, 2-3 \code{"said"}, 3-5 \code{"a"}, 5-7 \code{""}, 7-8 \code{"lot"}.
#'
#' This function is not implemented in Praat (6.0.14). And it is very useful
#' for adding separate intervals to an empty area in interval tier, e.g.,
#' result of voice activity detection algorithm.
#' On the other hand, if we want continuously add new consequential
#' intervals, \code{tgInsertBoundary()} may be more useful. Because, in the
#' \code{tgInsertInterval()} function, if we calculate both boundaries separately
#' for each interval, strange situations may happen due to numeric round-up
#' errors, like \code{3.14*5 != 15.7}. In such cases, it may be hard to obtain
#' precisely consequential time instances. As \code{3.14*5} is slightly larger than
#' \code{15.7} (let's try to calculate \code{15.7 - 3.14*5}), if you calculate \code{tEnd} of the
#' first interval as \code{3.14*5} and \code{tStart} of the second interval as \code{15.7}, this
#' function refuse to create the second interval because it would be an
#' intersection. In the opposite case (\code{tEnd} of the 1st: \code{15.7}, \code{tStart} of the
#' 2nd: \code{3.14*5}), it would create another "micro" interval between these two
#' slightly different time instances. Instead of that, if you insert only
#' one boundary using the \code{tgInsertBoundary()} function, you are safe that
#' only one new interval is created. But, if you calculate the "\code{15.7}" (no
#' matter how) and store in the variable and then, use this variable in
#' the \code{tgInsertInterval()} function both for the \code{tEnd} of the 1st interval and
#' \code{tStart} of the 2nd interval, you are safe, it works fine.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param tStart start time of the new interval
#' @param tEnd end time of the new interval
#' @param label [optional] label of the new interval
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.insertBoundary}}, \code{\link{tg.removeIntervalLeftBoundary}}, \code{\link{tg.removeIntervalRightBoundary}}, \code{\link{tg.removeIntervalBothBoundaries}}, \code{\link{tg.duplicateTierMergeSegments}}
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

    if (!isNum(tStart)) {
        stop("tStart must be a number.")
    }
    if (!isNum(tEnd)) {
        stop("tEnd must be a number.")
    }
    if (tStart >= tEnd) {
        stop(paste0("tStart [", as.character(tStart), "] must be lower than tEnd [", as.character(tEnd), "]."))
    }
    # Note: thanks to this condition, some situations (which were solved below) cannot happen
    # (tStart == tEnd), thus it is easier. By the way, Praat does not allow to have 2 boundaries
    # in the same time instance, do it is fully compatible.

    if (!isString(label)) {
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
        for (I in seqM(1, nint)) {
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
                    for (I in seqM(1, length(inters))) {
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


#' tg.findLabels
#'
#' Find label or consecutive sequence of labels and returns their indices.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#' @param labelVector character string (one label) or vector of character strings (consecutive sequence of labels) to be found
#' @param returnTime If \code{TRUE}, return vectors of begin (t1) and end time (t2) for each found group of sequence of labels instead of indices (when \code{FALSE} = default).
#'
#' @return If \code{returnTime == FALSE}, returns list of all occurrences, each member of the list is one occurence and contains vector of label indices, if \code{returnTime == TRUE}, returns list witch vectors t1 (begin) and t2 (end) for each found group of sequence of labels.
#' @export
#' @seealso \code{\link{tg.countLabels}}, \code{\link{tg.getLabel}}, \code{\link{tg.duplicateTierMergeSegments}}
#'
#' @examples
#' tg <- tg.sample()
#' i <- tg.findLabels(tg, "phoneme", "n")
#' i
#' length(i)
#' i[[1]]
#' i[[2]]
#' tg$phoneme$label[unlist(i)]
#'
#' i <- tg.findLabels(tg, "phone", c("?", "a"))
#' i
#' length(i)
#' tg$phone$label[i[[1]]]
#' tg$phone$label[i[[2]]]
#' tg$phone$label[unlist(i)]
#'
#' t <- tg.findLabels(tg, "phone", c("?", "a"), returnTime = TRUE)
#' t
#' t$t2[1] - t$t1[1]   # duration of the first result
#' t$t2[2] - t$t1[2]   # duration of the second result
#'
#' i <- tg.findLabels(tg.sample(), "word", c("ti", "reknu", "co"))
#' i
#' length(i)
#' length(i[[1]])
#' i[[1]]
#' i[[1]][3]
#' tg$word$label[i[[1]]]
#'
#' t <- tg.findLabels(tg.sample(), "word", c("ti", "reknu", "co"), returnTime = TRUE)
#' pt <- pt.sample()
#' tStart <- t$t1[1]
#' tEnd <- t$t2[1]
#' \dontrun{
#' pt.plot(pt.cut(pt, tStart, tEnd))
#' }
tg.findLabels <- function(tg, tierInd, labelVector, returnTime = FALSE) {
    if (!isLogical(returnTime)) {
        stop("returnTime must be a logical value.")
    }

    tierInd <- tg.checkTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (class(labelVector) != "character") {
        stop("labelVector must be a character string or vector of character strings")
    }

    nlabs <- length(labelVector)

    if (nlabs < 1)
        return(integer(0))
    else if (nlabs == 1) {
        indLab <- which(tg[[tierInd]]$label == labelVector)
        if (returnTime == FALSE) {
            return(as.list(indLab))
        } else {
            if (tg.isIntervalTier(tg, tierInd)) {
                return( list(t1 = tg[[tierInd]]$t1[indLab], t2 = tg[[tierInd]]$t2[indLab]) )
            } else {  # PointTier
                return( list(t1 = tg[[tierInd]]$t[indLab], t2 = tg[[tierInd]]$t[indLab]) )
            }
        }
    }
    else {
        indStart <- which(tg[[tierInd]]$label == labelVector[1])
        indStart <- indStart[indStart <= length(tg[[tierInd]]$label) - nlabs + 1]  # pokud zbyva do konce mene labelu, nez hledame, nema smysl hledat

        indLab <- list()

        for (I in indStart) {
            ok <- TRUE
            for (J in seqM(2, nlabs)) {
                if (tg[[tierInd]]$label[I+J-1] != labelVector[J]) {
                    ok <- FALSE
                    break
                }
            }
            if (ok) {
                indLab <- c(indLab, list(seqM(I, I+nlabs-1)))
            }
        }

        if (returnTime == FALSE) {
            return(indLab)
        } else {
            if (tg.isIntervalTier(tg, tierInd)) {
                t1 <- numeric(length(indLab))
                t2 <- numeric(length(indLab))
                for (I in seqM(1, length(indLab))) {
                    t1[I] <- tg[[tierInd]]$t1[ indLab[[I]][1] ]
                    t2[I] <- tg[[tierInd]]$t2[ indLab[[I]][ length(indLab[[I]]) ]   ]
                }
            } else { # PointTier
                t1 <- numeric(length(indLab))
                t2 <- numeric(length(indLab))
                for (I in seqM(1, length(indLab))) {
                    t1[I] <- tg[[tierInd]]$t[ indLab[[I]][1] ]
                    t2[I] <- tg[[tierInd]]$t[ indLab[[I]][ length(indLab[[I]]) ]   ]
                }
            }
            return(list(t1 = t1, t2 = t2))
        }

    }
}


#' tg.cut
#'
#' Cut the specified time frame from the TextGrid and preserve time
#'
#' @param tg TextGrid object
#' @param tStart beginning time of time frame to be cut (default \code{-Inf} = cut from the tmin of the TextGrid)
#' @param tEnd final time of time frame to be cut (default \code{Inf} = cut to the tmax of the TextGrid)
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.cut0}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{tg.read}}, \code{\link{tg.plot}}, \code{\link{tg.write}}, \code{\link{tg.insertInterval}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <-   tg.cut(tg,  tStart = 3)
#' tg2_0 <- tg.cut0(tg, tStart = 3)
#' tg3 <-   tg.cut(tg,  tStart = 2, tEnd = 3)
#' tg3_0 <- tg.cut0(tg, tStart = 2, tEnd = 3)
#' tg4 <-   tg.cut(tg,  tEnd = 1)
#' tg4_0 <- tg.cut0(tg, tEnd = 1)
#' tg5 <-   tg.cut(tg,  tStart = -1, tEnd = 5)
#' tg5_0 <- tg.cut0(tg, tStart = -1, tEnd = 5)
#' \dontrun{
#' tg.plot(tg)
#' tg.plot(tg2)
#' tg.plot(tg2_0)
#' tg.plot(tg3)
#' tg.plot(tg3_0)
#' tg.plot(tg4)
#' tg.plot(tg4_0)
#' tg.plot(tg5)
#' tg.plot(tg5_0)
#' }
tg.cut <- function(tg, tStart = -Inf, tEnd = Inf) {
    if (!isNum(tStart)) {
        stop("tStart must be a number.")
    }
    if (!isNum(tEnd)) {
        stop("tEnd must be a number.")
    }
    if (is.infinite(tStart) & tStart>0) {
        stop("infinite tStart can be negative only")
    }
    if (is.infinite(tEnd) & tEnd<0) {
        stop("infinite tEnd can be positive only")
    }
    if (tEnd < tStart) {
        stop("tEnd must be >= tStart")
    }

    ntiers <- length(tg)
    tmin <- as.numeric(class(tg)["tmin"])
    tmax <- as.numeric(class(tg)["tmax"])

    tgNew <- tg

    for (I in rPraat::seqM(1, ntiers)) {
        if (tgNew[[I]]$type == "point") {
            sel <- tg[[I]]$t >= tStart  &  tg[[I]]$t <= tEnd
            tgNew[[I]]$t     <-     tg[[I]]$t[sel]
            tgNew[[I]]$label <- tg[[I]]$label[sel]
        } else if (tgNew[[I]]$type == "interval") {
            sel <- (tg[[I]]$t1 >= tStart & tg[[I]]$t2 <= tEnd) | (tStart >= tg[[I]]$t1 & tEnd <= tg[[I]]$t2) | (tg[[I]]$t2 > tStart & tg[[I]]$t2 <= tEnd) | (tg[[I]]$t1 >= tStart & tg[[I]]$t1 < tEnd)
            tgNew[[I]]$t1    <-    tg[[I]]$t1[sel]
            tgNew[[I]]$t2    <-    tg[[I]]$t2[sel]
            tgNew[[I]]$label <- tg[[I]]$label[sel]

            tgNew[[I]]$t1[tgNew[[I]]$t1 < tStart] <- tStart
            tgNew[[I]]$t2[tgNew[[I]]$t2 > tEnd] <- tEnd
        } else {
            stop(paste0("unknown tier type:", tgNew[[I]]$type))
        }
    }

    if (is.infinite(tStart)) {
        class(tgNew)["tmin"] <- min(tmin, tEnd)
    } else {
        class(tgNew)["tmin"] <- tStart
    }

    if (is.infinite(tEnd)) {
        class(tgNew)["tmax"] <- max(tmax, tStart)
    } else {
        class(tgNew)["tmax"] <- tEnd
    }



    return(tgNew)
}


#' tg.cut0
#'
#' Cut the specified time frame from the TextGrid and shift time so that the new tmin = 0
#'
#' @param tg TextGrid object
#' @param tStart beginning time of time frame to be cut (default \code{-Inf} = cut from the tmin of the TextGrid)
#' @param tEnd final time of time frame to be cut (default \code{Inf} = cut to the tmax of the TextGrid)
#'
#' @return TextGrid object
#' @export
#' @seealso \code{\link{tg.cut}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{tg.read}}, \code{\link{tg.plot}}, \code{\link{tg.write}}, \code{\link{tg.insertInterval}}
#'
#' @examples
#' tg <- tg.sample()
#' tg2 <-   tg.cut(tg,  tStart = 3)
#' tg2_0 <- tg.cut0(tg, tStart = 3)
#' tg3 <-   tg.cut(tg,  tStart = 2, tEnd = 3)
#' tg3_0 <- tg.cut0(tg, tStart = 2, tEnd = 3)
#' tg4 <-   tg.cut(tg,  tEnd = 1)
#' tg4_0 <- tg.cut0(tg, tEnd = 1)
#' tg5 <-   tg.cut(tg,  tStart = -1, tEnd = 5)
#' tg5_0 <- tg.cut0(tg, tStart = -1, tEnd = 5)
#' \dontrun{
#' tg.plot(tg)
#' tg.plot(tg2)
#' tg.plot(tg2_0)
#' tg.plot(tg3)
#' tg.plot(tg3_0)
#' tg.plot(tg4)
#' tg.plot(tg4_0)
#' tg.plot(tg5)
#' tg.plot(tg5_0)
#' }
tg.cut0 <- function(tg, tStart = -Inf, tEnd = Inf) {
    if (!isNum(tStart)) {
        stop("tStart must be a number.")
    }
    if (!isNum(tEnd)) {
        stop("tEnd must be a number.")
    }
    if (is.infinite(tStart) & tStart>0) {
        stop("infinite tStart can be negative only")
    }
    if (is.infinite(tEnd) & tEnd<0) {
        stop("infinite tEnd can be positive only")
    }
    if (tEnd < tStart) {
        stop("tEnd must be >= tStart")
    }

    ntiers <- length(tg)
    tmin <- as.numeric(class(tg)["tmin"])
    tmax <- as.numeric(class(tg)["tmax"])

    tgNew <- tg

    if (is.infinite(tStart)) {
        class(tgNew)["tmin"] <- min(tmin, tEnd)
    } else {
        class(tgNew)["tmin"] <- tStart
    }

    if (is.infinite(tEnd)) {
        class(tgNew)["tmax"] <- max(tmax, tStart)
    } else {
        class(tgNew)["tmax"] <- tEnd
    }

    tNewMin <- tg.getStartTime(tgNew)
    tNewMax <- tg.getEndTime(tgNew)

    for (I in rPraat::seqM(1, ntiers)) {
        if (tgNew[[I]]$type == "point") {
            sel <- tg[[I]]$t >= tStart  &  tg[[I]]$t <= tEnd
            tgNew[[I]]$t     <-     tg[[I]]$t[sel] - tNewMin
            tgNew[[I]]$label <- tg[[I]]$label[sel]
        } else if (tgNew[[I]]$type == "interval") {
            sel <- (tg[[I]]$t1 >= tStart & tg[[I]]$t2 <= tEnd) | (tStart >= tg[[I]]$t1 & tEnd <= tg[[I]]$t2) | (tg[[I]]$t2 > tStart & tg[[I]]$t2 <= tEnd) | (tg[[I]]$t1 >= tStart & tg[[I]]$t1 < tEnd)
            tgNew[[I]]$t1    <-    tg[[I]]$t1[sel]
            tgNew[[I]]$t2    <-    tg[[I]]$t2[sel]
            tgNew[[I]]$label <- tg[[I]]$label[sel]

            tgNew[[I]]$t1[tgNew[[I]]$t1 < tStart] <- tStart
            tgNew[[I]]$t2[tgNew[[I]]$t2 > tEnd] <- tEnd

            tgNew[[I]]$t1 <- tgNew[[I]]$t1 - tNewMin
            tgNew[[I]]$t2 <- tgNew[[I]]$t2 - tNewMin
        } else {
            stop(paste0("unknown tier type:", tgNew[[I]]$type))
        }
    }

    class(tgNew)["tmin"] <- 0
    class(tgNew)["tmax"] <- tNewMax - tNewMin

    return(tgNew)
}

#' as.tg
#'
#' Renames the \code{class(tg)["name"]} attribute and sets {class(tg)["type"] <- "TextGrid"} (if it is not already set)
#'
#' @param tg TextGrid object
#' @param name New name
#'
#' @return TextGrid object
#' @export
#'
#' @examples
#' class(tg.sample())
#' class(as.tg(tg.sample(), name = "New Name"))
as.tg <- function(tg, name = "") {
    class(tg)["type"] <- "TextGrid"
    class(tg)["name"] <- name
    return(tg)
}
