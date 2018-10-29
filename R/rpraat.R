#' col.read
#'
#' Loads Collection from Praat in Text or Short text format.
#' Collection may contain combination of TextGrids, PitchTiers, Pitch objects, Formant objects, and IntensityTiers.
#'
#' @param fileName Input file name
#' @param encoding File encoding (default: "UTF-8"), "auto" for auto-detect of Unicode encoding
#'
#' @return Collection object
#' @export
#'
#' @seealso \code{\link{tg.read}}, \code{\link{pt.read}}, \code{\link{pitch.read}}, \code{\link{formant.read}}, \code{\link{it.read}}
#'
#' @examples
#' \dontrun{
#' coll <- col.read("coll_text.Collection")
#' length(coll)  # number of objects in collection
#' class(coll[[1]])["type"]  # 1st object type
#' class(coll[[1]])["name"]  # 1st object name
#' it <- coll[[1]]  # 1st object
#' it.plot(it)
#'
#' class(coll[[2]])["type"]  # 2nd object type
#' class(coll[[2]])["name"]  # 2nd object name
#' tg <- coll[[2]]  # 2nd object
#' tg.plot(tg)
#' length(tg)  # number of tiers in TextGrid
#' tg$word$label
#'
#' class(coll[[3]])["type"]  # 3rd object type
#' class(coll[[3]])["name"]  # 3rd object type
#' pitch <- coll[[3]]  # 3rd object
#' names(pitch)
#' pitch$nx  # number of frames
#' pitch$t[4]        # time instance of the 4th frame
#' pitch$frame[[4]]  # 4th frame: pitch candidates
#' pitch$frame[[4]]$frequency[2]
#' pitch$frame[[4]]$strength[2]
#'
#' class(coll[[4]])["type"]  # 4th object type
#' class(coll[[4]])["name"]  # 4th object name
#' pt <- coll[[4]]  # 2nd object
#' pt.plot(pt)
#' }
col.read <- function(fileName, encoding = "UTF-8") {
    # inspired by Pol van Rijn's function from mPraat toolbox

    if (!isString(fileName)) {
        stop("Invalid 'fileName' parameter.")
    }

    if (!isString(encoding)) {
        stop("Invalid 'encoding' parameter.")
    }

    if (encoding == "auto") {
        encoding <- detectEncoding(fileName)
    }

    if (encoding == "UTF-8") {
        flines <- readr::read_lines(fileName, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
    } else {
        fid <- file(fileName, open = "r", encoding = encoding)
        flines <- readLines(fid)
        close(fid)
    }

    if (length(flines) < 3) {
        stop("This is not a Collection file!")
    }

    if (flines[2] != "Object class = \"Collection\"") {
        stop("This is not a Collection file!")
    }

    r <- flines[4]
    if (str_contains(r, 'size = ')) {
        shortFormat <- FALSE
    } else {
        shortFormat <- TRUE
    }
    nobjects <- getNumberInLine(r, shortFormat) # Read the size, remove eventual spaces
    collection <- vector("list", nobjects)

    if (!shortFormat) {
        find <- 6   # ignore "item []: "
    } else {
        find <- 5
    }

    for (s in seqM(1, nobjects)) {
        if (!shortFormat) {
            find <- find + 1  # discard item [1]:
        }

        r <- flines[find]; find <- find + 1
        if (str_contains(r, "TextGrid")) {
            objClass <- "TextGrid"
        } else if (str_contains(r, "PitchTier")) {
            objClass <- "PitchTier"
        } else if (str_contains(r, "IntensityTier")) {
            objClass <- "IntensityTier"
        } else if (str_contains(r, "Pitch 1")) {
            objClass <- "Pitch 1"
        } else if (str_contains(r, "Formant 2")) {
            objClass <- "Formant 2"
        } else if (str_contains(r, "Sound")) {
            stop("Sound files are currently not supported, because of their inefficient loading and saving duration, rather use WAVE files")
        } else {
            stop(paste0("Class not recognized! Line: ", r))
        }

        name <- getTextInQuotes(flines[find]); find <- find + 1

        if (objClass == "TextGrid") {
            tg_ind <- tg.read_lines(flines, find)
            object <- tg_ind[[1]]
            find <- tg_ind[[2]]
        } else if (objClass == "PitchTier") {
            pt_ind <- pt.read_lines(flines, find, collection = TRUE)
            object <- pt_ind[[1]]
            find <- pt_ind[[2]]
        } else if (objClass == "IntensityTier") {
            it_ind <- it.read_lines(flines, find, collection = TRUE)
            object <- it_ind[[1]]
            find <- it_ind[[2]]
        } else if (objClass == "Pitch 1") {
            pitch_ind <- pitch.read_lines(flines, find, collection = TRUE)
            object <- pitch_ind[[1]]
            find <- pitch_ind[[2]]
        } else if (objClass == "Formant 2") {
            formant_ind <- formant.read_lines(flines, find, collection = TRUE)
            object <- formant_ind[[1]]
            find <- formant_ind[[2]]
        }

        class(object)["type"] <- objClass
        class(object)["name"] <- name
        collection[[s]] <- object
    }

    return(collection)
}

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


#' detectEncoding
#'
#' Detects unicode encoding of Praat text files
#'
#' @param fileName Input file name
#'
#' @return detected encoding of the text input file
#' @export
#'
#' @examples
#' \dontrun{
#' detectEncoding("demo/H.TextGrid")
#' detectEncoding("demo/H_UTF16.TextGrid")
#' }
detectEncoding <- function(fileName) {
    # Inspired by Weirong Chen.

    encodings <- c("UTF-8", "UTF-16", "UTF-16BE", "UTF-16LE")
    encodingWeight <- numeric(length(encodings))

    for (I in 1:length(encodings)) {
        encoding <- encodings[I]

        if (encoding == "UTF-8") {
            flines <- readr::read_lines(fileName, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
        } else {
            fid <- file(fileName, open = "r", encoding = encoding)
            flines <- suppressWarnings(readLines(fid))   # does not work with tests/testthat/utf8.TextGrid  :-(
            close(fid)
        }

        encodingWeight[I] <- length(grep('Text', flines))
    }

    return(encodings[which.max(encodingWeight)])
}


#' tg.read
#'
#' Loads TextGrid from Praat in Text or Short text format (UTF-8),
#' it handles both Interval and Point tiers.
#' Labels can may contain quotation marks and new lines.
#'
#' @param fileNameTextGrid Input file name
#' @param encoding File encoding (default: "UTF-8"), "auto" for auto-detect of Unicode encoding
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



# wrLine
#
# Write text line to a connection in binary mode.
#
# @param string Text line.
# @param fid A connection object.
#
# @return a raw vector (if fid is a raw vector) or invisibly NULL.
wrLine <- function(string, fid) {
    writeBin(c(charToRaw(string), as.raw(c(13, 10))), fid, endian = "little")
}


#' tg.write
#'
#' Saves TextGrid to the file. TextGrid may contain both interval and point
#' tiers (tg[[1]], tg[[2]], tg[[3]], etc.). If tier type is not specified in $type,
#' is is assumed to be "interval". If specified, $type have to be "interval" or "point".
#' If there is no class(tg)["tmin"] and class(tg)["tmax"], they are calculated as min and max of
#' all tiers. The file is saved in UTF-8 encoding.
#'
#' @param tg TextGrid object
#' @param fileNameTextGrid Output file name
#' @param format Output file format ("short" (default, short text format) or "text" (a.k.a. full text format))
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

    fid <- file(fileNameTextGrid, open = "wb", encoding = "UTF-8")
    if (!isOpen(fid)) {
        stop(paste0("cannot open file [", fileNameTextGrid, "]"))
    }

    wrLine('File type = "ooTextFile"', fid)
    wrLine('Object class = "TextGrid"', fid)
    wrLine("", fid)
    if (format == "short") {
        wrLine(as.character(round2(minTimeTotal, -15)), fid)  # min time from all tiers
        wrLine(as.character(round2(maxTimeTotal, -15)), fid)  # max time from all tiers
        wrLine("<exists>", fid)
        wrLine(as.character(nTiers), fid)
    } else if (format == "text") {
        wrLine(paste0("xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid)  # min time from all tiers
        wrLine(paste0("xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid)  # max time from all tiers
        wrLine("tiers? <exists> ", fid)
        wrLine(paste0("size = ", as.character(nTiers), " "), fid)
        wrLine("item []: ", fid)
    }

    for (N in seqM(1, nTiers)) {
        if (format == "text") {
            wrLine(paste0("    item [", as.character(N), "]:"), fid)
        }

        if (tg[[N]]$typInt == TRUE) {  # interval tier
            if (format == "short") {
                wrLine('"IntervalTier"', fid)
                wrLine(paste0('"', tg[[N]]$name, '"'), fid)
            } else if (format == "text") {
                wrLine('        class = "IntervalTier" ', fid)
                wrLine(paste0('        name = "', tg[[N]]$name, '" '), fid)
            }

            nInt <- length(tg[[N]]$t1)  # number of intervals
            if (nInt > 0) {
                if (format == "short") {
                    wrLine(as.character(round2(tg[[N]]$t1[1], -15)), fid)  # start time of the tier
                    wrLine(as.character(round2(tg[[N]]$t2[length(tg[[N]]$t2)], -15)), fid)  # end time of the tier
                    wrLine(as.character(nInt), fid)  # pocet intervalu textgrid
                } else if (format == "text") {
                    wrLine(paste0("        xmin = ", as.character(round2(tg[[N]]$t1[1], -15)), " "), fid)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(tg[[N]]$t2[length(tg[[N]]$t2)], -15)), " "), fid)  # end time of the tier
                    wrLine(paste0("        intervals: size = ", as.character(nInt), " "), fid)  # pocet intervalu textgrid
                }

                for (I in seqM(1, nInt)) {
                    if (format == "short") {
                        wrLine(as.character(round2(tg[[N]]$t1[I], -15)), fid)
                        wrLine(as.character(round2(tg[[N]]$t2[I], -15)), fid)
                        wrLine(paste0('"', tg[[N]]$label[I], '"'), fid)
                    } else if (format == "text") {
                        wrLine(paste0("        intervals [", as.character(I), "]:"), fid)
                        wrLine(paste0("            xmin = ", as.character(round2(tg[[N]]$t1[I], -15)), " "), fid)
                        wrLine(paste0("            xmax = ", as.character(round2(tg[[N]]$t2[I], -15)), " "), fid)
                        wrLine(paste0('            text = "', tg[[N]]$label[I], '" '), fid)
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
                    wrLine(paste0("        xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid)  # end time of the tier
                    wrLine("        intervals: size = 1 ", fid)  # number of intervals
                    wrLine("        intervals [1]:", fid)
                    wrLine(paste0("            xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid)
                    wrLine(paste0("            xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid)
                    wrLine('            text = "" ', fid)
                }
            }
        } else { # pointTier
            if (format == "short") {
                wrLine('"TextTier"', fid)
                wrLine(paste0('"', tg[[N]]$name, '"'), fid)
            } else if (format == "text") {
                wrLine('        class = "TextTier" ', fid)
                wrLine(paste0('        name = "', tg[[N]]$name, '" '), fid)
            }

            nInt <- length(tg[[N]]$t)  # number of points
            if (nInt > 0) {
                if (format == "short") {
                    wrLine(as.character(round2(tg[[N]]$t[1], -15)), fid)  # start time of the tier
                    wrLine(as.character(round2(tg[[N]]$t[length(tg[[N]]$t)], -15)), fid)  # end time of the tier
                    wrLine(as.character(nInt), fid)  # number of points
                } else if (format == "text") {
                    wrLine(paste0("        xmin = ", as.character(round2(tg[[N]]$t[1], -15)), " "), fid)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(tg[[N]]$t[length(tg[[N]]$t)], -15)), " "), fid)  # end time of the tier
                    wrLine(paste0("        points: size = ", as.character(nInt), " "), fid)  # number of points
                }

                for (I in seqM(1, nInt)) {
                    if (format == "short") {
                        wrLine(as.character(round2(tg[[N]]$t[I], -15)), fid)
                        wrLine(paste0('"', tg[[N]]$label[I], '"'), fid)
                    } else if (format == "text") {
                        wrLine(paste0("        points [", as.character(I), "]:"), fid)
                        wrLine(paste0("            number = ", as.character(round2(tg[[N]]$t[I], -15)), " "), fid)
                        wrLine(paste0('            mark = "', tg[[N]]$label[I], '" '), fid)
                    }
                }
            } else { # empty pointtier
                if (format == "short") {
                    wrLine(as.character(round2(minTimeTotal, -15)), fid)  # start time of the tier
                    wrLine(as.character(round2(maxTimeTotal, -15)), fid)  # end time of the tier
                    wrLine("0", fid)  # number of points
                } else if (format == "text") {
                    wrLine(paste0("        xmin = ", as.character(round2(minTimeTotal, -15)), " "), fid)  # start time of the tier
                    wrLine(paste0("        xmax = ", as.character(round2(maxTimeTotal, -15)), " "), fid)  # end time of the tier
                    wrLine("        points: size = 0 ", fid)  # number of points
                }
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
#' @param pt [optional] PitchTier object
#' @param it [optional] IntensityTier object
#'
#' @export
#' @seealso \code{\link{tg.read}}, \code{\link{pt.plot}}
#'
#' @examples
#' \dontrun{
#' tg <- tg.sample()
#' tg.plot(tg)
#' tg.plot(tg.sample(), pt = pt.sample())
#' }
tg.plot <- function(tg, group = "", pt = NULL, it = NULL) {
    ntiers <- tg.getNumberOfTiers(tg)

    y2Axis <- !is.null(pt) | !is.null(it)

    if (ntiers == 0) {
        dygraphs::dygraph(list(x = 0, y = NA), main = "Empty TextGrid")
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
    if (!is.null(it)) {
        tAll <- c(tAll, it$t)
    }

    tAll <- unique(sort(tAll))


    data <- list(t = tAll)

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

    # create tiers
    for (I in seqM(1, ntiers)) {

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
        g <- dygraphs::dyRangeSelector(g, fillColor = "", strokeColor = "")
    }

    # Labels
    for (I in seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            for (J in seqM(1, length(tg[[I]]$label))) {
                g <- dygraphs::dyAnnotation(g, tg[[I]]$t[J], text = tg[[I]]$label[J], width = -0.1, height = 25, series = tg[[I]]$name, tooltip = tg[[I]]$label[J])
                # width = -0.1: trick to get "right alignment". Original: width = 10*max(1, nchar(tg[[I]]$label[J]))
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

    # style of tiers
    for (I in seqM(1, ntiers)) {
        if (tg[[I]]$type == "point") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, pointSize = 2, strokeWidth = 0)

        } else if (tg[[I]]$type == "interval") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, pointSize = 2, strokeWidth = 1)

        } else {
            stop("Unknown tier type")
        }
    }

    if (!y2Axis) {
        g <- dygraphs::dyAxis(g, "y", label = "TextGrid", valueRange = c(0, length(tg)+2))
    } else {
        g <- dygraphs::dyAxis(g, "y", label = "TextGrid", valueRange = c(0, length(tg)*2+2))  # *2

        if (!is.null(pt) & !is.null(it)) {
            y_min <- min(c(pt$f, it$i))
            y_max <- max(c(pt$f, it$i))
            delta <- (y_max - y_min)*4/3
            yMin <- y_min - delta
            yMax <- y_min + delta
            g <- dygraphs::dyAxis(g, "y2", label = "PitchTier & IntensityTier", independentTicks = TRUE, valueRange = c(yMin, yMax))
            g <- dygraphs::dySeries(g, "PitchTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0)
            g <- dygraphs::dySeries(g, "IntensityTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0)
        } else if (!is.null(pt)) {
            delta <- (max(pt$f)-min(pt$f))*4/3
            yMin <- min(pt$f) - delta
            yMax <- min(pt$f) + delta
            g <- dygraphs::dyAxis(g, "y2", label = "PitchTier", independentTicks = TRUE, valueRange = c(yMin, yMax))
            g <- dygraphs::dySeries(g, "PitchTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0)
        } else if (!is.null(it)) {
            delta <- (max(it$i)-min(it$i))*4/3
            yMin <- min(it$i) - delta
            yMax <- min(it$i) + delta
            g <- dygraphs::dyAxis(g, "y2", label = "IntensityTier", independentTicks = TRUE, valueRange = c(yMin, yMax))
            g <- dygraphs::dySeries(g, "IntensityTier", axis = "y2", drawPoints = TRUE, pointSize = 2, strokeWidth = 0)
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
#' @param verbose [optional, default=TRUE] If FALSE, the function performs everything quietly.
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
#' Returns TRUE if the tier is IntervalTier, FALSE otherwise.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return TRUE / FALSE
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
#' Returns TRUE if the tier is PointTier, FALSE otherwise.
#'
#' @param tg TextGrid object
#' @param tierInd tier index or "name"
#'
#' @return TRUE / FALSE
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
#' @param newInd new tier index (1 = the first, Inf = the last [default])
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
#' Duplicate tier originalInd and merge segments (according to the pattern) to the new tier with specified index newInd
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
#'         as classical non-empty labels. See example - one label is " ", therefore it must be specified in the pattern.
#'
#' @param tg TextGrid object
#' @param originalInd tier index or "name"
#' @param newInd new tier index (1 = the first, Inf = the last [default])
#' @param newTierName name of the new tier
#' @param pattern merge segments pattern for the new tier (e.g., "he-llo-world")
#' @param sep separator in pattern (default: "-")
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

    # pozor, nějak se též vypořádat s prázdnými labely - ideálně je zachovat a brát je též jako oddělovač, tedy v rámci jedné "part" nemůže být uvnitř prázdný label

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
#' @param newInd new tier index (1 = the first, Inf = the last [default])
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
#' @param newInd new tier index (1 = the first, Inf = the last [default])
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
#' tStart <= time < tEnd. Tier index must belong to interval tier.
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
#' time <= pointTime. Tier index must belong to point tier.
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
#' pointTime <= time. Tier index must belong to point tier.
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
#' Returns index of point which is nearest the given time (from both sides).
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
#' @param returnTime If TRUE, return vectors of begin (t1) and end time (t2) for each found group of sequence of labels instead of indices (when FALSE = default).
#'
#' @return If returnTime == FALSE, returns list of all occurrences, each member of the list is one occurence and contains vector of label indices, if returnTime == TRUE, returns list witch vectors t1 (begin) and t2 (end) for each found group of sequence of labels.
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
        indStart <- indStart[indStart <= length(tg[[tierInd]]$label) - nlabs + 1]  # pokud zbývá do konce méně labelů, než hledáme, nemá smysl hledat

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
#' @param tStart beginning time of time frame to be cut (default -Inf = cut from the tmin of the TextGrid)
#' @param tEnd final time of time frame to be cut (default Inf = cut to the tmax of the TextGrid)
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
#' @param tStart beginning time of time frame to be cut (default -Inf = cut from the tmin of the TextGrid)
#' @param tEnd final time of time frame to be cut (default Inf = cut to the tmax of the TextGrid)
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





#' pitch.read
#'
#' Reads Pitch object from Praat.
#' Supported formats: text file, short text file.
#'
#' @param fileNamePitch file name of Pitch object
#' @param encoding File encoding (default: "UTF-8"), "auto" for auto-detect of Unicode encoding
#'
#' @return A Pitch object represents periodicity candidates as a function of time.
#' @return   [ref: Praat help, http://www.fon.hum.uva.nl/praat/manual/Pitch.html]
#' @return   p$xmin ... start time (seconds)
#' @return   p$xmax ... end time (seconds)
#' @return   p$nx   ... number of frames
#' @return   p$dx   ... time step = frame duration (seconds)
#' @return   p$x1   ... time associated with the first frame (seconds)
#' @return   p$t    ... vector of time instances associated with all frames
#' @return   p$ceiling        ... a frequency above which a candidate is considered  voiceless (Hz)
#' @return   p$maxnCandidates ... maximum number of candidates in frame
#' @return   p$frame[[1]] to p$frame[[p$nx]] ... frames
#' @return      p$frame[[1]]$intensity   ... intensity of the frame
#' @return      p$frame[[1]]$nCandidates ... actual number of candidates in this frame
#' @return      p$frame[[1]]$frequency ... vector of candidates' frequency (in Hz)
#' @return                               (for a voiced candidate), or 0 (for an unvoiced candidate)
#' @return      p$frame[[1]]$strength  ... vector of degrees of periodicity of candidates (between 0 and 1)
#' @export
#' @seealso \code{\link{pt.read}}, \code{\link{tg.read}}, \code{\link{it.read}}, \code{\link{col.read}}
#'
#' @examples
#' \dontrun{
#' p <- pitch.read('demo/sound.Pitch')
#' names(p)
#' p$nx
#' p$t[4]        # time instance of the 4th frame
#' p$frame[[4]]  # 4th frame: pitch candidates
#' p$frame[[4]]$frequency[2]
#' p$frame[[4]]$strength[2]
#' }
pitch.read <- function(fileNamePitch, encoding = "UTF-8") {
    if (!isString(fileNamePitch)) {
        stop("Invalid 'fileNamePitch' parameter.")
    }

    if (!isString(encoding)) {
        stop("Invalid 'encoding' parameter.")
    }

    if (encoding == "auto") {
        encoding <- detectEncoding(fileNamePitch)
    }

    if (encoding == "UTF-8") {
        flines <- readr::read_lines(fileNamePitch, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
    } else {
        fid <- file(fileNamePitch, open = "r", encoding = encoding)
        flines <- readLines(fid)   # does not work with tests/testthat/utf8.TextGrid  :-(
        close(fid)
    }

    if (length(flines) < 1) {
        stop("Empty file.")
    }

    pitch_ind <- pitch.read_lines(flines)
    class(pitch_ind[[1]])["type"] <- "Pitch 1"
    class(pitch_ind[[1]])["name"] <- basename(fileNamePitch)
    return(pitch_ind[[1]])
}


pitch.read_lines <- function(flines, find = 1, collection = FALSE) {
    if (collection  ||  flines[find-1+ 1] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile
        if (!collection) {
            if (length(flines)-find+1 < 11) {
                stop("Unknown Pitch format.")
            }

            if (flines[find-1+ 2] != "Object class = \"Pitch 1\"") {
                stop("Unknown Pitch format.")
            }

            if (flines[find-1+ 3] != "") {
                stop("Unknown Pitch format.")
            }

            if (nchar(flines[find-1+ 4]) < 1) {
                stop("Unknown Pitch format.")
            }
        } else {
            find <- find - 3
        }

        if (str_contains(flines[find-1+ 4], "xmin")) {  # TextFile
            xmin <- as.numeric(          substr(strTrim(flines[find-1+ 4]),   8, nchar(strTrim(flines[find-1+ 4]))))
            xmax <- as.numeric(          substr(strTrim(flines[find-1+ 5]),   8, nchar(strTrim(flines[find-1+ 5]))))
            nx <- as.numeric(            substr(strTrim(flines[find-1+ 6]),   6, nchar(strTrim(flines[find-1+ 6]))))
            dx <- as.numeric(            substr(strTrim(flines[find-1+ 7]),   6, nchar(strTrim(flines[find-1+ 7]))))
            x1 <- as.numeric(            substr(strTrim(flines[find-1+ 8]),   6, nchar(strTrim(flines[find-1+ 8]))))
            ceil <- as.numeric(          substr(strTrim(flines[find-1+ 9]),  11, nchar(strTrim(flines[find-1+ 9]))))
            maxnCandidates <- as.numeric(substr(strTrim(flines[find-1+ 10]), 18, nchar(strTrim(flines[find-1+ 10]))))

            frame <- vector("list", nx)

            if (!str_contains(flines[find-1+ 11], "frame []: ")) {
                stop("Unknown Pitch format.")
            }

            iline <- find-1+ 12  # index of line to read

            for (I in seqM(1, nx)) {
                if (strTrim(flines[iline]) != paste0("frame [", I, "]:")) {
                    stop(paste0("Unknown Pitch format, wrong frame id (", I, "')."))
                }
                iline <- iline + 1

                intensity <- as.numeric(substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline])))); iline <- iline + 1
                nCandidates <- as.numeric(substr(strTrim(flines[iline]), 15, nchar(strTrim(flines[iline])))); iline <- iline + 1

                if (!str_contains(flines[iline], "candidate []:")) {
                    stop("Unknown Pitch format.")
                }
                iline <- iline + 1

                frequency <- numeric(nCandidates)
                strength <- numeric(nCandidates)

                for (Ic in seqM(1, nCandidates)) {
                    if (strTrim(flines[iline]) != paste0("candidate [", Ic, "]:")) {
                        stop(paste0("Unknown Pitch format, wrong candidate nr. (", Ic, ") in frame id (", I, "')."))
                    }
                    iline <- iline + 1

                    frequency[Ic] <- as.numeric(substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline])))); iline <- iline + 1
                    strength[Ic] <-  as.numeric(substr(strTrim(flines[iline]), 12, nchar(strTrim(flines[iline])))); iline <- iline + 1
                }

                frame[[I]] <- list(intensity = intensity, nCandidates = nCandidates,
                                   frequency = frequency, strength =  strength)
            }

        } else {   # shortTextFile
            xmin <- as.numeric(flines[find-1+ 4])
            xmax <- as.numeric(flines[find-1+ 5])
            nx <- as.numeric(flines[find-1+ 6])
            dx <- as.numeric(flines[find-1+ 7])
            x1 <- as.numeric(flines[find-1+ 8])
            ceil <- as.numeric(flines[find-1+ 9])
            maxnCandidates <- as.numeric(flines[find-1+ 10])

            frame <- vector("list", nx)

            iline <- find-1+ 11  # index of line to read

            for (I in seqM(1, nx)) {
                intensity <- as.numeric(flines[iline]); iline <- iline + 1
                nCandidates <- as.numeric(flines[iline]); iline <- iline + 1

                frequency <- numeric(nCandidates)
                strength <- numeric(nCandidates)

                for (Ic in seqM(1, nCandidates)) {
                    frequency[Ic] <- as.numeric(flines[iline]); iline <- iline + 1
                    strength[Ic] <- as.numeric(flines[iline]); iline <- iline + 1
                }

                frame[[I]] <- list(intensity = intensity, nCandidates = nCandidates,
                                   frequency = frequency, strength =  strength)
            }
        }

    } else {   # unknown format
        stop("Unknown Pitch format.")
    }


    p <- list(xmin = xmin, xmax = xmax, nx = nx, dx = dx, x1 = x1, t = seqM(0, (nx-1))*dx + x1,
              ceiling = ceil, maxnCandidates = maxnCandidates,
              frame = frame)

    return(list(p, iline))
}



#' formant.read
#'
#' Reads Formant object from Praat.
#' Supported formats: text file, short text file.
#'
#' @param fileNameFormant file name of Formant object
#' @param encoding File encoding (default: "UTF-8"), "auto" for auto-detect of Unicode encoding
#'
#' @return A Formant object represents formants as a function of time.
#' @return   [ref: Praat help, http://www.fon.hum.uva.nl/praat/manual/Formant.html]
#' @return   f$xmin ... start time (seconds)
#' @return   f$xmax ... end time (seconds)
#' @return   f$nx   ... number of frames
#' @return   f$dx   ... time step = frame duration (seconds)
#' @return   f$x1   ... time associated with the first frame (seconds)
#' @return   f$t    ... vector of time instances associated with all frames
#' @return   f$maxnFormants ... maximum number of formants in frame
#' @return   f$frame[[1]] to f$frame[[f$nx]] ... frames
#' @return      f$frame[[1]]$intensity ... intensity of the frame
#' @return      f$frame[[1]]$nFormants ... actual number of formants in this frame
#' @return      f$frame[[1]]$frequency ... vector of formant frequencies (in Hz)
#' @return      f$frame[[1]]$bandwidth ... vector of formant bandwidths (in Hz)
#' @export
#' @seealso \code{\link{pitch.read}}, \code{\link{pt.read}}, \code{\link{tg.read}}, \code{\link{it.read}}, \code{\link{col.read}}
#'
#' @examples
#' \dontrun{
#' f <- formant.read('demo/maminka.Formant')
#' names(f)
#' f$nx
#' f$t[4]        # time instance of the 4th frame
#' f$frame[[4]]  # 4th frame: formants
#' f$frame[[4]]$frequency[2]
#' f$frame[[4]]$bandwidth[2]
#' }
formant.read <- function(fileNameFormant, encoding = "UTF-8") {
    if (!isString(fileNameFormant)) {
        stop("Invalid 'fileNameFormant' parameter.")
    }

    if (!isString(encoding)) {
        stop("Invalid 'encoding' parameter.")
    }

    if (encoding == "auto") {
        encoding <- detectEncoding(fileNameFormant)
    }

    if (encoding == "UTF-8") {
        flines <- readr::read_lines(fileNameFormant, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
    } else {
        fid <- file(fileNameFormant, open = "r", encoding = encoding)
        flines <- readLines(fid)   # does not work with tests/testthat/utf8.TextGrid  :-(
        close(fid)
    }

    if (length(flines) < 1) {
        stop("Empty file.")
    }

    formant_ind <- formant.read_lines(flines)
    class(formant_ind[[1]])["type"] <- "Formant 2"
    class(formant_ind[[1]])["name"] <- basename(fileNameFormant)
    return(formant_ind[[1]])
}

formant.read_lines <- function(flines, find = 1, collection = FALSE) {
    if (collection  ||  flines[find-1+ 1] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile
        if (!collection) {
            if (length(flines)-find+1 < 10) {
                stop("Unknown Formant format.")
            }

            if (flines[find-1+ 2] != "Object class = \"Formant 2\"") {
                stop("Unknown Formant format.")
            }

            if (flines[find-1+ 3] != "") {
                stop("Unknown Formant format.")
            }

            if (nchar(flines[find-1+ 4]) < 1) {
                stop("Unknown Formant format.")
            }
        } else {
            find <- find - 3
        }

        if (str_contains(flines[find-1+ 4], "xmin")) {  # TextFile
            xmin <- as.numeric(          substr(strTrim(flines[find-1+ 4]),   8, nchar(strTrim(flines[find-1+ 4]))))
            xmax <- as.numeric(          substr(strTrim(flines[find-1+ 5]),   8, nchar(strTrim(flines[find-1+ 5]))))
            nx <- as.numeric(            substr(strTrim(flines[find-1+ 6]),   6, nchar(strTrim(flines[find-1+ 6]))))
            dx <- as.numeric(            substr(strTrim(flines[find-1+ 7]),   6, nchar(strTrim(flines[find-1+ 7]))))
            x1 <- as.numeric(            substr(strTrim(flines[find-1+ 8]),   6, nchar(strTrim(flines[find-1+ 8]))))
            maxnFormants <- as.numeric(substr(strTrim(flines[find-1+ 9]), 16, nchar(strTrim(flines[find-1+ 9]))))

            frame <- vector("list", nx)

            if (!str_contains(flines[find-1+ 10], "frames []: ")) {
                stop("Unknown Formant format.")
            }

            iline <- find-1+ 11  # index of line to read

            for (I in seqM(1, nx)) {
                if (strTrim(flines[iline]) != paste0("frames [", I, "]:")) {
                    stop(paste0("Unknown Formant format, wrong frame id (", I, "')."))
                }
                iline <- iline + 1

                intensity <- as.numeric(substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline])))); iline <- iline + 1
                nFormants <- as.numeric(substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline])))); iline <- iline + 1

                if (!str_contains(flines[iline], "formant []:")) {
                    stop("Unknown Formants format.")
                }
                iline <- iline + 1

                frequency <- numeric(nFormants)
                bandwidth <- numeric(nFormants)

                for (If in seqM(1, nFormants)) {
                    if (strTrim(flines[iline]) != paste0("formant [", If, "]:")) {
                        stop(paste0("Unknown Formant format, wrong formant nr. (", If, ") in frame id (", I, "')."))
                    }
                    iline <- iline + 1

                    frequency[If] <- as.numeric(substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline])))); iline <- iline + 1
                    bandwidth[If] <- as.numeric(substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline])))); iline <- iline + 1
                }

                frame[[I]] <- list(intensity = intensity, nFormants = nFormants,
                                   frequency = frequency, bandwidth = bandwidth)
            }

        } else {   # shortTextFile
            xmin <- as.numeric(flines[find-1+ 4])
            xmax <- as.numeric(flines[find-1+ 5])
            nx <- as.numeric(flines[find-1+ 6])
            dx <- as.numeric(flines[find-1+ 7])
            x1 <- as.numeric(flines[find-1+ 8])
            maxnFormants <- as.numeric(flines[find-1+ 9])

            frame <- vector("list", nx)

            iline <- find-1+ 10  # index of line to read

            for (I in seqM(1, nx)) {
                intensity <- as.numeric(flines[iline]); iline <- iline + 1
                nFormants <- as.numeric(flines[iline]); iline <- iline + 1

                frequency <- numeric(nFormants)
                bandwidth <- numeric(nFormants)

                for (If in seqM(1, nFormants)) {
                    frequency[If] <- as.numeric(flines[iline]); iline <- iline + 1
                    bandwidth[If] <- as.numeric(flines[iline]); iline <- iline + 1
                }

                frame[[I]] <- list(intensity = intensity, nFormants = nFormants,
                                   frequency = frequency, bandwidth = bandwidth)
            }
        }

    } else {   # unknown format
        stop("Unknown Formant format.")
    }


    f <- list(xmin = xmin, xmax = xmax, nx = nx, dx = dx, x1 = x1, t = seqM(0, (nx-1))*dx + x1,
              maxnFormants = maxnFormants,
              frame = frame)

    return(list(f, iline))
}


#' formant.toArray
#'
#' @param formant Formant object
#'
#' @return Formant object with frames converted to frequency and bandwidth arrays and intensity vector
#' @export
#'
#' @seealso \code{\link{formant.read}}, \code{\link{formant.plot}}
#'
#' @examples
#' formantArray <- formant.toArray(formant.sample())
#' formantArray$t[1:10]
#' formantArray$frequencyArray[, 1:10]
#' formantArray$bandwidthArray[, 1:10]
#' formantArray$intensityVector[1:10]
#' \dontrun{
#' plot(formantArray$t, formantArray$frequencyArray[1, ]) # draw 1st formant track
#' }
formant.toArray <- function(formant) {
    frequencyArray <- array(NA_real_, dim = c(formant$maxnFormants, formant$nx))
    bandwidthArray <- array(NA_real_, dim = c(formant$maxnFormants, formant$nx))
    intensityVector <- numeric(formant$nx)
    # udelat na toto funkci?

    for (I in seqM(1, formant$nx)) {
        f <- formant$frame[[I]]$frequency
        frequencyArray[seqM(1,length(f)), I] <- f

        b <- formant$frame[[I]]$bandwidth
        bandwidthArray[seqM(1,length(f)), I] <- b

        intensityVector[I] <- formant$frame[[I]]$intensity
    }

    formantArray <- list(xmin = formant$xmin, xmax = formant$xmax, nx = formant$nx, dx = formant$dx, x1 = formant$x1, t = formant$t,
                         maxnFormants = formant$maxnFormants, frequencyArray = frequencyArray, bandwidthArray = bandwidthArray,
                         intensityVector = intensityVector)

    return(formantArray)
}


#' formant.plot
#'
#' Plots interactive Formant object using dygraphs package.
#'
#' @param formant Formant object
#' @param scaleIntensity Point size scaled according to relative intensity
#' @param drawBandwidth Draw formant bandwidth
#' @param group [optional] character string, name of group for dygraphs synchronization
#'
#' @export
#' @seealso \code{\link{formant.read}}, \code{\link{formant.sample}}, \code{\link{formant.toArray}}, \code{\link{tg.plot}}
#'
#' @examples
#' \dontrun{
#' formant <- formant.sample()
#' formant.plot(formant, drawBandwidth = TRUE)
#' }
formant.plot <- function(formant, scaleIntensity = TRUE, drawBandwidth = FALSE, group = "") {
    fArray <- formant.toArray(formant)

    if (scaleIntensity) {
        intensityNorm <- log10(fArray$intensityVector)
        intensityNorm <- intensityNorm - min(intensityNorm) + 1
        intensityNorm <- intensityNorm / max(intensityNorm) * 6 # maximum radius
        intensityNorm <- intensityNorm - min(intensityNorm) + 1 # minimum radius [e.g., 1.1]
    } else {
        intensityNorm <- rep(2, formant$nx)
    }


    data <- list(t = formant$t)

    for (I in seqM(1, formant$maxnFormants)) {
        data[[length(data)+1]] <- fArray$frequencyArray[I, ]
        names(data)[length(data)] <- paste0("F", I)

        if (drawBandwidth) {
            data[[length(data)+1]] <- fArray$frequencyArray[I, ] - fArray$bandwidthArray[I, ]/2
            names(data)[length(data)] <- paste0("lwr", I)

            data[[length(data)+1]] <- fArray$frequencyArray[I, ] + fArray$bandwidthArray[I, ]/2
            names(data)[length(data)] <- paste0("upr", I)
        }
    }

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    g <- dygraphs::dyOptions(g, drawPoints = TRUE)
    g <- dygraphs::dyCallbacks(g, "drawPointCallback" = sprintf(
        "
        function(g, name, ctx, canvasx, canvasy, color, radius, index) {
        var radius_str = %s;
        radius = radius_str[index];
        return Dygraph.Circles.DEFAULT(g, name, ctx, canvasx, canvasy, color, radius)
        }
        ",
        paste0("[", paste0(intensityNorm, collapse = ","), "]") ))

    if (drawBandwidth) {
        for (I in seqM(1, formant$maxnFormants)) {
            g <- dygraphs::dySeries(g, c(paste0("lwr", I), paste0("F", I), paste0("upr", I)))
        }
    }


    g <- dygraphs::dyRangeSelector(g, dateWindow = c(formant$xmin, formant$xmax), fillColor = "")

    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    print(g)
}





#' pt.read
#'
#' Reads PitchTier from Praat. Supported formats: text file, short text file,
#' spreadsheet, headerless spreadsheet (headerless not recommended,
#' it does not contain tmin and tmax info).
#'
#' @param fileNamePitchTier file name of PitchTier
#' @param encoding File encoding (default: "UTF-8"), "auto" for auto-detect of Unicode encoding
#'
#' @return PitchTier object
#' @export
#' @seealso \code{\link{pt.write}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{pt.interpolate}}, \code{\link{pt.legendre}}, \code{\link{tg.read}}, \code{\link{pitch.read}}, \code{\link{formant.read}}, \code{\link{it.read}}, \code{\link{col.read}}
#'
#' @examples
#' \dontrun{
#' pt <- pt.read("demo/H.PitchTier")
#' pt.plot(pt)
#' }
pt.read <- function(fileNamePitchTier, encoding = "UTF-8") {
    if (!isString(fileNamePitchTier)) {
        stop("Invalid 'fileNamePitchTier' parameter.")
    }

    if (!isString(encoding)) {
        stop("Invalid 'encoding' parameter.")
    }

    if (encoding == "auto") {
        encoding <- detectEncoding(fileNamePitchTier)
    }

    if (encoding == "UTF-8") {
        flines <- readr::read_lines(fileNamePitchTier, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
    } else {
        fid <- file(fileNamePitchTier, open = "r", encoding = encoding)
        flines <- readLines(fid)   # does not work with tests/testthat/utf8.TextGrid  :-(
        close(fid)
    }


    if (length(flines) < 1) {
        stop("Empty file.")
    }

    pt_ind <- pt.read_lines(flines)
    class(pt_ind[[1]])["type"] <- "PitchTier"
    class(pt_ind[[1]])["name"] <- basename(fileNamePitchTier)
    return(pt_ind[[1]])
}

pt.read_lines <- function(flines, find = 1, collection = FALSE) {
    if (flines[find-1+1] == "\"ooTextFile\"") {    # spreadSheet - cannot be in collection file
        if (collection) {
            stop("unsupported PitchTier format (SpreadSheet) in collection")
        }

        if (length(flines)-find+1 < 3) {
            stop("Unknown PitchTier format.")
        }

        if (flines[find-1+2] != "\"PitchTier\"") {
            stop("Unknown PitchTier format.")
        }

        fromToN <- stringr::str_split(flines[find-1+3], " ")
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

        for (I in seqM(1, N, by = 1)) {
            tf <- stringr::str_split(flines[find-1+I+3], "\\s")
            if (length(tf[[1]]) != 2) {
                stop("Unknown PitchTier format.")
            }
            t[I] <- as.numeric(tf[[1]][[1]])
            f[I] <- as.numeric(tf[[1]][[2]])
        }


    } else if (collection  ||  flines[find-1+1] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile - only these 2 formats can be stored in collection file
        if (!collection) {
            if (length(flines)-find+1 < 6) {
                stop("Unknown PitchTier format.")
            }

            if (strTrim(flines[find-1+2]) != "Object class = \"PitchTier\"") {
                stop("Unknown PitchTier format.")
            }

            if (strTrim(flines[find-1+3]) != "") {
                stop("Unknown PitchTier format.")
            }

            if (strTrim(nchar(flines[find-1+4])) < 1) {
                stop("Unknown PitchTier format.")
            }
        } else {
            find <- find - 3
        }

        if (str_contains(flines[find-1+4], "xmin")) {  # TextFile
            xmin <- as.numeric(substr(strTrim(flines[find-1+4]), 8,  nchar(strTrim(flines[find-1+4]))))
            xmax <- as.numeric(substr(strTrim(flines[find-1+5]), 8,  nchar(strTrim(flines[find-1+5]))))
            N <-    as.numeric(substr(strTrim(flines[find-1+6]), 16, nchar(strTrim(flines[find-1+6]))))

            # if (N != (length(flines)-6)/3) {
            #     stop("Wrong number of points in PitchTier format.")
            # }
            t <- numeric(N)
            f <- numeric(N)

            for (I in seqM(1, N, by = 1)) {
                t[I] <- as.numeric(substr(strTrim(flines[find-1+8 + (I-1)*3]), 10, nchar(strTrim(flines[find-1+8 + (I-1)*3]))))
                f[I] <- as.numeric(substr(strTrim(flines[find-1+9 + (I-1)*3]), 9, nchar(strTrim(flines[find-1+9 + (I-1)*3]))))
            }

            find <- find-1+9 + (N-1)*3 + 1

        } else {   # shortTextFile

            xmin <- as.numeric(flines[find-1+4])
            xmax <- as.numeric(flines[find-1+5])
            N <- as.integer(flines[find-1+6])

            # if (N != (length(flines)-6)/2) {
            #     stop("Wrong number of points in PitchTier format.")
            # }
            t <- numeric(N)
            f <- numeric(N)

            for (I in seqM(1, N, by = 1)) {
                t[I] <- as.numeric(flines[find-1+7 + (I-1)*2])
                f[I] <- as.numeric(flines[find-1+8 + (I-1)*2])
            }

            find <- find-1+8 + (N-1)*2 + 1
        }


    } else {   # headerless SpreadSheet - cannot be in collection file
        if (collection) {
            stop("unsupported PitchTier format (headerless SpreadSheet) in collection")
        }

        N <- length(flines)

        t <- numeric(N)
        f <- numeric(N)

        for (I in seqM(1, N, by = 1)) {
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

    return(list(pt, find))
}



#' pt.write
#'
#' Saves PitchTier to file (in UTF-8 encoding).
#' pt is list with at least $t and $f vectors (of the same length).
#' If there are no $tmin and $tmax values, there are
#' set as min and max of $t vector.
#'
#' @param pt PitchTier object
#' @param fileNamePitchTier file name to be created
#' @param format Output file format ("short" (short text format), "text" (a.k.a. full text format), "spreadsheet" (default), "headerless" (not recommended, it does not contain tmin and tmax info))
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
pt.write <- function(pt, fileNamePitchTier, format = "spreadsheet") {
    if (!isString(fileNamePitchTier)) {
        stop("Invalid 'fileNamePitchTier' parameter.")
    }

    if (!isString(format)) {
        stop("Invalid 'format' parameter.")
    }

    if (format != "short" && format != "text" && format != "spreadsheet" && format != "headerless") {
        stop("Unsupported format (supported: short, text, spreadsheet [default], headerless)")
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


    fid <- file(fileNamePitchTier, open = "wb", encoding = "UTF-8")
    if (!isOpen(fid)) {
        stop(paste0("cannot open file [", fileNamePitchTier, "]"))
    }

    if (format == "spreadsheet") {
        wrLine('"ooTextFile"', fid)
        wrLine('"PitchTier"', fid)
    } else if (format == "short" || format == "text") {
        wrLine('File type = "ooTextFile"', fid)
        wrLine('Object class = "PitchTier"', fid)
        wrLine('', fid)
    }

    if (format == "spreadsheet") {
        wrLine(paste0(as.character(round2(xmin, -15)), " ", as.character(round2(xmax, -15)), " ", as.character(N)), fid)
    } else if (format == "short") {
        wrLine(as.character(round2(xmin, -15)), fid)
        wrLine(as.character(round2(xmax, -15)), fid)
        wrLine(as.character(N), fid)
    } else if (format == "text") {
        wrLine(paste0("xmin = ", as.character(round2(xmin, -15)), " "), fid)
        wrLine(paste0("xmax = ", as.character(round2(xmax, -15)), " "), fid)
        wrLine(paste0("points: size = ", as.character(N), " "), fid)
    }

    for (n in seqM(1, N)) {
        if (format == "spreadsheet" || format == "headerless") {
            wrLine(paste0(as.character(round2(pt$t[n], -15)), "\t", as.character(round2(pt$f[n], -15))), fid)
        } else if (format == "short") {
            wrLine(as.character(round2(pt$t[n], -15)), fid)
            wrLine(as.character(round2(pt$f[n], -15)), fid)
        } else if (format == "text") {
            wrLine(paste0("points [", as.character(n), "]:"), fid)
            wrLine(paste0("    number = ", as.character(round2(pt$t[n], -15)), " "), fid)
            wrLine(paste0("    value = ", as.character(round2(pt$f[n], -15)), " "), fid)
        }
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
    g <- dygraphs::dyRangeSelector(g, dateWindow = c(pt$tmin, pt$tmax), fillColor = "")

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
#' pt.plot(pt2) %>% dygraphs::dyAxis("y", label = "Frequency (ST re 200 Hz)")
#' }
pt.Hz2ST <- function(pt, ref=100) {
    if (!isNum(ref) | ref <= 0) {
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
#' @return Vector of Legendre polynomials coefficients
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
    if (!isInt(npoints) | npoints < 0) {
        stop("npoints must be integer >= 0.")
    }

    if (!isInt(npolynomials) | npolynomials <= 0) {
        stop("npolynomials must be integer > 0.")
    }

    pt <- pt.interpolate(pt, seq(pt$tmin, pt$tmax, length.out = npoints))

    y <- pt$f


    lP <- npoints # počet vzorků polynomu
    nP <- npolynomials

    B <- matrix(nrow = nP, ncol = lP)  # báze
    x <- seq(-1, 1, length.out = lP)

    for (i in seqM(1, nP)) {
        n <- i - 1
        p <- numeric(lP)
        for (k in seqM(0, n)) {
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
#' @param c Vector of Legendre polynomials coefficients
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

    if (!isInt(npoints) | npoints < 0) {
        stop("npoints must be integer >= 0.")
    }

    lP <- npoints # počet vzorků polynomu
    nP <- length(c)

    B <- matrix(nrow = nP, ncol = lP)  # báze
    x <- seq(-1, 1, length.out = lP)

    for (i in seqM(1, nP)) {
        n <- i - 1
        p <- numeric(lP)
        for (k in seqM(0, n)) {
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
#' @param tStart beginning time of interval to be cut (default -Inf = cut from the tmin of the PitchTier)
#' @param tEnd final time of interval to be cut (default Inf = cut to the tmax of the PitchTier)
#'
#' @return PitchTier object
#' @export
#' @seealso \code{\link{pt.cut0}}, \code{\link{tg.cut}}, \code{\link{tg.cut0}}, \code{\link{pt.read}}, \code{\link{pt.plot}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.interpolate}}, \code{\link{pt.legendre}}, \code{\link{pt.legendreSynth}}, \code{\link{pt.legendreDemo}}
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





#' it.read
#'
#' Reads IntensityTier from Praat. Supported formats: text file, short text file.
#'
#' @param fileNameIntensityTier file name of IntensityTier
#' @param encoding File encoding (default: "UTF-8"), "auto" for auto-detect of Unicode encoding
#'
#' @return IntensityTier object
#' @export
#' @seealso \code{\link{it.write}}, \code{\link{it.plot}}, \code{\link{it.cut}}, \code{\link{it.cut0}}, \code{\link{it.interpolate}}, \code{\link{tg.read}}, \code{\link{pt.read}}, \code{\link{pitch.read}}, \code{\link{formant.read}}, \code{\link{col.read}}
#'
#' @examples
#' \dontrun{
#' it <- it.read("demo/maminka.IntensityTier")
#' it.plot(it)
#' }
it.read <- function(fileNameIntensityTier, encoding = "UTF-8") {
    if (!isString(fileNameIntensityTier)) {
        stop("Invalid 'fileNameIntensityTier' parameter.")
    }

    if (!isString(encoding)) {
        stop("Invalid 'encoding' parameter.")
    }

    if (encoding == "auto") {
        encoding <- detectEncoding(fileNameIntensityTier)
    }

    if (encoding == "UTF-8") {
        flines <- readr::read_lines(fileNameIntensityTier, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
    } else {
        fid <- file(fileNameIntensityTier, open = "r", encoding = encoding)
        flines <- readLines(fid)
        close(fid)
    }


    if (length(flines) < 1) {
        stop("Empty file.")
    }

    it_ind <- it.read_lines(flines)
    class(it_ind[[1]])["type"] <- "IntensityTier"
    class(it_ind[[1]])["name"] <- basename(fileNameIntensityTier)
    return(it_ind[[1]])
}

it.read_lines <- function(flines, find = 1, collection = FALSE) {
    if (collection  ||  flines[find-1+1] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile - only these 2 formats can be stored in collection file
        if (!collection) {
            if (length(flines)-find+1 < 6) {
                stop("Unknown IntensityTier format.")
            }

            if (strTrim(flines[find-1+2]) != "Object class = \"IntensityTier\"") {
                stop("Unknown IntensityTier format.")
            }

            if (strTrim(flines[find-1+3]) != "") {
                stop("Unknown IntensityTier format.")
            }

            if (strTrim(nchar(flines[find-1+4])) < 1) {
                stop("Unknown IntensityTier format.")
            }
        } else {
            find <- find - 3
        }

        if (str_contains(flines[find-1+4], "xmin")) {  # TextFile
            xmin <- as.numeric(substr(strTrim(flines[find-1+4]), 8,  nchar(strTrim(flines[find-1+4]))))
            xmax <- as.numeric(substr(strTrim(flines[find-1+5]), 8,  nchar(strTrim(flines[find-1+5]))))
            N <-    as.numeric(substr(strTrim(flines[find-1+6]), 16, nchar(strTrim(flines[find-1+6]))))

            t <- numeric(N)
            i <- numeric(N)

            for (I in seqM(1, N, by = 1)) {
                t[I] <- as.numeric(substr(strTrim(flines[find-1+8 + (I-1)*3]), 10, nchar(strTrim(flines[find-1+8 + (I-1)*3]))))
                i[I] <- as.numeric(substr(strTrim(flines[find-1+9 + (I-1)*3]), 9, nchar(strTrim(flines[find-1+9 + (I-1)*3]))))
            }

            find <- find-1+9 + (N-1)*3 + 1

        } else {   # shortTextFile

            xmin <- as.numeric(flines[find-1+4])
            xmax <- as.numeric(flines[find-1+5])
            N <- as.integer(flines[find-1+6])

            t <- numeric(N)
            i <- numeric(N)

            for (I in seqM(1, N, by = 1)) {
                t[I] <- as.numeric(flines[find-1+7 + (I-1)*2])
                i[I] <- as.numeric(flines[find-1+8 + (I-1)*2])
            }

            find <- find-1+8 + (N-1)*2 + 1
        }


    } else {
        stop("unsupported IntensityTier format")
    }


    it <- list(t = t, i = i, tmin = xmin, tmax = xmax)

    return(list(it, find))
}

#' it.write
#'
#' Saves IntensityTier to file (in UTF-8 encoding).
#' it is list with at least $t and $i vectors (of the same length).
#' If there are no $tmin and $tmax values, there are
#' set as min and max of $t vector.
#'
#' @param it IntensityTier object
#' @param fileNameIntensityTier file name to be created
#' @param format Output file format ("short" (short text format - default), "text" (a.k.a. full text format))
#'
#' @export
#' @seealso \code{\link{it.read}}, \code{\link{tg.write}}, \code{\link{it.interpolate}}
#'
#' @examples
#' \dontrun{
#' it <- it.sample()
#' it.plot(pt)
#' it.write(it, "demo/intensity.IntensityTier")
#' }
it.write <- function(it, fileNameIntensityTier, format = "short") {
    if (!isString(fileNameIntensityTier)) {
        stop("Invalid 'fileNameIntensityTier' parameter.")
    }

    if (!isString(format)) {
        stop("Invalid 'format' parameter.")
    }

    if (format != "short" && format != "text") {
        stop("Unsupported format (supported: short [default], text)")
    }

    if (!("t" %in% names(it))) {
        stop("it must be a list with 't' and 'i' and optionally 'tmin' and 'tmax'")
    }
    if (!("i" %in% names(it))) {
        stop("it must be a list with 't' and 'i' and optionally 'tmin' and 'tmax'")
    }
    if (length(it$t) != length(it$i)) {
        stop("t and i lengths mismatched.")
    }
    N <- length(it$t)


    if (!("tmin" %in% names(it))) {
        xmin <- min(it$t)
    } else {
        xmin <- it$tmin
    }
    if (!("tmax" %in% names(it))) {
        xmax <- max(it$t)
    } else {
        xmax <- it$tmax
    }


    fid <- file(fileNameIntensityTier, open = "wb", encoding = "UTF-8")
    if (!isOpen(fid)) {
        stop(paste0("cannot open file [", fileNameIntensityTier, "]"))
    }

    if (format == "short" || format == "text") {
        wrLine('File type = "ooTextFile"', fid)
        wrLine('Object class = "IntensityTier"', fid)
        wrLine('', fid)
    }

    if (format == "short") {
        wrLine(as.character(round2(xmin, -15)), fid)
        wrLine(as.character(round2(xmax, -15)), fid)
        wrLine(as.character(N), fid)
    } else if (format == "text") {
        wrLine(paste0("xmin = ", as.character(round2(xmin, -15)), " "), fid)
        wrLine(paste0("xmax = ", as.character(round2(xmax, -15)), " "), fid)
        wrLine(paste0("points: size = ", as.character(N), " "), fid)
    }

    for (n in seqM(1, N)) {
        if (format == "short") {
            wrLine(as.character(round2(it$t[n], -15)), fid)
            wrLine(as.character(round2(it$i[n], -15)), fid)
        } else if (format == "text") {
            wrLine(paste0("points [", as.character(n), "]:"), fid)
            wrLine(paste0("    number = ", as.character(round2(it$t[n], -15)), " "), fid)
            wrLine(paste0("    value = ", as.character(round2(it$i[n], -15)), " "), fid)
        }
    }

    close(fid)
}


#' it.plot
#'
#' Plots interactive IntensityTier using dygraphs package.
#'
#' @param it IntensityTier object
#' @param group [optional] character string, name of group for dygraphs synchronization
#'
#' @export
#' @seealso \code{\link{it.read}}, \code{\link{tg.plot}}, \code{\link{it.cut}}, \code{\link{it.cut0}}, \code{\link{it.interpolate}}, \code{\link{it.write}}
#'
#' @examples
#' \dontrun{
#' it <- it.sample()
#' it.plot(it)
#' }
it.plot <- function(it, group = "") {
    data <- list(t = it$t, i = it$i)

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    g <- dygraphs::dyOptions(g, drawPoints = TRUE, pointSize = 2, strokeWidth = 0)
    g <- dygraphs::dyRangeSelector(g, dateWindow = c(it$tmin, it$tmax), fillColor = "")

    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    g
}


#' it.interpolate
#'
#' Interpolates IntensityTier contour in given time instances.
#'
#'  a) If t < min(it$t) (or t > max(it$t)), returns the first (or the last) value of it$i.
#'  b) If t is existing point in it$t, returns the respective it$f.
#'  c) If t is Between two existing points, returns linear interpolation of these two points.
#'
#' @param it IntensityTier object
#' @param t vector of time instances of interest
#'
#' @return IntensityTier object
#' @export
#' @seealso \code{\link{it.read}}, \code{\link{it.write}}, \code{\link{it.plot}}, \code{\link{it.cut}}, \code{\link{it.cut0}}, \code{\link{it.legendre}}
#'
#' @examples
#' it <- it.sample()
#' it2 <- it.interpolate(it, seq(it$t[1], it$t[length(it$t)], by = 0.001))
#' \dontrun{
#' it.plot(it)
#' it.plot(it2)
#' }
it.interpolate <- function(it, t) {
    if (class(t) != "numeric"  &  class(t) != "integer") {
        stop("t must be numeric vector")
    }

    if (length(it$t) != length(it$i))
        stop("IntensityTier does not have equal length vectors $t and $i")

    if (length(it$t) < 1)
        return(NA)

    if (!identical(sort(it$t), it$t)) {
        stop("time instances $t in IntensityTier are not increasingly sorted")
    }

    if (!identical(unique(it$t), it$t)) {
        stop("duplicated time instances in $t vector of the IntensityTier")
    }

    it2 <- it
    it2$t <- t

    i <- numeric(length(t))
    for (I in seq_along(t)) {
        if (length(it$t) == 1) {
            i[I] <- it$i[1]
        } else if (t[I] < it$t[1]) {   # a)
            i[I] <- it$i[1]
        } else if (t[I] > it$t[length(it$t)]) {   # a)
            i[I] <- it$i[length(it$t)]
        } else {
            # b)
            ind <- which(it$t == t[I])
            if (length(ind) == 1) {
                i[I] <- it$i[ind]
            } else {
                # c)
                ind2 <- which(it$t > t[I]); ind2 <- ind2[1]
                ind1 <- ind2 - 1
                # y = ax + b;  a = (y2-y1)/(x2-x1);  b = y1 - ax1
                a <- (it$i[ind2] - it$i[ind1]) / (it$t[ind2] - it$t[ind1])
                b <- it$i[ind1] - a*it$t[ind1]
                i[I] <- a*t[I] + b
            }
        }
    }

    it2$i <- i
    return(it2)
}


#' it.legendre
#'
#' Interpolate the IntensityTier in 'npoints' equidistant points and approximate it by Legendre polynomials
#'
#' @param it IntensityTier object
#' @param npoints Number of points of IntensityTier interpolation
#' @param npolynomials Number of polynomials to be used for Legendre modelling
#'
#' @return Vector of Legendre polynomials coefficients
#' @export
#' @seealso \code{\link{it.legendreSynth}}, \code{\link{it.legendreDemo}}, \code{\link{it.cut}}, \code{\link{it.cut0}}, \code{\link{it.read}}, \code{\link{it.plot}}, \code{\link{it.interpolate}}
#'
#' @examples
#' it <- it.sample()
#' it <- it.cut(it, tStart = 0.2, tEnd = 0.4)  # cut IntensityTier and preserve time
#' c <- it.legendre(it)
#' print(c)
#' leg <- it.legendreSynth(c)
#' itLeg <- it
#' itLeg$t <- seq(itLeg$tmin, itLeg$tmax, length.out = length(leg))
#' itLeg$i <- leg
#' \dontrun{
#' plot(it$t, it$i, xlab = "Time (sec)", ylab = "Intensity (dB)")
#' lines(itLeg$t, itLeg$i, col = "blue")
#' }
it.legendre <- function(it, npoints = 1000, npolynomials = 4) {
    if (!isInt(npoints) | npoints < 0) {
        stop("npoints must be integer >= 0.")
    }

    if (!isInt(npolynomials) | npolynomials <= 0) {
        stop("npolynomials must be integer > 0.")
    }

    it <- it.interpolate(it, seq(it$tmin, it$tmax, length.out = npoints))

    y <- it$i


    lP <- npoints # počet vzorků polynomu
    nP <- npolynomials

    B <- matrix(nrow = nP, ncol = lP)  # báze
    x <- seq(-1, 1, length.out = lP)

    for (i in seqM(1, nP)) {
        n <- i - 1
        p <- numeric(lP)
        for (k in seqM(0, n)) {
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

#' it.legendreSynth
#'
#' Synthetize the contour from vector of Legendre polynomials 'c' in 'npoints' equidistant points
#'
#' @param c Vector of Legendre polynomials coefficients
#' @param npoints Number of points of IntensityTier interpolation
#'
#' @return Vector of values of synthetized contour
#' @export
#' @seealso \code{\link{it.legendre}}, \code{\link{it.legendreDemo}}, \code{\link{it.read}}, \code{\link{it.plot}}, \code{\link{it.interpolate}}
#'
#' @examples
#' it <- it.sample()
#' it <- it.cut(it, tStart = 0.2, tEnd = 0.4)  # cut IntensityTier and preserve time
#' c <- it.legendre(it)
#' print(c)
#' leg <- it.legendreSynth(c)
#' itLeg <- it
#' itLeg$t <- seq(itLeg$tmin, itLeg$tmax, length.out = length(leg))
#' itLeg$i <- leg
#' \dontrun{
#' plot(it$t, it$i, xlab = "Time (sec)", ylab = "Intensity (dB)")
#' lines(itLeg$t, itLeg$i, col = "blue")
#' }
it.legendreSynth <- function(c, npoints = 1000) {
    if (class(c) != "numeric"  &  class(c) != "integer") {
        stop("c must be numeric vector")
    }

    if (!isInt(npoints) | npoints < 0) {
        stop("npoints must be integer >= 0.")
    }

    lP <- npoints # počet vzorků polynomu
    nP <- length(c)

    B <- matrix(nrow = nP, ncol = lP)  # báze
    x <- seq(-1, 1, length.out = lP)

    for (i in seqM(1, nP)) {
        n <- i - 1
        p <- numeric(lP)
        for (k in seqM(0, n)) {
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

#' it.legendreDemo
#'
#' Plots first four Legendre polynomials
#'
#' @export
#' @seealso \code{\link{it.legendre}}, \code{\link{it.legendreSynth}}, \code{\link{it.read}}, \code{\link{it.plot}}, \code{\link{it.interpolate}}
#'
#' @examples
#' \dontrun{
#' it.legendreDemo()
#' }
it.legendreDemo <- function() {
    graphics::par(mfrow = c(2, 2))
    graphics::plot(it.legendreSynth(c(1, 0, 0, 0), 1024))
    graphics::plot(it.legendreSynth(c(0, 1, 0, 0), 1024))
    graphics::plot(it.legendreSynth(c(0, 0, 1, 0), 1024))
    graphics::plot(it.legendreSynth(c(0, 0, 0, 1), 1024))
    graphics::par(mfrow = c(1, 1))
}



#' it.cut
#'
#' Cut the specified interval from the IntensityTier and preserve time
#'
#' @param it IntensityTier object
#' @param tStart beginning time of interval to be cut (default -Inf = cut from the tMin of the IntensityTier)
#' @param tEnd final time of interval to be cut (default Inf = cut to the tMax of the IntensityTier)
#'
#' @return IntensityTier object
#' @export
#' @seealso \code{\link{it.cut0}}, \code{\link{it.read}}, \code{\link{it.plot}}, \code{\link{it.interpolate}}, \code{\link{it.legendre}}, \code{\link{it.legendreSynth}}, \code{\link{it.legendreDemo}}
#'
#' @examples
#' it <- it.sample()
#' it2 <-   it.cut(it,  tStart = 0.3)
#' it2_0 <- it.cut0(it, tStart = 0.3)
#' it3 <-   it.cut(it,  tStart = 0.2, tEnd = 0.3)
#' it3_0 <- it.cut0(it, tStart = 0.2, tEnd = 0.3)
#' it4 <-   it.cut(it,  tEnd = 0.3)
#' it4_0 <- it.cut0(it, tEnd = 0.3)
#' it5 <-   it.cut(it,  tStart = -1, tEnd = 1)
#' it5_0 <- it.cut0(it, tStart = -1, tEnd = 1)
#' \dontrun{
#' it.plot(it)
#' it.plot(it2)
#' it.plot(it2_0)
#' it.plot(it3)
#' it.plot(it3_0)
#' it.plot(it4)
#' it.plot(it4_0)
#' it.plot(it5)
#' it.plot(it5_0)
#' }
it.cut <- function(it, tStart = -Inf, tEnd = Inf) {
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

    it2 <- it
    it2$t <- it$t[it$t >= tStart  &  it$t <= tEnd]
    it2$i <- it$i[it$t >= tStart  &  it$t <= tEnd]

    if (is.infinite(tStart)) {
        it2$tmin <- it$tmin
    } else {
        it2$tmin <- tStart
    }

    if (is.infinite(tEnd)) {
        it2$tmax <- it$tmax
    } else {
        it2$tmax <- tEnd
    }

    return(it2)
}

#' it.cut0
#'
#' Cut the specified interval from the IntensityTier and shift time so that the new tmin = 0
#'
#' @param it IntensityTier object
#' @param tStart beginning time of interval to be cut (default -Inf = cut from the tMin of the IntensityTier)
#' @param tEnd final time of interval to be cut (default Inf = cut to the tMax of the IntensityTier)
#'
#' @return IntensityTier object
#' @export
#' @seealso \code{\link{it.cut}}, \code{\link{it.read}}, \code{\link{it.plot}}, \code{\link{it.interpolate}}, \code{\link{it.legendre}}, \code{\link{it.legendreSynth}}, \code{\link{it.legendreDemo}}
#'
#' @examples
#' it <- it.sample()
#' it2 <-   it.cut(it,  tStart = 0.3)
#' it2_0 <- it.cut0(it, tStart = 0.3)
#' it3 <-   it.cut(it,  tStart = 0.2, tEnd = 0.3)
#' it3_0 <- it.cut0(it, tStart = 0.2, tEnd = 0.3)
#' it4 <-   it.cut(it,  tEnd = 0.3)
#' it4_0 <- it.cut0(it, tEnd = 0.3)
#' it5 <-   it.cut(it,  tStart = -1, tEnd = 1)
#' it5_0 <- it.cut0(it, tStart = -1, tEnd = 1)
#' \dontrun{
#' it.plot(it)
#' it.plot(it2)
#' it.plot(it2_0)
#' it.plot(it3)
#' it.plot(it3_0)
#' it.plot(it4)
#' it.plot(it4_0)
#' it.plot(it5)
#' it.plot(it5_0)
#' }
it.cut0 <- function(it, tStart = -Inf, tEnd = Inf) {
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

    it2 <- it
    it2$t <- it$t[it$t >= tStart  &  it$t <= tEnd]
    it2$i <- it$i[it$t >= tStart  &  it$t <= tEnd]

    if (is.infinite(tStart)) {
        it2$tmin <- it$tmin
    } else {
        it2$tmin <- tStart
    }

    if (is.infinite(tEnd)) {
        it2$tmax <- it$tmax
    } else {
        it2$tmax <- tEnd
    }

    it2$t <- it2$t - it2$tmin
    it2$tmax <- it2$tmax - it2$tmin
    it2$tmin <- 0

    return(it2)
}


#' as.tg
#'
#' Renames the class(tg)["name"] attribute and sets class(tg)["type"] <- "TextGrid" (if it is not already set)
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

#' as.pt
#'
#' Renames the class(pt)["name"] attribute and sets class(pt)["type"] <- "PitchTier" (if it is not already set)
#'
#' @param pt PitchTier object
#' @param name New name
#'
#' @return PitchTier object
#' @export
#'
#' @examples
#' class(pt.sample())
#' class(as.pt(pt.sample(), name = "New Name"))
as.pt <- function(pt, name = "") {
    class(pt)["type"] <- "PitchTier"
    class(pt)["name"] <- name
    return(pt)
}

#' as.it
#'
#' Renames the class(it)["name"] attribute and sets class(it)["type"] <- "IntensityTier" (if it is not already set)
#'
#' @param it IntensityTier object
#' @param name New name
#'
#' @return IntensityTier object
#' @export
#'
#' @examples
#' class(it.sample())
#' class(as.it(it.sample(), name = "New Name"))
as.it <- function(it, name = "") {
    class(it)["type"] <- "IntensityTier"
    class(it)["name"] <- name
    return(it)
}

#' as.pitch
#'
#' Renames the class(pitch)["name"] attribute and sets class(pitch)["type"] <- "Pitch 1" (if it is not already set)
#'
#' @param pitch Pitch 1 object
#' @param name New name
#'
#' @return Pitch 1 object
#' @export
#'
#' @examples
#' class(pitch.sample())
#' class(as.pitch(pitch.sample(), name = "New Name"))
as.pitch <- function(pitch, name = "") {
    class(pitch)["type"] <- "Pitch 1"
    class(pitch)["name"] <- name
    return(pitch)
}

#' as.formant
#'
#' Renames the class(formant)["name"] attribute and sets class(formant)["type"] <- "Formant 2" (if it is not already set)
#'
#' @param formant Formant 2 object
#' @param name New name
#'
#' @return Formant 2 object
#' @export
#'
#' @examples
#' class(formant.sample())
#' class(as.formant(formant.sample(), name = "New Name"))
as.formant <- function(formant, name = "") {
    class(formant)["type"] <- "Formant 2"
    class(formant)["name"] <- name
    return(formant)
}

# tierInd <- tg.checkTierInd(tg, tierInd)
#     ntiers <- length(tg)



# if (!isString(name)) {
#     stop("Name must be a character string.")
# }

# if (!isLogical(name)) {
#     stop("Name must be a logical value.")
# }

# if (!isNum(tMin)) {
#     stop("tMin must be a number.")
# }

# if (!isInt(newInd)) {
#     stop("newInd must be integer >= 1.")
# }



#' strTrim
#'
#' Trim leading and trailing whitespace in character string.
#'
#' Like str_trim() in stringr package or trimws() in R3.2.0 but way faster.
#'
#' Source: Hadley Wickham comment at http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
#'
#' @param string character string
#'
#' @return returns a character string with removed leading and trailing whitespace characters.
#' @export
#' @seealso \code{\link{isString}} for testing whether it is 1 character vector, \code{\link{str_contains}} for finding string in string without regexp, \code{\link{str_find}} for all indices without regexp, \code{\link{str_find1}} for the first index withoud regexp.
#' @examples
#' strTrim("      Hello World!    ")
strTrim <- function (string) {
    gsub("^\\s+|\\s+$", "", string)
}






#' seqM
#'
#' Matlab-like behaviour of colon operator or linspace for creating sequences, for-loop friendly.
#'
#' Like \code{seq()} but with Matlab-like behavior ([: operator] with \code{by} or [linspace] with \code{length.out}).
#'
#' If I create a for-loop, I would like to get an empty vector for 3:1 (I want a default step +1)
#' and also an empty vector for seq(3, 1, by = 1) (not an error). This is solved by this \code{seqM} function.
#'
#' @section Comparison:
#' \tabular{lllll}{
#'   R: seqM  \tab    \tab                        Matlab  \tab \tab                          R: seq  \cr
#'   seqM(1, 3)  \tab       [1] 1 2 3      \tab           1:3           \tab  the same           \tab        the same \cr
#'   seqM(1, 3, by=.8) \tab [1] 1.0 1.8 2.6 \tab          1:.8:3        \tab  the same            \tab       the same \cr
#'   seqM(1, 3, by=5)  \tab [1] 1          \tab           1:5:3         \tab  the same             \tab      the same \cr
#'   seqM(3, 1)     \tab    integer(0)    \tab            3:1           \tab  the same            \tab       [1] 3 2 1 \cr
#'   seqM(3, 1, by=+1) \tab integer(0)    \tab            3:1:1         \tab  the same            \tab       Error: wrong 'by' \cr
#'   seqM(3, 1, by=-1) \tab [1] 3 2 1     \tab            3:-1:1        \tab  the same            \tab       the same \cr
#'   seqM(3, 1, by=-3) \tab [1] 3        \tab             3:-3:1        \tab  the same            \tab       the same \cr
#'   seqM(1, 3, len=5) \tab [1] 1.0 1.5 2.0 2.5 3.0  \tab linspace(1,3,5) \tab the same           \tab        the same \cr
#'   seqM(1, 3, len=3) \tab [1] 1 2 3       \tab          linspace(1,3,3) \tab the same           \tab        the same \cr
#'   seqM(1, 3, len=2) \tab [1] 1 3        \tab           linspace(1,3,2) \tab the same           \tab        the same \cr
#'   seqM(1, 3, len=1) \tab [1] 3          \tab          linspace(1,3,1) \tab the same             \tab      [1] 1 \cr
#'   seqM(1, 3, len=0) \tab integer(0) + warning \tab     linspace(1,3,0) \tab the same without warning \tab  the same without warning \cr
#'   seqM(3, 1, len=3) \tab [1] 3 2 1          \tab       linspace(3,1,3) \tab the same                \tab   the same  \cr
#' }
#'
#'
#' @param from starting value of the sequence (the first number)
#' @param to end value of the sequence (the last number or the boundary number)
#' @param by increment of the sequence (if specified, do not use the \code{length.out} parameter). If both \code{by} and \code{length.out} are not specified, then \code{by = +1}.
#' @param length.out desired length of the sequence (if specified, do not use the \code{by} parameter)
#'
#' @return returns a vector of type "integer" or "double"
#' @export
#' @seealso \code{\link{round2}}, \code{\link{isNum}}, \code{\link{isInt}}, \code{\link{ifft}}.
#'
#' @examples
#' seqM(1, 3)
#' seqM(1, 3, by=.8)
#' seqM(1, 3, by=5)
#' seqM(3, 1)
#' seqM(3, 1, by=+1)
#' seqM(3, 1, by=-1)
#' seqM(3, 1, by=-3)
#' seqM(1, 3, len=5)
#' seqM(1, 3, len=3)
#' seqM(1, 3, len=2)
#' seqM(1, 3, len=1)
#' seqM(1, 3, len=0)
#' seqM(3, 1, len=3)
#'
#'
#'
seqM <- function(from=NA, to=NA, by=NA, length.out=NA) {
    # nonsense or default parameters

    if (!is.na(from) & class(from) != "numeric" & class(from) != "integer")
        stop("'from' must be numeric or integer")
    if (!is.na(from) & length(from) != 1)
        stop("'from' must be 1 number")

    if (!is.na(to) & class(to) != "numeric" & class(to) != "integer")
        stop("'to' must be numeric or integer")
    if (!is.na(to) & length(to) != 1)
        stop("'to' must be 1 number")

    if (length(by) != 1)
        stop("'by' must be 1 number")
    if (!is.na(by)) {
        if (class(by) != "numeric" & class(by) != "integer")
            stop("'by' must be numeric or integer")
    }

    if (length(length.out) != 1)
        stop("'length.out' must be 1 number")
    if (!is.na(length.out)) {
        if (class(length.out) != "numeric" & class(length.out) != "integer")
            stop("'length.out' must be numeric or integer")
    }

    if (!is.na(by) & !is.na(length.out)) {
        if (!is.na(from) & !is.na(to)) {
            stop("too many arguments, cannot set 'by' and 'length.out' together with both 'from' and 'to'")
        }

        # VAR 3) length.out + by
        if (!isInt(length.out)) {
            len <- trunc(length.out)
            warning(paste0("length.out is not integer (length.out=", length.out, "), truncating it to: ", len))
            length.out <- len
        }
        if (length.out == 0) {
            warning("length.out == 0, return empty vector")
            return(integer(0))
        }
        if (length.out < 0) {
            warning(paste0("length.out < 0 (length.out=", length.out, "), return empty vector"))
            return(integer(0))
        }

        if (is.na(to)) {  # from
            if (isInt(from) & isInt(by)) {
                outInt <- TRUE
                from <- as.integer(from)
                by <- as.integer(by)
            }
            else {
                outInt <- FALSE
            }

            if (outInt)
                return(seq.int(from = from, by = by, length.out = length.out))
            else
                return(seq(from = from, by = by, length.out = length.out))

        } else {          # to
            if (isInt(to) & isInt(by)) {
                outInt <- TRUE
                to <- as.integer(to)
                by <- as.integer(by)
            }
            else {
                outInt <- FALSE
            }

            if (outInt)
                return(seq.int(to = to, by = by, length.out = length.out))
            else
                return(seq(to = to, by = by, length.out = length.out))
        }
    }

    if (is.na(by) & is.na(length.out))
        by <- 1


    # VAR 1) length.out
    if (!is.na(length.out)) {
        if (!isInt(length.out)) {
            len <- trunc(length.out)
            warning(paste0("length.out is not integer (length.out=", length.out, "), truncating it to: ", len))
            length.out <- len
        }
        if (length.out == 0) {
            warning("length.out == 0, return empty vector")
            return(integer(0))
        }
        if (length.out < 0) {
            warning(paste0("length.out < 0 (length.out=", length.out, "), return empty vector"))
            return(integer(0))
        }
        if (length.out == 1) {
            return(to)  # Matlab behavior
        }
        return(seq(from, to, length.out = length.out))
    }


    # VAR 2) by
    # integer or numeric?
    if (isInt(from) & isInt(to) & isInt(by)) {
        outInt <- TRUE
        from <- as.integer(from)
        to <- as.integer(to)
        by <- as.integer(by)
    }
    else {
        outInt <- FALSE
    }

    if (by == 0) {
        warning("by == 0, return empty vector")
        return(integer(0))
    }

    if (by > 0) {
        if (from > to)
            return(integer(0))

        if (outInt)
            return(seq.int(from, to, by))
        else
            return(seq(from, to, by))

    } else {
        if (from < to)
            return(integer(0))

        if (outInt)
            return(seq.int(from, to, by))
        else
            return(seq(from, to, by))
    }

}




#' isInt
#'
#' Returns TRUE / FALSE whether it is exactly 1 integer number (in fact, the class can be numeric but the number must be integer), non-missing
#'
#' @param num variable to be tested
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{isNum}}, \code{\link{isLogical}}, \code{\link{isString}}
#'
#' @examples
#' isInt(2)
#' isInt(2L)
#' isInt(-2)
#' isInt(-2L)
#' isInt(2.1)
#' isInt(-2.1)
#' isInt(1:5)
#' isInt(NA_integer_)
#' isInt(integer(0))
isInt <- function(num) {
    if (!("numeric" %in% class(num))  &  !("integer" %in% class(num)))
        return(FALSE)

    if (length(num) != 1)
        return(FALSE)

    if (is.na(num))
        return(FALSE)

    if (trunc(num) == num)
        return(TRUE)
    else
        return(FALSE)
}

#' isString
#'
#' Returns TRUE / FALSE whether it is exactly 1 character string (character vector of length 1, non-missing)
#'
#' @param string variable to be tested
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{isInt}}, \code{\link{isNum}}, \code{\link{isLogical}}
#'
#' @examples
#' isString("hello")
#' isString(2)
#' isString(c("hello", "world"))
#' isString(NA_character_)
isString <- function(string) {
    if (!("character" %in% class(string)))
        return(FALSE)

    if (length(string) != 1)
        return(FALSE)

    if (is.na(string))
        return(FALSE)

    return(TRUE)
}

#' isNum
#'
#' Returns TRUE / FALSE whether it is exactly 1 number (numeric or integer vector of length 1, non-missing)
#'
#' @param num variable to be tested
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{isInt}}, \code{\link{isLogical}}, \code{\link{isString}}
#'
#' @examples
#' isNum(2)
#' isNum(2L)
#' isNum(-2)
#' isNum(-2L)
#' isNum(2.1)
#' isNum(-2.1)
#' isNum(1:5)
#' isNum(NA_real_)
#' isNum(numeric(0))
isNum <- function(num) {
    if (!("numeric" %in% class(num))  &  !("integer" %in% class(num)))
        return(FALSE)

    if (length(num) != 1)
        return(FALSE)

    if (is.na(num))
        return(FALSE)

    return(TRUE)
}

#' isLogical
#'
#' Returns TRUE / FALSE whether it is exactly 1 logical value, non-missing
#'
#' @param logical variable to be tested
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{isNum}}, \code{\link{isInt}}, \code{\link{isString}}
#'
#' @examples
#' isLogical(TRUE)
#' isLogical(FALSE)
#' isLogical(1)
#' isLogical(0)
#' isLogical(2)
#' isLogical(NA)
#' isLogical(NaN)
#' isLogical(logical(0))
isLogical <- function(logical) {
    if (!("logical" %in% class(logical)))
        return(FALSE)

    if (length(logical) != 1)
        return(FALSE)

    if (is.na(logical))
        return(FALSE)

    return(TRUE)
}



#' round2
#'
#' Rounds a number to the specified order. Round half away from zero (this is the difference from built-in \code{round} function.)
#'
#' @param x number to be rounded
#' @param order 0 (default) = units, -1 = 0.1, +1 = 10
#'
#' @return rounded number to the specified order
#' @export
#' @seealso \code{\link{round}}, \code{\link{trunc}}, \code{\link{ceiling}}, \code{\link{floor}}
#'
#' @examples
#' round2(23.5)   # = 24, compare: round(23.5) = 24
#' round2(23.4)   # = 23
#' round2(24.5)   # = 25, compare: round(24.5) = 24
#' round2(-23.5)   # = -24, compare: round(-23.5) = -24
#' round2(-23.4)   # = -23
#' round2(-24.5)   # = -25, compare: round(-24.5) = -24
#' round2(123.456, -1)   # 123.5
#' round2(123.456, -2)   # 123.46
#' round2(123.456, 1)  # 120
#' round2(123.456, 2)  # 100
#' round2(123.456, 3)  # 0
#' round2(-123.456, -1)   # -123.5
#' round2(-123.456, -2)   # -123.46
#' round2(-123.456, 1)  # -120
#' round2(-123.456, 2)  # -100
#' round2(-123.456, 3)  # 0
round2 <- function(x, order = 0) {
    zaokrouhli <- function(cislo) {
        return(trunc(cislo + sign(cislo)*0.5))
    }

    return(zaokrouhli(x / 10^order) * 10^order)
}


#' str_contains
#'
#' Find string in another string (without regular expressions), returns TRUE / FALSE.
#'
#' @param string string in which we try to find something
#' @param patternNoRegex string we want to find, "as it is" - no regular exprressions
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{str_find}}, \code{\link{str_find1}}, \code{\link{isString}}
#'
#' @examples
#' str_contains("Hello world", "wor")  # TRUE
#' str_contains("Hello world", "WOR")  # FALSE
#' str_contains(tolower("Hello world"), tolower("wor"))  # TRUE
#' str_contains("Hello world", "")  # TRUE
str_contains <- function(string, patternNoRegex) {
    return(regexpr(patternNoRegex, string, fixed = TRUE)[1] != -1)
}


#' str_find
#'
#' Find string in another string (without regular expressions), returns indices of all occurences.
#'
#' @param string string in which we try to find something
#' @param patternNoRegex string we want to find, "as it is" - no regular exprressions
#'
#' @return indices of all occurences (1 = 1st character)
#' @export
#' @seealso \code{\link{str_find1}}, \code{\link{str_contains}}, \code{\link{isString}}
#'
#' @examples
#' str_find("Hello, hello, hello world", "ell")   # 2 9 16
#' str_find("Hello, hello, hello world", "q")     # integer(0)
str_find <- function(string, patternNoRegex) {
    indexy <- as.integer(gregexpr(patternNoRegex, string, fixed = TRUE)[[1]])
    if (length(indexy) == 1 & indexy[1] == -1)
        indexy <- integer(0)
    return(indexy)
}


#' str_find1
#'
#' Find string in another string (without regular expressions), returns indices of the first occurence only.
#'
#' @param string string in which we try to find something
#' @param patternNoRegex string we want to find, "as it is" - no regular exprressions
#'
#' @return index of the first occurence only (1 = 1st character)
#' @export
#' @seealso \code{\link{str_find}}, \code{\link{str_contains}}, \code{\link{isString}}
#'
#' @examples
#' str_find1("Hello, hello, hello world", "ell")   # 2
#' str_find1("Hello, hello, hello world", "q")     # integer(0)
str_find1 <- function(string, patternNoRegex) {
    index <- regexpr(patternNoRegex, string, fixed = TRUE)[1]
    if (index == -1)
        index <- integer(0)
    return(index)
}

# inspired by Pol van Rijn's function from mPraat toolbox
getNumberInLine <- function(str, shortFormat = FALSE) {
    if (!shortFormat) {
        numberIndex <- str_find1(str, " = ") + 3  # 3 because nchar(' = ') == 3
        return(as.numeric(stringr::str_sub(str, numberIndex)))
    } else {
        return(as.numeric(str))
    }
}

# inspired by Pol van Rijn's function from mPraat toolbox
getTextInQuotes <- function(str) {
    m <- gregexpr('".*?"', str)  # find quoted text
    text <- regmatches(str, m)
    if (length(text[[1]]) != 1) {
        stop("None or multiple texts in quotes found.")
    }
    return(stringr::str_sub(text[[1]][1], 2, -2)) # remove quotes
}


#' ifft
#'
#' Inverse Fast Fourier Transform (discrete FT), Matlab-like behavior.
#'
#' This is really the inverse of the fft function, so ifft(fft(x)) == x.
#'
#' @param sig input vector
#'
#' @return output vector of the same length as the input vector
#' @export
#' @seealso \code{\link[stats]{fft}}, \code{\link{Re}}, \code{\link{Im}}, \code{\link{Mod}}, \code{\link{Conj}}
#'
#' @examples
#' ifft(fft(1:5))
ifft <- function(sig) {
    return(stats::fft(sig, inverse = TRUE) / length(sig))
}
