#' formant.read
#'
#' Reads Formant object from Praat.
#' Supported formats: text file, short text file.
#'
#' @param fileNameFormant file name of Formant object
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
#'
#' @return A Formant object represents formants as a function of time.
#' @return   [ref: Praat help, http://www.fon.hum.uva.nl/praat/manual/Formant.html]
#' @return   \code{f$xmin} ... start time (seconds)
#' @return   \code{f$xmax} ... end time (seconds)
#' @return   \code{f$nx}   ... number of frames
#' @return   \code{f$dx}   ... time step = frame duration (seconds)
#' @return   \code{f$x1}   ... time associated with the first frame (seconds)
#' @return   \code{f$t}    ... vector of time instances associated with all frames
#' @return   \code{f$maxnFormants} ... maximum number of formants in frame
#' @return   \code{f$frame[[1]]} to \code{f$frame[[f$nx]]} ... frames
#' @return      \code{f$frame[[1]]$intensity} ... intensity of the frame
#' @return      \code{f$frame[[1]]$nFormants} ... actual number of formants in this frame
#' @return      \code{f$frame[[1]]$frequency} ... vector of formant frequencies (in Hz)
#' @return      \code{f$frame[[1]]$bandwidth} ... vector of formant bandwidths (in Hz)
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
        bandwidthArray[seqM(1,length(b)), I] <- b

        intensityVector[I] <- formant$frame[[I]]$intensity
    }

    formantArray <- list(xmin = formant$xmin, xmax = formant$xmax, nx = formant$nx, dx = formant$dx, x1 = formant$x1, t = formant$t,
                         maxnFormants = formant$maxnFormants, frequencyArray = frequencyArray, bandwidthArray = bandwidthArray,
                         intensityVector = intensityVector)

    class(formantArray)["type"] <- class(formant)["type"]
    class(formantArray)["name"] <- class(formant)["name"]

    return(formantArray)
}


#' formant.toFrame
#'
#' @param formantArray Formant object (array format)
#'
#' @return Formant object with frames
#' @export
#'
#' @seealso \code{\link{formant.toArray}}, \code{\link{formant.read}}, \code{\link{formant.plot}}
#'
#' @examples
#' formantArray <- formant.toArray(formant.sample())
#' formant <- formant.toFrame(formantArray)
formant.toFrame <- function(formantArray) {
    if (nrow(formantArray$frequencyArray) != formantArray$maxnFormants  |   ncol(formantArray$frequencyArray) != formantArray$nx) {
        stop("formantArray$frequencyArray dimensions mismatch.")
    }

    if (nrow(formantArray$bandwidthArray) != formantArray$maxnFormants  |   ncol(formantArray$bandwidthArray) != formantArray$nx) {
        stop("formantArray$bandwidthArray dimensions mismatch.")
    }

    if (length(formantArray$t) != formantArray$nx) {
        stop("formantArray$t dimensions mismatch.")
    }

    if (length(formantArray$intensityVector) != formantArray$nx) {
        stop("formantArray$intensityVector dimensions mismatch.")
    }

    frame <- vector("list", formantArray$nx)

    for (I in seqM(1, formantArray$nx)) {
        intensity <- formantArray$intensityVector[I]
        nFormants <- as.numeric(sum(!is.na(formantArray$frequencyArray[, I])))
        frequency <- formantArray$frequencyArray[seqM(1, nFormants), I]
        bandwidth <- formantArray$bandwidthArray[seqM(1, nFormants), I]

        frame[[I]] <- list(intensity = intensity, nFormants = nFormants,
                           frequency = frequency, bandwidth =  bandwidth)
    }


    formant <- list(xmin = formantArray$xmin, xmax = formantArray$xmax, nx = formantArray$nx, dx = formantArray$dx, x1 = formantArray$x1, t = formantArray$t,
                    maxnFormants = formantArray$maxnFormants, frame = frame)

    class(formant)["type"] <- class(formantArray)["type"]
    class(formant)["name"] <- class(formantArray)["name"]

    return(formant)
}


#' formant.plot
#'
#' Plots interactive Formant object using \code{dygraphs} package.
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
formant.plot <- function(formant, scaleIntensity = TRUE, drawBandwidth = TRUE, group = "") {
    fArray <- formant.toArray(formant)

    if (scaleIntensity) {
        intensityNorm <- normIntensity(fArray$intensityVector, 1, 6)  # minimum, maximum radius
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
    g
}

#' as.formant
#'
#' Renames the \code{class(formant)["name"]} attribute and sets \code{class(formant)["type"] <- "Formant 2"} (if it is not already set)
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

