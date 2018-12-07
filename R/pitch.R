
#' pitch.read
#'
#' Reads Pitch object from Praat.
#' Supported formats: text file, short text file.
#'
#' @param fileNamePitch file name of Pitch object
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
#'
#' @return A Pitch object represents periodicity candidates as a function of time.
#' @return   [ref: Praat help, http://www.fon.hum.uva.nl/praat/manual/Pitch.html]
#' @return   \code{p$xmin} ... start time (seconds)
#' @return   \code{p$xmax} ... end time (seconds)
#' @return   \code{p$nx}   ... number of frames
#' @return   \code{p$dx}   ... time step = frame duration (seconds)
#' @return   \code{p$x1}   ... time associated with the first frame (seconds)
#' @return   \code{p$t}    ... vector of time instances associated with all frames
#' @return   \code{p$ceiling}        ... a frequency above which a candidate is considered  voiceless (Hz)
#' @return   \code{p$maxnCandidates} ... maximum number of candidates in frame
#' @return   \code{p$frame[[1]]} to \code{p$frame[[p$nx]]} ... frames
#' @return      \code{p$frame[[1]]$intensity}   ... intensity of the frame
#' @return      \code{p$frame[[1]]$nCandidates} ... actual number of candidates in this frame
#' @return      \code{p$frame[[1]]$frequency} ... vector of candidates' frequency (in Hz)
#' @return                               (for a voiced candidate), or \code{0} (for an unvoiced candidate)
#' @return      \code{p$frame[[1]]$strength}  ... vector of degrees of periodicity of candidates (between \code{0} and \code{1})
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


#' pitch.toArray
#'
#' @param pitch Pitch object (frame format)
#'
#' @return Pitch object with frames converted to frequency and strength arrays and intensity vector
#' @export
#'
#' @seealso \code{\link{pitch.toFrame}}, \code{\link{pitch.read}}, \code{\link{pitch.plot}}
#'
#' @examples
#' pitchArray <- pitch.toArray(pitch.sample())
#' pitchArray$t[1:10]
#' pitchArray$frequencyArray[, 1:10]
#' pitchArray$bandwidthArray[, 1:10]
#' pitchArray$intensityVector[1:10]
pitch.toArray <- function(pitch) {
    frequencyArray <- array(NA_real_, dim = c(pitch$maxnCandidates, pitch$nx))
    strengthArray  <- array(NA_real_, dim = c(pitch$maxnCandidates, pitch$nx))
    intensityVector <- numeric(pitch$nx)
    # udelat na toto funkci?

    for (I in seqM(1, pitch$nx)) {
        f <- pitch$frame[[I]]$frequency
        frequencyArray[seqM(1,length(f)), I] <- f

        s <- pitch$frame[[I]]$strength
        strengthArray[seqM(1,length(s)), I] <- s

        intensityVector[I] <- pitch$frame[[I]]$intensity
    }

    pitchArray <- list(xmin = pitch$xmin, xmax = pitch$xmax, nx = pitch$nx, dx = pitch$dx, x1 = pitch$x1, t = pitch$t, ceiling = pitch$ceiling,
                       maxnCandidates = pitch$maxnCandidates, frequencyArray = frequencyArray, strengthArray = strengthArray,
                       intensityVector = intensityVector)

    class(pitchArray)["type"] <- class(pitch)["type"]
    class(pitchArray)["name"] <- class(pitch)["name"]

    return(pitchArray)
}

#' pitch.toFrame
#'
#' @param pitchArray Pitch object (array format)
#'
#' @return Pitch object with frames
#' @export
#'
#' @seealso \code{\link{pitch.toArray}}, \code{\link{pitch.read}}, \code{\link{pitch.plot}}
#'
#' @examples
#' pitchArray <- pitch.toArray(pitch.sample())
#' pitch <- pitch.toFrame(pitchArray)
pitch.toFrame <- function(pitchArray) {
    if (nrow(pitchArray$frequencyArray) != pitchArray$maxnCandidates  |   ncol(pitchArray$frequencyArray) != pitchArray$nx) {
        stop("pitchArray$frequencyArray dimensions mismatch.")
    }

    if (nrow(pitchArray$strengthArray) != pitchArray$maxnCandidates  |   ncol(pitchArray$strengthArray) != pitchArray$nx) {
        stop("pitchArray$strengthArray dimensions mismatch.")
    }

    if (length(pitchArray$t) != pitchArray$nx) {
        stop("pitchArray$t dimensions mismatch.")
    }

    if (length(pitchArray$intensityVector) != pitchArray$nx) {
        stop("pitchArray$intensityVector dimensions mismatch.")
    }

    frame <- vector("list", pitchArray$nx)

    for (I in seqM(1, pitchArray$nx)) {
        intensity <- pitchArray$intensityVector[I]
        nCandidates <- as.numeric(sum(!is.na(pitchArray$frequencyArray[, I])))
        frequency <- pitchArray$frequencyArray[seqM(1, nCandidates), I]
        strength <- pitchArray$strengthArray[seqM(1, nCandidates), I]

        frame[[I]] <- list(intensity = intensity, nCandidates = nCandidates,
                           frequency = frequency, strength =  strength)
    }


    pitch <- list(xmin = pitchArray$xmin, xmax = pitchArray$xmax, nx = pitchArray$nx, dx = pitchArray$dx, x1 = pitchArray$x1, t = pitchArray$t, ceiling = pitchArray$ceiling,
                  maxnCandidates = pitchArray$maxnCandidates, frame = frame)

    class(pitch)["type"] <- class(pitchArray)["type"]
    class(pitch)["name"] <- class(pitchArray)["name"]

    return(pitch)
}


#' pitch.plot
#'
#' Plots interactive Pitch object using \code{dygraphs} package.
#'
#' @param pitch Pitch object
#' @param scaleIntensity Point size scaled according to relative intensity
#' @param showStrength Show strength annotation
#' @param group [optional] character string, name of group for dygraphs synchronization
#' @param pt [optional] PitchTier object
#'
#' @export
#' @seealso \code{\link{pitch.read}}, \code{\link{pitch.sample}}, \code{\link{pitch.toArray}}, \code{\link{tg.plot}}, \code{\link{pt.plot}}, \code{\link{formant.plot}}
#'
#' @examples
#' \dontrun{
#' pitch <- pitch.sample()
#' pitch.plot(pitch, scaleIntensity = TRUE, showStrength = TRUE)
#'
#' pitch.plot(pitch, scaleIntensity = TRUE, showStrength = TRUE, pt = pt.sample())
#' }
pitch.plot <- function(pitch, scaleIntensity = TRUE, showStrength = FALSE, group = "", pt = NULL) {
    pArray <- pitch.toArray(pitch)

    if (scaleIntensity) {
        intensityNorm <- normIntensity(pArray$intensityVector, 0.5, 6)
    } else {
        intensityNorm <- rep(1, pArray$nx)
    }

    pArray$frequencyArray[which(pArray$strengthArray == 0)] <- NA
    pArray$frequencyArray[which(pArray$frequencyArray > pArray$ceiling)] <- NA

    tAll <- pitch$t
    if (!is.null(pt)) {
        tAll <- c(tAll, pt$t)
    }

    tAll <- unique(sort(tAll))

    data <- list(t = tAll)

    intensity2 <- rep(as.numeric(0), length(tAll))
    intensity2[tAll %in% pitch$t] <- intensityNorm
    intensityNorm <- intensity2


    for (I in seqM(1, pitch$maxnCandidates)) {
        y2 <- rep(as.numeric(NA), length(tAll))
        y2[tAll %in% pitch$t] <- pArray$frequencyArray[I, ]

        data[[length(data)+1]] <- y2

        names(data)[length(data)] <- paste0("c", I)
    }

    if (!is.null(pt)) {
        y2 <- rep(as.numeric(NA), length(tAll))  ### pt
        y2[tAll %in% pt$t] <- pt$f
        data[[length(data)+1]] <- y2
        names(data)[length(data)] <- "PitchTier"
    }

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    # g <- dygraphs::dyOptions(g, drawPoints = TRUE, strokeWidth = 0)
    for (I in seqM(1, pitch$maxnCandidates)) {
        g <- dygraphs::dySeries(g, paste0("c", I), drawPoints = TRUE, strokeWidth = 0, color = "black")
    }

    if (!is.null(pt)) {
        g <- dygraphs::dySeries(g, "PitchTier", drawPoints = TRUE, pointSize = 3, strokeWidth = 0, color = "blue")
    }

    if (showStrength) {
        pArray$strengthArray[is.na(pArray$strengthArray)] <- 0 # or NaN, the value does not matter
        pArray$strengthArray <- round2(pArray$strengthArray*10)

        g <- dygraphs::dyCallbacks(g, "drawPointCallback" = sprintf(
            "
            function(g, name, ctx, canvasx, canvasy, color, radius, index) {
            var c_strength = %s;
            var nc = %d;
            var radius_str = %s;
            if (name != 'PitchTier') {
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
            paste0("[", paste0(pArray$strengthArray, collapse = ","), "]"),
            pitch$maxnCandidates,
            paste0("[", paste0(intensityNorm, collapse = ","), "]") ))
    } else {
        g <- dygraphs::dyCallbacks(g, "drawPointCallback" = sprintf(
            "
            function(g, name, ctx, canvasx, canvasy, color, radius, index) {
            var radius_str = %s;
            if (name != 'PitchTier') {
            radius = radius_str[index];
            }
            return Dygraph.Circles.DEFAULT(g, name, ctx, canvasx, canvasy, color, radius)
            }
            ",
            paste0("[", paste0(intensityNorm, collapse = ","), "]") ))
    }

    g <- dygraphs::dyRangeSelector(g, dateWindow = c(pitch$xmin, pitch$xmax), fillColor = "", strokeColor = "")

    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    g
}

#' as.pitch
#'
#' Renames the \code{class(pitch)["name"]} attribute and sets \code{class(pitch)["type"] <- "Pitch 1"} (if it is not already set)
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

