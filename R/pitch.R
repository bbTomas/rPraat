
#' pitch.read
#'
#' Reads Pitch object from Praat.
#' Supported formats: text file, short text file.
#'
#' @param fileNamePitch file name of Pitch object
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
#'
#' @return A Pitch object represents periodicity candidates as a function of time.
#' @return   [ref: Praat help, https://www.fon.hum.uva.nl/praat/manual/Pitch.html]
#' @return   \code{p$xmin} ... start time (seconds)
#' @return   \code{p$xmax} ... end time (seconds)
#' @return   \code{p$nx}   ... number of frames
#' @return   \code{p$dx}   ... time step = frame duration (seconds)
#' @return   \code{p$x1}   ... time associated with the first frame (seconds)
#' @return   \code{p$t}    ... vector of time instances associated with all frames
#' @return   \code{p$ceiling}        ... a frequency above which a candidate is considered voiceless (Hz)
#' @return   \code{p$maxnCandidates} ... maximum number of candidates in frame
#' @return   \code{p$frame[[1]]} to \code{p$frame[[p$nx]]} ... frames
#' @return      \code{p$frame[[1]]$intensity}   ... intensity of the frame
#' @return      \code{p$frame[[1]]$nCandidates} ... actual number of candidates in this frame
#' @return      \code{p$frame[[1]]$frequency} ... vector of candidates' frequency (in Hz)
#' @return                               (for a voiced candidate), or \code{0} (for an unvoiced candidate)
#' @return      \code{p$frame[[1]]$strength}  ... vector of degrees of periodicity of candidates (between \code{0} and \code{1})
#' @export
#' @seealso \code{\link{pitch.write}}, \code{\link{pitch.plot}}, \code{\link{pitch.cut}}, \code{\link{pitch.getPointIndexNearestTime}}, \code{\link{pt.read}}, \code{\link{tg.read}}, \code{\link{it.read}}, \code{\link{col.read}}
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
    enc <- encoding

    if (encoding == "auto") {
        enc <- detectEncoding(fileNamePitch)
    }

    if (enc == "UTF-8") {
        flines <- readr::read_lines(fileNamePitch, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
    } else {
        fid <- file(fileNamePitch, open = "r", encoding = enc)
        flines <- readLines(fid)   # does not work with tests/testthat/utf8.TextGrid  :-(
        close(fid)
    }

    flines <- enc2utf8(flines)

    if (length(flines) < 1) {
        stop("Empty file.")
    }

    if (encoding == "UTF-8" & flines[1] != 'File type = "ooTextFile"') {
        warning('Not an UTF-8 Pitch format, trying encoding = "auto"...')
        x <- pitch.read(fileNamePitch, encoding = "auto")
        return(x)
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

            if (!str_contains(flines[find-1+ 11], "frame []: ") & !str_contains(flines[find-1+ 11], "frames []: ")) {
                stop("Unknown Pitch format.")
            }

            iline <- find-1+ 12  # index of line to read

            for (I in seqM(1, nx)) {
                if (strTrim(flines[iline]) != paste0("frame [", I, "]:") & strTrim(flines[iline]) != paste0("frames [", I, "]:")) {
                    stop(paste0("Unknown Pitch format, wrong frame id (", I, "')."))
                }
                iline <- iline + 1

                intensity <- as.numeric(substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline])))); iline <- iline + 1
                nCandidates <- as.numeric(substr(strTrim(flines[iline]), 15, nchar(strTrim(flines[iline])))); iline <- iline + 1

                if (!str_contains(flines[iline], "candidate []:") & !str_contains(flines[iline], "candidates []:")) {
                    stop("Unknown Pitch format.")
                }
                iline <- iline + 1

                frequency <- numeric(nCandidates)
                strength <- numeric(nCandidates)

                for (Ic in seqM(1, nCandidates)) {
                    if (strTrim(flines[iline]) != paste0("candidate [", Ic, "]:") & strTrim(flines[iline]) != paste0("candidates [", Ic, "]:")) {
                        stop(paste0("Unknown Pitch format, wrong candidate nr. (", Ic, ") in frame id (", I, "')."))
                    }
                    iline <- iline + 1

                    nmbr <- substr(strTrim(flines[iline]), 13, nchar(strTrim(flines[iline]))); iline <- iline + 1
                    if (nmbr != "--undefined--") {
                        frequency[Ic] <- as.numeric(nmbr)
                    } else {
                        frequency[Ic] <- NA
                    }

                    nmbr <- substr(strTrim(flines[iline]), 12, nchar(strTrim(flines[iline]))); iline <- iline + 1
                    if (nmbr != "--undefined--") {
                        strength[Ic] <-  as.numeric(nmbr)
                    } else {
                        strength[Ic] <-  NA
                    }
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
                    nmbr <- flines[iline]; iline <- iline + 1
                    if (nmbr != "--undefined--") {
                        frequency[Ic] <- as.numeric(nmbr)
                    } else {
                        frequency[Ic] <- NA
                    }

                    nmbr <- flines[iline]; iline <- iline + 1
                    if (nmbr != "--undefined--") {
                        strength[Ic] <- as.numeric(nmbr)
                    } else {
                        strength[Ic] <- NA
                    }
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
    if (!("frame" %in% names(pitch))) {
        pArray <- pitch
    } else {
        pArray <- pitch.toArray(pitch)
    }


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


#' pitch.cut
#'
#' Cut the specified interval from the Pitch object and preserve time
#'
#' @param pitch Pitch object (either in Frame or Array format)
#' @param tStart beginning time of interval to be cut (default \code{-Inf} = cut from the \code{xmin} of the Pitch)
#' @param tEnd final time of interval to be cut (default \code{Inf} = cut to the \code{xmax} of the Pitch)
#'
#' @return Pitch object
#' @export
#' @seealso \code{\link{pitch.cut0}}, \code{\link{tg.cut}}, \code{\link{tg.cut0}}, \code{\link{pitch.read}}, \code{\link{pitch.plot}}
#'
#' @examples
#' pitch <- pitch.sample()
#' pitch2 <-   pitch.cut(pitch,  tStart = 3)
#' pitch2_0 <- pitch.cut0(pitch, tStart = 3)
#' pitch3 <-   pitch.cut(pitch,  tStart = 2, tEnd = 3)
#' pitch3_0 <- pitch.cut0(pitch, tStart = 2, tEnd = 3)
#' pitch4 <-   pitch.cut(pitch,  tEnd = 1)
#' pitch4_0 <- pitch.cut0(pitch, tEnd = 1)
#' pitch5 <-   pitch.cut(pitch,  tStart = -1, tEnd = 1)
#' pitch5_0 <- pitch.cut0(pitch, tStart = -1, tEnd = 1)
#' \dontrun{
#' pitch.plot(pitch)
#' pitch.plot(pitch2)
#' pitch.plot(pitch2_0)
#' pitch.plot(pitch3)
#' pitch.plot(pitch3_0)
#' pitch.plot(pitch4)
#' pitch.plot(pitch4_0)
#' pitch.plot(pitch5)
#' pitch.plot(pitch5_0)
#' }
pitch.cut <- function(pitch, tStart = -Inf, tEnd = Inf) {
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

    formatFrame <- TRUE
    if (!("frame" %in% names(pitch))) {
        formatFrame <- FALSE
        pitch <- pitch.toFrame(pitch)
        pitch2 <- pitch
    } else {
        pitch2 <- pitch
    }

    pitch2$t <- pitch$t[pitch$t >= tStart  &  pitch$t <= tEnd]
    pitch2$frame <- pitch$frame[pitch$t >= tStart  &  pitch$t <= tEnd]

    if (is.infinite(tStart)) {
        pitch2$xmin <- pitch$xmin
    } else {
        pitch2$xmin <- tStart
    }

    if (is.infinite(tEnd)) {
        pitch2$xmax <- pitch$xmax
    } else {
        pitch2$xmax <- tEnd
    }

    pitch2$nx <- length(pitch2$t)

    if (pitch2$nx >= 1) {
        pitch2$x1 <- pitch2$t[1]
    } else {
        pitch2$x1 <- pitch2$xmin
    }

    if (!formatFrame) {
        return(pitch.toArray(pitch2))
    } else {
        return(pitch2)
    }
}


#' pitch.cut0
#'
#' Cut the specified interval from the Pitch object and shift time so that the new \code{xmin} = 0
#'
#' @param pitch Pitch object (either in Frame or Array format)
#' @param tStart beginning time of interval to be cut (default \code{-Inf} = cut from the \code{xmin} of the Pitch)
#' @param tEnd final time of interval to be cut (default \code{Inf} = cut to the \code{xmax} of the Pitch)
#'
#' @return Pitch object
#' @export
#' @seealso \code{\link{pitch.cut}}, \code{\link{tg.cut}}, \code{\link{tg.cut0}}, \code{\link{pitch.read}}, \code{\link{pitch.plot}}
#'
#' @examples
#' pitch <- pitch.sample()
#' pitch2 <-   pitch.cut(pitch,  tStart = 3)
#' pitch2_0 <- pitch.cut0(pitch, tStart = 3)
#' pitch3 <-   pitch.cut(pitch,  tStart = 2, tEnd = 3)
#' pitch3_0 <- pitch.cut0(pitch, tStart = 2, tEnd = 3)
#' pitch4 <-   pitch.cut(pitch,  tEnd = 1)
#' pitch4_0 <- pitch.cut0(pitch, tEnd = 1)
#' pitch5 <-   pitch.cut(pitch,  tStart = -1, tEnd = 1)
#' pitch5_0 <- pitch.cut0(pitch, tStart = -1, tEnd = 1)
#' \dontrun{
#' pitch.plot(pitch)
#' pitch.plot(pitch2)
#' pitch.plot(pitch2_0)
#' pitch.plot(pitch3)
#' pitch.plot(pitch3_0)
#' pitch.plot(pitch4)
#' pitch.plot(pitch4_0)
#' pitch.plot(pitch5)
#' pitch.plot(pitch5_0)
#' }
pitch.cut0 <- function(pitch, tStart = -Inf, tEnd = Inf) {
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

    formatFrame <- TRUE
    if (!("frame" %in% names(pitch))) {
        formatFrame <- FALSE
        pitch <- pitch.toFrame(pitch)
        pitch2 <- pitch
    } else {
        pitch2 <- pitch
    }

    pitch2$t <- pitch$t[pitch$t >= tStart  &  pitch$t <= tEnd]
    pitch2$frame <- pitch$frame[pitch$t >= tStart  &  pitch$t <= tEnd]

    if (is.infinite(tStart)) {
        pitch2$xmin <- pitch$xmin
    } else {
        pitch2$xmin <- tStart
    }

    if (is.infinite(tEnd)) {
        pitch2$xmax <- pitch$xmax
    } else {
        pitch2$xmax <- tEnd
    }

    pitch2$t <- pitch2$t - pitch2$xmin
    pitch2$xmax <- pitch2$xmax - pitch2$xmin
    pitch2$xmin <- 0

    pitch2$nx <- length(pitch2$t)

    if (pitch2$nx >= 1) {
        pitch2$x1 <- pitch2$t[1]
    } else {
        pitch2$x1 <- pitch2$xmin
    }

    if (!formatFrame) {
        return(pitch.toArray(pitch2))
    } else {
        return(pitch2)
    }
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


#' pitch.write
#'
#' Saves Pitch to the file.
#'
#' @param pitch Pitch object
#' @param fileNamePitch Output file name
#' @param format Output file format (\code{"short"} (default, short text format) or \code{"text"} (a.k.a. full text format))
#'
#' @export
#' @seealso \code{\link{pitch.read}}, \code{\link{pt.read}}
#'
#' @examples
#' \dontrun{
#' pitch <- pitch.sample()
#' pitch.write(pitch, "demo_output.Pitch")
#' }
pitch.write <- function(pitch, fileNamePitch, format = "short") {
    pitch.write0(pitch, fileNamePitch, format)
}

pitch.write0 <- function(pitch, fileNamePitch, format = "short", fid = NULL, collection = FALSE) {
    if (!isString(fileNamePitch)) {
        stop("Invalid 'fileNamePitch' parameter.")
    }

    if (!isString(format)) {
        stop("Invalid 'format' parameter.")
    }
    if (format != "short" && format != "text") {
        stop("Unsupported format (supported: short [default], text)")
    }

    if (!("frame" %in% names(pitch))) {
        pitch <- pitch.toFrame(pitch)
    }

    if (!collection) {
        fid <- file(fileNamePitch, open = "wb", encoding = "UTF-8")
        if (!isOpen(fid)) {
            stop(paste0("cannot open file [", fileNamePitch, "]"))
        }
    }

    if (!collection) {
        wrLine('File type = "ooTextFile"', fid)
        wrLine('Object class = "Pitch 1"', fid)
        wrLine("", fid)
    }

    if (format == "short") {
        wrLine(as.character(round2(pitch$xmin, -15)), fid)
        wrLine(as.character(round2(pitch$xmax, -15)), fid)
        wrLine(as.character(pitch$nx), fid)
        wrLine(as.character(round2(pitch$dx, -15)), fid)
        wrLine(as.character(round2(pitch$x1, -15)), fid)
        wrLine(as.character(round2(pitch$ceiling, -15)), fid)
        wrLine(as.character(pitch$maxnCandidates), fid)
    } else if (format == "text") {
        wrLine(paste0("xmin = ", as.character(round2(pitch$xmin, -15)), " "), fid, collection)
        wrLine(paste0("xmax = ", as.character(round2(pitch$xmax, -15)), " "), fid, collection)
        wrLine(paste0("nx = ", pitch$nx, " "), fid, collection)
        wrLine(paste0("dx = ", as.character(round2(pitch$dx, -15)), " "), fid, collection)
        wrLine(paste0("x1 = ", as.character(round2(pitch$x1, -15)), " "), fid, collection)
        wrLine(paste0("ceiling = ", as.character(round2(pitch$ceiling, -15)), " "), fid, collection)
        wrLine(paste0("maxnCandidates = ", pitch$maxnCandidates, " "), fid, collection)
        wrLine("frame []: ", fid, collection)
    }

    for (N in seqM(1, pitch$nx)) {
        if (format == "text") {
            wrLine(paste0("    frame [", as.character(N), "]:"), fid, collection)
        }

        if (format == "short") {
            wrLine(as.character(round2(pitch$frame[[N]]$intensity, -15)), fid)
            wrLine(as.character(pitch$frame[[N]]$nCandidates), fid)
        } else if (format == "text") {
            wrLine(paste0("        intensity = ", as.character(round2(pitch$frame[[N]]$intensity, -15)), " "), fid, collection)
            wrLine(paste0("        nCandidates = ", pitch$frame[[N]]$nCandidate, " "), fid, collection)
            wrLine("        candidate []: ", fid, collection)
        }


        for (I in seqM(1, pitch$frame[[N]]$nCandidate)) {
            if (format == "short") {
                if (!is.na(pitch$frame[[N]]$frequency[I])) {
                    wrLine(as.character(round2(pitch$frame[[N]]$frequency[I], -15)), fid)
                } else {
                    wrLine("--undefined--", fid)
                }
                if (!is.na(pitch$frame[[N]]$strength[I])) {
                    wrLine(as.character(round2(pitch$frame[[N]]$strength[I], -15)), fid)
                } else {
                    wrLine("--undefined--", fid)
                }
            } else if (format == "text") {
                wrLine(paste0("            candidate [", as.character(I), "]:"), fid, collection)
                if (!is.na(pitch$frame[[N]]$frequency[I])) {
                    wrLine(paste0("                frequency = ", as.character(round2(pitch$frame[[N]]$frequency[I], -15)), " "), fid, collection)
                } else {
                    wrLine(paste0("                frequency = --undefined-- "), fid, collection)
                }
                if (!is.na(pitch$frame[[N]]$strength[I])) {
                    wrLine(paste0("                strength = ", as.character(round2(pitch$frame[[N]]$strength[I], -15)), " "), fid, collection)
                } else {
                    wrLine(paste0("                strength = --undefined-- "), fid, collection)
                }

            }
        }
    }

    if (!collection) {
        close(fid)
    }
}


#' pitch.getPointIndexHigherThanTime
#'
#' Returns index of frame which is nearest the given time from right, i.e.
#' \code{time} <= frameTime.
#'
#' @param pitch Pitch object
#' @param time time which is going to be found in frames
#'
#' @return integer
#' @export
#' @seealso \code{\link{pitch.getPointIndexNearestTime}}, \code{\link{pitch.getPointIndexLowerThanTime}}
#'
#' @examples
#' pitch <- pitch.sample()
#' pitch.getPointIndexHigherThanTime(pitch, 0.5)
pitch.getPointIndexHigherThanTime <- function(pitch, time) {
    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(pitch$t)
    for (I in seqM(1, npoints)) {
        if (time <= pitch$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}




#' pitch.getPointIndexLowerThanTime
#'
#' Returns index of frame which is nearest the given time from left, i.e.
#' frameTime <= \code{time}.
#'
#' @param pitch Pitch object
#' @param time time which is going to be found in frames
#'
#' @return integer
#' @export
#' @seealso \code{\link{pitch.getPointIndexNearestTime}}, \code{\link{pitch.getPointIndexHigherThanTime}}
#'
#' @examples
#' pitch <- pitch.sample()
#' pitch.getPointIndexLowerThanTime(pitch, 0.5)
pitch.getPointIndexLowerThanTime <- function(pitch, time) {
    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(pitch$t)
    for (I in seqM(npoints, 1, by = -1)) {
        if (time >= pitch$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}




#' pitch.getPointIndexNearestTime
#'
#' Returns index of frame which is nearest the given \code{time} (from both sides).
#'
#' @param pitch Pitch object
#' @param time time which is going to be found in frames
#'
#' @return integer
#' @export
#' @seealso \code{\link{pitch.getPointIndexLowerThanTime}}, \code{\link{pitch.getPointIndexHigherThanTime}}
#'
#' @examples
#' pitch <- pitch.sample()
#' pitch.getPointIndexNearestTime(pitch, 0.5)
pitch.getPointIndexNearestTime <- function(pitch, time) {
    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(pitch$t)
    minDist <- Inf
    minInd <- NA

    for (I in seqM(1, npoints)) {
        dist <- abs(pitch$t[I] - time)
        if (dist < minDist) {
            minDist <- dist
            minInd <- I
        }
    }

    ind <- minInd


    return(ind)
}

