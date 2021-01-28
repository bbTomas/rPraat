#' snd.read
#'
#' Loads sound file (.wav or .mp3) using tuneR package.
#'
#' @param fileNameSound Sound file name (.wav or .mp3)
#' @param fileType \code{"wav"}, \code{"mp3"} or \code{"auto"}
#' @param from Where to start reading in \code{units} (beginning \code{"samples"}: 1, \code{"seconds"}: 0)
#' @param to Where to stop reading in \code{units} (\code{Inf} = end of the file)
#' @param units Units of \code{from} and \code{to} arguments: \code{"samples"} (starting from 1) or \code{"seconds"} (starting from 0)
#'
#' @return Sound object with normalized amplitude (PCM / 2^(nbits-1) - 1) resulting to the range of [-1; +1]. In fact, the minimum value can be one quantization step lower (e.g. PCM 16bit: -32768).
#' \code{t}   ... vector of discrete time instances (seconds)
#' \code{sig} ... signal matrix (\code{nrow(snd$sig)} = number of samples, \code{ncol(snd$sig)} = number of channels, i.e., \code{$sig[, 1]} ... 1st channel)
#' \code{fs}  ... sample rate (Hz)
#' \code{nChannels} ... number of signal channels (\code{ncol(snd$sig)}), 1 == mono, 2 == stereo
#' \code{nBits}     ... number of bits ped one sample
#' \code{nSamples}  ... number of samples (\code{nrow(snd$sig)})
#' \code{duration}  ... duration of signal (seconds), \code{snd$duration == snd$nSamples/snd$fs}
#'
#' @export
#' @seealso \code{\link{snd.write}}, \code{\link{snd.plot}}, \code{\link{snd.cut}}, \code{\link{snd.getPointIndexNearestTime}}
#'
#' @examples
#' \dontrun{
#' snd <- snd.read("demo/H.wav")
#' snd.plot(snd)
#' }
snd.read <- function(fileNameSound, fileType = "auto", from = 1, to = Inf, units = "samples") {
    if (!isString(fileNameSound)) {
        stop("Invalid 'fileNameSound' parameter.")
    }

    if (units != "samples"  &  units != "seconds") {
        stop(paste0("Unknown units: ", units))
    }

    if (fileType == "auto") {
        if (grepl("\\.mp3$", fileNameSound, ignore.case = TRUE)) {
            fileType <- "mp3"
        } else {
            fileType <- "wav"
        }
    }

    if (fileType == "wav") {
        snd <- tuneR::readWave(filename = fileNameSound, from = from, to = to, units = units)
    } else if (fileType == "mp3") {
        if (from != 1 | !is.infinite(to) | to < 0 | units != "samples") {
            stop(paste0("mp3 fileType, 'from', 'to' and 'units' arguments are unfortunately unsupported.", fileType))
        }
        snd <- tuneR::readMP3(filename = fileNameSound)
    } else {
        stop(paste0("Unsupported audio fileType: ", fileType))
    }

    nSamples <- length(snd@left)
    if (!snd@stereo) {
        sig <- matrix(snd@left / (2^(snd@bit-1) - 1), nrow = nSamples, ncol = 1)
    } else {
        sig <- matrix(c(snd@left / (2^(snd@bit-1) - 1), snd@right / (2^(snd@bit-1) - 1)), nrow = nSamples, ncol = 2)
    }

    tStart <- 0
    if (units == "samples") {
        tStart <- (from-1)/snd@samp.rate
    } else if (units == "seconds") {
        tStart <- from
    }

    s <- list(t = (seqM(0, nSamples-1)/snd@samp.rate + tStart),
              sig = sig,
              fs = snd@samp.rate,
              nChannels = ifelse(!snd@stereo, 1, 2), nBits = snd@bit, nSamples = nSamples, duration = nSamples/snd@samp.rate)

    class(s)["type"] <- "Sound"
    class(s)["name"] <- basename(fileNameSound)

    return(s)
}


#' snd.plot
#'
#' Plots interactive Sound object using dygraphs package. If the sound is 2-channel (stereo), the 1st channel is plotted around mean value +1, the 2nd around mean value -1.
#'
#' @param snd Sound object (with \code{$sig} and \code{$fs} members at least)
#' @param group [optional] character string, name of group for dygraphs synchronization
#' @param stemPlot [optional] discrete style of plot using
#'
#' @export
#' @seealso \code{\link{snd.read}}
#'
#' @examples
#' \dontrun{
#' snd <- snd.sample()
#' snd.plot(snd)
#'
#' snd.plot(list(sig = sin(seq(0, 2*pi, length.out = 4000)), fs = 8000))
#' }
snd.plot <- function(snd, group = "", stemPlot = FALSE) {
    snd <- as.snd(snd)

    if (snd$nChannels == 1) {
        if (is.null(nrow(snd$sig))) {
            data <- list(t = snd$t, ch1 = snd$sig)  # probably a vector
        } else {
            data <- list(t = snd$t, ch1 = snd$sig[, 1])
        }
    } else if (snd$nChannels == 2) {
        data <- list(t = snd$t, ch1 = snd$sig[, 1] + 1, ch2 = snd$sig[, 2] - 1)
    } else {
        stop("Only 1 or 2 channels are supported.")
    }

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    if (stemPlot) {
        g <- dygraphs::dyOptions(g, stemPlot = TRUE)
    }

    g <- dygraphs::dyRangeSelector(g, fillColor = "")

    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    g
}


#' snd.write
#'
#' Saves Sound object to a file.
#' snd is a list with \code{$sig} and \code{$fs} members at least. If \code{$nBits} is not present, default value of 16 bits is used. Vector \code{$t} is ignored. If the sound signal is 2-channel (stereo),
#' \code{$sig} must be a two-column matrix (1st column corresponds to the left channel, 2nd column to the right channel).
#' If the sound is 1-channel (mono), \code{$sig} can be either a numeric vector or a one-column matrix.
#' optional \code{$t}, \code{$nChannels}, \code{$nSamples}, \code{$duration} vectors are ignored.
#'
#' @param snd Sound object (with \code{$sig}, \code{$nBits} and \code{$fs} members)
#' @param fileNameSound file name to be created
#'
#' @export
#' @seealso \code{\link{snd.read}}
#'
#' @examples
#' \dontrun{
#' snd <- snd.sample()
#' snd.plot(snd)
#' snd.write(snd, "temp1.wav")
#'
#' signal <- 0.8*sin(seq(0, 2*pi*440, length.out = 8000))
#' snd.write(list(sig = signal, fs = 8000, nBits = 16), "temp2.wav")
#'
#' left  <- 0.3*sin(seq(0, 2*pi*440, length.out = 4000))
#' right <- 0.5*sin(seq(0, 2*pi*220, length.out = 4000))
#' snd.write(list(sig = matrix(c(left, right), ncol = 2), fs = 8000, nBits = 16), "temp3.wav")
#' }
snd.write <- function(snd, fileNameSound) {
    if (!isString(fileNameSound)) {
        stop("Invalid 'fileNameSound' parameter.")
    }

    if (!("nBits" %in% names(snd))) {
        snd$nBits <- 16  # default value
    }

    if (is.null(ncol(snd$sig))) {
        nch <- 1  # probably a vector
    } else {
        nch <- ncol(snd$sig)
    }

    if (nch == 1) {
        if (is.null(nrow(snd$sig))) {
            signal <- snd$sig  # probably a vector
        } else {
            signal <- snd$sig[, 1]
        }

        s <- tuneR::Wave(
            left = round(signal*(2^(snd$nBits - 1) - 1)),
            samp.rate = snd$fs, bit = snd$nBits)
    } else if (nch == 2) {
        s <- tuneR::Wave(
            left = round(snd$sig[, 1]*(2^(snd$nBits - 1) - 1)),
            right = round(snd$sig[, 2]*(2^(snd$nBits - 1) - 1)),
            samp.rate = snd$fs, bit = snd$nBits)
    } else {
        stop("Only 1 or 2 channels are supported.")
    }

    tuneR::writeWave(s, filename = fileNameSound)
}



#' snd.cut
#'
#' Cut the specified interval from the Sound object and preserve time
#'
#' @param snd Sound object (list with \code{$sig} and \code{$fs} members at least)
#' @param Start beginning sample/time of interval to be cut (default \code{-Inf} = cut from the beginning of the Sound)
#' @param End final sample/time of interval to be cut (default \code{Inf} = cut to the end of the Sound)
#' @param units Units of \code{Start} and \code{End} arguments: \code{"samples"} (starting from 1, i.e., 1 == index of the 1st sample) or \code{"seconds"} (starting from 0)
#'
#' @return Sound object
#' @export
#' @seealso \code{\link{snd.cut0}}, \code{\link{tg.cut}}, \code{\link{tg.cut0}}, \code{\link{snd.read}}, \code{\link{snd.plot}}
#'
#' @examples
#' snd <- snd.sample()
#' snd2 <-   snd.cut(snd,  Start = 0.3)
#' snd2_0 <- snd.cut0(snd, Start = 0.3)
#' snd3 <-   snd.cut(snd,  Start = 0.2, End = 0.3)
#' snd3_0 <- snd.cut0(snd, Start = 0.2, End = 0.3)
#' snd4 <-   snd.cut(snd,  End = 0.1)
#' snd4_0 <- snd.cut0(snd, End = 0.1)
#' snd5 <-   snd.cut(snd,  Start = -0.1, End = 0.1)
#' snd5_0 <- snd.cut0(snd, Start = -0.1, End = 0.1)
#' snd6 <-   snd.cut(snd,  End = 1000, units = "samples")
#' snd6_0 <- snd.cut0(snd, End = 1000, units = "samples")
#' \dontrun{
#' snd.plot(snd)
#' snd.plot(snd2)
#' snd.plot(snd2_0)
#' snd.plot(snd3)
#' snd.plot(snd3_0)
#' snd.plot(snd4)
#' snd.plot(snd4_0)
#' snd.plot(snd5)
#' snd.plot(snd5_0)
#' snd.plot(snd6)
#' snd.plot(snd6_0)
#' }
snd.cut <- function(snd, Start = -Inf, End = Inf, units = "seconds") {
    if (units != "samples"  &  units != "seconds") {
        stop(paste0("Unknown units: ", units))
    }

    if (!isNum(Start)) {
        stop("Start must be a number.")
    }
    if (!isNum(End)) {
        stop("End must be a number.")
    }
    if (is.infinite(Start) & Start>0) {
        stop("infinite tStart can be negative only")
    }
    if (is.infinite(End) & End<0) {
        stop("infinite tEnd can be positive only")
    }
    if (End < Start) {
        stop("End must be >= Start")
    }

    snd <- as.snd(snd)

    if (units == "samples") {
        if (is.infinite(Start)) {
            Start <- 1
        }
        if (is.infinite(End)) {
            End <- snd$nSamples
        }
        if (!isInt(Start)) {
            stop("Start must be an integer")
        }
        if (!isInt(End)) {
            stop("End must be an integer")
        }
        if (Start < 1) {
            stop("Start index must be >= 1")
        }
        if (End < Start) {
            stop("End index < Start index")
        }

        snd$t <- snd$t[seqM(Start, End)]

        if (is.null(nrow(snd$sig))) {  # vector
            snd$sig <- snd$sig[seqM(Start, End)]
        } else { # matrix
            snd$sig <- as.matrix(snd$sig[seqM(Start, End), ])
        }

        snd$nSamples <- length(snd$t)
        snd$duration <- snd$nSamples / snd$fs

    } else {  # "seconds"
        if (is.null(nrow(snd$sig))) {  # vector
            snd$sig <- snd$sig[snd$t >= Start  &  snd$t <= End]
        } else { # matrix
            snd$sig <- as.matrix(snd$sig[snd$t >= Start  &  snd$t <= End, ])
        }

        snd$t <- snd$t[snd$t >= Start  &  snd$t <= End]

        snd$nSamples <- length(snd$t)
        snd$duration <- snd$nSamples / snd$fs
    }

    return(snd)
}



#' snd.cut0
#'
#' Cut the specified interval from the Sound object and and shift time so that the new \code{snd$t[1]} = 0
#'
#' @param snd Sound object (list with \code{$sig} and \code{$fs} members at least)
#' @param Start beginning sample/time of interval to be cut (default \code{-Inf} = cut from the beginning of the Sound)
#' @param End final sample/time of interval to be cut (default \code{Inf} = cut to the end of the Sound)
#' @param units Units of \code{Start} and \code{End} arguments: \code{"samples"} (starting from 1, i.e., 1 == index of the 1st sample) or \code{"seconds"} (starting from 0)
#'
#' @return Sound object
#' @export
#' @seealso \code{\link{snd.cut}}, \code{\link{tg.cut}}, \code{\link{tg.cut0}}, \code{\link{snd.read}}, \code{\link{snd.plot}}
#'
#' @examples
#' snd <- snd.sample()
#' snd2 <-   snd.cut(snd,  Start = 0.3)
#' snd2_0 <- snd.cut0(snd, Start = 0.3)
#' snd3 <-   snd.cut(snd,  Start = 0.2, End = 0.3)
#' snd3_0 <- snd.cut0(snd, Start = 0.2, End = 0.3)
#' snd4 <-   snd.cut(snd,  End = 0.1)
#' snd4_0 <- snd.cut0(snd, End = 0.1)
#' snd5 <-   snd.cut(snd,  Start = -0.1, End = 0.1)
#' snd5_0 <- snd.cut0(snd, Start = -0.1, End = 0.1)
#' snd6 <-   snd.cut(snd,  End = 1000, units = "samples")
#' snd6_0 <- snd.cut0(snd, End = 1000, units = "samples")
#' \dontrun{
#' snd.plot(snd)
#' snd.plot(snd2)
#' snd.plot(snd2_0)
#' snd.plot(snd3)
#' snd.plot(snd3_0)
#' snd.plot(snd4)
#' snd.plot(snd4_0)
#' snd.plot(snd5)
#' snd.plot(snd5_0)
#' snd.plot(snd6)
#' snd.plot(snd6_0)
#' }
snd.cut0 <- function(snd, Start = -Inf, End = Inf, units = "seconds") {
    if (units != "samples"  &  units != "seconds") {
        stop(paste0("Unknown units: ", units))
    }

    if (!isNum(Start)) {
        stop("Start must be a number.")
    }
    if (!isNum(End)) {
        stop("End must be a number.")
    }
    if (is.infinite(Start) & Start>0) {
        stop("infinite tStart can be negative only")
    }
    if (is.infinite(End) & End<0) {
        stop("infinite tEnd can be positive only")
    }
    if (End < Start) {
        stop("End must be >= Start")
    }

    snd <- as.snd(snd)

    if (units == "samples") {
        if (is.infinite(Start)) {
            Start <- 1
        }
        if (is.infinite(End)) {
            End <- snd$nSamples
        }
        if (!isInt(Start)) {
            stop("Start must be an integer")
        }
        if (!isInt(End)) {
            stop("End must be an integer")
        }
        if (Start < 1) {
            stop("Start index must be >= 1")
        }
        if (End < Start) {
            stop("End index < Start index")
        }

        snd$t <- snd$t[seqM(Start, End)]

        if (is.null(nrow(snd$sig))) {  # vector
            snd$sig <- snd$sig[seqM(Start, End)]
        } else { # matrix
            snd$sig <- as.matrix(snd$sig[seqM(Start, End), ])
        }

        snd$nSamples <- length(snd$t)
        snd$duration <- snd$nSamples / snd$fs

    } else {  # "seconds"
        if (is.null(nrow(snd$sig))) {  # vector
            snd$sig <- snd$sig[snd$t >= Start  &  snd$t <= End]
        } else { # matrix
            snd$sig <- as.matrix(snd$sig[snd$t >= Start  &  snd$t <= End, ])
        }

        snd$t <- snd$t[snd$t >= Start  &  snd$t <= End]

        snd$nSamples <- length(snd$t)
        snd$duration <- snd$nSamples / snd$fs
    }

    snd$t <- snd$t - snd$t[1]

    return(snd)
}






#' as.snd
#'
#' Renames the \code{class(snd)["name"]} attribute and sets \code{class(snd)["type"] <- "Sound"} (if it is not already set)
#'
#' At least, \code{$sig} and \code{$fs} members must be present in \code{snd} list.
#'
#' If not present, it calculates \code{$t}, \code{$nChannels}, \code{$nBits} (default: 16), \code{$nSamples}, and \code{$duration} members of \code{snd} list
#'
#' @param snd snd object
#' @param name New name
#'
#' @return snd object
#' @export
#'
#' @examples
#' class(snd.sample())
#' class(as.snd(snd.sample(), name = "New Name"))
as.snd <- function(snd, name = "") {
    if (!("sig" %in% names(snd))) {
        stop("List 'snd' must contain $sig vector.")
    }

    if (!("fs" %in% names(snd))) {
        stop("List 'snd' must contain $fs vector.")
    }

    if (!("nSamples" %in% names(snd))) {
        if (is.null(nrow(snd$sig))) {
            nsamp <- length(snd$sig) # vector, not a matrix
        } else {
            nsamp <- nrow(snd$sig)   # matrix
        }
    } else {
        nsamp <- snd$nSamples
    }

    if (!("t" %in% names(snd))) {
        snd$t <- seqM(0, nsamp-1)/snd$fs
    }

    if (!("nChannels" %in% names(snd))) {
        if (is.null(ncol(snd$sig))) {
            snd$nChannels <- 1  # probably a vector
        } else {
            snd$nChannels <- ncol(snd$sig)
        }
    }

    if (!("nBits" %in% names(snd))) {
        snd$nBits <- 16  # default value, important for snd.write()
    }

    if (!("nSamples" %in% names(snd))) {
        snd$nSamples <- nsamp
    }

    if (!("duration" %in% names(snd))) {
        snd$duration <- snd$nSamples / snd$fs
    }

    class(snd)["type"] <- "Sound"
    class(snd)["name"] <- name

    return(snd)
}



#' snd.getPointIndexHigherThanTime
#'
#' Returns index of sample which is nearest the given time from right, i.e.
#' \code{time} <= sampleTime.
#'
#' @param snd Sound object
#' @param time time which is going to be found in samples
#'
#' @return integer
#' @export
#' @seealso \code{\link{snd.getPointIndexNearestTime}}, \code{\link{snd.getPointIndexLowerThanTime}}
#'
#' @examples
#' snd <- snd.sample()
#' snd.getPointIndexHigherThanTime(snd, 0.5)
snd.getPointIndexHigherThanTime <- function(snd, time) {
    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    snd <- as.snd(snd)

    ind <- NA

    npoints <- length(snd$t)
    for (I in seqM(1, npoints)) {
        if (time <= snd$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}




#' snd.getPointIndexLowerThanTime
#'
#' Returns index of sample which is nearest the given time from left, i.e.
#' sampleTime <= \code{time}.
#'
#' @param snd Sound object
#' @param time time which is going to be found in samples
#'
#' @return integer
#' @export
#' @seealso \code{\link{snd.getPointIndexNearestTime}}, \code{\link{snd.getPointIndexHigherThanTime}}
#'
#' @examples
#' snd <- snd.sample()
#' snd.getPointIndexLowerThanTime(snd, 0.5)
snd.getPointIndexLowerThanTime <- function(snd, time) {
    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    snd <- as.snd(snd)

    ind <- NA

    npoints <- length(snd$t)
    for (I in seqM(npoints, 1, by = -1)) {
        if (time >= snd$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}




#' snd.getPointIndexNearestTime
#'
#' Returns index of sample which is nearest the given \code{time} (from both sides).
#'
#' @param snd Sound object
#' @param time time which is going to be found in samples
#'
#' @return integer
#' @export
#' @seealso \code{\link{snd.getPointIndexLowerThanTime}}, \code{\link{snd.getPointIndexHigherThanTime}}
#'
#' @examples
#' snd <- snd.sample()
#' snd.getPointIndexNearestTime(snd, 0.5)
snd.getPointIndexNearestTime <- function(snd, time) {
    if (!isNum(time)) {
        stop("Time must be a number.")
    }

    snd <- as.snd(snd)

    ind <- NA

    npoints <- length(snd$t)
    minDist <- Inf
    minInd <- NA

    for (I in seqM(1, npoints)) {
        dist <- abs(snd$t[I] - time)
        if (dist < minDist) {
            minDist <- dist
            minInd <- I
        }
    }

    ind <- minInd


    return(ind)
}
