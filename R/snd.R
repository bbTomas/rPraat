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
#' @param snd Sound object (with \code{$sig} and \code{$fs} or \code{$t} members at least)
#' @param group [optional] character string, name of group for dygraphs synchronization
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
#' snd.plot(list(sig = 0.3*sin(seq(0, 2*pi, length.out = 4000)), t = 1:4000))
#' }
snd.plot <- function(snd, group = "") {
    if (is.null(nrow(snd$sig))) {
        nsamples <- length(snd$sig) # vector, not a matrix
    } else {
        nsamples <- nrow(snd$sig)   # matrix
    }


    if (!("nChannels" %in% names(snd))) {
        if (is.null(ncol(snd$sig))) {
            nch <- 1  # probably a vector
        } else {
            nch <- ncol(snd$sig)
        }
    } else {
        nch <- snd$nChannels
    }


    if (!("t" %in% names(snd))) {
        snd$t <- seqM(0, nsamples-1)/snd$fs
    }

    if (nch == 1) {
        if (is.null(nrow(snd$sig))) {
            data <- list(t = snd$t, ch1 = snd$sig)  # probably a vector
        } else {
            data <- list(t = snd$t, ch1 = snd$sig[, 1])
        }
    } else if (nch == 2) {
        data <- list(t = snd$t, ch1 = snd$sig[, 1] + 1, ch2 = snd$sig[, 2] - 1)
    } else {
        stop("Only 1 or 2 channels are supported.")
    }

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    g <- dygraphs::dyRangeSelector(g, fillColor = "")

    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    g
}


#' snd.write
#'
#' Saves Sound object to a file.
#' snd is a list with \code{$sig}, \code{$nBits} and \code{$fs} members at least. Vector \code{$t} is ignored. If the sound signal is 2-channel (stereo),
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
#' rigth <- 0.5*sin(seq(0, 2*pi*220, length.out = 4000))
#' snd.write(list(sig = matrix(c(left, right), ncol = 2), fs = 8000, nBits = 16), "temp3.wav")
#' }
snd.write <- function(snd, fileNameSound) {
    if (!isString(fileNameSound)) {
        stop("Invalid 'fileNameSound' parameter.")
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

#' as.snd
#'
#' Renames the \code{class(snd)["name"]} attribute and sets \code{class(snd)["type"] <- "Sound"} (if it is not already set)
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
    class(snd)["type"] <- "Sound"
    class(snd)["name"] <- name
    return(snd)
}

