#' it.read
#'
#' Reads IntensityTier from Praat. Supported formats: text file, short text file.
#'
#' @param fileNameIntensityTier file name of IntensityTier
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
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
#' it is list with at least \code{$t} and \code{$i} vectors (of the same length).
#' If there are no \code{$tmin} and \code{$tmax} values, there are
#' set as min and max of \code{$t} vector.
#'
#' @param it IntensityTier object
#' @param fileNameIntensityTier file name to be created
#' @param format Output file format (\code{"short"} (short text format - default), \code{"text"} (a.k.a. full text format))
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
    it.write0(it, fileNameIntensityTier, format)
}

it.write0 <- function(it, fileNameIntensityTier, format = "short", fid = NULL, collection = FALSE) {
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


    if (!collection) {
        fid <- file(fileNameIntensityTier, open = "wb", encoding = "UTF-8")
        if (!isOpen(fid)) {
            stop(paste0("cannot open file [", fileNameIntensityTier, "]"))
        }
    }

    if (!collection) {
        if (format == "short" || format == "text") {
            wrLine('File type = "ooTextFile"', fid)
            wrLine('Object class = "IntensityTier"', fid)
            wrLine('', fid)
        }
    }

    if (format == "short") {
        wrLine(as.character(round2(xmin, -15)), fid)
        wrLine(as.character(round2(xmax, -15)), fid)
        wrLine(as.character(N), fid)
    } else if (format == "text") {
        wrLine(paste0("xmin = ", as.character(round2(xmin, -15)), " "), fid, collection)
        wrLine(paste0("xmax = ", as.character(round2(xmax, -15)), " "), fid, collection)
        wrLine(paste0("points: size = ", as.character(N), " "), fid, collection)
    }

    for (n in seqM(1, N)) {
        if (format == "short") {
            wrLine(as.character(round2(it$t[n], -15)), fid)
            wrLine(as.character(round2(it$i[n], -15)), fid)
        } else if (format == "text") {
            wrLine(paste0("points [", as.character(n), "]:"), fid, collection)
            wrLine(paste0("    number = ", as.character(round2(it$t[n], -15)), " "), fid, collection)
            wrLine(paste0("    value = ", as.character(round2(it$i[n], -15)), " "), fid, collection)
        }
    }

    if (!collection) {
        close(fid)
    }
}


#' it.plot
#'
#' Plots interactive IntensityTier using \code{dygraphs} package.
#'
#' @param it IntensityTier object
#' @param group [optional] character string, name of group for dygraphs synchronization
#' @param snd [optional] Sound object
#'
#' @export
#' @seealso \code{\link{it.read}}, \code{\link{tg.plot}}, \code{\link{it.cut}}, \code{\link{it.cut0}}, \code{\link{it.interpolate}}, \code{\link{it.write}}
#'
#' @examples
#' \dontrun{
#' it <- it.sample()
#' it.plot(it)
#' }
it.plot <- function(it, group = "", snd = NULL) {
    tAll <- it$t

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
    y2 <- rep(as.numeric(NA), length(tAll))  ### it
    y2[tAll %in% it$t] <- it$i
    data[[length(data)+1]] <- y2
    names(data)[length(data)] <- "IntensityTier"

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
            ch1 <- snd$sig[, 1] + 1
            ch2 <-  snd$sig[, 2] - 1

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

    if (group != "") {  # dygraphs plot-synchronization group
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    if (!is.null(snd)) {
        if (snd$nChannels == 1) {
            g <- dygraphs::dyAxis(g, "y2", label = "Sound", independentTicks = TRUE, valueRange = c(-1, 1), drawGrid = FALSE)
            g <- dygraphs::dySeries(g, "Sound", axis = "y2")
        } else if (snd$nChannels == 2) {
            g <- dygraphs::dyAxis(g, "y2", label = "Sound", independentTicks = TRUE, valueRange = c(-2, 2), drawGrid = FALSE)
            g <- dygraphs::dySeries(g, "Sound1", axis = "y2")
            g <- dygraphs::dySeries(g, "Sound2", axis = "y2")
        }
    }

    g <- dygraphs::dySeries(g, "IntensityTier", drawPoints = TRUE, pointSize = 2, strokeWidth = 0, color = "green")
    g <- dygraphs::dyRangeSelector(g, dateWindow = c(it$tmin, it$tmax), fillColor = "")

    g <- dygraphs::dyAxis(g, "x", valueFormatter = "function(d){return d.toFixed(3)}")
    g
}


#' it.interpolate
#'
#' Interpolates IntensityTier contour in given time instances.
#'
#'  a) If \code{t < min(it$t}) (or \code{t > max(it$t)}), returns the first (or the last) value of \code{it$i}.
#'  b) If \code{t} is existing point in \code{it$t}, returns the respective \code{it$f}.
#'  c) If \code{t} is between two existing points, returns linear interpolation of these two points.
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
#' Interpolate the IntensityTier in \code{npoints} equidistant points and approximate it by Legendre polynomials
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


    lP <- npoints # number of points to interpolate the polynomial
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
        # coefficient ((I-1)*2+1) corresponds to power of components, and can also be calculated this way: mean((P.^2).')
    }

    return(c)
}

#' it.legendreSynth
#'
#' Synthetize the contour from vector of Legendre polynomials \code{c} in \code{npoints} equidistant points
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

    lP <- npoints # number of points to interpolate the polynomial
    nP <- length(c)

    B <- matrix(nrow = nP, ncol = lP)  # basis
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
#' @param tStart beginning time of interval to be cut (default \code{-Inf} = cut from the \code{tmin} of the IntensityTier)
#' @param tEnd final time of interval to be cut (default \code{Inf} = cut to the \code{tmax} of the IntensityTier)
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
#' Cut the specified interval from the IntensityTier and shift time so that the new \code{tmin} = 0
#'
#' @param it IntensityTier object
#' @param tStart beginning time of interval to be cut (default \code{-Inf} = cut from the \code{tmin} of the IntensityTier)
#' @param tEnd final time of interval to be cut (default \code{Inf} = cut to the \code{tmax} of the IntensityTier)
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

#' as.it
#'
#' Renames the \code{class(it)["name"]} attribute and sets \code{class(it)["type"] <- "IntensityTier"} (if it is not already set)
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

