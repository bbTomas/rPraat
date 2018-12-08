#' pt.read
#'
#' Reads PitchTier from Praat. Supported formats: text file, short text file,
#' spreadsheet, headerless spreadsheet (headerless not recommended,
#' it does not contain tmin and tmax info).
#'
#' @param fileNamePitchTier file name of PitchTier
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
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
#' Saves PitchTier to a file (in UTF-8 encoding).
#' \code{pt} is a list with \code{$t} and \code{$f} vectors (of the same length) at least.
#' If there are no \code{$tmin} and \code{$tmax} values, there are
#' set as min and max of \code{$t} vector.
#'
#' @param pt PitchTier object
#' @param fileNamePitchTier file name to be created
#' @param format Output file format (\code{"short"} (short text format), \code{"text"} (a.k.a. full text format), \code{"spreadsheet"} (default), \code{"headerless"} (not recommended, it does not contain \code{tmin} and \code{tmax} info))
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
    pt.write0(pt, fileNamePitchTier, format)
}

pt.write0 <- function(pt, fileNamePitchTier, format = "spreadsheet", fid = NULL, collection = FALSE) {
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


    if (!collection) {
        fid <- file(fileNamePitchTier, open = "wb", encoding = "UTF-8")
        if (!isOpen(fid)) {
            stop(paste0("cannot open file [", fileNamePitchTier, "]"))
        }
    }

    if (!collection) {
        if (format == "spreadsheet") {
            wrLine('"ooTextFile"', fid)
            wrLine('"PitchTier"', fid)
        } else if (format == "short" || format == "text") {
            wrLine('File type = "ooTextFile"', fid)
            wrLine('Object class = "PitchTier"', fid)
            wrLine('', fid)
        }
    }

    if (format == "spreadsheet") {
        wrLine(paste0(as.character(round2(xmin, -15)), " ", as.character(round2(xmax, -15)), " ", as.character(N)), fid)
    } else if (format == "short") {
        wrLine(as.character(round2(xmin, -15)), fid)
        wrLine(as.character(round2(xmax, -15)), fid)
        wrLine(as.character(N), fid)
    } else if (format == "text") {
        wrLine(paste0("xmin = ", as.character(round2(xmin, -15)), " "), fid, collection)
        wrLine(paste0("xmax = ", as.character(round2(xmax, -15)), " "), fid, collection)
        wrLine(paste0("points: size = ", as.character(N), " "), fid, collection)
    }

    for (n in seqM(1, N)) {
        if (format == "spreadsheet" || format == "headerless") {
            wrLine(paste0(as.character(round2(pt$t[n], -15)), "\t", as.character(round2(pt$f[n], -15))), fid)
        } else if (format == "short") {
            wrLine(as.character(round2(pt$t[n], -15)), fid)
            wrLine(as.character(round2(pt$f[n], -15)), fid)
        } else if (format == "text") {
            wrLine(paste0("points [", as.character(n), "]:"), fid, collection)
            wrLine(paste0("    number = ", as.character(round2(pt$t[n], -15)), " "), fid, collection)
            wrLine(paste0("    value = ", as.character(round2(pt$f[n], -15)), " "), fid, collection)
        }
    }

    if (!collection) {
        close(fid)
    }
}


#' pt.plot
#'
#' Plots interactive PitchTier using \code{dygraphs} package.
#'
#' @param pt PitchTier object
#' @param group [optional] character string, name of group for dygraphs synchronization
#'
#' @export
#' @seealso \code{\link{pt.read}}, \code{\link{pt.Hz2ST}}, \code{\link{pt.cut}}, \code{\link{pt.cut0}}, \code{\link{pt.interpolate}}, \code{\link{pt.write}}, \code{\link{tg.plot}}, \code{\link{pitch.plot}}, \code{\link{formant.plot}}
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
#'  a) If \code{t < min(pt$t)} (or \code{t > max(pt$t)}), returns the first (or the last) value of \code{pt$f}.
#'  b) If \code{t} is existing point in \code{pt$t}, returns the respective \code{pt$f}.
#'  c) If \code{t} is between two existing points, returns linear interpolation of these two points.
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
#' Interpolate the PitchTier in \code{npoints} equidistant points and approximate it by Legendre polynomials
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
#' Synthetize the contour from vector of Legendre polynomials \code{c} in \code{npoints} equidistant points
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
#' @param tStart beginning time of interval to be cut (default \code{-Inf} = cut from the \code{tmin} of the PitchTier)
#' @param tEnd final time of interval to be cut (default \code{Inf} = cut to the \code{tmax} of the PitchTier)
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
#' Cut the specified interval from the PitchTier and shift time so that the new \code{tmin} = 0
#'
#' @param pt PitchTier object
#' @param tStart beginning time of interval to be cut (default \code{-Inf} = cut from the \code{tmin} of the PitchTier)
#' @param tEnd final time of interval to be cut (default \code{Inf} = cut to the \code{tmax} of the PitchTier)
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

#' as.pt
#'
#' Renames the \code{class(pt)["name"]} attribute and sets \code{class(pt)["type"] <- "PitchTier"} (if it is not already set)
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

