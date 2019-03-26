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

    encodings <- c("UTF-8", "UTF-16", "UTF-16BE")   # "UTF-16LE" does not work anymore
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



# wrLine
#
# Write text line to a connection in binary mode.
#
# @param string Text line.
# @param fid A connection object.
# @param collectionFullText If TRUE, add extra 8 spaces at the beginning of the string
#
# @return a raw vector (if fid is a raw vector) or invisibly NULL.
wrLine <- function(string, fid, collectionFullText = FALSE) {
    if (collectionFullText) {
        writeBin(charToRaw("        "), fid, endian = "little")
    }
    writeBin(c(charToRaw(string), as.raw(c(13, 10))), fid, endian = "little")
}


normIntensity <- function(intensity, minValue = 1, maxValue = 9) {
    if (length(intensity) == 0) {
        return(intensity)
    }

    iZero <- intensity <= 0
    if (all(iZero)) {
        return(rep(NA, length(intensity)))
    }

    intensityNorm <- intensity
    intensityNorm[iZero] <- NaN
    intensityNorm[!iZero] <- log10(intensity[!iZero])
    iMin <- min(intensityNorm[!iZero])
    iMax <- max(intensityNorm[!iZero])

    if (iMin == iMax) {
        return(rep(maxValue, length(intensity)))
    }

    a <- (maxValue - minValue) / (iMax - iMin)
    b <- minValue - a*iMin

    return(a*intensityNorm + b)
}




#' strTrim
#'
#' Trim leading and trailing whitespace in character string.
#'
#' Like \code{str_trim()} in \code{stringr} package or \code{trimws()} in R3.2.0 but way faster.
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
#' If I create a for-loop, I would like to get an empty vector for \code{3:1} (I want a default step +1)
#' and also an empty vector for \code{seq(3, 1, by = 1)} (not an error). This is solved by this \code{seqM} function.
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
#' Returns \code{TRUE} / \code{FALSE} whether it is exactly 1 integer number (in fact, the class can be numeric but the number must be integer), non-missing
#'
#' @param num variable to be tested
#'
#' @return \code{TRUE} / \code{FALSE}
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
#' Returns \code{TRUE} / \code{FALSE} whether it is exactly 1 character string (character vector of length 1, non-missing)
#'
#' @param string variable to be tested
#'
#' @return \code{TRUE} / \code{FALSE}
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
#' Returns \code{TRUE} / \code{FALSE} whether it is exactly 1 number (numeric or integer vector of length 1, non-missing)
#'
#' @param num variable to be tested
#'
#' @return \code{TRUE} / \code{FALSE}
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
#' Returns \code{TRUE} / \code{FALSE} whether it is exactly 1 logical value, non-missing
#'
#' @param logical variable to be tested
#'
#' @return \code{TRUE} / \code{FALSE}
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
#' Find string in another string (without regular expressions), returns \code{TRUE} / \code{FALSE}.
#'
#' @param string string in which we try to find something
#' @param patternNoRegex string we want to find, "as it is" - no regular exprressions
#'
#' @return \code{TRUE} / \code{FALSE}
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
#' @return indices of all occurences (\code{1} = 1st character)
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
#' @return index of the first occurence only (\code{1} = 1st character)
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
#' This really is the inverse of the fft function, so \code{ifft(fft(x)) == x}.
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
