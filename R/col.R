#' col.read
#'
#' Loads Collection from Praat in Text or Short text format.
#' Collection may contain combination of TextGrids, PitchTiers, Pitch objects, Formant objects, and IntensityTiers.
#'
#' @param fileName Input file name
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
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
