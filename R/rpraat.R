# devtools::install_github("bbTomas/tbTools")

library(dplyr)   # we need this for pipeline operator %>%

tgCheckTierInd <- function(tg, tierInd) {
    ntiers <- length(tg)

    if (is.numeric(tierInd) | is.integer(tierInd)) {      # tier index
        if ((length(tierInd) != 1)) {
            stop(paste0('tierInd must be one integer number [length(tierInd) = ', length(tierInd), ']'))
        }

        if (!tbTools::isInt(tierInd)) {
            stop(paste0('tierInd must be integer >= 1 [',  as.character(tierInd), ']'))
        }

        if (tierInd < 1 | tierInd > ntiers) {
            stop(paste0('tierInd out of range, tierInd = ', tierInd, ', ntiers = ', ntiers))
        }
    } else if (is.character(tierInd)) {       # tier name
        if ((length(tierInd) != 1)) {
            stop(paste0('tierInd must be one character string (tier name) [length(tierInd) = ', length(tierInd), ']'))
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

#################


## Nacte TextGrid z Praat ve formatu Short text file ci plnem Text file, UTF-8,
## jednotlive vrstvy mohou byt jak typu IntervalTier, tak PointTier.
##
## Zvlada i labely obsahujici vice radek ci uvozovky.
## v1.4, Tomas Boril, borilt@gmail.com
##     tg <- tg.read("demo/H.TextGrid")
##     tg.plot(tg)
tg.read <- function(fileNameTextGrid) {
    if (!tbTools::isString(fileNameTextGrid)) {
        stop("Invalid 'fileNameTextGrid' parameter.")
    }

    tg <- list()  # new textgrid

    fid <- file(fileNameTextGrid, open = "r", encoding = "UTF-8")
    flines <- readLines(fid)
    close(fid)
    find <- 4   # index nacitaneho radku, prvni 3 ignorujeme

    xminStr <- flines[find]; find <- find + 1 # xmin
    xmaxStr <- flines[find]; find <- find + 1; # xmax

    r <- flines[find]; find <- find + 1; # bud '<exists>' -> shorttext nebo 'tiers? <exists> ' -> plny format

    if (r == '<exists>') {
        shortFormat <- TRUE
    } else if (substr(r, 1, 6) == 'tiers?') {
        shortFormat <- FALSE
    } else {
        stop('Unknown textgrid format.')
    }

    if (shortFormat) {
        xmin <- as.numeric(xminStr) # xmin
        xmax <- as.numeric(xmaxStr) # xmax
    } else {
        xmin <- tidyr::extract_numeric(xminStr) # xmin
        xmax <- tidyr::extract_numeric(xmaxStr) # xmax
    }

    if (shortFormat) {
        pocetTiers <- as.numeric(flines[find])
        find <- find + 1
    } else {
        pocetTiers <- tidyr::extract_numeric(flines[find])
        find <- find + 1
    }

    for (tier in tbTools::seqM(1, pocetTiers)) {

        if (shortFormat) {
            typ <- flines[find]; find <- find + 1
        } else {
            r <- tbTools::strTrim(flines[find]); find <- find + 1

            while (substr(r, 1, 4) == 'item') {
                r <- tbTools::strTrim(flines[find]); find <- find + 1
            }

            if (substr(r, 1, 9) != 'class = "') {
                stop('Unknown textgrid format')
            }
            typ <- substr(r, 9, nchar(r))
        }

        if (typ == '"IntervalTier"') {  # IntervalTier
            r <- flines[find]; find <- find + 1  # jmeno
            if (shortFormat) {
                tierName <- substr(r, 2, nchar(r)-1)
            } else {
                r <- tbTools::strTrim(r);
                tierName <- substr(r, 9, nchar(r)-1)
            }
            tierType <- 'interval'

            find <- find + 2; # ignorujeme xmin a xmax

            if (shortFormat) {
                pocetIntervalu <- as.numeric(flines[find]); find <- find + 1
            } else {
                r <- tbTools::strTrim(flines[find]); find <- find + 1
                pocetIntervalu <- tidyr::extract_numeric(r)
            }

            tierT1 <- numeric(0)
            tierT2 <- numeric(0)
            tierLabel <- character(0)

            for (I in tbTools::seqM(1, pocetIntervalu)) {
                if (!shortFormat) {
                    r <- flines[find]; find <- find + 1 # ignorujeme radek intervals [..]:
                }

                if (shortFormat) {
                    t <- as.numeric(flines[find]); find <- find + 1
                    t2 <- as.numeric(flines[find]); find <- find + 1
                } else {
                    r1 <- tbTools::strTrim(flines[find]); find <- find + 1
                    r2 <- tbTools::strTrim(flines[find]); find <- find + 1
                    if ( (substr(r1, 1, 7) != 'xmin = ')  ||  (substr(r2, 1, 7) != 'xmax = ') ) {
                        stop('Unknown textgrid format');
                    }
                    t <- tidyr::extract_numeric(r1)
                    t2 <- tidyr::extract_numeric(r2)
                }

                r <- flines[find]; find <- find + 1;
                if (!shortFormat) {
                    if (!tbTools::str_contains(r, 'text = "')) {
                        stop('Unknown textgrid format');
                    }
                    rind <- tbTools::str_find1(r, '"')
                    pocetUvozovek <- length(tbTools::str_find(r, '"'))  # v Matlabu: sum(r == '"')
                    if ((pocetUvozovek %% 2) != 1) { # odstranit mezeru na konci, ktera je pouze v pripade, ze je sudy pocet uvozovek
                        r <- substr(r, rind, nchar(r)-1)
                    } else {
                        r <- substr(r, rind, nchar(r))
                    }
                }
                pocetUvozovek <- length(tbTools::str_find(r, '"'))
                label <- substr(r, 2, nchar(r))
                if ((pocetUvozovek %% 2) == 1) {
                    label <- paste0(label, '\n')

                    repeat {
                        r <- flines[find]; find <- find + 1
                        pocetUvozovek <- length(tbTools::str_find(r, '"'))
                        if (!shortFormat & (pocetUvozovek %% 2 == 1)) {# odstranit mezeru na konci, ktera pouze v pripade, ze je lichy pocet uvozovek
                            r <- substr(r, 1, nchar(r)-1)
                        }

                        if (pocetUvozovek %% 2 == 1  &  stringr::str_sub(r, -1) == '"') {
                            label <- paste0(label, substr(r, 1, nchar(r)-1), '"')
                            break
                        } else {
                            label <- paste0(label, r, '\n')
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
            r <- flines[find]; find <- find + 1  # jmeno
            if (shortFormat) {
                tierName <- substr(r, 2, nchar(r)-1)
            } else {
                r <- tbTools::strTrim(r)
                tierName <- substr(r, 9, nchar(r)-1)
            }
            tierType <- 'point'

            find <- find + 2 # ignorujeme xmin a xmax

            if (shortFormat) {
                pocetIntervalu <- as.numeric(flines[find]); find <- find + 1
            } else {
                r <- tbTools::strTrim(flines[find]); find <- find + 1
                pocetIntervalu <- tidyr::extract_numeric(r)
            }

            tierT <- numeric(0)
            tierLabel <- character(0)

            for (I in tbTools::seqM(1, pocetIntervalu)) {
                if (!shortFormat) {
                    r <- flines[find]; find <- find + 1 # ignorujeme radek points [..]:
                }

                if (shortFormat) {
                    t <- as.numeric(flines[find]); find <- find + 1
                } else {
                    r <- tbTools::strTrim(flines[find]); find <- find + 1
                    if (substr(r, 1, 9) != 'number = ') {
                        stop('Unknown textgrid format');
                    }
                    t <- tidyr::extract_numeric(r)
                }

                r <- flines[find]; find <- find + 1
                if (!shortFormat) {
                    if (!tbTools::str_contains(r, 'mark = "')) {
                        stop('Unknown textgrid format');
                    }
                    rind <- tbTools::str_find1(r, '"')
                    pocetUvozovek <- length(tbTools::str_find(r, '"'))
                    if (pocetUvozovek %% 2 != 1) { # odstranit mezeru na konci, ktera pouze v pripade, ze je sudy pocet uvozovek
                        r <- substr(r, rind, nchar(r)-1)
                    } else {
                        r <- substr(r, rind, nchar(r))
                    }
                }
                pocetUvozovek <- length(tbTools::str_find(r, '"'))
                label <- substr(r, 2, nchar(r))
                if (pocetUvozovek %% 2 == 1) {
                    label <- paste0(label, '\n')
                    repeat {
                        r <- flines[find]; find <- find + 1
                        pocetUvozovek <- length(tbTools::str_find(r, '"'))
                        if (!shortFormat & (pocetUvozovek %% 2 == 1)) { # odstranit mezeru na konci, ktera pouze v pripade, ze je lichy pocet uvozovek
                            r <- substr(r, 1, nchar(r)-1)
                        }

                        if ((pocetUvozovek %% 2 == 1) & (stringr::str_sub(r, -1) == '"')) {
                            label <- paste0(label, substr(r, 1, nchar(r)-1), '"')
                            break
                        } else {
                            label <- paste0(label, r, '\n')
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
            stop(paste0('Unsupported tier type [tierInd = ', length(tg)+1, ']'))
        }

    }


    class(tg)["tmin"] <- xmin
    class(tg)["tmax"] <- xmax

    return(tg)
}


#################

## Ulozi textgrid s libovolnym poctem tiers (intervalove i bodove).
## Pokud v tier neni specifikovan $type, je automaticky brana jako
## intervalova (kvuli zpetne kompatibilite). Jinak je doporucovano $type
## uvadet ('interval' nebo 'point').
## Pokud neobsahuje textgrid class(tg)["tmin"] a class(tg)["tmin"], jsou urceny automaticky jako
## nejkrajnejsi hodnoty ze vsech tiers.
## Uklada ve formatu Short text file, UTF-8.
## v1.5 Tomas Boril, borilt@gmail.com
##     tg <- readTextGrid("demo/H.TextGrid")
##     tg.write(tg, "demo/vystup.TextGrid")
tg.write <- function(tg, fileNameTextGrid) {
    if (!tbTools::isString(fileNameTextGrid)) {
        stop("Invalid 'fileNameTextGrid' parameter.")
    }

    nTiers <- length(tg)  # pocet Tiers

    minCasTotal <-  NaN
    maxCasTotal <-  NaN
    if ("tmin" %in% names(class(tg))  &  "tmax" %in% names(class(tg))) {
        minCasTotal <- as.numeric(class(tg)["tmin"])
        maxCasTotal <- as.numeric(class(tg)["tmax"])
    }

    for (I in tbTools::seqM(1, nTiers)) {
        if ("type" %in% names(tg[[I]])) {

            if (tg[[I]]$type == "interval") {
                typInt <- TRUE
            } else if (tg[[I]]$type == "point") {
                typInt <- FALSE
            } else {
                stop(paste0('Unknown tier type [', tg[[I]]$type, ']'))
            }
        } else {
            typInt <- TRUE
        }
        tg[[I]]$typInt <- typInt

        if (typInt == TRUE) {
            nInt <- length(tg[[I]]$t1) # pocet intervalu
            if (nInt > 0) {
                minCasTotal <- min(tg[[I]]$t1[1], minCasTotal)
                maxCasTotal <- max(tg[[I]]$t2[length(tg[[I]]$t2)], maxCasTotal)
            }
        } else {
            nInt <- length(tg[[I]]$t) # pocet intervalu
            if (nInt > 0) {
                minCasTotal <- min(tg[[I]]$t[1], minCasTotal)
                maxCasTotal <- max(tg[[I]]$t[length(tg[[I]]$t)], maxCasTotal)
            }
        }
    }

    fid <- file(fileNameTextGrid, open = "w", encoding = "UTF-8")
    if (!isOpen(fid)) {
        stop(paste0('cannot open file [', fileNameTextGrid, ']'))
    }

    writeLines('File type = "ooTextFile"', fid)
    writeLines('Object class = "TextGrid"', fid)
    writeLines('', fid)
    writeLines(as.character(tbTools::round2(minCasTotal, -10)), fid)  # nejmensi cas ze vsech vrstev
    writeLines(as.character(tbTools::round2(maxCasTotal, -10)), fid)  # nejvetsi cas ze vsech vrstev
    writeLines('<exists>', fid)
    writeLines(as.character(nTiers), fid)

    for (N in tbTools::seqM(1, nTiers)) {
        if (tg[[N]]$typInt == TRUE) {
            writeLines('"IntervalTier"', fid)
            writeLines(paste0('"', tg[[N]]$name, '"'), fid)

            nInt <- length(tg[[N]]$t1)  # pocet intervalu
            if (nInt > 0) {
                writeLines(as.character(tbTools::round2(tg[[N]]$t1[1], -10)), fid)  # pocatecni cas tier
                writeLines(as.character(tbTools::round2(tg[[N]]$t2[length(tg[[N]]$t2)], -10)), fid)  # finalni cas tier
                writeLines(as.character(nInt), fid)  # pocet intervalu textgrid

                for (I in tbTools::seqM(1, nInt)) {
                    writeLines(as.character(tbTools::round2(tg[[N]]$t1[I], -10)), fid)
                    writeLines(as.character(tbTools::round2(tg[[N]]$t2[I], -10)), fid)
                    writeLines(paste0('"', tg[[N]]$label[I], '"'), fid)
                }
            } else {   # vytvoreni jednoho prazdneho intervalu
                writeLines(as.character(tbTools::round2(minCasTotal, -10)), fid)  # pocatecni cas tier
                writeLines(as.character(tbTools::round2(maxCasTotal, -10)), fid)  # finalni cas tier
                writeLines("1", fid)  # pocet intervalu textgrid
                writeLines(as.character(tbTools::round2(minCasTotal, -10)), fid)
                writeLines(as.character(tbTools::round2(maxCasTotal, -10)), fid)
                writeLines('""', fid)
            }
        } else { # je to pointTier
            writeLines('"TextTier"', fid)
            writeLines(paste0('"', tg[[N]]$name, '"'), fid)

            nInt <- length(tg[[N]]$t)  # pocet intervalu
            if (nInt > 0) {
                writeLines(as.character(tbTools::round2(tg[[N]]$t[1], -10)), fid)  # pocatecni cas tier
                writeLines(as.character(tbTools::round2(tg[[N]]$t[length(tg[[N]]$t)], -10)), fid)  # finalni cas tier
                writeLines(as.character(nInt), fid)  # pocet intervalu textgrid

                for (I in tbTools::seqM(1, nInt)) {
                    writeLines(as.character(tbTools::round2(tg[[N]]$t[I], -10)), fid)
                    writeLines(paste0('"', tg[[N]]$label[I], '"'), fid)
                }
            } else { # prazdny pointtier
                writeLines(as.character(tbTools::round2(minCasTotal, -10)), fid)  # pocatecni cas tier
                writeLines(as.character(tbTools::round2(maxCasTotal, -10)), fid)  # finalni cas tier
                writeLines('0', fid)  # pocet intervalu textgrid
            }
        }

    }

    close(fid)
}

#############


## Zobrazi textgrid ve schematicke a interaktivni podobe pomoci knihovny dygraphs.
## Nepovinny parametr group (string character) slouzi k synchronizaci skupin pripadne
## vice grafu dygraphs.
## v1.01 Tomas Boril, borilt@gmail.com
##     tg <- tg.read("demo/H.TextGrid")
##     tg.plot(tg)
tg.plot <- function(tg, group = "") {
    ntiers <- length(tg)

    if (ntiers == 0) {
        dygraphs::dygraph(list(x = 0, y = NA), main = "Empty TextGrid")
    }

    if (length(names(tg)) != length(unique(names(tg)))) {
        stop("Sorry, tg.plot cannot display TextGrids with duplicated tier names.")
    }

    if (!tbTools::isString(group)) {
        stop("group must be a character string.")
    }

    tAll <- as.numeric(c(class(tg)["tmin"], class(tg)["tmax"]))

    # Nalezeni vsech casovych okamziku
    for (I in tbTools::seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            tAll <- c(tAll, tg[[I]]$t)

        } else if (tg[[I]]$type == "interval") {
            tAll <- c(tAll, tg[[I]]$t1, tg[[I]]$t1)

        } else {
            stop('Unknown tier type')
        }

    }
    tAll <- unique(sort(tAll))

    data <- list(t = tAll)

    # Vytvoreni jednotlivych rad
    for (I in tbTools::seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            y <- rep(as.numeric(NA), length(tAll))

            y[tAll %in% tg[[I]]$t] <- ntiers + 1 - I  # Vyska grafickeho bodu dle indexu tier
            data[[length(data)+1]] <- y
            names(data)[length(data)] <- tg[[I]]$name

        } else if (tg[[I]]$type == "interval") {
            y <- rep(as.numeric(NA), length(tAll))

            # y[tAll %in% unique(c(tg[[I]]$t1, tg[[I]]$t2))] <- ntiers + 1 - I  # Vyska grafickeho bodu dle indexu tier
            y <- rep(ntiers + 1 - I, length(tAll))
            data[[length(data)+1]] <- y
            names(data)[length(data)] <- tg[[I]]$name

        } else {
            stop('Unknown tier type')
        }

    }

    if (group != "") {  # pro synchronizaci s jinymi grafy dygraph, ktere maji nastavenu group na stejny nazev
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
        g <- dygraphs::dyRangeSelector(g)
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
        g <- dygraphs::dyRangeSelector(g)
    }

    # Pridani popisku
    for (I in tbTools::seqM(1, ntiers)) {

        if (tg[[I]]$type == "point") {
            for (J in tbTools::seqM(1, length(tg[[I]]$label))) {
                g <- dygraphs::dyAnnotation(g, tg[[I]]$t[J], text = tg[[I]]$label[J], width = 10*max(1, nchar(tg[[I]]$label[J])), height = 25, series = tg[[I]]$name, tooltip = tg[[I]]$label[J])
            }

        } else if (tg[[I]]$type == "interval") {
            for (J in tbTools::seqM(1, length(tg[[I]]$label))) {
                g <- dygraphs::dyAnnotation(g, tg[[I]]$t1[J], text = tg[[I]]$label[J], series = tg[[I]]$name, tooltip = tg[[I]]$label[J], width = 10*max(1, nchar(tg[[I]]$label[J])), height = 25, tickHeight = 10)
            }

        } else {
            stop('Unknown tier type')
        }

    }

    # Styly jednotlivych tiers
    for (I in tbTools::seqM(1, ntiers)) {
        if (tg[[I]]$type == "point") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, pointSize = 2, strokeWidth = 0)

        } else if (tg[[I]]$type == "interval") {
            g <- dygraphs::dySeries(g, tg[[I]]$name, pointSize = 2, strokeWidth = 1)

        } else {
            stop('Unknown tier type')
        }
    }
    g <- dygraphs::dyAxis(g, "y", valueRange = c(0, length(tg)+2))
    g <- dygraphs::dyAxis(g, "x", valueFormatter = 'function(d){return d.toFixed(3)}')
    g
}


#################


## Opravi problem s navaznosti t2 a t1 v intervalovych vrstvach, ktery vznikl diky chybnemu zaokrouhlovani
## napr. v automatickem segmentatoru Prague Labeller, diky cemu nebylo mozne tyto hranice v Praatu manualne presunovat.
##
## Parametrem verbose = TRUE lze vypnout vypis problemovych mist.
## v1.0, Tomas Boril, borilt@gmail.com
##     tgProblem <- tg.read("demo/H_problem.TextGrid")
##     tgNew <- tg.repairContinuity(tgProblem)
##     tg.write(tgNew, "demo/H_problem_OK.TextGrid")
tg.repairContinuity <- function(tg, verbose = FALSE) {
    for (I in tbTools::seqM(1, length(tg))) {
        if (tg[[I]]$type == "interval") {
            for (J in tbTools::seqM(1, length(tg[[I]]$label)-1)) {
                if (tg[[I]]$t2[J] != tg[[I]]$t1[J+1]) {
                    newVal <- mean(c(tg[[I]]$t2[J], tg[[I]]$t1[J+1]))
                    if (!verbose) {
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


#################

## Vytvori novy zcela prazdny textgrid. Parametry tMin a tMax
## nastavi tmin a tmax, ktere jsou napr. pouzivany, kdyz se prida nova
## vrstva IntervaTier bez udaneho rozsahu.
## Tento prazdny textgrid je samostatne nepouzitelny, je potreba do nej
## pridat alespon jednu vrstvu pomoci tg.insertNewIntervalTier() nebo
## tg.insertNewPointTier().
## v1.0, Tomas Boril, borilt@gmail.com
tg.createNewTextGrid <- function(tMin, tMax) {
    if (!tbTools::isNum(tMin)) {
        stop("tMin must be a number")
    }

    if (!tbTools::isNum(tMax)) {
        stop("tMin must be a number")
    }

    if (tMin > tMax) {
        stop(paste0("Cannot be: tMin > tMax [", as.character(tMin), " > ", as.character(tMax), "]"))
    }

    tgNew <- list()
    class(tgNew)["tmin"] <- tMin
    class(tgNew)["tmax"] <- tMax

    return(tgNew)
}


#################

## Vrati TRUE/FALSE, zda tier je typu IntervalTier
## v1.0, Tomas Boril, borilt@gmail.com
##
## tierInd ... index vrstvy (tier)
tg.isIntervalTier <- function(tg, tierInd) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (tg[[tierInd]]$type == 'interval') {
        b <- TRUE
    } else {
        b <- FALSE
    }


    return(b)
}

#################

## Vrati TRUE/FALSE, zda tier je typu PointTier
## v1.0, Tomas Boril, borilt@gmail.com
##
## tierInd ... index vrstvy (tier)
tg.isPointTier <- function(tg, tierInd) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (tg[[tierInd]]$type == 'point') {
        b <- TRUE
    } else {
        b <- FALSE
    }


    return(b)
}


#################

## Vrati jmeno vrstvy (tier).
## v1.0, Tomas Boril, borilt@gmail.com
tg.getTierName <- function(tg, tierInd) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    n <- tg[[tierInd]]$name

    return(n)
}



#################

## Nastavi (zmeni) jmeno vrstvy (tier) s danym indexem.
## v1.0, Tomas Boril, borilt@gmail.com
tg.setTierName <- function(tg, tierInd, name) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isString(name)) {
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


#################


## Vrati pocet labelu v dane vrstve (tier), ktere se rovnaji pozadovanemu retezci.
## v1.0, Tomas Boril, borilt@gmail.com
tg.countLabels <- function(tg, tierInd, label) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isString(label)) {
        stop("label must be a character string")
    }

    c <- 0  # nalezeny pocet

    for (I in tbTools::seqM(1, length(tg[[tierInd]]$label))) {
        if (tg[[tierInd]]$label[I] == label) {
            c <- c + 1
        }
    }

    return(c)
}


#################

## Duplikuje vrstvu (tier) textgridu s danym indexem (1 = prvni) na pozici noveho indexu.
## Puvodni vrstvy od pozice newInd vyse posune o jednu dal.
## Doporucujeme nastavit nove vrstve jine jmeno parametrem newTierName, i kdyz to neni nutne,
## protoze dve vrstvy se mohou v Praatu jmenovat stejne. Komplikuje se tim ale moznost indexovani vrstev jmenem.
## v1.0, Tomas Boril, borilt@gmail.com
tg.duplicateTier <- function(tg, originalInd, newInd, newTierName = "") {
    originalInd <- tgCheckTierInd(tg, originalInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(newInd)) {
        stop("newInd must be integer >= 1")
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!tbTools::isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    tgNew <- tg

    tOrig <- tg[[originalInd]]

    for (I in tbTools::seqM(ntiers+1, newInd+1, by = -1)) {
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


#################

## Vrati pocatecni cas. Bud minimum vsech vrstev (default)
## ci konkretni vrstvy - tier (v takovem pripade vraci NA, kdyz vrsta nic
## neobsahuje).
## v1.0, Tomas Boril, borilt@gmail.com
tg.getStartTime <- function(tg, tierInd = 0) {
    if (tbTools::isInt(tierInd) & tierInd == 0) {
        t <- as.numeric(class(tg)["tmin"])
        return(t)
    }

    tierInd <- tgCheckTierInd(tg, tierInd)
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
        stop(paste0('Unknown tier type [tierInd = ', tierInd, ']'))
    }

    return(t)
}


#################

## Vrati konecny cas. Bud maximum vsech vrstev (default)
## ci konkretni vrstvy - tier (v takovem pripade vraci NA, kdyz vrsta nic
## neobsahuje).
## v1.0, Tomas Boril, borilt@gmail.com
tg.getEndTime <- function(tg, tierInd = 0) {
    if (tbTools::isInt(tierInd) & tierInd == 0) {
        t <- as.numeric(class(tg)["tmax"])
        return(t)
    }

    tierInd <- tgCheckTierInd(tg, tierInd)
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
        stop(paste0('Unknown tier type [tierInd = ', tierInd, ']'))
    }

    return(t)
}




#################

## Vrati celkove trvani. Bud maximum vsech vrstev (default)
## ci konkretni vrstvy - tier (v takovem pripade vraci NA, kdyz vrsta nic
## neobsahuje).
## v1.0, Tomas Boril, borilt@gmail.com
tg.getTotalDuration <- function(tg, tierInd = 0) {
    if (tbTools::isInt(tierInd) & tierInd == 0) {
        t <- tg.getEndTime(tg) - tg.getStartTime(tg)
        return(t)
    }

    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    t <- tg.getEndTime(tg, tierInd) - tg.getStartTime(tg, tierInd)
    return(t)
}


#################

## Vrati celkovy pocet vrstev (tiers) textgridu.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getNumberOfTiers <- function(tg) {
    ntiers <- length(tg)

    return(ntiers)
}


#################

## Vrati pocet bodu v dane vrstve (tier) typu PointTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getNumberOfPoints <- function(tg, tierInd) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not PointTier.'))
    }

    c <- length(tg[[tierInd]]$t)

    return(c)
}


#################

## Vrati pocet intervalu v dane vrstve (tier) typu IntervalTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getNumberOfIntervals <- function(tg, tierInd) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not IntervalTier.'))
    }

    c <- length(tg[[tierInd]]$t1)

    return(c)
}


#################

## Vrati label intervalu ci bodu s danym indexem ve vybrane vrstve (tier) typu IntervalTier ci PointTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getLabel <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }


    if (tg.isIntervalTier(tg, tierInd)) {
        nint <- tg.getNumberOfIntervals(tg, tierInd)
        if (index < 1 | index > nint) {
            stop(paste0('Index out of range [index = ', index, ', nint = ', nint))
        }
    } else if (tg.isPointTier(tg, tierInd)) {
        npoints <- tg.getNumberOfPoints(tg, tierInd)
        if (index < 1 | index > npoints) {
            stop(paste0('Index out of range [index = ', index, ', npoints = ', npoints))
        }
    } else {
        stop('Unknown tier type')
    }

    lab <- tg[[tierInd]]$label[index]

    return(lab)
}


#################
## Zmeni label intervalu ci bodu s danym indexem ve vybrane vrstve (tier) typu IntervalTier ci PointTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.setLabel <- function(tg, tierInd, index, newLabel) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tbTools::isString(newLabel)) {
        stop("newLabel must be a character string")
    }

    if (tg.isIntervalTier(tg, tierInd)) {
        nint <- tg.getNumberOfIntervals(tg, tierInd)
        if (index < 1 | index > nint) {
            stop(paste0('Index out of range [index = ', index, ', nint = ', nint))
        }
    } else if (tg.isPointTier(tg, tierInd)) {
        npoints <- tg.getNumberOfPoints(tg, tierInd)
        if (index < 1 | index > npoints) {
            stop(paste0('Index out of range [index = ', index, ', npoints = ', npoints))
        }
    } else {
        stop('Unknown tier type')
    }

    tgNew <- tg
    tgNew[[tierInd]]$label[index] <- newLabel

    return(tgNew)
}






#################

## Vrati cas zacatku intervalu s danym indexem ve vybrane vrstve (tier) typu IntervalTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getIntervalStartTime <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('Tier ', tierInd, ' is not IntervalTier.'))
    }


    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index > nint) {
        stop(paste0('Index out of range [index = ', index, ', nint = ', nint, ']'))
    }

    t <- tg[[tierInd]]$t1[index]

    return(t)
}


#################

## Vrati cas konce intervalu s danym indexem ve vybrane vrstve (tier) typu IntervalTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getIntervalEndTime <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('Tier ', tierInd, ' is not IntervalTier.'))
    }


    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index > nint) {
        stop(paste0('Index out of range [index = ', index, ', nint = ', nint, ']'))
    }

    t <- tg[[tierInd]]$t2[index]

    return(t)
}


#################

## Vrati trvani intervalu s danym indexem ve vybrane vrstve (tier) typu IntervalTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getIntervalDuration <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('Tier ', tierInd, ' is not IntervalTier.'))
    }


    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index > nint) {
        stop(paste0('Index out of range [index = ', index, ', nint = ', nint, ']'))
    }

    t <- tg[[tierInd]]$t2[index] - tg[[tierInd]]$t1[index]

    return(t)
}


#################

## Vrati cas bodu s danym indexem ve vybrane vrstve (tier) typu PointTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getPointTime <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tbTools::isInt(index)) {
        stop("index must be integer >= 1")
    }

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0('Tier ', tierInd, ' is not PointTier.'))
    }

    npoints <- tg.getNumberOfPoints(tg, tierInd)
    if (index < 1 | index > npoints) {
        stop(paste0('Index out of range [index = ', index, ', npoints = ', npoints, ']'))
    }

    t <- tg[[tierInd]]$t[index]


    return(t)
}


#################

## Odstrani vrstvu (tier) textgridu s danym indexem (1 = prvni).
## v1.0, Tomas Boril, borilt@gmail.com
tg.removeTier <- function(tg, tierInd) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    tgNew <- tg

    for (I in tbTools::seqM(tierInd, ntiers - 1)) {
        tgNew[[I]] <- tgNew[[I+1]]
        names(tgNew)[I] <- names(tgNew)[I+1]
    }

    tgNew[[ntiers]] <- NULL


    return(tgNew)
}


#################

## Vytvori novou vrstvu (tier) textgridu typu PointTier a vlozi ji na dany index (1 = prvni).
## Nasledujici vrstvy posune o jednu dal.
## Je treba zadat jmeno nove vrstvy - retezec newTierName.
## v1.0, Tomas Boril, borilt@gmail.com
tg.insertNewPointTier <- function(tg, newInd, newTierName) {
    ntiers <- length(tg)

    if (!tbTools::isInt(newInd)) {
        stop("newInd must be integer >= 1")
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!tbTools::isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    tgNew <- tg

    tNew <- list(name = newTierName, type = 'point', t = numeric(0), label = character(0))

    for (I in tbTools::seqM(ntiers+1, newInd+1, by = -1)) {
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


#################

## Vytvori novou vrstvu (tier) textgridu typu IntervalTier a vlozi ji na dany index (1 = prvni).
## Nasledujici vrstvy posune o jednu dal.
## Je treba zadat jmeno nove vrstvy - retezec newTierName.
## Po vzoru Praatu prazdna intervalova tier obsahuje jeden interval
## s prazdnym labelem pres cely casovy rozsah xmin az xmax textgridu,
## jedine tak je totiz mozne v Praatu tento interval "delit" na mensi, a tim vlastne
## vkladat nove intervaly.
## Rozsah tmin az tmax je mozne zadat nepovinnymi parametry tMin a tMax.
## v1.0, Tomas Boril, borilt@gmail.com
tg.insertNewIntervalTier <- function(tg, newInd, newTierName, tMin=NA, tMax=NA) {
    ntiers <- length(tg)

    if (!tbTools::isInt(newInd)) {
        stop("newInd must be integer >= 1")
    }

    if (newInd < 1  |  newInd > ntiers+1) {
        stop(paste0("newInd out of range <1; ntiers+1> [newInd = ", newInd, ", ntiers = ", ntiers, "]"))
    }

    if (!tbTools::isString(newTierName)) {
        stop("newTierName must be a character string")
    }

    if (class(tMin) != "logical" & !tbTools::isNum(tMin)) {
        stop("tMin must be a number")
    }
    if (class(tMin) == "logical" & length(tMin) != 1) {
        stop("tMin must be a number")
    }
    if (!tbTools::isNum(tMin) & !is.na(tMin)) {
        stop("tMin must be a number")
    }

    if (class(tMax) != "logical" & !tbTools::isNum(tMax)) {
        stop("tMax must be a number")
    }
    if (class(tMax) == "logical" & length(tMax) != 1) {
        stop("tMax must be a number")
    }
    if (!tbTools::isNum(tMax) & !is.na(tMax)) {
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



    tNew <- list(name = newTierName, type = 'interval', t1 = tMin, t2 = tMax, label = "")

    for (I in tbTools::seqM(ntiers+1, newInd+1, by = -1)) {
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



#################

## Vrati index intervalu obsahujici dany cas, vybrana vrstva (tier) musi byt typu IntervalTier.
## Interval musi splnovat tmin <= time < tmax. Pokud nenalezne, vrati NaN.
## Pro opravu nenavazujicich intervalu je mozne pouzit funkci tg.repairContinuity()
## v1.0, Tomas Boril, borilt@gmail.com
tg.getIntervalIndexAtTime <- function(tg, tierInd, time) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not IntervalTier.'))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    nint <- length(tg[[tierInd]]$t1)
    for (I in tbTools::seqM(1, nint)) {
        if (tg[[tierInd]]$t1[I] <= time  &  time < tg[[tierInd]]$t2[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}


#################

## Vrati index bodu, ktery je nejblize zprava danemu casu (vcetne), vybrana vrstva (tier) musi byt typu PointTier.
## Pokud nenalezne, vrati NA.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getPointIndexHigherThanTime <- function(tg, tierInd, time) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not PointTier.'))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    for (I in tbTools::seqM(1, npoints)) {
        if (time <= tg[[tierInd]]$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}



#################

## Vrati index bodu, ktery je nejblize zleva danemu casu (vcetne), vybrana vrstva (tier) musi byt typu PointTier.
## Pokud nenalezne, vrati NA.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getPointIndexLowerThanTime <- function(tg, tierInd, time) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not PointTier.'))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    for (I in tbTools::seqM(npoints, 1, by = -1)) {
        if (time >= tg[[tierInd]]$t[I]) {
            ind <- I
            break
        }
    }


    return(ind)
}



#################

## Vrati index bodu, ktery je nejblize danemu casu (z obou smeru), vybrana vrstva (tier) musi byt typu PointTier.
## Pokud nenalezne, vrati NA.
## v1.0, Tomas Boril, borilt@gmail.com
tg.getPointIndexNearestTime <- function(tg, tierInd, time) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not PointTier.'))
    }

    if (!tbTools::isNum(time)) {
        stop("Time must be a number.")
    }

    ind <- NA

    npoints <- length(tg[[tierInd]]$t)
    minDist <- Inf
    minInd <- NA

    for (I in tbTools::seqM(1, npoints)) {
        dist <- abs(tg[[tierInd]]$t[I] - time)
        if (dist < minDist) {
            minDist <- dist
            minInd <- I
        }
    }

    ind <- minInd


    return(ind)
}


#################

## Odstrani bod s danym indexem z PointTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.removePoint <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not PointTier.'))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    npoints <- length(tg[[tierInd]]$t)

    if (index < 1 | index>npoints) {
        stop(paste0('index out of range [index = ', index, ', npoints = ', npoints, '].'))
    }


    tgNew <- tg
    for (I in tbTools::seqM(index, npoints - 1)) {
        tgNew[[tierInd]]$t[I] <- tgNew[[tierInd]]$t[I+1]
        tgNew[[tierInd]]$label[I] <- tgNew[[tierInd]]$label[I+1]
    }

    tgNew[[tierInd]]$t <- tgNew[[tierInd]]$t[-length(tgNew[[tierInd]]$t)]
    tgNew[[tierInd]]$label <- tgNew[[tierInd]]$label[-length(tgNew[[tierInd]]$label)]

    return(tgNew)
}


#################

## Vlozi novy bod do PointTier.
## v1.0, Tomas Boril, borilt@gmail.com
tg.insertPoint <- function(tg, tierInd, time, label) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isPointTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not PointTier.'))
    }

    if (!tbTools::isNum(time)) {
        stop("time must be a number.")
    }

    if (!tbTools::isString(label)) {
        stop("label must be a character string.")
    }


    tgNew <- tg

    indPosun <- tg.getPointIndexHigherThanTime(tg, tierInd, time)
    npoints <- length(tg[[tierInd]]$t)

    if (!is.na(indPosun)) {
        for (I in tbTools::seqM(npoints, indPosun, by = -1)) {
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


#################

## Odstrani levou hranici intervalu s danym indexem z vrstvy (tier) tierInd typu IntervalTier.
## Slouci se tim dva intervaly do jednoho (spoji se i labely). Nelze pouzit
## pro prvni interval, protoze to je pocatecni hranice vrstvy.
## Napr. mam intervaly 1-2-3, dam odstranit levou hranici 2. intervalu.
## Vysledkem budou dva intervaly 12-3. Pokud mi vadi slouceni labelu, mohu
## label jeste pred odstranovanim hranice nastavit na prazdny retezec.
## v1.0, Tomas Boril, borilt@gmail.com
tg.removeIntervalLeftBoundary <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not IntervalTier.'))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index>nint) {
        stop(paste0('index out of range [index = ', index, ', nint = ', nint, '].'))
    }

    if (index == 1) {
        stop('Cannot remove left boundary of the first interval.')
    }

    t1 <- tg[[tierInd]]$t1[index-1]
    t2 <- tg[[tierInd]]$t2[index]
    lab <- paste0(tg[[tierInd]]$label[index-1], tg[[tierInd]]$label[index])

    tgNew <- tg
    for (I in tbTools::seqM(index, nint - 1)) {
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


#################

## Odstrani pravou hranici intervalu s danym indexem z vrstvy (tier) tierInd typu IntervalTier.
## Slouci se tim dva intervaly do jednoho (spoji se i labely). Nelze pouzit
## pro posledni interval, protoze to je konecna hranice vrstvy.
## Napr. mam intervaly 1-2-3, dam odstranit pravou hranici 2. intervalu.
## Vysledkem budou dva intervaly 1-23. Pokud mi vadi slouceni labelu, mohu
## label jeste pred odstranovanim hranice nastavit na prazdny retezec.
## v1.0, Tomas Boril, borilt@gmail.com
tg.removeIntervalRightBoundary <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not IntervalTier.'))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index>nint) {
        stop(paste0('index out of range [index = ', index, ', nint = ', nint, '].'))
    }

    if (index == nint) {
        stop('Cannot remove right boundary of the last interval.')
    }

    t1 <- tg[[tierInd]]$t1[index]
    t2 <- tg[[tierInd]]$t2[index+1]
    lab <- paste0(tg[[tierInd]]$label[index], tg[[tierInd]]$label[index+1])

    tgNew <- tg
    for (I in tbTools::seqM(index, nint - 1)) {
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


#################

## Odstrani levou i pravou hranici intervalu s danym indexem z vrstvy (tier) tierInd typu IntervalTier.
## Slouci se tim tri intervaly do jednoho (spoji se i labely). Nelze pouzit
## pro prvni a posledni interval, protoze to je konecna hranice vrstvy.
## Napr. mam intervaly 1-2-3, dam odstranit obe hranice 2. intervalu.
## Vysledkem bude jeden interval 123. Pokud mi vadi slouceni labelu (chtel
## jsem "odstranit interval vcetne labelu"), mohu
## label jeste pred odstranovanim hranice nastavit na prazdny retezec.
## Pokud chci jen "odstranit interval bez slucovani", tedy obdrzet 1-nic-3,
## nejedna se o odstranovani hranic. Staci pouze nastavit label 2. intervalu
## na prazdny retezec "".
## v1.0, Tomas Boril, borilt@gmail.com
tg.removeIntervalBothBoundaries <- function(tg, tierInd, index) {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not IntervalTier.'))
    }

    if (!tbTools::isInt(index)) {
        stop("index must be an integer.")
    }

    nint <- tg.getNumberOfIntervals(tg, tierInd)
    if (index < 1 | index>nint) {
        stop(paste0('index out of range [index = ', index, ', nint = ', nint, '].'))
    }


    if (index == 1) {
        stop('Cannot remove left boundary of the first interval.')
    }
    if (index == nint) {
        stop('Cannot remove right boundary of the last interval.')
    }

    t1 <- tg[[tierInd]]$t1[index-1]
    t2 <- tg[[tierInd]]$t2[index+1]
    lab <- paste0(tg[[tierInd]]$label[index-1], tg[[tierInd]]$label[index], tg[[tierInd]]$label[index+1])

    tgNew <- tg
    for (I in tbTools::seqM(index, nint - 2)) {
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


#################

## Vlozi novou hranici do IntervalTier, cimz vzdy vznikne novy interval,
## kteremu je prirazen label (nepovinny parametr) ci zustane s prazdnym
## labelem.
## Mozne jsou ruzne situace umisteni nove hranice:
## a) Do jiz existujiciho intervalu:
##    Interval se novou hranici rozdeli na dve casti. Leva si zachova
##    label puvodniho intervalu, prave je nastaven nepovinny novy label.
##
## b) Vlevo od existujicich intervalu:
##    Novy interval zacina zadanou hranici a konci v miste zacatku prvniho
##    jiz drive existujiciho intervalu. Noveme intervalu je nastaven
##    nepovinny novy label.
##
## c) Vpravo od existujicich intervalu:
##    Novy interval zacina v miste konce posledniho jiz existujiciho
##    intervalu a konci zadanou novou hranici. Tomuto novem intervalu je
##    nastaven nepovinny novy label. Situace je tak tedy ponekud odlisna od
##    situaci a) a b), kde novy label byl nastavovan vzdy intervalu, ktery
##    lezel napravo od nove hranice. V situaci c) lezi label naopak nalevo
##    od hranice. Ale je to jedina logicka moznost ve smyslu pridavani
##    novych intervalu za konec jiz existujicich.
##
## Situace, kdy by se vkladala hranice mezi existujici intervaly na pozici,
## kde jeste zadny interval neni, neni z hlediska logiky Praatu mozna.
## Neni totiz pripustne, aby existoval jeden interval, pak nic, a pak dalsi interval.
## Nic mezi intervaly Praat dusledne znaci jako interval s prazdnym labelem.
## Nova vrstva IntervalTier vzdy obsahuje prazdny interval
## pres celou dobu trvani. Tento interval je mozne hranicemi delit na
## podintervaly ci rozsirovat na obe strany. Mezery bez intervalu tak
## nemohou vzniknout. Pokud by presto byly pritomne, je nutne nejdrive
## tento problem napravit funkci tg.repairContinuity().
##
## v1.0, Tomas Boril, borilt@gmail.com
tg.insertBoundary <- function(tg, tierInd, time, label="") {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not IntervalTier.'))
    }

    if (!tbTools::isNum(time)) {
        stop("time must be a number.")
    }

    if (!tbTools::isString(label)) {
        stop("label must be a character string.")
    }

    tgNew <- tg

    index <- tg.getIntervalIndexAtTime(tg, tierInd, time)
    nint <- tg.getNumberOfIntervals(tg, tierInd);

    if (nint == 0) {
        stop('Nonsense: tier [', tierInd, '] has 0 intervals.')
    }

    if (is.na(index)) {
        if (time > tg[[tierInd]]$t2[length(tg[[tierInd]]$t2)]) {  # pripad c) vpravo od existujicich intervalu
            tgNew[[tierInd]]$t1[nint+1] <- tg[[tierInd]]$t2[nint]
            tgNew[[tierInd]]$t2[nint+1] <- time
            tgNew[[tierInd]]$label[nint+1] <- label
            class(tgNew)["tmax"] <- max(c(as.numeric(class(tg)["tmax"]), time), na.rm = TRUE)
        } else if (time < tg[[tierInd]]$t1[1]) { # pripad b) vlevo od existujicich intervalu
            for (I in tbTools::seqM(nint, 1, by = -1)) {
                tgNew[[tierInd]]$t1[I+1] <- tgNew[[tierInd]]$t1[I]
                tgNew[[tierInd]]$t2[I+1] <- tgNew[[tierInd]]$t2[I]
                tgNew[[tierInd]]$label[I+1] <- tgNew[[tierInd]]$label[I]
            }
            tgNew[[tierInd]]$t1[1] <- time
            tgNew[[tierInd]]$t2[1] <- tgNew[[tierInd]]$t1[2]
            tgNew[[tierInd]]$label[1] <- label
            class(tgNew)["tmin"] <- min(c(as.numeric(class(tg)["tmin"]), time), na.rm = TRUE)
        } else if (time == tg[[tierInd]]$t2[length(tg[[tierInd]]$t2)]) {  # pokus o nesmyslne vlozeni hranice presne na konec tier
            stop(paste0('Cannot insert boundary because one already exists at the same position [tierInd = ', tierInd, ', time = ', time, '].'))
        } else {
            stop('Nonsense: missing interval, even though time is between intervals. Please check continuity using tg.repairContinuity().')
        }
    } else { # pripad a) do jiz existujiciho intervalu
        for (I in tbTools::seqM(1, nint)) {
            if ((time %in% tgNew[[tierInd]]$t1) | (time %in% tgNew[[tierInd]]$t2)) {
                stop(paste0('Cannot insert boundary because one already exists at the same position [tierInd = ', tierInd, ', time = ', time, '].'))
            }
        }

        for (I in tbTools::seqM(nint, index+1, by = -1)) {
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


#################

## Vlozi novy interval do prazdneho mista v IntervalTier, tedy
## a) Do jiz existujiciho intervalu (musi mit prazdny label):
##    Nejcastejsi pripad, protoze samotna nova vrstva IntervalTier je cela
##    jeden prazdny interval od zacatku do konce.
## b) Mimo existujici intervaly nalevo ci napravo, vznikla mezera bude
##    zaplnena prazdnym intervalem.
## Intervalu tStart az tEnd je prirazen label (nepovinny parametr) ci zustane
## s prazdnym labelem.
##
## Tato funkce je ve vetsine pripadu totez, jako 1. tgInsertBoundary(tEnd)
## a 2. tgInsertBoundary(tStart, novy label). Ale navic je provadena kontrola,
## a) zda tStart a tEnd nalezeji do stejneho puvodniho prazdneho intervalu,
## b) nebo jsou oba mimo existujici intervaly nalevo ci napravo.
##
## Pruniky noveho intervalu s vice jiz existujicimi i prazdnymi intervaly
## nedavaji smysl a jsou zakazany.
##
## Je treba si uvedomit, ze ve skutecnosti tato funkce casto
## vytvari vice intervalu. Napr. mame zcela novou IntervalTier s jednim prazdnym
## intervalem 0 az 5 sec. Vlozime interval 1 az 2 sec s labelem 'rekl'.
## Vysledkem jsou tri intervaly: 0-1 '', 1-2 'rekl', 2-5 ''.
## Pak znovu vlozime touto funkci interval 7 az 8 sec s labelem 'ji',
## vysledkem bude pet intervalu: 0-1 '', 1-2 'rekl', 2-5 '', 5-7 '' (vklada se
## jako vypln, protoze jsme mimo rozsah puvodni vrstvy), 7-8 'ji'.
## Pokud vsak nyni vlozime interval presne 2 az 3 'to', prida se ve
## skutecnosti jen jeden interval, kde se vytvori prava hranice intervalu a
## leva se jen napoji na jiz existujici, vysledkem bude sest intervalu:
## 0-1 '', 1-2 'rekl', 2-3 'to', 3-5 '', 5-7 '', 7-8 'ji'.
## Muze take nastat situace, kdy nevytvori zadny novy interval, napr. kdyz
## do predchoziho vlozime interval 3 az 5 'asi'. Tim se pouze puvodne prazdnemu
## intervalu 3-5 nastavi label na 'asi', vysledkem bude opet jen sest intervalu:
## 0-1 '', 1-2 'rekl', 2-3 'to', 3-5 'asi', 5-7 '', 7-8 'ji'.
##
## Tato funkce v Praatu neni, zde je navic a je vhodna pro situace,
## kdy chceme napr. do prazdne IntervalTier pridat nekolik oddelenych intervalu
## (napr. intervaly detekovane recove aktivity).
## Naopak neni zcela vhodna pro pridavani na sebe primo napojenych
## intervalu (napr. postupne segmentujeme slovo na jednotlive navazujici
## hlasky), protoze kdyz napr. vlozime intervaly 1 az 2.1 a 2.1 az 3,
## kde obe hodnoty 2.1 byly vypocteny samostatne a diky zaokrouhlovacim chybam
## se zcela presne nerovnaji, ve skutecnosti tim vznikne bud jeste prazdny
## interval 'priblizne' 2.1 az 2.1, coz nechceme, a nebo naopak funkce skonci
## s chybou, ze tStart je vetsi nez tEnd, pokud zaokrouhleni dopadlo opacne.
## Pokud vsak hranice byla spoctena jen jednou a ulozena do promenne, ktera
## byla pouzita jako konecna hranice predchazejiciho intervalu, a zaroven jako
## pocatecni hranice noveho intervalu, nemel by byt problem a novy interval
## se vytvori jako napojeni bez vlozeneho 'mikrointervalu'.
## Kazdopadne, bezpecnejsi pro takove ucely je zpusob, jak se postupuje
## v Praatu, tedy vlozit hranici se  zacatkem prvni hlasky pomoci
## tgInsertBoundary(cas, labelHlasky), pak stejne casy zacatku a labely vsech
## nasledujicich hlasek, a nakonec vlozit jeste konecnou hranici posledni hlasky
## (tedy jiz bez labelu) pomoci tgInsertBoundary(cas).
##
## v1.0, Tomas Boril, borilt@gmail.com
tg.insertInterval <- function(tg, tierInd, tStart, tEnd, label="") {
    tierInd <- tgCheckTierInd(tg, tierInd)
    ntiers <- length(tg)

    if (!tg.isIntervalTier(tg, tierInd)) {
        stop(paste0('tier ', tierInd, ' is not IntervalTier.'))
    }

    if (!tbTools::isNum(tStart)) {
        stop("tStart must be a number.")
    }
    if (!tbTools::isNum(tEnd)) {
        stop("tEnd must be a number.")
    }
    if (tStart >= tEnd) {
        stop(paste0('tStart [', as.character(tStart), '] must be lower than tEnd [', as.character(tEnd), '].'))
    }
    # pozn. diky teto podmince nemohou nastat nektere situace podchycene nize
    # (tStart == tEnd), leccos se tim zjednodusuje a Praat stejne nedovoluje
    # mit dve hranice ve stejnem case, takze je to alespon kompatibilni.

    if (!tbTools::isString(label)) {
        stop("label must be a character string.")
    }

    # tgNew <- tg   # zakomentovano i v originale v Matlabu

    nint <- length(tg[[tierInd]]$t1)
    if (nint == 0) {
        # Zvlastni situace, tier nema ani jeden interval.
        tgNew <- tg
        tgNew[[tierInd]]$t1 <- tStart
        tgNew[[tierInd]]$t2 <- tEnd
        tgNew[[tierInd]]$label <- label
        class(tgNew)["tmin"] <- min(c(as.numeric(class(tgNew)["tmin"]), tStart), na.rm = TRUE)
        class(tgNew)["tmax"] <- max(c(as.numeric(class(tgNew)["tmax"]), tEnd), na.rm = TRUE)
        return(tgNew)
    }

    tgNalevo <- tg[[tierInd]]$t1[1]
    tgNapravo <- tg[[tierInd]]$t2[length(tg[[tierInd]]$t2)]

    if (tStart < tgNalevo & tEnd < tgNalevo) {
        # cat('vkladam uplne nalevo + prazdny interval jako vypln\n')
        tgNew <- tg.insertBoundary(tg, tierInd, tEnd)
        tgNew <- tg.insertBoundary(tgNew, tierInd, tStart, label)
        return(tgNew)
    } else if (tStart <= tgNalevo & tEnd == tgNalevo) {
        # cat('vkladam uplne nalevo, plynule navazuji\n')
        tgNew <- tg.insertBoundary(tg, tierInd, tStart, label)
        return(tgNew)
    } else if (tStart < tgNalevo & tEnd > tgNalevo) {
        stop(paste0('Intersection of new interval (', as.character(tStart), ' to ', as.character(tEnd), ' sec, "', label, '") and already existing intervals (region before "beginning" and also the first interval) is forbidden.'))
    } else if (tStart > tgNapravo & tEnd > tgNapravo) {
        # cat('vkladam uplne napravo + prazdny interval jako vypln\n')
        tgNew <- tg.insertBoundary(tg, tierInd, tEnd)
        tgNew <- tg.insertBoundary(tgNew, tierInd, tStart, label)
        return(tgNew)
    } else if (tStart == tgNapravo & tEnd >= tgNapravo) {
        # cat('vkladam uplne napravo, plynule navazuji\n')
        tgNew <- tg.insertBoundary(tg, tierInd, tEnd, label)
        return(tgNew)
    } else if (tStart < tgNapravo & tEnd > tgNapravo) {
        stop(paste0('Intersection of new interval (', as.character(tStart), ' to ', as.character(tEnd), ' sec, "', label, '") and already existing intervals (the last interval and also the region after "end") is forbidden.'))
    } else if (tStart >= tgNalevo & tEnd <= tgNapravo) {
        # cat('vkladani nekam do jiz existujici oblasti, nutna kontrola stejneho a prazdneho intervalu\n')
        # nalezeni vsech intervalu, kam casy spadaji - pokud se trefime na hranici, muze totiz cas nalezet dvema intervalum
        iStart <- integer(0)
        iEnd <- integer(0)
        for (I in tbTools::seqM(1, nint)) {
            if (dplyr::between(tStart, tg[[tierInd]]$t1[I], tg[[tierInd]]$t2[I])) {  # tStart >= tg[[tierInd]]$t1[I] & tStart <= tg[[tierInd]]$t2[I]
                iStart <- c(iStart, I)
            }
            if (dplyr::between(tEnd, tg[[tierInd]]$t1[I], tg[[tierInd]]$t2[I])) {  # tEnd >= tg[[tierInd]]$t1[I] & tEnd <= tg[[tierInd]]$t2[I]
                iEnd <- c(iEnd, I)
            }
        }

        if (!(length(iStart) == 1 & length(iEnd) == 1)) {
            prunik <- intersect(iStart, iEnd) # nalezeni spolecneho intervalu z vice moznych variant
            if (length(prunik) == 0) {
                # je to chyba, ale ta bude zachycena dale podminkou 'if (iStart == iEnd)'
                iStart <- iStart[length(iStart)]
                iEnd <- iEnd[1]
            } else {
                iStart <- prunik[1]
                iEnd <- prunik[1]
                if (length(prunik) > 1) { # pokus o nalezeni prvniho vhodneho kandidata
                    for (I in tbTools::seqM(1, length(prunik))) {
                        if (tg[[tierInd]]$label[prunik[I]] == "") {
                            iStart <- prunik[I]
                            iEnd <- prunik[I]
                            break
                        }
                    }
                }
            }
        }

        if (iStart == iEnd) {
            if (tg[[tierInd]]$label[iStart] == "") {
                # cat('vkladam dovnitr intervalu, otazka, zda napojit ci ne\n')
                t1 <- tg[[tierInd]]$t1[iStart]
                t2 <- tg[[tierInd]]$t2[iStart]
                if (tStart == t1 & tEnd == t2) {
                    # cat('jenom nastavim jiz existujicimu prazdnemu intervalu label\n');
                    tgNew <- tg
                    tgNew[[tierInd]]$label[iStart] <- label
                    return(tgNew)
                # } else if (tStart == t1 & tEnd == t1) {
                #    cat('puvodnimu intervalu nastavim label a vlozim jednu hranici do t1, tim vznikne novy nulovy interval na zacatku s novym labelem a cely puvodni interval bude stale prazdny\n')
                # } else if (tStart == t2 & tEnd == t2) {
                #    cat('vlozim jednu hranici do t2 s novym labelem, tim zustane puvodni cely prazdny interval a vznikne novy nulovy interval na konci s novym labelem\n')
                } else if (tStart == t1 & tEnd < t2) {
                    # cat('puvodnimu intervalu nastavim label a vlozim jednu hranici do tEnd, tim se puvodni interval rozdeli na dve casti, prvni bude mit novy label, druha zustane prazdna\n')
                    tgNew <- tg
                    tgNew[[tierInd]]$label[iStart] <- label
                    tgNew <- tg.insertBoundary(tgNew, tierInd, tEnd)
                    return(tgNew)
                } else if (tStart > t1 & tEnd == t2) {
                    # cat('vlozim jednu hranici do tStart s novym labelem, tim se puvodni interval rozdeli na dve casti, prvni zustane prazdna a druha bude mit novy label\n')
                    tgNew <- tg.insertBoundary(tg, tierInd, tStart, label)
                    return(tgNew)
                } else if (tStart > t1 & tEnd < t2) {
                    # cat('vlozim hranici do tEnd s prazdnym labelem, a pak vlozim hranici do tStart s novym labelem, tim se puvodni interval rozdeli na tri casti, prvni a posledni budou prazdne, prostredni bude mit novy label\n')
                    tgNew <- tg.insertBoundary(tg, tierInd, tEnd)
                    tgNew <- tg.insertBoundary(tgNew, tierInd, tStart, label)
                } else {
                    stop("Error in author's logic. This cannot happen. Please, contact the author but be kind. He is really unhappy about this confusion.")
                }
            } else {
                stop(paste0('Insertion of new interval (', as.character(tStart), ' to ', as.character(tEnd), ' sec, "', label, '") into the interval with unempty label (', as.character(tg[[tierInd]]$t1[iStart]), ' to ', as.character(tg[[tierInd]]$t2[iStart]), ' sec, "', tg[[tierInd]]$label[iStart], '") is forbidden.'))
            }
        } else {
            stop(paste0('Intersection of new interval (', as.character(tStart), ' to ', as.character(tEnd), ' sec, "', label, '") and more already existing (indexes ', iStart, ' and ', iEnd, ') is forbidden.'))
        }



    } else {
        stop("Error in author's logic. This cannot happen. Please, contact the author but be kind. He is really unhappy about this confusion.")
    }

    return(tgNew)
}




#################

## Nacte PitchTier z Praat ve formatu spreadSheet,
##
## v0.1, Tomas Boril, borilt@gmail.com
##     pt <- pt.read("demo/maminka_spreadSheet.PitchTier")
pt.read <- function(fileNamePitchTier) {
    if (!tbTools::isString(fileNamePitchTier)) {
        stop("Invalid 'fileNamePitchTier' parameter.")
    }

    fid <- file(fileNamePitchTier, open = "r", encoding = "UTF-8")
    flines <- readLines(fid)
    close(fid)

    if (length(flines) < 1) {
        stop("Empty file.")
    }

    if (flines[1] == "\"ooTextFile\"") {    # spreadSheet
        if (length(flines) < 3) {
            stop("Unknown PitchTier format.")
        }

        if (flines[2] != "\"PitchTier\"") {
            stop("Unknown PitchTier format.")
        }

        fromToN <- stringr::str_split(flines[3], " ")
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

        for (I in tbTools::seqM(1, N, by = 1)) {
            tf <- stringr::str_split(flines[I+3], "\\s")
            if (length(tf[[1]]) != 2) {
                stop("Unknown PitchTier format.")
            }
            t[I] <- as.numeric(tf[[1]][[1]])
            f[I] <- as.numeric(tf[[1]][[2]])
        }


    } else if (flines[1] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile
        if (length(flines) < 6) {
            stop("Unknown PitchTier format.")
        }

        if (flines[2] != "Object class = \"PitchTier\"") {
            stop("Unknown PitchTier format.")
        }

        if (flines[3] != "") {
            stop("Unknown PitchTier format.")
        }

        if (nchar(flines[4]) < 1) {
            stop("Unknown PitchTier format.")
        }

        if (stringr::str_sub(flines[4], 1, 1) == "x") {  # TextFile
            xmin <- tidyr::extract_numeric(flines[4])
            xmax <- tidyr::extract_numeric(flines[5])
            N <- tidyr::extract_numeric(flines[6])

            if (N != (length(flines)-6)/3) {
                stop("Wrong number of points in PitchTier format.")
            }
            t <- numeric(N)
            f <- numeric(N)

            for (I in tbTools::seqM(1, N, by = 1)) {
                t[I] <- tidyr::extract_numeric(flines[8 + (I-1)*3])
                f[I] <- tidyr::extract_numeric(flines[9 + (I-1)*3])
            }

        } else {   # shortTextFile

            xmin <- as.numeric(flines[4])
            xmax <- as.numeric(flines[5])
            N <- as.integer(flines[6])

            if (N != (length(flines)-6)/2) {
                stop("Wrong number of points in PitchTier format.")
            }
            t <- numeric(N)
            f <- numeric(N)

            for (I in tbTools::seqM(1, N, by = 1)) {
                t[I] <- as.numeric(flines[7 + (I-1)*2])
                f[I] <- as.numeric(flines[8 + (I-1)*2])
            }
        }


    } else {   # headerless SpreadSheet

        N <- length(flines)

        t <- numeric(N)
        f <- numeric(N)

        for (I in tbTools::seqM(1, N, by = 1)) {
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

    return(pt)
}


#################

## Ulozi PitchTier jako spreadSheet. PitchTier je list a musi obsahovat alespon t a f stejne delky.
## Pokud nejsou v PitchTier specifikovany tmin a tmax, jsou brany jako min a max z t.
## v0.1 Tomas Boril, borilt@gmail.com
##     pt <- pt.read("demo/maminka_spreadSheet.PitchTier")
##     pt.write(pt, "demo/vystup.PitchTier")
pt.write <- function(pt, fileNamePitchTier) {
    if (!tbTools::isString(fileNamePitchTier)) {
        stop("Invalid 'fileNamePitchTier' parameter.")
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


    fid <- file(fileNamePitchTier, open = "w", encoding = "UTF-8")
    if (!isOpen(fid)) {
        stop(paste0('cannot open file [', fileNamePitchTier, ']'))
    }

    writeLines('"ooTextFile"', fid)
    writeLines('"PitchTier"', fid)
    writeLines(paste0(as.character(tbTools::round2(xmin, -20)), " ", as.character(tbTools::round2(xmax, -20)), " ", as.character(N)), fid)

    for (n in tbTools::seqM(1, N)) {
        writeLines(paste0(as.character(tbTools::round2(pt$t[n], -20)), "\t", as.character(tbTools::round2(pt$f[n], -20))), fid)
    }

    close(fid)
}

#############

## Zobrazi PitchTier ve schematicke a interaktivni podobe pomoci knihovny dygraphs.
## Nepovinny parametr group (string character) slouzi k synchronizaci skupin pripadne
## vice grafu dygraphs.
## v1.00 Tomas Boril, borilt@gmail.com
##     pt <- pt.read("demo/maminka_TextFile.PitchTier")
##     pt.plot(pt)
pt.plot <- function(pt, group = "") {
    data <- list(t = pt$t, f = pt$f)

    if (group != "") {  # pro synchronizaci s jinymi grafy dygraph, ktere maji nastavenu group na stejny nazev
        g <- dygraphs::dygraph(data, group = group, xlab = "Time (sec)")
    } else {
        g <- dygraphs::dygraph(data, xlab = "Time (sec)")
    }

    g <- dygraphs::dyOptions(g, drawPoints = TRUE, pointSize = 2, strokeWidth = 0)
    g <- dygraphs::dyRangeSelector(g, dateWindow = c(pt$tmin, pt$tmax))

    g <- dygraphs::dyAxis(g, "x", valueFormatter = 'function(d){return d.toFixed(3)}')
    g
}


#################









# tierInd <- tgCheckTierInd(tg, tierInd)
#     ntiers <- length(tg)



# if (!tbTools::isString(name)) {
#     stop("Name must be a character string.")
# }

# if (!tbTools::isNum(tMin)) {
#     stop("tMin must be a number.")
# }

# if (!tbTools::isInt(newInd)) {
#     stop("newInd must be integer >= 1.")
# }


#################
#################
#################
#################
#################
