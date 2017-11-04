context("Samples")

test_that("tg.sample", {
    expect_equal(length(tg.sample()), 5)
    expect_equal(tg.sample()$word$label[5], "co")
    expect_equal(tg.sample()[[2]]$t2[16] > tg.sample()[[2]]$t1[17], FALSE)
})

test_that("tg.sampleProblem", {
    expect_equal(tg.sampleProblem()[[2]]$t2[16] > tg.sampleProblem()[[2]]$t1[17], TRUE)
})

test_that("pt.sample", {
    expect_equal(length(pt.sample()$t), 209)
    expect_equal(length(pt.sample()$f), 209)
    expect_equal(length(unique(pt.sample()$t)), 209)
})


context("PitchTier")

test_that("pt.read", {
    expect_equal({
        pt <- pt.read("H.PitchTier")
        c(length(unique(pt$t)), pt$tmax)
        }, c(209, 3.617125))
    expect_equal({
        pt <- pt.read("H_UTF16.PitchTier", encoding = "UTF-16")
        c(length(unique(pt$t)), pt$tmax)
    }, c(209, 3.617125))
    expect_equal({
        pt <- pt.read("H_headerlessSpreadSheet.PitchTier")
        c(length(unique(pt$t)), pt$tmax)
        }, c(209, 3.4935625))
    expect_equal({
        pt <- pt.read("H_shortTextFile.PitchTier")
        c(length(unique(pt$t)), pt$tmax)
        }, c(209, 3.617125))
    expect_equal({
        pt <- pt.read("H_spreadSheet.PitchTier")
        c(length(unique(pt$t)), pt$tmax)
        }, c(209, 3.617125))
})

test_that("pt.Hz2ST", {
    expect_equal({
        pt <- pt.sample()
        pt2 <- pt.Hz2ST(pt)
        pt3 <- pt.Hz2ST(pt, ref = 200)
        c(length(pt2$f), length(pt3$f), pt$f[1], pt$f[45], pt$f[209], pt2$f[1], pt2$f[45], pt2$f[209], pt3$f[1], pt3$f[45], pt3$f[209], var(pt2$f), var(pt3$f))
    }, c(209, 209, 210.0627306, 196.4245331, 161.7025771, 12.8498427, 11.6877016, 8.3201121, 0.8498427, -0.3122984, -3.6798879, 11.2833270, 11.2833270))
})

test_that("pt.interpolate", {
    expect_equal({
        pt <- pt.sample()
        t <- c(-1, 0, 0.1, pt$t[3], pt$t[length(pt$t)], pt$t[length(pt$t)]+1)
        pt2 <- pt.interpolate(pt, t)
        c(pt2$tmin, pt2$tmax, length(pt2$t), length(pt2$f), pt2$t, pt2$f)
    }, c(pt$tmin, pt$tmax, length(t), length(t), t, 210.0627306, 210.0627306, 213.8849744, 219.4930673, 161.7025771, 161.7025771))
})

test_that("pt.legendre", {
    expect_error(pt.legendre(pt.sample(), -1))
    expect_error(pt.legendre(pt.sample(), npoints = 0, npolynomials = 0))
    expect_error(pt.legendre(pt.sample(), npoints = -1, npolynomials = 1))
    expect_error(pt.legendreSynth(1, NA))
    expect_error(pt.legendreSynth(1, numeric(0)))
    expect_equal({sum(is.nan(pt.legendre(pt.sample(), 0)))}, 4)
    expect_equal({is.nan(pt.legendre(pt.sample(), npoints = 0, npolynomials = 1))}, TRUE)
    expect_equal({pt.legendre(list(tmin=0, tmax=0.4, t=c(0, 0.1, 0.2, 0.3, 0.4), f=c(1, 2, 3, 6, -1)))}, c(2.7472472, 0.8711174, -2.2633733, -2.4655033))
    expect_equal({pt.legendre(list(tmin=0, tmax=0.4, t=c(0, 0.1, 0.2, 0.3, 0.4), f=c(1, 2, 3, 6, -1)), npolynomials = 1)}, 2.7472472472472)
    expect_equal({pt.legendre(list(tmin=0, tmax=0.4, t=c(0, 0.1, 0.2, 0.3, 0.4), f=c(1, 2, 3, 6, -1)), npoints = 2)}, c(0, -3,  0, -7))
    expect_equal({length(pt.legendreSynth(5, 0))}, 0)
    expect_equal({pt.legendreSynth(5, 1)}, 5)
    expect_equal({pt.legendreSynth(5, 3)}, c(5, 5, 5))
    expect_equal({pt.legendreSynth(c(1, 2, 3), 1)}, 2)
    expect_equal({pt.legendreSynth(c(1, 2, 3), 2)}, c(2, 6))
    expect_equal({pt.legendreSynth(c(1, 2, 3), 5)}, c(2, -0.375, -0.5, 1.625, 6))
})

test_that("pt.cut", {
    expect_error(pt.cut(pt.sample(), numeric(0)))
    expect_error(pt.cut(pt.sample(), NA))
    expect_equal({
        pt <- pt.cut(pt.sample(),  tStart = 3)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[37], pt$f[1], pt$f[10], pt$f[37])},
        c(3, 3.617125, 37, 37, 3.083563, 3.223562, 3.493562, 199.417691, 194.807345, 161.702577))

    expect_equal({
        pt <- pt.cut(pt.sample(),  tStart = 2, tEnd = 3)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[81], pt$f[1], pt$f[10], pt$f[81])},
        c(2, 3, 81, 81, 2.003563, 2.093562, 2.993562, 198.818598, 258.404655, 196.152600))

    expect_equal({
        pt <- pt.cut(pt.sample(),  tEnd = 1)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[71], pt$f[1], pt$f[10], pt$f[71])},
        c(0, 1, 71, 71, 0.0935625, 0.1835625, 0.9935625, 210.0627306, 189.5803367, 150.0365144))

    expect_equal({
        pt <- pt.cut(pt.sample(),  tStart = -1, tEnd = 1)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[71], pt$f[1], pt$f[10], pt$f[71])},
        c(-1, 1, 71, 71, 0.0935625, 0.1835625, 0.9935625, 210.0627306, 189.5803367, 150.0365144))
})

test_that("pt.cut0", {
    expect_error(pt.cut0(pt.sample(), numeric(0)))
    expect_error(pt.cut0(pt.sample(), NA))
    expect_equal({
        pt <- pt.cut0(pt.sample(),  tStart = 3)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[37], pt$f[1], pt$f[10], pt$f[37])},
        c(0, 0.617125, 37, 37, 0.083563, 0.223562, 0.493562, 199.417691, 194.807345, 161.702577))

    expect_equal({
        pt <- pt.cut0(pt.sample(),  tStart = 2, tEnd = 3)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[81], pt$f[1], pt$f[10], pt$f[81])},
        c(0, 1, 81, 81, 0.003563, 0.093562, 0.993562, 198.818598, 258.404655, 196.152600))

    expect_equal({
        pt <- pt.cut0(pt.sample(),  tEnd = 1)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[71], pt$f[1], pt$f[10], pt$f[71])},
        c(0, 1, 71, 71, 0.0935625, 0.1835625, 0.9935625, 210.0627306, 189.5803367, 150.0365144))

    expect_equal({
        pt <- pt.cut0(pt.sample(),  tStart = -1, tEnd = 1)
        c(pt$tmin, pt$tmax, length(pt$t), length(pt$f), pt$t[1], pt$t[10], pt$t[71], pt$f[1], pt$f[10], pt$f[71])},
        c(0, 2, 71, 71, 1.0935625, 1.1835625, 1.9935625, 210.0627306, 189.5803367, 150.0365144))
})



context("TextGrid")

test_that("tg.read", {
    expect_equal({
        tg <- tg.read("H.TextGrid")
        c(length(tg), length(unique(tg$word$t2)), tg[[1]]$label[[7]], tg[[2]]$type, tg$word$label[4], tg$word$label[6])
        }, c("5", "13", "k", "interval", "\u0159eknu", "ud\u011bl\u00e1\u0161"))
    expect_equal({
        tg <- tg.read("H_short.TextGrid")
        c(length(tg), length(unique(tg$word$t2)), tg[[1]]$label[[7]], tg[[2]]$type, tg$word$label[4], tg$word$label[6])
        }, c("5", "13", "k", "interval", "\u0159eknu", "ud\u011bl\u00e1\u0161"))
    expect_equal({
        tg <- tg.read("utf8.TextGrid")
        tg$phone$label[2]
        }, "\u0294")
    expect_equal({
        tg <- tg.read("H_UTF16.TextGrid", "UTF-16")
        c(length(tg), length(unique(tg$word$t2)), tg[[1]]$label[[7]], tg[[2]]$type, tg$word$label[4], tg$word$label[6])
    }, c("5", "13", "k", "interval", "\u0159eknu", "ud\u011bl\u00e1\u0161"))
    expect_equal({
        tg <- tg.read("H_short_UTF16.TextGrid", "UTF-16")
        c(length(tg), length(unique(tg$word$t2)), tg[[1]]$label[[7]], tg[[2]]$type, tg$word$label[4], tg$word$label[6])
    }, c("5", "13", "k", "interval", "\u0159eknu", "ud\u011bl\u00e1\u0161"))
    expect_equal({
        tg <- tg.read("2pr.TextGrid")
        tg$ORT$label[4:6]
    }, c("wracal\npokus", "siebie\"\"\nah\"\"\na", "siebie\"\"\na\"\"h\"\"\na"))
    expect_equal({
        tg <- tg.read("sppas.TextGrid")
        tg$ORT$label[4:6]
    }, c("wracal\npokus", "siebe\"\"\nah\"\"\na", "siebie\"\"\na\"\"h\"\"\na"))
})


test_that("tg.repairContinuity", {
    expect_equal({
        tg <- tg.repairContinuity(tg.sampleProblem(), verbose = TRUE)
        tg[[2]]$t2[16] > tg[[2]]$t1[17]
        }, FALSE)
    expect_error(tg.repairContinuity(pt.sample()))
})


test_that("tg.checkTierInd", {
    expect_equal(tg.checkTierInd(tg.sample(), 4), 4)
    expect_equal(tg.checkTierInd(tg.sample(), "syllable"), 3)
    expect_error(tg.checkTierInd(tg.sample(), "WORD"))
    expect_error(tg.checkTierInd(tg.sample(), 6))
})


test_that("tg.insertNewIntervalTier, tg.insertBoundary, tg.insertInterval, tg.insertNewPointTier", {
    expect_equal({
        tg2 <- tg.insertNewIntervalTier(tg.sample(), 1, "INTERVALS")
        tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.8)
        tg2 <- tg.insertBoundary(tg2, "INTERVALS", 0.1, "Interval A")
        tg2 <- tg.insertInterval(tg2, "INTERVALS", 1.2, 2.5, "Interval B")
        tg2 <- tg.insertInterval(tg2, "INTERVALS", 2.5, 2.7, "Interval C")
        tg2 <- tg.insertInterval(tg2, "INTERVALS", 2.7, 3.616, "Interval D")
        c(length(tg2), tg2$INTERVALS$t1, tg2$INTERVALS$t2, tg2$INTERVALS$label)
        }, c(6, c(0.0, 0.1, 0.8, 1.2, 2.5, 2.7), c(0.1, 0.8, 1.2, 2.5, 2.7, 3.616), c("", "Interval A", "", "Interval B", "Interval C", "Interval D")))
    expect_equal({
        tg <- tg.insertNewPointTier(tg.sample(), 2, newTierName = "aha")
        c(tg[[2]]$name, tg[[3]]$name)
        }, c("aha", "phone"))
    expect_error(tg.insertNewIntervalTier(tg.sample(), 7, newTierName = "aha"))
    expect_error(tg.insertNewPointTier(tg.sample(), 7, newTierName = "aha"))
})

test_that("tg.duplicateTier", {
    expect_equal({
        tg2 <- tg.duplicateTier(tg.sample(), "word", 1, "NEW")
        c(sum(tg2$NEW$label == tg2$word$label), sum(tg2$NEW$t1 == tg2$word$t1), sum(tg2$NEW$t2 == tg2$word$t2), tg2[[1]]$name, tg2[[2]]$name, length(tg2))
        }, c("13", "13", "13", "NEW", "phoneme", "6"))
    expect_equal({
        tg2 <- tg.duplicateTier(tg.sample(), "phoneme", 3, "NEW")
        c(sum(tg2$NEW$label == tg2$phoneme$label), sum(tg2$NEW$t == tg2$phoneme$t), tg2[[1]]$name, tg2[[2]]$name, length(tg2), tg2[[3]]$name, tg2[[4]]$name)
        }, c("43", "43", "phoneme", "phone", "6", "NEW", "syllable"))
    expect_error(tg.duplicateTier(tg.sample(), "aha", 3, "NEW"))
})

test_that("tg.removeIntervalBothBoundaries", {
    expect_equal(tg.removeIntervalBothBoundaries(tg.sample(), "word", 3)$word$label[2], "jatireknu")
})

test_that("tg.removeIntervalRightBoundary", {
    expect_equal(tg.removeIntervalRightBoundary(tg.sample(), "word", 3)$word$label[3], "tireknu")
})

test_that("tg.removeIntervalLeftBoundary", {
    expect_equal(tg.removeIntervalLeftBoundary(tg.sample(), "word", 3)$word$label[2], "jati")
})

test_that("tg.insertPoint", {
    expect_equal({
        tg2 <- tg.insertPoint(tg.sample(), "phoneme", 1.4, "NEW POINT")
        c(length(tg2$phoneme$t), length(tg2$phoneme$label), tg2$phoneme$t[18], tg2$phoneme$label[18])
        }, c("44", "44", "1.4", "NEW POINT"))
})

test_that("tg.removePoint", {
    expect_equal({
        tg2 <- tg.removePoint(tg.sample(), "phoneme", 2)
        c(length(tg2$phoneme$t), length(tg2$phoneme$label), tg2$phoneme$t[18], tg2$phoneme$label[18],
          tg2$phoneme$t[1], tg2$phoneme$label[2],
          tg2$phoneme$t[1], tg2$phoneme$label[2])
        }, c("42", "42", "1.92282441350142", "e", "0.120889365898715", "c", "0.120889365898715", "c"))
    expect_error(tg.removePoint(tg, "phoneme", 44))
})

test_that("tg.getPointIndexNearestTime", {
    expect_equal(tg.getPointIndexNearestTime(tg.sample(), "phoneme", 0.5), 7)
})

test_that("tg.getPointIndexLowerThanTime", {
    expect_equal(tg.getPointIndexLowerThanTime(tg.sample(), "phoneme", 0.5), 6)
})

test_that("tg.getPointIndexHigherThanTime", {
    expect_equal(tg.getPointIndexHigherThanTime(tg.sample(), "phoneme", 0.5), 7)
})

test_that("tg.getIntervalIndexAtTime", {
    expect_equal(tg.getIntervalIndexAtTime(tg.sample(), "word", 0.5), 4)
    expect_equal(tg.getIntervalIndexAtTime(tg.sample(), "word", tg.sample()$word$t1[5]), 5)
})

test_that("tg.removeTier", {
    expect_equal({
        tg2 <- tg.removeTier(tg.sample(), "word")
        names(tg2)
        }, c("phoneme", "phone", "syllable", "phrase"))
    expect_error(tg2 <- tg.removeTier(tg.sample(), "wor"))
})

test_that("tg.getPointTime", {
    expect_equal(tg.getPointTime(tg.sample(), "phoneme", 4), 0.3235253313)
    expect_error(tg.getPointTime(tg.sample(), "phoneme", 44))
})

test_that("tg.getIntervalDuration", {
    expect_equal(tg.getIntervalDuration(tg.sample(), "phone", 5), 0.0572624682)
    expect_error(tg.getIntervalDuration(tg.sample(), "phone", 50))
})

test_that("tg.getIntervalEndTime", {
    expect_equal(tg.getIntervalEndTime(tg.sample(), "phone", 5), 0.3521565654)
    expect_error(tg.getIntervalEndTime(tg.sample(), "phone", 50))
})

test_that("tg.getIntervalStartTime", {
    expect_equal(tg.getIntervalStartTime(tg.sample(), "phone", 5), 0.2948940972)
    expect_error(tg.getIntervalStartTime(tg.sample(), "phone", 50))
})

test_that("tg.getLabel, tg.setLabel", {
    expect_equal({
        tg2 <- tg.setLabel(tg.sample(), "word", 3, "New Label")
        tg.getLabel(tg2, "word", 3)
        }, "New Label")
    expect_equal(tg.getLabel(tg.sample(), "phoneme", 4), "i")
    expect_error(tg.setLabel(tg.sample(), "Word", 3, "New Label"))
    expect_error(tg.setLabel(tg.sample(), "word", 14, "New Label"))
    expect_error(tg.getLabel(tg.sample(), "word", 14))
})

test_that("tg.getNumberOfIntervals", {
    expect_equal(tg.getNumberOfIntervals(tg.sample(), "phone"), 49)
    expect_error(tg.getNumberOfIntervals(tg.sample(), 52))
    expect_error(tg.getNumberOfIntervals(tg.sample(), "PHONE"))
    expect_error(tg.getNumberOfIntervals(tg.sample(), "phoneme"))
})

test_that("tg.getNumberOfPoints", {
    expect_equal(tg.getNumberOfPoints(tg.sample(), "phoneme"), 43)
    expect_error(tg.getNumberOfPoints(tg.sample(), "word"))
})

test_that("tg.getNumberOfTiers", {
    expect_equal(tg.getNumberOfTiers(tg.sample()), 5)
})

test_that("tg.getTotalDuration, tg.getEndTime, tg.getStartTime", {
    expect_equal(tg.getTotalDuration(tg.sample()), 3.616)
    expect_equal(tg.getTotalDuration(tg.sample(), "phone"), 3.608)
    expect_equal(tg.getTotalDuration(tg.sample(), "phoneme"), 3.3337929937)
    expect_equal(tg.getEndTime(tg.sample()), 3.616)
    expect_equal(tg.getEndTime(tg.sample(), "phone"), 3.616)
    expect_equal(tg.getEndTime(tg.sample(), "phoneme"), 3.4546823596)
    expect_equal(tg.getEndTime(tg.sample(), "phrase"), 3.608)
    expect_equal(tg.getStartTime(tg.sample()), 0)
    expect_equal(tg.getStartTime(tg.sample(), "phone"), 0.008)
    expect_equal(tg.getStartTime(tg.sample(), "phoneme"), 0.1208893659)
})

test_that("tg.countLabels", {
    expect_equal(tg.countLabels(tg.sample(), "phone", "a"), 5)
    expect_equal(tg.countLabels(tg.sample(), "phone", "a:"), 3)
    expect_equal(tg.countLabels(tg.sample(), "phone", ":"), 0)
    expect_equal(tg.countLabels(tg.sample(), "phoneme", "a"), 6)
})

test_that("tg.setTierName, tg.getTierName", {
    expect_equal({
        tg2 <- tg.setTierName(tg.sample(), "word", "WORDTIER")
        tg.getTierName(tg2, 4)
        }, "WORDTIER")
    expect_equal({
        tg2 <- tg.setTierName(tg.sample(), "word", "WORDTIER")
        names(tg2)
        }, c("phoneme", "phone", "syllable", "WORDTIER", "phrase"))
    expect_error(tg.setTierName(tg.sample(), 6, "WORDTIER"))
    expect_error(tg.getTierName(tg.sample(), 6))
})

test_that("tg.isPointTier, tg.isIntervalTier", {
    expect_equal(tg.isPointTier(tg.sample(), 1), TRUE)
    expect_equal(tg.isPointTier(tg.sample(), "word"), FALSE)
    expect_equal(tg.isIntervalTier(tg.sample(), 1), FALSE)
    expect_equal(tg.isIntervalTier(tg.sample(), "word"), TRUE)
})


test_that("tg.createNewTextGrid", {
    expect_equal({
        tg <- tg.createNewTextGrid(0, 5)
        c(length(tg), class(tg)["tmin"], class(tg)["tmax"])
        }, c("0", tmin="0", tmax="5"))
})

test_that("tg.findLabels", {
    expect_error(tg.findLabels(tg.sample(), "word", "nic", "aha"))
    expect_error(tg.findLabels(tg.sample(), "word"))
    expect_error(tg.findLabels(tg.sample(), "word", "co", 0))
    expect_error(tg.findLabels(tg.sample(), "word", 4))
    expect_equal(length(tg.findLabels(tg.sample(), "word", character(0))), 0)
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", "")
        c(class(q), length(q), q[[1]], q[[2]])},
        c("list", 2, 1, 13))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", "")
        c(class(q), length(q))},
        c("list", 0))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", "nic")
        c(class(q), length(q))},
        c("list", 0))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", "reknu")
        c(class(q), length(q), q[[1]])},
        c("list", 1, 4))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phone", "a")
        c(class(q), length(q), q[[1]], q[[2]], q[[3]], q[[4]], q[[5]])},
        c("list", 5, 29, 40, 42, 44, 46))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", c("n"))
        c(class(q), length(q), q[[1]], q[[2]], q[[3]], q[[4]])},
        c("list", 4, 8, 18, 25, 42))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", c("ti", "reknu", "co"))
        c(class(q), length(q), q[[1]][1], q[[1]][2], q[[1]][3])},
        c("list", 1, 3, 4, 5))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phone", c("?", "a"))
        c(class(q), length(q), q[[1]][1], q[[1]][2], q[[2]][1], q[[2]][2])},
        c("list", 2, 39, 40, 41, 42))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", c("n", "e"))
        c(class(q), length(q), q[[1]][1], q[[1]][2])},
        c("list", 1, 18, 19))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", c("n", "a"))
        c(class(q), length(q), q[[1]][1], q[[1]][2], q[[2]][1], q[[2]][2])},
        c("list", 2, 25, 26, 42, 43))

    expect_equal(length(tg.findLabels(tg.sample(), "word", character(0), TRUE)), 0)
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", "", TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1], q$t1[2], q$t2[2])},
        c("list", 2, 2, 2, 0.008, 0.0965724658757064, 3.495928125, 3.616))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", "", TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2))},
        c("list", 2, 0, 0))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", "nic", TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2))},
        c("list", 2, 0, 0))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", "reknu", TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1])},
        c("list", 2, 1, 1, 0.352156565444145, 0.632200305451128))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phone", "a", TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1], q$t1[2], q$t2[2], q$t1[3], q$t2[3], q$t1[4], q$t2[4], q$t1[5], q$t2[5])},
        c("list", 2, 5, 5, 2.24830876409774, 2.30352886461156, 2.96666963493613, 3.02360108418367, 3.07030520488411, 3.10631502016129, 3.18439423076923, 3.2390296474359, 3.3053099702381, 3.35952210541475))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", c("n"), TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1], q$t1[2], q$t2[2], q$t1[3], q$t2[3], q$t1[4], q$t2[4])},
        c("list", 2, 4, 4, 0.562717206724197, 0.562717206724197, 1.88902324993668, 1.88902324993668, 2.22032423657473, 2.22032423657473, 3.38647934980882, 3.38647934980882))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "word", c("ti", "reknu", "co"), TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1])},
        c("list", 2, 1, 1, 0.215988182773109, 0.760009490030675))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phone", c("?", "a"), TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1], q$t1[2], q$t2[2])},
        c("list", 2, 2, 2, 2.91140769675926, 3.02360108418367, 3.02360108418367, 3.10631502016129))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", c("n", "e"), TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1])},
        c("list", 2, 1, 1, 1.88902324993668, 1.92282441350142))
    expect_equal({
        q <- tg.findLabels(tg.sample(), "phoneme", c("n", "a"), TRUE)
        c(class(q), length(q), length(q$t1), length(q$t2), q$t1[1], q$t2[1], q$t1[2], q$t2[2])},
        c("list", 2, 2, 2, 2.22032423657473, 2.27591881435465, 3.38647934980882, 3.45468235960145))
})

test_that("tg.duplicateTierMergeSegments", {
    expect_error({
        tg <- tg.read("H3.TextGrid")   # prázdné segmenty uvnitř slabiky vadí
        pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-tso-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-t_soa-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S--nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:-at"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-nu-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "a:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f--naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = " ")
    })
    expect_error({
        tg <- tg.read("H.TextGrid")  # should not duplicate point tier
        pattern <- "ja:ciP\\eknut_souJ\\ela:SnejdP\\i:fnajdeZhut_Skuaatamana"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phoneme", 1, "syll", pattern, sep = "-")
    })
    expect_equal({
        tg <- tg.read("H.TextGrid")
        pattern <- "ja:-ci-P\\ek-nu-t_so-?u-J\\e-la:S- -nej-dP\\i:f-naj-deZ-h\\ut_S-ku-?a-?a-ta-ma-na:"
        tg2 <- tg.duplicateTierMergeSegments(tg, "phone", 1, "syll", pattern, sep = "-")
        c(length(tg2), tg2[[1]]$name, length(tg2$syll$label) - length(tg2$syllable$label),
          sum(tg2$syll$label == tg2$syllable$label), # one difference: pause " " != ""
          identical(tg2$syll$t1[2:22], tg2$syllable$t1[2:22]),
          tg2$syll$t1[1],
          identical(tg2$syll$t2, tg2$syllable$t2),
          tg2$syll$type
          )
        },
        c(6, "syll", 0, 21, TRUE, 0.008, TRUE, "interval"))
})

context("Pitch")

test_that("pitch.read", {
    expect_equal({
        p <- pitch.read("sound.Pitch")
        c(p$xmin, p$xmax, p$nx, p$dx, p$x1, length(p$t), p$t[1], p$t[2], p$t[508], p$ceiling, p$maxnCandidates, length(p$frame), p$frame[[4]]$intensity,
          p$frame[[4]]$nCandidates, length(p$frame[[4]]$frequency), length(p$frame[[4]]$strength), p$frame[[4]]$frequency[1], p$frame[[4]]$frequency[2],
          p$frame[[4]]$frequency[3], p$frame[[4]]$frequency[4], p$frame[[4]]$strength[1], p$frame[[4]]$strength[2], p$frame[[4]]$strength[3],
          p$frame[[4]]$strength[4], p$frame[[508]]$intensity, p$frame[[508]]$nCandidates, length(p$frame[[508]]$frequency), length(p$frame[[508]]$strength),
          p$frame[[508]]$frequency[1], p$frame[[508]]$strength[1])
    }, c(0, 5.112, 508, 0.01, 0.021000000000000015, 508, 0.021000000000000015, 0.031000000000000015, 5.091000000000000015, 600, 15, 508, 6.35938550499208e-005,
         4, 4, 4, 0, 6252.408223974137, 3392.821528656231, 1197.0707582170926, 0, 0.3169408893924507, 0.2917449063347636, 0.2758620333629818, 0, 1, 1, 1, 0, 0))
    expect_equal({
        p <- pitch.read("sound.Pitch")
        p2 <- pitch.read("sound_short.Pitch")
        p3 <- pitch.read("sound_UTF16.Pitch", encoding = "UTF-16")
        c(identical(p, p2), identical(p, p3))
    }, c(TRUE, TRUE))
})

context("strings")

test_that("strtrim works", {
    expect_equal(strTrim("      Hello World!    "), "Hello World!")
    expect_equal(strTrim("Hello World!    "), "Hello World!")
    expect_equal(strTrim("      Hello World!"), "Hello World!")
    expect_equal(strTrim("Hello World!"), "Hello World!")
    expect_equal(strTrim("   "), "")
    expect_equal(strTrim(""), "")
    expect_equal(strTrim(" ěšččřžýůú  "), "ěšččřžýůú")
    expect_output(strTrim(NA), NA)
    expect_equal(strTrim(1:5), as.character(1:5))
})

test_that("str_contains works", {
    expect_equal(str_contains("Hello world", "wor"), TRUE)
    expect_equal(str_contains("Hello world", "WOR"), FALSE)
    expect_equal(str_contains(tolower("Hello world"), tolower("wor")), TRUE)
    expect_equal(str_contains("Hello world", ""), TRUE)
})

test_that("str_find works", {
    expect_equal(str_find("Hello, hello, hello world", "ell"), c(2, 9, 16))
    expect_identical(str_find("Hello, hello, hello world", "q"), integer(0))
})

test_that("str_find1 works", {
    expect_equal(str_find1("Hello, hello, hello world", "ell"), 2)
    expect_identical(str_find1("Hello, hello, hello world", "q"), integer(0))
})


context("sequence")

test_that("seqM works", {
    expect_equal(seqM(1, 3, 0.5), c(1, 1.5, 2, 2.5, 3))
    expect_equal(seqM(1, 3), 1:3)
    expect_equal(seqM(1, 3, by=.8), c(1, 1.8, 2.6))
    expect_equal(seqM(1, 3, by=5), 1)
    expect_equal(seqM(3, 1), integer(0))
    expect_equal(seqM(3, 1, by=+1), integer(0))
    expect_equal(seqM(3, 1, by=-1), c(3, 2, 1))
    expect_equal(seqM(3, 1, by=-3), 3)
    expect_equal(seqM(1, 3, len=5), c(1, 1.5, 2, 2.5, 3))
    expect_equal(seqM(1, 3, len=3), c(1, 2, 3))
    expect_equal(seqM(1, 3, len=2), c(1, 3))
    expect_equal(seqM(1, 3, len=1), 3)
    expect_warning(seqM(1, 3, len=0), "length.out == 0, return empty vector")
    expect_warning(seqM(1, 3, len=-2))
    expect_equal(seqM(3, 1, len=3), c(3, 2, 1))
    expect_error(seqM(1, 3, 1, 3))
    expect_error(seqM(1, 3, 1, 2))
    expect_equal(seqM(from=2, by=1, len=3), c(2, 3, 4))
    expect_equal(seqM(from=2, by=-1, len=3), c(2, 1, 0))
    expect_equal(seqM(to=2, by=1, len=3), c(0, 1, 2))
    expect_equal(seqM(to=2, by=-1, len=3), c(4, 3, 2))
    expect_equal(seqM(from=2, by=0, len=3), c(2, 2, 2))
    expect_equal(seqM(to=2, by=0, len=3), c(2, 2, 2))
    expect_warning(seqM(from=1, by=1, len=0), "length.out == 0, return empty vector")
    expect_warning(seqM(to=1, by=1, len=0), "length.out == 0, return empty vector")
    expect_equal(class(seqM(3, 1, len=3)), "numeric")
    expect_equal(class(seqM(1, 3, len=3)), "numeric")
    expect_equal(class(seqM(3, 1, by=-1)), "integer")
    expect_equal(class(seqM(3, 1, by=-5)), "integer")
    expect_equal(class(seqM(1, 3, by=1)), "integer")
    expect_equal(class(seqM(1, 3, by=2)), "integer")
    expect_equal(class(seqM(from=1, by=3, len=5)), "integer")
    expect_equal(class(seqM(to=1, by=3, len=5)), "integer")
    expect_equal(class(seqM(from=1, by=-3, len=5)), "integer")
    expect_equal(class(seqM(to=1, by=-3, len=5)), "integer")
})


context("isSomething")

test_that("isInt works", {
    expect_equal(isInt(2), TRUE)
    expect_equal(isInt(2L), TRUE)
    expect_equal(isInt(-2), TRUE)
    expect_equal(isInt(-2L), TRUE)
    expect_equal(isInt(2.1), FALSE)
    expect_equal(isInt(-2.1), FALSE)
    expect_equal(isInt(1:5), FALSE)
    expect_equal(isInt(NA_integer_), FALSE)
    expect_equal(isInt(NA), FALSE)
    expect_equal(isInt(integer(0)), FALSE)
    expect_equal(isInt(mtcars), FALSE)
})



test_that("isString works", {
    expect_equal(isString("hello"), TRUE)
    expect_equal(isString(""), TRUE)
    expect_equal(isString(2), FALSE)
    expect_equal(isString(c("hello", "world")), FALSE)
    expect_equal(isString(NA_character_), FALSE)
    expect_equal(isString(NA), FALSE)
    expect_equal(isString(character(0)), FALSE)
    expect_equal(isString(mtcars), FALSE)
})

test_that("isNum works", {
    expect_equal(isNum(2), TRUE)
    expect_equal(isNum(2L), TRUE)
    expect_equal(isNum(-2), TRUE)
    expect_equal(isNum(-2L), TRUE)
    expect_equal(isNum(2.1), TRUE)
    expect_equal(isNum(-2.1), TRUE)
    expect_equal(isNum(1:5), FALSE)
    expect_equal(isNum(NA_integer_), FALSE)
    expect_equal(isNum(NA_real_), FALSE)
    expect_equal(isInt(NA), FALSE)
    expect_equal(isInt(integer(0)), FALSE)
    expect_equal(isInt(numeric(0)), FALSE)
    expect_equal(isInt(mtcars), FALSE)
})

test_that("isLogical works", {
    expect_equal(isLogical(TRUE), TRUE)
    expect_equal(isLogical(FALSE), TRUE)
    expect_equal(isLogical(T), TRUE)
    expect_equal(isLogical(F), TRUE)
    expect_equal(isLogical(1), FALSE)
    expect_equal(isLogical(0), FALSE)
    expect_equal(isLogical(2), FALSE)
    expect_equal(isLogical(c(TRUE, TRUE)), FALSE)
    expect_equal(isLogical(NaN), FALSE)
    expect_equal(isLogical(logical(0)), FALSE)
    expect_equal(isLogical(NA_integer_), FALSE)
    expect_equal(isLogical(NA_real_), FALSE)
    expect_equal(isLogical(NA), FALSE)
    expect_equal(isLogical(integer(0)), FALSE)
    expect_equal(isLogical(numeric(0)), FALSE)
    expect_equal(isLogical(mtcars), FALSE)
})


context("round2")

test_that("round2 works", {
    expect_equal(round2(23.5), 24)
    expect_equal(round2(23.4), 23)
    expect_equal(round2(24.5), 25)
    expect_equal(round2(-23.5), -24)
    expect_equal(round2(-23.4), -23)
    expect_equal(round2(-24.5), -25)
    expect_equal(round2(123.456, -1), 123.5)
    expect_equal(round2(123.456, -2), 123.46)
    expect_equal(round2(123.456, 1), 120)
    expect_equal(round2(123.456, 2), 100)
    expect_equal(round2(123.456, 3), 0)
    expect_equal(round2(-123.456, -1), -123.5)
    expect_equal(round2(-123.456, -2), -123.46)
    expect_equal(round2(-123.456, 1), -120)
    expect_equal(round2(-123.456, 2), -100)
    expect_equal(round2(-123.456, 3), 0)
    expect_output(round2(NA), NA)
    expect_equal(round2(c(0.3, 2, pi)), c(0, 2, 3))
})

context("ifft")

test_that("ifft works", {
    expect_equal(Re(ifft(3)), 3)
    expect_equal(Im(ifft(3)), 0)
    expect_equal(ifft(fft(1:5)), as.complex(1:5))
})

