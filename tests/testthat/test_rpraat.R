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



context("TextGrid")

test_that("tg.read", {
    expect_equal({
        tg <- tg.read("H.TextGrid")
        c(length(tg), length(unique(tg$word$t2)), tg[[1]]$label[[7]], tg[[2]]$type)
        }, c("5", "13", "k", "interval"))
    expect_equal({
        tg <- tg.read("H_short.TextGrid")
        c(length(tg), length(unique(tg$word$t2)), tg[[1]]$label[[7]], tg[[2]]$type)
        }, c("5", "13", "k", "interval"))
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
