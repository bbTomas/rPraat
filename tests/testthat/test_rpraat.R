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

test_that("it.sample", {
    expect_equal(length(it.sample()$t), 264)
    expect_equal(length(it.sample()$i), 264)
    expect_equal(length(unique(it.sample()$t)), 264)
})

test_that("pitch.sample", {
    expect_equal(length(pitch.sample()$t), 358)
    expect_equal(pitch.sample()$ceiling, 600)
    expect_equal(pitch.sample()$frame[[5]]$nCandidates, 3)
    expect_equal(pitch.sample()$frame[[5]]$intensity, 0.00283569949248335)
    expect_equal(pitch.sample()$frame[[5]]$frequency[3], 158.25417311564)
    expect_equal(pitch.sample()$frame[[5]]$strength[3], 0.250648211921512)
})

test_that("formant.sample", {
    expect_equal(length(formant.sample()$t), 571)
    expect_equal(formant.sample()$t[5], 0.0523)
    expect_equal(formant.sample()$frame[[5]]$nFormants, 5)
    expect_equal(formant.sample()$frame[[5]]$intensity, 1.77585520803808e-05)
    expect_equal(formant.sample()$frame[[5]]$frequency[4], 3394.4)
    expect_equal(formant.sample()$frame[[5]]$bandwidth[4], 82)
})

test_that("snd.sample", {
    expect_equal(length(snd.sample()), 7)
    expect_equal(length(snd.sample()$t), 5484)
    expect_equal(snd.sample()$t[1], 0)
    expect_equal(snd.sample()$t[2], 1/8000)
    expect_equal(snd.sample()$t[5484], 0.685375)
    expect_equal(snd.sample()$fs, 8000)
    expect_equal(snd.sample()$nChannels, 1)
    expect_equal(snd.sample()$nBits, 16)
    expect_equal(snd.sample()$nSamples, 5484)
    expect_equal(snd.sample()$duration, 0.6855)
    expect_equal(class(snd.sample()$sig), "matrix")
    expect_equal(dim(snd.sample()$sig), c(5484, 1))
    expect_equal(snd.sample()$sig[1, 1], 0)
    expect_equal(snd.sample()$sig[2, 1], 0.00116)
    expect_equal(snd.sample()$sig[1000, 1], -0.033235)
    expect_equal(snd.sample()$sig[5484, 1], 0)
})



context("PitchTier")

test_that("pt.read", {
    expect_equal({
        pt <- pt.read("H.PitchTier")
        c(length(unique(pt$t)), pt$tmin, pt$tmax, length(pt$t), pt$t[1], pt$t[16], pt$t[29], pt$t[209], pt$f[1], pt$f[16], pt$f[29], pt$f[209])
        }, c(209, 0, 3.617125, 209, 0.09356250000000005, 0.29356250000000006, 0.42356250000000006, 3.4935625000000003,
             210.06273060415666, 263.3608508907508, 259.37630326892423, 161.7025770872298))
    expect_equal({
        pt <- pt.read("H_UTF16.PitchTier", encoding = "UTF-16")
        c(length(unique(pt$t)), pt$tmin, pt$tmax, length(pt$t), pt$t[1], pt$t[16], pt$t[29], pt$t[209], pt$f[1], pt$f[16], pt$f[29], pt$f[209])
    }, c(209, 0, 3.617125, 209, 0.09356250000000005, 0.29356250000000006, 0.42356250000000006, 3.4935625000000003,
         210.06273060415666, 263.3608508907508, 259.37630326892423, 161.7025770872298))
    expect_equal({
        pt <- pt.read("H_headerlessSpreadSheet.PitchTier")
        c(length(unique(pt$t)), pt$tmin, pt$tmax, length(pt$t), pt$t[1], pt$t[16], pt$t[29], pt$t[209], pt$f[1], pt$f[16], pt$f[29], pt$f[209])
    }, c(209, 0.09356250000000005, 3.4935625000000003, 209, 0.09356250000000005, 0.29356250000000006, 0.42356250000000006, 3.4935625000000003,
         210.06273060415666, 263.3608508907508, 259.37630326892423, 161.7025770872298))
    expect_equal({
        pt <- pt.read("H_shortTextFile.PitchTier")
        c(length(unique(pt$t)), pt$tmin, pt$tmax, length(pt$t), pt$t[1], pt$t[16], pt$t[29], pt$t[209], pt$f[1], pt$f[16], pt$f[29], pt$f[209])
    }, c(209, 0, 3.617125, 209, 0.09356250000000005, 0.29356250000000006, 0.42356250000000006, 3.4935625000000003,
         210.06273060415666, 263.3608508907508, 259.37630326892423, 161.7025770872298))
    expect_equal({
        pt <- pt.read("H_spreadSheet.PitchTier")
        c(length(unique(pt$t)), pt$tmin, pt$tmax, length(pt$t), pt$t[1], pt$t[16], pt$t[29], pt$t[209], pt$f[1], pt$f[16], pt$f[29], pt$f[209])
    }, c(209, 0, 3.617125, 209, 0.09356250000000005, 0.29356250000000006, 0.42356250000000006, 3.4935625000000003,
         210.06273060415666, 263.3608508907508, 259.37630326892423, 161.7025770872298))
})

test_that("pt.write", {
    expect_equal({
        pt <- pt.read("Hround.PitchTier")
        f <- tempfile()
        pt.write(pt, f, "short")
        pt2 <- as.pt(pt.read(f), "Hround.PitchTier")
        unlink(f)
        identical(pt, pt2)
    }, TRUE)
    expect_equal({
        pt <- pt.read("Hround.PitchTier")
        f <- tempfile()
        pt.write(pt, f, "text")
        pt2 <- as.pt(pt.read(f), "Hround.PitchTier")
        unlink(f)
        identical(pt, pt2)
    }, TRUE)
    expect_equal({
        pt <- pt.read("Hround.PitchTier")
        f <- tempfile()
        pt.write(pt, f, "spreadsheet")
        pt2 <- as.pt(pt.read(f), "Hround.PitchTier")
        unlink(f)
        identical(pt, pt2)
    }, TRUE)
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
    expect_error(pt.cut(pt.sample(), 3, 2))
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
    expect_error(pt.cut0(pt.sample(), 3, 2))
})


context("IntensityTier")

test_that("it.read", {
    expect_equal({
        it <- it.read("maminka.IntensityTier")
        c(length(unique(it$t)), it$tmin, it$tmax, length(it$t), it$t[1], it$t[16], it$t[29], it$t[40], it$i[1], it$i[16], it$i[29], it$i[40])
    }, c(40, 0, 0.5460770975056689, 40, 0.0501814058956916, 0.22160997732426302, 0.3701814058956916, 0.4958956916099773, 59.5715903919772, 71.63843325188716, 64.17176220056767, 64.98963270408825))
    expect_equal({
        it <- it.read("maminka_short.IntensityTier")
        c(length(unique(it$t)), it$tmin, it$tmax, length(it$t), it$t[1], it$t[16], it$t[29], it$t[40], it$i[1], it$i[16], it$i[29], it$i[40])
    }, c(40, 0, 0.5460770975056689, 40, 0.0501814058956916, 0.22160997732426302, 0.3701814058956916, 0.4958956916099773, 59.5715903919772, 71.63843325188716, 64.17176220056767, 64.98963270408825))
})

test_that("it.write", {
    expect_equal({
        it <- it.read("maminka.IntensityTier")
        f <- tempfile()
        it.write(it, f, "short")
        it2 <- it.read(f)
        unlink(f)
        c(length(it), it$t, it$i, it$tmin, it$tmax)
    }, c(length(it2), it2$t, it2$i, it2$tmin, it2$tmax))
    expect_equal({
        it <- it.read("maminka.IntensityTier")
        f <- tempfile()
        it.write(it, f)
        it2 <- it.read(f)
        unlink(f)
        c(length(it), it$t, it$i, it$tmin, it$tmax)
    }, c(length(it2), it2$t, it2$i, it2$tmin, it2$tmax))
    expect_equal({
        it <- it.read("maminka.IntensityTier")
        f <- tempfile()
        it.write(it, f, "text")
        it2 <- it.read(f)
        unlink(f)
        c(length(it), it$t, it$i, it$tmin, it$tmax)
    }, c(length(it2), it2$t, it2$i, it2$tmin, it2$tmax))
})

test_that("it.interpolate", {
    expect_equal({
        it <- it.sample()
        t <- c(-1, 0, 0.1, it$t[3], it$t[length(it$t)], it$t[length(it$t)]+1)
        it2 <- it.interpolate(it, t)
        c(it2$tmin, it2$tmax, length(it2$t), length(it2$i), it2$t, it2$i)
    }, c(it$tmin, it$tmax, length(t), length(t), t, 40.85635685, 40.85635685, 69.94393210, 61.13001675, 39.57790479, 39.57790479))
})

test_that("it.legendre", {
    expect_error(it.legendre(it.sample(), -1))
    expect_error(it.legendre(it.sample(), npoints = 0, npolynomials = 0))
    expect_error(it.legendre(it.sample(), npoints = -1, npolynomials = 1))
    expect_error(it.legendreSynth(1, NA))
    expect_error(it.legendreSynth(1, numeric(0)))
    expect_equal({sum(is.nan(it.legendre(it.sample(), 0)))}, 4)
    expect_equal({is.nan(it.legendre(it.sample(), npoints = 0, npolynomials = 1))}, TRUE)
    expect_equal({it.legendre(list(tmin=0, tmax=0.4, t=c(0, 0.1, 0.2, 0.3, 0.4), i=c(1, 2, 3, 6, -1)))}, c(2.7472472, 0.8711174, -2.2633733, -2.4655033))
    expect_equal({it.legendre(list(tmin=0, tmax=0.4, t=c(0, 0.1, 0.2, 0.3, 0.4), i=c(1, 2, 3, 6, -1)), npolynomials = 1)}, 2.7472472472472)
    expect_equal({it.legendre(list(tmin=0, tmax=0.4, t=c(0, 0.1, 0.2, 0.3, 0.4), i=c(1, 2, 3, 6, -1)), npoints = 2)}, c(0, -3,  0, -7))
    expect_equal({length(it.legendreSynth(5, 0))}, 0)
    expect_equal({it.legendreSynth(5, 1)}, 5)
    expect_equal({it.legendreSynth(5, 3)}, c(5, 5, 5))
    expect_equal({it.legendreSynth(c(1, 2, 3), 1)}, 2)
    expect_equal({it.legendreSynth(c(1, 2, 3), 2)}, c(2, 6))
    expect_equal({it.legendreSynth(c(1, 2, 3), 5)}, c(2, -0.375, -0.5, 1.625, 6))
})

test_that("it.cut", {
    expect_error(it.cut(it.sample(), numeric(0)))
    expect_error(it.cut(it.sample(), NA))
    expect_equal({
        it <- it.cut(it.sample(),  tStart = 0.3)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[10], it$t[245], it$i[1], it$i[10], it$i[245])},
        c(0.3, 3.6171250, 245, 245, 0.3085625, 0.4285625, 3.5618958, 77.5888192, 79.3352420, 39.5779048))

    expect_equal({
        it <- it.cut(it.sample(),  tStart = .2, tEnd = .3)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[4], it$t[8], it$i[1], it$i[4], it$i[8])},
        c(0.2, 0.3, 8, 8, 0.2018958, 0.2418958, 0.2952292, 77.9335129, 58.7607129, 73.6365900))

    expect_equal({
        it <- it.cut(it.sample(),  tEnd = 1)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[35], it$t[71], it$i[1], it$i[35], it$i[71])},
        c(0, 1, 71, 71, 0.05522917, 0.50856250, 0.98856250, 40.85635685, 62.63965636, 67.69785165))

    expect_equal({
        it <- it.cut(it.sample(),  tStart = -1, tEnd = 1)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[35], it$t[71], it$i[1], it$i[35], it$i[71])},
        c(-1, 1, 71, 71, 0.05522917, 0.50856250, 0.98856250, 40.85635685, 62.63965636, 67.69785165))
    expect_error(it.cut(it.sample(), 0.3, 0.2))
})

test_that("it.cut0", {
    expect_error(it.cut0(it.sample(), numeric(0)))
    expect_error(it.cut0(it.sample(), NA))
    expect_equal({
        it <- it.cut0(it.sample(),  tStart = 0.3)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[10], it$t[245], it$i[1], it$i[10], it$i[245])},
        c(0, 3.3171250, 245, 245, 0.0085625, 0.1285625, 3.2618958, 77.5888192, 79.3352420, 39.5779048))

    expect_equal({
        it <- it.cut0(it.sample(),  tStart = .2, tEnd = .3)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[4], it$t[8], it$i[1], it$i[4], it$i[8])},
        c(0, 0.1, 8, 8, 0.001895833, 0.041895833, 0.095229167, 77.933512921, 58.760712936, 73.636590015))

    expect_equal({
        it <- it.cut0(it.sample(),  tEnd = 1)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[35], it$t[71], it$i[1], it$i[35], it$i[71])},
        c(0, 1, 71, 71, 0.05522917, 0.50856250, 0.98856250, 40.85635685, 62.63965636, 67.69785165))

    expect_equal({
        it <- it.cut0(it.sample(),  tStart = -1, tEnd = 1)
        c(it$tmin, it$tmax, length(it$t), length(it$i), it$t[1], it$t[35], it$t[71], it$i[1], it$i[35], it$i[71])},
        c(0, 2, 71, 71, 1.055229, 1.508563, 1.988563, 40.856357, 62.639656, 67.697852))
    expect_error(it.cut0(it.sample(), 0.3, 0.2))
})


context("Sound")

test_that("snd.read", {
    expect_equal({
        snd <- snd.read("H.wav")
        c(length(snd),
          length(snd$t),
          snd$t[1],
          snd$t[2],
          snd$t[28937],
          snd$fs,
          snd$nChannels,
          snd$nBits,
          snd$nSamples,
          snd$duration,
          dim(snd$sig),
          snd$sig[1, 1],
          snd$sig[2, 1],
          snd$sig[1000, 1],
          snd$sig[28937, 1])
    }, c(7, 28937, 0, 1/8000, 3.617, 8000, 1, 16, 28937, 3.617125, 28937, 1,
         3.05185094759972e-05, -6.10370189519944e-05, -3.35703604235969e-03, -1.34281441694388e-03))

    expect_equal({
        snd2 <- snd.read("H.wav", from = 0.5, units = "seconds")
        c(snd2$t[1], snd2$t[2], snd2$t[3], snd2$t[24935], snd2$t[24936], snd2$t[24937])
    }, c(0.5, 0.500125, 0.50025, 3.61675, 3.616875, 3.617))

    expect_equal({
        snd2 <- snd.read("H.wav", from = 4001, units = "samples")
        c(snd2$t[1], snd2$t[2], snd2$t[3], snd2$t[24935], snd2$t[24936], snd2$t[24937])
    }, c(0.5, 0.500125, 0.50025, 3.61675, 3.616875, 3.617))
})

test_that("snd.write", {
    expect_equal({
        snd <- snd.read("H.wav")
        f <- tempfile()
        snd.write(snd, f)
        snd2 <- as.snd(snd.read(f), "H.wav")
        unlink(f)
        identical(snd, snd2)
    }, TRUE)
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

test_that("tg.write", {
    expect_equal({
        tg <- tg.read("2pr.TextGrid")
        f <- tempfile()
        tg.write(tg, f, format = "text")
        tg2 <- as.tg(tg.read(f), "2pr.TextGrid")
        unlink(f)
        identical(tg, tg2)
    }, TRUE)
    expect_equal({
        tg <- tg.createNewTextGrid(0, 3)
        tg <- tg.insertNewIntervalTier(tg, 1, "word")
        tg <- tg.insertInterval(tg, 1, 0.8, 1.5, "s\u0105\u0123")
        f <- tempfile()
        tg.write(tg, f, format = "text")
        tg2 <- tg.read(f)
        unlink(f)
        tg2[[1]]$label[2] == "s\u0105\u0123"
    }, TRUE)
    expect_equal({
        tg <- tg.read("2pr.TextGrid")
        f <- tempfile()
        tg.write(tg, f, format = "short")
        tg2 <- as.tg(tg.read(f), "2pr.TextGrid")
        unlink(f)
        identical(tg, tg2)
    }, TRUE)
})


test_that("tg.repairContinuity", {
    expect_equal({
        tg <- tg.repairContinuity(tg.sampleProblem(), verbose = FALSE)
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
    expect_equal(
        unlist(stringr::str_split('-a--a-', stringr::coll('-'))),
        c("", "a", "", "a", "")
    )

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

test_that("tg.cut", {
    expect_error({tg.cut(tg.sample(), 3, 2)})

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("0.5-4p.TextGrid")
        tg2 <- as.tg(tg.cut(tg, 0.5, 4), "0.5-4p.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("1.25-3.75p.TextGrid")
        tg2 <- as.tg(tg.cut(tg, 1.25, 3.75), "1.25-3.75p.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("1.5-3.5p.TextGrid")
        tg2 <- as.tg(tg.cut(tg, 1.5, 3.5), "1.5-3.5p.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("1-4p.TextGrid")
        tg2 <- as.tg(tg.cut(tg, 1, 4), "1-4p.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("-1-6p.TextGrid")
        tg2 <- as.tg(tg.cut(tg, -1, 6), "-1-6p.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("0-3p.TextGrid")
        tg2 <- as.tg(tg.cut(tg, tEnd = 3), "0-3p.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("3-5p.TextGrid")
        tg2 <- as.tg(tg.cut(tg, tStart = 3), "3-5p.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg2 <- tg.cut(tg, tEnd = -1)
        c(tg.getStartTime(tg2), tg.getEndTime(tg2))
    }, c(-1, -1))

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg2 <- tg.cut(tg, tStart = 6)
        c(tg.getStartTime(tg2), tg.getEndTime(tg2))
    }, c(6, 6))

})

test_that("tg.cut0", {
    expect_error({tg.cut0(tg.sample(), 3, 2)})

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("0.5-4.TextGrid")
        tg2 <- as.tg(tg.cut0(tg, 0.5, 4), "0.5-4.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("1.25-3.75.TextGrid")
        tg2 <- as.tg(tg.cut0(tg, 1.25, 3.75), "1.25-3.75.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("1.5-3.5.TextGrid")
        tg2 <- as.tg(tg.cut0(tg, 1.5, 3.5), "1.5-3.5.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("1-4.TextGrid")
        tg2 <- as.tg(tg.cut0(tg, 1, 4), "1-4.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("-1-6.TextGrid")
        tg2 <- as.tg(tg.cut0(tg, -1, 6), "-1-6.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("0-3.TextGrid")
        tg2 <- as.tg(tg.cut0(tg, tEnd = 3), "0-3.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg1 <- tg.read("3-5.TextGrid")
        tg2 <- as.tg(tg.cut0(tg, tStart = 3), "3-5.TextGrid")
        identical(tg1, tg2)
    }, TRUE)

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg2 <- tg.cut0(tg, tEnd = -1)
        c(tg.getStartTime(tg2), tg.getEndTime(tg2))
    }, c(0, 0))

    expect_equal({
        tg <- tg.read("cut.TextGrid")
        tg2 <- tg.cut0(tg, tStart = 6)
        c(tg.getStartTime(tg2), tg.getEndTime(tg2))
    }, c(0, 0))

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
        p2 <- as.pitch(pitch.read("sound_short.Pitch"), "sound.Pitch")
        p3 <- as.pitch(pitch.read("sound_UTF16.Pitch", encoding = "UTF-16"), "sound.Pitch")
        c(identical(p, p2), identical(p, p3))
    }, c(TRUE, TRUE))
})

test_that("pitch.toArray", {
    expect_equal({
        p <- pitch.sample()
        p$nx <- 3
        p$t <- p$t[c(1, 4, 10)]
        p$frame <- p$frame[c(1, 4, 10)]
        pa <- pitch.toArray(p)
        c(pa$xmin, pa$xmax, pa$nx, pa$dx, pa$x1, pa$t, pa$ceiling, pa$maxnCandidates,
          dim(pa$frequencyArray), pa$frequencyArray[1,1], is.na(pa$frequencyArray[6, 1]), pa$frequencyArray[3, 3],
          dim(pa$strengthArray), pa$strengthArray[1,1], is.na(pa$strengthArray[6, 1]), pa$strengthArray[3, 3])
    },
    c(0, 3.617125, 3, 0.01, 0.0235625, 0.0235625, 0.0535625, 0.1135625, 600, 15, 15, 3, 0, 1, 109.982215319168006,
      15, 3, 0, 1, 0.927799045152899)
    )
})

test_that("pitch.toFrame", {
    expect_equal({
        p <- pitch.sample()
        pa <- pitch.toArray(p)
        p2 <- pitch.toFrame(pa)
        identical(p, p2)
    }, TRUE)
})


context("Formant")

test_that("formant.read", {
    expect_equal({
        f <- formant.read("maminka.Formant")
        c(f$xmin, f$xmax, f$nx, f$dx, f$x1, length(f$t), f$t[1], f$t[2], f$t[80], f$maxnFormants, length(f$frame), f$frame[[4]]$intensity,
          f$frame[[4]]$nFormants, length(f$frame[[4]]$frequency), length(f$frame[[4]]$bandwidth), f$frame[[4]]$frequency[1], f$frame[[4]]$frequency[2],
          f$frame[[4]]$frequency[3], f$frame[[4]]$frequency[4], f$frame[[4]]$frequency[5], f$frame[[4]]$bandwidth[1], f$frame[[4]]$bandwidth[2], f$frame[[4]]$bandwidth[3],
          f$frame[[4]]$bandwidth[4], f$frame[[4]]$bandwidth[5], f$frame[[80]]$intensity, f$frame[[80]]$nFormants, length(f$frame[[80]]$frequency), length(f$frame[[80]]$bandwidth),
          f$frame[[80]]$frequency[1], f$frame[[80]]$frequency[2], f$frame[[80]]$frequency[3], f$frame[[80]]$frequency[4],
          f$frame[[80]]$bandwidth[1], f$frame[[80]]$bandwidth[2], f$frame[[80]]$bandwidth[3], f$frame[[80]]$bandwidth[4])
    }, c(0, 0.5460770975056689, 80, 0.00625, 0.026163548752834397, 80, 0.026163548752834397, 0.032413548752834397, 0.5199135487528345, 5, 80, 1.033142541089274e-005,
         5, 5, 5, 192.48696491636466, 1479.2446721696026, 2883.3496059581475, 3969.3756273121708, 5231.531927706885, 234.84667765707806, 295.1069753187278,
         160.23124588560367, 452.2355186981254, 1242.9009292690093, 0.0017737676385123245, 4, 4, 4, 601.9084470780567, 1790.5091315894167,
         2896.3679369463307, 4329.2993398040635, 147.158951399607, 272.26394394370794, 723.7529213211043, 361.80775918364697))
    expect_equal({
        f <- formant.read("maminka.Formant")
        f2 <- as.formant(formant.read("maminka_short.Formant"), "maminka.Formant")
        f3 <- as.formant(formant.read("maminka_UTF16.Formant", encoding = "UTF-16"), "maminka.Formant")
        c(identical(f, f2), identical(f, f3))
    }, c(TRUE, TRUE))
})

test_that("formant.toArray", {
    expect_equal({
        f <- formant.sample()
        f$nx <- 3
        f$t <- f$t[c(1, 261, 571)]
        f$frame <- f$frame[c(1, 261, 571)]
        fa <- formant.toArray(f)
        c(fa$xmin, fa$xmax, fa$nx, fa$dx, fa$x1, fa$t, fa$maxnFormants,
          dim(fa$frequencyArray), fa$frequencyArray[1,1], is.na(fa$frequencyArray[5, 2]), fa$frequencyArray[5, 3],
          dim(fa$bandwidthArray), fa$bandwidthArray[1,1], is.na(fa$bandwidthArray[5, 2]), fa$bandwidthArray[5, 3])
        },
        c(0, 3.617125, 3, 6.25e-03, 2.73e-02, 2.73e-02, 1.6523,3.5898, 5, 5, 3, 3.399e+02, 1, 3.7346e+03,
          5, 3, 2.15e+01, 1, 7.83e+01)
    )
})

test_that("formant.toFrame", {
    expect_equal({
        f <- formant.sample()
        fa <- formant.toArray(f)
        f2 <- formant.toFrame(fa)
        identical(f, f2)
    }, TRUE)
})

test_that("normIntensity", {
    expect_equal({
        normIntensity(-3:3, 1, 9)
    }, c(NaN, NaN, NaN, NaN, 1, 6.047438028572, 9))
})


context("Collection")

test_that("col.read", {
    expect_equal({
        c1 <- col.read("coll_short.Collection")
        it <- it.read("1.IntensityTier")
        pitch <- pitch.read("sound.Pitch")
        formant <- formant.read("maminka.Formant")
        pt <- pt.read("H.PitchTier")
        tg <- tg.read("HC101bA.TextGrid")
        c(length(c1), class(c1[[1]])[["type"]], class(c1[[1]])[["name"]], class(c1[[2]])[["type"]], class(c1[[2]])[["name"]],
          class(c1[[3]])[["type"]], class(c1[[3]])[["name"]], class(c1[[4]])[["type"]], class(c1[[4]])[["name"]],
          class(c1[[5]])[["type"]], class(c1[[5]])[["name"]],
          identical(length(c1[[1]]), length(it)), identical(c1[[1]]$t, it$t), identical(c1[[1]]$i, it$i), identical(c1[[1]]$tmin, it$tmin), identical(c1[[1]]$tmax, it$tmax),
          identical(length(c1[[2]]), length(tg)), identical(c1[[2]]$phone, tg$phone), identical(c1[[2]]$word, tg$word), identical(c1[[2]]$points, tg$points), identical(c1[[2]]$phrase, tg$phrase),
          identical(length(c1[[3]]), length(pitch)), identical(c1[[3]]$xmin, pitch$xmin), identical(c1[[3]]$xmax, pitch$xmax), identical(c1[[3]]$nx, pitch$nx), identical(c1[[3]]$dx, pitch$dx),
                   identical(c1[[3]]$x1, pitch$x1), identical(c1[[3]]$t, pitch$t), identical(c1[[3]]$ceiling, pitch$ceiling), identical(c1[[3]]$maxnCandidates, pitch$maxnCandidates), identical(c1[[3]]$frame, pitch$frame),
          identical(length(c1[[4]]), length(pt)), identical(c1[[4]]$t, pt$t), identical(c1[[4]]$f, pt$f), identical(c1[[4]]$tmin, pt$tmin), identical(c1[[4]]$tmax, pt$tmax),
          identical(c1[[5]]$x1, formant$x1), identical(c1[[5]]$t, formant$t), identical(c1[[5]]$maxnFormants, formant$maxnFormants), identical(c1[[5]]$frame, formant$frame)
          )
    }, c("5", "IntensityTier", "1", "TextGrid", "HC101bA", "Pitch 1", "sound_short", "PitchTier", "H_shortTextFile", "Formant 2", "maminka",
         rep("TRUE", 29)))
    expect_equal({
        c1 <- col.read("coll_short.Collection")
        c2 <- col.read("coll_text.Collection")
        c3 <- col.read("coll_text_UTF16.Collection", encoding = "UTF-16")
        c(identical(c1, c2),
          identical(c2, c3))
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

