v1.2.0 (2018/12/08)
-------------------
- pitch.write(), formant.write(), col.write()

v1.1.4 (2018/12/08)
-------------------
- pitch.cut(), formant.cut(), snd.cut()
- pitch.cut0(), formant.cut0(), snd.cut0()

v1.1.3-4 (2018/12/03)
---------------------
- snd.read(), snd.write(), snd.plot(), as.snd()
- tg.plot() can plot TextGrid together with Sound object
- it.plot() can plot IntensityTier together with Sound object

v1.1.3-3 (2018/12/03)
---------------------

- formant.toFrame(), pitch.toArray(), pitch.toFrame()
- pitch.plot()
- tg.plot() can plot TextGrid together with Pitch object
- pitch.plot() can plot Pitch object together with PitchTier

v1.1.3-2 (2018/10/29)
---------------------
- formant.plot(), formant.toArray()
- tg.plot() can plot TextGrid together with Formant object

v1.1.3-1 (2018/10/26)
---------------------
- tg.plot() can plot TextGrid together with PitchTier and IntensityTier in one plot

v1.1.3 (2018/10/20)
-------------------
- formant.read(), formant.sample(), as.formant()
- col.read() can read formant objects

v1.1.2-1 (2018/10/20)
---------------------
- tg.plot(): right-aligned labels
- pitch.sample(), as.tg(), as.pt(), as.it(), as.pitch()
- .read(), .sample() and tg.createNewTextGrid(): class(object)["type"] set to "TextGrid", "PitchTier" etc. and class(object)["name"] set according to fileName

v1.1.2 (2018/08/04)
-------------------
- tg.cut(), tg.cut0()

v1.1.1 (2018/08/02)
-------------------
- IntensityTiers: it.read(), it.write(), it.plot(), it.sample(), it.interpolate(), it.cut(), it.cut0(), it.legendre(), it.legendreDemo(), it.legendreSynth()
- col.read() supports IntensityTiers

v1.1.0 (2018/08/01)
-------------------
- col.read() to read Collection in Text and Short-text format (Collection may store many objects in one file: TextGrids, PitchTiers, Pitch objects)

v1.0.8-3 (2017/11/27)
---------------------
- tg.write(): fix for possible UTF-8 encoding and Windows OS locale mismatch

v1.0.8-2 (2017/11/04)
---------------------
- tg.write() and pt.write() format support: "text" (full text format), "short" (short text format)
- and in addition for pt: "spreadsheet" and "headerless"
- tg.read(), pt.read() and pitch.read(): optional file encoding parameter (default: "UTF-8")

v1.0.8-1 (2017/09/01)
---------------------
- bugfix, short-text TextGrid with multiple lines in a label no longer produces "missing sppasFormat variable" error

v1.0.8 (2017/07/16)
-------------------
- pitch.read() to read Pitch-object files with time frames of pitch candidates

v1.0.7 (2017/04/19)
-------------------
- tg.findLabels(), tg.duplicateTierMergeSegments()

v1.0.6 (2017/04/11)
-------------------
- pt.cut(), pt.cut0()

v1.0.5 (2017/04/10)
-------------------
- pt.legendre(), pt.legendreSynth(), pt.legendreDemo()

v1.0.4 (2017/03/06)
-------------------
- pt.interpolate(), pt.Hz2ST()
