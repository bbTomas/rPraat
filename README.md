rPraat package for R
====================

rPraat package for R constitutes an interface between the most popular
software for phonetic analyses, Praat, and R.

Version 1.1.3-4.

For demonstration, see
[html/rDemo.html](http://htmlpreview.github.com/?https://github.com/bbTomas/rPraat/blob/master/html/rDemo.html)

Package documentation
<https://github.com/bbTomas/rPraat/blob/master/rPraat.pdf>

CRAN link: <https://CRAN.R-project.org/package=rPraat>

See also: [mPraat toolbox](https://github.com/bbTomas/mPraat) for
Matlab.

rPraat and mPraat homepage: <http://fu.ff.cuni.cz/praat/>

Installation
------------

One-time installation (internet connection needed).

#### CRAN: official version

    install.packages("rPraat")

#### GitHub: experimental, most recent version

    install.packages("devtools")
    devtools::install_github("bbTomas/rPraat")

### Initialize and Test

At each R session, you have to initialize the package (no internet
needed).

    library(rPraat)
    library(dplyr)   # optional, if you want to use the pipeline operator %>%

    tg.sample() %>% tg.plot()  # demo test

How to cite this package?
-------------------------

We have invested a lot of time and effort in creating mPraat toolbox /
rPraat package, please cite it when using it for data analysis.

> Bořil, T., & Skarnitzl, R. (2016). Tools rPraat and mPraat. In P.
> Sojka, A. Horák, I. Kopeček, & K. Pala (Eds.), Text, Speech, and
> Dialogue (pp. 367–374). Springer International Publishing.

[Download Tools rPraat and mPraat manuscript
.pdf](http://fu.ff.cuni.cz/praat/boril_skarnitzl_2016_Tools_rPraat_and_mPraat_%5Bmanuscript%5D.pdf)

The final publication is available at Springer via [DOI
10.1007/978-3-319-45510-5\_42](http://dx.doi.org/10.1007/978-3-319-45510-5_42)

[Download BibTeX reference
mpraatrpraat2016.bib](http://fu.ff.cuni.cz/praat/mpraatrpraat2016.bib)

What is new
-----------

v1.1.3-4 (2018/12/03): snd.read(), snd.write(), snd.plot(); tg.plot()
can plot TextGrid together with Sound object; it.plot() can plot
IntensityTier together with Sound object

v1.1.3-3 (2018/12/03): formant.toFrame(), pitch.toArray(),
pitch.toFrame(), pitch.plot(); tg.plot() can plot TextGrid together with
Pitch object; pitch.plot() can plot Pitch object together with PitchTier

v1.1.3-2 (2018/10/29): formant.plot(), formant.toArray(); tg.plot() can
plot TextGrid together with Formant object

v1.1.3-1 (2018/10/26): tg.plot() can plot TextGrid together with
PitchTier and IntensityTier in one plot

v1.1.3 (2018/10/20): formant.read(), formant.sample(), as.formant();
col.read() can read formant objects

v1.1.2-1 (2018/10/20): tg.plot(): right-aligned labels; pitch.sample(),
as.tg(), as.pt(), as.it(), as.pitch(); .read(), .sample() and
tg.createNewTextGrid(): class(object)\["type"\] set to "TextGrid",
"PitchTier" etc. and class(object)\["name"\] set according to fileName

v1.1.2 (2018/08/04): tg.cut(), tg.cut0()

v1.1.1 (2018/08/02): IntensityTiers: it.read(), it.write(), it.plot(),
it.sample(), it.interpolate(), it.cut(), it.cut0(), it.legendre(),
it.legendreDemo(), it.legendreSynth(), col.read() supports
IntensityTiers

v1.1.0 (2018/08/01): col.read() to read Collection in Text and
Short-text format (Collection may store many objects in one file –
TextGrids, PitchTiers, Pitch objects). Big thanks to Pol van Rijn for
initiating this and making the major portion of the new code in mPraat
toolbox. Supports for file encodings in all read functions (thanks to
Weirong Chen for his DetectEncoding idea in mPraat toolbox).

v1.0.8-3 (2017/11/27): tg.write(): fix for possible UTF-8 encoding and
Windows OS locale mismatch

v1.0.8-2 (2017/11/04): tg.write() and pt.write() format support: "text"
(full text format), "short" (short text format), and in addition for pt:
"spreadsheet" and "headerless"; tg.read(), pt.read() and pitch.read():
optional file encoding parameter (default: "UTF-8")

v1.0.8-1 (2017/09/01): bugfix, short-text TextGrid with multiple lines
in a label no longer produces "missing sppasFormat variable" error

v1.0.8 (2017/07/16): pitch.read() to read Pitch-object files with time
frames of pitch candidates

v1.0.7 (2017/04/19): tg.findLabels(), tg.duplicateTierMergeSegments()

v1.0.6 (2017/04/11): pt.cut(), pt.cut0()

v1.0.5 (2017/04/10): pt.legendre(), pt.legendreSynth(),
pt.legendreDemo()

v1.0.4 (2017/03/06): pt.interpolate(), pt.Hz2ST()

License
-------

License: MIT

Copyright (c) 2016 Tomáš Bořil, <borilt@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
