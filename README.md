## An introductory statistics course for geologists

### Graeme T. Lloyd

## Introduction

This repository contains materials (lectures, practicals and formative and summative assessments) for teaching introductory statistics to geologists. Aside from basic "standard" statistics (univariate, bivarate, multivariate) the reposotiory also includes some more specific to geology that might be less relevant to other sciences (e.g., circular statistics, plotting stereonets).

## License

These materials are released under a GNU General Public License Version 3 (see LICENSE for details) with the exception of material reused from other sources. Specifically:

* Volcano data ([NOAA Volcano Location Database](https://data.noaa.gov/dataset/dataset/global-volcano-locations-database))
* Meteor impact ages ([Earth Impact Database](http://www.passc.net/EarthImpactDatabase/New%20website_05-2018/Index.html))
* Dinosaur body mass data ([Dryad repositiory for Benson et al. 2018](https://datadryad.org/stash/dataset/doi:10.5061/dryad.1t3r4))

## Format

The course asssumes a standard structure of Lecture + Practical + Formative Multiple Choice Questions (MCQ; with Model Answers), with eight lectures total. However, an additional opening practical and MCQ also cover a basic introduction to the programming language R and the Integrated Development Environment (IDE) ([RStudio](https://rstudio.com/)) - the freely available, multiplatform software on which the practicals are based (and which students can easily install on their personal machines). The practicals use a variety of geologic example data sets, but are primarily based around an ammonite quadrat (see below) which serves as a training data set for the summative assessment (writing a scientific paper-style report on a different ammonite quadrat from higher in the saem geologic sequence).

## Ammonite quadrat

In order to create a sense of individual ownership over a data set that could also be used as the basis for a large range of statistical tests a series of ammonite quadrats were generated. The students are told these come from a large wave cut platform representing a Jurassic sea bed. A large ten-by-ten metre quadrat is laid out across this platform and further subdivided into one hundred one-by-one metre quadrats individually lebelled with a letter and a number with each student assigned one of these smaller data sets. Each individual data set is unique, in part to avoid plagarism. In reality this data is simulated such that each quadrat contains exactly twenty ammonites (each student has the same number of measurements to deal with), where ammonites are assigned to a quadrat based on the position of their central point (hence most quadrats will contain ammonites that extend outside their one-by-one metre grid cell).

Below is an example quadrat.

![An example ammonite quadrat.](AmmoniteQuadrat.png)


The students will iteratively learn what measurements they can take from these quadrats

Binoimal based on whic side they lie on (left or right).

When printed they will not be one-to-one (unless you want to spend a lot of money!) Hence also scaled rulers and protractors are provided that are intended to be printed onto acetate or other transparent material.

## References

Benson, R. B. J., Hunt, G., Carrano, M. T. and Campione, N., 2018. Cope's rule and the adaptive landscape of dinosaur body size evolution. *Palaeontology*, **61**, 13-48.


Lukeneder, S., Lukeneder, A. and Weber, G. W., 2014. Computed reconstruction of spatial ammonoid-shell orientation captured from digitized grinding and landmark data. *Computers & Geosciences*, **64**, 104-114.
