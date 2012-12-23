comparative-analysis for source apportionment
====================

Comparative analysis of environmental data from source apportionment

This project is about comparative analysis of environmental data coming out from source apportionment analysis.

The comparison is made between the results found by several laboratories that carried out source apportionment on a common database.

The output of source apportionment runs are:
- factor profiles (pollutant factors)
- uncertainties of factor profiles
- time trends of factor profiles
- factor contributions to a given chemical species

Data from each laboratory are loaded and classified according to given categories
which are chosen among the main pollutant sources .
The R code: CLASSIFY.R acts as follow:
- loads data,
- makes mathematical transformations for comparative analysis purposes, group factor profiles, 
- chooses and groups factor profiles into chosen categories
- extract the Source Contribution Estimations (and their uncertainties).

##################################################################################################################

Comparative analysis begins with performing Pearson correlation classified data.
Correlation is then carried on all possible pairs of factor profiles found by each laboratory.
Also correlation with a reference source profile is performed.
The R code: CORR.R acts as follow:
- loads data
- makes all possible correlations between factor profiles
- makes correlations between factor and reference source profiles.
- makes boxplots for correlation between factor profiles
- makes matplots for correlation between factor and reference source profiles.



