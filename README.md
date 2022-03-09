# medicarestars

The R package `medicarestars` contains data collected from CMS's [Part C&D Performance Data](https://www.cms.gov/Medicare/Prescription-Drug-Coverage/PrescriptionDrugCovGenIn/PerformanceData) page, along with functions to compute cut points and star ratings using that data.

## Install

```
devtools::install_github("jimurick/medicarestars")
```

## Tables

This package includes the following tables:

* `contract_df`: Medicare Part C&D Contracts
* `measure_df`: Measures
* `measure_score_df`: Measure-Level Values and Star Ratings
* `measure_star_range_df`: Measure-Level Star Ranges
* `cut_point_df`: Cut Points
* `domain_stars_df`: Domain-Level Star Ratings
* `summary_stars_df`: Summary Star Ratings
* `cai_df`: Categorical Adjustment Index (CAI)
* `disenrollment_df`: Disenrollment Data (starting 2020)
