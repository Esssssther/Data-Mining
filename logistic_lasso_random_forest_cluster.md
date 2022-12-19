logistic_lasso_random_forest_cluster
================
Shi Shi

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
options(scipen=999)
library(writexl)
library(C50)
library(tidyverse)
library(tidymodels)
library(janitor)
library(skimr)
library(kableExtra)
library(GGally)
library(kableExtra) # -- make nice looking resutls when we knitt 
library(vip)        # --  tidymodels variable importance
library(fastshap)   # -- shapley values for variable importance 
library(MASS)
library(rpart.plot) # -- plotting decision trees 
library(factoextra)
library(imputeMissings)
library(ISLR)
library(tree)
library(lubridate) 
library(ggplot2)
library(reshape2)
library(Hmisc)
library(DALEX)    # new 
library(DALEXtra) # new
```

``` r
domerge<-read_csv("DonorMerge_Final.csv") %>% clean_names()
```

    ## Rows: 328018 Columns: 40
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (17): projectid, teacher_acctid, schoolid, school_city, school_state, sc...
    ## dbl (11): school_ncesid, school_latitude, school_longitude, school_zip, grea...
    ## lgl (12): is_exciting, one_non_teacher_referred_donor_g, school_charter, sch...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(domerge)
```

    ## # A tibble: 6 × 40
    ##   projectid     teach…¹ schoo…² school…³ schoo…⁴ schoo…⁵ schoo…⁶ schoo…⁷ schoo…⁸
    ##   <chr>         <chr>   <chr>      <dbl>   <dbl>   <dbl> <chr>   <chr>     <dbl>
    ## 1 1a3aaeffc56d… f4c9ed… 9310d3…  3.3 e11    42.9   -71.3 Derry   NH         3038
    ## 2 33aa19ee4da4… 177680… 9ac70d…  5.1 e11    37.5   -77.5 Richmo… VA        23224
    ## 3 e31c0ea8b68f… 0f1bc5… cb9f68…  1.71e11    42.0   -87.7 Chicago IL        60613
    ## 4 c685c844476d… 4b1950… 34c650… NA          34.2   -79.8 Floren… SC        29501
    ## 5 a4b234feb2b7… 620982… da1985…  2.1 e11    38.2   -85.7 Louisv… KY        40206
    ## 6 0ff5dec32bf7… ec5b11… 72e2b0…  1.2 e11    30.5   -86.1 Freepo… FL        32439
    ## # … with 31 more variables: school_metro <chr>, school_district <chr>,
    ## #   is_exciting <lgl>, one_non_teacher_referred_donor_g <lgl>,
    ## #   great_messages_proportion <dbl>, teacher_referred_count <dbl>,
    ## #   non_teacher_referred_count <dbl>, school_county <chr>,
    ## #   school_charter <lgl>, school_magnet <lgl>, school_year_round <lgl>,
    ## #   school_nlns <lgl>, school_kipp <lgl>, school_charter_ready_promise <lgl>,
    ## #   teacher_prefix <chr>, teacher_teach_for_america <lgl>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
skim(domerge)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
domerge
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
328018
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
logical
</td>
<td style="text-align:left;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: character**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
empty
</th>
<th style="text-align:right;">
n_unique
</th>
<th style="text-align:right;">
whitespace
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
projectid
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
328018
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_acctid
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
160300
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
schoolid
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
46190
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_city
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7820
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_state
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_metro
</td>
<td style="text-align:right;">
40019
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_district
</td>
<td style="text-align:right;">
501
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7960
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_county
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1625
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_prefix
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
secondary_focus_subject
</td>
<td style="text-align:right;">
103265
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
secondary_focus_area
</td>
<td style="text-align:right;">
103265
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
poverty_level
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
date_posted
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3815
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

**Variable type: logical**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:left;">
count
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
is_exciting
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:left;">
FAL: 291308, TRU: 36710
</td>
</tr>
<tr>
<td style="text-align:left;">
one_non_teacher_referred_donor_g
</td>
<td style="text-align:right;">
47338
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:left;">
TRU: 202773, FAL: 77907
</td>
</tr>
<tr>
<td style="text-align:left;">
school_charter
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:left;">
FAL: 298820, TRU: 29198
</td>
</tr>
<tr>
<td style="text-align:left;">
school_magnet
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:left;">
FAL: 297022, TRU: 30996
</td>
</tr>
<tr>
<td style="text-align:left;">
school_year_round
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:left;">
FAL: 311592, TRU: 16426
</td>
</tr>
<tr>
<td style="text-align:left;">
school_nlns
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:left;">
FAL: 322847, TRU: 5171
</td>
</tr>
<tr>
<td style="text-align:left;">
school_kipp
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:left;">
FAL: 325684, TRU: 2334
</td>
</tr>
<tr>
<td style="text-align:left;">
school_charter_ready_promise
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:left;">
FAL: 326282, TRU: 1736
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_teach_for_america
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:left;">
FAL: 308069, TRU: 19949
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_ny_teaching_fellow
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:left;">
FAL: 322150, TRU: 5868
</td>
</tr>
<tr>
<td style="text-align:left;">
eligible_double_your_impact_matc
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:left;">
FAL: 237135, TRU: 90883
</td>
</tr>
<tr>
<td style="text-align:left;">
eligible_almost_home_match
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:left;">
FAL: 308053, TRU: 19965
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
<th style="text-align:left;">
hist
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
school_ncesid
</td>
<td style="text-align:right;">
21067
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
260675170815.24
</td>
<td style="text-align:right;">
159052207728.30
</td>
<td style="text-align:right;">
10000500870.00
</td>
<td style="text-align:right;">
90495001040.50
</td>
<td style="text-align:right;">
261000000000.00
</td>
<td style="text-align:right;">
370000000000.00
</td>
<td style="text-align:right;">
610000000000.00
</td>
<td style="text-align:left;">
▇▃▆▅▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_latitude
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
37.24
</td>
<td style="text-align:right;">
4.66
</td>
<td style="text-align:right;">
18.25
</td>
<td style="text-align:right;">
34.04
</td>
<td style="text-align:right;">
37.67
</td>
<td style="text-align:right;">
40.76
</td>
<td style="text-align:right;">
67.26
</td>
<td style="text-align:left;">
▁▇▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_longitude
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
-93.44
</td>
<td style="text-align:right;">
17.81
</td>
<td style="text-align:right;">
-171.69
</td>
<td style="text-align:right;">
-112.05
</td>
<td style="text-align:right;">
-87.69
</td>
<td style="text-align:right;">
-79.15
</td>
<td style="text-align:right;">
-66.63
</td>
<td style="text-align:left;">
▁▁▅▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
school_zip
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
53102.86
</td>
<td style="text-align:right;">
31232.05
</td>
<td style="text-align:right;">
410.00
</td>
<td style="text-align:right;">
27589.00
</td>
<td style="text-align:right;">
53089.00
</td>
<td style="text-align:right;">
85225.00
</td>
<td style="text-align:right;">
99950.00
</td>
<td style="text-align:left;">
▆▇▂▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
great_messages_proportion
</td>
<td style="text-align:right;">
89712
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
54.82
</td>
<td style="text-align:right;">
35.07
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
33.00
</td>
<td style="text-align:right;">
57.00
</td>
<td style="text-align:right;">
84.00
</td>
<td style="text-align:right;">
100.00
</td>
<td style="text-align:left;">
▆▃▆▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_referred_count
</td>
<td style="text-align:right;">
47338
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
125.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
non_teacher_referred_count
</td>
<td style="text-align:right;">
47338
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
4.58
</td>
<td style="text-align:right;">
5.91
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
304.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fulfillment_labor_materials
</td>
<td style="text-align:right;">
17492
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
27.62
</td>
<td style="text-align:right;">
8.81
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
17.00
</td>
<td style="text-align:right;">
30.00
</td>
<td style="text-align:right;">
35.00
</td>
<td style="text-align:right;">
35.00
</td>
<td style="text-align:left;">
▁▂▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
total_price_excluding_optional_s
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
551.09
</td>
<td style="text-align:right;">
18019.38
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
268.17
</td>
<td style="text-align:right;">
410.58
</td>
<td style="text-align:right;">
579.10
</td>
<td style="text-align:right;">
10250017.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
total_price_including_optional_s
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
656.19
</td>
<td style="text-align:right;">
21973.64
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
319.02
</td>
<td style="text-align:right;">
487.50
</td>
<td style="text-align:right;">
688.16
</td>
<td style="text-align:right;">
12500020.73
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
students_reached
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
99.60
</td>
<td style="text-align:right;">
2733.94
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
22.00
</td>
<td style="text-align:right;">
30.00
</td>
<td style="text-align:right;">
100.00
</td>
<td style="text-align:right;">
999999.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>

``` r
domerge<-domerge %>% dplyr::select(-c(projectid,teacher_acctid,schoolid,school_ncesid, secondary_focus_subject,secondary_focus_area,great_messages_proportion))

domerge$one_non_teacher_referred_donor_g <-ifelse(is.na(domerge$one_non_teacher_referred_donor_g)==TRUE,"unknown",domerge$one_non_teacher_referred_donor_g)

domerge$school_metro <-ifelse(is.na(domerge$school_metro)==TRUE,"unknown",domerge$school_metro)

domerge$primary_focus_subject <-ifelse(is.na(domerge$primary_focus_subject)==TRUE,"unknown",domerge$primary_focus_subject)

domerge$primary_focus_area <-ifelse(is.na(domerge$primary_focus_area)==TRUE,"unknown",domerge$primary_focus_area)

domerge$resource_type <-ifelse(is.na(domerge$resource_type)==TRUE,"unknown",domerge$resource_type)
domerge$grade_level <-ifelse(is.na(domerge$grade_level)==TRUE,"unknown",domerge$grade_level)

skim(domerge)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
domerge
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
328018
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
logical
</td>
<td style="text-align:left;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: character**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
empty
</th>
<th style="text-align:right;">
n_unique
</th>
<th style="text-align:right;">
whitespace
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
school_city
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7820
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_state
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_metro
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_district
</td>
<td style="text-align:right;">
501
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7960
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
one_non_teacher_referred_donor_g
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_county
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1625
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_prefix
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
poverty_level
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
date_posted
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3815
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

**Variable type: logical**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:left;">
count
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
is_exciting
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:left;">
FAL: 291308, TRU: 36710
</td>
</tr>
<tr>
<td style="text-align:left;">
school_charter
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:left;">
FAL: 298820, TRU: 29198
</td>
</tr>
<tr>
<td style="text-align:left;">
school_magnet
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:left;">
FAL: 297022, TRU: 30996
</td>
</tr>
<tr>
<td style="text-align:left;">
school_year_round
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:left;">
FAL: 311592, TRU: 16426
</td>
</tr>
<tr>
<td style="text-align:left;">
school_nlns
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:left;">
FAL: 322847, TRU: 5171
</td>
</tr>
<tr>
<td style="text-align:left;">
school_kipp
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:left;">
FAL: 325684, TRU: 2334
</td>
</tr>
<tr>
<td style="text-align:left;">
school_charter_ready_promise
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:left;">
FAL: 326282, TRU: 1736
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_teach_for_america
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:left;">
FAL: 308069, TRU: 19949
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_ny_teaching_fellow
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:left;">
FAL: 322150, TRU: 5868
</td>
</tr>
<tr>
<td style="text-align:left;">
eligible_double_your_impact_matc
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:left;">
FAL: 237135, TRU: 90883
</td>
</tr>
<tr>
<td style="text-align:left;">
eligible_almost_home_match
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:left;">
FAL: 308053, TRU: 19965
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
<th style="text-align:left;">
hist
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
school_latitude
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
37.24
</td>
<td style="text-align:right;">
4.66
</td>
<td style="text-align:right;">
18.25
</td>
<td style="text-align:right;">
34.04
</td>
<td style="text-align:right;">
37.67
</td>
<td style="text-align:right;">
40.76
</td>
<td style="text-align:right;">
67.26
</td>
<td style="text-align:left;">
▁▇▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_longitude
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
-93.44
</td>
<td style="text-align:right;">
17.81
</td>
<td style="text-align:right;">
-171.69
</td>
<td style="text-align:right;">
-112.05
</td>
<td style="text-align:right;">
-87.69
</td>
<td style="text-align:right;">
-79.15
</td>
<td style="text-align:right;">
-66.63
</td>
<td style="text-align:left;">
▁▁▅▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
school_zip
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
53102.86
</td>
<td style="text-align:right;">
31232.05
</td>
<td style="text-align:right;">
410.00
</td>
<td style="text-align:right;">
27589.00
</td>
<td style="text-align:right;">
53089.00
</td>
<td style="text-align:right;">
85225.00
</td>
<td style="text-align:right;">
99950.00
</td>
<td style="text-align:left;">
▆▇▂▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_referred_count
</td>
<td style="text-align:right;">
47338
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
125.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
non_teacher_referred_count
</td>
<td style="text-align:right;">
47338
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
4.58
</td>
<td style="text-align:right;">
5.91
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
304.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fulfillment_labor_materials
</td>
<td style="text-align:right;">
17492
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
27.62
</td>
<td style="text-align:right;">
8.81
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
17.00
</td>
<td style="text-align:right;">
30.00
</td>
<td style="text-align:right;">
35.00
</td>
<td style="text-align:right;">
35.00
</td>
<td style="text-align:left;">
▁▂▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
total_price_excluding_optional_s
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
551.09
</td>
<td style="text-align:right;">
18019.38
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
268.17
</td>
<td style="text-align:right;">
410.58
</td>
<td style="text-align:right;">
579.10
</td>
<td style="text-align:right;">
10250017.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
total_price_including_optional_s
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
656.19
</td>
<td style="text-align:right;">
21973.64
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
319.02
</td>
<td style="text-align:right;">
487.50
</td>
<td style="text-align:right;">
688.16
</td>
<td style="text-align:right;">
12500020.73
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
students_reached
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
99.60
</td>
<td style="text-align:right;">
2733.94
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
22.00
</td>
<td style="text-align:right;">
30.00
</td>
<td style="text-align:right;">
100.00
</td>
<td style="text-align:right;">
999999.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>

# explore target variables

``` r
domerge %>%
  count(is_exciting)%>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(x=is_exciting,y=pct))+
  geom_col()+
  labs(title = "exciting project distribution",y="percentage")+
  geom_text(aes(label=paste(round(pct*100,2),"%")),vjust=1.5,color="white")
```

![](final_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# explore categorical variables

``` r
categorical<- domerge%>% dplyr::select_if(is.character) %>% dplyr::select(-c(school_city,school_district,school_county,date_posted))
skim(categorical)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
categorical
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
328018
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: character**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
empty
</th>
<th style="text-align:right;">
n_unique
</th>
<th style="text-align:right;">
whitespace
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
school_state
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
school_metro
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
one_non_teacher_referred_donor_g
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_prefix
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
poverty_level
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
for (col in colnames(categorical)){
  print(
    domerge %>%
  ggplot(aes(x=factor(!!as.name(col)), fill=factor(is_exciting)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title=paste( col, "bar chart"),  x=col)
  )
}
```

![](final_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->
\# explore logical variables

``` r
logical<- domerge%>% dplyr::select_if(is.logical) %>% dplyr::select(-c(is_exciting))
for(col in colnames(logical)){
  print(
  domerge %>% 
  ggplot(aes(x=factor(!!as.name(col)), fill=factor(is_exciting)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title=paste( col, "bar chart"),  x=col)
  )
}
```

![](final_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->

# explore numeric variables

``` r
numeric<- domerge%>% dplyr::select_if(is.numeric) %>% dplyr::select(-c(school_zip))
for(col in colnames(numeric)){
  print(
  domerge %>% 
  ggplot(aes(x=is_exciting,y=!!as.name(col)))+
  geom_boxplot()+
  labs(title=paste(col,"box plot"),  y=col, x="is_exciting"))
}
```

![](final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](final_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->

``` r
for(col in colnames(numeric)){
  print(
  domerge %>% 
  ggplot(aes(x=!!as.name(col),fill=is_exciting))+
  geom_histogram(position = "fill")+
  labs(title=paste(col,"histogram plot"),  x=col, y="frequency"))
}
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-13.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-14.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-15.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-7-16.png)<!-- --> \#recipe

``` r
domerge1<-domerge%>% dplyr::select(-c(school_city, school_district, school_county,date_posted,school_zip,school_state))

domerge1$is_exciting=as.factor(domerge1$is_exciting)
domerge1$school_charter=as.integer(domerge1$school_charter)
domerge1$school_magnet=as.integer(domerge1$school_magnet)  
domerge1$school_year_round=as.integer(domerge1$school_year_round)  
domerge1$school_nlns=as.integer(domerge1$school_nlns)  
domerge1$school_kipp=as.integer(domerge1$school_kipp)  
domerge1$school_charter_ready_promise=as.integer(domerge1$school_charter_ready_promise)  
domerge1$teacher_teach_for_america=as.integer(domerge1$teacher_teach_for_america)  
domerge1$teacher_ny_teaching_fellow=as.integer(domerge1$teacher_ny_teaching_fellow) 
domerge1$eligible_double_your_impact_matc=as.integer(domerge1$eligible_double_your_impact_matc)
domerge1$eligible_almost_home_match=as.integer(domerge1$eligible_almost_home_match)

set.seed(1234)
dtsplit<- initial_split(domerge1,prop = 0.7)
train<- training(dtsplit)
test<- testing(dtsplit)


recipe1<-recipe(is_exciting~., data=train)%>%
  step_impute_median(all_numeric())%>%
  step_dummy(all_nominal(),-all_outcomes(),one_hot = TRUE)%>%
  themis::step_downsample(is_exciting, under_ratio = 3) %>%
  prep()
```

\#bake

``` r
bake_train<-bake(recipe1,new_data = train)
bake_test<-bake(recipe1,new_data = test)
skim(bake_train)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
bake_train
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
229612
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
82
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
factor
</td>
<td style="text-align:left;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
81
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: factor**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:left;">
ordered
</th>
<th style="text-align:right;">
n_unique
</th>
<th style="text-align:left;">
top_counts
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
is_exciting
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
FAL: 203939, TRU: 25673
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
<th style="text-align:left;">
hist
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
school_latitude
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
37.25
</td>
<td style="text-align:right;">
4.67
</td>
<td style="text-align:right;">
18.25
</td>
<td style="text-align:right;">
34.04
</td>
<td style="text-align:right;">
37.67
</td>
<td style="text-align:right;">
40.76
</td>
<td style="text-align:right;">
67.26
</td>
<td style="text-align:left;">
▁▇▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_longitude
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-93.42
</td>
<td style="text-align:right;">
17.81
</td>
<td style="text-align:right;">
-167.59
</td>
<td style="text-align:right;">
-112.04
</td>
<td style="text-align:right;">
-87.69
</td>
<td style="text-align:right;">
-79.15
</td>
<td style="text-align:right;">
-66.63
</td>
<td style="text-align:left;">
▁▁▅▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_referred_count
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
2.20
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
125.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
non_teacher_referred_count
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4.35
</td>
<td style="text-align:right;">
5.50
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
304.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_charter
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_magnet
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_year_round
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_nlns
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_kipp
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_charter_ready_promise
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_teach_for_america
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_ny_teaching_fellow
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fulfillment_labor_materials
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
27.75
</td>
<td style="text-align:right;">
8.58
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
17.00
</td>
<td style="text-align:right;">
30.00
</td>
<td style="text-align:right;">
35.00
</td>
<td style="text-align:right;">
35.00
</td>
<td style="text-align:left;">
▁▂▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
total_price_excluding_optional_s
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
519.66
</td>
<td style="text-align:right;">
2279.06
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
268.19
</td>
<td style="text-align:right;">
410.56
</td>
<td style="text-align:right;">
579.00
</td>
<td style="text-align:right;">
1000000.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
total_price_including_optional_s
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
617.86
</td>
<td style="text-align:right;">
2768.42
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
319.06
</td>
<td style="text-align:right;">
487.62
</td>
<td style="text-align:right;">
687.80
</td>
<td style="text-align:right;">
1219512.20
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
students_reached
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
98.92
</td>
<td style="text-align:right;">
2511.56
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
22.00
</td>
<td style="text-align:right;">
30.00
</td>
<td style="text-align:right;">
100.00
</td>
<td style="text-align:right;">
999999.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
eligible_double_your_impact_matc
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
eligible_almost_home_match
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_metro_rural
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_metro_suburban
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
school_metro_unknown
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
school_metro_urban
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
one_non_teacher_referred_donor_g\_FALSE.
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
one_non_teacher_referred_donor_g\_TRUE.
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▅▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
one_non_teacher_referred_donor_g\_unknown
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_prefix_Dr. 
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_prefix_Mr. 
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_prefix_Mrs. 
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
teacher_prefix_Ms. 
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▆
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Applied.Sciences
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Character.Education
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Civics…Government
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_College…Career.Prep
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Community.Service
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Early.Development
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Economics
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Environmental.Science
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_ESL
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Extracurricular
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Foreign.Languages
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Gym…Fitness
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Health…Life.Science
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Health…Wellness
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_History…Geography
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Literacy
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Literature…Writing
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Mathematics
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Music
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Nutrition
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Parent.Involvement
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Performing.Arts
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Social.Sciences
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Special.Needs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Sports
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_unknown
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_subject_Visual.Arts
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_Applied.Learning
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_Health…Sports
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_History…Civics
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_Literacy…Language
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▆
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_Math…Science
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_Music…The.Arts
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_Special.Needs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
primary_focus_area_unknown
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type_Books
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type_Other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type_Supplies
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▅
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type_Technology
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type_Trips
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type_unknown
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
resource_type_Visitors
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
poverty_level_high.poverty
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
poverty_level_highest.poverty
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▆▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
poverty_level_low.poverty
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
poverty_level_moderate.poverty
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level_Grades.3.5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level_Grades.6.8
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level_Grades.9.12
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level_Grades.PreK.2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▅
</td>
</tr>
<tr>
<td style="text-align:left;">
grade_level_unknown
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>

\#model

``` r
logistic_glm <- logistic_reg(mode="classification")%>%
  set_engine("glm")%>%
  fit(is_exciting~.,data=bake_train)
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
tidy(logistic_glm)%>%
  mutate_at(c("estimate","std.error","statistic","p.value"),round,4)
```

    ## # A tibble: 82 × 5
    ##    term                        estimate std.error statistic p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)                -9.84e+11  1.07e+12    -0.922  0.357 
    ##  2 school_latitude             2.03e- 2  1.6 e- 3    12.4    0     
    ##  3 school_longitude            4.3 e- 3  4   e- 4     9.76   0     
    ##  4 teacher_referred_count      3.52e- 1  3.5 e- 3   101.     0     
    ##  5 non_teacher_referred_count  6.26e- 2  1.1 e- 3    55.8    0     
    ##  6 school_charter              2.80e- 1  2.52e- 2    11.1    0     
    ##  7 school_magnet              -3.84e- 2  2.71e- 2    -1.42   0.157 
    ##  8 school_year_round          -6.95e- 2  3.7 e- 2    -1.88   0.0607
    ##  9 school_nlns                 1.48e- 2  5.81e- 2     0.254  0.799 
    ## 10 school_kipp                 9.9 e- 2  7.59e- 2     1.30   0.192 
    ## # … with 72 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

\#assessment

``` r
predict(logistic_glm,bake_train,type = "prob")%>%
  bind_cols(.,predict(logistic_glm,bake_train))%>%
  bind_cols(.,bake_train) -> scored_train_log
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
predict(logistic_glm,bake_test,type = "prob")%>%
  bind_cols(.,predict(logistic_glm,bake_test))%>%
  bind_cols(.,bake_test) -> scored_test_log
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
options(yardstick.event_first = FALSE)
 
scored_train_log %>%
  metrics(is_exciting, .pred_TRUE,estimate = .pred_class)%>%
  mutate(part="training")%>%
  bind_rows(scored_test_log %>%
              metrics(is_exciting,.pred_TRUE,estimate = .pred_class)%>%
            mutate(part="testing"))
```

    ## Warning: The `yardstick.event_first` option has been deprecated as of yardstick 0.0.7 and will be completely ignored in a future version.
    ## Instead, set the following argument directly in the metric function:
    ## `options(yardstick.event_first = TRUE)`  -> `event_level = 'first'` (the default)
    ## `options(yardstick.event_first = FALSE)` -> `event_level = 'second'`
    ## This warning is displayed once per session.

    ## # A tibble: 8 × 4
    ##   .metric     .estimator .estimate part    
    ##   <chr>       <chr>          <dbl> <chr>   
    ## 1 accuracy    binary         0.888 training
    ## 2 kap         binary         0.188 training
    ## 3 mn_log_loss binary         0.256 training
    ## 4 roc_auc     binary         0.893 training
    ## 5 accuracy    binary         0.888 testing 
    ## 6 kap         binary         0.185 testing 
    ## 7 mn_log_loss binary         0.256 testing 
    ## 8 roc_auc     binary         0.892 testing

``` r
# ROC Curve 
scored_train_log %>%
  mutate(model="train")%>%
  bind_rows(scored_test_log %>%
            mutate(model="test"))%>%
  group_by(model)%>%
  roc_curve(is_exciting,.pred_TRUE)%>%
  autoplot()
```

![](final_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Variable Importandce 
logistic_glm %>%
  vip(10)+
  labs(title="logistic model 1 variable importance plot")
```

![](final_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
# Confusion Matrix
scored_train_log%>%
  conf_mat(is_exciting,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Train confusion matrix")
```

![](final_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
scored_test_log%>%
  conf_mat(is_exciting,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Test confusion matrix")
```

![](final_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
# precision,recall
scored_train_log %>%
  yardstick::precision(is_exciting, .pred_class)%>%
mutate(part="train")%>%
  bind_rows(scored_train_log %>%
    yardstick::recall(is_exciting, .pred_class)%>%
    mutate(part="train"))%>%
  bind_rows(scored_test_log %>%
  yardstick::precision(is_exciting, .pred_class)%>%
    mutate(part="test")%>%
  bind_rows(scored_test_log %>%
    yardstick::recall(is_exciting, .pred_class)%>%mutate(part="test")))
```

    ## # A tibble: 4 × 4
    ##   .metric   .estimator .estimate part 
    ##   <chr>     <chr>          <dbl> <chr>
    ## 1 precision binary         0.504 train
    ## 2 recall    binary         0.149 train
    ## 3 precision binary         0.499 test 
    ## 4 recall    binary         0.147 test

``` r
lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

logistic_wf <- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(lasso_spec) %>%
  fit(train)
```

    ## Warning: from glmnet C++ code (error code -37); Convergence for 37th lambda
    ## value not reached after maxit=100000 iterations; solutions for larger lambdas
    ## returned

``` r
options(yardstick.event_first = FALSE)
predict(logistic_wf, train, type="prob") %>%
  bind_cols(predict(logistic_wf, train, type="class")) %>%
  bind_cols(train)  %>%
  metrics(is_exciting, estimate = .pred_class, .pred_TRUE)
```

    ## # A tibble: 4 × 3
    ##   .metric     .estimator .estimate
    ##   <chr>       <chr>          <dbl>
    ## 1 accuracy    binary         0.882
    ## 2 kap         binary         0.355
    ## 3 mn_log_loss binary         0.309
    ## 4 roc_auc     binary         0.910

``` r
logistic_wf %>%
  pull_workflow_fit() %>%
  vip()
```

    ## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
    ## ℹ Please use `extract_fit_parsnip()` instead.

![](final_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
logistic_wf %>%
 pull_workflow_fit() %>%
  tidy()%>%
  filter(estimate!=0)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 4.1-4

    ## # A tibble: 12 × 3
    ##    term                                       estimate penalty
    ##    <chr>                                         <dbl>   <dbl>
    ##  1 (Intercept)                              -5.33         0.01
    ##  2 school_latitude                           0.00724      0.01
    ##  3 school_longitude                          0.0000139    0.01
    ##  4 teacher_referred_count                    0.449        0.01
    ##  5 non_teacher_referred_count                0.0520       0.01
    ##  6 school_charter                            0.0782       0.01
    ##  7 teacher_teach_for_america                 0.204        0.01
    ##  8 fulfillment_labor_materials               0.0914       0.01
    ##  9 one_non_teacher_referred_donor_g_TRUE.    0.556        0.01
    ## 10 one_non_teacher_referred_donor_g_unknown -1.32         0.01
    ## 11 teacher_prefix_Ms.                        0.00564      0.01
    ## 12 resource_type_Technology                 -0.116        0.01

``` r
options(yardstick.event_first = FALSE)
predict(logistic_wf, train, type="prob") %>%
  bind_cols(predict(logistic_wf, train, type="class")) %>%
  bind_cols(train)  %>%
  metrics(is_exciting, estimate = .pred_class, .pred_TRUE)
```

    ## # A tibble: 4 × 3
    ##   .metric     .estimator .estimate
    ##   <chr>       <chr>          <dbl>
    ## 1 accuracy    binary         0.882
    ## 2 kap         binary         0.355
    ## 3 mn_log_loss binary         0.309
    ## 4 roc_auc     binary         0.910

``` r
predict(logistic_wf, test, type="prob") %>%
  bind_cols(predict(logistic_wf, test, type="class")) %>%
  bind_cols(test) -> lasso_test 

lasso_test %>%
  metrics(is_exciting, estimate = .pred_class, .pred_TRUE)
```

    ## # A tibble: 4 × 3
    ##   .metric     .estimator .estimate
    ##   <chr>       <chr>          <dbl>
    ## 1 accuracy    binary         0.881
    ## 2 kap         binary         0.348
    ## 3 mn_log_loss binary         0.310
    ## 4 roc_auc     binary         0.909

\#random forest

# random sample small recipe

``` r
#set.seed(1234)
#smsplit<- initial_split(domerge1,prop = 0.1)
#smtrain<- training(smsplit)


#recipe_small<-recipe(is_exciting~., data=smtrain)%>%
#  step_impute_median(all_numeric())%>%
#  step_dummy(all_nominal(),-all_outcomes(),one_hot = TRUE)%>%
#  themis::step_downsample(is_exciting, under_ratio = 3) %>%
#  prep()

#bake_train_small<-bake(recipe_small,new_data = smtrain)

#skim(bake_train_small)
```

# I tune the tree use sampling and get 1656 trees, when I knit, I comment those code.

``` r
#rf_model <- rand_forest( trees=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("ranger")

#set up a resampling strategy

#set.seed(1234)

#sm_rs <- bootstraps(bake_train_small, times=10)

#set up controls

#ctrl <- control_grid(verbose = FALSE, save_pred = TRUE)


#roc_vals <- metric_set(roc_auc)

#formula_res <-
 # rf_model %>%
#  tune_grid(
#    is_exciting ~ .,
#    resamples = sm_rs,
#    grid = 3,
#    metrics = roc_vals,
#    control = ctrl )



#estimates <- collect_metrics(formula_res)
#estimates

#show_best(formula_res, metric = "roc_auc")
```

``` r
full_rf_model <- rand_forest(trees = 1656) %>%
   set_mode("classification") %>%
   set_engine("ranger", importance="permutation")

full_rf_workflow <- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(full_rf_model) %>%
  fit(train)

full_rf_workflow
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 3 Recipe Steps
    ## 
    ## • step_impute_median()
    ## • step_dummy()
    ## • step_downsample()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, num.trees = ~1656,      importance = ~"permutation", num.threads = 1, verbose = FALSE,      seed = sample.int(10^5, 1), probability = TRUE) 
    ## 
    ## Type:                             Probability estimation 
    ## Number of trees:                  1656 
    ## Sample size:                      102692 
    ## Number of independent variables:  81 
    ## Mtry:                             9 
    ## Target node size:                 10 
    ## Variable importance mode:         permutation 
    ## Splitrule:                        gini 
    ## OOB prediction error (Brier s.):  0.05961837

\#assessment

``` r
# score training
predict(full_rf_workflow, train, type = "prob") %>%
  bind_cols(predict(full_rf_workflow, train, type = "class")) %>%
  mutate(part = "training") %>%
  bind_cols(., train) -> scored_train

# -- score testing
predict(full_rf_workflow, test, type = "prob") %>%
  bind_cols(predict(full_rf_workflow,  test, type = "class")) %>%
  mutate(part = "testing") %>%
  bind_cols(., test) -> scored_test


options(yardstick.event_first = FALSE)
 
scored_train %>%
  metrics(is_exciting, .pred_TRUE,estimate = .pred_class)%>%
  mutate(part="training")%>%
  bind_rows(scored_test %>%
              metrics(is_exciting,.pred_TRUE,estimate = .pred_class)%>%
            mutate(part="testing"))
```

    ## # A tibble: 8 × 4
    ##   .metric     .estimator .estimate part    
    ##   <chr>       <chr>          <dbl> <chr>   
    ## 1 accuracy    binary         0.938 training
    ## 2 kap         binary         0.747 training
    ## 3 mn_log_loss binary         0.126 training
    ## 4 roc_auc     binary         0.993 training
    ## 5 accuracy    binary         0.911 testing 
    ## 6 kap         binary         0.663 testing 
    ## 7 mn_log_loss binary         0.163 testing 
    ## 8 roc_auc     binary         0.965 testing

``` r
# ROC Curve 
scored_train %>%
  mutate(model="train")%>%
  bind_rows(scored_test %>%
            mutate(model="test"))%>%
  group_by(model)%>%
  roc_curve(is_exciting,.pred_TRUE)%>%
  autoplot()
```

![](final_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# Variable Importandce 
full_rf_workflow %>%
  extract_fit_parsnip() %>%
  vip()+
  labs(title="RF model variable importance plot")
```

![](final_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
# Confusion Matrix
scored_test%>%
  conf_mat(is_exciting,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Test confusion matrix")
```

![](final_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
# precision,recall
scored_test %>%
  yardstick::precision(is_exciting, .pred_class)%>%
  bind_rows(scored_test %>%
  yardstick::recall(is_exciting, .pred_class))
```

    ## # A tibble: 2 × 3
    ##   .metric   .estimator .estimate
    ##   <chr>     <chr>          <dbl>
    ## 1 precision binary         0.560
    ## 2 recall    binary         0.973

# donation cluster part

``` r
donation<-read_csv("Donations.csv") %>% clean_names()
```

    ## Rows: 1048575 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): donationid, projectid, donor_acctid, donor_city, donor_state, donat...
    ## dbl (4): donor_zip, donation_to_project, donation_optional_support, donation...
    ## lgl (8): is_teacher_acct, donation_included_optional_support, payment_includ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(donation)
```

    ## # A tibble: 6 × 21
    ##   donationid     proje…¹ donor…² donor…³ donor…⁴ donor…⁵ is_te…⁶ donat…⁷ donat…⁸
    ##   <chr>          <chr>   <chr>   <chr>   <chr>     <dbl> <lgl>   <chr>     <dbl>
    ## 1 431d720bc3dfd… ffffac… 22cbc9… Peacht… GA        30269 FALSE   27:34.8   42.5 
    ## 2 fcfedba1c8a0b… ffffac… 521f18… <NA>    GA           NA FALSE   54:21.6   26.8 
    ## 3 3fa95d29986aa… ffffac… 1e0a63… Rockvi… MD        20853 FALSE   53:53.0   55.4 
    ## 4 020ad6bd5e88a… ffffac… 1d4acb… Salem   IN        47167 FALSE   54:01.1    8.5 
    ## 5 4b44b03f304d6… ffffac… 59c3c3… anonym… <NA>          0 FALSE   21:00.0   20   
    ## 6 9be4b22432cae… ffffac… 2215e4… <NA>    <NA>         NA FALSE   59:09.4    4.25
    ## # … with 12 more variables: donation_optional_support <dbl>,
    ## #   donation_total <dbl>, dollar_amount <chr>,
    ## #   donation_included_optional_support <lgl>, payment_method <chr>,
    ## #   payment_included_acct_credit <lgl>,
    ## #   payment_included_campaign_gift_card <lgl>,
    ## #   payment_included_web_purchased_gift_card <lgl>,
    ## #   payment_was_promo_matched <lgl>, via_giving_page <lgl>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
skim(donation)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
donation
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
1048575
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
logical
</td>
<td style="text-align:left;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: character**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
empty
</th>
<th style="text-align:right;">
n_unique
</th>
<th style="text-align:right;">
whitespace
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
donationid
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8289
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1048572
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
projectid
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
177220
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
donor_acctid
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
535334
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
donor_city
</td>
<td style="text-align:right;">
680115
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
13989
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
donor_state
</td>
<td style="text-align:right;">
221523
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_timestamp
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
53536
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
dollar_amount
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_method
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_message
</td>
<td style="text-align:right;">
270826
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
12477
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
356172
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

**Variable type: logical**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:left;">
count
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
is_teacher_acct
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:left;">
FAL: 927315, TRU: 121252
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_included_optional_support
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:left;">
TRU: 943037, FAL: 105530
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_acct_credit
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:left;">
FAL: 937361, TRU: 111206
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_campaign_gift_card
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:left;">
FAL: 698155, TRU: 350412
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_web_purchased_gift_card
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:left;">
FAL: 991618, TRU: 56949
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_was_promo_matched
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:left;">
FAL: 961898, TRU: 86669
</td>
</tr>
<tr>
<td style="text-align:left;">
via_giving_page
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:left;">
FAL: 702061, TRU: 346506
</td>
</tr>
<tr>
<td style="text-align:left;">
for_honoree
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:left;">
FAL: 1027049, TRU: 21518
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
<th style="text-align:left;">
hist
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
donor_zip
</td>
<td style="text-align:right;">
580554
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
50667.01
</td>
<td style="text-align:right;">
33285.41
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
20003.0
</td>
<td style="text-align:right;">
46902.00
</td>
<td style="text-align:right;">
89135.00
</td>
<td style="text-align:right;">
99999
</td>
<td style="text-align:left;">
▇▆▂▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_to_project
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
65.75
</td>
<td style="text-align:right;">
215.43
</td>
<td style="text-align:right;">
-11.80
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
21.25
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
85000
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_optional_support
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
10.70
</td>
<td style="text-align:right;">
32.97
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
1.5
</td>
<td style="text-align:right;">
3.75
</td>
<td style="text-align:right;">
7.50
</td>
<td style="text-align:right;">
15000
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_total
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
76.45
</td>
<td style="text-align:right;">
243.75
</td>
<td style="text-align:right;">
-11.80
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
25.00
</td>
<td style="text-align:right;">
56.47
</td>
<td style="text-align:right;">
100000
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>

``` r
# Delete 8 rows that only have dornationid
donation1<-donation[!is.na(donation$projectid),]
donation1<-donation1 %>% dplyr::select(-c(donationid,projectid,donor_acctid,donation_message))
skim(donation1)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
donation1
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
1048567
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
logical
</td>
<td style="text-align:left;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: character**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
empty
</th>
<th style="text-align:right;">
n_unique
</th>
<th style="text-align:right;">
whitespace
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
donor_city
</td>
<td style="text-align:right;">
680107
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
13989
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
donor_state
</td>
<td style="text-align:right;">
221515
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_timestamp
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
53536
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
dollar_amount
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_method
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

**Variable type: logical**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:left;">
count
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
is_teacher_acct
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:left;">
FAL: 927315, TRU: 121252
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_included_optional_support
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:left;">
TRU: 943037, FAL: 105530
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_acct_credit
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:left;">
FAL: 937361, TRU: 111206
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_campaign_gift_card
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:left;">
FAL: 698155, TRU: 350412
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_web_purchased_gift_card
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:left;">
FAL: 991618, TRU: 56949
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_was_promo_matched
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:left;">
FAL: 961898, TRU: 86669
</td>
</tr>
<tr>
<td style="text-align:left;">
via_giving_page
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:left;">
FAL: 702061, TRU: 346506
</td>
</tr>
<tr>
<td style="text-align:left;">
for_honoree
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:left;">
FAL: 1027049, TRU: 21518
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
<th style="text-align:left;">
hist
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
donor_zip
</td>
<td style="text-align:right;">
580546
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
50667.01
</td>
<td style="text-align:right;">
33285.41
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
20003.0
</td>
<td style="text-align:right;">
46902.00
</td>
<td style="text-align:right;">
89135.00
</td>
<td style="text-align:right;">
99999
</td>
<td style="text-align:left;">
▇▆▂▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_to_project
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
65.75
</td>
<td style="text-align:right;">
215.43
</td>
<td style="text-align:right;">
-11.80
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
21.25
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
85000
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_optional_support
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
10.70
</td>
<td style="text-align:right;">
32.97
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
1.5
</td>
<td style="text-align:right;">
3.75
</td>
<td style="text-align:right;">
7.50
</td>
<td style="text-align:right;">
15000
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_total
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
76.45
</td>
<td style="text-align:right;">
243.75
</td>
<td style="text-align:right;">
-11.80
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
25.00
</td>
<td style="text-align:right;">
56.47
</td>
<td style="text-align:right;">
100000
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>

``` r
#donation1%>%
#  na.omit(donor_city,donor_state)%>%
 # skim()

#do2<-donation1[!is.na(donation1$donor_state),]
#do2<-do2[!is.na(do2$donor_city),]
#skim(do2)

do2<-donation1%>% dplyr::select(-c(donor_zip,donor_city,donor_state,donation_timestamp))
```

\#dummy

``` r
head(do2)
```

    ## # A tibble: 6 × 13
    ##   is_teacher_a…¹ donat…² donat…³ donat…⁴ dolla…⁵ donat…⁶ payme…⁷ payme…⁸ payme…⁹
    ##   <lgl>            <dbl>   <dbl>   <dbl> <chr>   <lgl>   <chr>   <lgl>   <lgl>  
    ## 1 FALSE            42.5     7.5     50   10_to_… TRUE    no_cas… FALSE   TRUE   
    ## 2 FALSE            26.8     4.73    31.6 10_to_… TRUE    credit… FALSE   TRUE   
    ## 3 FALSE            55.4     0       55.4 10_to_… FALSE   no_cas… TRUE    FALSE  
    ## 4 FALSE             8.5     1.5     10   10_to_… TRUE    paypal  FALSE   FALSE  
    ## 5 FALSE            20       0       20   10_to_… FALSE   no_cas… FALSE   FALSE  
    ## 6 FALSE             4.25    0.75     5   under_… TRUE    no_cas… FALSE   TRUE   
    ## # … with 4 more variables: payment_included_web_purchased_gift_card <lgl>,
    ## #   payment_was_promo_matched <lgl>, via_giving_page <lgl>, for_honoree <lgl>,
    ## #   and abbreviated variable names ¹​is_teacher_acct, ²​donation_to_project,
    ## #   ³​donation_optional_support, ⁴​donation_total, ⁵​dollar_amount,
    ## #   ⁶​donation_included_optional_support, ⁷​payment_method,
    ## #   ⁸​payment_included_acct_credit, ⁹​payment_included_campaign_gift_card
    ## # ℹ Use `colnames()` to see all variable names

``` r
donation_cluster<-do2
donation_cluster$is_teacher_acct <-ifelse(donation_cluster$is_teacher_acct=='TRUE',1,0)
donation_cluster$donation_included_optional_support <-ifelse(donation_cluster$donation_included_optional_support=='TRUE',1,0)
donation_cluster$payment_included_acct_credit <-ifelse(donation_cluster$payment_included_acct_credit=='TRUE',1,0)
donation_cluster$payment_included_campaign_gift_card <-ifelse(donation_cluster$payment_included_campaign_gift_card=='TRUE',1,0)
donation_cluster$payment_included_web_purchased_gift_card <-ifelse(donation_cluster$payment_included_web_purchased_gift_card=='TRUE',1,0)
donation_cluster$payment_was_promo_matched<-ifelse(donation_cluster$payment_was_promo_matched=='TRUE',1,0)
donation_cluster$via_giving_page<-ifelse(donation_cluster$via_giving_page=='TRUE',1,0)
donation_cluster$for_honoree<-ifelse(donation_cluster$for_honoree=='TRUE',1,0)



#donation_cluster%>%
 # group_by(payment_method)%>%
  #summarise(n=n())

donation_cluster$ten_to_one_hundred <-ifelse(donation_cluster$dollar_amount=='10_to_100',1,0)
donation_cluster$one_hundred_and_up <-ifelse(donation_cluster$dollar_amount=='100_and_up',1,0)
donation_cluster$under_ten <-ifelse(donation_cluster$dollar_amount=='under_10',1,0)


donation_cluster$almost_home_match <-ifelse(donation_cluster$payment_method=='almost_home_match',1,0)
donation_cluster$amazon <-ifelse(donation_cluster$payment_method=='amazon',1,0)
donation_cluster$check <-ifelse(donation_cluster$payment_method=='check',1,0)
donation_cluster$creditcard <-ifelse(donation_cluster$payment_method=='creditcard',1,0)
donation_cluster$double_your_impact_match <-ifelse(donation_cluster$payment_method=='double_your_impact_match',1,0)
donation_cluster$no_cash_received <-ifelse(donation_cluster$payment_method=='no_cash_received',1,0)
donation_cluster$paypal <-ifelse(donation_cluster$payment_method=='paypal',1,0)
donation_cluster$promo_code_match <-ifelse(donation_cluster$payment_method=='promo_code_match',1,0)

donation_cluster<-donation_cluster%>% dplyr::select(-c(payment_method,dollar_amount))
```

\#scale

``` r
donation_cluster<-donation_cluster%>%
  mutate_if(is.numeric,scale)

skim(donation_cluster)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
donation_cluster
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
1048567
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
<th style="text-align:left;">
hist
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
is_teacher_acct
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
2.77
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_to_project
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
394.25
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_optional_support
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.33
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
454.61
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_total
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
409.94
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
donation_included_optional_support
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-2.99
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:left;">
▁▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_acct_credit
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.34
</td>
<td style="text-align:right;">
-0.34
</td>
<td style="text-align:right;">
-0.34
</td>
<td style="text-align:right;">
-0.34
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_campaign_gift_card
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.71
</td>
<td style="text-align:right;">
-0.71
</td>
<td style="text-align:right;">
-0.71
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:left;">
▇▁▁▁▅
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_included_web_purchased_gift_card
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
4.17
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
payment_was_promo_matched
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
via_giving_page
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.70
</td>
<td style="text-align:right;">
-0.70
</td>
<td style="text-align:right;">
-0.70
</td>
<td style="text-align:right;">
1.42
</td>
<td style="text-align:right;">
1.42
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
for_honoree
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
6.91
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
ten_to_one_hundred
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-1.39
</td>
<td style="text-align:right;">
-1.39
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:left;">
▅▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
one_hundred_and_up
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.51
</td>
<td style="text-align:right;">
-0.51
</td>
<td style="text-align:right;">
-0.51
</td>
<td style="text-align:right;">
-0.51
</td>
<td style="text-align:right;">
1.98
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
under_ten
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.40
</td>
<td style="text-align:right;">
-0.40
</td>
<td style="text-align:right;">
-0.40
</td>
<td style="text-align:right;">
-0.40
</td>
<td style="text-align:right;">
2.52
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
almost_home_match
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
12.02
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
amazon
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
5.83
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
check
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
10.11
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
creditcard
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.64
</td>
<td style="text-align:right;">
-0.64
</td>
<td style="text-align:right;">
-0.64
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
double_your_impact_match
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
5.36
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
no_cash_received
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.94
</td>
<td style="text-align:right;">
-0.94
</td>
<td style="text-align:right;">
-0.94
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:left;">
▇▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
paypal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
promo_code_match
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>

# random sample

``` r
set.seed(1234)

cluster_sample<-sample_n(donation_cluster,5000)
```

``` r
# how many clusters do we need?

fviz_nbclust(cluster_sample, kmeans, method="wss")
```

![](final_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
set.seed(904)

clusters1 <- kmeans(donation_cluster, 8, iter.max = 200, nstart = 10)
print(clusters1)
```

    ## K-means clustering with 8 clusters of sizes 93441, 137906, 156574, 206515, 305494, 139788, 6976, 1873
    ## 
    ## Cluster means:
    ##   is_teacher_acct donation_to_project donation_optional_support donation_total
    ## 1     -0.05923888           0.3137064                 0.3598379      0.3259297
    ## 2      0.65559339          -0.2914221                -0.3110687     -0.2996498
    ## 3     -0.20989567           0.6328283                 0.6781331      0.6510436
    ## 4     -0.09572158          -0.1685552                -0.1827078     -0.1736854
    ## 5     -0.02161946          -0.1955917                -0.2107939     -0.2013806
    ## 6     -0.16505809          -0.1852805                -0.1954135     -0.1901870
    ## 7     -0.36160182           1.2296948                 1.4961975      1.2891984
    ## 8     -0.02268264          12.6397613                11.8012635     12.7674437
    ##   donation_included_optional_support payment_included_acct_credit
    ## 1                      -0.0539233384                    2.9032813
    ## 2                      -0.1309688495                    0.0168231
    ## 3                       0.0370428252                   -0.3385467
    ## 4                      -0.0191996716                   -0.3328315
    ## 5                       0.0007350961                   -0.3444375
    ## 6                       0.1332885210                   -0.3235044
    ## 7                       0.3345210072                   -0.3444375
    ## 8                       0.0399337520                    0.5260136
    ##   payment_included_campaign_gift_card payment_included_web_purchased_gift_card
    ## 1                         -0.64701810                               -0.2124937
    ## 2                          0.06187973                                0.2122672
    ## 3                         -0.55958712                                0.1504112
    ## 4                         -0.64829083                               -0.2250958
    ## 5                          1.22439390                                0.1505210
    ## 6                         -0.67801933                               -0.2319443
    ## 7                         -0.70845677                               -0.2396463
    ## 8                         -0.48095286                                0.8699471
    ##   payment_was_promo_matched via_giving_page for_honoree ten_to_one_hundred
    ## 1               -0.29772141      -0.3021485 -0.06910933         -0.2140931
    ## 2                0.04467347       0.2651547 -0.11057964         -1.3938965
    ## 3               -0.01309496       0.1805874  0.15626817         -1.3537669
    ## 4                0.46408403       0.2072135  0.13009567          0.7174127
    ## 5               -0.30008671      -0.4783347 -0.07317101          0.6123911
    ## 6                0.15841904       0.4175938 -0.04443514          0.7174127
    ## 7               -0.30016992       1.0796556 -0.14474553         -1.2495309
    ## 8               -0.27108600       0.4381917 -0.02800454         -1.3938965
    ##   one_hundred_and_up  under_ten almost_home_match       amazon       check
    ## 1          0.4683161 -0.2547659       -0.08316199 -0.131161680 -0.09567820
    ## 2         -0.5061446  2.5220293       -0.08316199  0.021096008 -0.08600503
    ## 3          1.9285455 -0.3965057       -0.08316199 -0.004638173  0.34227560
    ## 4         -0.5061446 -0.3965057       -0.08316199 -0.171607321  0.04676556
    ## 5         -0.3826908 -0.3965057       -0.08316199 -0.171607321 -0.09681664
    ## 6         -0.5061446 -0.3965057       -0.08316199  0.711261922 -0.09895447
    ## 7          1.8017459 -0.3914853       12.02471309 -0.171607321 -0.09895447
    ## 8          1.9757182 -0.3965057        1.37779891 -0.155593282  0.88173340
    ##   creditcard double_your_impact_match no_cash_received     paypal
    ## 1 -0.5443081              -0.18644899        0.9401066 -0.2664030
    ## 2 -0.1929893              -0.18640874        0.1831431  0.1108076
    ## 3  0.3333301               1.05878631       -0.6720974 -0.0787042
    ## 4  1.5293703              -0.18644899       -0.9417457 -0.3022266
    ## 5 -0.6406934              -0.18644899        1.0614370 -0.3022266
    ## 6 -0.6406934              -0.18644899       -0.9417457  1.2828744
    ## 7 -0.6406934              -0.18644899       -0.9417457 -0.3022266
    ## 8 -0.4244221               0.09504322        0.3034189 -0.2906591
    ##   promo_code_match
    ## 1      -0.28778228
    ## 2       0.04115368
    ## 3      -0.04081581
    ## 4      -0.28778228
    ## 5      -0.28778228
    ## 6       1.26942806
    ## 7      -0.28778228
    ## 8      -0.26166687
    ## 
    ## Clustering vector:
    ##     [1] 5 4 1 6 5 2 4 3 3 4 4 6 4 3 3 4 5 3 4 6 6 6 4 1 5 2 3 1 5 5 6 4 6 5 5 4
    ##    [37] 6 6 2 5 5 5 6 4 4 4 5 5 5 3 4 3 4 6 4 4 3 4 4 1 3 1 6 4 6 3 3 3 5 2 2 3
    ##    [73] 6 2 2 2 1 2 2 2 2 1 2 2 2 6 2 1 2 2 2 2 3 5 2 5 2 2 2 2 2 2 2 5 1 2 2 2
    ##   [109] 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 6 2 4 2 2 2 2 2 5 2 2 2 2 2 2 2 2
    ##   [145] 2 2 2 2 2 2 2 2 2 2 2 2 5 5 1 5 5 5 5 3 1 5 5 6 2 4 4 6 5 2 4 4 6 3 5 4
    ##   [181] 1 6 3 3 4 3 4 1 5 4 5 4 3 4 4 5 3 5 1 6 1 6 4 4 4 6 6 5 3 6 1 3 6 1 3 5
    ##   [217] 6 4 4 3 3 4 5 5 6 5 5 5 5 5 5 4 5 5 5 5 1 6 4 4 3 4 6 4 3 5 6 4 6 6 6 1
    ##   [253] 4 1 3 5 5 6 5 3 5 3 6 4 5 2 2 5 6 5 5 6 4 5 5 5 5 5 5 4 6 4 3 4 6 6 4 4
    ##   [289] 6 6 6 6 4 6 6 6 6 5 2 4 4 6 6 2 6 6 2 3 3 5 2 4 1 5 2 5 5 4 3 2 5 1 3 3
    ##   [325] 2 5 5 5 2 4 5 5 5 2 2 3 5 5 1 5 1 4 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 4
    ##   [361] 1 5 5 5 5 5 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 2 3 5 4 4 5 4 5 1 3 6
    ##   [397] 4 6 4 6 5 7 5 5 3 5 3 4 5 5 3 2 3 2 5 3 5 3 8 5 5 5 3 3 5 3 3 5 4 5 1 5
    ##   [433] 2 5 3 5 5 2 5 6 5 6 5 5 6 2 2 2 2 1 2 2 3 2 2 2 2 2 4 5 4 4 3 6 4 3 5 3
    ##   [469] 5 4 3 3 4 6 6 3 2 3 3 4 3 3 5 5 5 5 5 5 5 5 5 1 5 5 5 2 5 5 5 5 5 5 5 5
    ##   [505] 5 5 5 5 5 4 1 7 8 3 5 5 1 5 2 5 2 1 3 5 4 3 5 1 3 1 3 6 4 3 4 4 4 3 4 1
    ##   [541] 3 4 1 3 3 4 5 5 3 5 4 1 4 5 2 4 1 3 5 4 4 1 3 6 5 3 3 5 3 7 4 5 1 5 4 5
    ##   [577] 3 1 4 5 5 5 5 5 5 5 5 5 2 5 5 5 3 5 2 5 5 5 5 6 4 1 5 1 1 5 6 7 4 3 5 4
    ##   [613] 4 4 5 4 3 5 5 4 1 4 4 5 5 5 5 5 4 6 3 4 3 3 3 3 3 3 5 3 4 6 1 6 4 6 6 3
    ##   [649] 4 3 1 3 5 1 3 1 6 2 4 4 2 2 4 2 6 4 5 2 6 5 4 6 4 4 4 6 4 4 1 3 4 3 4 4
    ##   [685] 3 2 7 4 4 2 5 3 5 4 2 5 5 7 5 4 6 4 4 6 4 6 4 6 3 1 3 5 6 3 1 4 6 6 6 5
    ##   [721] 3 5 4 4 4 4 3 4 4 2 4 6 4 4 4 6 4 6 5 5 5 5 5 1 5 1 3 6 1 5 5 5 5 2 5 5
    ##   [757] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 2 4 4 1 1 1 5 5 5 3 3 5 3 3
    ##   [793] 4 5 5 5 5 5 5 4 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 6 4 4 4 3 3 3 3 5 1 7
    ##   [829] 3 5 5 3 5 1 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5
    ##   [865] 5 4 4 4 4 3 4 4 2 2 5 5 2 2 2 2 2 2 2 2 2 5 5 2 3 2 2 2 2 2 2 2 2 2 2 4
    ##   [901] 2 2 2 5 2 2 2 2 4 2 2 1 6 3 3 2 3 3 3 1 4 7 2 3 5 5 6 6 4 3 4 4 4 4 4 4
    ##   [937] 4 5 4 4 4 4 1 3 4 4 6 6 6 6 3 3 4 4 3 6 4 6 4 3 3 4 3 6 3 4 3 3 4 1 4 5
    ##   [973] 4 5 2 4 5 5 5 1 1 4 2 5 2 5 5 5 5 5 4 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ##  [1009] 5 5 4 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 6 5 4 1 5 5 5 5 3 5 5 5 2
    ##  [1045] 4 5 3 5 4 6 3 5 5 5 5 3 3 3 1 1 5 5 5 5 5 1 5 2 5 2 4 5 7 4 6 3 5 3 4 1
    ##  [1081] 5 5 4 5 5 5 2 6 4 6 4 5 4 3 3 5 5 5 1 3 3 1 4 4 6 6 4 7 6 3 5 5 3 3 6 3
    ##  [1117] 3 6 4 3 6 5 5 5 5 6 3 4 1 5 3 3 5 5 4 5 4 2 5 2 1 5 2 4 1 6 4 6 6 6 6 6
    ##  [1153] 6 6 6 6 2 2 5 3 1 1 3 3 6 5 5 4 5 2 1 5 2 1 1 6 2 6 6 2 6 2 2 1 4 6 2 2
    ##  [1189] 6 6 6 2 1 3 3 3 5 5 5 5 1 6 3 3 6 3 1 5 5 4 3 4 4 6 2 2 2 2 6 2 2 2 5 5
    ##  [1225] 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 4 2 4 2 2 2 5 2 2 2 2 1 2 4 3 5 4 3 4 2
    ##  [1261] 6 6 4 4 5 1 1 3 7 4 5 5 4 3 5 5 4 5 2 5 3 4 5 4 3 5 5 4 1 1 1 4 6 4 4 4
    ##  [1297] 4 3 5 1 2 2 2 2 1 1 3 4 3 6 5 4 4 3 6 6 5 3 6 6 1 6 3 4 6 6 4 3 3 4 4 6
    ##  [1333] 5 6 4 6 3 3 1 4 3 1 3 1 5 3 2 2 2 2 2 2 1 5 5 5 4 3 5 3 3 1 3 3 4 2 4 3
    ##  [1369] 5 4 1 4 4 6 4 5 5 2 5 6 5 2 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 4 5 5 5 5 5 5
    ##  [1405] 5 5 5 4 3 3 4 3 5 1 7 5 3 5 6 5 4 4 1 4 4 6 3 4 6 5 6 4 4 4 6 4 2 6 5 4
    ##  [1441] 5 1 1 6 1 1 1 5 1 5 1 4 3 4 1 5 5 4 1 1 6 5 4 3 4 6 6 4 3 2 2 2 3 2 2 3
    ##  [1477] 6 6 6 6 6 4 6 4 4 6 4 6 6 4 4 6 4 6 2 4 6 4 4 4 2 5 5 4 3 6 6 2 5 4 1 6
    ##  [1513] 6 6 4 6 4 4 3 4 4 3 1 4 2 5 3 6 6 1 6 4 3 6 4 3 3 6 6 3 5 5 6 1 6 4 3 5
    ##  [1549] 5 5 6 5 5 4 2 3 1 1 2 3 5 3 4 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5
    ##  [1585] 5 5 6 5 5 5 5 4 5 5 5 5 1 3 5 4 5 4 4 5 3 5 3 5 6 4 4 4 3 4 6 6 4 5 6 2
    ##  [1621] 5 5 5 5 4 5 5 4 5 5 5 4 5 4 5 5 5 1 4 4 6 5 6 4 8 3 3 5 5 5 5 5 3 5 5 5
    ##  [1657] 5 1 1 5 5 3 5 5 5 5 1 1 5 5 4 3 3 4 6 3 2 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1693] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 3 4 3 3 6 4 2 4 4 4 2
    ##  [1729] 4 6 2 4 4 4 3 4 3 4 1 2 4 3 5 1 6 3 1 3 5 2 5 2 4 5 3 4 5 5 4 5 1 4 1 1
    ##  [1765] 3 3 5 4 5 6 5 3 5 4 5 4 6 4 4 6 4 3 3 4 6 4 4 4 6 6 5 5 5 2 1 5 5 4 1 3
    ##  [1801] 5 5 6 5 5 2 5 5 5 5 4 6 2 2 6 6 6 4 3 6 3 4 3 4 4 4 6 6 2 3 1 2 5 1 5 3
    ##  [1837] 4 4 1 5 5 4 1 3 5 4 3 5 4 5 3 5 4 4 4 4 4 3 3 5 1 5 2 2 2 2 2 2 2 2 4 4
    ##  [1873] 6 6 5 4 5 5 5 5 6 5 3 1 1 1 3 1 4 4 6 6 5 6 4 6 5 4 4 5 6 5 2 4 5 3 3 4
    ##  [1909] 5 6 4 3 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 2 6 2 6 6 6 4 4
    ##  [1945] 4 6 4 6 6 6 6 6 6 4 6 4 4 6 4 4 6 4 2 4 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 1
    ##  [1981] 3 5 3 1 5 4 4 4 6 6 6 6 3 3 1 3 2 2 2 6 4 6 4 3 6 6 6 6 3 3 4 3 6 4 2 3
    ##  [2017] 5 3 6 4 2 1 3 4 4 4 1 4 1 3 3 3 5 3 4 3 3 6 1 7 1 3 4 1 3 6 6 5 6 3 5 3
    ##  [2053] 5 5 4 4 7 4 5 7 4 3 5 3 1 1 3 3 4 4 4 4 1 5 6 3 3 5 6 4 3 4 3 3 4 5 5 1
    ##  [2089] 3 3 2 6 5 5 5 5 2 2 2 3 3 1 6 5 3 1 6 2 5 3 1 3 5 3 6 3 3 3 3 3 5 4 6 4
    ##  [2125] 6 6 6 5 6 6 6 4 1 2 2 1 3 5 6 4 4 4 4 1 4 5 1 4 1 5 2 4 4 6 3 2 6 4 2 2
    ##  [2161] 4 1 6 1 5 2 2 5 2 2 2 2 5 3 4 5 5 5 2 2 2 3 4 2 4 5 4 7 7 6 3 3 1 4 1 4
    ##  [2197] 1 3 2 2 1 4 6 3 5 1 4 5 5 5 5 3 5 5 5 5 5 5 5 5 5 4 1 3 5 1 4 4 5 5 5 4
    ##  [2233] 5 5 5 5 6 4 5 5 6 5 2 5 5 5 5 5 5 2 5 5 6 5 5 5 6 4 5 4 5 5 5 5 5 5 5 6
    ##  [2269] 5 5 5 5 5 5 5 5 2 5 5 5 5 5 3 3 3 2 3 3 6 3 4 6 6 1 4 6 6 6 6 6 3 4 3 3
    ##  [2305] 1 3 3 1 1 5 5 5 6 3 5 6 6 4 6 4 6 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5
    ##  [2341] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 2 4 5 6 2 5 3 3 5 3 4 1 5 1 1 4
    ##  [2377] 5 2 4 6 6 2 5 4 4 1 1 1 7 8 5 4 3 5 5 5 5 4 5 4 3 4 4 4 3 5 5 3 5 1 4 4
    ##  [2413] 1 6 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 3 2 3 4 2 5 2 5
    ##  [2449] 2 5 3 5 4 4 3 4 2 4 3 2 2 3 4 6 6 4 6 4 6 3 3 4 4 3 3 4 1 5 2 3 3 1 5 4
    ##  [2485] 2 4 5 5 2 5 5 6 6 2 2 4 2 4 1 2 5 2 2 5 3 3 1 1 1 1 4 4 5 5 5 3 2 5 6 3
    ##  [2521] 6 5 6 1 5 5 1 3 5 3 1 6 5 4 6 6 3 6 4 4 4 4 3 4 2 1 1 2 1 1 1 4 3 5 1 4
    ##  [2557] 1 3 6 3 5 6 4 5 4 5 3 3 2 2 2 6 6 4 4 3 4 1 1 4 3 3 4 6 4 6 6 4 3 6 6 2
    ##  [2593] 2 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 1 5 6 6 4 4 4 1 4 4 4 4 2 4 4 6 6 4 5
    ##  [2629] 6 2 4 2 1 1 3 3 3 6 6 4 4 6 1 4 3 5 3 3 4 6 3 4 3 6 1 3 3 6 4 4 5 5 5 4
    ##  [2665] 6 4 3 3 3 6 6 6 3 4 4 4 6 4 4 4 4 5 3 7 1 5 5 3 4 5 1 3 4 6 4 6 5 6 2 6
    ##  [2701] 3 4 4 4 1 5 5 4 5 3 3 5 1 1 3 6 3 5 5 1 5 1 3 3 3 1 5 3 2 1 4 4 3 1 3 3
    ##  [2737] 4 4 2 6 6 3 4 4 4 4 5 3 5 5 6 3 3 6 5 4 1 3 4 4 5 2 7 3 6 4 6 2 7 5 4 3
    ##  [2773] 5 6 4 3 1 1 5 4 5 1 1 1 3 4 6 1 5 5 1 5 5 2 4 3 2 2 2 2 4 3 2 4 2 5 2 6
    ##  [2809] 4 2 2 2 2 2 2 4 3 5 3 5 5 2 4 5 1 3 5 4 4 4 7 6 2 5 2 1 5 5 1 3 5 5 1 5
    ##  [2845] 5 3 1 4 2 3 1 4 6 4 4 6 4 4 4 3 1 4 3 5 4 4 6 4 5 1 4 5 5 5 3 1 4 5 1 2
    ##  [2881] 2 2 4 2 3 6 2 2 4 2 4 5 3 4 4 2 1 1 1 1 1 1 1 1 1 1 1 2 3 2 5 1 2 2 1 1
    ##  [2917] 3 1 1 3 3 1 2 4 5 1 1 5 4 4 3 4 2 1 4 4 4 6 5 5 4 4 4 3 4 4 3 3 3 5 5 3
    ##  [2953] 1 1 3 4 3 4 4 4 6 4 6 6 4 4 4 4 4 3 4 3 4 4 6 3 2 4 4 2 1 4 2 3 4 4 5 4
    ##  [2989] 5 4 1 5 6 3 3 3 8 8 4 3 4 5 1 3 5 5 3 5 3 5 4 5 4 1 3 3 1 3 6 6 4 6 3 4
    ##  [3025] 4 6 6 4 4 6 6 4 5 2 1 2 6 4 6 6 6 5 6 4 3 3 5 4 4 5 5 5 3 1 4 6 5 3 1 5
    ##  [3061] 6 1 3 5 3 3 4 5 6 3 5 4 4 3 4 1 3 1 2 5 2 1 1 1 1 2 1 1 3 4 3 5 3 4 4 3
    ##  [3097] 6 6 6 1 5 4 4 6 6 3 4 3 2 4 5 3 4 4 1 7 5 1 5 4 2 4 3 4 6 6 6 3 6 4 4 4
    ##  [3133] 4 6 4 4 4 6 4 4 4 6 1 3 4 4 4 4 3 4 4 4 4 4 4 4 3 4 4 3 4 4 4 4 4 4 4 4
    ##  [3169] 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 3 4 4 4 4 3 3 4 4 4 4 4 4 4 4 4 4
    ##  [3205] 4 3 3 5 5 3 4 4 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ##  [3241] 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 4 3 1 6 4 6 6 4 6 6 4 6 6 1 4 1 3 3 3 1 2
    ##  [3277] 2 2 7 4 2 2 2 2 2 2 5 2 1 5 2 5 5 5 4 3 3 5 7 4 3 3 3 1 3 1 5 3 5 4 3 4
    ##  [3313] 5 2 4 5 5 5 5 5 3 4 6 3 4 4 3 5 3 3 6 6 6 4 4 6 6 4 3 3 1 3 2 5 4 5 5 1
    ##  [3349] 1 1 1 4 3 7 4 1 5 3 3 6 4 4 2 3 5 5 4 5 3 4 3 3 5 5 5 1 5 5 4 3 2 5 4 5
    ##  [3385] 5 2 4 5 5 7 4 6 5 2 7 6 4 3 5 3 5 4 5 5 5 5 5 5 2 1 5 5 1 5 5 5 5 5 5 5
    ##  [3421] 5 5 5 5 5 4 3 2 5 2 4 4 2 3 1 5 3 3 3 3 4 1 4 4 1 4 4 6 3 4 3 3 3 3 3 3
    ##  [3457] 1 1 2 4 6 5 1 1 4 7 3 1 4 1 4 1 4 4 4 4 4 6 6 4 4 4 5 4 5 3 3 5 2 1 5 3
    ##  [3493] 4 4 6 6 4 4 6 6 6 4 6 1 6 5 7 3 2 2 2 2 2 2 5 3 4 4 5 1 3 3 1 1 1 3 1 1
    ##  [3529] 1 4 4 5 4 4 3 4 4 6 6 1 1 1 1 3 5 1 1 1 1 1 1 6 6 3 6 4 5 4 1 6 4 6 4 6
    ##  [3565] 6 6 4 6 6 4 6 6 6 4 6 5 6 4 5 5 2 4 5 5 5 3 4 5 5 5 5 5 4 6 1 4 3 3 5 3
    ##  [3601] 5 1 5 3 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 2 5 5 5 5 5 3 6 5 5 5 1 3 4 4 3
    ##  [3637] 3 3 4 3 5 5 5 4 4 3 4 3 3 6 4 4 1 5 5 5 5 5 5 3 5 4 5 5 5 5 5 4 5 2 5 3
    ##  [3673] 1 1 4 1 3 3 2 6 3 4 3 3 3 4 3 4 3 1 6 5 4 5 5 5 6 4 5 3 5 1 1 2 1 5 1 5
    ##  [3709] 6 1 4 4 3 3 3 4 4 4 5 4 3 3 3 5 4 4 4 2 4 6 4 4 3 4 3 5 5 4 5 4 4 4 5 4
    ##  [3745] 5 3 4 1 4 6 5 5 4 5 4 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 3
    ##  [3781] 3 6 4 6 5 4 4 7 6 3 5 1 4 6 4 5 4 4 3 3 1 6 3 3 5 3 3 5 8 5 5 6 6 3 4 1
    ##  [3817] 3 5 6 6 3 1 1 2 1 1 6 4 5 6 6 4 6 4 6 6 5 3 6 3 6 4 2 4 4 4 4 4 4 1 3 3
    ##  [3853] 5 2 4 3 4 6 6 4 6 5 4 2 4 3 1 3 7 2 6 4 4 8 3 4 1 5 5 5 5 5 5 5 5 5 5 5
    ##  [3889] 5 5 5 5 6 5 5 5 5 5 5 5 1 3 1 7 5 3 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5
    ##  [3925] 5 5 5 5 4 5 4 4 6 4 5 5 5 3 3 6 4 6 6 6 6 1 3 6 5 5 5 5 5 5 5 5 5 5 5 4
    ##  [3961] 5 5 5 5 5 4 5 5 5 5 5 5 5 5 6 5 5 5 5 5 2 5 5 6 4 4 2 4 4 3 3 5 3 4 4 4
    ##  [3997] 1 4 5 3 6 6 1 5 3 5 4 6 5 1 3 2 2 6 7 6 3 2 2 1 3 5 1 5 6 1 5 5 2 5 3 1
    ##  [4033] 4 5 2 5 6 6 1 6 6 5 1 5 3 6 5 3 5 1 4 6 4 6 4 6 4 6 6 6 6 6 4 6 4 4 5 1
    ##  [4069] 3 3 3 3 3 2 5 3 4 3 1 3 2 6 4 2 6 3 5 5 4 2 2 4 4 4 2 4 2 6 2 3 5 1 3 4
    ##  [4105] 3 3 3 4 5 3 1 4 2 4 2 2 6 6 2 4 5 5 6 4 6 5 3 6 6 4 5 5 4 5 4 5 5 2 5 5
    ##  [4141] 1 3 3 3 5 5 5 4 3 5 2 6 1 3 4 6 2 2 2 2 3 2 5 5 5 3 5 5 5 5 5 5 5 5 5 5
    ##  [4177] 5 5 5 5 5 5 5 5 5 5 1 3 3 1 3 3 3 2 2 2 6 4 7 2 4 5 2 4 5 4 1 4 6 6 1 5
    ##  [4213] 5 1 1 6 5 8 6 4 5 4 5 4 4 5 5 5 3 6 3 2 5 4 4 3 1 3 3 6 8 1 1 4 3 3 3 3
    ##  [4249] 4 6 4 3 5 2 5 3 6 4 4 4 4 4 4 3 3 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 1 1 1
    ##  [4285] 1 6 4 4 4 5 2 3 6 4 5 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2
    ##  [4321] 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 5 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2
    ##  [4357] 1 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2 5 5 4 3 1 1 5 5 5 5
    ##  [4393] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 4 6 2 2 3 6 1 1 5 3 2 1
    ##  [4429] 3 4 4 4 4 2 1 4 4 1 3 6 4 6 4 4 6 3 6 4 3 3 1 3 3 4 4 4 5 5 6 3 6 4 4 6
    ##  [4465] 4 3 3 3 4 3 3 3 3 3 2 5 5 3 3 3 3 5 4 5 2 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5
    ##  [4501] 5 5 5 5 2 5 5 3 4 3 1 1 1 1 4 5 6 4 5 3 1 3 3 3 4 5 5 3 6 5 1 5 6 6 3 1
    ##  [4537] 5 4 6 5 3 1 6 3 1 6 6 1 5 5 3 4 2 4 1 6 6 4 6 6 6 6 6 5 5 6 1 4 5 5 3 1
    ##  [4573] 3 3 4 6 2 5 2 1 5 5 2 4 1 5 4 4 4 3 1 8 3 6 4 4 6 4 4 6 6 4 2 4 6 6 6 4
    ##  [4609] 6 6 4 4 4 6 6 6 4 6 3 3 3 1 2 2 2 2 2 2 5 2 2 2 2 6 2 1 2 2 2 2 2 2 2 2
    ##  [4645] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 4 2 2 2 2 2 2 6 2 2 2 4 2 4 2 2 2 2 2
    ##  [4681] 2 6 2 6 2 2 2 2 2 6 2 2 4 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 1 1 2 2 2 2 2 2
    ##  [4717] 2 2 4 2 2 5 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 3 5 1 5 5 3 6 1 1
    ##  [4753] 2 4 4 2 2 4 5 5 2 1 4 2 3 5 7 5 2 7 6 6 5 3 3 3 4 1 5 5 3 1 2 4 5 2 2 1
    ##  [4789] 4 3 5 6 2 2 4 2 4 1 5 6 5 2 5 5 6 4 1 5 5 7 5 3 1 5 2 4 5 4 3 2 5 8 2 2
    ##  [4825] 2 2 2 2 2 2 2 6 2 5 5 4 3 3 6 5 3 3 3 5 4 5 5 5 4 5 1 4 6 3 6 4 5 5 6 5
    ##  [4861] 4 3 6 5 5 6 3 4 4 3 3 5 6 1 5 2 6 5 3 3 4 3 4 3 3 5 1 3 1 6 6 6 6 2 2 2
    ##  [4897] 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4933] 2 2 2 2 2 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 5 2 5 2 6 2 2 2 2
    ##  [4969] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 2 2 2 2 2 2 2 2 1 2 2 2 2 2
    ##  [5005] 2 2 2 2 2 2 2 2 4 2 2 2 4 2 2 2 2 2 2 2 2 2 2 5 1 2 4 2 2 2 2 2 2 2 2 5
    ##  [5041] 5 5 5 5 4 5 5 5 5 3 5 5 5 5 5 1 3 5 4 4 2 4 7 5 4 5 5 5 5 5 5 5 5 5 1 3
    ##  [5077] 4 5 5 4 5 2 5 4 5 5 4 6 3 5 6 5 5 5 5 5 5 5 3 3 5 4 4 4 4 4 4 5 4 4 5 4
    ##  [5113] 3 6 1 3 3 4 7 3 3 6 3 3 3 4 3 3 3 3 1 1 4 1 5 4 3 6 6 4 4 6 6 3 3 3 3 3
    ##  [5149] 4 4 5 5 6 5 4 4 4 4 4 6 2 3 6 4 4 4 5 3 6 3 1 2 5 1 5 4 2 5 5 6 3 3 6 3
    ##  [5185] 5 3 3 3 4 4 3 3 3 4 5 5 5 3 5 3 5 6 5 5 5 5 5 3 4 6 4 3 3 1 3 3 4 1 2 2
    ##  [5221] 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 3 3 4 3
    ##  [5257] 6 6 6 4 4 4 3 3 3 6 6 4 3 4 5 3 4 3 5 5 5 5 2 5 5 5 5 4 3 5 4 5 6 5 5 5
    ##  [5293] 5 2 5 5 6 3 3 3 4 1 2 2 2 2 2 4 2 2 4 2 2 2 4 4 4 2 4 2 2 2 3 6 6 5 3 3
    ##  [5329] 3 5 2 3 3 3 5 5 6 5 4 6 5 4 4 6 2 3 6 6 4 3 6 5 5 5 5 3 6 4 2 6 5 5 4 6
    ##  [5365] 6 4 6 1 6 3 5 3 5 6 1 4 1 4 3 3 5 4 6 3 1 3 1 3 3 5 6 1 4 6 7 1 1 3 4 6
    ##  [5401] 6 6 6 4 4 4 4 6 6 4 6 1 3 5 4 3 2 2 1 3 5 1 4 1 1 3 3 4 3 4 4 4 5 1 3 1
    ##  [5437] 3 1 5 5 3 3 5 5 5 5 2 5 5 5 3 5 5 5 5 1 3 3 3 1 5 4 3 4 1 3 3 3 5 1 3 5
    ##  [5473] 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2
    ##  [5509] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 1 4 6 5 7 6 1 5 3 3 5 5 3 7 5 5 2 4 3 3
    ##  [5545] 3 3 3 3 6 4 3 5 5 5 1 5 5 1 4 4 2 2 4 3 5 6 4 2 3 2 2 2 6 2 2 2 2 3 5 2
    ##  [5581] 2 2 2 2 2 2 5 2 4 5 2 2 2 2 2 4 1 4 1 5 5 3 4 6 6 6 4 5 3 3 2 1 5 5 5 5
    ##  [5617] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 3 6 6 4 4 2
    ##  [5653] 2 5 5 5 6 4 3 1 4 5 4 4 4 3 4 2 2 2 5 6 1 6 6 2 3 3 3 1 3 3 3 4 4 3 4 5
    ##  [5689] 4 5 3 3 3 3 3 3 3 2 6 3 2 6 6 1 4 4 5 5 5 5 4 2 4 4 5 5 3 5 5 1 6 6 3 6
    ##  [5725] 3 3 4 3 3 1 3 6 2 4 4 4 2 4 2 6 6 5 5 4 4 1 3 4 5 6 6 4 6 3 5 6 5 3 3 3
    ##  [5761] 3 3 6 3 3 4 4 3 5 5 5 5 5 5 2 5 5 5 5 6 5 4 5 5 5 5 5 3 5 4 2 2 5 5 4 5
    ##  [5797] 3 5 4 5 5 3 5 6 5 5 2 6 5 1 3 4 3 4 3 4 4 4 4 4 3 4 4 3 4 3 6 3 3 5 5 1
    ##  [5833] 4 2 1 5 5 3 6 5 5 1 4 4 5 4 3 3 2 4 6 3 1 3 1 4 3 3 3 3 6 6 1 5 4 7 5 4
    ##  [5869] 6 2 6 2 4 4 6 4 4 4 3 4 5 5 5 3 5 6 1 5 5 3 5 5 4 3 6 4 3 5 1 5 4 6 5 5
    ##  [5905] 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 5 4 6 4 6 6 5 5 5 5 3 5 5 1 5 5 2
    ##  [5941] 3 4 4 3 3 3 6 4 3 4 6 4 1 4 6 5 4 3 3 3 5 1 5 3 3 3 5 5 3 2 1 4 6 4 5 4
    ##  [5977] 2 5 5 6 5 6 6 2 4 4 7 6 3 4 6 6 6 2 4 3 3 5 6 3 3 1 3 5 3 5 5 4 3 5 5 4
    ##  [6013] 2 5 5 2 5 5 5 1 4 5 2 6 4 4 4 4 1 1 5 6 4 3 6 3 4 2 1 4 2 3 6 1 3 1 5 4
    ##  [6049] 5 3 1 3 6 6 6 4 2 2 6 6 4 2 2 1 1 3 1 3 1 2 1 2 1 2 5 2 2 4 2 2 2 6 4 1
    ##  [6085] 3 3 3 5 4 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 2 5 5 5 5 5 5 5 5 5 5 3
    ##  [6121] 4 4 4 4 4 6 5 4 3 3 4 4 3 3 5 6 5 6 3 4 4 3 5 1 3 3 1 4 5 3 1 1 3 5 5 5
    ##  [6157] 5 5 4 5 5 5 6 3 4 5 6 4 5 4 5 4 6 4 5 6 5 6 5 4 5 5 3 1 3 3 5 6 4 4 3 4
    ##  [6193] 6 6 4 5 4 1 4 3 5 4 3 6 3 4 6 5 2 3 6 2 5 3 2 5 5 5 5 5 5 5 1 5 4 2 2 5
    ##  [6229] 5 5 5 2 5 4 5 5 5 5 5 5 3 3 3 5 5 4 3 3 3 4 3 4 3 3 3 6 3 6 3 5 4 3 1 5
    ##  [6265] 5 1 2 5 5 5 4 4 5 5 1 4 4 2 5 5 3 3 1 3 3 1 5 1 3 3 1 1 3 1 1 1 2 4 1 4
    ##  [6301] 1 4 3 1 3 6 4 6 5 4 6 6 6 6 6 6 6 4 6 6 3 5 1 4 5 6 6 4 1 5 1 6 4 4 5 4
    ##  [6337] 6 4 6 4 3 4 3 5 4 5 4 4 6 5 6 6 6 3 5 5 4 6 4 6 5 2 4 4 3 5 1 5 7 4 5 5
    ##  [6373] 2 2 5 6 5 6 6 5 5 5 4 5 6 5 2 2 2 5 5 7 4 5 5 1 3 4 7 4 5 5 1 1 5 5 5 6
    ##  [6409] 5 5 2 3 3 3 3 3 4 3 3 6 3 4 4 4 6 6 3 4 5 1 4 1 1 1 1 5 5 4 1 1 2 1 3 4
    ##  [6445] 1 4 6 4 4 4 6 1 3 6 2 1 6 6 4 6 4 4 1 4 2 6 4 2 3 3 4 6 4 4 4 2 4 4 6 4
    ##  [6481] 6 6 4 6 2 4 6 1 4 6 4 1 4 3 6 4 4 3 6 6 4 4 1 4 5 5 5 4 3 5 5 4 5 5 4 3
    ##  [6517] 3 5 1 3 4 5 1 5 5 4 5 5 5 6 5 4 5 6 5 5 5 6 5 5 1 1 4 5 7 5 5 4 4 6 3 5
    ##  [6553] 5 3 5 4 4 7 1 1 3 2 4 6 3 5 5 5 5 5 2 5 4 5 5 2 5 4 5 5 5 5 2 5 5 5 5 5
    ##  [6589] 2 5 5 6 5 2 5 1 1 4 1 1 2 6 4 4 6 6 6 4 5 4 4 5 1 5 4 4 5 4 4 4 4 4 5 6
    ##  [6625] 5 1 7 4 6 6 3 3 4 6 6 4 6 4 4 6 4 4 3 4 6 4 4 6 1 3 1 3 5 4 1 1 5 5 5 6
    ##  [6661] 5 4 5 5 1 4 2 6 6 2 4 4 5 6 2 3 4 6 3 6 2 6 3 5 2 1 1 3 3 3 4 6 6 4 4 5
    ##  [6697] 4 4 4 6 2 2 3 5 1 6 4 1 6 3 5 3 1 1 1 3 3 1 3 1 4 6 5 3 3 6 5 5 5 1 1 4
    ##  [6733] 3 3 1 2 4 4 4 5 1 5 4 3 1 4 2 5 2 5 2 5 3 3 2 2 2 5 3 3 3 3 6 4 4 4 4 3
    ##  [6769] 4 6 4 3 3 5 5 4 4 4 1 3 4 3 4 4 4 6 1 1 5 4 2 1 3 1 2 3 4 5 3 3 1 1 5 3
    ##  [6805] 1 3 5 5 2 5 5 5 5 2 3 5 1 5 5 5 5 4 5 6 5 5 5 5 5 6 5 5 3 2 2 2 2 2 6 2
    ##  [6841] 2 1 2 2 2 2 2 1 4 2 2 6 4 2 2 4 5 6 3 1 4 5 3 6 4 5 5 5 3 3 3 4 2 3 1 3
    ##  [6877] 1 1 1 1 1 5 1 1 5 2 1 1 1 5 4 4 4 2 5 4 5 1 3 1 5 5 5 3 5 5 5 5 2 2 3 2
    ##  [6913] 4 2 3 1 2 2 2 2 2 2 1 1 1 2 2 5 4 6 4 4 5 5 6 5 4 2 5 1 1 5 4 1 2 1 1 4
    ##  [6949] 5 4 6 3 4 2 3 3 4 4 5 6 5 4 4 1 4 4 4 3 4 6 3 3 5 5 5 5 5 3 1 3 1 6 5 5
    ##  [6985] 6 3 3 4 6 3 3 5 5 7 2 6 3 4 4 3 1 1 5 5 5 1 3 1 4 5 4 5 5 4 2 5 1 4 2 1
    ##  [7021] 3 4 5 4 3 1 4 5 8 1 3 5 3 5 4 3 6 3 6 3 6 6 6 5 4 3 4 6 6 3 4 6 4 5 4 8
    ##  [7057] 3 3 5 3 3 4 1 3 4 3 1 4 3 1 4 2 5 4 4 7 4 6 6 6 6 4 4 4 6 6 4 6 6 6 3 1
    ##  [7093] 1 3 5 4 4 4 3 3 3 5 4 4 3 4 6 4 6 3 3 6 6 6 3 4 6 4 6 4 4 6 4 6 6 6 6 1
    ##  [7129] 6 6 4 6 6 6 6 1 4 1 1 6 1 4 6 6 6 4 4 6 4 6 4 1 6 6 5 5 5 5 5 5 5 1 5 5
    ##  [7165] 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5
    ##  [7201] 2 6 5 5 5 5 1 5 4 5 5 5 5 3 5 3 5 5 5 3 6 4 5 4 7 4 2 4 2 5 5 1 2 1 4 3
    ##  [7237] 4 4 4 3 6 4 4 6 6 6 4 4 4 6 6 4 4 4 6 2 4 7 4 5 4 5 5 4 1 5 5 5 5 5 5 5
    ##  [7273] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 2 2 5 2 6 6 3 2 4 5 2 2 2 2 4 6 4
    ##  [7309] 3 6 6 4 1 4 6 1 1 3 5 4 2 1 4 3 6 4 6 4 4 4 1 3 6 4 4 4 6 3 3 3 4 1 4 2
    ##  [7345] 3 2 4 3 5 4 4 5 3 3 2 4 1 3 4 3 6 6 2 6 6 4 4 3 3 1 2 3 4 6 5 7 6 3 3 3
    ##  [7381] 3 5 3 4 3 3 1 4 3 4 3 3 3 3 6 6 6 6 6 5 6 4 5 4 4 4 5 3 3 3 5 5 4 5 5 4
    ##  [7417] 1 5 2 2 2 2 2 2 2 1 1 2 1 4 3 4 2 3 1 1 1 3 3 6 1 4 2 4 4 1 4 5 3 1 5 1
    ##  [7453] 1 5 4 3 2 2 2 2 2 4 3 3 3 3 3 3 3 3 3 4 4 6 3 4 6 1 4 4 1 1 3 4 2 2 2 2
    ##  [7489] 5 2 2 2 1 2 2 4 2 2 2 2 3 1 3 6 6 6 4 6 6 4 5 4 4 6 4 6 4 5 5 5 5 5 4 1
    ##  [7525] 6 4 3 5 4 5 5 3 5 1 1 4 4 5 4 1 4 4 4 6 5 6 5 1 5 4 5 3 1 3 4 7 5 3 3 5
    ##  [7561] 1 4 5 5 4 3 4 4 7 5 3 3 3 6 1 1 4 2 4 5 6 4 7 2 3 5 3 3 1 2 1 3 5 5 5 5
    ##  [7597] 2 5 4 4 2 4 3 5 5 4 5 5 3 3 4 3 6 6 4 6 4 4 4 5 7 1 5 5 5 5 3 3 4 3 4 4
    ##  [7633] 4 5 1 4 5 4 4 6 6 5 6 4 4 4 4 4 6 4 5 1 1 3 6 6 6 7 1 3 1 5 3 2 2 4 6 4
    ##  [7669] 6 6 4 6 4 4 6 3 5 4 4 4 5 4 3 4 6 3 5 4 1 1 1 4 5 6 4 5 5 5 5 2 5 5 5 5
    ##  [7705] 5 5 5 5 5 5 5 5 1 4 5 4 5 5 5 5 5 5 5 3 3 3 3 1 5 5 5 5 5 5 5 5 5 5 4 4
    ##  [7741] 2 6 4 3 6 3 2 6 5 5 4 5 5 5 5 6 5 6 5 5 6 2 4 5 5 2 5 4 4 5 1 3 6 4 4 5
    ##  [7777] 1 2 2 5 4 3 1 2 4 4 4 4 6 4 1 5 4 4 6 1 5 3 5 4 5 3 6 6 3 3 6 5 4 3 4 6
    ##  [7813] 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 4 4 5 2 5 5 5 5 2 6 2 5 5 5 2 2
    ##  [7849] 2 5 5 2 3 5 5 4 3 2 4 5 5 2 5 5 5 5 3 5 5 5 4 5 3 4 5 3 3 1 1 1 1 6 4 4
    ##  [7885] 3 3 4 3 1 4 3 3 3 1 2 5 1 5 1 4 5 5 3 5 5 3 5 4 4 1 3 3 5 5 5 5 3 5 1 4
    ##  [7921] 1 4 3 3 4 3 3 2 1 1 1 6 2 4 3 1 3 5 5 3 5 6 5 5 4 4 4 5 5 1 5 4 4 4 4 4
    ##  [7957] 5 5 5 5 5 6 1 2 1 4 5 2 5 5 5 5 1 5 4 2 1 1 5 5 6 4 4 3 4 5 5 5 5 5 3 4
    ##  [7993] 3 5 6 7 2 6 6 6 2 2 2 4 4 4 1 4 4 4 4 4 4 4 3 5 4 4 6 5 5 2 5 6 2 5 5 6
    ##  [8029] 1 6 3 6 4 4 6 6 4 6 4 6 4 6 6 3 6 4 6 6 4 4 6 6 6 6 4 3 3 2 3 3 3 6 6 4
    ##  [8065] 6 6 6 4 6 1 4 5 5 1 5 1 2 6 4 5 6 2 4 4 3 6 2 3 5 4 4 1 2 6 2 4 4 4 1 1
    ##  [8101] 1 4 4 4 1 4 4 3 6 6 4 6 6 4 6 6 6 6 4 4 1 4 6 5 5 5 5 5 5 5 5 5 5 5 5 5
    ##  [8137] 1 4 5 5 5 5 5 1 5 5 5 5 5 5 5 3 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 3 5
    ##  [8173] 2 5 3 5 1 5 5 5 1 3 3 1 5 5 5 6 1 6 2 4 4 4 3 4 6 5 6 6 4 5 6 6 6 4 3 6
    ##  [8209] 4 6 6 3 4 6 4 2 6 4 6 6 6 3 2 6 6 6 5 6 2 2 6 2 6 2 6 6 4 1 4 2 6 2 2 2
    ##  [8245] 6 6 3 6 3 6 3 3 3 3 6 3 4 4 6 2 4 2 1 5 4 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ##  [8281] 5 5 5 5 5 5 5 5 2 1 1 2 5 5 1 2 6 6 4 4 5 3 5 3 5 6 4 4 6 3 4 1 6 4 2 3
    ##  [8317] 4 6 4 4 4 6 4 1 1 5 5 5 5 5 4 1 5 2 4 2 5 5 1 4 1 3 1 4 2 1 3 7 3 4 4 6
    ##  [8353] 4 4 8 1 3 6 4 4 5 4 5 1 5 1 1 5 6 3 1 6 6 6 6 3 3 3 6 3 1 5 5 5 5 1 3 6
    ##  [8389] 5 4 5 5 4 4 5 5 5 5 5 2 6 5 5 3 3 3 5 5 4 5 3 5 4 3 5 5 3 5 1 3 4 3 3 3
    ##  [8425] 4 3 3 3 3 3 3 3 3 3 3 3 3 6 4 3 3 3 3 3 6 3 3 3 3 3 3 6 3 3 3 3 3 3 3 3
    ##  [8461] 2 4 5 6 2 3 6 3 4 4 4 6 6 4 6 5 5 5 5 1 3 3 4 3 3 5 1 3 4 6 4 3 1 1 4 1
    ##  [8497] 1 4 4 5 5 4 4 5 4 2 4 4 4 6 3 5 4 4 6 3 4 4 3 4 4 6 3 4 6 5 4 5 5 4 5 2
    ##  [8533] 5 5 4 5 5 5 5 4 5 5 5 3 5 5 5 5 5 3 3 2 5 5 4 5 3 5 5 3 3 3 5 4 3 5 1 2
    ##  [8569] 2 1 4 5 1 5 5 1 3 1 5 1 4 5 4 3 5 5 5 5 5 1 3 4 3 5 5 1 1 3 4 6 3 1 1 3
    ##  [8605] 5 3 2 5 3 3 5 5 5 5 5 5 5 5 5 5 4 5 4 5 5 3 5 5 5 5 5 5 5 5 5 5 4 2 1 2
    ##  [8641] 2 2 2 2 5 3 5 2 2 5 5 4 6 6 2 1 4 4 4 7 4 5 6 5 5 5 5 5 5 5 5 5 4 5 5 5
    ##  [8677] 5 5 5 5 5 5 1 4 4 6 1 4 1 4 4 6 4 3 6 6 1 6 3 5 3 3 5 5 5 5 5 5 5 5 5 5
    ##  [8713] 5 5 5 5 2 5 5 4 6 2 2 2 2 2 2 1 5 3 6 2 3 1 6 5 4 4 4 2 2 3 2 2 3 2 2 3
    ##  [8749] 2 6 2 2 2 2 2 2 4 1 5 5 4 3 3 5 5 5 1 5 5 4 5 5 5 5 5 5 5 5 5 2 5 2 5 5
    ##  [8785] 5 6 5 5 2 5 5 5 2 5 5 2 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5
    ##  [8821] 5 2 5 5 5 5 5 4 5 5 5 4 5 5 5 2 2 5 5 4 5 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4
    ##  [8857] 3 4 4 5 5 6 4 3 1 5 6 4 6 4 6 4 3 6 4 6 3 6 4 6 4 3 2 2 2 4 2 2 3 2 2 2
    ##  [8893] 4 2 2 2 2 2 4 2 4 6 4 4 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 4 2 2 4 2 2 4 2 4
    ##  [8929] 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 6 1 1 5 4 4 3 5 6 6 3 6 5 4 5
    ##  [8965] 1 1 4 3 3 4 4 3 2 1 6 4 2 4 6 4 4 5 1 1 3 3 1 2 2 2 4 2 5 2 2 2 2 2 3 3
    ##  [9001] 5 3 5 5 5 1 5 1 5 5 5 5 5 5 5 5 5 5 5 5 6 1 3 6 1 1 1 3 1 3 3 2 3 2 3 4
    ##  [9037] 2 2 2 2 5 2 4 2 5 5 2 2 4 6 4 2 2 2 2 2 2 2 3 4 6 5 6 3 4 4 5 6 1 1 5 1
    ##  [9073] 3 1 3 1 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 4 4 3 4 3 6 4 4 6 4 3 3 4 4 1
    ##  [9109] 1 4 6 4 6 3 6 3 4 3 1 3 1 3 2 2 5 5 5 5 5 3 1 5 5 5 3 5 3 5 5 5 5 5 5 5
    ##  [9145] 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 4 5 5 4 5 5 5 3 3 4 5 3 5 5 5 1 1 3
    ##  [9181] 4 3 5 2 1 2 2 4 4 6 4 6 4 6 3 5 2 1 3 4 6 5 2 1 2 5 5 5 3 5 5 6 4 4 5 4
    ##  [9217] 5 4 5 5 5 5 3 5 4 5 6 2 5 5 1 2 3 6 6 5 5 2 5 2 5 2 2 4 5 4 6 5 6 4 6 6
    ##  [9253] 4 2 6 6 2 4 1 4 2 6 6 6 6 2 2 4 6 3 2 4 7 5 4 1 3 3 6 6 3 3 4 6 5 4 3 3
    ##  [9289] 5 4 1 1 2 1 2 5 5 2 1 2 5 3 5 5 5 1 5 5 5 5 5 5 4 4 5 3 4 1 1 5 2 4 4 2
    ##  [9325] 5 3 3 6 6 4 2 2 2 2 2 2 2 2 2 3 5 5 4 5 3 4 4 4 6 5 5 6 4 3 2 4 1 5 5 5
    ##  [9361] 5 5 5 5 2 1 3 3 3 1 1 6 3 3 5 1 3 3 3 3 6 5 4 5 4 1 2 5 5 4 4 6 6 6 3 5
    ##  [9397] 4 2 3 3 4 1 5 5 5 2 3 4 5 4 2 1 5 3 3 5 5 5 5 5 3 5 1 1 1 2 5 5 3 5 5 5
    ##  [9433] 5 5 5 5 5 5 5 4 4 4 6 6 5 6 4 3 6 3 4 3 6 6 4 4 4 6 5 6 3 4 6 4 6 3 6 6
    ##  [9469] 4 6 6 3 6 1 2 4 2 6 4 6 1 4 4 3 6 3 4 3 6 4 6 3 4 4 3 3 6 4 3 3 3 3 6 6
    ##  [9505] 6 6 6 6 4 6 4 3 6 5 4 6 3 4 1 2 4 1 2 1 2 6 2 2 2 5 2 2 5 5 5 4 5 2 5 1
    ##  [9541] 6 3 3 5 3 3 7 4 2 5 2 2 2 2 2 5 5 1 5 1 5 6 1 3 3 2 5 5 4 4 6 5 3 3 5 1
    ##  [9577] 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 3 5 4 5 4 5 5 4 4 2 3 6 4 3 2 6 5 6
    ##  [9613] 5 2 1 6 6 5 3 5 3 6 4 4 2 6 6 2 2 5 4 2 2 1 2 2 4 4 4 2 3 1 4 5 1 2 2 5
    ##  [9649] 4 2 3 4 5 5 5 5 5 5 5 5 3 4 2 6 6 4 3 3 2 1 3 3 1 3 2 4 6 2 6 4 4 4 6 3
    ##  [9685] 6 6 5 5 4 1 5 1 1 1 3 3 3 4 6 3 6 3 6 4 4 6 4 1 6 6 4 3 6 4 6 6 4 6 2 6
    ##  [9721] 6 2 2 5 5 2 2 2 2 5 2 5 2 2 2 2 5 2 2 6 2 5 5 2 5 2 6 2 2 2 4 5 2 5 2 2
    ##  [9757] 2 2 2 2 2 2 2 5 5 2 2 5 2 2 2 2 6 2 1 2 1 1 3 1 5 5 3 3 6 4 3 1 4 5 5 5
    ##  [9793] 1 1 3 6 3 3 3 3 3 3 4 3 3 4 6 6 4 6 3 5 1 6 2 2 3 4 6 2 4 4 2 6 4 2 4 1
    ##  [9829] 5 4 1 3 3 3 4 3 4 4 3 6 1 2 7 6 2 5 5 5 5 5 5 5 5 5 5 1 5 3 2 7 2 4 2 2
    ##  [9865] 2 4 2 2 6 4 6 6 5 6 6 6 5 4 6 5 5 5 4 1 2 5 3 3 4 6 5 1 1 4 5 6 7 4 5 5
    ##  [9901] 5 5 2 5 1 5 5 2 6 1 6 6 1 1 2 1 1 1 1 6 1 1 4 6 3 3 5 3 5 5 3 5 5 4 5 5
    ##  [9937] 1 3 4 1 6 4 6 4 5 4 6 5 2 4 4 6 6 6 6 4 6 5 6 6 6 6 2 5 1 5 5 5 3 3 3 5
    ##  [9973] 5 5 1 6 4 6 1 6 1 1 5 5 1 3 3 1 1 3 6 2 3 2 2 2 2 2 2 4 2 2 2 3 3 4 4 4
    ## [10009] 4 3 2 4 1 4 6 4 1 4 3 4 3 4 3 5 3 5 5 2 5 5 1 3 1 3 3 3 4 4 6 6 3 6 6 4
    ## [10045] 6 6 3 4 6 6 3 3 3 5 3 6 3 6 3 3 1 1 1 6 5 5 1 5 1 3 5 1 1 1 2 2 1 5 2 3
    ## [10081] 5 1 5 3 3 1 4 5 6 5 5 5 5 2 5 5 2 5 4 5 5 5 4 5 5 5 5 5 5 7 6 4 3 3 5 6
    ## [10117] 6 4 1 3 2 1 2 6 5 3 2 3 3 5 5 3 3 3 3 3 5 5 4 7 4 2 5 4 5 3 2 2 7 4 2 3
    ## [10153] 1 3 1 3 1 3 3 3 7 4 4 3 5 6 3 3 5 2 3 3 3 6 6 4 6 4 5 1 3 6 4 1 4 6 1 5
    ## [10189] 6 1 3 3 4 5 1 4 6 4 6 4 6 3 4 1 1 1 5 4 5 5 1 3 3 2 2 1 3 4 3 5 4 5 6 3
    ## [10225] 4 1 5 5 1 7 4 4 6 6 6 5 4 5 5 5 5 3 6 6 6 2 4 2 4 3 6 4 4 2 2 4 4 6 2 5
    ## [10261] 5 5 3 3 5 2 5 1 1 4 4 4 3 1 4 3 1 3 5 2 4 2 5 2 2 5 5 2 5 2 8 8 1 2 3 1
    ## [10297] 4 6 7 6 4 3 6 6 4 6 6 4 6 4 2 5 5 4 2 2 2 7 3 1 1 5 2 5 5 3 1 4 1 4 4 4
    ## [10333] 4 4 4 4 4 3 4 1 3 3 3 4 6 6 6 4 6 6 6 4 3 6 4 5 4 5 6 3 5 6 5 5 5 3 5 5
    ## [10369] 5 5 4 5 3 3 5 4 3 3 2 5 1 6 3 5 5 5 2 5 3 5 3 5 6 1 4 6 4 4 6 4 4 4 6 6
    ## [10405] 1 4 4 1 4 3 4 1 3 5 4 6 1 6 6 2 5 1 6 2 7 5 3 4 5 4 5 5 5 4 5 5 5 5 5 5
    ## [10441] 6 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 6 4 6 6 3 4 4 6
    ## [10477] 6 1 1 5 4 5 5 5 5 3 3 5 3 1 5 5 1 6 6 4 6 4 6 4 3 6 6 3 4 6 4 6 4 6 2 6
    ## [10513] 6 4 4 4 6 6 4 4 6 4 6 2 4 6 6 4 4 4 1 6 4 6 5 6 4 4 4 6 6 6 4 4 4 6 4 6
    ## [10549] 4 4 4 4 4 6 6 6 6 6 4 3 4 4 6 2 2 2 2 2 6 2 3 3 2 2 2 2 3 3 3 3 2 2 2 2
    ## [10585] 2 2 2 2 2 2 2 3 2 4 2 4 2 2 2 2 2 5 4 2 1 4 4 2 2 5 1 2 6 2 2 2 2 1 2 5
    ## [10621] 1 2 2 2 5 4 2 2 2 1 2 2 5 1 2 2 5 5 2 2 2 1 2 2 5 2 2 2 5 5 2 2 2 5 2 2
    ## [10657] 2 2 6 5 4 2 2 2 2 2 2 4 2 5 5 1 2 6 3 2 6 2 5 1 4 3 3 3 5 5 4 1 6 6 2 1
    ## [10693] 1 7 1 4 3 4 6 6 4 4 1 6 6 6 4 4 4 2 6 2 2 2 4 4 2 6 4 2 2 2 4 4 4 4 4 2
    ## [10729] 2 4 6 4 4 2 4 2 4 3 4 2 4 2 5 5 4 1 4 4 5 3 6 5 5 5 6 3 1 3 1 3 1 3 1 1
    ## [10765] 5 4 2 4 2 2 5 6 4 2 3 2 6 4 4 1 6 2 1 1 6 4 2 3 4 4 5 2 5 3 3 3 4 6 6 4
    ## [10801] 3 4 6 4 4 4 4 5 6 1 1 1 1 5 1 1 1 1 2 4 2 6 6 4 1 5 1 1 4 1 4 2 4 1 6 1
    ## [10837] 1 4 3 2 1 2 6 2 1 5 6 7 1 4 3 6 6 4 1 3 6 4 3 7 4 6 5 5 3 5 5 1 6 3 3 4
    ## [10873] 3 6 6 6 5 4 3 5 3 6 4 4 6 4 4 5 3 5 3 3 3 4 4 4 4 3 3 3 4 6 6 6 4 4 4 1
    ## [10909] 1 4 3 4 3 3 5 4 3 4 5 3 6 4 5 5 5 5 4 6 3 2 1 5 6 5 4 5 5 1 5 5 5 5 5 5
    ## [10945] 5 5 5 3 5 5 3 3 3 4 3 1 5 6 2 5 6 3 1 4 5 1 1 1 4 6 6 1 5 5 5 4 2 4 4 4
    ## [10981] 5 5 5 5 6 5 4 6 5 5 6 1 3 4 2 4 2 4 3 5 4 4 1 2 3 6 6 4 4 1 2 3 4 2 1 1
    ## [11017] 4 4 8 1 2 3 3 4 1 1 6 3 3 5 1 1 1 5 5 5 2 4 5 6 5 6 6 6 6 2 5 4 2 6 3 5
    ## [11053] 1 1 5 6 6 4 6 6 4 4 4 4 6 4 5 5 6 2 4 5 3 5 4 4 3 3 6 3 4 6 4 3 6 6 1 6
    ## [11089] 4 5 4 6 6 4 6 5 4 4 4 3 6 4 6 6 6 4 6 6 3 4 4 5 1 4 6 4 6 5 3 4 3 3 1 6
    ## [11125] 6 5 2 4 6 6 2 4 4 2 2 2 2 2 2 2 2 6 6 2 2 4 4 2 6 4 2 4 2 6 2 2 2 2 4 4
    ## [11161] 2 2 2 6 2 2 2 2 3 4 1 1 6 5 5 5 3 3 3 4 3 3 4 6 6 4 3 1 3 7 1 5 1 3 5 3
    ## [11197] 5 1 5 4 7 5 5 5 4 1 5 5 2 5 5 3 1 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [11233] 5 3 5 5 5 5 5 5 6 4 3 4 4 3 4 4 4 5 3 3 3 4 4 1 5 4 5 4 1 5 5 5 5 5 5 5
    ## [11269] 5 3 5 5 5 5 5 5 5 5 5 5 4 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [11305] 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 3 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## [11341] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 4 6 6 6 4 2 1 4 5 1 5 4 5 5 3 3 5 3
    ## [11377] 3 3 3 3 3 1 3 6 4 6 4 3 3 4 5 4 3 5 2 3 4 5 6 5 5 4 3 1 3 3 7 1 1 2 1 6
    ## [11413] 5 3 4 5 3 4 4 5 4 6 6 2 4 4 2 1 6 2 2 6 1 4 1 4 2 2 4 2 4 3 2 5 2 2 2 2
    ## [11449] 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 1 4 4 8 4 3 1 3 1 2 6 5 3 4 4 3 4 1 3 5
    ## [11485] 4 5 5 5 5 4 1 6 3 4 4 5 4 1 6 4 2 2 1 2 6 1 4 2 2 2 2 2 4 2 2 1 2 6 1 6
    ## [11521] 4 1 2 1 1 2 2 2 1 1 1 1 1 2 1 4 1 6 4 6 2 1 1 6 1 2 1 5 5 2 5 5 7 4 4 2
    ## [11557] 2 4 6 4 5 4 6 3 5 3 4 4 4 4 3 6 3 3 3 2 3 5 3 3 5 1 3 3 4 2 5 7 4 5 2 5
    ## [11593] 4 4 4 3 3 1 5 3 1 1 4 6 1 6 6 3 6 3 4 4 6 4 6 4 6 4 1 1 3 4 4 4 4 4 6 6
    ## [11629] 2 4 4 6 4 4 4 4 6 1 1 6 1 3 4 6 2 5 4 4 3 1 5 4 1 5 3 2 1 1 4 4 6 1 3 6
    ## [11665] 5 1 5 3 2 1 5 3 6 6 1 1 3 1 5 5 3 3 6 2 4 3 1 1 5 5 4 4 4 4 4 3 4 4 5 5
    ## [11701] 4 5 1 5 2 4 5 1 4 1 4 3 5 1 1 3 4 4 4 6 3 3 1 4 4 5 5 4 4 5 4 5 5 2 5 2
    ## [11737] 5 5 5 5 3 6 5 1 2 5 5 5 5 5 5 5 3 3 3 3 4 4 4 4 5 6 6 6 4 5 6 6 4 4 4 6
    ## [11773] 6 6 6 4 4 4 4 3 3 5 4 6 1 5 6 4 6 6 6 4 6 6 6 4 6 6 4 6 3 4 1 3 6 6 6 6
    ## [11809] 6 3 3 5 5 5 5 5 5 5 1 1 2 1 4 3 5 6 6 4 4 5 3 1 8 3 6 3 5 6 5 5 5 5 5 5
    ## [11845] 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 3 2 1 1 2 1 1 4 6 3 6 7 1 1
    ## [11881] 1 4 6 1 4 3 6 6 6 3 4 5 6 3 3 4 3 3 3 3 1 1 2 5 2 4 5 1 1 2 5 3 1 5 5 5
    ## [11917] 5 3 5 3 5 4 4 3 5 4 4 3 3 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 1 2 1 2 7 1 2 1
    ## [11953] 2 4 4 4 4 1 4 5 4 1 3 4 3 1 2 3 3 3 6 6 6 2 6 6 6 4 5 4 4 6 4 4 6 4 7 5
    ## [11989] 4 3 3 4 3 4 3 4 4 3 5 1 3 1 1 6 5 3 4 3 1 2 2 2 2 2 2 1 2 3 2 2 2 2 1 2
    ## [12025] 2 6 2 2 4 2 2 2 2 2 4 4 2 2 3 5 3 1 5 6 6 4 4 3 3 5 4 5 4 5 6 3 5 3 5 5
    ## [12061] 5 8 5 4 1 2 1 6 3 1 4 1 5 3 3 3 2 5 2 5 6 3 2 2 5 5 3 1 2 6 4 5 1 5 5 3
    ## [12097] 5 2 6 3 6 5 5 5 5 5 5 6 5 5 7 2 4 5 2 2 5 5 4 3 2 3 1 1 4 3 4 5 3 6 4 6
    ## [12133] 6 4 4 3 3 1 3 5 3 3 3 3 4 6 6 6 3 6 3 6 6 6 2 6 2 2 4 2 4 2 5 6 2 2 5 6
    ## [12169] 2 6 4 4 2 4 4 2 2 4 4 2 2 3 2 6 2 2 3 2 2 4 4 4 4 2 4 6 2 6 4 3 3 3 5 4
    ## [12205] 5 3 4 6 3 5 5 1 5 5 3 5 4 5 4 5 3 3 2 4 3 1 6 6 5 4 1 4 4 6 4 3 4 4 5 3
    ## [12241] 3 5 5 5 1 2 5 1 5 4 3 5 4 5 4 5 5 5 5 3 1 4 5 4 4 4 5 5 2 1 5 1 5 1 4 3
    ## [12277] 2 5 2 6 2 3 6 4 4 6 6 4 6 4 6 6 6 4 6 3 4 4 2 4 4 4 6 4 4 4 6 1 1 3 3 6
    ## [12313] 3 5 7 2 2 5 5 5 5 2 6 2 2 7 2 5 2 5 3 3 4 6 4 4 4 4 4 6 6 2 3 6 5 5 5 5
    ## [12349] 4 4 4 4 2 3 4 6 4 1 3 5 4 3 5 6 5 4 5 5 1 4 3 3 4 5 5 5 5 5 5 5 1 5 3 4
    ## [12385] 8 5 3 3 5 5 5 3 2 5 4 4 1 5 5 1 5 4 4 1 5 3 3 1 4 6 6 6 6 6 3 6 6 2 6 4
    ## [12421] 4 6 6 6 2 3 6 6 4 4 1 6 4 6 6 2 3 5 6 5 4 8 4 1 4 4 6 6 6 4 4 4 4 6 4 4
    ## [12457] 3 1 5 6 4 8 3 4 3 5 4 6 6 5 5 5 4 6 5 5 5 6 5 5 4 4 6 6 6 5 5 5 5 4 3 4
    ## [12493] 3 6 4 6 5 5 7 2 4 6 4 6 6 4 6 6 6 4 3 1 6 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5
    ## [12529] 5 5 5 5 5 5 5 5 7 1 4 4 5 3 7 5 3 1 3 1 3 3 1 6 2 5 2 6 4 2 3 5 4 5 4 4
    ## [12565] 4 3 6 6 4 6 4 5 5 5 5 5 5 5 5 2 5 5 3 5 1 5 5 4 6 6 6 3 6 6 4 4 4 6 3 4
    ## [12601] 5 4 4 6 2 3 1 2 5 5 5 5 1 5 1 5 3 5 5 3 4 3 5 5 2 3 5 5 5 5 5 5 7 5 1 5
    ## [12637] 3 3 4 6 3 5 5 5 6 4 6 3 3 3 3 6 6 3 1 5 3 5 5 5 5 5 3 3 3 1 2 5 3 3 2 2
    ## [12673] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 2 2 4 3 2 2
    ## [12709] 2 2 2 2 2 2 2 2 2 2 2 2 5 3 4 4 6 4 6 6 4 1 4 4 6 5 5 6 4 5 5 3 5 4 5 3
    ## [12745] 5 1 1 4 6 6 6 7 7 5 5 3 3 3 1 3 6 6 3 3 3 1 1 2 4 5 2 4 4 1 3 4 4 4 6 1
    ## [12781] 1 1 1 5 5 5 4 5 5 5 5 5 5 3 5 5 5 5 5 5 5 4 5 4 4 5 5 5 5 5 6 2 5 2 2 5
    ## [12817] 1 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 5 6 2 2 2 5 2 2 4 2 5 2 2 2 2 5 4 4 2 4
    ## [12853] 1 5 1 5 5 5 5 5 5 5 5 5 5 5 6 3 3 1 1 3 4 5 1 3 6 5 1 4 3 6 3 4 6 4 6 6
    ## [12889] 4 4 6 6 6 4 4 4 4 2 6 6 5 1 5 4 3 3 3 1 5 3 5 5 1 5 5 5 1 5 1 5 3 6 4 4
    ## [12925] 1 6 5 5 4 2 3 2 4 5 2 3 4 3 6 3 4 5 5 5 5 5 5 5 6 6 4 3 3 4 3 3 4 3 4 6
    ## [12961] 6 6 1 2 5 2 2 1 2 1 1 1 4 3 3 4 4 3 5 3 5 4 4 6 1 4 2 4 2 6 5 5 1 5 3 3
    ## [12997] 5 3 4 6 4 4 6 2 6 1 6 3 6 6 4 4 2 5 4 6 6 1 4 8 5 3 4 4 5 6 3 6 4 3 2 2
    ## [13033] 2 3 2 2 2 5 4 3 2 5 5 5 1 1 1 3 5 3 6 4 3 4 1 5 1 6 3 3 5 5 3 5 5 3 1 5
    ## [13069] 5 4 5 5 4 5 2 7 5 1 2 4 3 3 3 6 5 4 6 1 4 6 2 2 5 1 3 3 3 3 3 3 3 3 3 5
    ## [13105] 3 3 2 5 5 4 5 1 1 1 4 5 4 5 4 5 6 6 3 4 6 4 6 5 6 6 1 2 4 5 2 6 4 5 2 5
    ## [13141] 5 6 5 4 3 6 3 4 2 2 3 5 2 2 1 5 2 2 3 3 3 1 5 4 1 4 2 4 4 1 3 3 4 4 1 1
    ## [13177] 4 1 6 7 1 1 3 4 6 6 6 6 6 6 4 3 4 4 6 4 4 4 4 4 6 6 4 4 3 5 5 5 2 5 3 6
    ## [13213] 5 6 5 4 6 4 3 4 1 2 4 4 6 4 4 3 1 6 1 4 6 6 4 4 2 6 2 4 4 1 5 3 4 4 6 3
    ## [13249] 5 5 4 5 3 5 3 4 5 3 2 5 4 4 4 2 4 2 4 3 5 6 4 8 3 1 1 1 5 5 2 5 3 3 4 6
    ## [13285] 4 2 3 3 2 3 4 2 4 3 1 6 4 6 4 5 6 2 3 4 4 5 4 5 4 1 5 5 4 3 5 5 5 6 4 4
    ## [13321] 4 3 4 2 4 4 4 4 4 6 5 6 4 1 4 3 4 6 3 4 1 4 7 5 5 1 5 5 5 5 5 6 3 1 1 3
    ## [13357] 3 1 2 4 4 4 4 1 1 3 4 4 4 3 1 3 6 6 6 6 6 1 7 5 3 2 5 5 2 3 5 1 4 2 6 2
    ## [13393] 6 2 2 6 4 2 4 1 5 2 4 4 4 2 4 6 2 4 4 4 2 6 1 6 4 6 4 3 7 1 4 5 5 4 5 2
    ## [13429] 2 5 5 4 2 2 5 3 2 2 2 5 6 6 6 8 4 4 3 4 3 5 6 3 6 5 4 4 3 3 3 4 4 4 2 3
    ## [13465] 6 4 4 6 5 2 5 2 3 6 6 5 1 3 4 3 6 1 1 1 3 1 4 4 1 1 3 3 4 5 4 5 5 6 1 5
    ## [13501] 5 2 5 2 5 5 5 5 5 5 5 6 2 4 5 5 5 5 5 5 5 5 1 4 5 5 4 2 7 2 5 3 5 6 2 3
    ## [13537] 4 2 1 1 6 6 6 4 6 6 4 3 4 6 4 1 2 5 3 4 1 5 1 4 2 5 2 2 2 5 2 1 3 5 1 1
    ## [13573] 3 5 6 3 5 4 6 1 1 5 6 6 6 4 2 4 4 4 2 2 4 4 2 6 5 4 2 4 4 4 4 4 4 4 6 4
    ## [13609] 4 4 4 4 4 1 6 4 2 4 4 4 4 4 4 4 4 6 4 4 4 6 4 2 4 4 3 4 5 4 3 4 5 1 7 1
    ## [13645] 5 3 3 3 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 6
    ## [13681] 4 4 4 4 5 4 3 6 4 4 3 6 3 6 6 6 4 6 3 6 4 3 5 5 5 5 4 2 2 1 4 3 5 5 3 5
    ## [13717] 4 4 6 4 6 5 6 4 5 5 6 5 5 3 5 4 5 5 5 5 5 5 5 4 5 4 3 3 1 4 5 1 4 6 4 4
    ## [13753] 4 5 3 3 1 2 5 5 2 5 5 5 5 5 1 3 1 5 5 5 4 5 5 6 3 6 6 6 4 4 2 3 4 4 6 4
    ## [13789] 3 6 4 2 2 4 4 4 5 5 5 6 4 5 1 4 3 4 5 3 6 1 4 6 4 6 6 6 2 4 4 4 3 4 4 1
    ## [13825] 5 3 3 4 4 4 6 4 3 6 2 1 4 3 3 2 5 5 5 2 2 5 2 2 5 2 5 2 1 2 5 2 6 6 2 4
    ## [13861] 3 6 4 1 4 4 4 6 4 2 4 4 4 6 4 2 6 2 4 3 4 4 3 3 1 3 5 1 3 3 4 3 1 1 5 5
    ## [13897] 5 3 5 3 1 5 3 4 4 3 4 3 1 1 3 3 3 3 1 5 1 1 5 4 6 6 1 3 4 1 3 2 2 2 2 6
    ## [13933] 2 2 2 6 2 2 2 2 2 6 4 6 6 2 2 2 5 2 2 2 2 2 6 6 2 2 6 5 2 2 2 2 2 2 6 2
    ## [13969] 6 6 6 2 4 2 6 2 6 2 2 2 2 2 2 2 2 2 4 2 2 2 2 3 5 1 4 4 5 2 3 2 2 2 2 2
    ## [14005] 2 2 3 3 4 1 5 1 3 5 5 4 4 5 5 5 6 1 6 1 1 5 1 4 2 2 5 5 1 5 3 4 4 5 5 5
    ## [14041] 4 5 2 5 1 5 3 5 3 1 1 3 3 3 3 6 4 3 3 3 4 2 2 2 2 2 3 1 5 4 2 5 5 1 2 2
    ## [14077] 2 2 4 2 1 2 2 2 2 6 2 2 1 2 2 2 3 4 1 4 1 6 5 4 4 6 1 3 6 3 3 6 6 4 5 6
    ## [14113] 5 4 6 6 5 1 1 6 3 1 6 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [14149] 2 5 5 5 1 5 4 5 5 5 4 6 2 2 4 2 2 6 2 6 2 2 1 6 2 4 6 4 6 2 5 4 6 2 2 4
    ## [14185] 4 4 6 2 2 4 4 2 4 4 4 4 6 2 2 6 2 4 4 6 6 5 3 1 6 6 4 4 6 3 1 5 5 5 5 5
    ## [14221] 5 5 3 5 5 5 5 5 5 4 3 1 3 3 6 5 5 4 3 3 4 4 2 2 3 6 1 5 5 5 3 3 1 5 5 2
    ## [14257] 2 5 5 4 6 5 3 4 6 4 1 1 2 3 4 5 5 2 5 3 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [14293] 5 5 4 5 5 5 6 1 3 4 5 2 1 2 1 2 5 1 5 5 3 4 5 4 5 5 5 5 3 5 5 4 5 5 5 5
    ## [14329] 5 5 5 5 3 1 1 4 6 3 5 5 5 5 5 1 5 1 1 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5
    ## [14365] 5 5 5 5 5 5 6 5 5 5 5 3 6 1 5 4 3 6 4 6 5 1 3 7 2 2 2 2 2 2 2 2 2 2 2 5
    ## [14401] 2 2 2 1 4 7 4 4 3 5 3 4 4 3 4 3 3 4 2 2 1 6 1 3 1 1 1 2 6 5 5 2 2 6 4 6
    ## [14437] 4 4 6 6 6 5 6 6 4 1 1 2 5 3 1 4 4 6 4 5 2 6 5 5 5 2 5 5 3 5 5 5 5 5 5 5
    ## [14473] 5 3 4 6 3 4 3 3 5 5 5 5 3 5 5 2 5 5 5 5 5 5 5 5 2 5 5 2 2 5 5 5 5 1 5 1
    ## [14509] 3 3 4 2 5 3 6 6 5 5 2 6 4 4 3 5 4 5 5 1 1 3 1 4 6 4 6 3 6 6 4 4 4 6 3 3
    ## [14545] 4 4 4 4 4 4 3 3 6 5 6 3 1 2 2 4 2 3 5 4 4 3 4 3 6 4 4 4 3 4 4 4 6 6 5 6
    ## [14581] 4 4 4 5 5 4 4 1 3 5 5 4 7 6 1 4 4 5 6 3 1 3 5 5 1 5 3 1 1 1 6 6 3 4 3 6
    ## [14617] 4 4 6 4 5 1 1 1 3 3 5 4 3 5 1 3 4 4 4 4 4 3 4 3 5 3 8 5 3 5 5 7 3 7 4 5
    ## [14653] 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 4 4 5 4 1 3 1 3 5 4 3 4 4 6 4 4 4 4 6
    ## [14689] 6 4 6 6 4 3 6 6 3 6 5 6 5 3 4 4 2 5 2 5 5 6 4 3 3 4 3 4 4 4 3 4 4 4 3 4
    ## [14725] 3 4 4 5 7 5 3 6 3 6 6 4 4 6 1 3 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 6 5 5 5 3
    ## [14761] 5 2 3 5 5 2 6 1 5 5 4 5 5 2 5 5 5 7 1 3 3 4 5 3 4 5 6 6 4 5 3 5 5 5 5 5
    ## [14797] 5 6 3 3 4 6 6 6 1 4 4 6 4 3 1 3 3 4 4 6 3 4 4 3 6 5 4 5 4 4 1 5 4 4 3 5
    ## [14833] 2 2 2 2 2 5 5 5 5 5 5 5 4 5 5 5 5 1 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 4 6
    ## [14869] 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 6 5 5 5 5 5 1 5 5 5 5 5 5 6 5 1 6 6 6
    ## [14905] 3 6 6 1 6 4 4 6 4 5 6 1 5 5 3 3 4 4 4 4 3 4 6 1 4 5 5 1 6 3 1 5 5 5 3 5
    ## [14941] 5 5 3 3 3 3 8 1 2 1 2 2 3 3 6 1 5 5 5 3 5 5 6 6 4 4 4 4 3 4 3 3 3 3 3 4
    ## [14977] 3 3 1 5 4 2 4 5 5 5 5 4 6 2 6 4 6 5 3 1 5 3 4 1 4 6 4 4 5 6 4 6 5 3 3 3
    ## [15013] 3 3 3 5 1 2 6 2 3 2 2 2 2 4 2 3 3 3 4 4 4 4 4 5 5 3 5 6 3 4 6 4 6 6 5 4
    ## [15049] 6 3 3 3 3 1 4 5 3 5 5 1 2 5 3 2 4 2 5 6 4 5 5 3 4 4 3 4 2 2 2 2 3 2 2 6
    ## [15085] 2 2 6 2 2 5 6 2 4 1 6 4 4 6 6 6 3 6 3 3 6 6 3 3 3 3 4 1 1 1 1 3 1 4 4 4
    ## [15121] 3 4 6 4 1 5 1 3 6 1 3 5 6 6 2 6 3 3 4 4 5 4 5 3 5 4 5 1 5 5 5 5 4 5 5 5
    ## [15157] 5 5 3 5 5 5 5 5 4 2 4 4 5 1 4 6 5 4 6 5 5 7 4 3 5 3 1 5 5 6 1 6 3 3 6 6
    ## [15193] 7 4 1 2 5 4 4 4 4 6 3 6 5 2 2 4 4 4 6 4 3 3 6 2 6 6 3 2 5 1 2 1 4 5 5 4
    ## [15229] 1 5 5 4 1 2 3 5 5 2 2 5 2 5 8 1 5 1 2 4 3 2 4 2 2 4 2 2 4 2 2 4 4 4 4 2
    ## [15265] 2 2 4 2 5 5 5 4 4 5 5 5 4 1 4 5 4 5 3 2 3 5 3 1 5 5 1 5 5 5 5 4 1 5 5 5
    ## [15301] 7 4 5 3 2 4 3 5 4 6 1 1 5 2 5 5 5 4 6 3 3 4 4 3 6 1 4 8 8 6 5 4 6 6 4 6
    ## [15337] 6 4 4 4 5 1 5 6 3 1 1 2 3 1 6 6 4 4 5 4 5 6 1 2 1 1 4 1 5 3 3 4 4 5 5 5
    ## [15373] 6 1 5 5 2 2 5 1 2 5 1 5 1 1 5 5 2 6 4 2 2 2 2 7 2 4 5 5 1 5 5 6 5 5 3 5
    ## [15409] 4 5 3 4 5 3 3 3 4 3 3 4 6 6 4 4 6 3 4 1 5 3 3 3 5 5 6 5 4 5 5 4 5 3 1 3
    ## [15445] 1 5 5 5 1 2 6 2 4 4 4 3 4 6 4 6 4 6 4 6 4 3 2 4 2 4 2 4 4 4 4 3 1 3 6 4
    ## [15481] 4 6 6 3 6 4 4 4 4 6 6 3 3 4 4 3 6 6 6 6 6 6 6 3 3 3 3 3 4 6 3 3 6 3 3 3
    ## [15517] 3 3 3 3 3 3 4 4 4 1 5 2 3 3 4 4 5 3 1 3 4 7 4 4 5 4 6 3 4 3 1 5 5 2 3 5
    ## [15553] 5 5 2 5 5 2 3 1 2 5 6 3 4 3 3 1 1 4 1 4 1 2 2 4 6 6 2 6 3 3 4 2 3 4 4 6
    ## [15589] 4 3 4 3 3 3 4 5 3 3 4 4 6 6 3 4 6 6 3 4 5 4 6 5 5 3 4 4 4 4 6 2 6 3 6 6
    ## [15625] 6 4 6 6 4 4 6 6 2 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 4 3 1 4 1 3 5 5 3 2 6 2
    ## [15661] 2 4 2 2 2 3 5 2 5 5 6 5 4 5 5 5 2 4 5 5 4 5 5 5 6 4 3 3 3 2 5 3 3 2 1 3
    ## [15697] 2 4 2 2 2 2 2 3 3 3 3 4 4 6 4 6 6 6 6 5 4 6 6 6 6 6 4 1 4 1 3 4 1 4 5 6
    ## [15733] 1 3 3 4 3 1 1 3 1 1 3 1 5 6 3 5 4 3 3 3 5 5 2 1 5 6 2 5 5 6 1 5 5 5 5 5
    ## [15769] 5 5 5 6 4 5 5 5 6 5 6 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 4 5 5 5 6 4 5 5 3 3
    ## [15805] 2 3 4 3 3 5 1 1 6 6 6 3 6 2 4 3 4 2 6 6 4 6 4 5 6 3 6 5 2 6 5 4 4 6 3 2
    ## [15841] 1 5 2 6 5 6 3 5 5 1 5 2 5 5 5 5 5 5 5 2 2 2 5 5 4 1 6 3 5 5 5 2 5 2 2 1
    ## [15877] 2 5 5 2 2 5 2 5 5 5 2 5 2 5 5 6 5 2 2 3 5 5 5 5 4 4 4 3 4 4 4 4 5 5 6 3
    ## [15913] 4 4 3 4 3 6 4 6 6 5 4 5 5 4 5 3 4 5 4 6 4 6 4 2 6 4 4 4 5 6 4 4 4 6 4 4
    ## [15949] 1 6 5 4 5 1 6 3 3 5 3 6 5 5 2 6 1 4 2 2 2 2 2 2 4 2 5 1 2 2 2 2 2 2 2 2
    ## [15985] 4 4 4 4 2 1 5 4 5 3 5 4 3 3 3 3 5 1 5 7 1 1 5 1 1 3 3 6 3 5 4 6 1 5 6 4
    ## [16021] 4 4 3 6 1 4 3 5 7 4 1 5 3 1 5 5 6 4 3 5 5 5 5 5 5 5 5 5 5 2 6 3 3 4 5 2
    ## [16057] 1 3 4 1 3 3 4 5 3 4 3 1 6 3 6 4 5 5 4 5 5 5 6 5 5 5 4 3 5 5 5 5 5 3 5 4
    ## [16093] 5 4 5 5 5 5 5 5 5 5 5 5 5 4 5 4 4 5 5 5 5 4 5 5 3 4 5 4 5 3 3 5 6 1 5 3
    ## [16129] 5 3 1 4 4 2 6 4 4 1 4 6 5 5 5 5 5 5 5 5 5 5 5 2 5 6 3 1 3 6 6 1 4 6 5 4
    ## [16165] 2 4 1 4 4 5 5 6 4 5 4 6 3 2 1 5 1 5 4 5 6 5 1 2 1 3 5 5 2 6 2 2 5 5 4 2
    ## [16201] 2 5 4 2 5 6 5 2 6 4 3 6 4 4 6 3 4 6 4 3 6 6 4 3 6 6 6 4 3 3 6 4 6 6 4 4
    ## [16237] 5 4 3 4 3 4 6 5 6 4 3 3 2 4 3 5 5 5 5 5 5 3 6 6 4 6 3 3 1 3 2 4 5 5 3 4
    ## [16273] 4 4 5 4 5 3 3 3 5 5 3 5 5 5 4 5 5 5 5 2 5 2 4 1 5 5 6 5 4 4 3 1 3 6 5 5
    ## [16309] 1 5 5 5 5 5 5 5 5 4 5 5 1 5 5 5 1 6 6 6 6 5 5 4 6 6 5 3 3 6 2 4 2 4 3 5
    ## [16345] 6 2 6 5 4 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 3 3 5 5 5 5 2 5 5 6 4 6 3 6 6 6
    ## [16381] 6 6 3 6 1 4 3 3 4 6 6 6 4 6 6 4 6 6 4 6 4 3 4 5 5 1 1 1 4 1 4 5 3 3 1 5
    ## [16417] 5 1 5 1 1 1 6 1 5 6 1 5 1 5 4 1 5 5 5 3 5 5 5 5 6 5 2 5 5 5 5 5 5 4 5 5
    ## [16453] 5 5 5 4 5 4 2 5 4 4 5 1 3 1 5 1 1 1 3 1 3 7 4 5 6 4 3 6 3 4 6 6 2 4 4 6
    ## [16489] 6 6 6 6 6 4 4 4 6 6 3 3 2 5 1 5 3 3 3 5 5 5 3 4 6 4 4 2 4 4 4 4 4 4 6 4
    ## [16525] 4 6 6 1 5 6 6 4 1 4 3 5 1 1 3 3 3 1 3 3 3 3 2 4 3 1 6 6 4 6 5 5 5 3 5 4
    ## [16561] 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 2 4 2 4 3 5 5 3 2 4 3 3 3 4 1 3 3 3
    ## [16597] 6 7 3 4 7 1 5 3 5 4 1 3 3 3 5 6 3 3 2 3 4 5 5 5 5 5 3 5 5 5 2 5 3 2 1 6
    ## [16633] 4 4 6 2 2 4 2 4 6 6 1 5 1 2 1 5 3 3 3 1 5 4 4 6 5 6 6 6 6 4 3 4 5 6 2 2
    ## [16669] 4 4 3 2 3 1 5 3 1 5 1 1 3 4 4 2 6 5 4 4 4 4 5 3 5 3 6 5 4 7 4 5 5 4 4 3
    ## [16705] 4 3 4 6 4 3 4 5 6 6 3 6 4 5 4 4 4 1 3 1 5 2 6 2 3 1 1 3 4 5 1 1 6 4 1 5
    ## [16741] 5 2 5 1 3 2 4 5 5 4 5 4 4 3 3 4 4 3 6 5 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [16777] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 4 2 2 2 5 4 5 5 5 3 4 6 5 4 6 6 6 3 6
    ## [16813] 6 4 4 6 4 6 6 6 4 6 4 3 6 3 6 6 6 4 6 4 5 6 5 6 2 5 6 4 6 4 6 1 4 4 6 3
    ## [16849] 1 6 5 6 6 1 6 4 4 6 3 1 1 3 5 5 4 3 3 7 4 5 5 5 5 5 5 5 4 6 5 5 5 5 5 5
    ## [16885] 5 5 5 5 4 3 4 1 5 5 6 6 6 6 6 5 4 4 5 4 4 6 3 4 4 6 5 5 4 5 5 5 1 4 5 5
    ## [16921] 4 6 1 2 2 2 2 2 3 2 3 3 5 2 5 5 4 4 6 5 1 4 1 4 5 2 1 4 3 2 2 4 3 4 5 5
    ## [16957] 3 4 6 3 2 1 5 1 5 4 5 2 5 2 2 5 6 6 7 4 4 4 2 4 6 4 4 4 4 3 4 4 5 6 2 5
    ## [16993] 1 4 4 3 5 5 4 1 6 4 5 1 5 4 3 4 5 3 3 3 3 5 3 3 2 2 6 3 3 2 2 4 2 3 4 2
    ## [17029] 2 2 3 2 2 2 2 2 2 2 4 2 6 2 2 2 2 2 4 2 4 2 6 2 2 6 2 2 2 2 6 6 4 2 2 2
    ## [17065] 2 4 1 5 5 4 6 3 5 1 6 6 4 6 4 1 3 5 5 5 5 3 3 4 5 2 5 3 1 5 1 1 3 2 1 4
    ## [17101] 6 4 6 4 5 6 1 6 5 4 6 6 4 1 4 1 5 1 5 1 3 4 4 6 4 6 4 3 5 4 4 6 6 6 4 5
    ## [17137] 4 3 4 2 6 4 1 5 5 5 4 3 5 1 4 4 2 1 2 6 4 1 3 6 3 5 5 5 5 5 3 2 5 3 1 5
    ## [17173] 5 5 6 5 4 3 3 4 4 4 3 4 4 3 6 7 1 1 3 5 1 4 4 6 3 2 4 3 6 2 2 3 4 4 2 6
    ## [17209] 4 4 6 2 2 4 2 6 6 4 6 2 4 6 5 6 6 5 4 2 4 2 6 6 1 3 6 4 4 1 5 5 5 5 5 5
    ## [17245] 2 5 5 5 5 5 5 5 5 5 5 5 5 3 3 4 1 4 1 6 6 1 4 6 4 6 5 2 5 6 4 6 3 3 3 4
    ## [17281] 6 6 3 4 5 2 4 3 3 4 6 4 1 1 3 4 2 2 6 6 4 5 1 2 2 6 4 5 4 7 4 3 3 5 3 1
    ## [17317] 2 2 2 2 6 2 2 2 2 6 2 2 2 6 2 2 2 6 6 2 4 2 2 2 2 4 2 2 2 2 2 2 2 2 2 4
    ## [17353] 4 6 2 6 2 2 2 4 6 6 2 6 2 6 6 6 6 4 6 6 6 4 4 6 4 6 6 1 4 5 1 4 5 5 4 3
    ## [17389] 5 3 3 2 2 5 4 3 3 6 1 5 3 1 5 6 3 3 1 5 6 5 3 6 5 5 5 6 6 5 5 1 3 3 3 2
    ## [17425] 5 2 4 2 4 2 1 3 4 6 3 1 5 3 5 2 5 5 5 5 2 5 2 5 3 4 5 4 3 4 1 3 3 4 6 6
    ## [17461] 4 5 1 5 5 5 5 3 4 5 1 5 5 5 5 5 5 5 3 5 3 1 4 3 4 5 5 5 5 5 5 2 5 2 3 5
    ## [17497] 5 5 5 4 4 6 5 6 5 4 6 6 6 4 4 4 6 4 3 4 5 3 3 6 2 2 3 2 2 2 2 2 2 2 6 2
    ## [17533] 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 6 2 2 2
    ## [17569] 5 5 5 5 3 5 5 5 5 6 5 2 5 3 3 3 2 4 6 2 4 2 3 4 5 5 4 1 6 3 3 5 3 1 5 5
    ## [17605] 5 2 5 5 5 5 5 5 2 5 4 5 5 1 1 2 6 6 3 6 5 6 6 6 4 4 4 6 4 6 6 4 4 4 6 6
    ## [17641] 6 4 6 6 6 6 6 6 6 4 4 4 6 6 5 4 1 4 4 5 1 6 1 3 5 6 6 3 5 6 5 4 5 5 5 5
    ## [17677] 1 5 4 5 5 5 5 4 5 2 6 2 4 3 5 5 2 3 1 2 1 1 6 2 5 2 6 1 2 2 2 4 2 2 2 2
    ## [17713] 6 5 2 5 2 2 2 4 1 2 1 1 2 2 1 4 2 2 2 2 2 6 2 2 2 1 2 2 2 2 1 2 6 2 6 2
    ## [17749] 1 2 2 2 4 2 1 2 3 5 5 1 4 5 5 5 6 5 4 3 5 2 1 5 5 4 5 5 5 5 4 1 5 1 2 4
    ## [17785] 5 5 4 6 2 3 6 2 2 6 2 2 4 2 2 2 2 2 5 5 5 1 4 5 3 3 5 4 6 4 3 4 4 6 6 5
    ## [17821] 5 1 4 4 4 4 4 4 3 6 4 4 1 6 6 6 1 3 4 4 6 4 3 4 4 3 4 3 4 2 5 4 4 7 4 6
    ## [17857] 5 4 6 6 6 6 6 6 1 6 4 6 4 6 6 6 7 4 5 2 5 4 3 2 6 2 6 2 2 2 2 2 2 2 1 2
    ## [17893] 1 2 1 2 2 1 3 2 4 3 3 3 3 4 4 3 1 6 6 5 3 2 6 5 6 1 4 5 3 2 4 4 3 2 1 5
    ## [17929] 3 5 1 3 5 4 3 3 1 5 1 5 5 1 1 4 4 3 3 5 5 5 5 4 5 2 5 5 5 3 5 5 5 5 2 5
    ## [17965] 5 5 5 4 5 5 1 3 4 1 3 3 2 4 6 4 6 1 6 4 3 6 4 6 6 3 6 3 3 5 5 1 2 1 6 1
    ## [18001] 4 1 2 1 2 1 4 5 6 5 5 4 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5
    ## [18037] 2 5 5 6 6 2 2 2 5 5 5 5 2 4 5 2 2 2 2 5 5 2 4 5 5 2 5 4 3 5 6 6 5 4 6 6
    ## [18073] 4 4 5 5 4 5 6 3 4 3 3 3 4 4 6 6 3 8 8 1 3 4 7 5 7 4 4 5 5 5 3 1 2 2 4 2
    ## [18109] 5 3 3 3 1 1 2 6 4 5 2 1 5 5 2 4 5 4 1 6 2 2 5 2 5 3 4 3 4 3 4 3 4 1 6 4
    ## [18145] 6 5 4 3 6 6 3 4 5 3 3 1 5 5 5 5 5 5 6 3 3 5 1 5 3 5 4 3 5 5 5 3 1 1 1 1
    ## [18181] 5 5 6 6 6 3 2 2 6 2 5 5 5 6 4 1 5 3 4 4 4 2 3 2 4 6 4 5 3 3 2 2 2 2 3 8
    ## [18217] 6 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 6 3 1 3 6 2 2 4 5 5 4 4 2 6 5 5 4
    ## [18253] 4 2 5 5 6 3 5 1 4 2 2 2 3 1 6 2 6 3 5 4 4 1 5 3 3 1 5 7 1 2 5 5 5 5 5 5
    ## [18289] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 4 5 5 5 5 4 5 4 5 5 5 5 5 6 4 5
    ## [18325] 5 5 2 5 1 5 5 5 6 5 5 5 5 6 1 5 6 3 4 4 5 1 3 5 2 2 2 2 2 5 2 2 2 6 5 1
    ## [18361] 1 1 5 4 6 6 3 4 6 4 3 6 4 4 6 4 6 4 3 6 4 6 6 4 3 4 5 2 5 2 1 5 5 4 6 4
    ## [18397] 5 7 5 1 5 3 3 6 3 4 3 4 6 3 4 4 3 6 3 3 4 3 4 6 1 5 1 4 5 4 6 4 6 6 6 6
    ## [18433] 4 6 6 6 4 4 4 4 3 3 4 2 6 2 5 3 4 4 3 4 4 4 3 6 2 1 2 6 6 6 6 5 3 5 5 1
    ## [18469] 1 5 1 2 5 5 3 6 6 3 5 5 5 5 1 2 4 2 2 2 2 2 2 3 2 2 2 3 2 2 2 1 2 2 2 1
    ## [18505] 4 6 4 3 1 5 2 5 5 5 5 5 5 5 5 5 5 5 2 5 3 5 5 5 5 5 5 5 5 5 5 2 5 4 7 5
    ## [18541] 3 1 1 3 3 4 1 4 2 3 2 5 1 2 2 4 2 2 1 4 6 4 2 6 4 3 3 4 3 6 4 3 6 6 6 1
    ## [18577] 3 6 4 4 3 5 4 5 3 3 3 3 6 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [18613] 3 3 3 3 3 3 5 3 3 3 3 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 6 3 3 3 3 3
    ## [18649] 3 5 3 3 3 3 3 5 4 3 1 1 6 4 3 6 3 5 3 4 3 3 3 3 4 6 6 1 3 3 5 5 5 3 5 5
    ## [18685] 3 1 5 5 5 5 3 5 5 5 5 5 4 2 4 5 5 5 5 2 2 5 5 2 5 4 4 6 2 6 4 6 4 3 3 5
    ## [18721] 3 4 6 5 4 5 2 2 2 1 3 1 1 4 3 2 4 2 2 5 6 4 5 5 2 6 3 3 3 3 3 4 4 4 6 4
    ## [18757] 4 4 4 6 6 4 4 6 6 4 4 4 6 6 6 4 6 6 4 4 3 3 6 6 3 4 4 6 4 6 6 4 6 3 6 5
    ## [18793] 4 3 1 5 5 1 3 4 3 3 3 3 1 1 1 1 3 2 6 4 2 1 7 4 4 3 4 4 4 4 4 4 6 4 4 6
    ## [18829] 6 6 6 6 6 1 1 4 4 6 3 3 5 4 6 6 4 4 4 3 4 4 3 4 4 4 1 5 5 5 5 5 3 5 3 3
    ## [18865] 2 2 2 3 2 2 1 5 5 4 5 2 2 2 2 2 2 2 2 5 4 2 5 5 4 4 6 5 2 4 2 3 3 5 3 1
    ## [18901] 3 2 4 6 4 4 3 6 3 2 2 6 2 5 5 4 6 4 4 1 4 6 4 3 6 3 4 5 1 5 5 4 5 4 5 5
    ## [18937] 5 4 6 5 1 5 6 4 6 5 6 4 6 6 2 6 6 4 6 6 2 4 6 6 4 4 6 6 6 6 6 4 2 4 3 1
    ## [18973] 2 4 5 5 5 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 7 2 2 2 2 2 2 2 2 2 2 2
    ## [19009] 2 2 2 4 6 4 4 4 5 4 6 5 5 5 2 5 5 1 2 4 6 5 3 5 3 4 5 6 6 5 5 3 1 6 4 6
    ## [19045] 6 6 3 4 1 6 6 6 1 6 4 5 5 5 5 5 3 2 5 3 5 5 5 5 4 4 4 4 3 4 5 4 5 5 4 4
    ## [19081] 5 5 1 6 4 4 1 6 3 5 3 6 3 6 4 6 1 1 4 5 5 4 5 3 7 6 1 5 5 5 5 5 4 6 5 6
    ## [19117] 5 6 5 5 6 6 6 6 4 4 6 4 4 4 5 4 5 6 6 1 1 5 5 4 3 3 4 7 5 8 5 6 6 5 4 3
    ## [19153] 3 6 6 4 4 4 3 6 3 4 2 4 6 2 4 4 6 2 2 3 4 4 6 4 2 3 5 5 5 5 5 5 5 5 5 5
    ## [19189] 5 4 5 3 5 2 5 5 5 5 5 4 5 3 5 5 3 1 1 4 1 6 1 3 3 6 3 6 4 4 6 5 5 5 1 5
    ## [19225] 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [19261] 5 5 5 5 5 3 5 1 6 6 4 5 2 3 4 2 6 4 4 2 6 4 1 4 4 4 4 4 4 4 4 4 4 4 2 4
    ## [19297] 3 2 3 3 6 3 3 4 6 1 6 6 6 4 3 4 6 6 6 6 6 1 1 1 6 3 4 5 3 4 4 4 5 4 4 6
    ## [19333] 5 4 3 1 5 5 4 5 5 4 4 4 4 6 8 3 5 5 4 4 3 4 5 4 5 5 3 3 5 4 1 1 1 2 5 2
    ## [19369] 2 6 2 3 3 5 6 3 6 4 4 7 5 4 4 3 1 3 3 6 4 3 6 4 2 1 5 4 3 3 3 4 3 6 4 4
    ## [19405] 5 3 2 2 1 3 5 5 5 5 5 5 1 5 5 5 5 6 5 5 5 5 5 4 5 5 5 3 5 5 5 3 3 2 5 2
    ## [19441] 2 5 6 6 4 6 6 6 4 6 4 4 6 4 6 4 4 6 6 3 6 3 3 5 5 1 1 3 1 3 3 1 5 1 1 5
    ## [19477] 2 2 3 5 5 5 5 1 3 5 2 6 4 2 5 1 5 5 3 5 5 4 6 4 4 3 4 5 3 4 3 3 3 3 3 1
    ## [19513] 5 3 3 4 1 3 3 4 5 5 6 5 6 5 3 5 6 5 6 6 5 5 6 5 5 1 3 3 1 4 5 5 5 5 5 5
    ## [19549] 5 2 4 5 5 5 5 5 5 5 5 3 5 5 5 5 3 5 5 5 5 3 2 6 5 6 4 6 2 2 2 5 2 2 2 2
    ## [19585] 2 6 2 2 4 5 2 2 2 2 2 5 5 2 2 1 2 2 4 5 2 2 2 2 2 5 5 5 2 5 2 2 4 2 2 2
    ## [19621] 2 2 2 4 2 2 2 2 2 2 2 2 2 5 4 2 6 2 2 5 2 2 2 5 4 2 2 6 4 5 2 5 2 2 2 2
    ## [19657] 2 2 1 2 4 2 5 2 5 5 2 5 2 2 5 2 5 2 5 2 5 4 4 2 2 5 2 2 2 4 2 1 3 5 4 5
    ## [19693] 5 5 5 5 8 5 5 1 5 3 3 5 3 3 4 7 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [19729] 5 5 5 6 2 3 4 3 2 6 4 3 3 6 4 3 2 3 2 5 5 3 5 3 6 4 6 3 2 4 4 6 6 4 4 6
    ## [19765] 6 6 3 1 5 3 5 4 6 6 4 2 3 4 4 4 3 5 1 1 5 5 6 5 5 5 4 5 5 4 5 5 4 5 5 2
    ## [19801] 1 5 5 5 5 6 5 5 3 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5
    ## [19837] 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 2 5 2 2 2 4 1 4 1 5 4 4 4 6 6 6 3 3 6
    ## [19873] 1 1 1 5 5 3 5 5 3 4 6 6 4 6 4 3 4 6 6 6 6 6 3 1 3 3 6 6 6 4 4 6 4 4 4 6
    ## [19909] 4 3 1 3 6 4 4 6 1 3 5 3 3 3 1 3 3 3 3 3 3 3 3 3 4 1 5 5 3 7 6 4 5 4 4 5
    ## [19945] 1 3 2 1 3 2 3 3 3 3 3 3 3 3 3 3 4 3 3 7 5 4 4 4 6 5 4 5 3 4 4 4 3 3 1 2
    ## [19981] 5 5 2 6 2 2 2 1 4 2 2 1 2 3 1 5 5 5 2 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 3
    ## [20017] 5 5 5 3 3 3 3 4 4 1 3 5 3 3 3 5 3 1 3 3 6 4 4 1 1 6 6 6 4 3 6 1 4 4 4 6
    ## [20053] 4 4 6 6 6 4 6 4 2 5 5 2 5 5 5 3 1 1 4 1 4 3 3 3 5 3 6 3 3 3 4 4 4 1 5 1
    ## [20089] 4 6 1 4 4 4 4 1 6 6 6 6 1 6 2 4 2 2 3 5 1 3 3 5 1 6 6 3 6 2 3 1 3 3 6 4
    ## [20125] 4 3 1 5 3 5 4 2 1 5 5 4 4 5 1 2 3 5 2 5 4 1 2 5 5 5 2 5 5 5 3 3 4 5 5 4
    ## [20161] 3 2 5 1 5 5 5 6 2 5 5 5 5 3 6 2 4 3 6 4 5 3 1 1 5 5 3 5 5 5 5 4 5 3 5 6
    ## [20197] 4 6 6 4 3 6 6 3 3 6 3 6 6 5 5 3 3 1 4 2 5 4 6 1 6 1 4 3 4 3 1 5 3 4 4 5
    ## [20233] 3 2 5 6 2 1 4 6 4 4 4 4 4 4 2 4 3 4 4 3 3 1 3 5 3 5 5 5 5 5 5 5 5 5 2 5
    ## [20269] 5 2 5 5 5 5 5 5 5 5 4 2 5 5 2 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 6 5 5
    ## [20305] 5 5 5 5 5 5 5 3 5 4 6 8 5 5 3 5 5 1 5 5 5 6 6 4 6 6 4 3 4 6 3 6 4 5 2 5
    ## [20341] 3 3 3 3 4 5 3 4 4 3 5 1 3 4 6 5 4 4 6 3 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5
    ## [20377] 5 5 5 5 5 3 5 5 6 4 6 1 4 6 4 3 6 5 3 4 3 5 1 1 3 3 3 3 5 5 5 5 5 5 5 2
    ## [20413] 5 5 5 5 5 5 5 1 6 3 6 1 7 5 3 4 6 4 2 1 3 3 2 4 4 6 5 6 2 3 5 5 5 5 2 4
    ## [20449] 5 5 5 5 3 3 3 1 7 3 5 4 4 3 5 5 5 5 5 5 5 5 5 5 5 5 5 2 2 5 5 5 3 2 5 5
    ## [20485] 5 2 5 2 2 2 2 2 5 2 2 2 4 2 5 5 4 6 3 3 6 5 4 6 3 6 1 7 3 3 6 5 3 4 5 4
    ## [20521] 4 5 4 6 4 4 3 3 1 5 3 5 3 4 4 4 5 5 1 3 3 5 4 6 6 6 4 3 6 3 3 3 2 2 2 2
    ## [20557] 1 2 5 6 2 1 5 2 5 6 5 5 5 5 5 5 5 3 5 4 5 3 5 5 5 5 5 5 5 3 5 5 5 5 3 5
    ## [20593] 5 6 5 5 5 5 5 5 4 5 5 5 5 6 4 3 5 4 6 5 5 5 3 1 6 3 1 5 1 1 1 1 5 5 5 5
    ## [20629] 5 5 5 5 4 5 5 5 1 5 3 4 3 1 5 5 4 1 1 5 1 6 6 2 5 4 4 6 4 6 2 3 2 4 6 6
    ## [20665] 4 1 1 4 4 4 6 6 4 4 1 4 4 2 6 4 6 6 4 6 4 4 3 6 4 3 4 3 6 6 1 1 5 4 5 1
    ## [20701] 3 4 6 1 5 3 4 3 5 4 5 4 4 4 3 2 5 3 5 5 4 4 5 1 4 1 5 3 3 5 5 3 3 2 1 4
    ## [20737] 2 2 4 6 6 4 4 3 6 6 4 4 7 5 3 5 4 4 4 6 4 3 3 1 7 4 5 5 5 5 5 5 5 5 5 5
    ## [20773] 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 2 5 5 6 5 4 5 6 3 3 4 3 3 3 7 2 2 4
    ## [20809] 3 2 4 3 4 3 4 4 6 4 4 4 3 5 5 5 5 5 4 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [20845] 5 5 5 5 5 4 1 4 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [20881] 5 5 5 5 5 5 5 7 3 5 6 3 4 6 6 6 4 6 4 4 4 4 6 6 4 4 4 4 3 4 3 3 3 3 4 6
    ## [20917] 4 4 3 1 5 5 4 5 4 6 4 4 5 5 3 6 6 4 2 2 2 2 5 2 2 2 1 5 2 3 5 2 4 5 2 2
    ## [20953] 5 5 2 5 3 2 2 5 5 2 2 2 2 5 2 2 5 2 2 2 2 2 2 4 5 5 5 3 5 5 5 5 5 1 5 2
    ## [20989] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 4 3 5 5 3 5 6 4 4 3 1 5 5 3 5
    ## [21025] 1 1 1 4 6 4 6 6 6 4 4 6 2 4 6 4 2 1 5 2 6 4 5 6 3 4 6 4 4 3 3 6 2 5 3 6
    ## [21061] 4 4 3 1 2 1 2 2 1 4 5 5 5 6 5 5 5 5 4 5 5 5 5 5 5 5 5 4 6 6 6 6 6 6 6 4
    ## [21097] 4 6 6 6 1 2 5 2 3 2 2 3 5 2 1 2 6 3 2 1 2 2 7 2 2 2 2 2 2 4 2 2 2 2 2 2
    ## [21133] 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 5 4 5 4 5 5 5 6 2 1 5 4 2 5 4 4 5 3 5
    ## [21169] 3 3 4 5 5 5 5 5 5 5 5 3 4 1 3 5 4 3 5 1 5 1 8 1 5 3 3 6 4 4 3 4 4 4 6 4
    ## [21205] 6 5 3 4 6 5 5 4 2 5 5 5 2 4 4 4 5 7 2 5 2 5 1 5 5 5 4 1 5 5 5 6 5 5 5 5
    ## [21241] 5 5 5 1 5 3 6 6 4 4 4 3 6 4 3 5 3 1 3 3 1 3 3 4 4 5 6 4 4 3 5 3 5 4 5 3
    ## [21277] 4 5 1 1 4 6 3 3 6 6 6 6 3 6 6 4 4 6 4 6 6 6 6 6 4 1 4 2 6 5 6 5 6 6 4 4
    ## [21313] 4 4 6 6 2 2 2 4 4 2 2 1 1 2 2 5 2 2 2 2 2 2 4 2 3 2 6 6 2 4 3 3 5 5 5 5
    ## [21349] 5 5 5 5 3 5 5 5 5 3 5 3 3 4 3 5 3 3 3 6 5 5 5 3 6 4 3 6 1 3 1 1 5 6 1 6
    ## [21385] 5 5 5 5 5 5 5 6 4 5 5 5 5 4 5 5 1 3 5 4 5 3 1 3 5 3 6 4 1 4 4 4 3 4 6 6
    ## [21421] 2 4 4 4 6 3 3 4 3 5 1 4 3 3 1 1 6 4 6 6 4 3 1 6 3 3 5 5 5 5 5 5 5 5 5 5
    ## [21457] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 2 5 5 5 5 5 5
    ## [21493] 5 5 5 5 5 5 5 5 5 5 5 3 6 4 4 5 4 5 7 6 4 4 1 3 5 2 1 3 2 5 3 2 2 5 2 2
    ## [21529] 2 5 2 2 1 5 5 3 5 5 5 5 5 2 5 2 5 5 1 1 6 5 5 5 5 5 5 5 3 5 5 5 5 2 5 5
    ## [21565] 5 5 5 5 5 2 3 2 3 2 2 2 2 1 3 3 5 6 5 5 5 5 5 6 3 5 5 5 2 5 5 5 5 5 4 6
    ## [21601] 5 5 1 5 1 5 4 5 1 4 2 5 5 7 4 2 5 5 1 5 3 6 3 6 3 6 3 1 6 4 5 5 5 6 5 5
    ## [21637] 5 3 4 5 5 5 5 5 5 5 3 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [21673] 5 5 5 5 1 4 4 4 5 5 4 5 5 5 5 5 3 2 5 7 4 6 3 3 1 1 5 4 4 3 6 4 5 5 5 5
    ## [21709] 5 5 5 3 5 5 5 4 5 5 5 3 4 5 5 5 5 4 4 4 3 3 3 6 3 5 7 5 4 4 3 4 4 4 5 3
    ## [21745] 7 4 2 2 2 2 2 2 2 5 3 4 4 3 4 4 6 4 7 1 1 1 1 4 7 4 1 1 4 4 4 5 4 4 6 6
    ## [21781] 3 2 1 3 6 5 1 6 3 5 5 3 4 1 6 4 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [21817] 5 1 1 4 3 1 2 7 4 2 2 2 2 2 2 2 2 1 4 6 4 5 2 2 6 2 7 6 4 5 4 3 2 6 4 4
    ## [21853] 3 6 6 3 6 6 3 6 6 6 6 6 6 6 5 5 4 3 2 4 4 6 4 6 6 4 4 2 4 6 6 6 4 6 6 4
    ## [21889] 4 4 3 3 4 3 3 6 5 3 3 1 4 3 3 1 3 3 5 3 3 3 4 4 4 3 5 6 4 4 4 3 1 4 4 4
    ## [21925] 4 4 3 3 3 1 5 2 2 4 5 1 3 1 5 4 7 2 5 4 3 3 6 3 6 4 5 5 5 3 3 5 1 2 4 2
    ## [21961] 2 2 1 4 2 3 2 6 4 1 4 6 6 4 3 6 4 6 4 3 5 3 5 5 3 5 3 5 5 5 5 5 1 6 1 4
    ## [21997] 3 3 4 3 3 2 6 2 4 4 4 2 4 4 2 6 6 6 2 2 4 6 2 4 4 6 5 6 4 2 5 1 1 6 3 5
    ## [22033] 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 2 6 1 1 5 5 4 7 4 3 2 5 8 4 5 4 5 4 5 1 5
    ## [22069] 5 2 2 5 3 2 5 1 2 4 5 4 5 6 1 5 2 5 2 6 2 4 1 3 5 5 5 2 5 5 5 5 5 5 5 5
    ## [22105] 5 5 6 5 2 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 1 4 3 3 3 4 3 6
    ## [22141] 3 3 5 1 1 5 5 5 3 2 5 2 5 2 2 1 6 2 1 5 2 4 1 5 3 5 4 4 1 4 1 4 3 4 2 4
    ## [22177] 4 6 6 4 4 3 3 5 5 6 3 5 3 3 4 4 4 6 4 6 4 4 6 6 4 6 6 6 2 5 5 5 5 5 5 4
    ## [22213] 4 3 5 5 5 5 5 5 5 5 5 5 5 5 5 1 4 5 1 1 5 5 3 1 5 5 1 4 5 5 5 5 4 1 5 5
    ## [22249] 5 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 4 3 6 4 6 6 6 6 4 6 6 3 6 6 6
    ## [22285] 6 6 4 6 4 6 3 5 6 4 4 4 5 6 4 5 1 3 3 5 6 4 3 5 3 6 4 6 5 6 1 5 6 2 2 3
    ## [22321] 5 4 5 4 1 1 4 6 6 4 6 4 3 6 4 6 3 6 6 6 6 4 3 6 4 5 4 4 6 4 4 3 2 2 4 5
    ## [22357] 5 5 5 5 5 5 1 3 3 4 3 1 3 1 4 6 4 1 4 3 5 3 4 5 4 3 1 4 5 3 6 6 1 6 2 3
    ## [22393] 5 3 4 3 1 6 5 3 5 5 1 5 1 5 6 2 3 1 4 5 4 5 5 4 3 6 8 2 1 3 4 4 4 4 3 5
    ## [22429] 1 4 5 2 5 1 5 4 5 5 5 6 5 3 4 4 6 6 6 6 3 2 6 4 3 1 3 1 7 4 4 4 1 3 3 5
    ## [22465] 2 2 2 2 2 2 2 2 2 2 2 5 4 1 2 2 2 2 2 2 2 1 2 2 5 2 5 4 4 1 5 6 4 4 5 4
    ## [22501] 5 4 6 4 1 5 3 5 4 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 4 3 3 3 6
    ## [22537] 6 6 2 3 3 6 3 3 1 1 1 3 3 1 5 4 1 3 6 3 3 6 6 4 6 3 3 4 3 4 4 6 4 6 4 4
    ## [22573] 6 2 3 2 2 2 2 2 2 3 2 4 6 5 2 2 5 3 2 2 4 2 5 2 1 2 2 2 2 2 2 6 7 4 4 3
    ## [22609] 2 1 5 5 4 2 4 2 6 6 2 6 6 6 6 4 2 2 6 4 5 3 2 5 1 7 5 5 5 5 5 5 2 5 5 6
    ## [22645] 5 2 5 5 5 2 4 2 5 5 3 2 1 5 5 6 6 4 6 5 5 2 5 5 4 2 2 2 2 5 2 2 2 5 2 2
    ## [22681] 1 3 4 3 6 6 3 1 4 5 3 3 6 6 5 5 2 3 4 5 2 5 5 4 4 5 2 1 3 4 1 6 3 3 1 5
    ## [22717] 4 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5
    ## [22753] 5 5 5 2 4 1 6 5 4 6 3 5 3 5 7 5 4 4 3 4 1 6 1 4 4 3 4 4 4 2 2 5 1 3 1 3
    ## [22789] 3 3 3 5 5 2 5 3 5 5 6 5 5 4 6 2 5 5 5 5 2 5 3 5 2 5 5 5 1 5 3 2 5 2 5 5
    ## [22825] 3 5 5 3 4 1 5 5 6 5 5 3 3 4 6 6 1 4 2 3 6 6 3 5 5 3 3 1 1 1 3 1 6 3 4 4
    ## [22861] 2 5 4 4 3 4 5 1 4 5 1 5 2 3 3 3 6 2 3 5 2 3 5 2 6 4 2 5 5 5 1 6 3 3 3 1
    ## [22897] 5 4 6 6 4 6 4 6 5 3 4 6 5 6 6 6 5 6 3 3 5 2 6 2 2 2 2 7 4 4 2 2 2 2 2 2
    ## [22933] 4 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 6 6 4 6 4 6 4 1 3 1 6 3 6
    ## [22969] 1 6 1 3 5 6 6 3 6 4 4 6 6 6 4 6 6 4 5 5 4 7 1 5 6 5 1 6 3 4 1 5 1 5 5 3
    ## [23005] 5 5 5 5 3 5 5 5 5 1 3 5 5 5 5 5 5 4 5 5 5 5 4 6 6 4 6 6 6 6 6 4 4 6 4 6
    ## [23041] 6 4 6 4 3 4 4 6 6 3 4 4 4 4 3 4 4 4 1 3 4 5 5 1 5 5 5 6 5 4 5 4 3 1 1 2
    ## [23077] 6 2 2 2 2 2 4 2 2 6 5 2 4 2 2 2 2 2 1 2 2 2 6 2 2 2 2 2 5 1 6 6 1 4 5 4
    ## [23113] 3 6 1 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 4
    ## [23149] 4 3 4 4 4 4 5 4 4 3 5 5 5 4 4 3 3 4 3 1 1 5 5 4 5 4 4 4 6 4 4 4 5 6 6 6
    ## [23185] 6 4 4 1 6 6 6 6 3 6 4 4 5 1 5 6 6 3 6 5 5 1 6 6 6 5 3 3 3 3 3 4 3 6 3 4
    ## [23221] 6 5 3 3 4 2 3 6 4 2 6 2 2 6 6 3 3 6 6 6 3 2 5 5 5 4 4 6 6 6 4 6 6 6 4 4
    ## [23257] 3 4 4 4 6 6 3 3 1 4 3 5 5 4 4 3 4 6 1 4 3 4 3 5 5 3 3 3 1 5 6 6 6 6 6 5
    ## [23293] 4 5 4 4 6 6 6 6 6 6 6 3 8 3 5 3 4 1 5 5 5 1 3 4 4 3 4 5 3 4 7 4 2 6 5 1
    ## [23329] 1 1 3 4 1 6 1 3 4 4 7 2 3 1 1 4 5 4 5 3 1 6 4 3 6 5 5 4 5 5 3 5 4 5 4 5
    ## [23365] 3 4 5 6 6 5 5 4 5 5 2 2 6 4 4 6 2 6 4 3 2 2 5 5 3 6 3 3 3 3 1 3 5 2 3 3
    ## [23401] 5 5 5 2 5 5 5 5 2 5 5 5 3 5 5 4 5 5 5 5 7 5 5 5 5 2 5 5 3 5 4 6 3 1 3 5
    ## [23437] 1 1 5 6 3 2 2 2 2 2 5 2 2 2 2 2 2 2 6 1 2 2 2 2 2 4 2 2 2 2 2 5 2 2 2 4
    ## [23473] 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 5 1 4 4 3 4 4 4 6 4 6 4 5 4 1
    ## [23509] 6 5 6 3 6 6 3 6 4 3 4 4 6 1 3 3 5 2 5 6 4 6 6 2 4 4 6 4 5 5 6 4 5 5 4 4
    ## [23545] 4 5 5 2 4 3 2 4 5 5 2 1 4 4 4 4 6 6 6 1 5 6 6 4 5 6 4 1 6 6 6 6 5 4 5 5
    ## [23581] 2 5 1 5 4 4 6 6 5 5 1 5 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 6
    ## [23617] 2 6 2 3 3 2 3 5 5 5 4 6 1 1 3 4 6 6 4 6 4 3 3 4 6 6 4 8 4 6 1 6 6 4 6 6
    ## [23653] 4 6 5 4 6 2 4 3 5 6 4 4 1 2 1 5 1 2 3 3 5 1 4 1 7 6 5 5 1 6 5 6 5 4 3 5
    ## [23689] 3 5 1 3 5 2 1 5 2 5 6 6 5 5 2 5 6 4 6 4 3 4 4 3 1 2 1 1 5 3 5 3 5 1 3 3
    ## [23725] 5 4 7 5 5 5 3 1 4 4 4 3 4 4 4 4 3 3 3 4 2 2 6 4 6 4 6 6 5 2 4 1 5 5 5 3
    ## [23761] 5 2 5 1 2 4 4 5 4 4 2 2 5 4 3 5 6 6 6 3 5 5 5 6 3 1 1 4 5 5 5 3 5 5 5 4
    ## [23797] 5 5 5 5 5 5 5 3 3 6 1 4 6 2 5 6 4 3 4 6 6 6 4 4 4 1 5 5 5 5 5 5 5 5 5 5
    ## [23833] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5 4 6 4 5 5 5 4 6
    ## [23869] 5 5 5 5 6 5 3 5 4 5 6 5 5 4 5 6 6 5 6 4 5 5 5 4 2 1 4 5 2 6 4 4 3 3 6 1
    ## [23905] 1 5 3 4 1 5 5 3 3 4 3 3 3 3 1 3 5 5 5 5 6 6 5 3 3 3 1 1 5 4 3 4 4 4 4 5
    ## [23941] 1 4 5 1 4 5 4 6 5 5 1 1 2 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2
    ## [23977] 1 2 2 2 2 2 1 2 2 1 1 2 1 1 1 1 2 1 2 1 1 2 1 1 2 1 1 1 1 1 1 2 1 2 1 2
    ## [24013] 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 2 2 2 1 1 1 2 2 1 1 1 2 2 2 1 1 1 2 1 1 2
    ## [24049] 1 2 2 1 1 2 2 5 1 1 1 1 1 2 1 2 1 1 2 1 1 1 1 2 1 2 1 1 1 2 2 2 1 1 1 2
    ## [24085] 5 1 1 1 2 1 1 1 1 2 1 2 2 1 1 1 2 1 2 1 1 4 3 3 4 5 2 3 3 3 6 4 4 3 4 1
    ## [24121] 5 3 4 5 5 1 3 4 4 6 1 4 3 6 4 3 3 4 6 6 6 4 3 3 4 6 4 6 6 4 4 3 3 4 5 6
    ## [24157] 4 5 5 5 3 4 5 3 4 6 4 6 4 6 4 4 5 6 1 3 2 5 3 3 4 4 4 3 4 3 3 3 4 4 1 3
    ## [24193] 3 3 4 3 3 4 1 5 7 2 5 4 4 4 3 4 4 5 6 6 4 4 6 5 5 5 1 5 5 5 5 4 4 1 4 6
    ## [24229] 3 4 5 3 3 1 3 4 3 4 6 4 4 6 1 4 6 4 3 6 3 4 4 6 1 1 3 4 3 3 5 6 4 3 1 6
    ## [24265] 5 1 5 3 4 5 3 1 6 4 4 5 4 6 6 1 4 6 6 4 6 6 6 1 5 6 4 6 6 5 6 7 4 6 6 2
    ## [24301] 2 2 2 2 2 6 6 2 6 2 2 4 2 4 4 2 2 6 2 2 4 2 4 6 3 5 3 1 2 3 5 3 3 5 4 5
    ## [24337] 4 6 3 4 6 5 4 1 6 3 4 4 5 4 6 4 5 4 6 5 5 5 5 6 6 4 4 4 6 5 2 4 4 5 3 1
    ## [24373] 3 6 4 2 6 3 3 2 1 3 1 6 6 1 4 4 6 5 1 3 4 6 4 4 4 5 3 4 3 3 3 3 5 5 4 2
    ## [24409] 2 2 3 2 2 4 2 2 6 5 2 4 4 4 6 6 1 1 2 1 4 1 2 1 3 5 4 5 5 4 5 3 4 1 3 4
    ## [24445] 5 6 6 2 6 6 3 6 4 3 5 3 2 2 2 3 2 2 4 2 4 3 4 5 5 2 4 2 6 2 4 3 1 5 2 5
    ## [24481] 1 6 4 4 2 2 1 2 2 3 5 1 5 5 3 5 5 5 5 3 4 5 5 5 5 5 5 4 6 6 6 4 1 5 6 3
    ## [24517] 3 1 4 6 6 1 4 3 4 6 6 1 4 3 5 3 6 3 5 3 1 6 4 4 4 4 4 6 4 4 4 4 6 4 6 6
    ## [24553] 4 6 3 3 3 5 3 3 8 4 2 6 3 3 2 4 6 4 6 4 6 6 6 3 6 5 5 6 6 4 6 1 4 4 6 6
    ## [24589] 3 4 6 4 5 1 5 4 5 5 1 4 1 1 1 4 6 5 6 6 6 6 4 6 5 5 5 5 7 4 5 5 1 3 2 3
    ## [24625] 5 1 5 7 4 1 6 4 3 1 6 4 3 6 4 5 2 4 6 2 6 5 5 6 3 4 6 4 6 4 5 6 4 3 1 4
    ## [24661] 1 4 3 4 3 4 2 5 5 4 6 5 2 5 5 4 5 5 1 1 5 3 1 4 5 5 3 2 5 5 3 6 5 6 6 6
    ## [24697] 6 6 6 6 6 6 5 3 4 3 4 1 2 6 4 4 4 6 1 2 6 2 2 5 2 5 4 4 2 5 2 2 6 4 4 6
    ## [24733] 4 6 4 4 6 4 1 4 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 3 1 3 3 5 3 4 6 5 1 5 5 5
    ## [24769] 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 4 5 3 1 4 3 5 1 1 3 6 7 6 3 6 6 6 1 4 3 6
    ## [24805] 5 1 5 4 2 6 4 6 4 2 6 4 5 6 6 4 6 4 5 5 6 6 6 1 5 5 3 5 5 1 5 3 6 2 5 5
    ## [24841] 1 5 5 5 5 5 1 5 3 5 1 3 4 3 4 4 4 3 4 5 3 4 4 4 4 4 3 6 6 6 6 3 6 2 4 4
    ## [24877] 4 6 6 6 6 6 6 6 6 4 6 3 3 4 1 6 6 6 6 6 4 6 5 3 4 3 5 5 5 5 3 5 5 5 5 5
    ## [24913] 5 5 1 5 1 1 1 3 4 1 5 1 3 5 4 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5
    ## [24949] 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 4 2 4 3 4 4 5 1 4 3 6 1 4 4 6 2 3 6 6
    ## [24985] 1 5 1 6 3 1 2 4 2 2 2 2 2 2 2 2 2 2 2 5 2 2 2 4 2 2 2 2 2 2 4 4 3 5 1 5
    ## [25021] 4 5 5 1 4 1 5 5 4 6 3 6 4 2 6 6 6 2 4 4 4 2 6 4 5 3 6 6 1 6 4 4 3 3 1 3
    ## [25057] 3 5 5 6 4 4 5 6 5 3 1 5 5 1 5 3 5 3 3 4 6 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [25093] 5 5 5 5 5 3 4 1 5 1 5 5 4 5 5 3 5 1 2 1 1 3 6 4 1 5 6 4 4 6 4 2 5 1 6 2
    ## [25129] 4 4 1 6 1 4 2 3 5 1 1 1 1 1 1 1 1 7 2 2 6 2 6 3 2 5 2 2 2 6 2 3 3 3 3 3
    ## [25165] 3 3 3 3 1 1 4 5 1 5 5 2 4 5 1 1 5 4 5 3 1 2 1 5 4 3 4 4 1 5 1 3 1 2 6 4
    ## [25201] 6 3 6 4 6 4 4 4 6 4 4 4 6 4 1 6 6 6 3 6 4 3 3 5 5 6 3 1 5 4 6 4 6 6 1 6
    ## [25237] 6 4 6 4 4 6 6 3 4 4 6 6 4 6 6 6 6 4 3 4 6 7 6 1 3 3 5 1 3 1 3 4 4 4 1 3
    ## [25273] 3 4 5 5 5 5 5 5 3 6 5 5 5 4 6 5 6 6 4 4 4 4 4 1 6 4 4 1 3 5 3 4 4 4 4 3
    ## [25309] 6 4 6 1 2 6 4 6 4 6 1 4 2 5 3 3 4 5 4 4 4 5 4 1 5 4 2 4 4 4 2 4 5 4 4 3
    ## [25345] 5 4 3 4 8 5 6 1 4 4 3 2 4 4 2 6 4 4 5 3 4 4 6 6 1 5 5 5 5 3 5 4 6 6 2 2
    ## [25381] 2 2 2 6 5 6 2 4 6 6 2 6 2 6 2 6 3 6 3 3 3 5 4 6 6 5 2 1 1 1 8 4 2 2 2 1
    ## [25417] 3 6 2 2 2 2 2 2 2 2 4 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [25453] 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 3 2 3 4
    ## [25489] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 2 2 5 5 5 5 5 5 2 5 5
    ## [25525] 5 5 5 5 5 5 5 5 4 6 6 6 4 4 6 6 6 3 5 5 3 1 7 3 4 5 1 5 1 1 3 4 2 4 4 1
    ## [25561] 4 6 1 2 4 1 4 1 6 3 6 4 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 3 1 5 4 6 5 1 3 5
    ## [25597] 5 3 4 5 4 1 4 3 6 4 6 5 5 4 6 1 5 6 5 6 3 5 2 4 3 3 5 5 5 5 5 5 5 5 5 5
    ## [25633] 4 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 2 5 4 5 7 4 4 3 6 5 5 3 3 2 2 4 2 2 4 2
    ## [25669] 5 5 5 3 5 5 5 4 4 4 5 5 4 4 4 5 2 6 4 4 4 6 6 4 4 6 3 4 6 6 4 1 5 2 5 5
    ## [25705] 5 5 5 5 5 5 5 5 5 5 5 3 5 5 6 6 2 5 5 2 4 5 5 4 3 6 1 4 6 4 3 4 4 6 3 4
    ## [25741] 3 3 4 4 4 3 5 5 5 1 4 4 3 5 3 5 6 1 3 8 5 5 5 5 5 5 5 5 5 4 5 5 5 2 5 5
    ## [25777] 5 5 2 5 5 3 5 3 3 5 3 3 1 3 2 2 2 2 2 2 6 2 2 6 1 2 1 2 2 2 2 2 2 2 2 2
    ## [25813] 2 2 2 6 2 5 2 2 2 2 1 5 2 2 3 3 4 2 6 6 2 4 7 1 5 8 4 5 5 3 2 2 2 2 2 2
    ## [25849] 2 4 4 4 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 6 2 2 2 2 2 2 4 2 4 2 2 5 5 5 5
    ## [25885] 3 5 5 5 4 4 5 5 5 5 4 5 6 5 5 3 5 5 5 5 5 4 5 5 5 2 2 5 3 4 1 6 3 3 5 3
    ## [25921] 5 4 4 3 5 1 5 6 5 5 4 4 5 4 5 5 5 5 6 5 5 4 5 2 5 5 4 2 5 5 2 4 2 5 5 5
    ## [25957] 5 6 2 4 2 4 5 6 5 5 6 4 2 2 5 5 1 3 1 7 4 3 6 6 6 6 6 6 4 6 6 4 6 6 6 4
    ## [25993] 4 4 4 3 6 6 4 4 6 6 6 4 6 6 1 3 1 6 5 6 4 6 7 5 5 5 5 5 5 5 5 2 5 5 5 5
    ## [26029] 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 4 5 4 5 5 5 5 5 5 5 5 5 5
    ## [26065] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5
    ## [26101] 5 1 3 1 4 6 2 6 6 4 2 4 4 4 3 2 5 2 4 4 4 3 5 2 4 4 2 2 5 3 5 1 5 5 3 5
    ## [26137] 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 3 5 5 5 5 5 5 4 5 1 3
    ## [26173] 1 5 2 4 5 4 5 5 5 3 5 5 1 4 1 3 4 8 5 5 1 1 2 5 6 6 5 3 4 5 6 4 4 6 2 1
    ## [26209] 2 2 2 3 4 2 2 5 5 5 3 3 4 6 6 4 4 6 6 3 4 6 4 5 4 5 4 5 5 5 4 5 3 5 3 5
    ## [26245] 5 3 2 3 6 6 3 4 4 6 4 4 4 6 6 6 6 4 6 6 4 6 4 6 6 4 5 6 6 6 4 4 4 6 6 4
    ## [26281] 6 4 5 6 6 4 4 5 6 8 1 3 1 6 5 4 4 4 2 1 6 6 2 2 2 4 2 4 3 6 6 5 3 1 1 5
    ## [26317] 3 3 4 2 2 4 4 5 2 6 4 3 6 4 6 6 2 5 5 5 3 6 5 5 4 6 5 4 6 5 4 3 5 2 5 5
    ## [26353] 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 2 5 5 5 5 5 5 5 5 4 7 3 3 5
    ## [26389] 5 1 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 3 3 1 1 5 6 5 4 5 5
    ## [26425] 5 2 6 3 5 5 1 5 5 2 5 1 5 5 6 6 5 2 6 4 5 3 4 4 5 5 5 4 4 4 4 2 1 4 3 1
    ## [26461] 7 3 6 6 4 4 6 6 1 4 5 7 1 5 5 2 5 5 1 2 5 2 5 6 6 2 2 2 6 6 5 4 5 4 5 3
    ## [26497] 5 2 2 4 6 6 6 4 4 4 5 5 6 1 3 3 6 3 4 3 6 3 4 4 5 6 6 4 1 2 5 6 2 4 4 6
    ## [26533] 6 5 5 5 3 1 2 5 1 5 5 5 3 5 5 3 6 4 5 2 4 4 5 1 5 5 4 1 5 5 5 5 5 5 3 5
    ## [26569] 2 5 5 5 5 5 5 5 5 5 5 6 3 3 4 6 3 3 4 2 5 5 3 2 5 5 5 4 4 6 2 6 4 4 4 4
    ## [26605] 4 4 6 5 4 6 1 4 5 5 4 5 5 5 5 2 3 1 5 4 5 5 5 3 3 3 5 5 5 3 6 3 5 3 3 5
    ## [26641] 5 5 5 5 5 4 4 4 1 4 4 5 6 4 6 6 6 6 6 4 6 6 5 2 6 2 6 4 6 3 5 6 3 6 4 4
    ## [26677] 4 2 2 4 4 2 2 5 1 5 3 3 5 4 5 3 3 1 6 4 1 6 5 2 2 4 4 4 5 2 2 2 2 2 2 2
    ## [26713] 2 2 2 2 2 2 1 5 2 2 5 2 2 2 5 4 2 2 6 5 2 2 2 5 5 2 2 2 2 2 2 3 4 5 6 6
    ## [26749] 6 3 6 4 1 6 6 4 5 3 1 3 3 3 6 4 1 1 4 2 4 4 7 4 3 3 1 3 2 6 3 1 3 6 4 5
    ## [26785] 4 5 1 1 5 6 4 2 2 4 3 1 4 6 4 3 5 5 5 3 5 3 5 3 4 4 6 5 6 3 3 5 3 5 5 3
    ## [26821] 5 5 4 1 1 1 5 1 5 3 1 3 1 2 3 1 4 2 1 5 3 1 4 1 1 5 3 4 4 5 5 6 4 6 5 3
    ## [26857] 4 3 3 5 1 2 5 4 5 5 2 5 5 3 5 1 1 1 3 1 1 4 6 1 4 6 4 1 4 5 4 5 3 3 4 4
    ## [26893] 3 4 3 3 3 3 4 4 3 1 1 6 3 3 3 1 3 3 4 1 4 4 3 3 1 4 1 4 4 4 3 4 4 1 5 1
    ## [26929] 5 5 4 3 5 5 6 5 4 5 5 3 6 4 6 4 5 5 5 1 5 4 3 6 4 6 6 6 4 4 6 6 4 6 6 6
    ## [26965] 6 4 1 1 5 5 3 5 1 1 3 3 4 2 4 4 6 6 1 5 1 3 3 5 3 4 1 1 3 4 4 1 3 5 1 4
    ## [27001] 5 3 4 6 6 3 6 4 4 6 6 3 3 5 3 3 2 1 2 4 5 3 5 1 4 5 5 3 4 1 4 4 4 5 5 5
    ## [27037] 5 5 5 4 4 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 6 5 5 5 5 5 5
    ## [27073] 5 5 5 2 5 5 5 2 5 5 4 5 2 4 2 5 4 2 2 2 2 2 2 4 6 4 2 3 2 2 4 4 4 4 4 1
    ## [27109] 3 6 5 5 5 4 5 4 2 5 5 1 3 5 1 5 2 1 3 1 1 5 5 4 7 5 3 3 5 5 1 1 5 5 5 5
    ## [27145] 3 3 5 4 5 3 1 4 3 3 3 1 5 1 5 1 3 3 1 1 3 1 1 1 3 5 3 4 2 4 3 1 5 5 5 6
    ## [27181] 1 1 4 3 1 4 4 4 1 4 4 1 4 3 5 3 5 5 4 4 6 6 3 6 6 4 6 6 3 6 3 4 3 4 5 5
    ## [27217] 5 5 5 5 5 5 5 4 5 5 5 6 5 6 1 3 1 4 4 3 4 1 6 3 3 5 5 5 5 2 2 2 2 6 1 2
    ## [27253] 4 5 5 3 1 1 1 6 6 5 1 3 3 4 4 4 4 1 5 4 5 4 5 2 2 3 2 4 2 2 2 2 4 2 2 2
    ## [27289] 2 4 3 2 2 2 2 2 2 6 2 4 2 2 2 2 4 3 3 6 4 6 6 4 4 2 2 3 4 6 3 2 6 2 4 4
    ## [27325] 6 6 4 6 4 4 6 6 6 6 6 3 5 5 5 5 6 3 5 4 5 4 6 1 3 6 4 3 3 3 6 6 4 4 6 4
    ## [27361] 7 5 4 5 3 4 4 3 1 1 3 6 3 2 2 1 2 2 5 4 4 2 2 4 4 4 4 2 4 2 2 2 5 3 5 2
    ## [27397] 5 5 4 4 3 4 4 6 6 4 6 3 4 5 1 5 5 1 2 4 4 3 1 2 4 5 1 5 4 3 3 4 5 5 5 5
    ## [27433] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 3 1 3 3 4 6 4 5 4 6 2 5 2
    ## [27469] 1 3 5 2 3 3 2 2 4 4 5 4 4 4 3 5 4 2 4 5 6 3 2 5 1 3 1 4 3 3 4 4 5 2 5 2
    ## [27505] 5 5 5 2 5 6 6 2 5 5 6 2 4 2 4 2 6 4 6 6 4 4 6 4 4 4 4 1 6 4 4 4 4 4 6 4
    ## [27541] 6 6 6 4 4 6 6 4 2 6 4 4 4 4 6 4 4 6 6 1 4 6 4 4 6 4 4 6 4 4 6 6 6 4 6 4
    ## [27577] 4 6 4 4 6 4 6 4 6 6 6 4 4 6 4 4 4 4 6 2 6 6 6 4 4 6 4 6 4 1 4 6 6 6 6 6
    ## [27613] 6 6 4 6 4 6 6 1 4 3 6 3 4 5 1 1 2 1 1 3 2 4 5 1 4 2 6 4 4 4 4 1 5 4 2 2
    ## [27649] 2 2 6 4 2 2 8 4 2 4 4 6 3 4 4 2 4 6 6 2 6 4 6 6 5 5 1 5 5 5 5 5 5 5 2 5
    ## [27685] 5 5 5 5 5 5 5 5 5 5 3 1 1 6 6 3 6 6 1 5 7 6 3 5 2 1 5 6 5 6 2 4 4 6 1 6
    ## [27721] 4 4 4 3 5 5 3 5 5 5 5 5 5 5 3 4 2 1 1 5 3 3 4 3 1 2 6 5 5 4 4 4 2 5 5 3
    ## [27757] 4 2 4 6 6 1 5 3 4 3 1 6 2 3 1 3 6 2 1 6 1 6 4 3 3 1 5 5 5 3 3 2 4 4 7 4
    ## [27793] 5 3 5 5 6 4 5 5 5 5 5 3 5 5 4 1 4 1 6 7 4 2 4 4 4 3 4 4 4 6 1 3 1 5 5 3
    ## [27829] 6 2 4 3 2 5 1 5 5 1 3 3 2 2 2 3 5 3 4 3 5 5 5 1 1 2 3 1 3 6 6 3 3 4 6 6
    ## [27865] 6 4 4 4 6 6 6 4 6 6 3 3 6 1 1 2 3 5 4 6 6 6 3 3 5 3 3 6 4 1 4 3 2 3 3 3
    ## [27901] 6 4 4 1 6 4 3 6 2 2 4 7 4 3 6 6 3 3 6 6 3 4 4 4 4 4 2 4 4 4 4 5 4 5 4 5
    ## [27937] 4 4 4 4 4 1 4 4 5 5 3 4 4 3 3 3 1 1 5 1 3 5 5 2 3 5 5 2 5 5 5 5 5 5 5 3
    ## [27973] 4 3 6 2 6 2 3 8 3 4 6 3 6 2 6 1 4 4 4 5 3 4 4 3 4 4 4 6 3 4 3 5 5 5 4 3
    ## [28009] 1 1 3 2 5 4 4 5 5 5 1 5 4 3 5 3 4 3 6 1 3 5 5 5 5 5 1 5 5 5 5 5 5 3 5 4
    ## [28045] 2 2 2 5 2 5 4 2 2 2 5 2 5 2 2 2 2 5 5 3 1 6 4 4 5 1 3 1 5 6 4 4 4 4 3 5
    ## [28081] 6 5 7 4 3 1 5 5 5 5 5 1 5 5 5 5 2 4 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 4 5
    ## [28117] 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 3 5 5 5
    ## [28153] 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 6 3 4 4 4 3 5 4 6 3 1 1 5 4 3 1
    ## [28189] 6 5 1 3 5 4 4 4 3 4 3 4 4 5 4 1 1 3 1 4 3 4 3 2 3 3 5 3 1 5 4 5 5 5 5 5
    ## [28225] 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3
    ## [28261] 1 3 3 4 1 6 6 3 6 6 4 6 6 2 3 3 1 3 3 5 3 5 5 3 3 4 4 3 3 1 6 4 6 6 6 3
    ## [28297] 5 5 3 6 6 4 3 4 6 3 3 1 4 1 5 5 5 6 6 5 4 1 3 5 4 4 6 4 6 6 3 3 1 3 3 5
    ## [28333] 3 5 3 3 4 6 4 6 7 4 4 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 6 4 4 2 4 3 3
    ## [28369] 3 3 4 5 5 1 1 4 4 6 3 6 3 3 5 6 4 4 1 1 4 4 3 2 4 5 2 2 2 2 2 2 2 3 2 6
    ## [28405] 2 2 2 2 2 2 2 5 4 5 4 4 1 5 6 6 4 4 6 3 2 6 4 3 4 3 4 4 5 2 4 5 5 5 1 4
    ## [28441] 5 1 5 5 3 3 5 2 5 5 4 4 5 5 1 6 1 3 6 4 5 5 5 4 5 5 5 5 5 6 4 5 6 6 4 5
    ## [28477] 5 3 5 5 4 5 5 5 4 5 6 5 3 1 3 4 3 4 6 1 6 4 3 4 4 3 6 6 4 5 5 5 3 6 1 5
    ## [28513] 5 5 5 5 5 5 5 6 5 5 5 4 5 5 3 5 5 5 4 5 5 5 5 5 1 3 6 4 2 6 2 6 6 6 4 4
    ## [28549] 4 6 5 4 4 3 6 6 6 4 4 6 6 6 6 4 4 5 6 4 6 6 4 6 3 6 4 4 6 6 6 4 6 4 6 3
    ## [28585] 5 3 3 1 4 3 4 4 1 3 1 6 6 6 6 4 3 1 3 3 5 3 5 6 4 4 4 4 3 4 3 4 4 3 6 4
    ## [28621] 4 4 4 4 4 4 6 6 4 5 5 5 6 5 6 5 5 5 5 5 5 5 2 5 4 6 3 4 3 3 3 4 4 6 6 4
    ## [28657] 4 6 6 6 3 5 5 1 4 6 2 4 4 4 4 6 4 4 2 5 4 4 2 2 4 6 6 1 5 5 3 7 1 5 4 5
    ## [28693] 5 3 3 3 6 6 6 1 6 3 3 6 6 1 4 6 6 6 6 4 4 4 6 4 3 3 4 6 3 6 4 5 4 1 7 6
    ## [28729] 3 3 4 5 5 5 4 5 5 5 2 2 2 4 4 1 3 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [28765] 2 2 2 2 2 2 2 6 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2
    ## [28801] 2 2 2 6 2 2 2 4 5 5 4 4 5 4 3 5 4 3 4 3 5 4 3 5 2 2 4 6 4 3 6 2 6 5 4 3
    ## [28837] 4 4 6 4 4 4 1 3 3 4 3 3 6 4 3 4 3 5 3 6 3 1 1 6 6 5 3 1 4 1 4 5 4 5 3 4
    ## [28873] 3 5 1 5 3 3 4 1 4 1 1 4 1 1 4 4 1 4 1 4 2 4 5 4 4 4 5 4 5 5 4 6 5 3 5 5
    ## [28909] 6 5 5 4 4 5 1 1 4 3 4 3 1 4 4 6 6 6 4 4 5 3 1 3 3 4 6 6 4 4 4 3 2 2 3 5
    ## [28945] 1 3 5 4 3 3 6 4 4 4 5 5 7 5 3 6 3 4 6 6 3 4 1 4 3 3 1 4 5 3 2 3 2 3 4 3
    ## [28981] 2 4 3 3 6 3 3 3 3 3 4 2 2 5 5 5 5 3 5 2 2 5 3 5 1 5 1 5 1 3 3 1 5 5 3 3
    ## [29017] 5 6 3 1 2 1 5 4 5 5 4 5 5 3 5 2 5 4 3 4 3 6 6 4 5 4 5 7 4 6 4 1 3 3 3 1
    ## [29053] 1 1 2 5 3 6 2 2 2 2 4 4 4 6 4 3 4 2 5 6 2 2 5 4 5 5 4 5 6 5 5 5 5 5 6 5
    ## [29089] 5 5 6 6 2 5 5 5 5 5 5 5 2 4 4 5 3 1 1 6 1 4 5 3 3 3 6 5 5 5 5 5 4 3 2 2
    ## [29125] 6 1 5 2 1 4 3 5 1 1 1 1 5 5 5 3 3 4 3 4 5 1 5 5 1 1 5 5 6 6 4 6 6 6 4 1
    ## [29161] 5 4 3 1 4 4 6 6 5 4 6 3 4 6 6 3 3 6 4 3 2 6 1 3 5 4 6 6 1 6 4 4 6 4 1 1
    ## [29197] 5 4 1 3 3 5 6 6 2 1 5 6 3 1 6 6 6 4 3 3 3 5 3 3 3 3 4 4 3 1 3 4 1 3 2 3
    ## [29233] 2 2 5 6 2 1 1 3 1 4 3 4 5 1 3 1 1 2 6 4 4 6 5 3 5 6 7 4 4 5 3 6 4 2 2 6
    ## [29269] 4 5 6 2 2 6 6 3 2 5 2 1 1 6 5 2 6 3 4 6 5 6 5 3 5 5 5 5 1 6 3 4 4 4 5 5
    ## [29305] 5 5 4 1 5 5 2 2 2 2 5 2 1 3 1 2 2 1 1 6 2 1 2 2 5 2 2 2 3 2 2 2 2 3 2 2
    ## [29341] 3 1 5 4 2 4 4 2 4 4 2 4 4 4 2 2 2 2 2 6 4 4 2 2 2 2 2 2 2 2 2 2 2 4 4 2
    ## [29377] 2 1 2 2 4 2 2 2 2 2 6 1 3 4 6 4 4 3 4 4 4 4 4 4 4 6 4 1 5 5 4 4 4 4 7 4
    ## [29413] 4 4 4 1 5 5 5 4 5 6 3 2 1 4 4 2 3 4 3 4 3 1 5 6 5 6 4 5 2 3 4 5 2 4 5 4
    ## [29449] 4 6 4 4 6 6 6 4 6 6 4 4 4 6 4 6 6 6 6 4 3 5 3 4 2 5 1 1 4 5 5 3 1 1 1 1
    ## [29485] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 3 5 3 3 2 4 6
    ## [29521] 4 6 3 3 4 6 1 1 5 6 4 5 4 5 5 4 5 5 4 1 3 3 1 4 6 3 3 3 3 6 6 3 4 4 3 4
    ## [29557] 3 2 2 6 6 6 6 4 4 1 3 3 3 5 5 6 5 5 5 1 5 6 1 1 5 4 5 5 5 1 1 1 3 5 5 4
    ## [29593] 2 5 5 5 5 5 5 3 1 1 2 4 5 4 1 5 5 5 5 5 5 3 5 5 3 4 1 1 3 4 6 3 3 3 1 5
    ## [29629] 5 4 5 6 6 4 6 1 3 4 2 3 2 4 3 2 3 6 5 5 5 5 3 4 5 5 6 7 2 4 5 3 2 5 4 4
    ## [29665] 4 5 5 4 4 5 5 5 4 5 5 5 1 5 3 2 2 7 4 2 2 2 2 2 2 2 1 6 4 2 2 2 6 2 1 2
    ## [29701] 2 4 2 2 6 2 4 2 2 4 6 4 1 4 3 4 5 3 3 5 5 5 5 5 1 5 5 5 5 5 4 5 5 5 5 5
    ## [29737] 5 5 5 2 2 2 2 2 2 2 2 2 2 5 2 2 2 2 2 2 2 6 2 2 2 2 2 4 2 2 6 2 1 1 2 2
    ## [29773] 2 2 2 2 6 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 6 2 2 1
    ## [29809] 2 2 2 2 2 2 2 6 5 5 4 6 1 3 5 3 5 5 3 3 4 5 6 2 6 4 5 6 6 4 6 6 2 4 6 6
    ## [29845] 6 1 5 5 6 4 4 4 6 5 5 5 5 1 5 5 5 5 5 5 5 1 5 3 3 4 3 5 3 3 5 4 1 3 4 5
    ## [29881] 4 4 5 4 5 5 4 1 8 5 5 3 1 1 1 3 5 3 3 5 1 1 1 5 5 1 5 5 5 5 5 3 5 5 5 4
    ## [29917] 3 4 4 4 4 5 4 6 1 2 2 1 2 2 2 2 2 2 2 1 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 2
    ## [29953] 2 2 2 2 5 5 4 1 4 6 4 6 4 3 4 1 5 1 1 5 6 1 5 1 1 2 6 3 4 6 1 1 4 4 4 1
    ## [29989] 4 3 3 5 5 4 5 5 5 5 5 5 3 3 5 4 4 5 5 5 3 3 3 5 5 3 5 1 3 5 5 1 5 5 5 5
    ## [30025] 5 5 5 5 5 5 5 1 3 4 3 3 5 3 4 5 5 2 2 4 5 4 3 6 5 5 5 5 4 5 5 5 5 5 5 5
    ## [30061] 5 3 3 3 4 3 1 5 5 2 5 5 5 5 3 5 2 4 6 1 4 5 1 1 3 3 5 1 1 1 5 2 5 5 5 5
    ## [30097] 5 5 5 3 2 5 1 5 5 3 3 1 5 1 5 5 4 5 5 1 4 5 1 5 6 4 6 6 3 1 1 7 2 2 6 1
    ## [30133] 2 7 4 6 4 1 5 7 4 3 4 3 6 5 3 6 1 2 1 1 4 2 5 5 4 3 3 4 4 4 1 4 5 4 5 5
    ## [30169] 5 5 4 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 1 5 5 5 2 2 2 2 2
    ## [30205] 2 2 2 2 2 2 2 5 5 2 2 4 2 2 5 3 4 5 3 1 3 3 1 5 5 5 4 3 4 2 4 1 5 5 5 3
    ## [30241] 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 4 6 4 5 4 4 6 6 5 6 5 6 5 5 5 6 4 1 3 4 5
    ## [30277] 6 5 5 2 1 4 4 1 3 5 3 1 1 5 5 5 3 5 4 6 2 2 1 1 1 5 1 5 6 4 4 3 4 4 3 4
    ## [30313] 3 3 3 4 3 6 3 3 1 5 6 6 6 5 6 2 4 2 1 4 3 2 1 3 1 5 4 8 8 8 3 1 3 7 4 1
    ## [30349] 2 1 2 2 2 2 2 2 2 2 1 2 1 2 2 2 3 2 2 2 4 2 2 2 2 2 5 5 5 5 5 5 5 5 5 5
    ## [30385] 5 5 2 2 2 2 2 2 2 2 2 2 3 1 4 4 4 4 4 4 8 4 4 3 5 4 4 3 4 5 4 5 6 3 4 4
    ## [30421] 4 5 5 5 5 6 4 3 4 4 6 1 6 5 5 5 5 5 2 2 5 3 2 4 4 4 1 1 3 4 4 4 4 4 3 5
    ## [30457] 4 4 4 6 1 4 5 4 1 4 6 6 6 4 6 4 6 4 1 2 5 1 3 4 4 5 5 5 5 2 5 5 5 5 6 5
    ## [30493] 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 2 5 5 5 5 4 4 5 5 5 5 5 5 5
    ## [30529] 2 5 5 5 5 5 5 5 3 5 4 5 5 5 1 3 5 5 5 5 1 1 1 4 6 2 6 6 2 6 4 4 1 2 2 6
    ## [30565] 2 5 1 4 1 6 4 3 3 5 5 5 5 3 4 3 5 5 3 4 7 4 6 4 4 7 3 1 5 4 7 3 2 2 2 2
    ## [30601] 2 2 2 2 5 2 2 2 5 2 2 2 2 2 2 2 2 5 2 2 2 1 2 2 2 4 2 2 2 4 4 4 5 4 2 3
    ## [30637] 4 4 4 6 6 6 4 3 3 5 3 1 1 3 5 2 3 2 6 4 6 6 6 4 4 7 6 6 6 2 3 6 4 1 4 2
    ## [30673] 3 4 3 4 2 5 6 4 2 5 5 3 4 1 3 1 3 3 5 7 5 3 3 1 3 3 3 3 6 5 3 5 4 7 5 5
    ## [30709] 5 5 3 5 4 2 5 6 3 5 2 5 5 6 5 5 6 2 4 2 5 2 2 5 3 1 4 4 4 4 4 3 1 6 6 4
    ## [30745] 6 6 4 4 6 6 3 6 3 5 1 3 4 6 3 5 4 4 3 4 3 3 5 3 4 3 6 3 3 4 6 3 6 4 6 5
    ## [30781] 5 5 5 2 3 1 4 3 5 5 5 4 3 3 1 4 5 5 4 6 1 7 4 1 3 2 6 4 6 4 6 6 2 4 3 4
    ## [30817] 2 4 4 2 4 3 4 4 4 2 3 1 3 5 4 5 5 3 3 4 5 3 5 5 1 4 3 4 4 4 2 2 5 5 4 5
    ## [30853] 3 3 3 6 1 6 4 4 1 6 6 2 2 1 5 7 5 3 1 5 5 6 4 3 5 1 6 6 4 6 4 5 5 5 5 5
    ## [30889] 5 5 3 5 5 5 2 5 5 5 5 5 5 5 5 7 1 1 7 5 3 3 5 5 3 6 5 5 5 7 5 3 3 4 5 3
    ## [30925] 1 1 4 5 3 2 6 3 5 5 5 3 5 3 5 1 6 5 5 5 4 5 4 5 4 4 4 4 4 5 3 5 5 5 5 5
    ## [30961] 5 5 6 6 4 6 3 4 4 5 3 5 5 3 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [30997] 5 5 5 5 5 5 5 5 5 5 5 2 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 1
    ## [31033] 1 3 3 6 4 5 5 1 2 5 3 1 3 4 6 3 5 6 1 7 4 6 5 1 5 3 7 4 3 3 3 5 5 4 4 2
    ## [31069] 2 6 6 6 6 6 6 4 4 1 6 1 4 4 6 6 4 4 2 3 4 4 6 3 5 3 4 3 2 5 5 2 2 5 2 2
    ## [31105] 2 2 2 2 5 2 5 2 5 5 5 5 5 5 5 5 4 5 5 5 4 1 1 3 5 5 1 5 5 4 5 2 6 1 2 5
    ## [31141] 1 5 5 5 3 5 2 5 2 2 2 2 2 5 5 4 6 6 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5
    ## [31177] 5 5 5 5 6 5 3 1 1 3 4 1 1 5 6 2 6 2 4 6 5 6 6 6 4 5 1 4 1 1 5 5 5 1 5 5
    ## [31213] 5 5 5 1 1 3 1 5 3 3 4 3 4 6 3 6 5 3 1 4 5 3 6 5 5 6 4 1 3 1 4 1 4 5 4 4
    ## [31249] 4 4 5 4 1 1 4 4 2 1 6 5 4 5 6 4 4 4 3 2 5 4 4 4 4 3 5 5 5 5 1 6 4 1 4 6
    ## [31285] 4 3 4 5 4 3 5 6 1 4 3 2 6 2 2 2 2 2 2 2 2 2 2 2 3 6 6 3 3 6 4 4 3 4 6 5
    ## [31321] 6 6 4 5 7 5 4 3 1 5 3 5 5 3 4 6 3 3 5 5 5 4 5 3 3 3 4 1 4 3 6 3 3 3 5 6
    ## [31357] 4 3 5 5 2 6 5 5 1 1 3 3 5 5 5 3 3 6 6 3 4 3 4 6 4 4 4 6 4 4 6 4 3 4 6 5
    ## [31393] 5 5 5 5 5 2 2 6 5 6 5 3 4 6 4 4 3 3 6 4 3 1 5 6 1 2 1 1 1 1 5 4 5 3 3 8
    ## [31429] 3 6 5 5 6 5 2 5 5 5 3 1 2 1 2 2 2 2 2 1 5 5 2 5 4 6 3 2 4 5 4 5 4 5 3 1
    ## [31465] 6 1 3 6 4 6 5 7 4 3 3 3 3 4 4 3 3 4 3 3 4 4 2 5 4 2 5 3 2 4 3 4 4 3 6 4
    ## [31501] 5 3 3 2 3 5 3 3 3 4 1 3 5 5 5 5 5 5 3 5 5 5 5 5 5 4 5 5 5 5 2 5 5 5 5 3
    ## [31537] 4 4 5 4 5 4 5 5 4 4 3 5 2 4 1 6 4 4 5 4 4 2 5 4 6 4 3 6 4 1 2 3 4 6 4 4
    ## [31573] 5 3 3 6 4 5 4 5 4 5 3 3 4 6 6 4 5 5 4 1 4 4 3 6 3 3 3 6 4 4 2 4 6 3 6 6
    ## [31609] 4 6 6 5 3 4 3 4 6 4 4 3 7 2 4 2 2 5 5 1 3 4 4 6 6 6 4 6 4 6 6 4 4 6 4 3
    ## [31645] 1 4 3 6 6 6 4 6 6 6 6 6 3 2 4 4 4 6 6 4 3 4 4 3 1 5 3 5 1 6 6 2 3 6 6 4
    ## [31681] 4 6 2 2 4 4 6 4 2 4 2 2 5 5 5 5 7 6 6 3 4 4 3 4 4 5 7 2 5 5 2 1 5 4 6 5
    ## [31717] 5 2 4 2 5 5 5 6 2 4 5 5 3 5 5 1 5 5 8 5 3 5 3 6 1 4 4 6 4 6 3 1 3 1 4 7
    ## [31753] 3 3 5 4 1 4 5 3 5 5 5 5 3 6 4 7 1 3 3 3 6 4 6 6 4 6 4 4 6 2 2 2 2 4 3 3
    ## [31789] 2 5 2 2 4 2 2 2 2 5 2 2 2 2 2 2 2 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [31825] 5 2 2 5 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2
    ## [31861] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [31897] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [31933] 2 2 2 2 2 2 2 3 5 1 3 3 4 1 4 1 4 2 4 1 2 2 6 2 2 3 5 2 2 2 2 2 2 2 2 2
    ## [31969] 2 2 2 2 2 2 2 2 4 2 2 2 2 5 6 2 2 2 2 6 5 2 2 2 2 2 5 2 4 2 2 2 2 2 2 2
    ## [32005] 2 2 2 2 2 2 2 3 4 5 3 5 5 5 6 7 4 5 2 3 5 5 5 2 5 5 5 4 5 5 5 5 5 5 5 5
    ## [32041] 5 2 5 5 5 5 5 4 5 5 1 5 3 5 5 1 5 5 4 1 3 5 5 4 5 5 4 1 4 5 4 4 3 6 4 4
    ## [32077] 4 6 6 6 6 4 6 6 6 4 6 2 5 1 4 3 5 5 5 5 2 5 5 5 2 5 5 5 3 5 5 5 5 3 3 6
    ## [32113] 4 2 3 4 3 3 4 3 3 3 3 3 3 3 3 6 6 4 3 3 3 4 3 3 3 3 3 3 4 3 3 6 3 2 3 3
    ## [32149] 6 3 3 3 3 4 4 5 3 5 2 3 3 3 5 3 3 2 2 3 3 3 3 3 3 3 3 3 6 3 3 3 3 3 1 5
    ## [32185] 3 4 3 3 3 3 6 6 3 3 6 3 3 6 3 3 3 3 3 3 6 3 6 2 5 3 3 3 3 3 5 6 3 3 3 3
    ## [32221] 3 2 3 6 3 6 3 3 6 3 3 3 3 3 2 8 3 3 3 3 3 4 3 3 3 4 6 3 3 3 3 3 3 4 4 3
    ## [32257] 3 3 4 3 3 4 6 3 2 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 4 3 3 3 3 3 6 3 3
    ## [32293] 6 3 3 3 3 3 8 3 3 3 3 2 3 4 6 3 3 2 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 2 3 3
    ## [32329] 3 3 3 4 3 3 6 3 3 3 3 3 3 3 3 4 3 3 3 3 6 3 3 3 3 3 3 3 5 3 5 2 3 1 4 3
    ## [32365] 3 1 4 4 1 4 1 4 5 1 4 4 4 6 4 4 1 1 6 5 1 2 2 2 2 4 2 2 2 2 2 6 2 2 2 2
    ## [32401] 2 4 2 2 2 2 2 2 5 1 2 2 5 4 6 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 4 3 5 3 3 5
    ## [32437] 5 1 3 3 5 4 7 6 6 5 2 5 2 2 3 2 3 4 3 3 3 1 1 4 1 6 3 3 1 5 3 1 5 1 4 6
    ## [32473] 1 3 2 6 5 4 5 3 4 5 1 3 4 1 5 1 4 1 4 2 4 2 6 6 2 1 3 4 6 3 6 5 2 6 5 4
    ## [32509] 6 5 2 4 2 3 2 4 1 6 6 4 3 6 4 2 5 6 4 2 4 6 6 1 4 6 2 6 6 4 3 4 1 1 2 2
    ## [32545] 2 3 4 4 4 4 3 3 4 4 3 1 4 3 3 3 5 3 6 4 5 5 3 5 3 5 4 5 5 5 3 5 5 5 5 5
    ## [32581] 4 5 5 5 2 1 5 5 6 4 6 4 3 6 3 6 6 3 3 6 5 5 5 5 5 5 5 6 3 5 3 5 4 1 5 5
    ## [32617] 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 5 1 3 4 5 5 5 5 5 2 5 5 5 5 5 5
    ## [32653] 6 5 5 6 4 5 5 5 5 3 5 5 5 6 5 5 5 5 1 5 5 5 3 7 5 5 5 5 5 5 5 5 5 5 5 5
    ## [32689] 5 6 6 3 3 3 3 3 5 5 3 5 3 3 1 3 2 5 4 6 6 3 3 4 5 4 4 4 4 7 5 6 1 3 3 6
    ## [32725] 1 1 6 3 6 1 2 1 5 1 2 2 4 6 7 6 6 2 3 2 4 5 2 2 2 4 2 2 2 2 2 6 3 1 1 5
    ## [32761] 5 5 1 1 2 3 5 3 4 3 3 4 3 1 3 2 5 3 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 6 5
    ## [32797] 5 5 5 5 5 4 5 5 1 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5
    ## [32833] 5 3 5 1 5 4 4 4 5 4 4 5 5 1 3 1 5 4 3 4 3 4 3 4 4 3 4 2 5 2 2 5 5 5 3 3
    ## [32869] 5 2 2 5 5 5 5 5 5 5 3 3 4 4 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5
    ## [32905] 5 5 5 5 5 5 5 5 5 5 5 4 2 4 4 5 3 5 6 4 4 6 6 6 8 3 1 3 1 6 6 4 1 4 6 4
    ## [32941] 6 3 4 4 3 5 6 4 1 6 3 4 4 5 5 3 6 2 4 5 5 5 5 5 3 5 5 5 5 5 5 2 5 5 4 2
    ## [32977] 2 2 4 2 6 2 2 4 2 6 2 2 2 4 2 4 4 2 2 6 1 4 1 5 3 1 2 5 1 5 2 2 2 2 2 5
    ## [33013] 5 2 5 3 3 4 3 6 6 3 4 4 6 3 3 1 5 5 5 4 2 4 3 4 4 4 4 5 6 2 4 2 1 6 4 8
    ## [33049] 1 5 5 4 4 5 6 3 2 4 4 6 4 3 4 1 1 3 3 3 5 5 6 5 3 4 6 1 4 6 5 6 6 3 4 4
    ## [33085] 5 5 5 5 4 2 5 3 3 5 5 3 3 5 5 1 3 3 3 4 5 5 4 1 5 5 5 3 3 4 5 6 3 2 5 3
    ## [33121] 3 6 3 3 3 3 5 1 5 5 5 6 3 3 1 3 4 3 3 3 4 7 4 4 2 6 4 2 1 1 6 1 1 4 4 3
    ## [33157] 3 2 5 4 5 5 7 2 5 5 3 3 6 4 6 4 7 4 3 1 5 5 4 5 3 3 4 6 4 3 4 1 3 1 1 1
    ## [33193] 1 3 1 1 6 4 4 3 4 6 1 4 1 1 4 1 6 4 4 1 5 3 5 5 1 5 5 6 3 5 3 1 3 4 1 3
    ## [33229] 4 7 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 1 4 5 5 5 5 5 1
    ## [33265] 5 4 4 5 4 3 5 2 1 2 2 1 2 6 1 4 3 2 2 2 2 2 4 1 2 3 6 6 4 2 4 2 3 5 5 3
    ## [33301] 6 3 3 4 3 5 5 5 3 5 7 5 5 5 4 2 5 5 5 2 5 1 6 5 1 5 5 5 5 5 4 4 1 5 4 2
    ## [33337] 3 5 5 5 3 5 5 5 5 5 5 5 5 1 5 5 5 5 5 4 4 4 3 3 3 5 3 3 3 3 1 3 3 3 3 4
    ## [33373] 5 1 4 4 6 4 4 4 6 4 4 5 4 6 1 5 4 4 6 6 4 4 4 2 4 6 4 2 4 1 2 7 2 1 5 5
    ## [33409] 5 2 2 5 6 6 4 4 5 2 5 2 5 5 5 2 5 5 5 3 4 5 4 2 4 3 5 2 4 4 2 3 4 6 5 5
    ## [33445] 4 3 6 2 2 5 6 4 4 4 3 2 4 6 4 4 4 4 3 1 3 4 3 2 4 3 2 4 2 2 2 2 2 6 4 2
    ## [33481] 2 2 4 4 4 3 2 2 2 4 2 4 2 2 4 2 6 4 2 4 3 2 2 6 3 4 6 4 6 4 4 6 3 4 3 1
    ## [33517] 3 1 3 1 1 1 4 1 3 4 4 3 4 5 4 5 5 5 2 5 5 5 5 5 5 5 5 3 5 5 4 6 4 4 6 6
    ## [33553] 6 4 6 4 3 6 1 3 1 5 5 5 4 5 5 2 5 1 5 5 5 5 5 5 4 4 4 5 4 5 5 6 2 1 4 5
    ## [33589] 5 5 3 6 2 5 5 5 2 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 4 5 5 1 3 5 5 5 1 5 3
    ## [33625] 5 4 7 4 4 6 3 3 4 5 3 6 4 5 3 3 4 5 4 2 1 6 6 2 4 4 5 2 2 4 4 4 6 6 3 1
    ## [33661] 3 3 1 5 3 1 1 5 5 5 3 5 2 2 5 2 2 2 6 2 5 2 2 2 2 2 2 2 3 2 6 6 4 2 4 6
    ## [33697] 6 6 2 6 4 4 6 2 1 2 6 6 4 4 4 4 2 4 5 3 3 4 4 6 6 3 1 2 2 6 2 2 2 6 2 2
    ## [33733] 2 2 4 6 6 2 2 2 2 2 1 3 2 2 2 2 2 2 2 3 4 6 2 3 4 2 4 6 3 3 2 2 2 2 2 1
    ## [33769] 4 2 4 2 2 2 2 6 2 2 2 2 1 2 2 2 2 2 2 2 2 2 5 3 5 4 5 4 4 3 1 3 3 5 5 5
    ## [33805] 1 5 2 2 2 2 2 2 2 2 2 4 2 6 2 4 2 4 1 2 2 2 2 2 4 2 2 2 2 2 2 2 2 6 6 6
    ## [33841] 2 2 2 2 2 2 4 6 2 2 3 2 2 2 2 2 4 4 2 2 2 2 2 2 4 2 6 2 4 2 2 2 2 1 2 2
    ## [33877] 2 4 4 4 4 5 4 4 1 3 5 6 4 1 6 1 1 2 1 1 4 3 1 1 4 5 3 5 5 5 5 1 5 5 5 5
    ## [33913] 5 5 5 1 5 5 1 5 1 3 5 3 5 3 3 4 5 4 1 1 4 7 4 4 6 4 6 1 5 3 1 7 4 5 1 5
    ## [33949] 5 1 4 1 6 6 6 1 6 6 4 4 1 4 1 4 6 6 6 4 6 6 6 4 4 6 6 5 5 3 5 3 1 3 7 2
    ## [33985] 5 1 5 4 2 7 3 3 5 4 4 3 4 4 4 5 5 4 6 6 2 3 3 3 3 1 5 5 3 5 5 5 5 5 1 1
    ## [34021] 5 1 1 4 3 5 6 6 4 1 5 5 5 6 5 5 5 3 5 5 5 5 5 5 5 1 4 5 1 4 3 3 5 4 1 1
    ## [34057] 2 3 2 6 6 6 1 2 6 2 6 2 2 2 7 2 4 2 2 2 2 2 2 1 3 1 5 3 5 3 2 5 3 5 5 5
    ## [34093] 5 4 5 1 4 4 4 1 6 1 4 6 1 4 3 4 1 1 5 3 5 1 5 5 6 6 3 6 6 4 3 6 3 4 4 6
    ## [34129] 6 5 3 3 5 5 1 5 5 4 6 5 5 4 2 4 5 6 4 3 3 2 6 6 3 4 5 5 4 1 6 1 5 6 3 4
    ## [34165] 2 6 5 1 3 3 1 3 4 1 5 6 3 5 4 3 5 5 3 5 5 2 3 1 1 1 3 6 1 3 1 5 3 3 3 4
    ## [34201] 3 3 5 5 5 4 4 4 4 1 1 3 5 4 6 4 6 4 3 4 6 6 6 4 4 6 6 6 4 4 6 4 6 6 5 5
    ## [34237] 5 5 1 5 4 5 3 4 4 6 3 4 7 1 4 3 5 3 3 5 5 5 5 6 1 4 4 5 5 1 4 3 1 5 5 3
    ## [34273] 1 4 4 1 4 1 5 6 4 6 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 4 5 3 2
    ## [34309] 4 1 4 1 2 2 2 2 4 4 4 4 2 2 6 5 2 2 2 3 4 2 2 4 2 2 2 2 2 2 2 2 2 2 4 2
    ## [34345] 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 5 5 6 4 1 4 6 1 4 6 3 6 4 1 6 1 3 3 4
    ## [34381] 4 4 3 5 2 2 2 2 2 3 3 2 4 5 3 3 3 1 3 1 5 5 4 6 1 6 5 3 5 5 3 3 2 2 2 3
    ## [34417] 1 3 2 4 2 6 3 3 3 4 2 6 7 2 4 6 3 2 6 3 6 4 4 2 6 3 5 5 5 4 3 3 5 5 4 4
    ## [34453] 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 2 6 4 4 5 4 4 6 6 4 5 4 5 5 5 4 3 5 4 5 3
    ## [34489] 1 4 1 5 4 4 5 4 6 1 4 3 4 4 3 3 3 1 4 2 3 5 4 4 3 4 3 3 4 3 4 4 4 4 6 5
    ## [34525] 5 5 5 1 3 3 4 4 4 2 3 5 5 2 5 4 5 4 3 5 3 4 1 3 4 4 4 3 3 5 3 5 1 4 1 3
    ## [34561] 1 5 5 5 5 4 5 1 5 2 6 4 2 6 3 2 1 3 3 3 4 6 2 3 1 1 3 4 3 4 3 5 4 3 3 4
    ## [34597] 4 5 4 1 3 1 1 5 1 5 1 5 1 1 4 3 4 1 6 6 2 1 4 4 5 4 4 4 3 1 4 1 4 1 6 4
    ## [34633] 4 4 6 5 6 4 5 3 6 5 3 5 4 1 5 3 5 2 4 6 4 2 2 1 5 1 4 4 5 6 5 4 1 6 4 4
    ## [34669] 3 4 4 3 3 5 3 5 5 3 4 6 6 5 4 3 4 5 3 4 2 2 2 5 2 2 4 5 3 2 2 6 4 1 3 4
    ## [34705] 5 5 3 6 4 3 6 3 4 4 6 4 6 6 3 6 4 4 3 6 4 6 6 3 5 5 6 3 8 5 3 5 5 3 5 5
    ## [34741] 2 3 5 4 5 1 6 1 4 5 1 3 3 4 4 3 5 4 3 5 4 4 3 5 5 5 5 5 5 5 5 5 4 4 1 3
    ## [34777] 5 5 5 4 5 5 5 5 5 2 5 5 5 3 3 3 6 4 3 1 3 6 4 4 8 3 1 3 6 6 4 4 3 5 5 4
    ## [34813] 6 4 3 6 4 4 6 4 6 4 6 3 4 5 6 4 1 3 5 5 6 4 4 5 4 4 5 5 6 4 3 4 6 3 6 4
    ## [34849] 3 5 5 4 3 5 3 3 2 6 4 2 2 2 2 2 2 2 2 3 5 4 3 5 5 2 4 4 5 5 6 2 4 4 5 4
    ## [34885] 4 5 3 5 3 1 3 3 4 5 4 4 5 6 5 5 4 5 4 5 4 5 6 5 5 5 2 4 5 5 5 2 4 2 3 5
    ## [34921] 3 6 6 3 3 2 2 2 2 6 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 1 2 2 2 2 2
    ## [34957] 2 4 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 1 1
    ## [34993] 4 3 3 4 6 5 6 3 3 3 3 4 3 4 4 6 3 4 6 6 4 4 4 6 6 4 6 3 5 5 5 4 6 4 5 5
    ## [35029] 5 5 5 5 1 2 1 5 1 5 5 2 1 3 6 6 5 6 6 6 5 5 5 3 4 3 4 6 4 6 6 4 4 4 2 6
    ## [35065] 2 6 4 4 4 5 4 4 4 6 4 1 3 6 6 4 6 1 1 5 3 2 5 3 3 3 4 3 3 1 4 4 4 5 5 6
    ## [35101] 5 4 6 5 6 4 4 5 5 5 5 5 5 5 5 5 5 2 1 5 5 5 5 5 5 5 6 5 4 7 5 2 6 5 4 1
    ## [35137] 3 1 4 4 2 4 4 6 4 4 2 1 5 5 5 2 3 1 5 5 5 1 5 4 7 3 4 3 3 7 4 5 3 1 6 2
    ## [35173] 1 3 4 5 6 1 1 3 3 4 4 6 6 1 6 5 6 6 4 6 6 5 1 1 3 4 3 6 1 4 4 6 5 4 4 4
    ## [35209] 1 4 5 2 5 6 5 5 5 3 3 6 4 2 4 4 3 2 3 2 4 2 4 6 4 3 1 4 4 4 5 5 4 5 6 5
    ## [35245] 5 1 3 6 4 4 4 4 5 6 6 3 1 6 4 4 4 4 4 6 3 6 6 6 6 4 4 5 1 5 1 3 4 4 6 1
    ## [35281] 4 3 6 4 4 1 4 5 1 4 5 5 5 5 5 1 5 3 1 4 4 2 4 1 4 1 3 5 1 5 4 5 1 1 5 2
    ## [35317] 5 5 3 4 4 4 3 3 4 4 4 4 6 4 3 3 2 2 2 2 4 2 2 6 5 2 2 1 5 5 5 5 2 3 2 4
    ## [35353] 4 4 2 6 2 4 6 6 6 4 2 2 4 4 4 4 6 2 4 4 4 2 2 5 5 4 2 4 4 3 4 4 4 5 5 4
    ## [35389] 5 5 5 5 5 6 5 5 1 3 5 4 5 4 1 5 3 3 3 5 2 2 5 3 6 2 1 1 3 6 4 1 1 5 1 3
    ## [35425] 6 5 1 4 4 3 5 4 1 1 1 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 6 2 5 5 5 5 2
    ## [35461] 2 4 2 5 5 5 5 6 5 5 5 5 2 4 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 4
    ## [35497] 5 5 1 1 3 1 1 1 3 5 4 5 4 2 5 5 5 5 3 5 4 5 5 3 5 6 4 4 3 3 4 1 6 6 1 4
    ## [35533] 4 4 3 6 5 4 3 1 6 6 3 4 3 3 5 3 6 4 4 3 3 4 6 6 5 5 4 5 6 5 5 3 2 5 5 4
    ## [35569] 5 5 5 5 2 4 1 1 1 4 1 8 8 4 6 1 5 5 4 1 3 6 1 5 4 6 1 4 6 3 5 4 5 1 6 5
    ## [35605] 1 1 4 6 5 4 3 4 3 4 3 4 5 6 6 5 4 3 4 4 6 3 3 4 3 4 4 6 3 5 3 4 5 5 5 5
    ## [35641] 3 3 3 3 3 6 4 5 5 3 5 3 1 5 7 6 3 4 4 4 4 3 5 2 5 5 2 3 5 4 5 5 4 3 5 5
    ## [35677] 3 1 3 5 2 6 1 1 3 6 3 3 5 3 5 3 5 5 4 2 5 3 5 5 6 2 4 6 4 4 4 6 5 3 3 3
    ## [35713] 6 4 4 3 3 3 4 1 1 4 7 5 3 3 1 5 4 3 3 3 3 1 5 5 4 4 4 2 5 4 4 4 2 6 4 4
    ## [35749] 6 6 3 6 6 6 6 6 6 4 3 6 4 5 4 4 6 6 6 3 4 4 6 6 6 4 6 5 5 3 5 5 1 5 2 5
    ## [35785] 5 3 2 6 5 4 6 6 3 6 6 5 4 4 6 6 4 2 3 5 3 4 5 3 3 1 6 3 5 4 2 6 6 6 3 4
    ## [35821] 3 6 4 4 4 3 5 3 3 1 1 4 3 4 4 6 6 6 4 6 4 6 4 6 6 6 4 6 6 6 4 6 4 4 4 6
    ## [35857] 3 1 4 1 5 5 1 5 4 3 5 5 4 5 5 6 6 4 6 3 6 3 3 2 4 3 4 4 6 6 3 2 5 5 5 5
    ## [35893] 3 3 5 3 3 4 3 3 4 6 3 6 6 4 6 4 4 4 6 2 2 5 6 6 3 5 6 5 2 1 5 1 4 1 4 1
    ## [35929] 3 2 1 3 5 4 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 1 5 4 1
    ## [35965] 1 1 1 1 1 1 1 1 1 5 1 1 5 5 5 5 1 4 5 3 5 5 5 5 5 5 3 5 5 5 5 5 3 4 1 1
    ## [36001] 1 1 4 1 1 6 2 2 4 6 5 1 4 6 8 5 5 4 1 5 4 1 5 1 5 1 1 5 5 1 4 5 5 3 1 5
    ## [36037] 5 4 1 3 5 1 5 3 1 5 5 5 3 5 2 5 4 2 2 4 4 2 7 4 6 3 5 3 5 5 5 2 2 2 2 2
    ## [36073] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 6 2 2 2 2 6 2 2 1 2 2 2 2 2 2 2 4 2 4 4 3
    ## [36109] 4 7 2 2 4 2 4 5 5 5 5 5 5 1 5 1 4 5 4 4 3 4 4 6 4 4 4 3 6 3 6 3 4 6 4 4
    ## [36145] 3 6 4 3 2 5 3 1 6 5 4 5 1 3 6 4 3 5 4 4 1 4 3 3 1 3 3 1 3 4 1 6 7 3 1 1
    ## [36181] 5 5 3 3 4 5 6 4 6 6 6 5 4 4 4 3 6 5 1 3 3 4 6 4 5 5 5 5 5 5 5 5 5 2 5 5
    ## [36217] 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 3 1 3 4 4 1 3 1 1 1 5 4 3 1 3 4 3 3 2 4
    ## [36253] 4 5 3 5 5 3 4 4 3 5 6 4 5 1 6 5 3 5 5 5 2 1 5 4 3 3 5 4 6 4 8 4 6 5 1 3
    ## [36289] 6 3 3 3 4 3 4 4 3 6 4 4 4 6 6 6 6 6 6 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6
    ## [36325] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 1 5 4 1 5 3 5 5 5 5 5 5 1 4 5 5 5 2
    ## [36361] 5 2 5 2 2 5 2 2 1 6 2 2 2 5 2 2 1 2 5 5 5 3 3 3 3 5 5 4 3 5 2 5 1 5 4 2
    ## [36397] 4 5 1 6 4 5 4 1 4 5 1 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 3
    ## [36433] 5 5 5 3 5 5 1 1 3 2 6 5 4 2 6 4 3 5 2 5 4 2 6 5 2 1 2 2 5 5 4 2 5 4 4 5
    ## [36469] 3 4 2 6 6 3 6 3 6 1 2 6 3 3 3 4 6 6 2 5 6 4 5 5 4 3 3 1 1 4 5 3 5 5 3 4
    ## [36505] 3 4 5 6 6 4 5 5 3 4 7 4 6 6 1 6 1 1 4 5 2 6 4 4 4 4 4 6 6 6 6 6 4 4 6 6
    ## [36541] 4 6 6 4 4 6 6 3 6 6 6 6 6 6 6 3 4 3 6 6 5 2 5 5 1 2 3 1 3 2 6 6 4 4 5 5
    ## [36577] 5 1 3 3 2 5 1 4 4 4 5 3 3 4 5 5 5 5 1 5 5 4 5 5 5 5 6 5 5 3 5 5 5 5 5 5
    ## [36613] 5 5 5 5 5 5 2 3 1 5 3 5 5 4 6 5 2 6 1 4 4 4 3 3 4 3 6 4 6 6 6 4 6 4 1 4
    ## [36649] 7 1 5 4 4 1 5 2 2 6 6 4 4 4 3 4 3 6 5 3 2 5 5 6 6 6 5 2 6 2 5 5 5 5 2 5
    ## [36685] 5 4 5 5 5 4 4 1 5 6 3 4 3 3 3 1 5 5 3 3 4 6 1 5 5 5 5 5 5 5 4 5 5 5 5 5
    ## [36721] 5 2 5 5 5 5 4 4 1 4 4 4 4 3 2 2 2 3 3 2 2 5 4 6 6 2 4 4 6 6 2 6 3 2 3 2
    ## [36757] 2 5 4 5 2 4 5 5 4 6 5 5 5 5 3 5 6 6 5 1 5 4 5 6 6 6 1 4 4 4 4 1 4 1 5 3
    ## [36793] 3 6 1 1 6 1 4 3 5 1 1 5 3 1 4 5 5 1 5 2 5 2 5 5 5 5 2 5 5 5 5 5 5 5 6 4
    ## [36829] 2 5 5 5 2 4 6 5 4 4 4 4 3 6 6 6 6 5 6 4 4 4 5 4 6 6 6 6 6 4 4 7 5 2 4 3
    ## [36865] 1 5 5 3 4 2 6 3 6 3 4 4 3 3 1 4 3 6 4 7 5 2 5 4 4 3 3 3 6 1 4 3 4 6 3 5
    ## [36901] 5 5 5 5 5 1 1 5 5 5 5 5 4 5 3 5 5 5 3 3 1 4 1 1 6 1 6 3 3 3 5 6 1 4 4 3
    ## [36937] 6 6 1 5 5 5 5 5 5 5 5 5 3 3 5 5 5 5 2 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5
    ## [36973] 5 5 5 5 5 2 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 3 5 5 5
    ## [37009] 5 2 5 5 1 3 4 6 5 6 5 5 5 5 5 5 5 3 5 5 5 5 5 4 5 4 5 4 4 1 6 4 4 5 4 3
    ## [37045] 4 3 5 2 2 6 6 5 5 2 5 1 3 5 5 5 4 4 1 5 5 5 2 4 5 2 2 2 5 2 3 2 2 4 2 2
    ## [37081] 2 2 4 2 2 2 2 2 4 3 2 2 5 2 2 2 2 2 2 2 6 2 2 2 5 3 5 5 5 5 5 5 5 5 5 5
    ## [37117] 3 2 2 2 2 2 2 4 2 2 4 3 5 2 5 2 2 1 2 6 2 2 2 2 3 2 3 6 3 2 1 2 4 6 4 6
    ## [37153] 3 5 5 3 2 4 4 4 1 5 3 6 3 6 4 4 3 3 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 4 6 4
    ## [37189] 4 4 3 6 6 4 1 4 1 3 6 4 6 4 6 6 4 2 4 4 6 4 4 5 4 5 5 1 5 3 4 5 3 8 3 4
    ## [37225] 4 5 4 5 5 5 4 4 4 6 4 6 4 6 4 6 6 6 6 4 4 6 4 4 6 3 6 5 1 5 5 1 5 4 5 5
    ## [37261] 2 5 5 1 5 4 3 4 4 4 1 4 4 4 5 5 3 1 3 4 4 2 6 4 4 6 3 1 5 2 5 8 5 6 1 3
    ## [37297] 6 6 1 6 6 4 5 1 4 4 1 2 4 2 3 2 2 4 3 2 6 4 4 1 1 5 5 4 1 5 5 1 2 5 4 4
    ## [37333] 4 3 4 4 1 7 5 4 7 4 4 4 4 4 1 1 1 1 4 3 1 6 1 3 1 5 5 3 3 4 5 1 5 3 4 3
    ## [37369] 1 5 3 6 2 2 3 2 3 5 3 6 4 4 4 3 3 6 1 1 1 1 5 1 1 4 4 5 5 5 4 2 5 5 5 2
    ## [37405] 5 4 1 5 5 5 5 5 5 5 5 4 6 3 4 1 3 3 1 5 5 6 3 3 4 1 4 6 4 4 6 6 3 4 4 6
    ## [37441] 6 5 4 2 4 4 6 4 6 6 6 4 6 4 6 6 6 4 4 4 4 6 3 3 1 5 3 3 3 3 4 3 5 5 4 5
    ## [37477] 3 3 1 5 5 5 5 5 4 5 5 5 5 5 4 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 2 5 3 1 1 4
    ## [37513] 5 7 1 6 2 4 5 5 1 4 1 4 2 1 5 3 3 6 1 3 3 3 4 1 6 3 6 1 5 2 6 1 1 5 1 3
    ## [37549] 8 3 3 5 5 3 3 8 3 6 4 4 6 4 4 6 6 4 6 4 4 6 6 4 6 3 5 4 2 2 5 1 2 2 2 4
    ## [37585] 3 5 2 5 2 2 3 3 6 5 7 5 3 4 3 1 1 5 5 5 3 6 4 5 1 1 3 5 5 3 1 5 5 4 3 1
    ## [37621] 3 5 5 1 5 1 5 4 2 4 3 1 5 1 3 5 2 2 1 2 2 2 2 2 2 5 2 5 2 2 2 3 2 2 2 2
    ## [37657] 2 2 6 5 2 4 4 6 1 5 2 1 6 4 5 5 6 1 5 6 6 5 6 5 4 1 5 3 3 3 5 5 4 5 1 5
    ## [37693] 4 4 5 3 4 5 5 6 6 7 4 3 6 4 5 5 6 6 6 4 4 3 6 2 4 5 4 4 3 4 3 1 5 1 5 5
    ## [37729] 5 5 4 4 3 4 5 5 5 4 4 6 6 4 3 6 4 4 3 3 6 6 6 6 3 4 4 4 4 3 3 4 4 5 3 5
    ## [37765] 5 5 5 5 5 4 5 5 1 3 6 3 3 1 3 1 3 3 5 5 4 1 4 6 4 3 4 3 2 4 4 3 2 3 3 6
    ## [37801] 4 4 4 6 3 6 3 4 3 3 1 5 3 1 6 4 4 1 3 3 4 6 6 4 4 6 1 6 5 6 6 6 6 6 4 6
    ## [37837] 6 4 4 3 4 6 4 6 6 4 4 4 6 2 6 3 4 4 4 4 4 5 6 6 4 6 6 5 1 3 3 3 3 3 4 6
    ## [37873] 3 4 3 3 3 3 2 4 3 5 3 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 3 4 3 6
    ## [37909] 4 3 4 6 6 4 5 6 4 6 4 6 5 1 4 6 1 3 4 3 4 3 6 3 4 3 6 3 5 5 1 1 1 4 2 6
    ## [37945] 4 3 4 4 6 3 6 6 6 4 6 4 4 6 6 6 4 4 3 5 6 3 4 6 5 3 2 3 5 5 4 5 6 6 6 4
    ## [37981] 5 3 4 4 2 7 2 2 2 5 2 6 4 5 3 1 5 2 5 3 5 4 5 5 3 5 5 3 1 1 1 5 3 5 3 3
    ## [38017] 3 5 4 5 5 5 2 5 5 5 5 5 5 5 2 5 5 5 4 5 3 4 5 3 6 5 4 3 5 5 5 4 6 6 4 4
    ## [38053] 6 3 3 4 4 4 4 3 6 6 1 6 4 6 6 6 7 6 5 2 2 2 4 3 4 3 4 3 6 4 6 4 5 3 6 6
    ## [38089] 5 5 6 3 4 1 6 4 1 6 6 6 4 1 4 4 6 6 6 5 6 3 6 7 4 4 5 5 5 5 5 5 5 5 5 5
    ## [38125] 5 5 5 5 3 5 5 5 5 5 5 5 5 2 5 3 5 2 4 5 3 4 2 1 3 2 5 4 1 5 5 5 4 5 5 3
    ## [38161] 5 5 5 5 5 2 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 1 3 4 3 4 5 1 3 3 5
    ## [38197] 4 3 4 6 6 4 6 4 3 3 3 3 3 4 6 5 5 5 3 4 5 5 3 3 4 6 2 3 3 3 3 3 3 3 3 6
    ## [38233] 1 3 3 5 4 4 2 3 6 2 1 2 6 2 3 6 4 6 4 4 4 4 2 1 4 5 3 4 6 4 6 6 4 4 6 6
    ## [38269] 4 4 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 4 6 6 6 5 5 5 5 5 5
    ## [38305] 5 5 5 5 5 5 5 5 5 2 5 5 5 5 2 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 6
    ## [38341] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 7 6 7 4 1 3 2 5 4
    ## [38377] 7 6 6 4 6 6 6 6 6 6 1 1 5 2 6 5 5 1 3 5 6 1 2 3 5 2 1 2 5 5 4 5 2 5 5 4
    ## [38413] 4 2 5 5 5 1 5 5 5 5 5 5 5 4 4 4 6 1 4 6 3 3 5 5 1 4 6 5 6 1 6 6 1 5 6 6
    ## [38449] 4 5 6 6 6 6 6 3 1 6 6 6 6 3 5 2 4 4 4 6 5 8 4 3 6 4 5 5 4 6 3 4 6 4 2 2
    ## [38485] 2 2 2 5 2 4 4 4 2 2 2 2 2 2 2 1 5 4 6 2 2 5 2 4 2 2 2 2 4 5 4 1 3 3 4 4
    ## [38521] 3 4 1 2 5 6 3 2 4 3 5 3 2 5 5 5 3 5 3 5 5 3 3 3 1 3 5 6 4 5 5 4 3 2 2 5
    ## [38557] 1 5 4 1 5 5 5 3 3 4 3 3 3 1 3 3 1 5 5 5 5 5 1 5 4 3 1 5 3 5 4 5 3 5 4 3
    ## [38593] 4 6 3 2 3 5 3 5 3 3 3 5 5 1 5 5 1 5 5 1 5 5 1 4 1 3 5 5 3 1 3 3 6 4 6 6
    ## [38629] 4 2 2 5 4 4 6 6 2 2 6 4 3 2 6 2 6 5 5 5 3 2 3 1 6 3 2 6 3 4 4 3 4 4 5 1
    ## [38665] 4 4 4 6 4 1 6 4 6 6 6 6 1 4 4 4 4 6 6 2 6 2 6 6 6 4 6 3 3 4 4 3 4 5 6 4
    ## [38701] 5 4 5 3 4 5 3 6 3 2 3 4 7 4 3 4 5 2 5 5 3 3 6 3 3 6 4 5 6 3 6 1 2 6 1 2
    ## [38737] 2 3 4 6 6 2 1 2 1 5 3 4 5 4 4 4 3 2 3 5 5 5 5 5 5 5 5 3 3 5 2 5 5 5 5 5
    ## [38773] 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 6 3 6 4 6 3 1 1 4 4 4 1 4 1 4 4 4 4 4 4 5
    ## [38809] 5 4 5 5 5 6 4 6 5 5 6 5 6 5 5 5 5 2 5 5 6 5 5 5 5 5 4 4 5 5 5 5 5 4 5 5
    ## [38845] 5 5 5 5 5 5 4 5 6 5 6 5 2 6 6 5 5 6 5 5 5 5 6 5 6 2 6 4 2 2 2 2 2 5 2 2
    ## [38881] 3 2 2 2 2 5 5 5 5 5 5 5 5 5 5 5 3 4 5 5 6 3 4 6 4 6 4 2 4 3 4 1 6 4 6 4
    ## [38917] 6 4 1 4 3 3 3 3 4 6 5 3 6 4 2 5 5 4 4 3 5 3 5 3 5 2 6 5 3 6 5 5 1 1 4 6
    ## [38953] 3 6 6 3 4 1 1 1 5 4 5 1 2 1 3 3 4 3 2 4 3 1 5 1 5 1 3 4 3 3 4 2 5 5 4 5
    ## [38989] 3 5 4 3 6 2 5 2 3 3 1 5 3 3 6 6 4 2 4 2 1 1 1 1 1 1 1 1 4 1 5 5 6 5 5 5
    ## [39025] 2 2 5 2 2 2 2 3 2 4 6 2 2 3 1 2 1 2 1 2 4 6 2 4 4 5 5 4 3 6 4 2 4 1 3 3
    ## [39061] 1 1 1 1 1 1 5 7 5 5 3 5 3 4 3 6 5 3 3 4 5 5 6 3 2 3 3 5 5 3 5 5 5 5 3 5
    ## [39097] 5 1 6 1 6 3 4 6 3 4 4 4 4 4 4 2 2 6 4 2 6 2 6 4 2 5 4 5 6 4 4 2 4 5 5 3
    ## [39133] 1 6 4 5 5 5 5 4 6 3 4 4 4 5 4 4 5 4 3 4 3 4 3 5 6 5 3 4 2 2 3 6 6 4 4 4
    ## [39169] 4 3 4 2 4 2 1 2 6 4 5 1 5 4 4 4 1 2 5 2 3 3 3 5 5 5 4 4 2 4 1 4 6 4 1 4
    ## [39205] 2 4 4 2 4 6 4 1 3 6 6 6 3 4 4 6 6 6 4 4 3 6 6 6 6 6 3 3 6 4 4 3 4 5 3 5
    ## [39241] 3 4 6 5 3 2 6 4 5 4 6 4 4 4 4 2 4 4 4 3 4 3 5 6 6 4 5 4 5 3 2 6 4 2 6 1
    ## [39277] 1 3 1 3 3 3 1 3 4 6 6 4 4 6 6 6 4 4 4 6 4 6 6 6 8 5 5 5 5 7 5 2 5 3 5 3
    ## [39313] 5 6 6 4 6 6 1 1 1 1 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 4 6 6 5 6 3 2 3 3
    ## [39349] 5 3 5 1 4 3 4 1 3 1 4 4 6 4 6 3 5 1 3 3 4 3 3 2 4 5 4 3 4 4 6 5 3 6 3 2
    ## [39385] 5 3 3 3 5 2 5 2 5 3 5 6 6 3 3 2 4 5 3 2 3 4 1 4 3 6 4 4 3 2 3 3 5 5 3 4
    ## [39421] 1 1 5 6 6 3 1 5 1 3 5 1 6 2 4 5 1 4 3 2 1 2 4 5 5 3 4 2 4 2 2 5 2 6 2 6
    ## [39457] 3 5 2 5 5 2 6 5 1 3 3 2 5 5 1 4 3 2 4 2 2 2 4 2 2 2 2 7 2 2 2 2 2 3 4 5
    ## [39493] 5 4 2 6 2 6 2 4 2 6 3 4 3 3 3 4 5 5 5 1 5 5 5 5 5 5 5 5 5 2 4 1 4 2 4 2
    ## [39529] 4 6 2 6 2 6 2 6 2 2 6 3 2 6 6 6 2 6 2 6 4 2 5 4 6 5 2 2 6 6 4 4 4 4 4 5
    ## [39565] 4 4 6 2 6 3 4 3 6 2 4 1 4 4 4 4 4 1 3 5 6 5 7 5 5 5 5 5 4 4 3 5 6 6 6 5
    ## [39601] 1 5 1 5 1 1 5 2 2 6 1 5 2 4 2 2 6 2 2 2 2 4 2 2 6 6 6 6 4 6 6 8 3 3 2 3
    ## [39637] 6 4 3 6 3 4 6 4 3 4 4 5 4 6 1 4 4 4 5 1 5 5 5 5 4 3 5 5 1 3 6 6 6 4 6 4
    ## [39673] 6 3 6 3 2 2 4 4 5 5 5 2 3 1 1 1 3 3 3 5 3 3 4 3 6 4 3 1 4 4 5 4 7 4 4 4
    ## [39709] 6 5 1 4 4 5 2 5 6 2 2 6 6 6 3 5 1 4 2 5 6 2 5 1 5 5 5 2 1 5 5 5 4 5 3 5
    ## [39745] 5 4 4 1 4 6 4 4 4 4 6 3 3 6 4 6 6 4 4 4 5 6 4 5 2 4 5 6 3 6 4 4 4 4 3 4
    ## [39781] 6 6 6 4 3 4 3 3 4 6 6 6 4 6 6 4 6 6 6 4 3 5 5 1 2 2 4 4 1 3 6 4 3 4 1 1
    ## [39817] 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [39853] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5
    ## [39889] 5 4 5 5 5 5 1 5 6 5 4 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [39925] 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5
    ## [39961] 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 3 4 3 3 1 3 4 3 5 1 3 3 6 5 6 6 4 4 4 4
    ## [39997] 5 4 3 4 5 4 2 2 4 4 7 4 4 4 7 4 4 5 3 5 4 4 2 5 5 5 5 5 2 5 5 5 5 5 4 5
    ## [40033] 5 5 5 5 5 4 3 5 6 3 5 3 5 3 5 2 2 2 7 2 1 2 2 1 4 5 3 1 3 4 1 5 2 6 1 5
    ## [40069] 4 4 3 4 6 4 4 4 3 2 5 5 5 3 5 5 4 5 3 4 4 6 6 2 4 6 6 4 6 4 5 4 6 3 6 4
    ## [40105] 4 2 2 6 6 4 1 3 4 2 1 3 4 4 3 1 4 2 3 3 3 4 3 5 1 3 3 6 7 4 5 4 6 5 5 5
    ## [40141] 3 5 5 6 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 6 3 5 2 4 4 6 3 4 4 1 1 4 4 1 3
    ## [40177] 3 4 4 4 4 4 4 4 2 3 4 6 2 6 4 4 4 4 6 6 4 4 6 1 2 3 5 5 5 6 6 5 6 2 6 5
    ## [40213] 4 1 1 5 5 5 5 5 5 6 5 5 3 5 2 5 5 5 5 3 6 1 6 1 3 5 1 5 6 2 5 3 5 5 4 7
    ## [40249] 5 2 6 4 3 3 3 3 3 6 4 4 5 3 4 3 3 4 4 4 3 4 3 1 5 5 4 5 5 1 3 3 5 4 4 4
    ## [40285] 5 1 5 6 3 3 5 1 5 5 3 4 4 5 3 5 3 4 4 3 5 5 4 3 4 6 4 3 3 5 5 8 6 5 1 2
    ## [40321] 2 4 5 1 5 4 5 1 4 2 5 1 5 5 5 5 5 4 5 3 5 5 5 3 5 1 1 5 5 1 1 6 5 5 4 4
    ## [40357] 3 5 2 4 4 4 5 5 4 5 5 5 5 6 4 4 5 1 6 4 3 6 4 6 4 3 5 6 1 6 4 4 4 4 6 4
    ## [40393] 5 3 3 3 4 4 3 5 5 3 5 5 2 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 2 5 3 1 3 3 3 5
    ## [40429] 5 4 1 3 6 1 3 3 3 5 4 5 4 3 3 5 3 1 1 4 1 4 4 1 4 4 6 4 1 4 5 3 5 6 6 1
    ## [40465] 4 5 5 5 3 5 5 3 5 1 3 3 1 3 4 5 1 4 6 4 6 3 4 1 2 1 4 1 5 5 5 5 5 5 5 3
    ## [40501] 4 4 4 4 5 3 3 1 2 4 5 5 6 2 1 1 1 6 1 4 1 1 3 6 4 5 6 5 5 6 3 3 3 4 3 4
    ## [40537] 6 3 5 4 3 3 4 5 3 1 6 4 4 5 3 3 4 4 4 4 4 4 5 6 6 1 1 1 5 5 5 5 5 5 5 5
    ## [40573] 5 5 5 5 5 5 5 5 5 5 4 5 2 1 5 2 4 5 5 5 5 1 5 5 5 5 5 5 5 5 5 4 4 5 2 5
    ## [40609] 4 5 5 5 5 5 5 5 5 4 4 3 3 3 6 6 6 4 4 3 4 5 6 1 4 6 6 6 7 5 5 5 5 5 5 5
    ## [40645] 5 5 5 2 6 6 4 3 3 2 4 6 6 5 6 3 6 4 5 5 5 3 3 1 1 2 2 2 2 5 2 2 2 2 2 2
    ## [40681] 2 3 2 2 5 4 2 2 2 2 2 6 2 3 3 3 3 1 7 4 5 5 5 5 2 5 5 5 5 5 5 5 3 5 5 5
    ## [40717] 5 5 5 3 2 1 6 3 4 1 3 5 4 3 5 4 3 3 4 4 6 6 6 1 5 5 5 5 5 5 5 5 5 2 5 5
    ## [40753] 5 5 6 5 4 5 3 6 5 5 6 5 3 3 2 4 3 6 1 1 5 3 3 3 4 6 3 4 6 6 4 3 5 1 2 7
    ## [40789] 5 4 4 2 2 4 2 2 2 4 7 2 2 2 2 2 2 4 4 2 2 3 3 1 5 5 5 5 5 5 3 5 5 5 5 5
    ## [40825] 5 1 5 5 5 5 3 6 4 5 6 4 3 4 3 3 5 5 5 4 5 3 4 4 4 4 4 4 6 5 4 4 3 4 1 3
    ## [40861] 4 3 4 1 3 3 5 5 2 1 2 2 2 2 2 4 1 4 6 6 4 4 6 6 4 3 6 6 3 3 3 1 3 4 4 3
    ## [40897] 4 5 5 4 6 6 6 6 6 4 4 2 6 6 6 6 6 5 2 4 6 4 6 6 6 6 6 6 6 6 5 1 1 4 1 3
    ## [40933] 1 1 4 7 2 4 2 4 4 4 4 4 4 2 2 4 2 3 4 6 6 4 6 6 5 4 4 4 3 3 1 3 3 5 5 5
    ## [40969] 5 5 5 5 1 5 5 4 6 3 5 5 5 3 3 6 6 5 5 6 1 1 3 5 5 5 3 5 5 5 5 1 5 5 5 5
    ## [41005] 5 4 5 5 5 2 1 3 5 4 5 4 5 5 5 5 5 5 2 2 4 5 1 5 5 5 3 3 5 1 5 3 1 5 1 1
    ## [41041] 1 5 4 3 6 2 2 3 1 1 1 3 1 3 4 5 1 3 6 6 1 5 4 4 5 4 1 4 5 4 1 1 5 5 4 3
    ## [41077] 5 5 6 6 4 2 4 4 4 6 5 6 6 6 4 6 4 5 6 6 3 6 4 4 3 1 6 6 6 6 4 1 6 1 4 3
    ## [41113] 3 1 5 5 7 5 1 5 5 5 5 5 5 5 6 3 3 3 3 7 1 5 3 3 4 5 4 6 6 6 4 6 6 5 1 6
    ## [41149] 6 6 4 4 4 3 5 5 5 5 5 5 5 5 3 1 4 4 3 6 4 1 3 6 5 6 3 1 3 6 6 3 6 3 4 4
    ## [41185] 4 3 5 6 3 3 3 4 5 5 4 5 5 5 4 3 1 1 2 2 2 2 2 2 2 5 2 2 2 2 2 2 2 2 2 2
    ## [41221] 4 4 4 6 5 2 5 5 3 5 3 1 1 1 3 5 5 5 6 5 4 3 3 3 1 3 2 3 6 8 6 5 3 4 5 1
    ## [41257] 3 3 6 4 3 6 5 1 1 4 1 1 4 1 1 2 1 5 5 4 5 1 5 5 5 4 5 1 5 3 1 3 1 1 3 1
    ## [41293] 4 2 2 4 2 3 1 3 5 6 6 6 6 5 5 5 5 5 1 5 1 5 5 5 5 5 5 1 5 5 5 5 5 5 5 4
    ## [41329] 5 3 6 6 4 6 4 3 3 6 5 4 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 2 1 5 5 5
    ## [41365] 5 4 5 5 5 2 5 5 1 5 3 5 2 4 5 4 3 3 4 3 2 4 4 4 4 4 4 4 4 4 4 4 4 4 1 5
    ## [41401] 4 2 5 4 5 4 6 4 2 4 6 6 6 3 4 4 4 4 6 5 2 5 5 5 2 5 5 3 5 5 5 5 5 5 5 5
    ## [41437] 5 5 5 3 3 5 4 3 4 3 3 5 2 3 3 4 6 3 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 7 5 5
    ## [41473] 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 5 4 5 6 5 5 5 1 3 6 4 3 2
    ## [41509] 2 6 2 2 2 2 2 4 4 2 2 1 4 1 2 3 2 1 2 2 1 1 2 4 4 5 3 5 6 5 3 6 3 5 5 5
    ## [41545] 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 1 1 1 1 4 1 1 2 4 5 1 4 2 4 6 1 1 6 1 5 4
    ## [41581] 5 6 5 5 5 5 6 6 4 6 1 3 4 3 3 1 2 4 2 4 2 2 5 5 2 2 2 2 2 3 2 6 6 5 4 3
    ## [41617] 4 5 5 5 6 5 4 2 2 2 4 2 6 2 6 2 2 5 2 2 4 2 2 2 6 3 6 3 5 3 5 3 5 2 2 2
    ## [41653] 2 2 3 3 1 2 2 2 4 2 1 2 2 2 1 1 1 5 5 4 3 3 5 6 1 1 2 5 3 5 5 4 4 4 1 4
    ## [41689] 4 5 4 3 4 4 3 4 4 4 5 1 5 3 3 1 3 2 5 3 4 5 3 5 6 5 5 6 2 5 4 5 5 1 5 6
    ## [41725] 5 4 3 5 5 5 3 2 4 1 3 5 3 5 1 1 1 6 1 1 6 5 4 1 3 3 6 5 2 5 3 3 6 6 4 3
    ## [41761] 6 4 6 6 6 4 6 6 4 6 4 6 6 1 1 8 2 6 2 6 2 6 4 2 2 5 3 5 5 4 6 6 6 6 6 4
    ## [41797] 2 4 5 2 2 4 5 4 4 5 6 4 4 4 5 4 1 4 4 4 4 3 4 3 4 3 3 4 4 5 5 5 6 6 3 1
    ## [41833] 4 5 4 5 5 4 6 4 4 6 5 5 1 2 5 4 5 5 4 5 5 1 2 5 5 4 2 3 2 6 2 2 3 3 4 1
    ## [41869] 1 3 1 4 3 5 4 3 3 5 4 1 4 2 4 4 6 6 4 4 6 6 6 1 4 4 4 3 3 8 5 5 4 5 5 4
    ## [41905] 5 5 5 5 5 4 5 3 4 5 5 5 5 5 5 6 5 5 1 5 5 5 3 5 5 4 5 6 5 5 5 5 5 5 5 5
    ## [41941] 5 1 5 4 3 3 4 4 5 3 1 3 1 1 6 6 6 6 4 6 4 6 4 4 6 2 6 6 6 6 6 6 6 4 6 4
    ## [41977] 6 6 6 4 5 5 5 3 3 3 3 5 4 5 3 3 2 3 5 5 5 5 3 3 5 5 5 3 2 5 5 5 5 4 4 4
    ## [42013] 4 6 1 3 3 5 3 5 5 5 5 5 5 6 1 4 4 4 3 4 1 1 5 1 4 5 4 4 4 6 6 4 4 5 6 3
    ## [42049] 6 6 4 6 6 6 6 4 3 6 4 3 6 3 5 3 5 5 5 5 4 4 3 4 4 2 2 4 4 6 4 3 5 4 3 6
    ## [42085] 6 6 4 6 3 1 5 3 1 5 6 3 6 2 2 6 3 2 4 6 4 6 3 2 5 6 1 5 5 4 5 3 4 5 3 4
    ## [42121] 4 5 5 2 5 5 4 5 2 5 5 5 5 5 5 5 5 5 5 5 3 5 3 5 5 1 5 5 4 5 5 5 6 6 6 6
    ## [42157] 6 3 3 5 4 6 1 5 5 5 5 5 5 5 5 5 3 6 6 5 1 5 6 2 5 1 6 6 6 6 6 2 3 1 1 7
    ## [42193] 3 4 4 5 4 3 4 4 6 4 5 6 4 5 6 5 6 5 2 5 2 2 5 5 3 2 1 2 4 2 1 5 6 5 6 6
    ## [42229] 3 3 3 1 3 6 6 4 3 3 3 6 4 3 3 4 4 4 4 4 4 3 1 4 5 4 4 4 5 6 4 4 4 5 6 4
    ## [42265] 4 7 3 3 3 3 5 5 4 5 1 4 1 5 3 3 4 5 4 4 4 1 6 1 5 3 4 5 6 5 1 5 5 1 2 5
    ## [42301] 1 4 6 3 2 6 2 4 2 3 1 4 5 1 4 4 2 5 2 5 4 6 7 3 2 4 5 2 2 2 7 1 2 5 6 6
    ## [42337] 5 6 4 4 5 5 4 4 5 4 6 5 6 5 4 6 3 4 4 5 4 5 6 5 5 5 5 3 4 3 4 3 4 5 4 2
    ## [42373] 4 6 3 3 6 3 4 4 3 6 5 6 3 5 6 6 4 3 4 4 5 6 6 4 6 6 5 6 4 6 4 6 5 6 5 3
    ## [42409] 6 5 4 5 5 5 6 6 6 6 2 6 4 6 3 5 6 3 6 3 3 5 3 6 6 2 2 4 5 6 5 3 6 6 5 4
    ## [42445] 6 2 5 3 6 4 4 4 6 4 4 4 4 4 4 4 1 4 4 4 5 5 4 6 6 4 4 6 4 3 3 1 3 3 1 4
    ## [42481] 2 2 4 2 7 5 5 4 3 3 3 2 1 1 1 4 3 2 6 6 6 2 6 5 5 2 1 3 6 3 3 5 6 5 5 5
    ## [42517] 3 4 5 5 3 5 5 5 5 5 3 5 3 6 3 4 4 3 4 3 5 1 4 3 1 6 4 6 6 5 1 5 1 5 5 4
    ## [42553] 2 1 6 5 2 1 3 6 4 3 3 1 4 1 2 1 6 4 2 6 3 6 1 6 7 4 4 4 4 4 3 4 3 3 4 1
    ## [42589] 6 6 4 4 5 5 1 3 3 5 3 4 5 5 4 4 5 4 5 3 3 4 3 5 1 1 3 1 5 2 5 3 5 5 5 5
    ## [42625] 5 5 5 3 4 5 6 6 1 3 4 5 2 6 2 6 5 6 4 4 3 5 5 5 5 5 3 5 5 5 5 3 5 2 4 2
    ## [42661] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 2 3 6 4 2 6 4 2 4 2 2 3 6 3 3 5 5 3 5 6
    ## [42697] 3 4 4 4 4 4 4 4 1 6 4 4 6 3 4 5 4 4 4 1 3 6 4 5 3 1 3 6 4 5 4 4 5 3 6 6
    ## [42733] 4 5 5 4 5 5 5 4 2 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 4 5 5 5 5 4 6 4 1
    ## [42769] 5 1 5 4 6 4 4 3 6 4 4 4 6 3 4 4 2 4 4 4 4 5 5 3 6 2 2 2 2 2 6 2 2 2 2 2
    ## [42805] 2 5 2 2 2 2 2 2 1 2 6 2 2 2 2 2 2 2 5 2 4 5 2 2 2 2 1 2 2 6 6 5 2 2 5 1
    ## [42841] 2 1 2 2 2 2 2 2 2 2 2 5 5 4 5 4 5 5 5 4 5 5 5 5 3 5 3 4 3 6 6 3 5 4 3 5
    ## [42877] 1 3 4 4 4 2 5 2 3 3 4 3 3 4 3 3 5 1 2 2 3 6 6 1 5 5 6 6 6 1 5 4 2 2 2 2
    ## [42913] 2 2 2 2 2 5 1 2 3 2 2 2 5 2 2 1 3 6 4 3 4 4 4 6 2 2 2 2 2 2 2 2 2 2 2 2
    ## [42949] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 4 1 4 3
    ## [42985] 4 8 6 3 5 3 3 5 2 4 2 1 3 1 3 5 4 3 4 3 5 5 5 3 5 5 1 4 5 4 3 4 5 3 3 5
    ## [43021] 5 3 5 4 6 2 2 4 4 1 1 5 5 5 3 5 5 4 4 1 5 5 5 5 1 5 5 4 5 4 5 5 5 1 5 1
    ## [43057] 5 4 5 5 5 5 6 5 5 5 5 5 5 5 5 5 2 5 5 2 5 5 5 4 3 4 4 4 7 3 1 6 2 1 4 6
    ## [43093] 3 6 4 4 5 6 4 5 6 6 2 4 2 4 2 1 2 2 4 6 5 6 6 3 6 5 3 1 3 5 4 4 5 1 4 3
    ## [43129] 4 1 3 3 5 6 5 4 4 4 6 4 4 1 3 3 5 5 5 3 4 4 6 6 5 6 6 6 5 6 5 4 1 5 4 4
    ## [43165] 1 4 1 4 5 5 4 4 6 5 5 2 6 5 3 2 5 3 1 2 2 6 2 2 2 2 4 6 3 2 2 4 2 2 2 2
    ## [43201] 2 2 4 2 2 2 2 2 1 1 4 5 5 4 3 1 1 1 1 8 5 1 5 3 7 5 4 3 6 4 3 4 2 1 2 1
    ## [43237] 2 5 5 3 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [43273] 4 3 4 5 5 5 3 1 6 4 6 4 6 6 6 6 5 3 4 3 3 3 1 4 4 4 4 3 4 4 4 3 5 5 5 5
    ## [43309] 5 5 5 5 5 5 2 5 5 5 1 5 5 6 5 1 5 5 1 5 3 3 4 1 4 4 3 4 5 2 3 6 5 6 3 3
    ## [43345] 6 3 5 4 4 2 4 6 4 6 4 3 4 4 4 3 4 4 4 5 5 3 4 6 6 4 6 6 1 6 4 6 6 2 6 6
    ## [43381] 6 6 2 6 4 4 2 4 1 6 4 4 4 6 2 6 4 4 6 4 6 2 2 3 3 1 4 6 4 3 6 5 1 5 5 6
    ## [43417] 2 5 5 5 4 3 5 5 5 5 5 5 4 5 4 5 2 5 4 5 5 5 6 3 5 6 2 2 2 5 4 6 2 2 6 2
    ## [43453] 2 4 4 2 2 2 2 3 3 6 3 6 6 4 4 5 6 5 3 6 6 3 5 1 4 6 4 4 4 6 2 6 3 3 5 5
    ## [43489] 2 5 5 5 4 5 5 3 5 5 2 4 5 1 5 5 5 5 2 4 6 3 6 4 3 6 1 5 5 5 5 4 5 5 5 5
    ## [43525] 5 5 5 4 1 4 3 1 5 4 6 4 4 5 5 5 5 2 5 5 5 5 5 5 5 5 3 5 5 5 3 5 3 5 5 7
    ## [43561] 4 5 4 3 3 4 3 6 6 6 5 5 5 5 5 5 5 5 5 5 5 5 1 1 3 4 4 4 4 4 3 4 3 2 1 3
    ## [43597] 5 4 7 5 5 5 1 3 5 3 3 3 4 3 6 6 1 3 1 3 3 5 1 5 4 5 5 4 4 3 5 4 5 3 3 2
    ## [43633] 2 2 2 2 5 4 2 1 6 5 5 5 4 3 4 4 4 5 4 1 1 5 2 5 3 5 2 5 2 5 6 5 4 5 1 5
    ## [43669] 5 1 3 5 3 5 6 3 1 1 4 1 1 1 1 4 4 3 4 4 6 3 4 4 4 4 3 4 4 1 6 5 5 5 5 5
    ## [43705] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5
    ## [43741] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 3 5 6 5 5 4 5 5 4 5 4 5 1 5 3 5 4 3
    ## [43777] 3 5 3 6 6 5 1 1 4 1 5 5 3 5 3 4 5 1 3 1 4 2 5 5 2 5 5 5 5 5 5 5 2 2 2 4
    ## [43813] 5 5 5 5 6 4 7 4 2 7 5 5 5 5 1 1 4 7 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4
    ## [43849] 5 5 5 5 2 5 4 5 5 4 5 5 6 5 6 6 3 6 6 6 6 1 6 3 4 2 1 3 6 1 1 4 1 2 5 5
    ## [43885] 5 5 3 5 4 5 4 3 4 6 6 2 4 4 6 6 6 6 6 6 4 1 4 6 6 3 1 1 3 4 4 4 3 7 2 1
    ## [43921] 4 4 4 1 5 3 5 5 6 1 1 4 6 6 1 4 1 1 3 5 5 2 5 2 6 6 5 4 4 5 6 4 1 3 3 2
    ## [43957] 6 6 5 6 3 3 4 3 4 3 4 6 4 1 3 4 2 1 3 5 3 3 3 3 5 3 5 5 5 5 5 5 5 5 5 4
    ## [43993] 1 5 5 2 1 5 4 5 6 6 5 4 1 4 6 1 4 5 1 1 2 2 3 4 2 1 2 3 4 5 5 1 3 3 1 4
    ## [44029] 4 5 4 5 6 5 5 1 6 2 4 6 4 7 4 6 4 5 4 5 3 3 5 5 3 4 5 5 5 3 6 6 4 6 6 4
    ## [44065] 1 6 2 6 6 4 1 3 1 1 5 5 3 6 5 5 5 4 5 5 5 3 1 3 1 3 4 3 4 3 2 3 5 5 5 6
    ## [44101] 1 4 6 6 3 6 6 6 5 2 5 5 6 5 4 3 5 1 4 5 1 5 5 4 5 5 5 1 5 5 1 4 4 3 4 5
    ## [44137] 4 7 3 1 3 3 6 3 3 6 3 6 3 3 1 3 1 5 1 1 4 6 2 1 2 5 2 5 3 4 3 3 5 5 4 6
    ## [44173] 6 6 6 6 4 1 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 1 5 1 5 4 3 1 1 1 4 2 4
    ## [44209] 1 4 6 5 2 2 5 4 1 2 4 5 2 5 6 5 5 2 4 5 5 5 1 3 5 5 5 2 1 1 3 3 3 3 1 4
    ## [44245] 5 4 6 6 4 4 6 6 5 1 5 3 4 3 4 4 4 3 4 4 4 4 6 4 4 4 5 4 1 5 2 3 5 5 2 5
    ## [44281] 5 2 5 5 5 2 5 2 5 6 2 5 2 5 2 6 2 5 5 2 2 5 5 2 5 2 5 2 5 3 4 3 1 1 1 5
    ## [44317] 4 5 1 5 1 3 3 3 5 1 3 4 6 4 6 6 4 6 6 4 3 6 1 6 6 6 6 4 4 6 4 1 5 2 3 3
    ## [44353] 4 3 4 6 1 4 3 1 2 6 2 3 2 4 7 3 3 5 3 3 5 2 5 4 4 7 4 4 6 1 1 1 4 3 5 5
    ## [44389] 5 5 3 3 1 4 5 4 4 4 2 6 2 4 5 4 3 4 5 1 1 5 6 5 6 7 1 4 1 1 6 3 4 4 4 4
    ## [44425] 3 4 5 1 3 1 4 5 3 3 4 4 4 4 5 4 4 3 5 4 6 5 5 4 4 1 6 5 5 5 5 5 5 5 5 5
    ## [44461] 5 3 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 3 5 6 5 4 5 5 5 5 5
    ## [44497] 5 5 6 5 6 4 2 1 1 5 2 3 5 6 5 2 3 4 5 5 2 2 2 2 4 5 6 5 1 1 2 1 4 1 4 1
    ## [44533] 3 3 3 3 4 5 5 4 3 4 4 6 3 3 5 1 2 5 5 3 2 2 2 2 2 2 5 2 2 2 2 5 2 2 2 2
    ## [44569] 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2 2 2 2 2 2 2 4 2 5 6 2 3 5 1 3 1
    ## [44605] 3 4 5 5 5 4 5 4 5 5 4 3 1 1 6 6 4 4 1 5 1 1 2 5 5 1 1 1 3 4 3 5 1 5 5 5
    ## [44641] 5 1 3 2 5 4 5 5 3 5 5 6 1 3 3 1 5 3 6 3 5 6 6 3 3 3 6 5 4 4 4 6 5 2 4 5
    ## [44677] 1 3 4 6 4 4 2 4 4 4 3 4 2 4 6 4 6 1 4 5 3 4 6 3 5 1 6 1 6 1 4 4 4 4 3 4
    ## [44713] 4 3 4 3 4 4 4 4 5 4 4 4 4 6 3 6 8 5 5 5 3 3 5 4 3 5 5 5 5 5 1 1 5 5 5 5
    ## [44749] 5 1 5 3 3 2 1 4 4 4 5 5 3 5 2 5 4 1 5 2 5 4 5 4 1 5 3 3 8 4 5 6 4 2 6 3
    ## [44785] 5 5 5 5 5 5 5 5 6 5 5 5 5 4 1 1 1 4 7 4 3 1 3 2 5 2 2 2 5 3 1 2 1 5 5 5
    ## [44821] 5 5 5 5 5 2 2 5 2 5 3 3 1 8 3 5 5 5 6 4 2 4 3 6 2 6 4 4 6 7 4 5 5 5 5 5
    ## [44857] 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 1 2 2 3 2 1 5 6 3 5 5 1 2 4 7 6 4 4
    ## [44893] 6 4 6 6 6 4 4 1 6 4 6 3 4 6 6 6 6 4 3 3 3 1 1 3 3 5 5 1 6 3 3 2 6 3 6 1
    ## [44929] 6 4 6 3 5 2 5 5 3 4 7 5 4 6 4 1 3 1 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5
    ## [44965] 5 5 5 5 5 5 5 2 5 4 6 7 4 3 5 2 5 5 2 4 5 4 5 4 4 6 5 5 1 5 4 5 1 3 2 2
    ## [45001] 2 2 2 2 2 2 2 2 2 2 2 2 1 5 5 1 5 3 2 2 6 6 2 6 1 6 2 6 6 5 5 5 5 5 5 5
    ## [45037] 5 5 5 3 5 5 5 5 3 6 4 3 6 4 1 4 4 6 4 4 1 5 6 3 6 4 5 1 1 5 5 3 3 1 6 3
    ## [45073] 4 3 5 3 4 3 4 5 3 4 2 2 1 5 4 1 2 3 5 5 5 6 5 5 5 6 4 3 5 6 5 1 4 2 4 4
    ## [45109] 4 5 4 4 6 6 4 4 3 4 4 4 4 3 3 1 3 4 5 4 6 4 4 6 4 5 1 3 3 5 1 5 5 4 1 5
    ## [45145] 1 4 4 3 3 3 4 4 7 6 2 2 5 1 4 5 3 1 4 5 4 4 2 5 4 5 6 5 4 5 4 5 5 5 1 3
    ## [45181] 1 3 3 5 1 5 2 2 2 2 2 2 2 2 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 1
    ## [45217] 3 4 3 3 4 5 5 2 5 3 5 5 5 5 5 5 5 5 3 1 4 4 4 5 5 4 5 4 5 5 5 4 5 5 3 1
    ## [45253] 6 6 3 6 1 3 3 6 3 1 5 4 5 2 5 1 5 4 5 1 3 5 3 3 3 4 6 5 6 5 2 2 7 6 6 3
    ## [45289] 3 3 8 3 6 2 3 4 6 4 2 2 2 2 5 1 5 4 4 1 3 2 5 1 1 3 4 5 2 5 5 4 5 5 5 5
    ## [45325] 4 5 5 5 5 4 6 5 2 2 6 6 2 4 1 1 1 6 6 1 4 6 4 6 6 3 4 6 4 6 3 3 1 4 3 1
    ## [45361] 5 6 6 2 3 6 4 6 2 1 3 5 5 5 5 5 3 5 2 5 5 5 3 1 1 5 3 5 5 5 5 5 5 6 5 3
    ## [45397] 3 3 4 2 6 4 3 6 2 3 4 3 4 4 4 6 6 4 4 2 5 6 4 3 6 6 2 6 6 6 6 2 6 2 2 4
    ## [45433] 6 4 6 5 4 5 5 2 5 5 5 5 3 5 2 4 5 5 5 2 3 4 5 4 3 4 4 4 4 3 3 4 4 3 5 1
    ## [45469] 6 5 3 1 6 6 3 1 5 4 4 3 4 4 3 6 1 3 1 5 3 3 1 4 3 3 1 2 2 7 2 5 4 4 4 7
    ## [45505] 5 3 6 6 3 4 6 2 3 3 6 4 3 4 4 6 3 6 4 1 3 3 1 4 4 1 5 4 5 1 6 4 6 3 4 3
    ## [45541] 1 5 6 6 6 4 6 4 4 6 4 6 4 5 1 4 1 4 1 3 1 2 2 1 5 2 4 3 5 5 5 3 5 5 5 5
    ## [45577] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 2 2 2 5 2 2 2 2 4 5 4 6 4 4 3 3 3 4
    ## [45613] 3 6 1 3 3 4 4 4 4 4 3 3 5 5 1 5 5 1 1 4 1 1 5 5 5 5 3 5 5 5 5 5 5 5 5 2
    ## [45649] 5 5 5 5 5 5 5 1 6 4 6 6 4 4 4 3 3 1 6 3 5 3 6 3 6 1 2 5 5 5 2 3 6 6 4 6
    ## [45685] 6 4 1 1 6 3 1 4 4 3 6 4 6 6 3 6 6 3 4 6 5 2 3 5 5 4 4 4 6 4 5 4 4 5 6 3
    ## [45721] 5 4 1 5 1 4 4 1 3 5 6 3 4 5 6 6 6 3 5 1 5 1 5 5 5 4 5 5 1 5 5 5 5 4 4 4
    ## [45757] 4 2 5 1 3 4 5 3 1 2 2 3 2 6 3 5 4 4 4 1 3 3 3 4 6 4 6 3 3 3 5 5 5 5 5 5
    ## [45793] 6 4 5 4 5 5 4 6 5 5 6 1 5 4 5 4 3 4 4 5 3 4 3 6 1 1 3 1 5 4 5 3 1 6 4 4
    ## [45829] 1 1 4 5 1 5 2 2 5 6 6 5 5 5 5 5 5 4 4 5 2 5 2 5 5 6 3 6 6 6 4 3 6 2 4 6
    ## [45865] 5 4 6 3 4 6 4 4 3 4 4 4 5 6 4 6 6 6 6 4 4 6 4 4 6 2 6 6 6 6 6 6 2 6 2 1
    ## [45901] 6 2 6 2 3 4 3 4 3 3 5 4 5 1 2 6 4 3 3 6 3 4 6 4 4 4 1 4 3 1 5 3 4 4 5 5
    ## [45937] 5 3 6 3 6 4 3 4 3 5 3 1 5 1 5 5 5 5 4 4 1 6 6 5 4 5 6 5 4 4 4 4 6 4 4 4
    ## [45973] 4 4 3 3 6 4 6 3 3 4 5 5 5 5 5 1 2 5 1 3 4 5 3 5 3 4 2 6 2 2 2 4 4 4 6 4
    ## [46009] 1 3 2 2 4 4 2 4 6 5 4 4 6 2 2 6 6 6 5 6 3 3 5 3 4 5 5 1 3 5 5 1 3 4 1 2
    ## [46045] 5 5 5 4 5 5 5 5 5 5 5 5 5 5 2 1 2 5 2 2 3 3 5 2 3 5 4 4 3 3 1 5 4 5 5 5
    ## [46081] 5 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [46117] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [46153] 2 2 2 2 2 2 2 1 5 5 6 4 3 4 4 5 4 4 3 2 4 1 3 3 3 4 5 5 3 5 5 4 4 4 4 4
    ## [46189] 6 6 6 6 4 4 2 6 4 6 6 4 4 4 3 6 4 5 4 6 6 2 2 6 5 5 3 4 1 5 3 3 3 4 3 3
    ## [46225] 3 6 2 6 2 1 4 7 6 2 2 3 3 3 5 5 5 5 5 5 5 4 5 4 5 5 3 1 1 1 3 1 6 3 6 4
    ## [46261] 3 6 3 4 6 1 2 2 2 3 4 6 5 6 4 1 4 5 3 4 1 6 5 5 5 5 5 3 3 3 2 6 5 6 4 6
    ## [46297] 4 4 2 3 1 5 2 2 1 4 6 5 4 1 1 4 2 3 3 3 1 1 4 4 4 4 4 4 6 6 6 4 6 6 6 6
    ## [46333] 5 6 6 4 6 1 6 7 4 4 4 4 4 4 4 4 4 2 4 4 4 4 4 5 4 4 4 4 4 4 6 4 6 1 3 5
    ## [46369] 5 3 5 6 2 5 4 5 2 1 4 2 2 5 3 5 4 5 4 4 1 2 5 3 1 5 5 3 5 5 5 3 5 3 2 3
    ## [46405] 5 5 2 5 2 4 2 5 6 4 2 3 6 5 2 2 2 5 5 2 2 5 2 4 2 2 2 6 2 2 2 5 5 2 5 2
    ## [46441] 5 5 2 5 6 5 5 5 2 2 4 5 5 2 4 4 1 3 4 4 3 4 2 3 1 5 5 5 5 1 1 5 1 1 3 5
    ## [46477] 5 5 5 5 5 5 5 5 2 5 5 5 6 4 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5
    ## [46513] 5 5 2 5 5 5 5 1 5 1 5 5 5 3 5 6 5 6 6 6 6 6 4 4 4 6 1 3 5 4 1 5 1 5 4 5
    ## [46549] 6 5 5 6 4 4 4 5 2 1 2 5 4 2 2 4 4 4 5 4 5 3 3 5 2 3 6 2 2 3 6 6 6 4 6 4
    ## [46585] 3 6 6 6 6 6 6 5 6 3 4 6 3 6 4 6 5 3 2 3 3 4 4 5 1 3 4 2 6 6 3 4 4 2 6 4
    ## [46621] 3 2 6 6 1 1 1 4 2 6 1 1 2 1 1 4 4 6 3 4 3 4 4 4 4 3 3 5 5 5 5 5 5 5 5 5
    ## [46657] 5 1 6 3 1 4 4 5 5 5 1 5 5 5 1 5 2 4 3 5 5 5 4 4 2 1 4 6 4 3 2 4 6 5 6 4
    ## [46693] 2 2 2 6 2 4 4 2 2 2 5 4 4 5 5 4 5 1 5 5 7 1 3 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [46729] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 3 5 5 5 4 5 5 5
    ## [46765] 5 4 5 5 5 5 2 5 5 5 5 1 5 5 5 5 5 5 5 5 3 5 5 3 2 2 1 5 1 2 2 1 6 2 2 2
    ## [46801] 1 4 3 1 1 2 2 2 2 5 2 2 6 4 2 4 4 5 1 6 2 4 5 1 2 1 4 1 1 6 3 5 5 2 3 1
    ## [46837] 6 5 5 3 6 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [46873] 5 5 5 5 3 5 5 5 5 3 6 4 6 3 3 3 6 1 3 3 5 5 2 1 5 4 5 5 4 4 3 3 3 1 3 3
    ## [46909] 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 1 2 2 2 2 2 6 4 4 2 3 1 3 4
    ## [46945] 6 4 6 4 4 4 4 4 6 5 1 4 1 3 1 1 3 3 1 5 3 3 3 1 5 5 4 5 5 4 1 5 1 6 6 6
    ## [46981] 4 6 4 6 5 3 5 6 3 1 5 3 6 3 4 5 6 5 5 5 1 2 2 2 2 2 5 2 3 1 5 5 5 5 4 4
    ## [47017] 3 1 4 1 1 3 4 5 5 5 5 5 5 5 5 6 2 5 5 5 5 5 5 3 5 5 5 5 5 5 1 4 5 5 5 4
    ## [47053] 5 1 3 4 4 4 6 3 2 5 5 1 2 5 5 1 5 5 5 5 2 1 5 2 1 5 5 2 5 2 1 5 5 5 5 5
    ## [47089] 5 4 5 5 5 6 4 5 5 5 6 6 2 3 6 2 5 5 5 5 5 3 4 6 6 6 4 4 3 1 5 4 4 2 5 4
    ## [47125] 5 5 2 5 5 2 5 2 5 5 5 5 2 6 5 5 4 5 2 5 1 3 5 2 3 1 1 3 2 4 5 5 1 5 1 2
    ## [47161] 6 6 5 2 3 3 4 2 6 6 4 4 3 4 6 6 6 4 6 5 5 5 5 5 1 5 5 4 6 5 1 5 5 3 5 4
    ## [47197] 5 2 4 3 6 5 5 4 5 5 1 6 6 4 3 4 4 3 5 4 5 3 4 3 1 1 2 3 5 1 5 1 2 5 5 6
    ## [47233] 4 5 1 4 4 6 1 3 6 3 3 6 5 5 6 6 4 6 6 6 6 3 4 4 6 4 3 3 3 3 6 6 4 3 3 3
    ## [47269] 3 5 1 6 4 1 1 3 4 4 4 4 5 4 6 5 3 1 3 4 6 4 6 4 6 6 3 5 3 5 2 5 5 5 5 5
    ## [47305] 2 5 5 5 5 1 3 5 3 1 8 3 8 3 3 5 5 5 5 3 2 6 4 5 5 1 1 5 5 5 5 5 5 5 5 4
    ## [47341] 2 3 4 3 5 5 5 2 5 5 5 5 3 2 5 5 5 5 4 4 6 3 4 6 3 3 3 3 6 5 6 3 2 2 2 2
    ## [47377] 2 2 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 6 3 1 4 2 2 3 5 4 4 1 5 3 5 5 6 3 6
    ## [47413] 4 1 3 5 8 3 3 3 3 5 1 1 5 1 4 2 1 4 5 5 1 3 5 6 1 3 3 3 1 5 5 5 2 5 5 5
    ## [47449] 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 1 3 3 1 6 5 2 4 5 3 4 5 5 5 5 5 5
    ## [47485] 4 3 5 5 1 3 5 4 4 6 2 1 4 4 4 6 4 4 6 6 6 6 3 4 5 5 5 4 4 4 1 6 4 3 3 6
    ## [47521] 1 4 8 5 1 3 3 3 6 3 3 6 2 3 6 3 6 5 3 4 3 5 2 3 3 2 5 5 5 5 5 2 5 5 5 2
    ## [47557] 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 4 4 3 6 5 3 6 1 1 3 6 6
    ## [47593] 3 1 6 5 6 6 6 6 4 6 6 1 4 5 1 2 5 6 4 6 2 2 4 6 5 3 2 6 5 5 5 5 5 3 5 5
    ## [47629] 5 5 5 4 3 4 8 8 4 6 6 6 4 4 4 6 6 2 4 4 4 6 6 4 6 3 1 3 5 1 2 2 4 3 3 2
    ## [47665] 4 5 5 2 5 5 5 1 7 2 2 6 4 6 2 4 4 6 2 4 4 2 4 4 4 3 4 3 3 5 1 5 1 2 2 5
    ## [47701] 2 5 5 5 2 2 5 2 5 2 5 5 2 2 2 2 2 2 5 2 2 5 2 2 5 2 5 5 2 5 5 2 2 2 4 5
    ## [47737] 2 2 2 5 2 5 2 2 5 5 5 2 5 2 5 2 1 2 1 2 2 2 5 5 2 2 2 4 2 5 2 5 2 5 3 1
    ## [47773] 5 1 3 3 1 3 3 6 3 4 3 3 3 2 6 6 4 5 4 4 3 6 5 6 1 1 4 5 6 6 6 6 6 2 6 4
    ## [47809] 6 4 6 6 6 6 5 2 6 4 4 6 1 6 4 7 4 5 5 3 5 3 5 3 5 1 4 3 5 5 6 6 5 4 6 6
    ## [47845] 1 1 4 1 1 5 2 2 2 2 2 2 6 2 2 2 2 2 2 6 5 2 6 2 2 2 2 6 2 2 2 2 2 2 2 2
    ## [47881] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 4 3 6 4 6 3 6 1 1 4 5 3 3 3 3 5 2 2
    ## [47917] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 5 4 2 2 2 2 2 2 2 2 2 2 2 5 2 4 2 2 2 2
    ## [47953] 2 2 6 2 2 2 2 3 2 4 2 2 2 2 3 2 5 3 3 3 4 4 2 5 4 1 6 5 5 5 2 2 5 5 5 5
    ## [47989] 2 5 2 5 5 2 4 2 5 5 2 5 2 5 3 2 2 2 5 2 5 5 5 5 5 5 2 2 3 4 5 3 7 2 6 2
    ## [48025] 4 5 5 4 3 5 1 6 4 4 6 6 4 4 6 6 6 6 4 4 1 5 3 4 3 3 3 6 3 6 3 1 4 1 3 5
    ## [48061] 6 4 4 6 6 4 3 2 2 2 2 2 2 6 2 2 3 5 5 1 5 6 5 2 2 3 5 2 3 2 5 5 5 2 5 3
    ## [48097] 5 4 5 1 5 5 3 5 1 3 5 1 3 5 6 4 3 4 4 6 6 3 3 3 4 4 6 5 6 6 3 2 6 3 6 6
    ## [48133] 2 5 6 2 6 3 1 6 4 5 5 5 3 3 1 2 4 4 5 1 1 5 5 3 5 5 5 3 5 2 5 5 4 4 5 4
    ## [48169] 6 6 6 5 6 6 5 3 4 6 6 4 5 5 5 5 5 2 5 5 4 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5
    ## [48205] 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 4 5 5 5 5 2 5 5 5 5 5
    ## [48241] 4 3 2 2 2 2 2 3 3 5 4 5 1 3 5 3 5 3 6 6 5 6 6 4 1 2 5 4 6 4 2 6 5 6 6 5
    ## [48277] 6 3 3 2 3 2 6 5 4 1 4 4 4 6 2 6 3 3 3 3 3 3 4 5 5 5 1 5 2 3 3 4 3 6 5 4
    ## [48313] 2 5 5 5 5 5 1 3 5 5 1 1 1 3 4 1 5 5 5 4 3 6 6 4 5 6 6 6 3 5 4 4 4 3 2 6
    ## [48349] 4 4 6 2 4 4 6 5 1 5 4 3 3 1 5 3 5 4 6 6 4 4 1 2 3 1 1 3 4 5 4 4 2 5 5 4
    ## [48385] 5 5 3 5 5 5 5 5 5 5 5 6 5 5 5 3 4 3 3 3 6 2 6 5 4 4 4 3 6 5 6 4 2 3 1 4
    ## [48421] 1 1 6 1 6 3 4 6 3 6 4 4 5 4 4 4 4 6 3 5 6 4 3 5 1 6 4 2 6 3 1 1 1 1 5 5
    ## [48457] 5 6 5 3 3 3 3 1 4 1 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [48493] 5 4 5 5 5 5 5 5 6 2 3 2 5 6 3 4 2 1 3 4 3 4 6 3 2 5 5 4 1 5 4 5 5 3 3 5
    ## [48529] 5 5 4 5 5 5 4 5 5 5 5 5 5 4 5 5 5 6 4 6 1 4 1 5 6 4 4 2 6 4 1 4 3 3 3 4
    ## [48565] 4 4 4 4 7 5 2 1 5 6 6 6 6 6 3 6 5 4 6 6 6 4 6 4 6 6 4 4 4 5 5 4 5 5 5 5
    ## [48601] 5 5 5 5 5 5 1 3 4 5 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 6 5 5 5 5 5 5 4
    ## [48637] 3 5 3 5 1 5 3 4 5 2 5 4 1 1 3 1 5 5 6 5 3 5 3 2 4 3 3 3 5 5 6 5 5 5 1 1
    ## [48673] 5 3 5 2 3 4 5 3 4 4 6 3 3 4 6 4 3 3 6 5 4 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [48709] 2 5 5 5 5 4 4 3 4 4 5 4 6 6 6 6 4 5 4 5 5 4 5 5 1 5 5 5 5 5 5 5 3 5 3 3
    ## [48745] 5 6 3 2 6 5 5 5 5 4 4 5 5 3 5 5 5 5 5 4 6 6 1 4 6 4 4 3 5 3 1 3 3 1 5 5
    ## [48781] 5 5 4 5 5 5 5 5 5 5 5 5 5 3 5 5 5 6 2 6 2 6 4 6 3 6 3 5 1 2 6 3 5 3 4 3
    ## [48817] 5 5 3 1 4 3 4 1 2 1 4 1 4 1 6 2 6 3 1 2 6 3 2 6 6 6 6 2 3 1 6 6 1 2 4 5
    ## [48853] 4 1 2 3 1 4 5 3 3 6 6 1 1 6 6 4 4 6 6 3 4 4 3 4 6 6 6 6 3 6 3 4 3 1 5 5
    ## [48889] 5 3 4 3 4 5 4 6 6 6 6 6 6 6 4 2 3 2 6 3 4 6 4 6 5 3 5 5 5 5 6 3 5 6 2 3
    ## [48925] 5 5 2 2 6 5 2 5 6 1 5 5 5 2 3 6 3 6 6 6 6 6 6 4 4 4 3 1 4 4 6 3 4 4 5 5
    ## [48961] 4 1 4 5 5 6 5 3 4 4 3 3 3 5 3 5 4 1 6 3 5 1 1 3 5 5 5 3 4 2 4 6 6 4 4 4
    ## [48997] 3 4 5 4 1 4 7 2 1 2 4 2 2 3 5 5 5 3 5 5 3 1 4 4 1 4 2 4 4 5 5 5 7 5 5 5
    ## [49033] 5 4 5 2 5 5 5 5 3 5 5 3 5 3 1 3 5 5 5 5 5 2 5 6 5 5 5 5 5 5 3 5 5 4 4 1
    ## [49069] 4 4 4 3 1 1 1 2 5 4 4 6 4 6 6 3 5 4 3 5 3 6 5 5 5 5 1 5 5 5 5 5 3 5 5 5
    ## [49105] 5 5 4 3 3 3 5 3 2 3 3 1 1 1 3 4 1 1 5 6 4 6 6 6 6 1 6 6 6 5 6 5 5 6 5 6
    ## [49141] 5 6 4 5 5 4 4 4 4 2 2 2 2 2 6 4 4 4 4 2 6 3 2 3 4 3 6 4 3 1 4 4 4 3 4 4
    ## [49177] 4 2 4 4 3 5 5 2 5 5 5 5 2 1 4 3 2 2 5 5 5 1 2 1 5 1 2 1 1 3 3 4 3 4 3 3
    ## [49213] 5 6 4 1 4 4 6 4 2 6 5 3 3 3 3 5 2 5 5 5 2 5 5 2 5 5 4 2 2 5 2 1 7 4 3 3
    ## [49249] 3 6 2 5 3 4 4 7 1 1 2 3 3 4 3 5 4 4 4 4 4 6 6 6 3 5 6 4 4 6 6 1 4 6 4 7
    ## [49285] 5 5 6 2 5 2 2 5 5 5 3 1 3 6 5 1 2 5 4 5 2 5 4 6 2 5 1 6 5 5 4 3 6 1 1 6
    ## [49321] 5 5 7 5 2 5 5 2 3 5 3 3 3 3 4 3 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 2 5 5
    ## [49357] 5 5 5 5 5 4 5 5 2 2 6 4 4 3 4 3 3 5 5 3 4 5 5 4 1 4 5 5 5 5 5 5 5 5 5 5
    ## [49393] 2 5 5 5 3 6 1 2 3 4 4 4 6 3 6 1 2 5 5 5 5 2 5 5 5 5 5 5 5 2 5 4 5 5 2 2
    ## [49429] 4 6 2 5 5 5 5 5 5 3 3 2 3 3 3 3 1 6 4 5 6 6 4 6 1 1 4 4 6 2 6 6 4 1 2 4
    ## [49465] 4 4 6 3 6 2 6 4 3 4 6 4 5 5 1 3 5 4 3 5 4 5 4 3 2 3 3 3 4 3 3 3 3 3 5 3
    ## [49501] 5 3 5 6 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 1 2 4 3 6 6
    ## [49537] 6 5 6 4 1 4 3 3 1 6 6 4 1 3 4 6 2 2 4 3 2 3 1 3 4 1 5 6 1 2 4 5 8 4 5 3
    ## [49573] 5 2 2 2 2 4 6 5 5 4 4 4 6 1 5 1 3 1 5 5 5 3 5 1 4 5 5 1 5 5 2 5 2 2 5 1
    ## [49609] 5 5 2 1 5 5 6 1 5 3 5 1 8 1 4 5 6 4 6 4 4 6 6 4 6 3 3 6 4 6 2 4 2 5 1 5
    ## [49645] 5 5 5 1 5 5 1 6 3 4 3 3 4 7 6 4 4 4 4 4 6 4 4 3 3 1 5 1 1 3 4 7 6 1 5 5
    ## [49681] 2 6 3 4 3 5 5 5 5 2 5 5 4 5 3 6 6 6 6 6 6 6 5 4 6 6 4 7 4 7 1 3 3 1 3 5
    ## [49717] 3 5 5 3 4 2 2 1 2 2 4 4 6 2 3 6 2 1 5 2 4 4 4 1 3 5 1 1 5 5 4 5 3 3 4 3
    ## [49753] 1 3 5 5 5 4 5 5 5 4 5 5 4 4 5 3 3 5 6 2 4 5 3 2 5 7 6 1 5 6 5 4 1 5 6 3
    ## [49789] 5 1 3 6 4 6 4 6 4 7 4 4 4 4 3 3 5 5 5 5 4 5 6 1 4 2 2 5 2 2 2 5 5 5 2 5
    ## [49825] 3 2 2 4 2 2 5 5 3 2 5 6 2 5 2 2 5 5 2 2 5 5 2 5 5 2 5 2 4 3 4 5 6 3 5 3
    ## [49861] 4 3 3 6 4 4 4 4 4 4 1 4 4 3 3 4 5 6 4 4 1 5 1 2 1 4 3 5 6 6 1 1 1 3 1 4
    ## [49897] 3 3 3 3 4 4 3 4 2 6 6 2 6 4 6 5 4 4 6 2 1 3 6 4 5 5 5 5 4 5 5 5 2 3 6 4
    ## [49933] 4 4 4 3 4 6 6 3 2 2 5 6 1 5 6 5 5 5 4 5 6 5 1 1 5 5 5 5 5 5 5 3 1 5 2 2
    ## [49969] 4 5 6 6 4 4 6 5 6 5 4 4 5 4 1 5 5 4 1 3 2 4 6 6 6 4 6 6 4 4 2 4 2 2 6 6
    ## [50005] 6 4 4 4 5 1 6 5 2 6 2 7 4 7 4 6 2 4 4 4 3 6 4 1 6 4 1 5 2 3 2 2 2 4 1 1
    ## [50041] 4 4 3 3 3 3 5 5 1 5 5 5 1 5 3 3 1 4 4 1 1 5 5 5 5 1 1 5 1 5 5 5 1 5 5 5
    ## [50077] 5 5 5 5 5 5 5 5 5 1 4 5 5 1 5 1 4 2 4 4 4 3 5 5 2 3 2 3 1 6 6 4 3 3 6 3
    ## [50113] 5 5 5 1 3 3 5 1 3 3 1 6 5 6 4 4 4 6 1 2 1 1 6 6 4 1 3 2 3 4 4 4 2 4 5 5
    ## [50149] 1 4 4 2 5 5 2 6 5 2 1 2 4 5 6 4 4 4 2 4 1 1 5 5 2 5 5 5 2 5 5 5 5 5 5 5
    ## [50185] 5 5 5 5 5 5 1 5 3 2 3 3 3 4 5 4 5 3 5 4 1 6 4 1 6 6 4 4 3 5 4 6 5 3 3 4
    ## [50221] 4 6 2 4 6 3 4 4 4 4 4 3 4 1 3 3 2 5 5 5 5 5 5 5 5 5 5 2 3 5 5 5 5 5 5 3
    ## [50257] 2 2 4 2 3 2 2 2 6 2 3 2 2 1 2 2 2 5 5 2 2 3 5 5 5 2 4 1 6 5 5 5 3 1 3 3
    ## [50293] 3 3 3 4 6 4 6 6 6 6 4 4 4 6 4 5 4 6 4 4 6 3 2 6 3 2 3 1 3 4 3 3 3 3 3 1
    ## [50329] 5 3 1 1 3 4 4 3 4 1 2 5 3 4 4 1 2 4 5 5 5 5 2 1 4 3 6 3 6 6 3 3 3 6 3 6
    ## [50365] 3 6 6 6 7 5 5 5 4 6 4 4 4 4 4 6 4 4 3 1 4 4 6 4 3 6 3 2 6 5 4 6 4 6 6 4
    ## [50401] 2 6 4 6 2 6 4 6 4 5 4 6 6 4 4 1 1 5 5 6 1 1 4 5 4 5 5 5 5 3 3 6 3 4 3 4
    ## [50437] 3 4 6 6 6 5 3 2 5 5 5 5 4 4 5 4 4 5 3 5 5 5 6 5 5 5 6 5 5 3 3 2 4 5 1 5
    ## [50473] 5 3 5 5 5 3 1 6 4 5 3 4 6 2 3 4 3 3 2 5 5 4 7 5 5 5 5 5 5 5 5 5 3 5 5 5
    ## [50509] 2 5 5 5 5 5 4 4 3 1 6 4 1 1 4 6 6 1 3 6 4 4 6 6 3 4 6 6 3 3 4 5 5 5 1 1
    ## [50545] 1 1 3 3 3 5 4 2 4 5 4 4 4 4 3 4 5 1 3 4 6 4 4 6 6 3 6 4 4 6 6 6 4 3 5 3
    ## [50581] 3 5 5 4 5 4 3 3 4 5 3 3 1 1 3 5 5 2 5 5 5 5 5 5 2 5 3 5 5 5 5 5 5 3 3 4
    ## [50617] 6 4 4 4 3 1 4 4 3 1 4 4 1 1 1 7 1 1 4 1 4 1 3 4 4 4 5 4 1 2 5 5 5 5 5 1
    ## [50653] 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 2 3 2 6 2 5 2 2 5 2
    ## [50689] 1 4 3 4 4 3 4 4 4 4 4 6 5 4 1 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 4 1 4 5 5
    ## [50725] 3 2 6 1 5 4 3 3 5 5 5 5 5 3 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 4 3
    ## [50761] 3 3 3 2 3 4 1 1 1 1 5 1 4 5 1 4 4 5 4 6 4 3 5 5 5 5 5 1 5 5 5 5 5 1 5 5
    ## [50797] 1 5 5 5 5 5 4 5 5 5 5 5 5 5 5 7 5 1 4 6 3 4 6 3 6 5 5 5 2 1 4 1 6 6 6 3
    ## [50833] 4 6 4 3 1 4 1 4 5 5 6 3 3 5 1 6 4 5 5 3 4 1 3 6 6 1 6 6 3 5 1 5 5 5 5 5
    ## [50869] 5 5 5 3 5 3 6 6 6 5 6 4 6 6 6 5 6 5 5 5 5 5 5 1 3 5 5 4 5 3 4 4 4 6 5 4
    ## [50905] 3 6 4 3 3 3 3 1 2 3 4 5 3 4 5 6 5 5 3 5 5 6 6 4 4 5 5 5 5 5 5 5 6 5 5 5
    ## [50941] 5 4 8 3 4 6 4 4 4 3 6 6 6 6 6 5 4 6 3 6 4 1 6 6 4 4 6 4 7 6 7 4 5 3 4 1
    ## [50977] 5 5 3 3 5 5 5 5 5 5 5 3 5 5 5 2 5 5 5 5 5 5 5 5 3 1 1 1 4 3 3 4 1 3 1 5
    ## [51013] 3 4 3 6 5 5 5 5 5 5 5 5 2 5 5 3 3 1 1 3 4 4 4 1 4 6 4 6 4 4 1 4 4 4 6 4
    ## [51049] 4 6 3 4 4 4 4 4 2 6 4 4 4 4 3 4 4 3 5 4 6 1 1 1 4 5 5 5 2 5 6 5 5 5 5 5
    ## [51085] 5 5 5 5 5 5 4 5 5 5 1 3 5 1 4 5 3 5 4 5 5 1 6 5 5 4 3 6 5 4 5 4 5 3 1 2
    ## [51121] 5 2 4 5 6 1 3 1 1 3 4 3 3 4 2 5 2 5 6 4 3 4 4 2 2 3 4 5 6 5 3 6 3 4 3 3
    ## [51157] 3 1 5 5 2 5 5 4 4 1 2 3 5 1 2 2 2 2 3 1 3 3 3 3 6 5 3 4 6 5 4 3 4 6 6 4
    ## [51193] 3 3 5 6 6 4 4 6 3 3 3 3 4 4 5 8 4 2 1 2 1 6 3 4 6 6 6 5 3 4 5 2 3 1 1 4
    ## [51229] 4 5 5 5 5 4 4 1 4 5 1 3 3 3 5 3 6 6 6 6 4 4 3 4 4 5 6 6 5 5 6 3 6 2 4 4
    ## [51265] 4 3 3 1 1 1 1 5 5 3 4 8 1 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 2 5 5
    ## [51301] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 3 7 2 2 4 2 2 2 2 2 2 2 2 5 7 6 4
    ## [51337] 5 5 4 6 1 3 3 3 3 3 3 1 4 3 4 4 3 3 4 4 3 4 2 4 2 1 3 4 3 3 3 1 1 5 2 5
    ## [51373] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 2 5 5 4 3 4 6 4 3 4 5 6
    ## [51409] 3 3 2 4 4 4 3 3 1 6 4 3 2 6 6 5 1 5 5 4 4 4 4 3 6 3 6 3 3 4 5 4 4 3 4 4
    ## [51445] 5 1 3 1 4 6 3 4 1 2 5 1 6 6 3 5 4 3 1 5 3 1 3 1 1 6 4 4 5 6 1 4 5 4 6 4
    ## [51481] 6 6 2 4 6 3 3 5 5 5 5 5 5 6 5 2 5 3 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5
    ## [51517] 5 5 1 1 2 4 2 1 4 2 4 3 5 4 4 4 4 4 4 3 2 4 4 4 6 4 3 4 4 4 3 4 4 4 4 4
    ## [51553] 6 4 4 3 5 5 5 5 5 5 5 5 5 5 5 4 5 3 5 5 5 5 3 3 3 1 5 5 5 3 5 4 4 5 4 2
    ## [51589] 1 2 6 7 6 2 4 2 2 2 2 2 2 3 2 1 2 2 2 2 2 6 2 2 2 5 5 4 5 5 1 5 5 4 5 5
    ## [51625] 5 5 5 5 5 5 5 1 5 3 5 5 8 2 3 2 2 4 6 4 2 1 3 1 3 3 5 3 1 1 6 5 5 2 1 4
    ## [51661] 6 6 5 5 1 1 3 2 4 5 3 6 1 4 4 4 3 3 3 5 4 3 4 3 3 3 3 3 3 1 5 7 2 2 2 1
    ## [51697] 2 2 2 1 1 5 3 5 5 3 3 4 6 2 3 6 2 4 2 6 3 2 3 3 5 3 3 3 3 3 2 1 1 5 3 5
    ## [51733] 1 5 5 5 4 4 3 4 4 4 4 6 4 4 6 6 3 6 6 6 4 6 4 3 5 1 5 3 4 6 5 2 6 6 2 2
    ## [51769] 6 2 6 1 1 4 1 2 4 4 2 6 2 6 4 3 6 6 2 4 6 4 3 4 2 6 6 6 4 3 6 4 2 6 4 4
    ## [51805] 6 4 4 2 6 4 6 4 2 6 4 3 6 2 4 6 3 3 6 4 2 6 6 6 4 6 3 6 6 6 6 4 3 4 6 3
    ## [51841] 2 6 4 6 2 4 2 4 4 6 6 2 6 6 3 3 4 5 4 3 4 3 1 2 3 6 2 2 3 3 1 1 3 5 2 4
    ## [51877] 5 4 5 4 6 3 5 3 3 3 4 1 3 4 1 3 2 5 2 2 5 5 2 5 1 6 6 6 3 5 5 5 5 5 5 5
    ## [51913] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 4 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 3
    ## [51949] 3 3 3 6 4 1 3 4 5 5 5 5 3 5 5 4 6 5 5 5 1 5 4 4 5 4 1 3 5 5 1 2 3 3 6 3
    ## [51985] 3 1 4 1 2 5 5 2 4 6 4 6 6 4 6 4 6 4 6 3 5 5 3 5 5 1 6 1 4 3 1 4 7 5 5 1
    ## [52021] 2 2 2 4 3 6 5 5 5 5 3 3 3 5 3 5 4 5 1 4 2 2 6 2 5 3 3 4 4 4 2 4 1 2 1 5
    ## [52057] 1 5 1 4 6 2 2 5 2 4 5 5 5 5 5 5 5 5 5 5 3 3 5 4 5 5 6 4 3 5 3 5 5 5 5 1
    ## [52093] 2 2 2 2 2 1 2 2 2 2 2 2 6 2 2 2 2 2 6 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [52129] 4 2 6 2 6 2 6 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 6 2 2 2 2 2 2 1 2 2 2 2
    ## [52165] 6 2 2 6 2 2 2 2 2 2 2 2 4 2 5 2 2 2 2 4 6 6 4 2 6 2 2 2 5 2 2 2 2 2 2 2
    ## [52201] 2 2 2 2 6 2 2 2 4 2 2 2 3 5 2 1 4 2 5 4 4 4 1 5 5 4 4 3 4 3 3 2 2 2 3 4
    ## [52237] 1 5 3 1 3 6 4 3 4 3 6 4 6 4 3 5 3 4 4 3 3 6 6 1 3 3 6 6 4 3 6 3 3 4 4 5
    ## [52273] 4 6 4 5 4 4 4 3 5 5 3 5 6 6 4 6 5 4 4 4 6 6 3 4 6 4 3 4 3 6 5 1 5 4 6 6
    ## [52309] 6 4 6 4 4 5 6 4 4 6 6 4 5 1 5 3 4 2 1 2 1 2 5 1 2 1 4 4 4 2 7 2 4 2 2 2
    ## [52345] 2 5 1 3 2 4 1 3 5 5 2 1 5 1 2 5 5 3 5 4 3 6 6 6 4 4 4 3 6 3 5 5 5 5 5 5
    ## [52381] 2 4 5 5 5 5 5 5 5 5 5 5 5 5 4 1 4 3 5 5 5 3 1 1 1 6 6 4 4 4 4 6 6 4 4 4
    ## [52417] 3 5 4 2 5 4 4 5 4 6 4 4 5 4 4 5 5 5 1 4 4 4 6 4 2 4 4 4 2 6 2 1 5 5 4 4
    ## [52453] 4 5 6 5 4 5 5 4 4 5 4 6 5 4 4 5 5 2 2 6 4 4 5 5 4 3 4 5 2 3 5 6 4 5 5 1
    ## [52489] 4 3 5 1 1 7 1 5 5 5 1 5 5 5 2 3 2 2 4 2 4 6 4 5 6 5 1 5 6 4 5 4 6 3 5 4
    ## [52525] 6 4 4 3 1 3 6 2 6 4 5 3 5 5 4 5 4 5 1 4 3 4 1 4 4 3 5 5 4 4 6 6 3 3 6 6
    ## [52561] 1 6 4 1 1 4 2 3 3 3 3 6 3 6 3 6 4 6 4 6 6 3 4 4 6 4 4 2 6 4 6 6 3 1 4 1
    ## [52597] 3 5 1 4 4 3 3 5 1 5 3 1 6 2 4 1 2 4 6 4 6 4 2 1 5 5 5 4 5 5 3 5 5 5 5 5
    ## [52633] 4 4 4 4 6 1 1 6 1 2 1 4 1 4 4 4 4 4 4 3 4 3 4 3 4 4 4 3 4 3 3 5 3 5 4 3
    ## [52669] 4 4 1 1 5 5 5 5 3 3 1 1 5 1 4 4 1 2 4 4 6 4 7 5 2 5 1 4 5 5 4 1 1 1 1 3
    ## [52705] 5 1 5 3 3 5 3 5 3 5 2 3 3 4 4 5 4 4 5 3 1 6 4 4 3 3 6 4 4 4 4 3 4 3 5 4
    ## [52741] 6 4 6 4 6 1 6 5 6 1 4 5 4 6 4 4 4 5 3 2 2 2 5 5 5 4 3 5 5 5 1 5 5 3 3 5
    ## [52777] 5 5 3 6 4 3 3 5 5 3 5 4 3 3 2 1 4 4 1 3 1 3 2 3 5 4 3 1 1 3 6 5 1 3 4 3
    ## [52813] 4 6 6 3 4 4 4 4 5 4 4 6 6 2 1 2 2 5 2 1 4 1 5 1 2 1 1 1 5 3 4 4 4 6 4 6
    ## [52849] 6 2 1 5 5 5 5 7 5 5 5 3 4 4 4 4 4 3 6 4 6 3 6 2 7 4 4 5 6 5 5 5 3 4 5 5
    ## [52885] 5 1 3 1 3 3 3 4 6 2 3 3 4 1 5 4 5 3 4 5 2 1 5 3 2 6 2 6 2 2 2 2 2 2 5 5
    ## [52921] 3 5 5 4 4 4 4 1 6 1 3 3 3 1 4 3 3 6 4 6 4 3 3 2 3 2 2 2 2 2 2 2 2 2 2 2
    ## [52957] 2 2 2 2 2 2 6 2 6 4 6 6 6 6 6 4 2 4 6 6 4 4 5 4 3 5 5 4 3 1 1 1 1 1 1 1
    ## [52993] 1 1 3 4 5 5 4 5 2 3 4 5 5 5 5 5 3 4 6 1 5 5 5 5 1 5 5 5 5 5 1 5 5 5 5 5
    ## [53029] 5 5 2 2 5 1 5 5 5 5 5 5 5 5 5 5 5 2 5 3 3 3 3 3 3 4 3 3 3 1 6 5 4 2 5 5
    ## [53065] 6 3 1 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 4 5 5 5 1 5 5 4 3 4 4 1 4 3 4 3 3
    ## [53101] 6 6 6 4 6 6 3 6 4 3 3 2 3 6 1 1 1 4 5 5 1 5 5 5 5 5 1 3 5 7 4 5 2 5 4 5
    ## [53137] 2 5 5 5 3 3 5 5 5 5 1 5 2 3 5 3 1 4 5 5 5 4 5 4 1 3 3 5 5 1 5 5 5 5 3 5
    ## [53173] 5 3 5 5 1 1 3 2 2 2 2 2 4 2 2 2 2 4 1 2 3 2 2 2 2 2 2 6 2 2 5 2 2 2 2 1
    ## [53209] 2 2 5 2 5 5 5 5 5 2 1 5 5 5 2 5 1 2 3 4 3 6 4 3 1 8 1 3 3 4 4 6 4 3 3 4
    ## [53245] 4 4 5 4 2 3 3 5 4 2 4 2 2 2 2 2 2 2 2 4 2 3 3 4 5 1 5 3 5 1 1 3 5 5 1 3
    ## [53281] 5 3 1 5 3 3 4 3 4 4 3 3 4 1 3 2 2 2 2 2 6 2 2 2 2 2 7 2 2 2 5 5 5 3 5 1
    ## [53317] 3 3 3 3 4 4 5 4 5 3 5 4 4 4 6 4 4 5 3 5 4 6 2 5 4 3 1 5 5 5 6 5 3 3 4 4
    ## [53353] 3 3 4 3 5 2 2 6 2 5 5 3 5 5 1 1 5 2 6 1 5 2 2 1 1 6 3 4 1 2 2 2 2 2 4 2
    ## [53389] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 4 5 5 2 2 2 2 4 2 2 2 2 2 2 2 2 2
    ## [53425] 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [53461] 2 2 6 2 2 2 2 2 2 2 4 2 2 2 5 2 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2 6
    ## [53497] 2 4 2 4 2 2 6 6 2 2 2 2 2 2 2 2 2 2 2 2 1 5 2 2 2 2 2 2 2 2 4 2 2 2 2 2
    ## [53533] 2 2 2 2 1 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2 4 2 2 2 2 2 2 2 2
    ## [53569] 2 3 1 5 5 5 6 4 4 5 2 1 1 5 1 2 5 4 5 6 4 4 2 4 6 4 3 4 3 4 5 4 5 5 6 5
    ## [53605] 5 6 4 6 6 5 3 3 3 3 3 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 6 3 4 5 5 5 5 5
    ## [53641] 5 5 2 3 1 6 4 4 3 4 6 4 4 6 3 4 6 3 6 2 2 5 2 7 5 5 5 4 3 6 4 1 1 1 6 5
    ## [53677] 7 1 3 4 6 5 5 5 5 2 5 5 3 5 2 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5
    ## [53713] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 4 5 5 5 5 7 5 5 5 5 4 5 7 6 5
    ## [53749] 3 6 1 3 6 1 4 4 5 5 6 6 5 1 1 1 3 5 3 5 4 6 3 4 4 4 6 4 3 6 4 5 1 1 3 5
    ## [53785] 5 5 4 1 5 4 1 5 5 3 4 3 1 4 4 4 3 5 5 4 4 4 1 6 1 4 1 4 1 5 4 6 5 6 6 4
    ## [53821] 6 4 6 6 6 6 6 6 4 6 4 4 3 5 5 5 5 5 3 3 1 2 4 2 2 2 2 2 1 2 2 2 2 2 2 5
    ## [53857] 1 2 2 5 3 3 1 4 5 5 1 1 1 1 7 5 2 2 2 2 2 2 2 2 2 2 5 2 2 2 2 2 4 2 2 2
    ## [53893] 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 5 5 5 4 3 3 6 6 4 1 6 4 5 5 6 6 5 6 2 5
    ## [53929] 1 5 1 5 5 2 3 5 5 5 5 5 5 5 5 5 5 5 6 4 4 4 6 6 3 3 3 1 5 1 5 3 3 5 5 1
    ## [53965] 4 4 5 3 5 6 4 6 4 6 3 1 5 4 1 4 5 4 3 6 4 3 4 4 1 5 5 5 5 5 2 5 5 5 4 5
    ## [54001] 5 5 5 3 5 5 5 5 3 3 4 2 3 5 4 4 3 1 4 3 1 4 3 1 3 3 2 3 5 3 1 5 3 3 4 1
    ## [54037] 5 6 6 5 6 4 6 3 3 3 2 3 3 4 4 4 5 3 1 3 4 6 3 3 1 7 4 5 1 4 4 3 6 4 4 4
    ## [54073] 3 4 5 4 5 5 1 3 3 1 4 1 5 3 3 6 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [54109] 5 5 5 5 5 5 5 5 5 5 5 3 5 4 5 4 1 3 3 4 4 5 5 3 6 2 4 4 4 4 3 3 4 1 1 6
    ## [54145] 3 6 6 6 4 1 6 6 1 3 4 3 3 4 8 4 6 4 4 3 3 3 5 5 5 3 1 6 4 4 6 4 4 2 4 4
    ## [54181] 6 6 1 4 6 4 6 6 4 4 6 6 6 6 6 3 2 6 6 6 5 1 4 3 5 1 5 6 4 6 4 5 1 3 3 5
    ## [54217] 6 4 4 6 5 2 3 3 4 5 1 4 2 2 5 2 5 4 2 5 2 5 5 5 5 3 5 5 5 5 5 5 5 4 3 4
    ## [54253] 6 4 6 4 6 6 5 6 4 6 4 4 4 4 6 3 5 2 6 1 5 4 4 3 6 3 4 4 3 4 2 2 2 3 2 2
    ## [54289] 3 2 2 4 3 3 6 1 1 1 1 3 4 6 4 4 4 6 4 4 5 5 3 1 1 4 3 6 2 4 6 4 2 5 6 5
    ## [54325] 5 5 5 5 5 5 5 5 5 5 5 5 3 3 4 6 4 4 2 6 3 2 2 2 2 4 2 2 2 2 2 2 2 6 2 2
    ## [54361] 2 6 2 2 1 1 4 3 5 6 6 6 3 4 6 4 4 4 6 4 4 1 4 3 3 3 3 6 1 5 5 4 5 5 3 5
    ## [54397] 4 3 5 4 4 2 4 2 5 2 4 4 3 4 4 4 4 4 4 4 4 4 5 5 3 4 3 1 5 3 4 2 4 4 5 1
    ## [54433] 1 3 3 3 2 5 1 1 1 5 3 1 5 4 4 1 6 5 6 3 1 5 5 4 3 1 6 3 3 5 3 3 3 1 3 5
    ## [54469] 6 6 3 6 3 5 3 4 5 6 4 6 5 5 3 5 4 5 4 2 4 5 5 2 4 3 3 2 2 1 3 1 5 5 6 4
    ## [54505] 4 4 6 3 6 4 6 4 2 3 1 4 3 2 5 4 1 4 4 3 6 4 6 5 3 1 5 3 4 4 4 4 4 4 4 3
    ## [54541] 4 6 3 3 1 7 4 1 1 1 1 2 7 2 4 5 2 3 3 5 6 4 4 5 4 6 3 2 3 4 4 3 5 6 4 4
    ## [54577] 4 6 6 4 6 6 6 4 6 4 1 6 6 6 3 3 7 5 4 4 3 3 4 5 5 3 4 6 1 6 4 3 6 1 3 3
    ## [54613] 5 3 4 3 5 5 4 6 4 4 6 6 6 4 3 3 4 6 4 6 5 6 3 3 6 2 1 6 3 6 4 4 4 5 5 5
    ## [54649] 2 6 3 4 4 3 6 6 6 3 4 1 3 5 5 5 5 5 6 5 5 2 3 5 5 3 4 1 4 5 4 1 2 2 2 6
    ## [54685] 5 2 1 5 2 5 2 3 3 3 1 6 3 4 3 6 4 6 4 6 4 4 4 3 4 4 4 4 4 6 4 7 4 5 5 4
    ## [54721] 3 3 1 3 1 5 5 1 5 5 5 5 5 1 4 1 1 6 4 2 2 2 3 2 2 2 2 6 2 2 2 2 2 2 2 2
    ## [54757] 2 4 2 5 2 2 2 2 5 2 2 1 2 2 2 2 2 4 2 1 2 1 5 6 5 4 3 3 3 2 2 2 2 3 6 3
    ## [54793] 6 6 3 4 6 1 3 3 5 2 5 6 4 4 3 6 4 6 6 4 6 6 4 4 6 6 4 6 4 5 1 5 4 5 5 5
    ## [54829] 5 5 5 4 5 5 5 2 5 5 2 1 3 3 5 4 4 3 5 5 4 5 4 1 6 7 4 5 5 5 7 4 2 1 5 2
    ## [54865] 5 2 5 5 5 5 6 2 4 5 5 5 1 2 1 5 2 2 2 5 3 1 1 1 1 2 1 6 5 5 4 3 4 3 6 4
    ## [54901] 4 4 4 5 6 6 6 6 4 4 6 6 6 6 6 3 6 1 6 3 3 2 6 3 5 2 3 1 3 2 1 6 1 2 3 1
    ## [54937] 3 2 2 6 2 2 5 2 1 5 5 5 2 2 2 3 4 3 5 4 2 2 2 5 2 5 5 5 5 2 3 5 5 2 2 2
    ## [54973] 1 4 4 3 3 3 3 4 3 6 1 1 5 5 5 6 6 1 4 6 3 4 3 3 2 2 3 3 4 4 4 3 4 4 6 4
    ## [55009] 3 6 1 1 4 1 1 4 5 5 5 5 6 5 6 3 4 3 3 3 1 3 3 4 4 4 4 5 1 3 3 1 3 3 3 3
    ## [55045] 6 6 6 1 4 1 5 3 4 3 3 6 1 3 4 1 5 3 4 3 4 6 6 2 4 3 4 3 4 2 3 4 4 3 4 3
    ## [55081] 4 2 6 4 4 4 3 4 4 3 3 1 3 3 3 1 3 3 4 3 2 3 5 3 5 6 4 4 6 3 5 5 3 3 4 5
    ## [55117] 4 4 4 6 3 6 5 3 5 6 3 3 1 4 4 1 2 2 2 2 4 2 2 2 2 4 4 3 2 4 4 4 3 4 5 5
    ## [55153] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 6
    ## [55189] 6 4 4 3 1 5 3 3 3 5 1 4 1 4 6 2 2 3 6 4 6 2 2 6 2 1 1 6 4 2 4 4 4 5 5 5
    ## [55225] 5 5 5 5 5 5 5 5 5 5 1 5 2 5 5 5 3 5 5 5 5 5 5 6 4 6 6 6 1 6 5 3 4 6 6 3
    ## [55261] 4 1 5 3 5 5 4 3 5 2 3 1 5 1 1 3 5 4 5 3 5 3 5 5 4 2 3 4 4 4 1 1 4 5 1 3
    ## [55297] 4 5 5 1 1 5 3 1 3 1 3 1 2 3 1 4 5 1 6 4 4 1 4 4 2 6 1 5 3 5 3 5 4 1 4 5
    ## [55333] 5 5 3 3 4 5 4 5 1 6 5 6 1 5 7 1 3 6 1 3 4 4 4 6 5 4 5 3 7 2 6 6 6 6 2 6
    ## [55369] 3 1 1 1 1 3 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 3 6
    ## [55405] 3 2 3 3 3 3 2 3 4 4 4 5 5 5 2 5 5 5 5 5 5 5 5 3 3 4 1 3 4 1 5 5 3 1 7 4
    ## [55441] 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 4 4 5 5 5 5 5 5 5 5
    ## [55477] 5 4 5 5 5 8 5 5 5 2 5 3 5 2 3 2 2 2 5 5 4 5 2 2 2 5 5 5 2 5 6 2 5 4 2 2
    ## [55513] 5 5 2 3 4 4 5 5 4 4 6 4 6 4 5 5 5 5 2 2 2 6 7 5 5 1 5 5 5 5 5 3 4 1 1 1
    ## [55549] 1 3 3 3 4 6 5 3 2 1 6 1 6 3 6 1 4 4 4 5 1 3 4 3 3 4 3 1 3 3 1 3 3 2 1 1
    ## [55585] 1 8 2 6 4 5 5 5 6 1 4 4 3 3 5 3 3 4 3 1 1 4 3 5 5 3 3 3 5 1 5 5 5 2 5 5
    ## [55621] 2 6 5 4 2 2 4 4 5 5 1 4 6 5 5 4 2 5 5 4 5 2 6 4 6 5 6 6 6 6 4 5 2 4 6 2
    ## [55657] 5 6 4 6 3 3 5 4 5 6 5 4 3 6 1 4 3 5 3 4 3 4 6 3 1 3 3 6 3 6 5 4 5 5 5 5
    ## [55693] 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6
    ## [55729] 3 6 6 4 5 5 3 1 6 6 4 3 1 3 4 6 4 6 4 4 6 6 6 3 3 5 5 4 3 3 1 5 5 4 5 5
    ## [55765] 4 3 4 5 6 3 4 4 7 4 1 6 6 4 4 6 6 4 3 4 4 1 6 3 2 4 6 6 4 4 4 4 6 3 3 4
    ## [55801] 4 4 4 4 6 6 4 4 4 3 4 1 6 4 6 4 6 4 6 4 6 3 4 4 4 6 3 4 4 5 5 1 6 3 6 2
    ## [55837] 3 3 4 5 5 4 5 5 1 4 5 4 1 3 5 3 5 5 3 2 4 4 1 1 1 3 4 5 5 1 5 5 5 3 3 3
    ## [55873] 4 6 1 4 2 3 6 3 5 5 3 5 3 1 4 6 1 2 5 2 2 7 2 2 5 2 2 5 5 4 5 4 4 1 1 4
    ## [55909] 4 4 6 4 6 3 1 5 4 6 4 4 5 6 4 6 6 3 3 3 5 3 6 6 4 6 4 4 4 4 3 6 6 6 3 4
    ## [55945] 3 6 3 1 4 1 5 1 4 5 4 8 6 3 1 3 4 6 2 4 1 5 3 3 5 1 3 1 2 3 5 3 5 5 5 5
    ## [55981] 5 5 5 3 3 5 3 6 4 6 6 4 1 6 4 4 4 3 5 6 6 3 3 3 2 5 2 5 2 5 4 3 5 2 1 5
    ## [56017] 5 2 5 5 5 5 4 2 5 1 5 5 4 5 5 5 1 5 4 4 5 1 5 1 1 2 8 6 2 2 2 3 3 5 3 2
    ## [56053] 1 1 2 3 5 2 5 3 5 5 1 3 4 6 3 6 3 6 4 6 6 4 6 6 6 4 6 1 3 5 3 3 4 3 4 3
    ## [56089] 2 3 6 4 1 4 3 5 4 1 5 5 5 3 4 3 5 5 4 3 2 1 5 2 3 3 5 1 3 6 6 4 1 6 5 6
    ## [56125] 6 3 4 3 3 3 3 3 2 5 6 2 3 4 5 5 4 2 1 1 3 5 5 1 1 3 3 3 1 5 3 4 2 3 3 3
    ## [56161] 1 3 4 5 4 4 4 4 5 4 4 4 4 4 4 4 4 3 4 1 4 4 1 4 5 4 4 5 4 4 6 6 3 4 6 4
    ## [56197] 4 6 4 6 3 4 4 4 3 4 4 4 2 4 2 6 4 4 6 4 6 6 6 5 5 1 6 5 5 6 4 4 4 4 6 5
    ## [56233] 2 3 3 6 4 6 6 4 6 2 5 2 6 6 6 6 6 6 2 4 3 3 3 1 2 5 5 3 5 1 5 5 5 5 5 3
    ## [56269] 4 4 4 1 4 4 3 2 2 2 2 2 2 2 5 2 2 2 5 2 2 4 5 5 2 2 5 1 1 2 2 2 2 4 4 4
    ## [56305] 4 3 4 4 4 3 5 5 5 5 4 4 5 1 6 6 4 2 2 2 2 2 5 2 5 2 2 2 2 3 6 3 2 5 2 2
    ## [56341] 2 2 2 2 2 5 2 4 1 1 3 6 5 1 2 1 6 2 6 1 8 3 2 5 4 1 4 2 2 3 4 2 5 4 2 5
    ## [56377] 4 5 5 3 5 5 3 8 4 4 3 1 5 1 1 1 3 3 1 3 1 5 1 5 5 3 6 4 2 6 3 5 2 6 2 5
    ## [56413] 3 6 6 5 5 4 5 5 5 3 6 4 1 4 1 3 1 3 4 1 1 3 3 5 1 6 3 3 3 2 4 3 4 3 4 1
    ## [56449] 2 3 3 2 4 3 4 7 3 3 3 3 4 4 5 6 4 5 6 5 5 5 7 5 1 5 5 1 1 5 1 1 1 2 2 5
    ## [56485] 4 1 5 3 4 3 4 1 6 6 4 5 5 6 3 1 3 2 2 2 4 2 2 2 2 2 2 2 2 2 3 2 1 4 4 4
    ## [56521] 4 6 4 2 2 4 6 2 4 4 4 2 5 6 6 6 6 6 6 5 6 6 6 5 6 5 5 6 6 6 4 5 3 6 5 2
    ## [56557] 3 4 3 1 2 1 1 5 1 1 5 4 3 5 5 5 5 5 5 5 5 5 5 5 4 5 2 5 5 5 5 5 5 5 4 5
    ## [56593] 5 5 5 2 5 5 5 5 5 3 6 1 5 1 3 5 5 5 5 5 3 4 3 1 5 3 1 4 6 7 5 3 3 1 4 1
    ## [56629] 5 1 5 1 5 1 5 5 2 5 3 1 3 5 6 2 4 6 2 3 3 4 6 4 4 6 3 3 6 6 3 2 5 2 2 5
    ## [56665] 5 5 5 2 5 1 2 5 5 3 5 1 5 5 5 4 5 5 4 6 4 3 5 5 1 5 1 5 5 5 5 5 5 5 5 5
    ## [56701] 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 4 5 1 1 1 4 4 4 6 3 6 3
    ## [56737] 3 3 3 6 2 6 6 6 6 6 6 6 4 5 4 6 4 6 3 1 5 4 5 4 4 4 4 4 6 6 6 4 6 3 4 4
    ## [56773] 1 1 1 5 1 6 6 5 2 1 3 3 3 2 6 3 4 3 4 4 4 4 4 6 4 4 6 6 6 5 6 2 2 5 2 4
    ## [56809] 4 2 6 2 6 6 4 1 2 6 2 6 4 3 3 5 4 1 5 6 3 2 5 3 4 4 3 1 5 1 6 6 5 3 4 6
    ## [56845] 6 6 2 5 3 1 5 6 4 4 3 5 4 2 4 4 4 6 6 3 2 6 4 4 4 3 3 1 4 3 3 4 3 3 4 4
    ## [56881] 3 4 4 4 3 4 4 3 4 4 2 4 5 4 5 3 3 4 6 1 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [56917] 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3 6 4 6 6 4 6 5 1 4 4 1 4 1 5 5 5 5 5 5 4 5
    ## [56953] 1 5 5 5 1 5 4 3 5 2 5 5 5 5 2 5 5 1 5 6 5 1 3 3 4 2 6 6 4 3 5 5 5 1 3 4
    ## [56989] 4 4 4 4 1 3 4 2 6 5 1 1 1 5 6 1 4 5 1 1 1 5 5 3 3 3 4 4 1 5 3 1 6 3 4 6
    ## [57025] 4 4 4 1 4 6 5 6 2 5 5 6 5 4 5 5 2 4 5 5 2 5 5 5 5 5 6 3 3 4 2 6 5 6 6 5
    ## [57061] 2 5 3 6 5 2 2 5 3 3 1 3 4 4 3 3 6 1 3 5 5 3 5 4 3 3 4 3 5 5 5 4 5 4 4 3
    ## [57097] 6 3 4 2 1 2 4 5 6 2 2 4 2 5 2 2 5 2 5 2 5 5 6 5 2 5 5 4 5 2 2 5 5 5 5 2
    ## [57133] 2 6 5 5 2 5 5 5 2 5 2 4 6 5 4 5 6 5 4 2 3 1 1 3 5 5 5 5 4 6 4 4 4 4 4 5
    ## [57169] 2 4 4 6 6 4 3 4 6 6 3 5 3 1 4 4 6 6 4 4 3 6 6 4 1 6 4 5 1 4 4 5 6 3 1 3
    ## [57205] 4 5 5 4 4 7 5 1 3 3 1 1 5 1 6 2 2 2 2 2 2 2 4 2 2 2 4 2 2 4 4 2 4 4 5 6
    ## [57241] 4 3 4 4 2 4 2 6 6 7 5 5 4 6 6 6 4 6 6 6 6 4 1 3 4 5 1 1 2 2 1 3 3 2 4 6
    ## [57277] 4 3 2 4 3 4 1 4 6 3 4 3 3 5 4 5 4 1 2 5 1 5 5 3 3 5 3 3 1 5 4 4 4 6 4 4
    ## [57313] 6 4 3 1 4 6 2 7 5 1 3 4 3 5 5 3 4 2 4 1 7 2 3 5 5 5 7 4 5 5 4 3 3 3 4 6
    ## [57349] 6 6 4 4 4 4 4 6 6 6 4 6 4 6 4 6 6 4 6 6 4 6 3 5 5 4 6 4 1 4 6 4 2 4 4 6
    ## [57385] 4 3 5 5 4 1 5 3 5 5 3 1 6 4 1 5 3 1 4 3 4 3 3 1 1 5 3 1 6 1 1 4 3 4 5 6
    ## [57421] 6 6 4 4 4 4 6 4 6 6 1 3 3 7 5 5 5 5 5 1 4 5 4 5 5 2 5 5 5 2 2 5 5 5 1 4
    ## [57457] 5 5 4 5 5 5 5 4 6 4 4 3 4 4 4 4 1 4 5 6 4 5 5 1 6 5 5 1 3 3 5 3 3 3 3 5
    ## [57493] 4 4 1 2 1 2 6 4 6 6 3 3 3 4 1 4 5 5 5 5 3 4 5 4 5 5 3 4 3 5 3 1 4 4 4 1
    ## [57529] 3 3 3 3 3 6 3 3 4 6 4 6 4 4 5 7 4 3 1 3 1 6 4 5 6 4 6 3 4 4 4 4 6 4 3 5
    ## [57565] 6 6 3 5 5 4 5 5 5 5 4 6 5 3 5 3 5 5 4 5 1 5 4 4 5 6 1 6 6 4 1 3 5 5 4 5
    ## [57601] 1 2 2 4 3 4 4 2 3 4 7 5 4 5 3 6 4 4 6 2 4 4 3 2 6 4 6 6 4 4 3 6 4 6 6 2
    ## [57637] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [57673] 2 3 2 2 2 2 2 2 2 2 2 2 2 2 4 2 3 2 3 6 5 4 4 3 4 4 6 4 4 6 3 4 4 4 4 3
    ## [57709] 3 5 3 4 1 1 1 5 5 3 5 5 1 3 3 3 4 2 1 2 1 5 5 5 5 5 5 5 2 5 5 5 1 2 3 6
    ## [57745] 2 4 3 4 6 6 4 4 6 4 4 6 3 5 6 4 6 4 6 6 6 6 6 6 4 4 4 3 3 4 1 1 4 1 1 6
    ## [57781] 4 3 4 3 1 7 1 1 4 4 6 6 6 3 4 4 4 4 4 5 4 4 7 5 1 3 6 1 6 3 3 3 4 4 4 3
    ## [57817] 4 4 1 1 6 1 1 2 1 5 1 3 5 5 3 4 5 5 8 4 5 3 3 2 6 3 3 4 6 6 4 4 4 7 5 3
    ## [57853] 5 2 3 3 5 4 2 5 5 4 5 6 3 2 4 5 6 3 5 5 5 5 3 1 3 5 3 3 3 6 4 6 6 4 6 3
    ## [57889] 3 3 4 5 1 2 4 5 3 5 3 6 5 1 5 5 3 5 3 5 5 3 3 3 3 3 5 1 5 4 2 3 3 3 4 3
    ## [57925] 6 6 4 4 6 4 4 3 4 4 3 4 1 3 5 5 5 5 5 5 2 6 6 6 4 5 4 5 3 3 4 5 6 4 5 4
    ## [57961] 5 5 6 5 5 5 5 4 4 4 6 3 6 6 1 4 4 6 4 6 3 6 4 4 4 3 4 4 4 4 4 6 4 6 4 4
    ## [57997] 4 1 3 4 6 1 1 3 4 5 3 5 5 1 3 6 5 5 5 7 6 2 5 4 5 1 4 4 1 5 3 5 5 3 5 3
    ## [58033] 5 5 1 1 4 5 5 4 6 5 5 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [58069] 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 3 2 2 5 6 5 2 3 2 2 2 3 5 5 5 5 2 2
    ## [58105] 3 6 5 6 6 1 1 2 2 2 2 2 2 5 4 2 4 2 2 2 1 2 2 2 2 4 2 2 4 5 2 2 2 6 2 1
    ## [58141] 5 2 2 2 2 1 5 1 2 3 3 4 4 5 4 2 6 3 2 6 1 5 5 3 1 1 1 5 3 1 4 4 6 3 4 3
    ## [58177] 5 5 4 5 5 5 7 5 6 4 5 6 4 4 4 4 3 4 6 4 6 4 4 3 4 5 1 6 3 6 6 1 3 2 2 5
    ## [58213] 5 5 5 5 5 1 5 5 5 4 5 5 2 4 5 4 8 8 3 1 6 6 1 4 2 4 4 4 4 1 4 3 3 3 3 3
    ## [58249] 6 6 5 3 4 5 6 2 6 3 3 1 6 4 1 3 3 4 1 4 1 3 3 4 1 7 5 5 1 5 3 6 5 4 5 4
    ## [58285] 4 2 5 5 2 5 5 5 5 5 5 5 5 5 5 5 1 1 3 7 4 3 1 5 6 3 6 4 6 6 2 4 4 3 4 6
    ## [58321] 2 2 2 1 1 2 6 3 6 5 3 6 3 3 4 3 1 3 1 5 5 5 5 5 5 3 4 4 4 3 6 5 4 6 4 4
    ## [58357] 6 6 6 6 3 5 1 2 4 3 1 4 3 3 3 3 5 5 5 3 1 5 1 5 6 5 6 5 3 6 4 6 4 6 6 6
    ## [58393] 6 5 4 5 5 5 5 3 5 6 4 1 5 5 3 5 5 3 2 1 4 5 5 5 5 5 5 5 5 5 5 5 4 5 2 4
    ## [58429] 4 3 6 4 4 4 4 3 4 4 1 7 1 3 5 5 5 5 5 5 5 5 5 5 5 6 5 6 5 5 5 5 5 5 5 5
    ## [58465] 4 5 6 4 5 5 5 6 6 6 5 4 6 5 1 5 2 1 3 6 6 2 4 5 7 3 6 1 4 4 1 4 4 4 4 4
    ## [58501] 4 3 6 1 3 4 5 4 3 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 2 2 4 2 2 2 2
    ## [58537] 2 2 2 1 4 5 3 3 1 3 5 1 2 2 5 5 5 4 2 2 5 5 2 2 5 6 5 5 4 2 2 1 2 2 4 1
    ## [58573] 4 1 4 6 4 6 1 1 3 6 4 6 1 6 6 6 5 6 4 6 4 4 6 6 6 2 4 4 4 4 4 4 6 6 1 5
    ## [58609] 5 4 5 4 4 1 5 5 5 3 3 5 1 3 1 4 5 5 5 5 6 6 4 5 5 5 5 3 2 3 5 3 3 5 4 5
    ## [58645] 5 4 4 5 3 6 4 4 6 3 4 3 1 4 6 4 6 5 2 6 4 6 5 5 6 5 2 5 4 6 5 6 4 4 5 4
    ## [58681] 6 6 6 4 4 5 6 3 4 5 3 5 4 5 5 5 5 5 1 4 5 1 5 5 3 6 6 2 1 4 5 4 5 2 6 5
    ## [58717] 4 3 6 3 4 6 1 4 6 3 5 5 5 2 3 1 3 4 3 3 4 4 3 5 4 3 5 5 2 4 1 2 1 5 4 3
    ## [58753] 2 7 4 3 5 4 3 3 5 1 5 4 3 3 5 7 2 5 4 1 3 3 3 5 5 5 5 5 1 5 5 5 4 7 2 3
    ## [58789] 1 5 5 5 5 5 5 5 3 5 4 5 5 5 5 5 3 4 4 2 2 2 2 4 4 2 2 2 4 2 4 2 2 2 2 2
    ## [58825] 2 2 2 3 3 4 2 2 2 2 2 2 2 4 4 4 4 2 2 2 2 2 4 2 4 2 2 2 2 2 2 2 2 2 2 2
    ## [58861] 2 2 2 2 2 4 4 4 2 4 1 4 2 2 2 2 2 4 4 1 2 2 4 4 1 2 2 2 2 4 4 2 3 6 2 4
    ## [58897] 6 4 4 4 6 3 6 4 6 5 4 1 5 5 1 1 4 6 4 6 6 2 6 6 3 1 3 5 2 2 4 6 4 4 1 4
    ## [58933] 4 4 4 4 4 5 3 3 3 3 3 4 5 4 6 4 5 4 4 1 4 4 6 6 3 3 6 5 5 6 5 5 5 5 5 5
    ## [58969] 5 5 5 5 4 5 5 3 5 5 5 6 4 5 5 5 5 2 5 5 5 5 5 5 5 5 4 6 5 5 5 5 5 5 5 5
    ## [59005] 5 5 4 5 5 5 5 5 5 5 4 5 4 7 2 5 3 4 4 6 4 4 4 6 4 3 5 4 2 3 2 6 3 3 4 4
    ## [59041] 4 4 4 4 1 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [59077] 5 5 5 5 5 3 5 5 5 5 2 5 5 5 5 3 3 3 4 6 4 3 6 4 6 4 4 6 4 4 6 4 6 2 2 4
    ## [59113] 2 2 3 4 2 2 6 6 2 6 3 4 6 2 6 5 1 4 1 1 4 4 1 3 3 4 3 4 6 2 4 4 6 5 3 6
    ## [59149] 2 3 5 1 1 3 3 1 3 5 1 4 6 6 6 6 6 4 4 6 6 6 6 2 6 6 4 4 6 6 6 6 2 4 4 6
    ## [59185] 6 8 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [59221] 5 5 5 5 4 5 5 5 5 5 5 5 5 1 5 4 1 6 3 3 1 4 3 4 4 3 3 6 2 2 5 4 7 3 1 3
    ## [59257] 5 5 5 3 4 4 5 6 5 5 5 1 4 5 4 6 2 3 5 3 2 5 4 4 3 6 5 5 3 4 4 1 3 5 5 4
    ## [59293] 4 5 5 5 5 5 4 3 4 6 1 6 4 6 4 5 5 5 3 5 5 5 5 5 5 5 3 5 5 2 5 1 1 3 5 5
    ## [59329] 5 3 3 4 6 2 3 6 4 6 6 6 4 2 4 3 6 6 2 6 6 3 4 6 6 4 2 3 5 3 2 3 6 6 1 4
    ## [59365] 4 3 5 4 3 3 3 4 1 3 5 3 2 2 3 1 5 3 5 5 3 5 5 6 6 6 6 4 6 5 1 4 4 2 5 4
    ## [59401] 6 4 3 5 2 1 7 4 4 5 4 6 3 3 3 3 6 6 2 4 4 4 1 6 6 1 6 4 6 6 2 1 4 2 5 1
    ## [59437] 1 5 8 1 3 1 5 1 3 3 5 6 4 6 1 6 5 1 6 6 5 6 4 5 5 5 6 4 6 5 4 1 1 6 3 7
    ## [59473] 4 6 4 4 3 4 4 4 4 4 6 6 4 3 1 6 5 4 4 1 1 3 1 1 1 5 1 4 4 5 5 5 5 5 5 5
    ## [59509] 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 1 6 4 3 3 4 1 4 3 2 3 4 5 2 5 1 5 4 2 5 5
    ## [59545] 1 3 4 4 3 5 2 2 2 5 6 2 4 2 2 3 3 3 3 6 3 3 4 4 2 6 3 4 3 2 6 6 6 4 4 4
    ## [59581] 2 3 2 4 4 4 3 2 2 4 4 3 1 1 5 5 6 6 3 6 2 2 6 1 1 1 4 2 1 1 3 5 5 4 5 5
    ## [59617] 3 5 5 5 5 3 5 5 5 5 4 5 3 4 5 4 3 5 2 2 5 1 6 6 6 6 1 3 1 5 3 1 4 3 1 4
    ## [59653] 4 3 1 3 4 5 1 1 2 5 5 2 5 4 2 5 5 4 3 4 3 2 3 3 6 4 4 6 5 1 5 4 3 4 4 4
    ## [59689] 4 6 4 6 6 3 5 4 5 5 4 5 6 5 3 2 2 2 2 2 2 1 3 4 2 2 1 2 2 5 2 2 1 2 2 1
    ## [59725] 2 3 3 6 3 4 3 5 5 5 1 5 4 3 6 5 3 6 5 3 4 5 5 5 5 5 3 5 4 4 2 2 5 2 1 4
    ## [59761] 2 5 2 5 5 5 5 5 2 5 5 6 1 3 5 5 5 1 5 5 4 6 6 6 5 3 5 3 2 6 4 6 3 4 5 3
    ## [59797] 4 6 4 4 2 1 6 1 1 4 1 1 2 4 6 2 6 4 6 3 4 4 6 4 4 4 5 4 6 6 4 3 2 4 5 4
    ## [59833] 3 4 6 4 4 4 3 4 6 4 4 4 7 6 3 5 1 2 7 6 3 3 3 5 3 5 5 5 3 2 4 4 5 3 5 6
    ## [59869] 3 5 5 4 5 5 4 5 6 5 5 2 5 5 5 2 5 5 2 5 5 5 5 5 5 5 3 5 4 5 5 2 2 2 2 2
    ## [59905] 2 2 3 2 2 2 2 2 1 2 2 4 2 2 2 4 6 5 4 2 2 6 4 4 2 1 3 1 4 4 2 3 3 5 3 3
    ## [59941] 3 5 5 1 5 5 4 5 5 3 4 2 4 5 3 5 4 6 6 6 6 6 6 4 2 5 5 6 6 6 6 6 6 6 3 1
    ## [59977] 3 6 6 4 6 4 4 4 5 5 6 1 4 5 1 5 5 5 1 5 5 7 3 5 2 6 3 3 4 3 5 4 6 5 4 5
    ## [60013] 5 4 4 7 4 4 3 1 1 5 6 3 1 5 5 5 2 2 2 3 2 5 6 2 5 2 2 2 5 2 2 2 5 3 2 2
    ## [60049] 6 5 6 2 1 1 5 3 4 7 4 1 5 1 5 4 4 3 1 3 3 3 5 2 6 4 2 2 4 5 4 6 5 3 3 5
    ## [60085] 4 4 6 5 6 4 6 6 5 5 5 4 4 4 1 3 3 4 1 5 4 3 5 6 5 5 2 5 3 1 5 3 4 3 3 3
    ## [60121] 1 6 3 1 4 3 1 1 1 4 6 6 5 2 1 5 3 6 3 3 5 5 2 6 6 6 4 4 5 5 5 5 5 5 5 5
    ## [60157] 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 2 6 6 2 5 6 5 6 3 3
    ## [60193] 5 5 3 3 3 3 1 4 5 5 5 3 5 3 5 5 1 5 2 5 5 5 5 5 1 5 5 5 5 2 5 5 5 5 5 5
    ## [60229] 5 3 5 5 5 5 4 1 4 5 3 5 5 2 4 1 6 2 5 4 6 4 6 5 2 5 2 5 4 5 4 5 2 4 2 4
    ## [60265] 3 5 3 5 6 5 5 1 6 1 5 5 4 5 5 3 4 5 5 5 6 4 4 4 6 3 6 6 4 6 6 1 3 1 4 6
    ## [60301] 4 5 4 3 6 3 6 6 2 4 4 5 5 4 4 4 6 6 1 6 4 4 5 5 3 6 5 6 2 5 5 3 5 5 4 5
    ## [60337] 5 5 5 2 5 5 5 3 6 6 4 4 1 3 5 5 1 1 6 4 3 4 3 1 6 6 2 5 3 2 4 3 3 8 4 1
    ## [60373] 4 4 5 1 5 3 5 5 5 5 5 5 1 1 3 4 3 6 3 6 3 4 4 6 2 2 2 4 4 5 3 2 2 5 4 2
    ## [60409] 1 2 1 5 5 1 5 7 6 5 5 3 5 5 6 3 4 3 4 6 6 3 6 4 6 4 6 4 2 6 3 6 2 6 2 6
    ## [60445] 4 5 5 4 6 6 5 5 5 5 5 5 3 3 4 4 5 6 5 4 3 3 5 5 5 3 5 4 5 5 5 6 6 6 5 4
    ## [60481] 6 4 6 3 6 6 4 5 1 5 8 5 4 2 4 1 2 4 5 2 5 4 2 2 2 2 2 2 4 4 3 2 4 2 3 5
    ## [60517] 1 4 6 4 2 5 2 4 3 4 3 3 6 6 4 6 4 6 1 5 5 5 5 3 4 4 3 4 3 4 6 6 5 1 5 3
    ## [60553] 5 4 5 4 3 6 6 1 6 1 1 3 2 5 2 2 2 4 2 3 2 2 2 1 4 6 3 4 4 6 1 1 6 1 2 4
    ## [60589] 2 2 5 3 3 5 6 3 3 5 3 5 5 3 4 5 4 3 5 5 2 5 8 4 1 2 1 5 7 6 3 5 2 5 3 4
    ## [60625] 6 1 5 1 4 5 5 5 5 5 5 5 5 6 3 5 2 5 3 5 3 5 4 1 3 3 1 7 3 3 3 2 1 3 5 5
    ## [60661] 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 3 4 6 3 6 4 5 5 4 7 5 5 4 5 5
    ## [60697] 3 4 5 6 5 5 3 5 3 6 1 3 4 1 4 4 4 5 1 4 1 4 5 6 1 5 5 5 5 5 5 5 3 5 5 5
    ## [60733] 5 5 1 4 6 2 2 2 5 2 2 2 2 4 4 2 2 6 6 5 4 3 5 4 2 2 4 5 4 2 5 5 2 5 2 5
    ## [60769] 2 2 2 2 5 2 5 1 2 2 2 2 2 2 5 2 5 5 4 2 2 3 2 2 2 1 2 3 5 2 2 4 2 6 2 2
    ## [60805] 2 2 6 2 2 6 1 6 4 6 3 4 5 6 4 5 4 4 3 4 4 4 3 3 3 5 3 2 6 2 4 4 4 6 3 6
    ## [60841] 6 5 4 4 6 4 1 1 3 4 5 1 1 4 2 1 5 5 2 6 4 5 3 3 4 6 5 2 4 5 4 5 5 5 5 5
    ## [60877] 5 1 6 6 1 5 3 3 4 7 4 4 8 5 5 2 5 5 5 5 5 5 3 5 5 2 1 1 4 1 1 5 5 5 5 3
    ## [60913] 3 5 4 5 3 3 6 4 7 1 5 2 6 5 2 2 2 2 6 2 2 6 4 6 6 2 2 3 1 4 4 4 3 4 4 4
    ## [60949] 5 4 4 2 3 6 4 4 1 3 4 2 4 5 5 5 2 5 1 5 5 4 5 5 5 5 5 5 5 5 5 5 6 6 4 4
    ## [60985] 4 1 3 3 3 4 6 5 6 4 5 4 3 6 2 5 4 1 3 6 5 1 4 2 1 6 5 3 1 5 5 4 1 1 1 5
    ## [61021] 5 2 3 2 6 2 2 2 2 6 2 2 6 2 2 2 6 4 2 4 2 2 4 2 2 3 3 4 4 4 6 3 4 4 3 4
    ## [61057] 4 4 6 4 4 5 4 4 6 5 5 5 5 5 5 6 5 5 3 3 4 3 3 3 3 6 3 5 5 5 5 5 3 5 5 2
    ## [61093] 6 4 5 3 3 1 3 6 6 3 6 6 4 4 1 3 1 3 6 2 5 5 4 2 3 2 2 2 1 4 4 1 1 4 5 6
    ## [61129] 5 1 4 5 5 1 5 4 2 1 2 2 2 4 2 5 2 4 4 2 2 2 2 3 2 4 4 4 4 4 6 4 4 6 4 6
    ## [61165] 6 5 5 3 6 1 6 6 6 6 2 3 3 1 1 3 5 4 1 1 1 2 1 3 4 5 4 5 5 5 5 4 5 4 5 1
    ## [61201] 1 5 2 4 6 2 2 4 2 2 2 5 1 4 5 2 1 2 2 2 2 2 6 2 6 2 2 2 4 2 2 2 2 4 2 1
    ## [61237] 4 6 4 2 1 2 2 3 2 2 2 2 2 4 4 2 5 2 2 2 2 4 4 2 6 2 2 2 2 1 5 5 5 5 5 5
    ## [61273] 5 5 2 5 5 5 5 4 5 1 4 3 5 5 5 1 4 4 4 5 3 3 6 6 4 4 2 5 3 4 6 6 2 4 4 4
    ## [61309] 6 4 6 1 3 4 5 4 3 3 5 3 4 5 5 6 4 1 4 4 3 6 6 6 4 3 4 3 4 6 2 3 4 3 4 4
    ## [61345] 6 4 4 4 4 6 4 4 4 6 6 6 6 4 4 6 4 6 6 6 4 4 6 4 4 6 6 4 4 5 4 4 1 5 4 3
    ## [61381] 4 6 4 2 4 5 5 4 5 5 5 5 5 2 6 5 5 5 5 5 5 4 5 3 5 4 5 3 5 5 1 3 2 3 2 1
    ## [61417] 2 2 2 2 5 1 3 1 1 3 1 4 3 3 1 3 5 4 5 5 3 3 4 4 6 6 5 1 6 5 5 4 5 2 7 6
    ## [61453] 6 1 8 4 3 6 6 1 6 3 5 2 4 6 4 1 4 4 6 3 6 4 4 5 6 4 5 3 5 5 3 5 3 4 5 5
    ## [61489] 5 5 2 4 5 6 6 1 5 6 4 4 6 4 1 1 3 3 5 5 5 5 4 5 5 5 3 5 5 5 1 3 4 6 3 4
    ## [61525] 6 3 5 4 6 6 6 6 4 4 4 3 4 4 6 3 6 6 4 4 1 4 3 4 6 6 6 4 6 6 6 4 6 6 5 3
    ## [61561] 4 4 4 3 4 6 4 6 1 6 4 3 6 4 4 4 4 4 6 4 3 4 3 5 4 3 5 4 4 6 4 4 4 6 6 6
    ## [61597] 4 4 5 3 3 4 6 3 6 4 6 4 3 4 4 2 1 3 2 5 6 3 5 5 5 2 2 4 4 3 5 5 6 4 4 6
    ## [61633] 6 2 4 4 2 2 4 6 2 5 4 5 4 3 5 4 5 6 2 2 2 2 2 2 2 1 6 6 6 2 2 1 2 2 4 2
    ## [61669] 2 2 2 2 6 2 1 5 5 3 5 4 3 4 3 3 2 2 6 1 3 6 6 6 4 2 6 4 6 5 5 6 4 5 5 6
    ## [61705] 5 2 4 5 1 5 4 4 6 4 4 4 4 6 6 2 6 6 6 4 3 4 6 4 4 6 4 4 2 6 6 2 6 6 6 5
    ## [61741] 6 6 6 5 4 5 4 6 4 5 6 3 4 3 4 6 1 4 2 2 1 2 2 2 2 5 2 5 5 3 1 4 4 5 5 5
    ## [61777] 5 5 2 3 5 6 5 5 3 4 5 1 4 5 5 3 5 5 5 5 5 5 5 5 5 6 5 5 4 7 6 3 3 2 5 3
    ## [61813] 1 4 2 4 6 1 4 4 1 1 1 5 1 1 7 2 7 4 3 5 6 4 5 6 4 6 5 5 5 5 5 5 5 3 5 1
    ## [61849] 3 3 3 3 1 4 4 7 2 2 5 3 3 4 1 5 1 2 5 5 6 5 5 6 5 5 3 4 5 1 1 6 5 3 6 5
    ## [61885] 5 5 3 3 2 2 4 2 2 2 4 2 2 2 4 6 3 2 5 1 5 1 6 6 1 5 4 2 1 5 4 4 5 2 4 3
    ## [61921] 4 2 6 1 6 1 1 1 1 2 4 3 2 4 4 3 6 3 6 3 4 3 3 3 3 5 5 5 1 3 5 4 1 2 2 3
    ## [61957] 3 4 5 2 4 5 1 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5
    ## [61993] 2 2 2 4 2 2 6 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 1
    ## [62029] 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 4 2 2 2 2 2 2 6 2 2 2 2 6 2 2 6 2 2 2 2 2
    ## [62065] 4 2 6 2 2 6 2 2 2 4 6 5 1 1 5 5 5 5 5 5 5 5 5 5 5 5 2 4 4 2 2 4 3 7 4 6
    ## [62101] 4 6 6 6 6 3 4 3 6 6 6 4 4 6 6 6 6 5 7 5 4 4 3 6 3 4 6 4 3 3 4 4 6 6 4 6
    ## [62137] 6 3 4 4 1 4 6 4 6 4 5 6 6 4 6 6 6 5 6 6 6 4 6 4 6 6 6 6 6 5 5 5 5 5 5 3
    ## [62173] 5 5 1 4 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2
    ## [62209] 2 2 2 2 2 2 4 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 6 2 2 2 2 2 2
    ## [62245] 2 2 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2
    ## [62281] 2 2 2 2 2 2 2 5 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 3 3 4 3 5 4 1 4 1 4 4 1 1
    ## [62317] 2 3 4 1 1 4 1 3 5 4 2 3 3 4 5 2 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [62353] 5 5 5 5 5 4 5 5 5 5 5 5 5 4 6 6 4 1 3 3 3 4 5 5 5 4 5 2 4 6 6 5 5 5 5 5
    ## [62389] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 4 5 5 5 5 5
    ## [62425] 5 5 5 5 1 5 5 5 5 5 1 5 5 5 5 5 5 5 6 4 4 5 5 5 1 3 5 5 1 3 1 5 1 1 3 1
    ## [62461] 5 6 3 4 5 5 4 4 4 5 5 5 4 5 6 3 1 5 1 4 5 5 3 4 3 2 4 4 4 6 6 6 6 6 5 3
    ## [62497] 5 2 6 6 4 4 6 2 2 6 4 6 6 4 6 4 6 6 6 6 6 6 4 6 2 2 1 6 3 2 4 2 5 5 5 5
    ## [62533] 3 5 6 5 5 5 2 5 2 4 5 6 7 5 3 4 4 5 1 5 4 4 1 2 4 4 5 5 2 4 3 5 2 5 2 4
    ## [62569] 2 5 1 5 5 5 5 5 5 5 5 5 1 3 3 3 6 3 3 4 6 6 5 4 6 6 3 5 3 6 1 1 6 5 4 1
    ## [62605] 3 7 6 4 4 2 1 1 3 3 4 6 4 6 4 5 3 6 2 2 2 6 4 5 6 4 1 6 6 4 4 6 3 4 6 4
    ## [62641] 6 3 3 4 4 3 4 4 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [62677] 5 5 2 5 5 5 1 5 1 1 5 3 5 2 3 5 5 5 5 2 2 5 3 2 5 5 5 5 3 3 1 1 6 6 4 6
    ## [62713] 6 4 4 4 4 4 5 4 6 6 4 3 4 6 3 6 6 1 1 1 3 4 5 6 1 4 6 2 4 3 4 5 2 5 6 3
    ## [62749] 2 2 2 6 4 4 2 4 4 4 4 3 4 1 2 2 2 3 7 5 2 5 1 4 3 4 5 2 2 2 2 2 2 2 2 2
    ## [62785] 3 2 2 2 2 2 2 2 2 2 6 4 2 6 2 3 4 3 3 4 5 6 4 3 3 3 3 3 1 4 2 5 5 5 5 1
    ## [62821] 2 5 1 6 1 6 6 4 6 4 5 2 5 2 7 5 3 3 4 4 4 4 6 4 4 1 3 2 2 2 2 2 1 2 6 2
    ## [62857] 2 1 3 2 2 2 2 2 2 8 8 3 1 5 5 5 5 5 5 5 5 5 5 5 4 5 2 5 5 5 5 5 5 5 5 5
    ## [62893] 5 5 5 3 5 5 5 5 5 5 3 4 5 4 1 3 1 4 4 3 3 3 2 4 6 3 6 7 1 3 3 3 1 5 6 4
    ## [62929] 6 1 4 4 5 4 5 4 6 6 6 1 5 4 4 6 2 3 3 3 4 3 4 5 3 3 3 3 3 1 1 5 1 2 6 6
    ## [62965] 4 4 4 4 3 4 6 4 6 6 1 1 6 4 2 5 4 2 6 5 4 3 4 4 3 4 4 4 4 5 4 4 4 4 4 4
    ## [63001] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 4 4 4 4 4 3 4 4 4 4 4 4 3 5 3 3 6 3
    ## [63037] 5 5 5 5 1 2 4 2 2 2 2 2 1 2 2 2 2 2 2 2 2 4 2 1 2 2 2 6 2 2 2 2 1 2 2 2
    ## [63073] 2 3 3 6 3 4 3 4 5 4 4 3 4 6 6 1 3 3 6 7 4 5 4 5 5 4 5 4 3 5 4 5 5 5 5 5
    ## [63109] 5 5 5 5 5 5 4 3 5 5 5 5 5 5 5 5 4 3 4 6 3 4 6 4 4 3 6 7 5 4 1 4 4 5 1 2
    ## [63145] 5 5 5 5 5 5 3 1 1 3 5 3 3 5 5 3 3 3 5 5 5 5 5 5 5 5 4 5 5 5 5 5 1 5 5 5
    ## [63181] 1 4 5 4 1 3 5 4 1 6 1 5 5 1 5 5 5 5 3 3 3 4 6 4 6 5 3 3 1 5 5 2 4 3 5 3
    ## [63217] 5 6 6 6 6 5 6 6 1 6 5 4 6 6 4 3 1 6 6 3 1 6 6 5 4 5 5 5 4 5 5 5 5 5 2 4
    ## [63253] 5 1 4 4 6 4 2 4 2 4 6 5 4 2 5 1 3 2 5 6 4 4 6 6 4 2 4 5 6 4 3 6 3 4 5 3
    ## [63289] 1 5 4 5 5 3 4 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 3 3 4 3 5 2 5 5 3 4 7 3 4 3
    ## [63325] 5 6 4 5 3 6 4 5 3 5 5 6 3 5 5 5 5 5 5 1 3 5 2 3 3 6 6 6 6 6 6 5 4 3 4 2
    ## [63361] 4 6 5 4 4 3 4 5 4 3 3 3 6 6 5 1 5 4 5 5 7 2 5 6 4 3 5 2 4 6 5 5 4 4 6 3
    ## [63397] 3 5 5 5 4 1 5 5 3 3 3 4 3 5 5 5 1 5 3 1 4 4 3 1 2 2 2 2 2 4 2 2 2 2 2 1
    ## [63433] 2 4 2 1 6 2 3 6 2 2 2 2 4 6 4 4 6 6 6 6 6 4 4 6 6 5 4 4 2 2 6 4 4 6 4 6
    ## [63469] 2 6 4 2 4 4 7 3 3 3 3 4 6 4 3 3 3 1 5 2 6 2 1 2 6 2 2 2 3 6 6 2 1 2 2 2
    ## [63505] 5 6 5 5 4 5 5 6 5 5 5 5 5 6 4 3 6 4 4 4 4 1 4 1 4 6 4 6 4 1 2 4 4 4 4 4
    ## [63541] 4 4 6 1 4 4 3 6 1 6 4 1 6 6 6 4 6 3 4 3 1 3 3 5 2 3 2 2 4 5 2 2 5 5 5 6
    ## [63577] 5 3 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 1 1 1 1
    ## [63613] 1 2 1 1 1 6 3 4 3 5 6 5 6 7 3 5 4 3 4 6 5 4 5 5 3 4 4 3 4 6 1 3 4 4 6 5
    ## [63649] 1 5 3 5 5 5 5 5 6 4 6 3 4 3 4 1 7 6 6 1 1 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [63685] 5 5 4 5 5 5 5 5 5 5 4 4 1 5 3 5 4 5 4 3 5 5 4 6 5 4 6 5 1 1 3 4 4 4 3 4
    ## [63721] 6 4 4 4 4 4 4 4 4 4 4 4 6 4 6 3 4 4 4 6 4 6 3 4 2 2 2 6 4 2 2 2 2 3 2 2
    ## [63757] 2 2 2 2 2 3 3 3 6 4 2 4 3 6 5 5 4 6 5 5 1 6 5 6 5 4 5 6 4 4 6 6 4 6 6 6
    ## [63793] 4 6 4 5 4 4 6 6 4 6 6 6 4 1 4 1 4 4 3 5 3 4 1 1 1 1 6 1 4 3 1 1 2 5 5 3
    ## [63829] 4 2 3 3 1 1 4 5 4 5 5 5 5 1 4 1 4 3 3 6 5 5 3 3 1 3 4 6 6 6 4 6 3 5 6 6
    ## [63865] 4 6 3 4 3 6 5 4 3 1 3 5 3 4 3 6 6 6 2 5 5 5 5 1 2 3 6 3 3 5 3 4 6 3 3 4
    ## [63901] 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 1 3 3 1 5 4 5 3 5 6 3 5
    ## [63937] 5 5 5 3 4 6 5 5 3 4 5 4 4 4 4 5 5 4 3 5 4 3 4 1 3 6 2 5 1 5 1 5 2 2 7 5
    ## [63973] 5 5 4 3 5 5 6 4 6 6 2 2 5 4 2 4 5 4 3 4 4 2 4 6 5 4 5 4 4 5 4 4 6 5 4 6
    ## [64009] 4 3 6 3 5 3 4 5 3 5 5 5 5 5 5 5 5 6 5 5 5 5 4 5 5 5 5 5 3 3 4 6 4 6 6 5
    ## [64045] 2 2 4 3 1 3 5 4 1 4 1 6 5 5 6 5 8 5 4 5 6 5 1 5 5 6 3 5 6 5 5 1 3 2 2 3
    ## [64081] 3 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 3 1 1 3 6 5 5 5 4 5 5 5 5
    ## [64117] 3 5 5 5 5 5 5 5 5 5 7 4 4 5 5 5 5 6 3 3 6 6 6 4 6 6 4 6 4 4 4 3 6 6 6 4
    ## [64153] 4 6 4 1 4 2 6 4 6 4 3 4 6 5 5 5 5 5 5 4 5 5 5 3 5 5 5 5 5 1 7 5 5 5 5 5
    ## [64189] 4 6 1 1 6 6 5 4 4 4 5 6 4 5 5 2 2 3 4 6 6 4 4 4 4 6 3 4 2 4 4 5 3 5 4 4
    ## [64225] 4 4 4 4 4 4 5 5 5 5 5 5 3 5 4 1 5 5 5 4 5 4 6 6 1 3 4 6 6 6 6 4 6 6 5 6
    ## [64261] 6 4 6 3 6 6 2 5 3 3 5 1 3 2 5 6 6 4 5 6 6 6 6 2 5 5 5 5 5 1 4 6 5 5 3 5
    ## [64297] 3 5 5 4 4 5 1 5 8 5 5 1 6 5 4 5 5 4 5 1 3 4 4 6 6 3 6 4 4 6 6 4 5 3 5 4
    ## [64333] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [64369] 5 5 5 5 5 5 5 5 5 5 5 5 2 6 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 4 2 5 4 3 2 6
    ## [64405] 6 6 6 6 6 4 4 6 4 1 1 3 4 5 4 1 3 1 3 4 5 4 6 4 6 4 1 6 6 1 6 5 4 1 6 5
    ## [64441] 4 3 5 5 3 1 4 3 5 2 2 4 3 6 4 4 5 5 1 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5
    ## [64477] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3 5 5 6 6
    ## [64513] 1 4 6 6 6 4 1 4 6 6 3 5 5 5 5 3 5 2 2 2 2 2 2 5 4 6 5 4 5 7 3 4 4 4 3 3
    ## [64549] 3 4 4 2 4 6 1 4 1 2 2 2 2 2 2 3 4 6 2 6 2 4 4 4 4 5 4 4 4 5 5 5 1 3 6 5
    ## [64585] 5 5 5 5 5 5 5 5 5 5 5 4 3 5 5 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 4 4 2 5 3 2
    ## [64621] 2 2 5 6 4 2 6 1 6 2 1 1 3 3 4 6 4 4 6 4 4 4 6 1 5 5 1 3 3 3 3 3 3 4 4 4
    ## [64657] 5 4 5 4 6 4 6 6 3 6 5 5 5 5 5 5 2 6 5 5 5 5 5 5 5 5 3 5 5 5 3 4 5 5 6 1
    ## [64693] 1 1 4 2 1 3 3 5 1 3 3 5 3 6 3 3 6 4 1 5 3 6 6 6 1 6 3 1 7 1 5 4 3 6 5 3
    ## [64729] 3 5 5 3 5 5 4 2 4 6 4 5 4 4 4 2 6 1 6 6 4 2 3 3 6 4 3 4 4 5 3 5 3 3 3 4
    ## [64765] 6 6 4 5 4 5 4 4 5 4 4 4 4 3 6 3 6 6 3 6 4 1 6 4 3 3 5 1 4 4 1 4 4 1 1 3
    ## [64801] 5 2 1 5 2 5 5 5 5 5 5 1 5 2 3 5 3 3 4 4 5 3 5 3 3 5 4 3 4 1 4 5 1 6 2 6
    ## [64837] 4 6 6 6 6 6 3 3 3 3 5 2 2 3 3 3 1 5 6 2 6 1 2 6 4 3 5 2 4 5 2 3 2 5 2 2
    ## [64873] 3 1 4 6 4 4 4 6 4 3 4 4 5 5 5 5 3 5 4 3 4 1 5 2 5 3 2 1 2 1 2 7 2 2 1 2
    ## [64909] 2 2 3 3 1 1 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [64945] 2 2 2 2 2 2 2 2 2 1 7 1 5 5 4 5 4 5 3 4 2 4 3 6 4 4 5 5 5 5 5 5 5 5 5 5
    ## [64981] 3 5 5 5 4 5 5 5 1 1 3 1 3 3 3 4 5 2 5 5 5 5 4 1 5 4 5 3 1 6 4 2 5 3 4 4
    ## [65017] 6 5 3 3 1 1 5 5 3 4 4 6 3 6 6 3 4 4 4 3 6 6 5 5 5 3 4 4 4 4 5 6 6 6 5 6
    ## [65053] 4 4 4 4 5 3 5 5 5 6 5 4 7 4 6 4 2 5 3 5 2 3 1 1 1 4 1 3 3 1 5 3 1 4 3 3
    ## [65089] 3 2 2 4 2 4 3 2 5 3 6 4 4 3 5 3 4 5 6 3 6 4 5 4 6 5 6 6 4 8 6 6 6 5 4 7
    ## [65125] 1 3 1 5 3 5 5 4 5 3 6 5 5 4 2 3 3 5 5 3 4 4 4 4 4 1 3 4 6 1 6 5 5 5 5 5
    ## [65161] 5 3 5 4 2 2 5 2 2 3 3 3 3 1 4 3 5 5 1 4 1 3 5 6 4 6 1 6 1 4 4 4 6 4 5 4
    ## [65197] 3 5 6 4 5 4 6 4 4 4 6 6 5 4 6 3 3 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 1 5 5
    ## [65233] 5 3 3 5 4 3 1 3 5 3 6 4 6 6 3 2 1 4 4 6 6 3 6 3 4 5 5 4 3 6 3 1 5 6 5 5
    ## [65269] 5 3 5 5 5 5 5 5 5 3 3 3 1 4 3 4 4 4 3 4 4 4 4 4 4 4 5 4 6 6 4 6 4 6 4 6
    ## [65305] 4 6 4 6 6 6 6 4 6 6 6 6 6 6 6 6 4 4 4 6 6 4 6 6 6 1 6 6 4 4 4 4 4 6 3 3
    ## [65341] 1 2 3 5 5 5 2 5 5 1 4 1 3 2 1 4 6 4 1 3 4 3 2 1 6 5 5 5 5 5 1 5 5 5 5 3
    ## [65377] 5 4 4 7 4 3 2 3 5 3 3 3 3 3 6 5 5 5 6 1 5 5 5 5 5 3 4 6 4 3 5 5 5 5 3 5
    ## [65413] 5 3 5 5 4 6 4 5 5 2 4 5 5 5 5 5 5 6 6 5 3 2 3 5 3 2 2 1 6 4 1 3 4 2 6 4
    ## [65449] 4 6 6 6 6 5 5 4 6 4 2 5 5 3 2 5 1 6 4 1 1 4 4 2 4 1 3 3 4 3 6 3 3 3 3 3
    ## [65485] 3 3 3 3 3 3 4 3 3 3 5 1 3 3 3 1 5 4 5 4 5 3 5 6 3 5 3 1 4 1 3 3 3 1 1 4
    ## [65521] 4 1 2 3 2 3 6 6 2 1 6 4 2 6 5 2 4 6 1 2 2 2 2 6 4 6 6 5 6 5 2 5 2 2 4 4
    ## [65557] 4 2 3 2 3 4 4 4 2 2 2 6 6 2 4 3 4 6 6 6 2 4 3 6 4 1 3 1 3 2 1 5 5 2 5 5
    ## [65593] 5 6 4 6 4 2 6 2 4 2 5 2 2 2 2 3 2 2 3 4 4 3 4 6 3 4 1 6 1 6 4 4 4 6 4 6
    ## [65629] 3 3 6 7 4 2 4 1 5 5 3 2 4 2 1 4 4 2 6 4 2 6 3 6 2 2 6 6 5 3 3 5 5 5 3 4
    ## [65665] 6 5 1 3 1 3 4 1 5 2 5 1 1 3 3 5 1 4 6 4 6 6 6 6 6 4 6 6 6 6 4 4 4 6 4 6
    ## [65701] 4 4 6 6 5 5 6 5 4 3 3 3 1 5 4 3 3 8 3 3 5 5 1 4 3 4 4 1 5 5 3 5 6 4 4 3
    ## [65737] 6 2 6 5 4 6 2 2 6 6 7 2 4 4 6 4 3 2 4 4 4 6 2 6 4 6 4 3 2 4 6 6 4 3 5 1
    ## [65773] 3 1 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 4 1 1 1 5 3 4 4 4 1 5 4 5
    ## [65809] 4 4 5 3 3 3 4 4 4 1 3 4 5 4 7 4 4 2 2 2 1 3 4 3 3 6 1 1 1 5 5 1 3 1 1 4
    ## [65845] 4 4 3 3 6 1 3 3 1 1 6 6 6 5 2 6 2 2 6 2 2 2 4 2 2 4 3 6 6 3 6 4 4 6 3 5
    ## [65881] 2 5 5 3 3 1 3 5 5 5 5 5 5 5 5 3 5 1 4 2 3 5 2 2 4 3 5 5 2 3 5 5 5 5 5 6
    ## [65917] 3 5 1 4 4 3 4 1 5 5 2 2 2 2 2 2 4 2 5 2 2 2 3 4 2 2 2 2 4 5 5 3 3 4 3 5
    ## [65953] 6 5 5 5 5 5 5 5 3 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 1 3 6 5 2 5 4
    ## [65989] 6 2 4 4 4 4 5 4 6 2 5 4 4 4 6 4 6 6 4 6 4 6 1 3 3 4 4 3 6 3 1 5 7 1 5 6
    ## [66025] 6 5 1 6 4 6 4 5 5 3 3 4 3 6 5 4 4 3 3 3 1 1 3 6 4 5 3 4 6 3 4 4 5 5 1 1
    ## [66061] 3 5 3 5 3 5 5 4 1 5 3 5 4 3 5 1 4 6 1 1 1 8 7 5 5 6 5 4 4 4 4 1 4 5 4 5
    ## [66097] 5 4 3 6 5 4 5 4 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 6 3 6 4 5 4 1 5 4 4 2 3 5
    ## [66133] 6 5 4 5 4 6 3 6 5 5 2 6 5 6 5 5 5 5 5 5 5 5 3 5 4 4 5 5 4 5 4 5 5 1 5 4
    ## [66169] 3 6 6 1 1 3 2 2 3 5 2 2 2 5 2 3 5 5 3 3 2 5 5 5 4 3 5 5 4 5 4 1 4 6 3 4
    ## [66205] 1 6 4 5 5 6 6 4 6 2 5 1 5 6 4 5 5 5 5 5 5 2 1 3 5 3 5 3 6 6 6 4 6 4 5 4
    ## [66241] 2 4 5 5 4 5 5 5 4 2 5 5 5 5 1 5 5 5 5 5 5 4 8 4 6 3 8 4 4 1 3 6 4 4 3 3
    ## [66277] 6 6 1 3 3 3 4 1 1 6 1 5 3 4 4 5 3 3 1 3 6 6 5 4 3 4 3 5 1 8 3 3 2 4 4 7
    ## [66313] 5 3 3 5 5 5 5 5 3 5 5 5 4 4 5 5 2 2 4 3 1 4 2 2 1 6 5 1 5 3 5 3 5 1 4 5
    ## [66349] 1 5 7 5 3 5 5 5 3 5 5 8 3 4 5 3 4 5 2 5 4 3 3 4 3 4 3 1 5 2 3 6 3 2 3 3
    ## [66385] 4 3 4 6 6 5 4 3 3 6 6 3 6 6 3 3 6 6 4 2 6 6 3 3 3 4 3 1 4 4 4 4 1 4 5 5
    ## [66421] 2 5 5 2 5 5 3 3 2 4 5 2 5 6 5 2 4 1 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 3 5 5
    ## [66457] 5 5 5 5 3 3 3 1 6 4 4 4 5 6 4 5 4 6 6 6 3 4 2 6 6 6 6 3 2 4 4 4 4 4 3 4
    ## [66493] 5 6 5 4 3 1 3 5 5 5 1 5 1 1 5 1 5 2 4 2 2 5 6 1 4 3 4 1 5 5 6 5 6 1 1 5
    ## [66529] 1 5 5 4 5 1 5 5 2 5 5 2 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 4 4 6 3 4 4
    ## [66565] 3 3 2 3 3 3 3 4 3 3 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5
    ## [66601] 5 1 3 3 4 5 3 3 5 4 5 5 5 5 4 4 5 4 3 5 4 4 6 6 4 4 4 3 6 6 4 6 4 3 4 1
    ## [66637] 3 5 3 1 5 3 1 2 3 6 3 4 6 6 2 6 5 4 1 1 1 5 3 4 1 1 3 5 3 3 1 4 1 1 4 3
    ## [66673] 5 3 4 2 3 3 2 1 2 6 1 3 1 2 4 4 5 5 4 5 4 4 5 6 6 4 6 3 6 4 5 5 5 5 5 5
    ## [66709] 5 5 5 5 5 5 5 3 4 3 4 3 1 1 3 4 6 4 6 6 6 6 4 4 1 6 4 4 4 5 6 4 2 4 5 5
    ## [66745] 4 4 4 4 4 3 7 4 1 5 4 5 4 6 1 5 4 6 4 6 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [66781] 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 6 5 2 1 3 4 4 4 6 5 5 5 4 5 5 5 2 2 5 5 5
    ## [66817] 5 5 3 5 5 6 3 4 4 4 6 6 4 6 6 3 4 6 4 6 6 4 6 3 4 6 3 4 1 4 6 6 3 4 4 3
    ## [66853] 4 4 4 6 5 1 5 4 6 6 3 5 4 6 6 3 4 4 1 4 1 5 1 1 3 5 1 5 3 5 1 4 1 3 1 1
    ## [66889] 5 3 6 2 5 4 4 6 6 4 3 6 6 4 4 4 4 4 4 2 4 6 6 4 6 3 3 6 4 3 5 1 6 3 4 4
    ## [66925] 4 1 5 4 5 1 4 3 4 1 5 5 5 2 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 2 3 5 5 5 5 5
    ## [66961] 5 5 5 1 3 1 3 8 5 1 1 1 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3
    ## [66997] 3 3 3 3 1 5 5 7 3 6 1 3 3 3 3 3 1 8 1 3 3 6 3 3 4 4 3 3 3 3 3 1 1 4 7 1
    ## [67033] 1 4 4 1 1 7 3 5 5 1 6 5 4 5 4 5 4 6 5 3 5 5 5 5 5 6 6 5 6 5 3 2 6 1 3 3
    ## [67069] 4 4 3 1 5 5 5 5 3 1 3 6 5 3 3 4 2 6 3 3 1 6 6 1 2 6 6 3 3 3 3 3 3 2 5 5
    ## [67105] 5 5 5 5 5 5 5 3 5 4 5 4 5 6 4 6 4 6 3 3 6 6 4 3 4 5 5 5 5 5 5 5 5 5 5 5
    ## [67141] 5 5 5 5 5 5 5 5 1 3 6 6 6 6 4 4 6 5 3 1 4 4 3 6 4 3 4 3 2 4 5 6 4 2 4 3
    ## [67177] 3 4 6 4 1 1 1 4 3 8 5 8 5 6 5 3 5 5 3 3 3 3 1 3 4 4 6 4 6 5 3 3 4 5 4 4
    ## [67213] 4 2 4 4 3 2 4 1 5 6 6 6 5 1 1 2 5 5 2 4 2 5 4 7 6 4 5 5 3 3 2 6 2 2 2 4
    ## [67249] 6 3 2 2 2 6 2 2 2 6 6 4 2 2 4 3 6 4 4 6 4 4 2 4 5 4 5 6 4 2 6 6 4 4 2 6
    ## [67285] 6 4 4 1 1 3 5 5 5 1 3 4 2 3 5 1 6 5 5 5 6 1 5 5 5 5 5 5 5 5 5 5 3 2 5 5
    ## [67321] 5 5 5 5 5 5 5 1 5 3 3 5 5 6 3 1 3 1 4 6 1 5 3 5 5 4 6 5 1 5 1 4 3 3 3 5
    ## [67357] 6 3 6 1 6 5 2 5 5 4 2 1 4 1 4 1 5 5 1 6 3 5 5 5 3 4 3 4 5 5 5 7 5 5 2 2
    ## [67393] 5 5 4 5 5 5 4 8 1 1 5 1 5 5 5 4 5 1 5 5 5 3 5 5 4 4 5 5 5 5 4 6 7 4 5 5
    ## [67429] 3 5 4 5 2 5 3 4 4 3 6 1 3 5 1 6 6 3 5 5 4 4 3 3 3 3 5 3 4 3 7 5 3 5 8 1
    ## [67465] 1 3 5 5 5 4 5 5 1 3 1 5 5 6 4 4 3 4 4 2 3 2 5 6 4 4 6 3 4 4 6 4 4 5 4 6
    ## [67501] 5 5 5 3 4 4 1 1 2 2 2 2 2 1 1 1 2 5 3 5 4 1 2 1 8 3 5 7 5 1 2 1 3 7 4 4
    ## [67537] 2 5 5 5 5 4 5 5 5 5 1 3 3 1 3 1 4 3 2 4 6 6 6 4 6 6 4 6 6 6 4 5 4 3 1 4
    ## [67573] 3 5 8 3 6 6 3 4 4 3 5 2 5 2 5 1 3 5 5 5 3 6 4 4 6 4 6 4 6 4 6 4 3 3 6 4
    ## [67609] 3 4 4 2 3 6 4 4 4 3 1 5 6 3 4 3 3 6 1 2 2 4 1 2 5 1 3 5 3 3 3 6 4 2 4 4
    ## [67645] 5 5 7 5 4 3 3 6 3 4 3 3 3 3 3 1 6 5 4 4 1 4 1 4 4 6 6 6 3 6 6 4 4 6 4 4
    ## [67681] 5 5 5 4 3 6 6 5 5 4 4 4 5 3 5 4 5 6 7 5 3 1 5 4 6 3 1 4 4 4 6 6 4 6 4 4
    ## [67717] 6 6 6 4 6 6 4 4 6 2 4 2 2 6 6 2 4 2 4 2 4 2 1 6 4 2 4 3 6 1 5 4 6 4 6 5
    ## [67753] 4 5 5 2 2 3 5 2 2 1 3 6 6 4 1 1 4 5 3 1 1 5 5 5 1 1 1 5 1 5 5 1 1 2 1 1
    ## [67789] 4 3 6 3 6 3 3 6 6 4 4 6 6 6 4 6 4 1 1 5 6 4 3 6 4 3 4 6 2 6 6 3 6 5 4 3
    ## [67825] 5 4 3 3 2 3 3 4 4 4 4 4 4 6 3 5 2 1 1 4 1 4 5 6 2 2 2 6 4 6 6 4 6 3 4 6
    ## [67861] 6 6 4 2 6 2 3 6 2 3 5 2 1 1 2 5 3 5 3 5 3 3 4 2 3 1 3 1 5 5 3 4 3 7 1 6
    ## [67897] 7 3 3 1 3 1 5 6 6 6 6 6 3 4 6 6 6 6 3 6 4 6 2 2 6 4 3 6 4 4 3 4 4 5 1 3
    ## [67933] 5 6 3 5 1 5 1 1 6 1 5 5 5 5 5 4 6 3 3 5 4 2 4 5 3 2 3 3 3 3 3 3 3 3 4 5
    ## [67969] 6 4 4 1 3 3 4 3 4 6 6 6 6 4 1 4 6 4 4 6 6 3 6 2 4 3 5 5 4 5 2 3 3 3 2 2
    ## [68005] 2 2 2 4 2 6 6 6 6 4 3 4 2 5 5 5 5 5 4 5 5 5 4 4 4 5 5 3 5 5 6 4 5 5 2 5
    ## [68041] 4 4 5 2 5 5 3 4 4 8 3 4 3 6 1 4 6 6 6 4 6 4 6 6 6 1 5 4 3 4 3 4 4 4 5 4
    ## [68077] 3 8 3 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5
    ## [68113] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [68149] 1 1 3 4 4 3 5 6 3 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5
    ## [68185] 5 1 1 6 4 3 3 3 3 5 6 2 5 5 3 5 5 5 5 2 1 2 5 5 2 5 5 5 5 5 5 2 6 3 5 4
    ## [68221] 6 4 6 4 4 4 6 4 4 4 4 3 4 3 4 4 4 2 4 4 4 4 4 4 3 3 1 5 5 5 5 6 2 2 5 6
    ## [68257] 6 5 2 6 4 3 3 1 1 5 5 5 5 5 5 5 5 3 2 3 5 5 3 3 3 5 3 5 5 5 5 5 5 5 5 5
    ## [68293] 5 5 5 5 5 5 5 5 4 3 2 4 1 4 6 6 5 5 2 1 4 5 4 4 4 5 7 4 5 1 5 5 4 3 5 4
    ## [68329] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 3 5 5 3 4 6 4 4 4 6 6 6
    ## [68365] 4 4 4 6 4 4 6 3 5 3 1 3 6 6 4 3 6 5 6 6 6 6 6 6 6 2 6 6 4 2 6 6 4 4 4 6
    ## [68401] 6 6 6 6 4 6 6 4 6 4 6 4 6 6 6 4 6 5 6 6 6 4 6 4 5 4 5 4 2 2 5 5 6 4 3 4
    ## [68437] 5 4 4 2 6 5 5 4 4 5 3 1 5 5 2 2 2 1 1 1 1 2 2 1 1 1 1 2 1 2 5 1 2 2 2 4
    ## [68473] 5 6 1 2 2 2 2 1 5 5 4 4 3 1 4 3 5 5 4 4 5 5 1 3 1 5 4 4 4 4 3 6 4 3 4 4
    ## [68509] 4 3 6 4 5 5 5 5 5 3 5 5 5 5 5 5 3 6 3 2 2 4 4 6 4 6 4 4 3 5 2 5 5 5 5 5
    ## [68545] 5 5 5 5 5 5 5 5 5 4 2 5 5 5 5 5 5 5 5 5 1 7 4 4 2 4 5 4 5 3 6 5 4 4 6 6
    ## [68581] 3 2 2 4 4 3 3 3 4 7 1 1 3 2 3 1 3 4 4 6 4 2 3 3 6 6 6 2 3 3 2 2 5 3 3 1
    ## [68617] 4 7 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 2 5 5 5
    ## [68653] 5 5 2 3 4 5 5 5 6 3 3 6 5 6 3 2 3 6 5 6 1 6 4 3 4 1 1 1 4 5 3 1 1 4 3 3
    ## [68689] 1 4 4 4 6 4 1 3 5 4 3 4 1 4 6 3 1 4 5 3 2 2 5 5 4 4 3 1 4 3 3 2 2 1 6 6
    ## [68725] 1 6 6 6 6 3 2 5 2 5 5 5 6 6 1 1 5 1 5 5 5 1 5 3 4 3 6 4 6 6 6 6 4 6 3 4
    ## [68761] 3 3 3 4 4 4 4 6 6 4 3 4 3 6 6 5 3 6 6 4 6 4 4 6 4 5 4 1 2 2 2 2 2 2 2 2
    ## [68797] 2 4 2 1 2 3 2 6 2 2 6 4 4 6 2 6 3 4 6 2 4 5 1 2 2 4 5 2 5 3 5 4 4 5 5 1
    ## [68833] 6 1 2 1 1 6 5 5 5 1 4 7 5 5 1 5 2 2 2 5 5 5 5 3 6 5 5 4 4 5 5 5 4 4 2 6
    ## [68869] 4 6 1 6 6 4 3 3 5 3 4 3 6 2 3 6 6 6 6 6 6 4 4 4 4 6 4 4 6 4 6 1 6 2 1 2
    ## [68905] 3 5 4 3 5 4 1 3 5 5 2 2 3 5 2 5 5 7 1 4 4 6 3 3 4 1 4 4 5 3 1 5 4 4 4 4
    ## [68941] 3 3 3 3 5 4 3 3 3 1 2 3 1 3 1 3 1 4 5 5 1 5 5 5 1 7 5 5 5 1 3 5 5 5 5 4
    ## [68977] 6 6 4 6 6 4 6 7 4 4 3 1 3 6 1 5 5 5 3 1 2 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5
    ## [69013] 5 5 5 5 5 5 5 5 5 3 1 5 5 5 1 3 5 3 6 5 1 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5
    ## [69049] 5 5 5 5 5 6 4 6 4 4 3 2 3 3 4 4 5 3 4 3 5 3 6 3 3 3 5 4 5 6 6 3 4 4 3 3
    ## [69085] 1 5 3 6 2 2 2 2 2 2 2 2 2 2 2 3 6 3 2 5 5 5 5 5 5 4 6 5 5 5 5 5 3 1 5 4
    ## [69121] 6 2 6 4 6 6 4 6 5 5 4 6 5 4 6 6 4 6 4 2 4 4 6 4 3 4 4 4 4 4 4 4 3 5 1 5
    ## [69157] 4 6 6 6 4 6 6 6 5 4 5 2 5 1 6 5 5 5 4 5 5 1 3 1 2 1 3 1 4 3 2 6 6 4 1 5
    ## [69193] 2 4 6 2 6 5 4 5 4 5 5 2 2 1 3 3 1 3 4 2 4 6 1 6 1 4 2 2 1 2 1 6 4 4 3 6
    ## [69229] 5 1 5 5 2 5 3 5 5 4 6 6 4 6 6 6 5 6 6 6 4 4 4 4 6 6 6 4 3 3 5 5 5 5 4 4
    ## [69265] 3 4 3 1 4 1 3 3 1 4 3 5 5 3 4 5 5 5 5 5 5 5 5 5 5 5 5 1 6 4 3 3 5 4 5 5
    ## [69301] 5 5 5 1 5 5 5 5 5 4 5 5 5 4 4 3 7 4 2 3 3 3 1 3 2 6 1 5 4 5 5 5 5 5 5 5
    ## [69337] 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 5 5 4 3 4 4 1 3 4 5 4 4 3
    ## [69373] 4 5 5 1 1 4 4 6 3 2 4 4 4 5 5 6 6 2 1 2 6 4 4 4 3 4 3 1 3 5 5 3 4 3 8 1
    ## [69409] 8 1 6 4 6 5 4 4 4 6 4 4 4 4 4 4 5 3 3 3 4 4 4 6 1 6 1 4 4 3 3 3 5 3 5 5
    ## [69445] 5 6 3 4 3 5 6 4 6 4 4 5 6 6 4 3 4 6 3 6 1 1 3 2 3 6 6 5 4 3 3 6 1 6 6 6
    ## [69481] 1 5 3 6 6 5 3 4 4 6 1 6 3 5 1 1 3 4 1 1 1 1 1 1 4 5 1 1 4 1 1 4 6 6 4 6
    ## [69517] 4 5 4 1 5 5 5 5 1 4 5 5 1 5 5 1 2 1 5 5 5 3 5 1 3 5 5 5 6 1 1 3 3 5 1 3
    ## [69553] 5 5 5 5 5 5 5 5 2 5 5 4 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 2 5 5 5 5 5 5 5
    ## [69589] 5 4 5 5 5 1 5 4 3 6 4 5 5 5 5 4 5 5 5 5 3 5 5 5 5 5 5 5 3 5 4 4 5 5 5 5
    ## [69625] 5 5 1 1 5 4 3 3 5 5 2 2 6 4 4 6 4 6 2 2 4 2 4 4 2 2 2 4 2 4 6 5 5 5 5 3
    ## [69661] 5 5 1 4 4 4 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 2 1 1 5 4 1 4 4 2 2 3 3 4 3 1
    ## [69697] 3 5 5 1 6 6 3 3 6 6 6 6 4 3 1 4 6 3 3 4 1 3 6 6 6 4 6 4 3 3 6 6 3 6 3 1
    ## [69733] 4 3 4 4 5 3 5 5 3 5 5 3 2 2 4 4 2 2 4 4 3 5 2 4 2 4 8 6 5 5 5 5 5 2 5 3
    ## [69769] 2 5 1 5 5 2 5 5 5 5 5 5 7 1 3 5 5 5 5 4 5 6 4 3 5 3 5 4 6 3 5 5 5 2 5 5
    ## [69805] 3 5 2 6 3 4 7 6 6 5 4 5 5 5 5 3 5 5 5 5 4 3 2 4 4 4 4 1 1 1 6 6 4 6 1 4
    ## [69841] 6 4 4 4 4 6 4 6 6 3 1 4 6 6 3 4 4 3 6 5 2 4 5 4 4 2 5 5 2 3 2 1 2 2 4 7
    ## [69877] 5 6 6 4 1 3 6 3 3 2 6 2 6 6 2 4 6 4 6 4 5 2 4 2 6 3 2 2 6 6 4 6 2 2 2 6
    ## [69913] 6 4 2 2 4 1 4 1 6 3 4 2 3 3 4 5 5 5 5 5 5 2 4 5 5 5 5 5 3 5 5 5 5 5 5 5
    ## [69949] 4 5 3 6 5 5 1 5 1 5 6 5 3 6 5 5 6 6 5 5 5 3 3 3 6 5 3 5 6 5 4 5 2 3 2 2
    ## [69985] 2 2 2 2 6 2 2 2 2 2 2 4 2 6 6 6 2 6 2 2 6 2 2 6 2 2 2 6 6 3 3 3 4 1 1 4
    ## [70021] 5 2 1 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 1 1 4 1 3 3 5 3 3 6 2 4 7 4 2 1 3 3
    ## [70057] 5 4 1 4 3 1 1 3 6 6 4 3 3 3 3 3 4 8 5 3 6 5 4 5 3 5 4 5 6 4 5 6 4 3 4 3
    ## [70093] 5 5 5 5 5 2 5 5 5 5 3 5 5 1 4 2 2 5 2 6 2 2 1 5 2 2 2 2 4 5 2 2 2 2 2 2
    ## [70129] 5 1 3 5 4 4 3 6 4 4 7 1 6 4 6 5 5 5 5 4 2 3 6 2 5 4 2 3 3 4 4 4 3 1 4 5
    ## [70165] 5 5 4 5 5 5 1 4 4 5 3 1 1 3 4 5 5 6 2 1 4 4 5 3 5 2 3 5 4 4 3 5 4 4 4 5
    ## [70201] 5 5 1 5 5 5 5 5 5 5 5 5 5 3 2 3 3 4 4 5 4 4 2 4 3 4 4 5 2 2 5 2 4 4 3 6
    ## [70237] 6 4 4 5 2 3 3 5 5 7 4 4 4 4 4 4 4 4 4 4 2 2 3 2 5 4 3 3 5 3 1 3 1 5 2 2
    ## [70273] 5 2 5 5 3 5 1 3 3 1 4 5 3 4 2 3 6 6 6 4 6 6 2 3 6 4 6 4 6 2 4 6 6 4 4 4
    ## [70309] 4 6 6 6 6 6 6 6 4 6 6 4 6 6 5 4 1 3 3 3 5 3 3 3 1 5 1 1 3 3 5 6 3 5 4 5
    ## [70345] 4 1 7 1 6 6 1 3 6 2 3 4 4 3 3 6 5 1 1 6 3 4 3 6 4 5 2 3 3 1 5 1 4 2 3 2
    ## [70381] 6 1 3 6 4 1 1 2 5 2 5 2 5 4 5 3 5 1 2 3 2 2 2 2 2 2 2 2 5 1 3 5 4 5 5 5
    ## [70417] 5 5 5 5 5 3 3 3 6 6 6 6 6 6 4 6 3 1 1 6 4 6 5 4 4 5 4 3 5 1 3 5 3 3 5 3
    ## [70453] 6 1 4 5 6 3 6 5 3 5 2 5 5 6 2 5 2 5 6 3 4 6 4 4 4 6 4 3 4 1 6 2 4 6 5 3
    ## [70489] 6 4 6 3 6 5 6 7 5 4 4 4 4 4 4 4 4 3 4 3 2 1 3 4 4 3 1 6 2 3 5 3 3 3 3 3
    ## [70525] 1 3 4 3 3 3 6 4 4 5 6 6 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 3
    ## [70561] 5 5 5 6 2 5 2 1 2 2 2 5 2 5 5 2 5 2 2 2 5 5 2 2 1 5 4 3 5 5 6 5 6 5 4 1
    ## [70597] 4 5 3 1 5 5 5 5 5 5 5 3 4 5 4 6 8 6 3 6 5 3 4 4 5 6 4 4 6 3 1 6 6 1 4 4
    ## [70633] 6 6 6 1 4 4 6 6 4 3 3 3 5 3 5 5 3 4 4 5 4 1 6 4 3 5 3 3 1 1 3 5 2 5 6 1
    ## [70669] 1 3 6 1 6 5 3 5 3 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 2 3 3 6 3 4
    ## [70705] 3 3 5 5 5 5 4 3 3 5 1 3 1 3 4 4 3 1 1 2 5 4 4 5 6 6 6 6 4 4 5 4 4 5 2 4
    ## [70741] 4 4 2 5 5 3 5 5 5 5 5 5 5 5 5 5 5 1 3 5 4 4 3 5 3 5 5 5 5 5 5 4 5 5 3 5
    ## [70777] 2 4 4 4 6 6 6 4 6 6 5 5 6 4 5 5 4 5 1 5 3 3 5 5 3 3 6 3 6 6 4 5 5 3 3 4
    ## [70813] 6 6 6 4 4 6 4 3 5 5 1 6 6 2 6 4 4 1 4 6 6 1 4 1 4 6 4 4 4 5 4 6 6 4 6 1
    ## [70849] 5 5 5 3 4 3 5 3 3 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 7 4 1 5 5 3 4 3 4 4 4 5
    ## [70885] 5 5 2 5 2 5 2 4 1 5 5 5 5 5 2 5 2 2 6 2 2 5 5 1 2 2 5 3 3 4 6 4 4 5 6 5
    ## [70921] 5 6 2 6 2 2 3 2 4 5 4 6 4 5 6 6 4 5 6 6 6 6 4 4 6 5 6 2 5 5 5 6 6 6 4 6
    ## [70957] 6 4 6 6 3 1 3 1 1 4 4 6 6 4 6 4 4 1 6 6 6 4 2 4 3 1 5 5 5 3 5 5 5 5 5 5
    ## [70993] 5 1 3 4 3 3 1 1 4 6 6 4 6 4 6 4 4 4 6 4 6 6 5 4 1 2 4 1 3 1 1 3 3 5 1 5
    ## [71029] 4 7 5 5 5 5 1 7 4 5 5 1 5 3 4 6 4 3 3 1 4 6 2 3 4 2 6 3 6 2 1 2 5 1 5 3
    ## [71065] 5 1 4 2 1 3 5 5 1 5 5 3 6 2 5 3 1 3 3 3 3 3 5 4 1 6 4 6 1 4 6 4 1 1 3 4
    ## [71101] 5 4 4 5 5 1 6 4 4 3 4 6 4 4 4 4 4 6 3 6 5 4 2 3 6 3 4 4 4 4 4 4 5 2 3 1
    ## [71137] 1 1 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 1 3 4 1
    ## [71173] 4 5 5 5 5 5 2 5 2 5 5 5 3 2 5 5 5 5 5 5 5 5 5 5 1 1 4 6 4 6 6 6 3 1 3 3
    ## [71209] 6 4 5 6 5 4 3 1 6 4 4 3 5 2 5 5 5 6 1 5 5 5 5 5 5 1 5 5 5 3 3 1 5 3 4 3
    ## [71245] 4 1 3 1 3 4 3 4 4 3 5 5 4 6 6 6 3 6 6 3 4 6 6 4 4 4 1 5 5 5 5 5 2 5 6 4
    ## [71281] 5 5 5 5 5 5 5 6 6 5 1 4 4 6 3 4 5 5 3 6 5 4 4 3 4 5 1 4 4 5 2 2 2 2 2 3
    ## [71317] 2 4 6 1 6 6 6 4 6 2 6 4 4 6 4 4 6 4 2 6 4 4 4 4 5 3 6 6 3 6 4 4 5 6 6 6
    ## [71353] 2 4 4 4 5 6 5 3 6 3 6 6 6 6 6 2 4 6 2 2 6 2 5 4 5 3 5 5 2 3 2 2 2 1 2 6
    ## [71389] 6 4 3 2 3 3 6 5 6 5 5 2 4 6 6 4 5 4 6 3 6 6 6 4 3 3 3 3 3 4 4 1 4 5 7 5
    ## [71425] 5 5 3 5 3 4 4 5 4 6 4 4 6 5 2 4 4 2 4 5 1 4 5 4 4 3 2 5 3 1 1 1 5 1 1 4
    ## [71461] 3 5 5 6 4 3 3 5 5 4 4 3 4 5 6 3 5 1 5 5 4 5 5 5 4 4 5 6 3 4 3 2 3 1 5 1
    ## [71497] 6 3 3 3 4 5 3 5 1 5 6 5 1 4 3 3 4 1 3 7 1 2 5 5 5 5 5 5 5 5 3 1 5 4 1 3
    ## [71533] 5 4 6 5 4 5 3 5 6 4 6 4 6 5 3 6 6 4 1 6 4 3 4 6 6 4 4 3 6 5 6 5 3 6 3 3
    ## [71569] 3 1 4 4 6 6 4 4 4 4 4 4 6 3 4 6 4 1 5 5 1 1 1 1 1 4 5 1 5 5 2 3 6 4 3 1
    ## [71605] 3 1 2 5 4 4 5 3 5 1 5 1 5 3 5 5 5 5 5 6 5 6 5 5 5 5 1 5 1 1 3 3 5 4 4 5
    ## [71641] 1 4 1 4 1 3 3 3 3 3 4 5 6 6 2 3 2 2 3 3 2 3 3 2 6 4 3 2 5 3 3 1 4 5 7 3
    ## [71677] 3 1 7 3 3 1 4 1 5 5 4 5 3 2 6 4 6 6 2 4 5 5 5 1 6 5 6 6 5 5 5 5 5 4 4 2
    ## [71713] 4 3 4 5 5 5 5 5 5 5 5 5 5 5 4 5 5 4 5 3 1 5 5 3 3 3 4 5 7 4 1 1 1 4 3 3
    ## [71749] 6 4 3 6 4 4 4 6 4 4 6 3 4 6 4 5 3 5 1 1 4 1 5 4 5 5 1 5 5 4 6 3 3 4 4 4
    ## [71785] 4 4 3 4 3 5 5 5 5 5 3 5 2 1 6 3 4 5 4 4 3 5 4 3 4 4 5 4 3 5 4 3 5 5 4 1
    ## [71821] 6 3 2 2 2 8 1 2 3 4 2 2 6 2 2 2 2 2 2 5 2 5 2 4 5 5 2 6 2 2 2 2 5 2 5 2
    ## [71857] 4 2 2 5 5 5 3 4 5 3 5 6 4 3 3 1 2 5 2 4 3 1 5 1 3 5 3 3 5 5 5 5 6 6 4 6
    ## [71893] 5 5 5 4 5 3 4 4 4 1 3 5 6 3 4 3 6 4 1 4 5 5 5 4 5 3 5 3 6 3 5 1 5 5 5 1
    ## [71929] 1 1 5 5 5 5 3 6 3 5 5 5 5 5 5 5 5 5 5 3 5 5 4 3 6 4 2 7 4 6 4 1 1 1 5 1
    ## [71965] 3 4 3 3 6 6 1 6 4 6 3 6 1 4 4 6 1 3 1 3 6 4 2 6 5 6 4 5 2 6 4 4 6 4 6 4
    ## [72001] 3 4 4 4 1 5 5 1 4 4 4 3 1 4 1 3 1 5 5 5 5 5 4 1 2 2 2 6 2 6 1 5 2 5 5 1
    ## [72037] 3 5 5 5 5 5 5 5 5 4 2 4 2 6 4 2 4 4 3 5 5 3 3 4 4 4 3 6 3 4 3 5 3 5 4 5
    ## [72073] 5 4 7 1 3 4 6 4 5 4 6 2 4 6 4 6 2 4 1 4 4 4 3 4 4 4 1 3 6 3 6 6 6 6 5 5
    ## [72109] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5
    ## [72145] 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 3 3 1 5 4 5 3 3 5 5 5 3 5 2 5 5 5 5 3 5
    ## [72181] 5 3 5 1 4 5 5 1 5 4 4 3 4 1 5 3 5 3 5 5 6 6 4 1 3 6 6 6 6 4 4 1 5 6 6 5
    ## [72217] 6 4 5 6 3 4 6 1 4 4 5 3 5 5 4 5 6 5 5 6 5 4 1 5 3 5 1 1 1 3 3 1 5 1 4 1
    ## [72253] 5 3 4 7 5 6 2 4 6 1 3 2 2 4 5 4 4 2 4 3 5 1 5 3 5 5 2 1 6 6 6 6 5 5 4 5
    ## [72289] 3 1 1 2 5 4 3 4 2 4 1 6 5 4 5 5 5 5 5 5 5 5 5 4 5 1 4 1 1 5 5 5 5 1 5 5
    ## [72325] 3 5 3 6 6 4 6 3 6 4 3 3 1 5 4 1 3 4 5 5 5 6 5 5 4 4 4 3 4 4 4 4 1 6 4 4
    ## [72361] 6 6 4 4 5 4 6 4 5 5 6 6 5 4 6 3 5 1 5 5 3 4 4 5 4 5 6 1 5 5 3 6 4 1 4 5
    ## [72397] 1 5 3 3 3 5 5 3 8 5 4 3 3 2 4 5 6 3 3 3 4 4 3 4 1 6 4 3 6 6 5 6 1 5 1 5
    ## [72433] 4 5 1 5 5 5 5 6 6 6 3 6 3 6 6 3 4 4 6 1 4 1 5 3 1 1 4 4 1 6 6 4 6 3 4 6
    ## [72469] 6 6 6 6 6 3 5 1 2 3 4 1 4 2 6 6 2 4 2 3 2 4 3 5 5 4 3 4 2 3 2 6 4 3 3 3
    ## [72505] 3 4 3 4 4 3 4 6 6 1 4 5 5 4 3 5 5 3 3 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [72541] 5 5 5 5 4 4 1 3 6 5 3 5 5 5 1 3 1 6 2 7 2 4 5 2 6 2 6 4 4 4 5 5 4 5 8 6
    ## [72577] 4 4 4 6 6 4 1 5 3 5 5 4 4 5 5 3 1 1 3 5 3 1 1 5 4 1 2 2 5 5 5 1 5 5 1 5
    ## [72613] 5 4 1 4 4 5 4 6 1 1 1 6 4 1 1 5 5 5 5 5 5 2 2 5 5 5 5 2 2 3 2 4 2 2 1 5
    ## [72649] 6 7 6 2 1 3 1 3 4 4 2 4 3 1 1 4 4 4 1 8 4 1 1 6 1 5 1 5 5 5 5 5 5 5 5 1
    ## [72685] 5 5 5 5 5 5 5 5 5 5 5 4 1 1 1 6 3 5 1 6 5 5 3 5 5 6 2 5 5 4 5 2 5 5 4 5
    ## [72721] 5 4 5 1 3 3 3 5 6 6 3 5 5 4 4 3 6 5 4 1 5 3 1 5 4 3 5 1 6 3 3 6 4 4 4 4
    ## [72757] 4 3 4 6 4 4 4 2 4 6 4 4 6 6 6 4 4 3 4 6 6 3 4 6 4 3 4 6 6 6 6 4 2 6 6 3
    ## [72793] 6 4 4 3 5 6 6 4 5 3 8 5 5 5 5 4 5 4 3 3 5 5 5 4 5 2 1 5 2 5 1 1 1 5 1 3
    ## [72829] 5 3 3 4 4 3 6 5 3 4 5 3 1 4 5 3 2 2 7 4 4 6 2 5 4 6 1 2 3 5 5 5 5 5 5 5
    ## [72865] 5 5 3 5 3 1 4 4 5 3 6 3 4 3 4 4 2 6 5 1 5 3 3 5 3 5 2 2 5 4 3 5 7 1 1 2
    ## [72901] 5 3 5 5 5 6 4 3 6 3 4 6 3 4 1 1 2 6 6 6 5 3 4 3 5 5 3 5 5 3 6 3 5 3 5 4
    ## [72937] 4 4 5 1 4 1 5 5 6 3 1 6 4 5 5 3 2 3 1 2 2 2 2 2 2 6 2 2 3 1 1 3 1 5 5 3
    ## [72973] 5 5 5 3 5 5 3 3 1 3 2 2 3 2 5 2 2 3 4 3 5 2 1 5 5 4 2 1 4 4 5 6 4 6 6 6
    ## [73009] 6 3 4 4 6 6 6 6 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 1 5 5 4 5 5 2 5 1 3
    ## [73045] 5 5 4 7 6 4 3 5 3 4 5 5 5 3 1 2 2 2 2 2 2 2 2 3 2 2 1 2 1 2 2 2 2 2 4 3
    ## [73081] 4 4 6 6 6 3 4 5 6 5 5 4 5 6 4 6 4 6 6 3 4 4 3 6 6 4 4 4 1 6 3 3 4 7 4 3
    ## [73117] 2 2 3 1 4 6 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2 7 4 4 4 3 5 5 1 3 1 5 1 1 6 6
    ## [73153] 4 1 1 1 6 1 1 4 4 1 3 5 6 1 3 6 5 2 4 5 5 5 3 3 3 3 4 5 1 2 5 1 4 7 4 6
    ## [73189] 2 6 1 3 1 3 5 5 4 5 6 5 4 5 4 4 2 2 2 2 3 2 2 1 5 5 2 5 6 4 5 4 2 6 4 5
    ## [73225] 5 1 6 6 5 1 5 4 4 5 4 6 3 2 4 4 6 4 3 5 4 1 3 3 4 7 5 5 5 3 3 6 1 6 4 5
    ## [73261] 6 4 5 5 3 3 4 2 2 2 3 2 6 2 3 2 2 2 2 6 1 3 6 6 6 6 3 4 4 5 5 4 4 1 5 3
    ## [73297] 4 4 3 5 3 3 5 5 3 4 5 6 6 6 5 8 4 6 5 1 6 4 4 1 1 6 1 4 4 4 5 5 4 5 6 4
    ## [73333] 5 5 6 6 4 6 4 6 4 4 1 6 6 4 4 7 6 4 4 2 6 5 2 4 4 6 1 1 5 6 3 4 1 4 5 4
    ## [73369] 5 5 2 5 3 3 4 3 3 3 1 3 3 1 6 2 4 3 5 1 2 5 2 2 4 1 3 1 4 2 7 6 5 5 5 5
    ## [73405] 6 6 5 5 4 2 2 5 5 1 4 3 5 2 5 5 1 5 3 1 6 3 3 4 5 2 6 1 2 6 3 2 3 3 6 5
    ## [73441] 5 3 5 5 4 3 4 1 4 3 1 3 1 5 3 5 5 6 1 5 5 3 6 4 6 6 1 5 6 4 4 5 6 6 6 3
    ## [73477] 5 5 6 5 5 5 5 5 5 4 5 5 4 5 5 5 5 3 6 5 3 3 3 4 5 4 6 5 4 3 5 5 5 5 5 5
    ## [73513] 5 3 5 4 3 5 3 5 7 4 1 5 5 4 5 4 5 5 5 1 3 5 1 3 3 1 6 6 6 6 4 6 3 4 4 4
    ## [73549] 3 5 5 5 5 4 5 5 5 4 2 4 5 2 5 3 5 5 5 5 5 4 5 5 2 3 5 1 1 3 6 3 4 4 4 6
    ## [73585] 6 6 6 4 1 4 4 3 3 4 6 5 2 2 2 2 5 3 4 3 3 1 5 5 5 5 5 5 5 5 6 5 4 3 4 3
    ## [73621] 1 5 1 3 1 1 4 3 4 7 4 2 3 4 5 5 3 5 5 5 5 5 1 5 5 5 5 3 5 3 5 6 5 4 7 1
    ## [73657] 2 4 4 5 4 2 3 6 5 5 5 4 5 4 4 2 4 1 5 4 4 1 3 4 1 5 4 6 4 4 4 4 3 4 2 4
    ## [73693] 4 3 3 5 5 2 5 1 1 5 5 7 5 5 2 5 5 5 4 5 5 5 4 6 6 5 5 4 5 5 6 5 6 1 3 1
    ## [73729] 3 5 5 5 4 6 1 6 4 1 2 3 6 1 5 3 4 3 6 1 4 1 1 1 5 5 5 5 1 3 3 3 4 3 1 3
    ## [73765] 4 2 4 4 5 6 6 6 3 6 5 5 5 5 1 5 5 5 5 5 1 5 2 2 2 2 3 3 4 7 4 3 3 6 3 5
    ## [73801] 3 6 4 4 4 6 3 3 3 3 4 4 1 3 4 5 1 1 3 5 5 1 3 1 5 3 3 6 6 5 5 1 3 5 4 1
    ## [73837] 3 5 4 1 5 2 5 3 3 3 1 3 3 5 5 3 1 2 2 1 6 3 5 4 5 2 2 1 2 5 2 3 1 4 4 1
    ## [73873] 4 4 3 3 6 5 3 4 5 2 5 5 5 6 4 4 5 5 5 5 5 5 5 5 2 5 5 1 1 1 2 4 4 3 6 6
    ## [73909] 3 3 1 3 3 1 1 2 2 2 5 1 1 3 3 4 4 6 6 3 2 2 4 1 1 5 4 2 6 4 5 5 5 5 5 6
    ## [73945] 6 3 3 5 2 5 6 4 5 2 5 3 5 5 5 3 3 5 6 4 5 6 6 3 5 5 1 5 1 5 1 5 7 5 5 5
    ## [73981] 1 3 6 2 2 3 2 3 2 5 5 1 1 2 2 5 2 2 2 2 2 2 2 5 7 2 4 4 4 4 4 3 4 4 1 4
    ## [74017] 4 4 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 3 2 2 2 2 2 2 2 2
    ## [74053] 2 2 2 2 2 5 3 3 4 3 1 3 3 1 6 1 1 1 3 3 3 6 1 5 5 2 2 1 1 3 6 4 6 3 6 3
    ## [74089] 3 3 1 3 5 5 5 5 4 6 5 4 2 5 5 5 5 6 5 5 8 8 4 3 6 4 4 4 4 4 4 6 3 3 6 3
    ## [74125] 6 4 4 1 3 3 6 6 5 4 6 6 4 6 6 6 5 5 5 5 5 5 3 5 5 2 5 5 5 5 5 5 5 5 5 5
    ## [74161] 5 5 5 5 5 2 4 6 4 4 4 1 6 4 6 1 5 1 2 4 5 2 2 2 2 6 2 6 3 6 2 1 5 5 2 4
    ## [74197] 1 4 5 6 3 4 2 2 2 4 5 1 2 5 2 2 2 2 2 5 2 2 1 6 2 4 6 1 2 1 2 2 2 3 3 5
    ## [74233] 4 5 4 4 1 5 3 7 5 3 4 5 5 6 4 4 3 4 6 1 5 5 5 5 5 5 2 1 2 6 6 2 2 2 6 6
    ## [74269] 2 5 6 2 2 2 2 1 2 6 2 2 2 4 4 2 2 6 4 2 1 2 2 2 2 4 3 4 3 5 5 5 5 4 5 5
    ## [74305] 5 5 5 5 5 5 3 5 5 3 5 5 3 5 1 3 6 4 3 4 6 4 4 3 3 4 4 4 5 5 1 5 5 4 5 1
    ## [74341] 5 2 1 5 5 5 5 5 5 5 4 4 6 5 5 4 1 5 1 5 1 3 6 3 3 3 3 2 6 6 5 2 1 3 3 1
    ## [74377] 5 3 5 4 4 5 3 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 2 5 5 1 4 5 5
    ## [74413] 4 5 5 5 3 1 5 6 1 5 4 6 5 6 5 4 5 5 5 4 5 1 5 1 1 1 1 1 1 1 1 1 8 8 4 4
    ## [74449] 5 5 5 5 5 5 5 4 5 5 5 5 5 5 2 3 2 1 2 2 2 2 2 2 2 2 2 4 2 2 6 6 2 2 2 1
    ## [74485] 5 1 4 3 5 1 3 4 2 2 2 3 2 1 4 5 2 2 2 2 2 2 2 2 4 5 2 2 1 2 1 3 3 3 3 1
    ## [74521] 6 1 1 1 3 8 5 5 5 5 3 4 3 1 4 4 4 5 6 6 4 5 6 4 3 4 6 6 6 4 6 6 4 6 6 3
    ## [74557] 6 4 4 6 4 3 2 6 2 3 4 4 4 4 4 3 4 2 4 4 4 4 4 3 4 4 4 1 1 1 1 5 3 3 5 2
    ## [74593] 5 5 5 5 5 5 4 1 2 5 4 2 6 1 1 6 5 4 4 5 5 3 5 5 2 5 4 5 1 1 5 5 5 5 3 5
    ## [74629] 1 5 3 3 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 5 2 1 5 5
    ## [74665] 5 5 5 5 1 1 2 5 5 7 2 2 5 4 6 3 3 6 3 6 3 4 5 3 3 3 6 6 4 1 5 5 6 4 3 6
    ## [74701] 5 5 5 4 5 5 3 3 3 6 6 5 5 5 7 5 3 5 1 2 6 3 5 1 3 8 3 3 5 5 1 1 1 4 5 4
    ## [74737] 3 3 5 5 5 5 5 5 5 5 2 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2
    ## [74773] 5 5 3 3 1 5 5 5 5 3 4 1 6 4 3 5 6 3 1 6 6 6 3 5 6 6 4 6 6 6 6 6 6 3 6 6
    ## [74809] 6 6 5 6 6 6 4 6 5 3 4 6 1 4 4 6 4 4 1 1 3 5 3 3 5 4 4 5 3 5 4 4 4 4 4 6
    ## [74845] 6 3 6 6 3 6 3 4 4 1 4 1 4 3 4 4 5 1 5 2 5 5 5 2 5 3 4 5 5 5 5 3 4 6 5 5
    ## [74881] 5 5 2 4 5 6 3 6 6 4 5 6 6 5 6 6 4 5 4 6 5 1 5 3 1 6 6 6 4 6 5 1 2 6 2 4
    ## [74917] 6 4 6 3 4 6 2 3 4 4 6 4 4 7 3 3 4 2 6 6 5 4 6 4 1 2 7 1 5 2 3 3 5 5 5 5
    ## [74953] 5 5 5 5 5 5 5 5 4 5 3 3 6 3 6 3 4 1 1 3 5 4 5 6 5 1 4 4 5 6 1 4 4 2 3 4
    ## [74989] 5 5 6 6 4 6 4 6 3 3 6 3 3 3 5 1 1 1 4 3 3 5 4 5 5 5 5 5 5 5 2 5 1 4 1 5
    ## [75025] 5 5 5 5 5 1 1 5 5 5 4 5 3 3 4 3 4 4 4 4 4 4 1 2 1 3 6 6 3 6 6 3 5 6 4 4
    ## [75061] 3 3 3 2 6 3 5 5 5 4 4 3 3 1 3 3 5 3 5 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5
    ## [75097] 3 6 6 4 4 4 3 2 1 5 6 5 5 5 2 5 2 5 5 5 2 5 5 4 6 5 5 6 3 5 1 5 3 3 4 2
    ## [75133] 5 5 4 2 2 4 1 1 4 5 6 6 4 3 6 3 6 6 4 6 5 6 1 2 3 4 4 6 4 3 5 5 5 5 5 5
    ## [75169] 5 5 5 6 5 3 4 5 5 3 4 1 3 7 5 5 3 3 4 4 1 3 4 5 5 4 4 3 5 5 6 6 6 4 6 6
    ## [75205] 6 6 6 6 3 5 5 1 1 5 5 5 1 4 4 6 4 4 4 1 6 5 5 5 1 5 4 1 5 6 1 5 5 6 1 5
    ## [75241] 1 5 6 1 4 4 3 3 4 4 3 7 1 1 4 5 5 1 5 4 4 2 1 5 6 5 1 4 5 5 3 4 3 4 5 5
    ## [75277] 5 5 5 5 5 4 5 5 2 5 1 2 5 2 1 5 2 5 2 1 1 5 5 4 2 5 2 5 2 2 5 2 5 2 3 4
    ## [75313] 4 4 3 2 1 5 2 3 5 5 3 2 2 2 2 5 5 7 4 1 1 4 1 4 3 1 1 2 1 1 5 1 1 3 1 3
    ## [75349] 5 3 3 3 5 4 5 4 4 4 5 5 6 5 6 6 5 3 3 1 4 1 3 3 2 1 4 6 7 1 5 1 1 5 5 4
    ## [75385] 3 5 5 2 1 2 5 2 5 2 5 6 5 2 5 5 5 3 5 2 2 4 5 2 5 3 1 2 5 2 2 5 2 5 2 5
    ## [75421] 1 5 6 5 2 5 2 2 5 2 2 5 2 3 2 2 2 2 1 5 5 4 1 1 6 5 4 3 5 2 5 3 3 4 3 4
    ## [75457] 3 5 5 1 2 2 6 2 2 2 6 5 3 2 2 2 2 5 2 2 2 6 2 2 2 2 4 2 2 6 2 4 6 2 2 2
    ## [75493] 5 8 1 4 1 5 1 5 3 3 5 3 5 5 2 7 5 5 2 5 5 5 5 4 5 3 1 5 1 5 5 5 5 5 5 3
    ## [75529] 5 5 5 5 2 2 2 2 2 2 4 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2
    ## [75565] 2 2 2 2 6 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 6 5 2 4 6 6 6 4 6 4 6 6 6 2 6
    ## [75601] 1 4 3 3 5 5 5 5 3 4 5 4 4 5 4 4 4 4 5 5 5 4 3 1 5 5 4 6 6 4 4 3 6 5 6 4
    ## [75637] 6 4 6 6 3 6 6 4 6 6 4 4 7 6 1 6 4 6 6 4 4 4 7 5 5 4 3 4 3 1 5 2 3 5 5 5
    ## [75673] 6 5 4 4 4 4 4 4 4 4 4 4 4 4 1 5 6 6 5 4 5 5 4 2 4 5 4 4 1 7 4 1 3 3 3 4
    ## [75709] 1 1 6 5 1 6 4 5 2 4 2 6 4 4 1 3 3 4 1 1 1 3 5 6 3 5 1 3 4 1 3 5 5 3 5 4
    ## [75745] 1 5 5 3 5 1 5 3 5 5 5 5 5 3 6 6 5 3 4 6 4 6 4 6 4 6 6 6 4 6 1 5 5 5 5 5
    ## [75781] 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 4 5 3 5 5 5 4 4 5 5 4 3 4 7
    ## [75817] 4 1 2 2 2 1 2 2 2 5 2 2 2 5 2 5 5 2 2 2 2 5 2 2 2 2 2 2 2 2 5 5 2 2 5 2
    ## [75853] 2 2 2 2 2 5 2 5 2 1 1 5 3 5 7 1 5 5 5 5 6 6 5 6 6 4 6 4 5 4 5 5 4 1 5 4
    ## [75889] 1 3 5 4 4 1 4 7 3 1 5 2 1 3 1 6 3 1 1 3 7 1 2 5 2 2 1 5 1 1 1 6 3 4 6 5
    ## [75925] 4 7 4 2 3 5 3 4 6 5 6 5 5 5 5 5 5 5 5 5 5 4 7 1 4 3 3 3 5 5 4 1 4 4 4 4
    ## [75961] 3 4 6 1 3 6 4 6 4 6 6 3 4 1 6 6 1 4 5 3 5 5 4 3 2 4 4 4 5 4 4 6 4 6 4 4
    ## [75997] 4 4 5 5 5 5 2 5 5 3 3 5 4 3 1 2 3 1 5 6 5 5 6 4 6 4 3 6 6 6 4 3 3 4 1 1
    ## [76033] 3 3 5 4 5 5 3 6 3 5 5 5 5 5 5 7 4 5 5 1 3 4 5 3 2 6 6 5 6 2 2 4 5 5 4 5
    ## [76069] 4 5 2 6 5 5 4 4 6 6 4 4 4 6 3 3 5 6 6 6 3 4 3 4 3 4 6 5 3 4 6 6 1 1 3 5
    ## [76105] 3 4 6 1 2 2 2 2 3 4 4 4 4 2 4 5 3 4 2 1 4 5 6 5 5 5 5 4 6 4 5 6 4 5 4 2
    ## [76141] 2 2 4 6 6 6 6 3 4 6 4 6 6 4 6 4 6 1 2 2 2 2 2 2 2 2 6 2 4 2 2 2 2 4 2 6
    ## [76177] 2 2 2 5 2 5 2 2 2 2 2 5 6 6 2 6 3 2 1 2 3 3 3 5 5 2 2 1 1 2 3 2 5 2 5 2
    ## [76213] 2 1 2 5 1 1 2 2 6 2 5 5 4 2 5 5 3 3 3 1 1 3 1 3 5 3 3 3 3 4 3 6 6 4 3 3
    ## [76249] 3 2 1 5 3 3 4 1 5 4 3 5 1 1 1 6 1 3 6 6 4 3 3 3 4 1 1 4 1 5 5 4 4 4 2 6
    ## [76285] 6 2 3 3 6 2 4 6 1 1 5 3 1 3 1 1 5 5 1 5 5 5 7 6 6 3 4 5 6 6 4 2 5 5 3 1
    ## [76321] 5 5 5 5 3 6 3 6 5 3 4 4 5 3 6 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [76357] 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 2 5 1 5 5 4 6 6 3 3 6 5 1 5 5 3 5 5
    ## [76393] 5 5 1 5 7 3 5 5 5 2 2 3 3 4 4 6 4 1 6 4 6 5 2 5 6 4 1 4 5 5 6 6 3 4 6 1
    ## [76429] 1 6 3 3 5 3 4 4 3 3 4 4 3 4 5 4 5 4 4 1 4 4 1 1 4 4 3 4 4 4 1 3 3 6 2 4
    ## [76465] 4 4 3 1 4 6 3 3 4 4 4 5 6 7 4 4 5 3 1 1 5 5 5 2 5 5 2 5 3 5 5 5 6 5 5 5
    ## [76501] 5 5 5 4 3 4 5 4 4 3 4 5 4 5 1 3 5 4 2 2 4 1 6 4 6 2 4 4 4 1 4 2 1 5 2 2
    ## [76537] 4 2 4 2 4 2 2 4 3 1 1 3 4 3 6 3 3 3 3 6 3 4 6 3 4 6 4 6 1 3 6 5 3 4 4 6
    ## [76573] 4 6 6 6 3 6 6 3 3 1 4 1 2 3 5 3 3 5 5 3 3 3 6 3 3 5 5 5 4 5 3 6 2 5 6 3
    ## [76609] 5 6 5 1 1 5 4 6 3 4 1 3 3 5 4 4 3 5 6 5 2 3 1 3 5 5 5 5 5 5 5 5 5 5 5 5
    ## [76645] 5 5 5 5 5 5 6 5 5 5 5 5 5 5 6 3 6 6 6 3 2 6 6 6 3 6 3 3 4 1 3 4 4 6 3 4
    ## [76681] 6 6 3 3 3 6 4 3 3 6 6 5 3 3 4 6 6 4 3 4 6 3 4 6 3 6 3 3 3 6 3 4 3 4 3 4
    ## [76717] 4 3 3 3 6 3 3 6 6 4 6 6 6 4 6 4 5 4 6 6 6 3 6 3 6 3 3 3 3 3 5 2 2 3 3 2
    ## [76753] 4 5 5 5 5 1 5 5 3 6 4 4 6 4 6 5 1 4 6 1 3 3 3 1 6 5 1 4 5 2 2 2 2 4 6 4
    ## [76789] 2 3 2 4 2 4 2 6 4 4 3 6 4 4 6 3 4 2 4 6 2 2 4 6 2 6 6 6 6 6 4 4 2 2 2 6
    ## [76825] 2 6 4 1 1 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [76861] 5 5 5 5 1 3 6 5 3 5 5 5 5 5 1 3 6 6 4 6 5 6 3 4 6 6 3 5 3 6 4 6 4 6 5 5
    ## [76897] 6 5 5 2 2 6 3 5 4 5 6 6 6 3 5 5 4 4 4 4 4 4 4 4 4 3 5 3 3 3 4 4 4 3 3 3
    ## [76933] 5 4 4 5 5 3 4 3 3 3 5 3 5 5 5 6 4 6 6 5 4 6 6 3 5 5 5 5 5 2 6 5 2 5 6 4
    ## [76969] 2 3 3 3 4 3 3 3 5 3 3 6 5 5 4 6 5 3 6 1 5 5 3 5 2 2 2 1 2 2 2 2 2 2 2 2
    ## [77005] 2 2 2 2 2 5 2 2 2 2 5 2 2 2 2 1 2 2 1 6 2 2 2 2 2 1 2 2 2 5 2 2 3 5 3 6
    ## [77041] 4 1 6 6 5 3 5 5 2 2 4 2 1 2 2 4 2 6 2 2 4 2 1 5 3 5 1 5 3 2 5 5 5 5 5 5
    ## [77077] 5 5 5 5 5 3 1 2 6 3 1 6 1 3 5 4 6 2 5 2 2 5 4 1 5 1 5 4 4 2 3 1 1 5 6 3
    ## [77113] 3 1 5 3 5 5 5 3 1 6 4 3 4 5 3 3 3 1 6 3 4 4 6 6 4 4 4 4 6 6 6 4 3 3 4 6
    ## [77149] 6 3 6 6 4 6 4 6 1 4 3 6 3 4 6 3 4 5 3 6 3 3 5 4 2 6 2 4 4 2 2 4 2 4 4 2
    ## [77185] 4 5 5 3 3 2 1 5 4 4 6 6 3 6 6 6 4 5 4 6 7 5 5 5 6 4 6 5 5 5 4 6 4 3 3 6
    ## [77221] 6 6 4 4 6 3 5 4 6 6 6 6 1 1 6 5 4 6 1 5 6 6 4 1 6 4 1 2 6 4 6 4 4 3 6 3
    ## [77257] 2 4 4 6 6 5 4 5 3 3 1 5 4 4 5 5 4 5 5 2 2 6 4 4 5 2 2 4 2 4 5 4 3 4 5 1
    ## [77293] 4 4 2 2 3 2 6 3 5 2 4 5 5 4 2 6 4 4 1 5 5 4 5 4 4 5 1 4 4 5 2 7 4 4 4 5
    ## [77329] 5 3 3 1 3 5 5 3 1 1 3 4 4 1 3 4 6 5 1 1 5 5 2 5 5 5 5 5 5 4 5 5 5 4 5 1
    ## [77365] 5 5 6 3 1 4 5 4 6 3 6 5 4 5 3 4 6 1 6 6 4 2 4 3 4 6 6 6 4 6 1 6 6 4 4 3
    ## [77401] 6 2 1 2 6 6 1 1 4 6 6 6 3 4 4 3 2 3 4 4 6 6 4 6 6 3 6 6 4 6 4 4 6 4 4 6
    ## [77437] 4 3 5 6 3 5 3 5 3 5 5 6 2 6 3 2 4 5 4 5 4 6 3 4 1 1 2 5 5 4 5 5 1 5 5 5
    ## [77473] 5 5 2 5 3 1 4 3 6 1 5 4 4 4 4 6 1 3 5 3 5 3 5 3 3 5 3 5 5 1 5 4 4 6 5 3
    ## [77509] 1 3 3 4 4 4 3 3 3 3 4 6 5 4 2 3 5 5 3 3 6 5 5 5 4 7 3 3 3 4 5 3 1 5 1 1
    ## [77545] 4 3 3 4 4 4 6 3 6 3 5 4 5 2 1 5 3 4 6 2 2 3 5 3 5 3 6 6 6 5 4 5 3 5 4 3
    ## [77581] 1 1 3 1 1 3 6 4 6 4 3 6 6 4 4 5 3 3 4 4 4 6 1 1 5 5 5 3 2 1 4 5 5 5 5 5
    ## [77617] 5 5 5 2 5 5 3 5 3 4 5 5 5 5 5 5 5 1 4 4 3 4 5 5 5 5 2 2 2 4 1 4 2 3 3 6
    ## [77653] 1 4 4 3 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [77689] 5 5 5 5 5 5 5 3 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 4 5 5 5 6 5 4 3 5 1 5 5 6
    ## [77725] 6 4 4 3 5 4 5 5 5 4 5 4 6 5 5 4 5 5 4 4 4 5 1 4 5 4 2 4 1 4 2 4 4 4 4 6
    ## [77761] 3 1 3 3 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 4 3 5
    ## [77797] 1 4 4 5 4 3 1 4 3 5 6 5 2 3 6 1 2 6 6 5 6 5 5 5 5 6 5 4 5 1 5 2 2 4 4 6
    ## [77833] 3 1 1 5 5 1 6 5 2 3 3 3 3 5 3 5 6 3 1 5 6 1 3 4 3 4 3 4 1 6 6 1 6 1 1 3
    ## [77869] 6 1 3 3 3 5 4 4 1 4 3 2 5 5 5 3 4 5 4 6 3 4 5 4 3 4 1 4 4 1 3 1 1 3 5 1
    ## [77905] 3 6 1 1 1 1 3 3 1 1 3 5 4 5 5 5 2 7 2 5 4 5 1 1 5 4 4 3 4 4 1 4 1 3 1 5
    ## [77941] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5
    ## [77977] 5 5 5 5 5 5 5 5 5 6 5 5 4 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6
    ## [78013] 5 5 5 5 5 5 5 3 3 5 5 3 5 5 5 4 3 2 1 4 4 3 4 3 4 4 5 5 2 5 1 5 5 2 5 5
    ## [78049] 5 5 5 5 5 5 5 2 2 2 5 5 5 5 4 5 4 5 2 5 5 5 4 2 5 5 5 5 8 5 3 5 3 3 3 4
    ## [78085] 5 4 4 4 2 4 3 6 3 4 3 4 6 6 6 7 2 2 4 1 1 1 1 5 4 1 3 2 4 1 6 6 6 3 2 3
    ## [78121] 6 4 6 2 6 6 3 2 4 5 1 3 5 1 4 4 6 6 4 4 2 2 5 1 1 4 1 4 5 5 2 5 5 5 1 5
    ## [78157] 5 2 3 3 1 4 3 3 2 3 4 3 3 4 1 1 1 2 2 5 5 5 5 2 2 4 4 4 3 5 5 5 5 5 5 5
    ## [78193] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5
    ## [78229] 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 4 5 5 4 5 4 5 4 1 6 6 3 3 4 4 4 6 6 4 3
    ## [78265] 5 4 1 3 6 6 6 1 5 1 6 6 5 1 3 1 5 6 5 2 2 4 2 3 2 2 2 6 6 5 4 3 5 3 4 4
    ## [78301] 5 5 5 5 5 1 1 5 1 3 1 5 5 1 4 1 5 2 3 3 5 5 4 3 3 3 2 2 6 4 2 2 6 2 2 2
    ## [78337] 2 2 2 2 2 2 2 2 6 4 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2
    ## [78373] 2 2 2 2 2 2 2 4 2 6 2 2 2 2 2 6 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2
    ## [78409] 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 4 2 2 6 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 6
    ## [78445] 2 2 2 6 2 2 1 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 6 4 4 3 2 2 4 5 3 1 3 5 5
    ## [78481] 2 3 6 4 5 3 5 7 5 2 1 3 5 4 2 4 4 5 3 6 6 4 5 4 5 4 5 1 2 6 1 1 1 2 3 3
    ## [78517] 3 2 3 3 4 6 5 2 5 2 2 3 6 5 4 4 5 6 6 5 5 1 6 4 1 3 4 6 6 5 5 5 3 5 5 4
    ## [78553] 3 1 2 6 4 6 2 6 6 4 6 4 6 3 6 4 6 5 3 5 5 5 5 3 1 2 5 5 3 2 6 6 4 4 4 3
    ## [78589] 2 6 4 6 5 4 2 5 5 1 5 1 6 4 4 6 3 4 6 3 5 3 4 4 7 5 3 5 5 5 5 5 5 3 4 4
    ## [78625] 5 3 1 4 5 2 5 4 2 5 2 2 3 4 2 2 4 2 2 2 2 4 4 4 4 4 4 2 1 5 5 1 3 5 5 5
    ## [78661] 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 2 5 5 5 5 3 3 5 5 4
    ## [78697] 3 5 5 4 3 5 3 5 3 5 3 1 4 4 1 6 4 1 4 1 1 4 4 4 3 1 1 1 5 1 5 5 5 5 5 5
    ## [78733] 4 5 5 5 5 6 6 4 2 6 2 2 3 3 3 3 3 5 4 3 6 6 6 3 6 2 4 6 6 6 2 6 6 5 5 2
    ## [78769] 5 4 4 4 2 2 2 4 5 2 4 5 5 5 5 4 2 5 5 6 6 2 2 2 2 2 2 2 2 4 6 6 6 6 6 4
    ## [78805] 5 4 5 6 5 5 4 3 4 6 6 3 3 3 4 3 4 3 6 2 4 2 4 4 4 4 3 4 2 6 4 6 4 4 4 4
    ## [78841] 2 4 4 5 2 4 2 1 4 3 1 3 3 3 3 3 3 4 2 4 3 3 4 4 2 4 3 4 6 1 3 6 3 6 1 5
    ## [78877] 4 4 3 3 3 3 3 6 6 1 6 2 1 2 6 6 4 3 6 6 3 1 5 1 6 6 4 3 6 3 3 4 6 3 5 5
    ## [78913] 1 1 5 6 5 7 4 5 3 5 5 5 4 5 4 1 5 4 5 5 6 3 4 7 4 4 7 1 1 1 3 3 1 7 5 5
    ## [78949] 5 2 5 6 2 5 5 6 6 5 3 4 3 3 5 3 3 2 3 4 5 5 5 3 1 1 4 6 7 3 2 5 6 3 6 4
    ## [78985] 4 6 6 1 5 1 5 1 1 1 4 4 3 6 5 1 3 2 6 1 4 4 1 1 3 5 4 3 4 5 3 6 6 6 6 5
    ## [79021] 1 5 5 4 4 4 4 4 5 4 1 5 1 6 5 5 5 5 5 3 5 3 5 5 5 2 3 3 4 4 6 4 5 3 1 4
    ## [79057] 6 5 6 5 3 5 3 3 3 3 3 2 3 3 6 6 4 4 5 4 4 3 6 3 4 3 6 4 4 1 4 6 4 6 2 3
    ## [79093] 1 5 2 2 5 5 5 5 5 5 5 5 5 6 5 3 3 1 4 1 1 3 6 4 5 4 5 5 5 5 4 5 2 5 5 5
    ## [79129] 5 5 5 5 2 5 5 5 5 3 5 5 5 4 5 3 5 6 6 2 1 4 5 5 1 5 3 5 1 3 4 4 3 3 5 4
    ## [79165] 3 2 6 5 3 6 5 5 5 5 3 5 5 3 5 3 6 4 5 3 4 5 4 4 4 4 3 4 4 4 4 4 3 3 3 4
    ## [79201] 1 3 5 5 3 6 4 6 6 1 2 4 4 6 4 2 4 6 1 2 4 5 5 5 3 2 2 5 1 5 5 3 3 4 4 2
    ## [79237] 4 4 4 4 4 6 6 1 6 5 6 6 3 1 5 3 3 6 3 3 3 4 1 3 4 4 4 4 2 4 3 1 4 3 4 4
    ## [79273] 2 5 5 6 7 6 2 4 4 3 5 5 5 5 3 1 5 5 3 3 3 3 3 4 5 5 5 5 2 5 5 5 2 2 5 5
    ## [79309] 5 5 5 2 3 5 2 5 2 2 5 5 5 2 2 2 5 5 2 5 5 2 2 5 5 5 2 5 5 5 5 5 5 5 2 5
    ## [79345] 2 5 5 5 5 2 2 5 5 2 5 5 8 5 5 2 2 5 5 2 5 2 5 2 2 5 5 2 5 2 2 5 2 5 5 5
    ## [79381] 5 2 2 5 5 5 5 5 5 2 5 5 5 5 2 5 5 2 5 5 5 5 5 2 2 5 5 2 5 2 5 2 2 5 2 5
    ## [79417] 2 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 2 5 5 5 5 2 4 2 5 2 5 2 5 5 5 5 5 5
    ## [79453] 5 5 3 4 3 3 3 3 6 3 3 3 3 4 4 5 3 1 5 4 5 4 1 3 1 5 3 4 5 5 1 1 6 5 3 2
    ## [79489] 6 5 4 5 2 5 5 1 2 5 5 1 3 3 4 6 4 3 4 2 3 4 1 3 4 8 2 1 2 5 1 3 4 5 1 4
    ## [79525] 5 5 5 3 5 5 6 2 4 4 4 1 2 4 4 4 4 6 6 2 5 5 5 2 2 2 4 5 2 2 3 5 1 6 1 7
    ## [79561] 3 5 5 5 1 5 5 5 5 5 5 3 3 3 4 4 4 4 6 3 4 4 1 4 6 2 4 2 5 4 6 5 4 3 3 1
    ## [79597] 2 4 4 4 4 6 6 4 5 4 6 4 4 1 5 1 3 1 4 3 4 3 4 3 5 4 3 5 3 4 1 5 3 5 5 1
    ## [79633] 5 4 3 1 1 6 6 6 6 3 5 5 5 5 2 2 4 2 6 2 4 2 3 2 4 8 1 5 2 2 2 5 4 2 3 6
    ## [79669] 5 5 3 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 2 1 4 3 6 4 2 4 3 4 4 3 4 4 4
    ## [79705] 1 4 2 6 2 2 2 6 1 2 5 2 2 2 4 3 2 6 2 2 3 3 4 5 1 7 5 4 1 1 2 3 4 2 6 2
    ## [79741] 6 6 3 4 3 5 5 3 3 6 4 1 4 3 5 5 3 5 2 5 5 2 5 2 2 4 5 5 2 2 5 2 2 5 5 5
    ## [79777] 5 5 5 2 3 4 6 5 4 6 6 4 1 6 5 2 2 5 2 1 2 2 2 2 2 2 3 2 2 2 6 2 2 4 4 2
    ## [79813] 2 3 4 2 2 4 5 2 4 2 3 2 5 5 4 4 4 4 6 4 4 4 6 6 4 3 4 3 3 4 5 3 6 1 3 3
    ## [79849] 4 4 6 8 3 4 4 5 6 5 2 3 6 6 5 2 3 5 5 5 1 5 1 4 5 3 3 5 4 2 4 6 3 3 3 4
    ## [79885] 3 4 3 4 4 4 5 5 5 5 5 3 5 2 2 5 5 5 4 2 5 5 5 5 2 5 4 5 1 5 5 5 5 5 2 2
    ## [79921] 2 5 5 5 5 2 2 2 2 5 5 5 5 2 5 5 2 3 5 5 5 5 5 5 5 5 5 5 5 5 5 1 1 1 2 5
    ## [79957] 5 5 5 5 2 2 5 5 5 3 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 2 6
    ## [79993] 5 4 4 3 3 1 4 4 4 5 1 5 1 4 3 1 3 3 4 1 3 3 3 3 5 4 3 2 2 2 2 2 3 2 2 2
    ## [80029] 2 3 1 5 5 5 2 5 2 3 3 4 5 5 4 5 5 1 5 1 3 3 4 3 3 3 4 4 5 1 5 6 3 5 5 3
    ## [80065] 5 5 5 4 4 6 2 4 4 6 4 6 4 4 6 6 6 6 3 2 4 6 2 6 2 3 6 2 6 1 4 1 1 5 5 5
    ## [80101] 4 5 3 5 5 5 5 5 4 6 1 3 3 2 6 1 4 2 3 2 4 3 8 8 6 2 6 4 4 4 6 1 4 5 4 6
    ## [80137] 1 6 5 2 4 6 5 5 1 5 5 5 5 3 3 5 5 5 3 3 4 5 5 5 1 5 1 1 2 5 1 1 5 3 1 3
    ## [80173] 3 5 5 1 1 1 6 6 4 5 6 6 6 6 1 1 5 5 5 5 4 4 4 6 5 5 5 5 6 5 6 5 5 1 4 6
    ## [80209] 3 5 1 1 6 3 6 4 6 3 6 3 4 1 3 4 4 3 4 6 5 3 3 6 4 4 5 5 5 5 3 6 4 3 1 6
    ## [80245] 3 4 6 6 3 6 4 2 6 4 6 6 6 4 3 4 6 5 3 2 3 5 5 2 4 3 4 3 6 6 4 1 1 3 1 3
    ## [80281] 1 5 5 5 2 5 2 5 5 4 5 2 2 2 5 5 5 2 2 5 5 6 2 5 5 3 5 5 5 2 3 6 4 4 6 6
    ## [80317] 6 3 6 3 3 6 4 6 3 6 3 4 6 6 6 4 1 1 4 4 4 1 3 1 4 2 2 2 2 2 2 2 2 6 2 2
    ## [80353] 3 3 3 1 2 4 6 4 6 4 4 4 4 3 4 7 4 5 3 5 3 6 5 4 5 5 5 2 1 4 4 5 6 4 3 1
    ## [80389] 4 6 4 4 5 3 3 6 6 4 4 4 3 4 4 2 1 4 4 4 4 4 3 6 5 4 5 4 3 5 5 4 6 5 5 4
    ## [80425] 5 5 5 5 5 5 6 6 5 5 5 4 5 4 7 2 1 1 2 3 1 7 4 1 3 5 2 2 2 2 2 4 2 2 2 2
    ## [80461] 2 3 3 3 3 3 3 5 1 3 5 6 1 5 1 5 5 4 3 4 4 4 4 4 5 5 6 6 4 6 4 6 4 6 4 6
    ## [80497] 6 6 4 3 4 4 5 1 4 4 5 5 2 5 5 5 5 5 5 5 5 2 5 3 4 4 2 5 2 6 3 4 4 4 6 4
    ## [80533] 6 4 6 1 1 3 5 5 3 6 4 3 5 6 5 5 1 6 5 3 6 4 4 3 2 1 3 1 4 3 6 4 6 6 6 4
    ## [80569] 4 6 3 6 6 3 4 4 6 6 5 6 4 1 3 3 5 6 6 1 4 5 5 1 3 4 2 3 3 3 4 2 4 5 5 1
    ## [80605] 5 1 5 3 5 5 3 3 3 5 4 4 5 4 5 3 5 5 4 1 1 3 3 2 4 4 4 3 6 6 4 4 6 3 4 6
    ## [80641] 6 4 3 3 1 3 5 1 5 1 1 4 4 5 5 3 1 4 1 5 3 3 1 2 2 5 3 6 3 5 5 3 4 5 5 1
    ## [80677] 1 1 3 6 5 6 4 6 6 4 3 3 3 2 6 2 6 4 4 4 6 2 4 6 6 1 2 4 4 3 6 3 6 5 4 1
    ## [80713] 5 5 6 3 5 1 6 4 6 3 3 1 5 2 4 5 5 2 1 5 1 1 5 3 5 5 5 1 4 2 1 5 4 4 4 3
    ## [80749] 1 1 1 1 4 2 3 4 1 4 6 6 6 3 5 1 5 5 1 6 4 5 3 2 3 5 5 6 3 5 2 3 6 5 6 5
    ## [80785] 5 5 5 5 4 5 5 5 4 5 3 5 5 2 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 2 5 5 5 5 5 5
    ## [80821] 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 4 3 2 3 5 3
    ## [80857] 1 5 3 4 3 3 4 5 5 2 5 3 5 6 4 6 6 4 1 6 3 1 7 7 4 4 4 4 5 1 4 4 7 4 3 5
    ## [80893] 4 5 5 1 5 5 5 3 5 5 5 5 5 5 5 5 5 3 4 1 4 1 3 4 2 4 2 4 4 4 2 2 4 6 3 6
    ## [80929] 6 2 5 2 2 5 1 3 6 3 3 6 3 4 4 3 4 4 1 3 1 4 5 3 4 3 1 8 6 5 3 3 6 4 5 4
    ## [80965] 4 4 4 4 2 2 5 1 1 5 4 4 6 4 2 4 2 6 4 4 5 4 3 5 1 6 3 1 4 4 4 5 3 3 2 4
    ## [81001] 5 6 3 3 5 5 5 5 5 5 2 1 2 5 5 5 5 2 5 5 4 5 2 4 3 6 1 4 3 1 5 6 4 5 5 3
    ## [81037] 5 4 5 5 4 4 4 5 6 4 1 1 4 6 6 1 3 1 5 5 1 5 5 2 5 5 5 5 4 5 5 5 6 4 6 5
    ## [81073] 1 4 4 1 1 4 1 4 5 1 4 3 3 3 4 3 4 4 1 2 6 5 3 5 5 1 5 5 5 3 1 3 3 2 5 2
    ## [81109] 5 1 5 1 2 2 2 3 4 5 6 3 4 5 1 4 4 4 6 2 2 1 1 4 4 1 4 4 1 5 5 1 3 3 5 3
    ## [81145] 5 4 1 4 3 1 2 2 3 1 2 1 2 5 3 5 3 6 4 3 5 3 4 4 4 4 4 3 1 5 3 1 2 1 6 4
    ## [81181] 4 6 6 6 4 3 4 6 4 4 6 6 4 4 3 3 2 2 3 6 5 2 7 1 5 3 4 4 8 1 5 5 5 5 5 5
    ## [81217] 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5
    ## [81253] 5 5 5 5 5 5 5 6 6 6 6 4 4 4 6 4 6 4 5 4 5 1 5 4 2 5 3 5 4 5 3 3 3 3 4 3
    ## [81289] 4 3 6 6 6 4 3 6 1 3 4 3 5 3 3 3 4 6 6 6 6 6 4 4 4 6 6 6 6 6 5 6 5 2 4 6
    ## [81325] 5 2 4 6 3 3 3 6 6 6 6 6 4 6 6 6 6 5 6 6 6 4 4 1 4 4 6 1 5 3 5 6 4 3 8 2
    ## [81361] 3 5 6 4 3 4 1 4 1 4 6 3 1 1 1 1 5 3 3 5 3 5 5 5 5 5 5 5 5 5 5 5 5 3 2 4
    ## [81397] 3 2 1 3 2 6 4 2 5 3 5 6 4 6 6 4 3 6 4 6 5 4 3 1 6 3 1 2 4 6 4 4 6 6 6 4
    ## [81433] 4 6 4 6 4 6 6 3 3 6 1 5 7 4 2 5 3 2 3 2 3 2 2 5 4 3 4 3 3 3 5 4 6 5 5 5
    ## [81469] 3 6 6 6 6 6 4 6 6 2 6 6 6 6 4 6 1 2 4 3 4 3 3 1 3 1 1 5 5 1 2 4 3 2 2 1
    ## [81505] 3 4 1 1 6 6 3 5 1 2 3 1 4 4 3 3 4 6 2 4 1 6 2 4 6 4 4 3 3 5 5 5 5 2 5 5
    ## [81541] 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 4 6 6 6 6 3 6 6 6 6 6 3 4 3 4 3 1 3 5
    ## [81577] 5 4 5 5 5 1 2 3 3 4 4 4 5 1 1 1 3 4 4 4 5 5 6 1 1 5 1 5 3 4 3 6 3 3 3 1
    ## [81613] 6 3 1 1 1 5 3 5 1 4 3 2 6 4 3 5 5 1 7 3 3 1 2 7 2 4 2 2 3 2 3 1 5 5 5 5
    ## [81649] 5 5 5 5 1 5 5 5 3 3 4 3 6 3 3 4 5 5 6 6 5 4 3 1 5 3 5 5 5 5 5 4 5 4 6 4
    ## [81685] 1 1 3 3 4 5 3 5 5 2 1 5 1 5 4 7 2 4 2 3 5 5 3 1 5 1 2 3 1 5 1 4 4 2 7 5
    ## [81721] 3 5 4 5 5 6 6 6 3 5 4 3 3 3 3 3 3 6 6 6 6 5 6 4 3 6 6 6 6 6 4 6 6 3 3 4
    ## [81757] 3 4 3 6 4 4 4 6 4 4 6 6 4 6 3 3 3 4 4 6 6 3 2 2 2 4 3 3 4 6 1 4 6 1 1 4
    ## [81793] 2 2 6 5 2 1 3 3 5 1 2 6 6 6 2 4 6 6 3 3 6 2 4 4 4 3 3 4 3 4 5 3 3 4 5 4
    ## [81829] 4 3 2 1 3 1 6 4 3 4 3 6 3 4 4 3 4 6 6 6 4 3 2 4 3 2 5 2 1 2 4 1 5 4 5 5
    ## [81865] 3 3 6 4 4 6 6 5 5 6 1 4 5 6 5 5 4 5 3 2 3 1 1 4 4 6 4 4 1 3 1 2 2 6 2 3
    ## [81901] 3 6 2 6 4 2 5 2 4 1 5 4 7 4 5 3 7 5 1 3 4 4 4 4 4 4 1 3 1 5 4 5 5 1 5 5
    ## [81937] 5 5 5 5 3 3 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 4 2 4 4 2 2 2 6 2 2 2 2
    ## [81973] 5 2 2 2 2 3 5 1 4 4 3 1 2 2 5 5 4 1 5 5 4 3 3 4 3 1 6 5 1 5 5 5 1 5 4 5
    ## [82009] 4 3 7 4 3 5 4 1 2 1 5 5 4 3 3 2 5 2 2 2 3 4 3 1 2 1 4 5 1 3 3 6 6 3 3 6
    ## [82045] 6 6 3 4 3 6 6 3 3 4 2 1 3 4 3 3 6 3 3 1 5 1 3 5 5 3 1 1 5 5 5 5 4 1 5 5
    ## [82081] 1 5 5 5 4 5 4 3 4 6 6 4 6 4 5 4 1 1 3 6 3 6 2 5 4 4 1 3 3 3 4 3 3 3 3 1
    ## [82117] 4 6 6 4 4 6 5 4 3 3 3 6 4 6 6 6 6 4 4 4 3 6 2 4 5 2 6 3 4 6 1 2 3 4 4 2
    ## [82153] 6 3 5 4 6 4 6 4 3 4 3 6 3 6 4 4 4 3 5 4 4 6 5 6 3 6 4 6 3 6 4 6 3 6 6 4
    ## [82189] 4 4 6 4 4 3 5 4 4 4 4 3 3 5 5 5 5 5 5 2 5 5 5 3 2 5 5 3 2 3 5 5 1 5 5 7
    ## [82225] 1 4 3 1 3 1 2 2 1 1 4 7 4 5 5 5 5 4 5 4 1 5 1 7 5 5 5 5 5 3 5 5 5 5 2 2
    ## [82261] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3 6 3 3 4 4 6 6 4 3 5 3 1 1 1 2 2 3
    ## [82297] 2 1 5 1 3 6 6 5 5 3 5 5 4 4 4 5 5 3 4 4 6 3 3 4 3 4 4 2 6 5 5 5 5 2 2 6
    ## [82333] 5 3 6 4 6 4 4 6 1 5 1 5 1 4 4 4 6 6 6 3 4 4 3 6 4 6 1 6 4 4 4 3 6 4 1 4
    ## [82369] 6 5 2 3 3 6 6 6 4 1 6 6 4 3 3 6 3 6 2 4 6 6 6 6 3 3 6 6 6 6 6 6 6 6 4 3
    ## [82405] 4 4 4 6 3 4 3 6 3 6 4 6 6 4 4 3 6 4 6 4 4 4 4 6 3 6 3 6 6 3 6 4 6 6 6 6
    ## [82441] 4 4 3 3 4 4 4 6 6 6 6 4 4 6 6 4 6 4 4 4 4 6 6 3 6 6 4 4 6 6 6 4 4 4 6 6
    ## [82477] 6 6 6 6 3 3 5 6 3 2 4 5 4 4 2 4 4 4 6 6 4 4 3 4 6 6 6 6 3 3 4 2 2 5 1 1
    ## [82513] 5 2 2 6 5 6 3 4 3 1 3 1 3 4 3 3 6 4 4 6 4 6 6 1 7 6 5 3 5 5 4 5 5 5 5 5
    ## [82549] 5 5 2 1 5 4 5 5 5 1 4 1 1 1 6 3 5 6 1 2 2 5 6 3 5 3 3 4 3 6 5 1 4 3 5 3
    ## [82585] 2 2 6 2 2 4 5 3 1 4 6 6 4 3 4 3 5 5 5 4 3 3 5 1 4 4 4 6 4 6 6 6 3 6 5 6
    ## [82621] 5 4 4 6 2 5 5 4 6 3 4 4 6 6 3 4 4 6 6 2 4 2 5 1 6 6 6 6 6 6 4 1 5 1 2 4
    ## [82657] 4 4 2 4 6 4 5 5 3 5 2 4 4 4 1 1 4 4 2 2 4 4 4 3 4 3 3 3 4 3 4 3 3 4 6 4
    ## [82693] 6 3 4 4 6 3 4 1 1 1 3 2 2 2 2 2 2 7 4 2 2 2 2 2 2 1 2 1 5 5 5 5 1 2 4 5
    ## [82729] 6 5 5 1 1 4 3 5 2 1 5 5 1 1 4 5 5 5 1 3 4 3 4 5 4 5 5 4 5 5 5 5 2 5 5 5
    ## [82765] 5 5 5 3 5 5 5 5 5 4 6 5 5 5 5 6 5 3 4 4 3 3 4 3 1 5 1 3 1 1 1 3 5 2 2 5
    ## [82801] 5 6 6 6 3 1 4 4 6 2 2 2 4 4 4 6 4 2 2 2 2 1 3 5 1 3 3 1 3 3 4 3 4 4 6 6
    ## [82837] 6 6 6 4 6 6 3 6 3 4 4 5 4 4 4 4 3 5 3 3 4 4 6 5 5 5 5 7 6 4 4 5 2 5 5 4
    ## [82873] 1 2 4 5 3 2 4 3 4 4 3 4 4 3 5 6 4 6 4 4 6 6 2 4 6 6 6 3 4 4 4 2 2 2 2 6
    ## [82909] 5 5 2 3 2 4 2 4 6 4 4 6 4 2 2 2 6 4 4 6 6 4 6 4 2 6 2 2 2 6 2 4 4 2 2 6
    ## [82945] 2 2 6 2 3 4 6 4 6 2 3 3 6 5 2 4 5 3 5 5 5 6 4 5 2 4 4 1 2 2 2 2 6 4 5 2
    ## [82981] 3 4 5 4 5 5 3 3 3 4 4 2 6 4 3 4 4 4 6 4 2 7 4 4 5 5 5 4 1 4 5 5 4 2 3 3
    ## [83017] 5 5 5 4 5 8 4 1 1 5 1 5 5 2 8 5 3 5 3 1 5 3 3 3 2 5 5 5 5 3 1 1 3 1 3 5
    ## [83053] 5 5 2 2 5 5 6 6 2 4 4 1 4 6 4 4 6 5 4 4 6 4 3 3 3 2 1 1 1 4 6 3 6 3 3 3
    ## [83089] 6 4 4 7 6 4 6 6 3 5 5 3 1 3 3 4 3 6 6 3 1 3 5 5 5 6 3 4 3 3 4 5 4 5 1 4
    ## [83125] 3 4 5 3 1 3 3 5 1 2 4 7 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5
    ## [83161] 5 3 3 2 3 2 4 6 2 5 2 5 5 3 3 3 5 5 1 2 5 5 2 5 6 5 4 5 5 6 5 3 1 3 3 4
    ## [83197] 3 6 4 4 5 3 1 4 4 4 6 3 4 2 4 4 4 4 4 4 5 5 5 6 6 1 4 4 6 6 6 4 6 4 4 6
    ## [83233] 5 4 4 6 6 4 6 4 6 6 5 5 6 5 3 3 6 1 4 6 3 2 3 3 5 2 4 5 4 4 6 5 4 6 4 4
    ## [83269] 6 5 4 4 3 1 5 2 4 5 5 1 2 2 4 2 4 2 2 6 2 7 2 3 6 3 6 4 3 3 6 3 4 6 4 4
    ## [83305] 4 3 3 6 4 6 6 4 5 4 6 6 1 3 6 2 3 5 5 5 5 7 4 4 5 2 2 5 1 5 5 1 1 4 4 3
    ## [83341] 4 3 1 5 5 3 1 5 5 4 4 5 4 6 5 3 2 4 2 6 3 5 2 2 3 6 4 6 4 4 6 3 4 4 3 5
    ## [83377] 5 4 3 5 5 5 5 5 5 5 5 5 1 3 6 5 4 5 4 1 1 4 5 4 4 4 2 2 5 5 5 5 3 5 5 3
    ## [83413] 5 5 5 5 4 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 6 5 4 5 1 4 6 4 6 4 3 5 2 5 5 4
    ## [83449] 4 5 4 2 4 4 1 6 5 5 5 3 3 6 3 6 4 6 6 3 4 6 5 4 4 4 6 1 6 6 6 1 6 5 4 5
    ## [83485] 3 5 5 2 6 5 5 5 6 5 4 5 5 5 5 5 5 6 5 5 3 4 6 4 6 4 6 4 6 6 5 3 6 4 4 6
    ## [83521] 6 4 4 5 5 4 4 5 3 4 3 4 5 3 6 3 5 5 3 6 5 5 3 5 5 5 6 5 5 5 5 5 3 4 1 3
    ## [83557] 3 4 1 6 3 4 1 6 1 1 1 1 1 1 2 5 1 4 3 3 1 1 5 1 1 2 2 6 6 6 4 4 2 6 2 6
    ## [83593] 6 2 4 2 2 2 5 5 5 5 1 2 5 2 2 5 2 5 1 4 4 5 5 3 5 4 5 5 4 4 5 4 4 4 4 4
    ## [83629] 4 4 6 4 4 4 6 4 3 4 4 5 3 4 4 4 3 3 4 4 3 5 5 4 2 5 5 5 5 5 3 5 5 5 4 3
    ## [83665] 4 4 5 4 6 6 6 5 5 4 4 3 6 5 3 6 4 5 3 4 4 7 3 5 4 3 3 4 5 7 5 5 5 5 5 5
    ## [83701] 5 5 3 5 5 5 5 5 5 5 5 5 6 3 5 3 1 4 6 6 1 3 2 4 4 4 5 2 2 2 2 2 1 2 4 2
    ## [83737] 4 2 2 4 3 2 2 5 4 5 3 5 7 4 3 5 6 6 5 4 6 6 4 3 1 5 8 3 3 3 2 2 2 5 2 5
    ## [83773] 2 5 5 2 2 5 5 5 5 5 5 5 5 5 5 2 5 2 5 5 2 5 5 5 5 1 1 6 6 1 1 1 3 1 4 3
    ## [83809] 5 3 2 5 4 6 1 4 5 4 1 1 4 4 5 1 5 5 4 5 4 5 3 3 4 4 6 6 3 6 6 6 3 2 4 4
    ## [83845] 5 6 2 3 2 5 1 2 2 5 1 7 6 5 4 3 3 2 2 4 4 2 3 5 1 4 2 2 5 5 5 5 5 2 2 5
    ## [83881] 5 6 4 5 5 6 4 1 3 2 3 5 5 5 2 6 5 5 5 6 5 4 5 2 5 3 4 5 2 2 1 2 2 5 1 5
    ## [83917] 3 1 3 5 3 2 5 5 5 4 2 5 5 2 3 3 5 4 4 1 6 5 6 1 5 1 5 5 3 4 6 2 1 3 2 1
    ## [83953] 2 4 1 5 3 5 5 5 3 1 2 1 5 4 3 4 6 6 3 1 4 4 4 3 4 2 2 6 3 2 5 4 3 3 6 6
    ## [83989] 4 2 6 4 4 6 3 4 7 4 5 3 5 5 2 5 3 4 4 7 3 3 4 3 5 5 5 5 5 5 5 5 5 5 5 5
    ## [84025] 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [84061] 5 5 5 6 5 5 3 3 3 4 6 3 5 3 5 3 3 3 5 5 1 4 5 1 3 3 5 5 4 4 3 3 6 6 2 6
    ## [84097] 2 2 2 2 2 2 2 5 2 2 2 2 6 4 3 6 3 4 5 2 4 4 1 5 4 5 4 5 3 3 3 1 1 5 5 5
    ## [84133] 5 4 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 4 6 4 1 4 4 4 6 6 1
    ## [84169] 5 4 4 6 1 2 4 4 4 4 3 6 4 5 4 4 4 2 3 6 4 3 3 6 3 4 1 1 5 4 5 5 5 4 5 5
    ## [84205] 5 5 5 4 3 5 5 5 5 1 6 4 4 7 6 3 4 6 3 6 4 6 4 6 4 6 4 4 4 5 5 5 5 6 3 6
    ## [84241] 6 1 2 2 4 6 4 4 5 4 3 1 4 6 6 6 3 3 3 3 5 5 5 6 1 4 5 1 6 3 3 4 3 6 4 6
    ## [84277] 3 5 2 3 4 2 4 6 3 3 4 3 2 5 1 7 5 5 5 5 3 3 2 6 5 2 1 4 5 5 2 5 5 3 2 4
    ## [84313] 1 3 4 4 4 4 3 5 4 3 3 4 4 3 3 3 3 3 4 5 5 1 1 4 6 4 3 5 4 4 5 4 2 2 2 4
    ## [84349] 5 2 2 2 1 2 5 1 2 2 5 2 5 1 2 2 4 5 6 4 4 5 5 6 4 4 1 7 4 3 6 3 5 4 5 5
    ## [84385] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 2 5 5 5 5 5 5 5 5 6 4 5 5 5 3 5
    ## [84421] 3 1 3 1 6 1 1 1 5 3 3 4 4 5 6 4 5 3 6 6 4 4 5 4 6 5 5 1 5 1 5 5 3 3 3 1
    ## [84457] 1 8 2 3 1 2 4 3 3 6 6 3 3 3 3 4 6 1 3 5 4 6 4 6 4 5 4 4 2 5 4 4 8 3 2 3
    ## [84493] 2 5 2 4 5 5 6 5 5 5 4 6 6 4 3 1 5 4 4 3 4 2 3 3 5 3 4 5 4 3 1 2 2 2 5 3
    ## [84529] 2 5 6 2 2 4 2 2 1 6 3 1 5 5 5 5 5 3 5 5 5 4 4 5 4 5 1 1 6 3 4 3 6 4 4 3
    ## [84565] 4 2 4 4 2 6 5 4 2 6 5 6 2 3 6 5 3 1 3 4 4 5 5 8 5 5 3 1 1 3 5 5 3 5 5 6
    ## [84601] 3 5 2 1 3 1 1 2 1 5 6 2 3 5 1 5 1 4 3 6 4 4 6 3 3 5 4 4 5 6 6 5 4 6 5 5
    ## [84637] 5 5 3 1 4 6 6 6 4 3 1 5 3 4 4 6 4 4 4 3 5 3 2 5 3 4 5 5 6 5 4 5 5 2 5 5
    ## [84673] 5 2 5 2 5 5 4 3 2 5 2 4 2 2 4 5 2 2 2 6 2 6 2 2 4 2 5 2 2 2 4 6 2 2 5 5
    ## [84709] 5 4 7 4 4 6 5 6 6 4 4 5 4 6 3 4 6 6 5 5 4 4 3 5 5 1 3 4 4 2 5 3 4 3 4 6
    ## [84745] 4 4 3 4 6 6 4 6 6 3 6 4 4 6 4 6 6 6 3 5 3 5 5 5 3 5 1 3 6 1 3 1 6 4 1 6
    ## [84781] 1 6 4 4 4 4 5 3 3 6 4 1 5 5 5 5 5 5 5 3 5 5 5 5 6 4 6 4 3 5 3 6 3 5 3 1
    ## [84817] 4 4 3 3 5 5 1 5 5 4 6 4 4 6 6 4 4 6 4 6 4 6 6 3 4 4 4 6 5 6 1 4 6 6 4 6
    ## [84853] 3 4 4 3 6 4 4 4 5 1 5 4 3 3 3 3 5 6 6 3 5 4 4 2 1 5 3 4 3 3 4 2 1 3 5 5
    ## [84889] 3 5 4 3 4 3 1 4 3 3 3 5 4 4 4 3 4 3 3 5 4 4 3 5 3 3 5 1 1 3 3 1 4 7 4 5
    ## [84925] 3 3 3 5 3 5 6 7 3 3 3 6 6 6 4 6 4 4 6 3 1 5 5 4 4 1 6 2 2 4 2 1 5 2 2 2
    ## [84961] 2 2 2 2 5 2 2 2 2 2 2 2 2 4 5 4 2 2 2 2 2 2 2 5 5 2 2 2 2 2 5 2 2 2 2 6
    ## [84997] 2 2 2 4 5 5 5 2 5 5 2 4 2 5 5 5 3 1 4 5 3 3 5 2 2 3 3 3 5 3 4 3 2 2 6 2
    ## [85033] 4 3 6 5 5 2 1 1 1 4 1 1 5 3 5 6 4 3 3 4 6 6 3 3 1 3 3 5 8 2 2 2 2 2 3 4
    ## [85069] 3 5 5 5 5 6 5 5 5 3 5 4 6 5 5 5 1 1 3 4 3 4 1 3 4 1 7 3 1 3 3 4 7 1 1 4
    ## [85105] 4 5 3 6 5 4 5 5 3 3 3 3 5 5 5 1 5 5 4 4 5 4 5 5 5 3 6 6 4 3 4 5 4 6 4 5
    ## [85141] 4 4 4 6 1 3 1 4 4 2 4 6 5 1 3 4 5 4 1 3 3 3 4 3 4 3 6 5 6 3 5 5 5 5 5 5
    ## [85177] 4 5 3 5 2 4 2 5 1 2 4 5 3 5 2 2 2 6 3 4 4 5 3 5 5 2 5 4 3 5 5 5 5 5 4 2
    ## [85213] 4 5 6 1 4 2 1 5 2 5 5 6 3 3 6 6 1 3 3 6 6 1 2 6 5 4 3 3 2 3 4 3 4 5 2 7
    ## [85249] 2 4 5 2 5 1 3 6 6 6 3 5 4 5 5 5 5 6 1 3 5 1 4 5 3 1 6 2 4 1 5 3 1 1 3 5
    ## [85285] 3 5 4 5 3 6 7 2 2 2 2 2 5 2 4 2 2 2 2 4 6 5 6 2 6 4 4 5 6 4 4 6 6 1 3 6
    ## [85321] 5 6 6 6 6 6 6 4 4 4 4 6 4 2 5 4 6 6 6 5 6 3 6 6 4 6 4 6 4 4 4 4 6 5 6 5
    ## [85357] 6 5 4 6 6 5 4 5 6 4 4 6 4 6 6 6 3 6 1 2 5 5 2 4 2 3 4 4 6 4 3 3 4 4 4 4
    ## [85393] 5 5 2 2 2 2 5 5 1 2 2 5 3 5 6 2 5 4 4 6 5 5 4 4 4 1 1 4 5 2 2 4 3 4 2 2
    ## [85429] 6 1 1 6 1 4 4 3 6 5 5 5 1 5 5 5 5 5 5 4 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [85465] 5 5 5 3 5 5 5 5 4 1 5 5 5 5 5 5 5 5 5 5 3 3 1 2 1 5 1 3 2 1 5 1 5 1 1 1
    ## [85501] 4 1 2 1 4 3 5 5 3 3 1 3 5 4 5 4 6 3 3 7 6 1 4 1 1 1 1 1 5 1 2 5 3 3 6 5
    ## [85537] 4 5 5 5 5 5 2 6 1 3 2 4 4 5 5 4 1 1 4 1 1 3 1 3 6 4 3 3 3 3 3 1 4 3 1 4
    ## [85573] 1 5 2 5 1 5 5 4 5 4 3 6 3 5 4 4 4 6 3 4 5 4 4 6 5 5 6 1 5 4 5 6 1 3 1 5
    ## [85609] 5 1 4 5 5 5 5 5 3 5 4 5 5 5 5 5 5 5 5 3 5 5 5 3 1 3 2 5 1 5 3 2 5 5 2 2
    ## [85645] 3 3 2 5 5 4 1 1 4 4 3 4 4 3 3 4 4 5 6 4 3 6 1 2 5 4 2 4 6 3 1 5 5 1 1 3
    ## [85681] 3 3 4 5 5 3 4 5 5 1 1 1 1 1 1 1 1 5 5 5 4 2 2 2 2 2 2 2 2 2 2 4 2 2 2 4
    ## [85717] 4 2 2 6 6 2 2 2 2 2 2 2 6 2 2 2 4 2 2 2 4 4 2 2 2 2 2 2 2 2 2 2 2 6 5 2
    ## [85753] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 4 2 6 5 4 2 3 2 2 2 2 1 2 5 8 2 3
    ## [85789] 4 5 5 5 5 4 5 5 5 5 4 4 3 6 5 4 4 6 6 3 3 2 3 6 3 5 4 4 6 5 4 4 5 5 5 5
    ## [85825] 5 5 5 5 5 5 5 5 5 5 3 5 5 1 1 3 1 1 6 5 3 5 3 1 3 2 5 4 6 4 3 3 4 4 3 4
    ## [85861] 3 5 2 5 6 5 5 5 5 5 1 1 6 4 1 6 6 5 5 6 2 2 1 2 2 2 2 2 3 2 4 1 2 3 3 4
    ## [85897] 5 6 1 5 5 5 5 4 3 1 2 2 2 2 2 5 2 2 2 2 2 2 2 2 2 2 2 5 2 5 2 2 2 2 2 2
    ## [85933] 2 6 3 2 1 2 1 4 6 4 4 6 3 6 6 4 3 6 6 6 3 3 3 4 3 4 3 6 6 6 3 4 4 5 3 3
    ## [85969] 3 4 3 6 6 6 6 6 4 5 7 2 2 2 4 5 2 2 4 6 4 4 4 2 2 5 4 4 6 6 6 3 4 4 4 1
    ## [86005] 1 3 6 1 2 2 5 2 4 2 1 1 2 3 2 2 2 2 3 1 6 3 1 6 4 6 2 5 4 4 7 5 5 2 5 2
    ## [86041] 5 4 5 5 5 1 5 5 2 6 5 6 6 5 5 5 5 3 5 5 3 6 1 4 5 5 4 2 2 2 2 2 2 2 2 2
    ## [86077] 2 2 2 2 3 2 2 2 2 5 2 2 2 2 2 2 2 2 2 5 2 2 2 3 5 5 3 1 3 1 5 4 6 6 4 6
    ## [86113] 4 4 5 4 4 3 5 5 3 5 6 4 4 2 2 6 2 2 6 4 6 2 4 4 4 4 6 4 2 6 2 6 6 5 5 5
    ## [86149] 5 5 5 5 5 5 5 5 5 5 5 5 5 2 2 5 5 5 2 3 4 3 4 5 2 2 3 3 6 6 3 2 2 6 6 2
    ## [86185] 3 6 3 3 6 3 3 6 3 6 6 4 5 6 4 3 3 5 2 5 3 5 3 5 4 5 6 6 4 4 3 4 1 6 6 6
    ## [86221] 6 3 6 5 1 1 3 3 6 2 6 4 4 6 2 4 6 5 2 6 3 4 1 6 1 5 4 2 4 3 3 4 3 1 3 1
    ## [86257] 3 5 5 1 3 4 5 5 2 2 1 6 2 3 4 5 2 1 2 2 5 3 6 6 3 6 5 3 2 5 5 6 3 5 1 6
    ## [86293] 5 4 4 4 5 3 4 1 4 5 4 6 4 6 7 3 3 5 3 3 4 7 4 5 3 3 3 7 4 3 3 4 5 3 3 4
    ## [86329] 4 4 3 4 4 6 4 1 5 5 5 4 6 3 2 5 3 5 5 5 5 5 5 3 4 3 1 3 5 5 5 5 5 3 6 6
    ## [86365] 5 5 3 2 2 3 5 4 5 5 4 5 3 3 3 4 5 4 5 5 5 3 5 5 5 4 3 6 5 6 2 5 5 4 3 5
    ## [86401] 4 5 3 3 5 5 6 3 2 4 6 5 4 4 5 4 4 6 6 4 4 3 6 6 7 4 8 8 3 3 1 6 1 1 4 6
    ## [86437] 4 4 3 4 4 6 6 6 4 6 4 6 6 4 6 4 4 6 3 4 3 3 3 3 3 3 3 6 3 6 1 3 4 3 5 6
    ## [86473] 2 6 5 2 4 6 4 2 3 3 2 4 4 3 3 1 1 3 5 5 3 6 5 4 4 4 4 5 6 5 3 6 6 6 1 4
    ## [86509] 1 5 5 5 5 4 5 5 5 5 5 4 5 5 5 4 5 5 5 5 2 5 5 3 5 5 5 1 3 4 6 4 4 4 7 4
    ## [86545] 4 3 3 4 4 5 7 6 4 3 6 6 5 3 4 5 4 3 6 6 4 3 6 3 4 3 1 3 4 4 3 2 4 4 4 3
    ## [86581] 1 4 4 2 6 6 3 6 3 3 3 4 3 6 3 5 5 5 5 5 5 5 5 5 5 5 5 3 5 5 2 5 5 5 5 5
    ## [86617] 5 3 2 3 3 3 4 3 3 4 6 3 1 2 3 3 1 4 4 2 5 6 5 5 3 5 3 1 3 4 4 6 6 4 5 3
    ## [86653] 6 6 6 5 4 5 1 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [86689] 5 5 5 5 3 3 1 1 5 3 5 3 3 7 4 6 1 3 1 3 5 1 4 4 4 4 3 6 6 6 6 6 6 6 6 5
    ## [86725] 5 5 7 4 5 5 3 4 3 5 5 5 5 5 5 3 3 1 6 5 5 4 3 3 4 3 4 4 1 4 1 1 4 1 5 1
    ## [86761] 3 5 5 4 6 4 5 5 4 6 3 4 3 3 2 4 2 2 5 2 2 2 5 2 6 6 4 6 6 5 5 3 1 3 1 5
    ## [86797] 5 5 7 6 6 3 3 1 4 6 3 3 6 6 4 4 3 3 3 1 3 4 6 3 4 4 5 3 4 6 5 6 3 6 3 3
    ## [86833] 4 5 5 6 6 6 1 1 6 1 2 1 3 5 5 5 5 5 5 3 4 4 4 4 4 6 4 3 4 4 4 4 3 3 3 3
    ## [86869] 1 4 4 4 4 3 3 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 5 3 3 6 6 3 3 3 5
    ## [86905] 4 5 4 3 6 4 3 4 3 6 3 3 5 5 5 5 1 5 4 4 4 4 1 5 3 3 3 6 4 3 1 6 6 4 4 4
    ## [86941] 3 6 5 5 5 4 5 2 2 5 2 2 4 2 5 6 5 2 5 2 2 2 5 5 2 5 5 2 2 4 2 6 2 5 6 6
    ## [86977] 4 6 4 2 5 4 3 3 1 4 4 6 6 2 3 4 1 4 5 5 1 6 4 6 3 3 4 4 6 5 5 5 5 5 5 4
    ## [87013] 4 3 6 5 1 6 3 4 3 5 3 3 4 1 6 6 2 6 6 4 6 6 6 6 2 6 2 6 6 6 6 5 2 6 2 3
    ## [87049] 3 6 3 6 4 4 1 3 3 3 3 6 6 1 4 5 2 1 1 6 3 2 6 1 5 5 6 1 5 5 3 5 5 5 5 4
    ## [87085] 5 5 5 5 2 5 5 5 5 5 5 3 4 1 1 6 3 5 6 2 5 4 2 5 6 2 5 3 3 3 1 3 3 3 4 5
    ## [87121] 5 5 4 4 4 2 6 4 1 6 6 6 3 4 6 4 5 6 4 6 4 3 4 6 4 1 3 5 5 4 3 4 4 3 5 3
    ## [87157] 1 5 1 1 1 1 1 5 1 3 5 1 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 3 1 5 1 5 5 5 3 2
    ## [87193] 2 1 1 1 4 3 5 2 2 6 4 5 5 5 6 5 5 4 4 2 3 5 2 5 2 1 2 5 5 6 6 4 3 2 5 4
    ## [87229] 4 2 3 2 5 1 5 5 5 4 4 6 5 5 5 5 5 4 3 4 4 4 4 4 2 4 3 3 2 4 7 1 1 1 7 5
    ## [87265] 5 7 4 2 2 4 3 4 6 2 3 3 4 2 2 2 1 1 4 5 5 5 5 5 5 5 5 5 5 5 5 5 6 5 4 5
    ## [87301] 1 6 6 6 6 1 6 6 6 4 4 5 3 6 3 3 6 3 4 4 7 4 4 3 1 2 1 1 4 4 4 5 5 5 4 1
    ## [87337] 6 4 6 3 1 6 5 6 1 4 4 4 6 4 4 4 3 1 1 5 6 4 4 4 3 6 4 4 1 5 1 4 6 5 5 3
    ## [87373] 5 3 3 5 3 5 5 5 5 1 5 5 5 5 5 5 5 5 5 5 4 4 4 3 4 4 4 5 4 2 4 4 4 4 4 6
    ## [87409] 3 3 3 6 4 3 2 4 4 4 4 3 5 5 5 5 6 5 5 2 3 2 6 1 3 6 5 3 3 3 5 4 5 5 1 1
    ## [87445] 3 3 3 3 3 5 3 4 5 4 5 2 3 4 5 5 5 5 5 5 6 6 5 5 6 4 5 3 3 5 3 4 4 6 3 1
    ## [87481] 5 3 5 5 5 5 5 3 5 5 5 3 4 5 3 5 1 4 4 3 6 4 6 4 4 1 5 3 5 5 1 4 5 1 5 5
    ## [87517] 5 5 3 2 5 5 2 3 1 1 1 4 3 5 2 5 5 5 4 5 5 1 6 4 4 5 5 1 6 5 4 3 6 4 4 4
    ## [87553] 4 4 4 1 4 4 4 6 4 4 1 4 4 4 6 3 6 2 6 6 4 3 5 3 4 3 2 4 3 4 4 4 3 4 1 1
    ## [87589] 2 6 3 5 1 2 7 6 3 5 5 5 5 1 4 4 5 5 6 6 6 2 7 6 5 2 5 5 5 1 5 3 4 3 6 6
    ## [87625] 4 5 3 4 6 5 5 1 1 1 3 6 6 5 2 6 6 4 4 6 5 2 2 4 3 5 2 4 4 3 5 6 3 3 3 3
    ## [87661] 5 4 3 3 1 1 1 5 6 4 4 3 4 6 4 5 4 3 6 3 1 6 3 6 4 4 6 4 6 3 6 4 3 3 4 4
    ## [87697] 5 5 5 5 6 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 2 4 4 3 3 5 6 5 5 3 5 5 3 3
    ## [87733] 3 5 1 4 5 4 3 4 4 2 3 3 3 4 1 3 5 5 5 5 5 5 5 2 5 3 5 1 5 5 3 1 2 2 2 6
    ## [87769] 4 6 6 1 2 4 4 5 6 1 1 2 3 4 2 4 4 3 3 1 3 1 4 1 1 4 2 3 4 1 3 2 3 5 4 6
    ## [87805] 3 4 4 4 4 4 4 6 4 4 5 3 6 3 3 7 2 2 2 6 3 4 1 6 3 4 4 6 6 6 6 6 6 3 3 4
    ## [87841] 2 5 5 5 3 2 2 3 4 3 4 4 4 4 1 4 4 6 4 1 6 1 3 2 2 6 6 4 6 4 6 2 4 2 2 4
    ## [87877] 2 6 2 4 6 3 5 2 2 2 2 3 3 7 4 1 5 4 3 3 6 3 3 6 3 2 3 3 4 3 5 3 5 5 5 5
    ## [87913] 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 4 3 1 5 5 4 4 3 3 3 5 3 5 4
    ## [87949] 5 5 5 1 3 3 3 3 4 3 6 4 6 4 1 4 4 6 6 3 4 6 4 4 3 4 4 6 6 6 6 6 6 1 4 3
    ## [87985] 4 4 4 4 4 3 4 4 3 4 4 4 4 4 4 3 4 5 4 5 4 4 5 4 4 4 5 3 5 4 3 3 4 4 6 3
    ## [88021] 3 5 5 4 4 1 1 3 5 1 1 3 3 5 5 5 5 5 5 2 5 7 5 4 4 2 3 5 2 3 5 5 5 5 5 4
    ## [88057] 4 5 5 3 1 5 5 5 3 1 3 6 6 6 6 6 5 6 6 6 6 6 6 6 6 6 3 6 5 3 5 1 4 5 1 1
    ## [88093] 1 4 1 5 1 5 4 1 5 1 5 2 3 6 4 1 4 5 1 2 3 4 4 4 4 4 4 4 6 1 6 6 4 4 1 5
    ## [88129] 3 1 4 4 6 3 3 6 4 2 4 4 6 6 2 4 4 4 6 3 6 4 6 6 6 3 5 2 4 3 4 6 4 3 1 5
    ## [88165] 2 2 6 3 6 6 5 4 4 5 6 6 4 4 5 4 5 4 4 5 6 6 5 4 6 3 1 3 4 3 4 6 3 3 2 2
    ## [88201] 3 1 2 5 5 4 5 1 3 4 4 3 4 3 6 1 3 6 3 3 1 5 4 4 3 4 4 3 5 3 6 1 7 5 4 6
    ## [88237] 4 6 3 2 1 3 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 3 5 4 5 4 5 2 6 4 3 4 5 5 5
    ## [88273] 5 5 5 5 2 5 3 5 5 5 5 5 5 4 5 5 1 5 6 6 6 5 6 3 3 6 6 4 4 5 6 4 6 3 4 5
    ## [88309] 5 5 5 5 5 5 5 5 5 2 5 5 2 5 3 6 2 5 5 5 1 5 5 4 5 6 3 6 1 2 4 5 1 2 5 5
    ## [88345] 2 5 5 4 5 5 2 5 5 5 2 2 1 1 5 6 6 5 5 5 6 5 5 5 4 5 3 3 1 4 3 6 4 1 1 6
    ## [88381] 4 2 5 5 5 5 5 5 5 5 5 5 5 5 5 4 2 5 5 1 5 3 5 5 5 5 5 5 5 5 5 5 2 5 2 5
    ## [88417] 5 5 5 5 5 5 4 1 3 4 5 6 5 2 1 5 4 2 3 4 6 2 4 4 1 5 3 7 1 3 5 5 5 5 5 1
    ## [88453] 3 5 4 2 6 3 2 4 5 5 5 1 5 3 3 2 5 5 5 5 3 5 5 5 5 5 5 5 5 5 3 4 1 4 5 6
    ## [88489] 4 4 4 4 4 6 6 3 4 4 3 5 3 1 4 1 3 1 2 5 5 5 5 3 3 5 5 3 4 4 4 4 4 4 3 2
    ## [88525] 3 8 1 6 4 4 1 6 3 5 6 4 4 3 1 5 4 4 5 3 3 5 5 3 3 3 4 5 4 4 5 6 6 4 5 3
    ## [88561] 3 3 1 6 4 7 3 3 3 3 3 5 4 3 1 1 3 1 3 5 3 4 1 4 2 2 5 3 5 5 5 3 3 6 2 3
    ## [88597] 2 2 2 4 3 2 3 6 6 6 2 4 2 6 6 4 4 6 6 6 4 5 6 4 4 6 4 1 4 4 5 1 5 3 5 5
    ## [88633] 2 4 3 5 5 5 5 3 5 2 5 3 3 6 3 6 5 5 5 2 1 5 5 4 3 4 6 2 4 5 6 6 5 5 5 6
    ## [88669] 4 6 1 1 3 1 6 6 3 4 4 4 3 3 4 4 1 1 1 4 4 3 3 4 4 4 4 1 3 3 2 3 1 1 2 4
    ## [88705] 4 6 3 1 1 4 4 4 1 4 6 2 3 5 4 5 4 5 5 6 5 5 6 5 5 4 4 4 5 5 5 6 5 3 1 4
    ## [88741] 6 4 5 5 5 3 5 5 2 4 2 2 2 2 2 2 4 7 2 2 4 1 6 6 4 5 4 6 6 3 4 3 3 6 1 1
    ## [88777] 1 3 5 5 3 2 3 6 4 3 4 6 2 4 5 1 3 2 3 4 4 6 6 4 5 3 3 1 4 4 4 4 4 4 5 6
    ## [88813] 6 3 6 5 6 6 1 2 2 6 1 2 5 1 3 6 6 6 6 4 4 6 6 4 3 3 5 3 4 5 5 5 5 4 3 3
    ## [88849] 5 5 2 3 4 5 5 5 5 1 3 3 6 6 6 4 2 6 2 5 1 4 5 5 5 1 2 5 5 5 2 2 2 5 4 3
    ## [88885] 3 4 4 3 4 4 4 1 6 3 2 2 5 2 2 5 4 2 2 2 2 2 2 5 2 5 5 5 1 5 5 5 7 3 5 1
    ## [88921] 5 2 2 2 2 5 2 2 2 2 2 2 1 5 2 2 2 2 2 2 2 2 2 2 5 5 2 2 6 2 2 2 2 5 2 2
    ## [88957] 2 2 2 2 5 2 2 2 5 4 5 5 5 3 1 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [88993] 5 5 5 5 5 3 1 3 3 6 4 4 4 4 6 3 6 6 4 4 4 1 1 6 3 8 3 5 5 5 5 5 6 8 5 4
    ## [89029] 2 2 2 2 2 7 5 4 2 2 2 2 2 2 2 2 2 1 2 3 2 5 6 4 5 5 2 2 5 4 3 6 4 4 4 1
    ## [89065] 5 5 5 4 4 4 7 5 3 4 5 5 3 5 5 5 5 1 6 5 6 4 4 6 4 5 6 2 4 6 4 4 4 6 4 4
    ## [89101] 4 4 4 6 6 6 4 7 5 5 2 5 3 1 6 1 2 5 5 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6
    ## [89137] 2 4 6 6 3 3 6 6 6 4 6 4 3 5 4 4 4 6 3 5 5 6 3 5 4 5 5 5 3 4 5 5 5 5 5 6
    ## [89173] 4 5 3 6 4 5 2 2 1 4 5 5 1 5 1 3 3 5 5 5 1 2 3 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [89209] 3 5 5 5 5 5 5 5 5 5 3 5 5 4 5 5 5 5 5 5 5 5 2 5 5 5 5 2 5 5 5 4 3 3 4 3
    ## [89245] 3 1 5 5 5 3 1 3 5 1 2 2 5 4 4 2 4 4 2 6 1 6 3 6 4 8 3 4 4 4 4 5 3 3 3 2
    ## [89281] 2 2 2 1 3 6 4 4 6 2 4 5 1 3 5 5 4 4 3 4 4 2 2 6 4 3 3 4 6 2 6 2 6 6 3 3
    ## [89317] 5 4 5 5 5 4 4 5 4 3 5 4 3 3 1 5 2 4 3 2 4 2 4 5 5 5 4 5 5 3 5 4 4 6 4 4
    ## [89353] 2 4 6 4 6 1 4 6 2 6 6 4 1 6 4 2 4 6 6 6 2 1 4 6 2 4 4 6 6 6 4 4 3 3 5 3
    ## [89389] 6 5 4 6 3 4 3 5 5 5 6 4 4 5 2 4 4 3 1 6 3 5 5 4 3 5 3 1 5 5 6 6 6 5 5 5
    ## [89425] 4 4 5 3 1 1 6 3 3 4 4 4 4 4 4 6 2 6 4 6 4 4 4 5 5 6 2 3 1 5 2 2 4 6 6 3
    ## [89461] 3 4 4 5 3 4 4 4 6 6 4 6 3 4 6 6 4 4 1 5 1 6 4 1 5 4 3 4 1 6 1 6 5 4 5 4
    ## [89497] 4 5 5 2 6 6 3 2 4 2 6 2 6 5 6 7 4 5 1 6 4 1 3 4 1 5 1 2 2 1 3 4 4 3 5 4
    ## [89533] 4 4 5 3 4 5 6 5 6 4 7 2 2 1 7 5 4 5 5 1 4 2 6 2 6 4 2 4 3 2 6 2 2 3 6 2
    ## [89569] 4 1 2 6 2 2 2 1 2 2 5 6 2 3 6 2 3 2 6 6 2 3 3 3 2 5 5 2 2 5 2 5 5 2 5 2
    ## [89605] 5 2 5 2 5 5 5 5 5 5 5 2 5 5 5 5 5 5 2 5 5 5 5 5 4 3 5 3 3 3 6 5 5 1 3 4
    ## [89641] 2 3 3 1 6 4 6 6 6 4 4 6 4 7 4 6 4 4 4 4 3 3 3 4 6 5 2 2 5 3 5 5 4 3 1 5
    ## [89677] 4 3 2 3 6 4 4 5 5 3 1 4 4 3 1 3 5 1 3 4 5 3 1 1 6 1 1 6 6 4 4 1 3 3 6 6
    ## [89713] 1 1 1 1 6 4 4 4 2 5 5 5 5 1 5 5 1 3 1 1 5 2 3 3 5 5 5 5 5 4 5 5 5 5 5 5
    ## [89749] 4 3 6 4 3 1 4 5 5 5 5 5 5 2 5 5 5 5 5 4 5 5 5 5 5 3 2 2 5 5 2 3 6 4 4 6
    ## [89785] 3 4 6 6 4 5 3 3 3 3 3 3 4 3 4 4 4 4 6 4 1 6 7 5 3 1 4 4 6 3 7 4 3 3 2 3
    ## [89821] 3 3 1 3 3 6 2 5 2 5 2 2 2 2 2 5 5 2 4 3 3 3 5 1 1 3 4 2 4 2 5 5 3 3 6 3
    ## [89857] 2 2 3 5 5 5 5 5 5 2 5 6 5 5 5 5 5 2 5 5 2 5 5 5 2 5 5 2 6 5 5 5 2 5 5 5
    ## [89893] 5 2 3 1 6 1 5 6 5 5 6 5 1 5 1 1 3 3 4 5 5 3 4 6 4 4 5 6 5 1 4 6 4 3 3 6
    ## [89929] 4 3 6 5 5 5 5 5 5 5 4 5 5 5 3 5 5 3 5 2 4 5 5 5 4 5 5 6 5 4 5 5 5 5 5 3
    ## [89965] 5 5 5 5 5 5 5 5 3 5 5 3 3 3 5 4 6 3 3 5 6 6 5 6 5 3 6 3 4 4 3 6 3 3 3 4
    ## [90001] 1 3 1 5 5 1 5 1 5 5 5 5 1 1 1 1 3 5 4 3 6 3 2 3 4 4 6 6 4 4 6 6 6 4 1 4
    ## [90037] 4 3 2 2 4 6 2 4 4 2 3 4 1 4 1 3 5 2 3 2 3 3 4 4 3 4 1 5 5 6 6 2 5 5 5 6
    ## [90073] 5 6 5 4 6 5 1 5 5 4 5 3 4 5 5 5 4 3 1 1 3 5 2 4 4 4 4 3 5 4 4 4 5 2 5 3
    ## [90109] 3 4 4 4 5 6 2 5 4 3 4 4 5 1 5 3 5 5 4 5 3 5 3 3 5 1 1 2 1 8 3 4 5 5 5 3
    ## [90145] 5 3 5 5 5 6 3 4 6 6 5 3 3 3 2 1 4 4 6 6 3 5 3 1 4 5 1 2 2 2 2 2 2 6 2 2
    ## [90181] 2 5 2 4 5 6 4 2 6 2 1 4 2 1 1 5 2 2 2 2 3 4 1 5 5 2 3 5 5 1 5 5 3 5 5 4
    ## [90217] 6 5 5 5 5 4 3 5 5 4 5 5 5 4 2 3 5 7 5 3 1 4 6 5 5 3 5 5 5 5 4 5 5 5 2 5
    ## [90253] 1 1 4 1 4 6 3 5 5 1 1 3 5 6 2 1 3 1 4 5 1 3 1 5 3 3 6 6 3 4 6 6 4 6 4 6
    ## [90289] 4 4 6 3 5 6 4 4 3 4 5 3 4 4 6 6 6 6 3 4 4 4 4 4 3 5 4 4 3 4 1 4 5 3 1 3
    ## [90325] 6 4 2 2 1 3 3 3 4 4 2 4 5 5 3 3 5 1 4 4 6 1 3 3 3 1 6 4 5 1 4 3 6 2 1 8
    ## [90361] 2 2 2 2 2 3 5 4 2 2 2 5 2 5 6 1 1 5 5 5 5 5 5 5 5 1 3 5 5 5 5 5 5 5 5 5
    ## [90397] 6 1 3 1 4 7 4 4 4 6 7 4 5 4 5 3 7 4 6 5 5 5 5 1 3 4 7 5 4 3 3 4 3 5 5 4
    ## [90433] 5 6 6 5 6 4 3 4 5 5 4 6 5 6 5 4 4 4 6 6 5 6 4 4 6 6 1 1 5 1 4 6 1 3 3 5
    ## [90469] 5 5 4 1 3 6 4 7 5 1 6 2 3 3 3 3 3 3 3 6 6 3 2 1 1 1 5 6 4 4 4 2 4 3 4 5
    ## [90505] 6 6 6 4 4 4 2 4 3 6 6 6 6 4 6 1 5 2 5 6 6 6 6 6 1 4 4 6 6 6 4 4 4 5 4 5
    ## [90541] 5 5 3 4 7 3 1 5 4 3 5 3 1 1 1 5 3 5 1 2 5 5 5 5 6 4 4 4 3 3 3 6 1 6 6 1
    ## [90577] 3 6 5 2 1 3 6 3 1 4 4 4 2 4 6 4 4 4 6 4 5 6 1 6 4 6 4 6 4 4 4 4 4 6 4 4
    ## [90613] 4 4 4 6 1 1 4 4 3 4 6 4 6 4 4 6 4 4 2 4 3 1 3 6 6 3 4 3 4 3 6 4 3 3 5 4
    ## [90649] 2 6 2 2 2 2 2 2 6 3 2 2 2 2 2 2 3 4 6 4 4 3 6 5 3 4 1 6 1 5 4 6 1 4 6 3
    ## [90685] 6 6 4 4 4 6 6 6 6 6 5 5 3 5 3 5 5 2 2 2 2 2 3 2 2 2 4 2 2 2 2 2 2 4 2 2
    ## [90721] 2 5 3 4 4 4 4 4 6 4 6 6 5 6 6 4 4 6 6 6 6 6 5 6 5 4 3 3 5 4 5 6 6 6 1 1
    ## [90757] 5 4 6 4 4 6 5 6 6 5 6 6 6 5 6 6 6 6 4 4 4 6 4 6 4 4 4 6 4 2 6 6 4 6 6 6
    ## [90793] 4 6 4 4 3 6 3 1 3 6 1 5 3 4 4 4 5 5 5 5 4 3 4 3 5 1 4 4 4 2 4 4 3 7 6 2
    ## [90829] 4 3 2 4 3 4 1 4 4 6 2 4 4 4 4 4 4 1 4 4 4 4 4 4 4 4 4 3 6 2 2 1 4 5 2 1
    ## [90865] 4 2 1 3 5 5 5 2 5 5 5 4 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 3 5 4 6 3 4 3 3
    ## [90901] 4 4 3 5 5 5 4 1 3 6 4 6 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 1 6 5 6 5 6 1 4
    ## [90937] 5 5 5 5 5 3 3 5 3 5 7 5 5 5 4 1 6 4 6 5 3 7 5 4 6 5 4 4 3 5 1 3 4 3 2 6
    ## [90973] 2 6 5 3 4 5 1 5 5 6 7 6 4 3 4 2 4 2 2 2 1 5 4 6 4 4 3 6 1 2 1 4 3 6 4 4
    ## [91009] 4 4 3 5 4 3 1 5 5 5 5 5 5 2 5 5 5 5 5 4 6 5 4 8 5 1 1 3 3 3 3 5 5 4 5 4
    ## [91045] 3 5 3 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 6 5 5 4 6 5 5 5 5 5 5 5 5
    ## [91081] 5 5 1 3 3 5 2 5 5 5 5 7 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [91117] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 6 4 6 4 6 2 4 6 5 4 4 4 4 4 4 4 5 3 4 8
    ## [91153] 4 2 2 6 6 3 4 6 5 4 5 5 5 5 5 3 5 2 1 3 5 4 3 4 3 4 2 5 6 4 3 4 4 1 1 1
    ## [91189] 7 1 2 4 2 5 4 2 4 1 4 2 2 1 3 4 4 1 5 5 1 3 4 3 4 3 3 3 5 5 5 5 3 5 4 1
    ## [91225] 6 5 4 3 6 4 4 6 3 1 5 3 5 5 5 1 5 5 5 5 5 1 5 5 1 5 5 5 6 1 6 6 1 4 6 5
    ## [91261] 6 1 3 3 4 2 4 5 4 5 1 3 3 1 3 5 3 6 3 4 3 4 2 3 3 1 6 5 5 4 5 6 5 5 5 5
    ## [91297] 5 5 5 4 5 5 5 4 5 5 5 5 5 5 5 2 5 3 5 5 5 5 2 5 5 5 5 5 5 5 1 2 2 2 6 2
    ## [91333] 2 6 6 2 2 2 2 6 4 2 2 2 3 2 4 5 6 5 1 4 4 5 4 6 3 3 4 3 6 6 4 6 4 6 3 6
    ## [91369] 4 3 5 5 5 5 3 3 3 1 5 7 5 1 5 4 4 3 4 3 1 4 4 4 4 5 1 5 5 5 3 1 3 7 5 5
    ## [91405] 2 2 4 2 2 3 6 1 4 1 1 4 5 3 3 2 4 5 3 5 3 1 1 6 1 3 6 3 4 1 4 5 5 5 4 5
    ## [91441] 5 5 5 5 5 5 5 5 5 2 5 5 5 5 2 5 5 3 5 5 3 1 5 4 3 3 5 4 5 3 5 5 4 5 4 4
    ## [91477] 4 5 3 4 4 2 1 5 5 5 2 4 4 4 3 3 5 3 5 5 2 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [91513] 3 1 2 2 5 2 2 5 4 1 4 5 3 2 5 1 5 1 5 5 1 3 3 3 3 6 4 4 3 6 4 6 4 4 7 4
    ## [91549] 5 1 2 2 5 2 7 4 5 2 6 2 6 1 4 2 4 5 6 4 4 4 2 4 2 5 4 3 1 3 5 1 4 1 1 3
    ## [91585] 1 5 4 6 5 1 1 3 4 5 5 5 4 5 5 4 3 2 3 5 6 5 5 4 5 5 3 5 5 5 5 5 5 5 4 4
    ## [91621] 3 4 3 4 6 3 4 3 3 5 3 5 2 5 5 3 1 6 5 3 3 1 3 3 5 3 5 4 6 4 2 3 1 6 4 4
    ## [91657] 4 6 1 6 3 4 3 5 1 1 6 5 2 6 5 3 5 4 5 6 2 5 3 3 5 4 5 2 2 5 2 4 6 4 1 6
    ## [91693] 6 6 4 5 6 4 3 3 5 5 5 4 6 6 5 1 1 1 4 3 2 6 4 3 1 6 1 3 4 6 3 3 6 4 4 6
    ## [91729] 4 3 5 5 2 5 4 1 3 4 3 3 4 6 6 1 4 4 6 4 3 4 6 6 6 4 4 4 2 4 5 4 6 2 5 5
    ## [91765] 4 2 6 6 2 4 4 3 1 5 3 3 5 5 3 3 5 6 6 5 5 5 2 6 4 1 3 2 6 3 4 5 2 4 6 3
    ## [91801] 4 4 5 3 5 4 4 2 5 3 3 3 6 6 3 3 6 6 4 3 4 4 1 5 6 4 6 4 1 6 3 4 2 1 3 5
    ## [91837] 4 4 4 3 6 3 5 4 4 4 6 6 3 5 1 3 3 3 3 2 5 4 5 4 5 5 2 2 5 6 6 4 5 2 4 6
    ## [91873] 4 6 4 5 1 5 1 2 4 6 5 6 5 6 5 5 6 4 5 4 5 6 6 5 5 6 6 4 5 6 3 4 6 3 2 4
    ## [91909] 5 1 1 5 1 2 1 1 5 1 1 1 1 1 3 1 5 3 6 4 3 4 4 6 3 3 6 4 4 3 4 3 3 1 4 1
    ## [91945] 6 2 4 4 4 4 3 4 3 4 8 5 5 4 4 4 4 4 6 4 4 5 5 4 2 2 2 5 3 2 2 5 2 2 4 2
    ## [91981] 2 2 2 6 2 6 6 5 5 2 4 2 1 5 1 5 5 5 5 5 5 5 3 5 3 5 5 3 3 1 1 3 3 2 5 4
    ## [92017] 3 3 3 6 3 1 1 6 3 4 6 4 4 3 4 4 5 5 4 3 5 5 1 3 3 4 1 5 1 5 5 3 4 3 5 2
    ## [92053] 5 5 2 1 6 1 4 4 1 6 4 4 5 2 4 4 4 1 4 4 4 4 4 5 7 2 1 5 4 4 4 4 1 3 1 3
    ## [92089] 5 4 5 2 2 2 2 2 2 2 2 2 2 6 2 1 1 1 2 2 2 1 5 2 2 2 2 2 2 5 2 2 2 2 2 2
    ## [92125] 2 5 2 6 2 2 1 5 1 4 4 3 6 5 5 3 1 4 5 5 5 5 5 5 6 4 2 3 5 1 1 3 4 3 5 3
    ## [92161] 3 2 1 2 3 3 3 4 6 4 5 1 5 5 3 5 3 6 5 3 5 4 4 7 4 4 5 7 4 4 1 5 3 5 5 3
    ## [92197] 5 3 5 4 3 3 1 3 3 3 6 4 6 3 6 4 5 4 4 6 4 5 5 5 5 1 6 3 4 6 6 6 6 5 2 2
    ## [92233] 3 5 2 2 2 1 2 2 2 2 2 4 2 2 1 5 2 5 5 2 4 1 5 5 5 1 5 4 5 5 3 4 1 4 2 4
    ## [92269] 4 6 6 6 6 6 6 5 5 3 1 5 1 4 6 3 4 4 4 6 6 2 6 6 2 4 1 3 2 2 4 3 5 4 3 6
    ## [92305] 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 3 5 5 5 3 4 6 6 3 4 1 5 3
    ## [92341] 6 4 4 6 5 4 6 4 6 3 1 4 2 6 6 5 6 1 5 4 3 5 5 2 6 2 2 1 3 3 1 1 2 1 1 3
    ## [92377] 3 3 5 5 4 1 6 1 6 5 2 2 5 4 6 4 3 1 1 4 5 5 3 2 2 2 2 2 2 1 2 2 2 1 2 2
    ## [92413] 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2
    ## [92449] 2 2 2 2 2 2 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2
    ## [92485] 2 2 2 2 2 6 6 6 6 4 4 3 6 3 1 3 6 6 4 5 5 4 5 5 3 4 1 4 5 5 1 3 5 5 5 1
    ## [92521] 4 6 7 3 3 1 2 2 4 3 2 4 2 6 3 1 4 2 3 2 4 1 3 5 3 1 5 3 4 6 4 4 5 3 5 6
    ## [92557] 3 4 1 4 4 4 4 6 3 4 3 5 5 1 1 5 5 1 5 1 1 1 5 5 1 5 4 3 2 1 2 6 1 3 6 4
    ## [92593] 4 2 3 2 4 3 3 6 4 6 6 1 3 3 6 5 4 5 5 5 5 5 3 4 5 5 5 5 2 2 5 5 5 5 5 5
    ## [92629] 3 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 1 5 5 5 5 5 5 5 6 5 5 4 4 5
    ## [92665] 2 5 4 5 5 5 2 2 2 2 2 2 1 5 6 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2
    ## [92701] 2 2 2 2 2 2 2 4 2 2 2 3 1 6 5 3 1 4 5 6 3 3 4 4 4 4 2 6 6 6 4 6 6 6 2 6
    ## [92737] 6 6 4 4 6 6 5 3 3 4 4 1 4 6 3 3 3 6 6 3 6 3 4 3 5 5 3 5 5 5 5 5 5 5 3 5
    ## [92773] 5 5 5 5 5 5 1 4 4 4 4 5 2 6 4 4 4 4 5 3 6 2 5 5 3 5 5 5 3 4 4 4 3 5 4 5
    ## [92809] 5 5 4 1 5 3 5 5 3 1 3 5 3 4 3 4 4 3 4 5 1 4 3 5 5 5 3 5 5 5 3 5 5 2 5 5
    ## [92845] 5 5 2 2 2 2 5 5 6 4 6 3 3 3 4 1 4 1 1 3 3 4 1 5 5 3 4 6 4 6 6 6 3 3 3 6
    ## [92881] 6 3 3 3 5 6 6 4 6 6 5 6 4 4 4 6 4 4 3 4 4 4 4 6 6 5 3 4 6 4 5 6 3 4 4 3
    ## [92917] 4 1 1 2 3 3 6 5 5 4 4 3 5 1 4 2 4 3 4 4 5 5 6 4 5 5 5 2 4 1 2 3 1 6 3 5
    ## [92953] 5 5 6 4 4 6 4 4 6 4 6 1 6 3 3 4 6 3 4 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5
    ## [92989] 5 5 5 5 5 5 5 5 5 4 3 6 4 3 2 4 6 6 5 4 3 3 4 5 5 4 4 4 4 4 6 1 3 4 4 4
    ## [93025] 4 4 1 6 4 4 3 4 5 5 3 4 1 2 4 3 3 4 5 6 4 6 5 5 5 5 1 6 4 3 3 2 4 5 3 6
    ## [93061] 5 4 4 3 6 4 6 6 6 4 3 1 4 1 3 4 3 6 5 6 6 6 5 3 5 5 5 5 4 5 5 4 4 4 4 4
    ## [93097] 3 4 3 4 1 5 4 4 5 6 5 4 3 4 6 3 1 3 4 5 3 1 1 1 1 6 3 4 5 5 5 5 1 4 4 6
    ## [93133] 6 5 3 3 5 4 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 3 3 6 6 5 2 2 6 6 6 6 1 5 5 2
    ## [93169] 5 1 4 5 6 4 4 4 2 3 5 2 2 4 4 2 2 4 1 2 4 5 4 2 4 6 4 2 2 2 3 3 3 4 3 4
    ## [93205] 3 6 3 4 5 6 2 5 4 4 6 6 5 6 4 5 6 4 6 3 3 4 1 3 4 4 4 3 4 3 4 4 4 4 4 6
    ## [93241] 6 6 6 3 6 4 6 6 6 3 1 5 4 3 5 2 2 4 4 2 2 5 5 2 2 2 5 1 1 1 5 4 3 4 5 3
    ## [93277] 4 4 4 3 2 6 3 2 6 4 6 4 6 4 6 3 6 6 6 6 6 4 1 3 6 3 6 4 2 6 6 4 4 6 4 6
    ## [93313] 6 4 4 4 3 6 4 6 4 4 3 2 2 5 2 2 2 2 2 2 2 2 4 6 3 4 4 4 3 4 6 3 3 6 3 3
    ## [93349] 5 5 4 4 5 5 6 3 5 5 5 5 5 5 5 5 3 3 3 3 4 6 4 4 1 6 5 3 4 6 6 3 3 6 6 4
    ## [93385] 6 1 3 5 2 5 4 1 4 5 5 1 5 6 4 1 1 2 4 4 6 3 6 5 4 4 1 3 1 1 4 6 3 4 4 6
    ## [93421] 4 4 6 5 3 5 5 5 1 5 5 5 4 6 3 6 6 5 5 4 2 5 4 4 1 1 5 1 1 1 5 4 4 4 1 1
    ## [93457] 4 4 4 5 7 4 6 6 5 5 1 5 5 5 5 1 6 6 3 3 5 4 5 3 1 4 3 1 1 6 4 6 5 3 3 5
    ## [93493] 5 5 3 4 5 3 4 5 3 5 4 3 1 5 6 4 1 5 1 4 4 5 5 5 5 3 6 3 3 4 6 4 4 6 4 4
    ## [93529] 6 3 4 6 2 6 3 4 3 6 6 4 4 3 5 5 3 1 1 3 3 4 5 2 4 6 4 5 1 2 6 3 4 5 3 1
    ## [93565] 5 5 3 3 3 6 3 3 3 3 3 3 1 5 5 5 5 2 3 3 3 4 4 4 4 2 6 6 6 4 2 4 4 6 1 4
    ## [93601] 4 5 5 5 2 5 4 3 3 3 3 3 3 3 4 5 5 3 5 5 3 1 1 4 1 1 4 5 5 6 3 4 3 4 4 4
    ## [93637] 1 1 5 6 4 5 4 5 5 5 4 4 6 4 4 4 6 3 4 6 4 3 6 4 4 6 3 4 4 3 3 5 5 5 5 3
    ## [93673] 5 5 6 5 5 5 5 5 5 6 4 5 4 4 7 4 5 4 4 4 4 4 4 4 4 4 4 3 3 5 3 3 4 3 3 4
    ## [93709] 3 6 6 3 3 3 3 4 5 3 5 2 6 5 4 5 5 5 1 4 4 4 5 5 5 5 5 4 5 5 5 4 3 5 4 5
    ## [93745] 5 3 5 5 4 5 5 5 2 4 5 5 4 5 1 3 3 5 5 4 5 5 3 5 5 4 5 6 3 2 2 4 4 4 5 4
    ## [93781] 4 2 3 3 3 1 4 5 4 5 4 4 1 3 6 4 6 3 3 3 5 5 1 5 2 1 5 5 3 3 1 5 5 4 6 5
    ## [93817] 3 4 3 4 6 3 6 4 4 3 4 3 4 6 6 2 3 6 6 4 6 3 4 3 4 2 3 1 1 1 1 5 1 3 2 3
    ## [93853] 1 5 3 2 2 1 3 4 5 4 6 3 2 1 4 1 3 1 4 4 5 2 1 3 1 5 5 4 3 4 5 4 1 1 7 2
    ## [93889] 5 6 4 6 2 7 4 4 1 2 2 5 1 1 2 4 1 3 4 5 5 5 6 6 5 5 5 5 6 5 5 2 5 2 5 5
    ## [93925] 5 5 1 1 5 3 1 5 1 1 5 1 5 6 3 3 3 5 3 6 3 6 3 3 3 3 3 3 4 3 6 2 2 2 6 3
    ## [93961] 2 5 2 3 6 2 2 2 2 2 2 2 2 3 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5
    ## [93997] 5 5 5 4 3 1 6 6 4 6 4 2 6 6 4 2 6 2 1 6 6 6 6 6 2 6 5 1 6 6 6 6 4 2 6 6
    ## [94033] 1 4 5 1 3 1 3 1 5 3 3 1 3 2 5 2 2 1 2 1 5 2 2 5 2 5 5 5 5 5 5 4 7 2 4 6
    ## [94069] 1 4 4 5 4 4 4 3 2 4 2 4 5 5 3 1 1 3 3 5 4 3 6 4 5 1 5 3 3 3 3 1 5 5 5 4
    ## [94105] 6 5 5 5 3 5 4 5 6 5 3 4 4 1 5 5 5 5 5 5 4 3 2 3 4 4 1 4 2 5 5 6 2 6 3 2
    ## [94141] 5 3 3 3 4 3 1 5 5 5 5 5 4 5 5 5 1 3 4 4 4 6 6 6 3 3 6 6 3 3 3 3 4 4 4 3
    ## [94177] 4 4 4 4 4 1 2 4 3 4 6 1 5 1 5 6 4 5 4 3 3 6 3 6 3 5 4 5 4 6 6 6 6 4 4 4
    ## [94213] 6 6 6 4 3 6 4 6 4 3 3 4 3 4 1 5 3 6 6 1 3 4 2 4 6 4 1 6 6 6 3 4 4 5 5 6
    ## [94249] 5 3 4 4 3 3 1 4 1 6 6 1 7 2 5 6 4 6 6 3 3 4 4 4 6 4 4 1 3 4 3 6 3 3 1 5
    ## [94285] 7 1 4 4 5 5 3 1 3 1 3 6 4 4 3 6 6 3 3 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 3
    ## [94321] 3 3 6 6 6 1 5 4 1 6 4 2 3 3 3 1 2 6 5 7 2 2 2 4 6 3 4 1 4 4 4 4 5 5 4 4
    ## [94357] 4 2 5 5 4 1 4 3 2 3 5 3 6 6 6 3 3 6 3 4 1 6 6 4 4 2 4 4 6 4 4 4 4 3 6 6
    ## [94393] 6 3 3 5 4 5 5 1 4 5 4 2 5 6 5 4 4 5 4 5 5 1 5 2 5 6 5 5 1 3 4 4 5 4 1 5
    ## [94429] 5 3 4 5 3 4 1 3 3 4 2 4 4 4 4 6 4 4 3 5 4 4 4 4 6 4 4 1 6 5 4 6 6 3 5 6
    ## [94465] 6 1 5 3 4 6 5 5 2 6 6 4 5 6 2 3 2 2 6 6 6 2 6 6 1 6 3 3 1 4 2 5 5 5 5 5
    ## [94501] 4 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 2 5 2 5 3 3 3 1 3 1 1 5 5 4 2 5 1 2 2 5
    ## [94537] 2 2 2 6 4 4 6 3 6 4 6 3 3 1 5 4 2 2 5 1 4 3 3 4 3 1 4 2 3 6 6 4 2 1 1 1
    ## [94573] 5 5 5 5 5 5 5 5 5 3 4 4 4 4 3 6 6 6 4 6 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [94609] 5 5 5 5 5 5 5 2 5 5 6 3 3 2 2 2 2 4 2 4 6 2 1 4 2 6 4 2 2 4 2 3 5 3 3 6
    ## [94645] 1 6 5 5 3 2 5 2 5 5 1 6 3 1 2 1 4 3 3 5 4 3 1 1 4 3 5 4 3 3 5 2 5 5 1 3
    ## [94681] 1 3 5 5 5 5 5 5 5 5 5 5 4 3 4 2 5 2 4 4 6 5 4 4 4 4 4 3 4 4 4 5 4 2 2 3
    ## [94717] 1 3 1 6 3 4 4 3 1 3 5 6 1 5 3 2 3 3 4 3 3 4 5 5 4 1 4 6 4 6 6 6 4 4 4 7
    ## [94753] 4 5 4 5 5 5 5 4 5 5 4 5 4 5 5 5 5 5 3 5 3 3 5 3 5 3 5 1 3 3 3 3 5 2 4 3
    ## [94789] 4 4 3 6 6 3 4 4 4 3 3 4 3 4 1 4 3 3 5 3 4 5 3 5 5 4 4 6 6 6 6 5 6 6 5 6
    ## [94825] 4 5 5 4 1 1 1 6 1 1 6 1 1 1 5 1 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 2 5
    ## [94861] 5 5 5 4 5 5 5 4 2 1 3 4 4 1 5 2 2 2 2 3 2 3 6 3 5 6 6 5 3 4 4 3 4 5 6 4
    ## [94897] 5 4 6 4 6 6 4 6 3 5 4 4 1 6 1 5 6 3 6 1 2 1 2 6 1 5 6 2 5 2 5 6 3 4 4 6
    ## [94933] 2 2 2 1 5 1 5 5 5 1 2 2 3 3 6 6 3 4 6 4 6 6 4 6 3 4 5 2 6 2 4 6 2 2 2 1
    ## [94969] 3 4 4 4 3 6 5 4 4 3 3 6 4 4 3 3 4 6 5 4 4 4 4 2 3 3 6 3 3 4 1 1 3 1 5 5
    ## [95005] 5 5 5 1 2 4 2 2 2 6 5 4 6 5 1 5 1 5 2 2 2 2 6 2 2 3 3 4 5 3 4 3 4 4 5 1
    ## [95041] 1 5 5 6 1 3 2 5 2 2 2 2 2 4 2 6 4 1 6 6 4 6 4 6 6 5 5 4 4 4 4 5 4 4 4 4
    ## [95077] 4 4 4 4 4 4 3 5 6 3 3 3 3 4 6 4 3 5 5 4 1 3 5 6 1 6 5 4 5 6 4 3 4 5 5 1
    ## [95113] 1 5 4 6 1 5 3 5 6 5 4 5 3 5 5 4 5 5 5 5 6 1 5 6 6 5 4 4 5 6 6 6 4 4 6 3
    ## [95149] 6 5 5 5 2 5 5 5 5 5 5 3 5 5 5 3 3 4 1 1 6 3 4 6 4 6 4 5 4 6 6 6 4 4 6 2
    ## [95185] 6 4 4 4 4 5 3 6 4 1 3 3 2 4 2 2 1 2 5 3 4 3 5 1 5 5 5 4 6 6 5 3 3 2 2 2
    ## [95221] 2 2 6 6 2 1 2 2 2 1 4 5 5 2 2 5 5 5 3 5 1 5 3 4 6 5 5 4 5 6 5 5 5 3 2 3
    ## [95257] 2 3 4 3 3 2 4 6 4 4 4 1 2 3 2 4 3 6 4 3 1 5 7 5 5 4 6 4 4 5 4 3 4 3 4 3
    ## [95293] 3 1 4 4 3 4 5 5 4 3 3 3 1 5 5 1 4 5 4 3 3 1 3 3 3 5 3 5 2 5 4 3 4 4 4 4
    ## [95329] 5 2 2 3 1 4 2 1 2 3 4 5 3 3 2 2 2 2 2 2 2 2 2 5 2 2 2 5 5 2 5 5 2 2 2 2
    ## [95365] 5 4 7 6 3 6 3 3 6 6 4 1 2 1 1 2 1 1 5 1 5 1 1 1 1 3 3 2 5 5 5 5 5 5 6 6
    ## [95401] 3 6 6 4 4 6 3 6 5 4 6 6 6 4 5 1 3 1 1 2 6 6 4 5 4 4 6 4 5 4 6 6 6 6 3 4
    ## [95437] 3 6 4 6 1 4 6 2 1 4 1 3 5 6 4 6 1 2 2 1 2 5 1 5 1 2 5 2 5 2 5 2 4 2 3 6
    ## [95473] 3 6 3 3 3 3 3 4 3 3 5 3 5 4 1 1 2 5 5 4 4 1 5 5 5 5 1 5 5 5 5 2 5 5 5 5
    ## [95509] 5 5 2 5 5 1 5 5 5 2 3 5 2 5 5 3 5 3 4 4 4 1 4 6 3 3 4 6 2 4 6 6 4 6 4 6
    ## [95545] 6 6 4 4 4 5 3 5 3 3 8 4 3 4 3 4 5 1 5 4 5 5 7 3 1 2 4 1 3 1 4 4 5 3 1 6
    ## [95581] 3 6 2 2 3 3 4 3 3 3 6 1 3 1 5 3 4 5 5 5 5 5 5 5 5 5 5 5 4 4 5 5 4 6 4 8
    ## [95617] 5 5 3 3 1 7 1 5 2 2 5 1 1 5 5 3 1 6 6 4 6 6 6 6 6 4 4 2 3 6 3 6 4 4 6 4
    ## [95653] 6 6 6 6 6 3 6 6 3 6 6 6 4 6 6 6 5 7 6 3 2 4 6 6 1 3 4 6 4 6 4 4 4 2 4 6
    ## [95689] 4 3 3 4 5 6 3 3 3 4 2 3 2 4 5 2 6 4 4 4 1 3 3 4 5 1 2 3 2 6 1 2 2 6 2 2
    ## [95725] 2 2 2 8 3 5 5 5 1 5 5 1 3 3 3 3 5 1 5 3 5 2 5 5 5 5 5 5 5 6 1 2 6 4 3 4
    ## [95761] 3 1 6 5 3 3 4 6 4 5 1 5 4 1 5 4 4 3 4 1 1 5 1 4 1 4 4 1 1 5 5 4 5 5 5 4
    ## [95797] 5 1 1 1 7 6 3 5 3 5 5 5 1 1 4 1 5 5 5 1 3 4 4 3 2 3 2 6 6 6 2 2 2 6 6 2
    ## [95833] 5 4 4 2 1 2 4 2 2 5 2 2 2 6 5 1 3 3 2 2 2 5 5 6 2 2 3 3 5 4 4 3 6 2 2 1
    ## [95869] 3 3 1 6 5 6 4 2 2 2 5 2 2 2 5 5 5 2 5 5 5 3 5 6 4 4 5 5 3 5 4 2 2 7 5 6
    ## [95905] 6 1 1 6 2 1 1 2 2 1 2 2 2 5 2 3 5 3 3 2 4 2 2 2 3 5 5 2 2 2 2 2 2 5 2 2
    ## [95941] 2 2 2 3 5 5 2 4 2 2 2 2 2 5 2 2 5 5 2 2 2 2 2 5 5 2 2 4 1 4 4 5 1 3 2 5
    ## [95977] 4 4 5 4 1 6 4 3 4 6 4 1 5 1 1 1 1 1 1 1 1 1 5 1 1 1 1 1 4 3 5 3 1 3 1 3
    ## [96013] 1 4 4 4 4 3 4 2 6 4 4 4 6 4 6 4 5 4 6 4 4 5 6 4 4 6 6 6 4 4 3 2 2 3 1 5
    ## [96049] 4 1 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 4 6 3 4 3 1 2 1 1 1
    ## [96085] 3 6 2 1 5 3 8 8 7 5 5 1 6 6 1 4 3 6 4 2 4 6 6 4 4 4 4 6 6 6 4 4 4 6 5 4
    ## [96121] 3 5 2 3 3 4 1 3 3 6 5 6 3 6 5 4 3 4 6 6 4 1 5 2 5 2 4 4 3 4 3 1 3 3 1 3
    ## [96157] 5 5 3 5 5 1 4 1 5 2 6 6 4 6 2 4 6 6 5 4 3 4 4 4 4 2 2 4 6 6 6 6 6 3 4 6
    ## [96193] 6 3 3 4 6 3 6 5 3 3 2 4 3 2 1 3 3 6 6 4 3 3 3 3 1 4 4 4 1 6 4 5 1 3 1 5
    ## [96229] 5 5 5 5 5 5 5 4 4 5 3 6 4 3 3 4 5 5 5 2 5 5 6 5 3 2 6 2 3 2 4 1 3 1 2 2
    ## [96265] 2 2 2 2 2 2 1 2 2 5 2 1 5 7 2 2 2 4 2 2 4 4 4 3 3 1 6 3 4 6 1 3 1 3 3 3
    ## [96301] 1 1 4 4 5 4 3 4 4 3 4 4 3 4 6 4 3 5 5 5 3 6 6 3 3 5 5 5 6 5 1 3 6 5 2 3
    ## [96337] 4 1 6 4 6 6 4 4 4 4 4 6 4 6 4 6 4 4 4 4 4 3 5 5 5 5 5 6 6 6 6 6 6 6 4 6
    ## [96373] 4 4 4 5 4 2 5 5 5 2 5 3 6 6 4 2 6 6 2 5 2 6 2 6 6 4 6 1 4 5 5 3 4 4 6 4
    ## [96409] 6 4 6 6 3 6 1 5 1 1 5 5 5 4 5 1 1 1 4 3 4 4 3 3 4 2 6 5 5 6 5 3 4 6 6 3
    ## [96445] 4 4 4 6 3 3 3 3 3 6 4 4 6 2 6 3 6 4 4 6 4 3 3 6 4 6 4 4 3 5 5 4 4 5 5 4
    ## [96481] 3 5 5 4 3 4 4 5 4 4 6 6 6 6 4 4 4 6 6 1 5 1 1 1 2 4 1 3 5 3 5 2 3 4 4 5
    ## [96517] 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 4 5 5 5 5 4 3 3 6 3 3 6 3 6 6 6 4 6
    ## [96553] 6 4 5 4 4 4 4 6 4 6 4 6 6 6 5 5 2 3 7 4 5 6 5 3 3 3 4 3 6 2 6 2 5 5 4 5
    ## [96589] 5 5 5 4 5 3 3 6 6 4 3 5 3 5 5 4 7 5 5 1 5 4 5 5 5 5 5 5 3 5 5 5 5 5 5 5
    ## [96625] 3 5 5 5 5 1 1 5 1 4 5 6 6 4 4 6 6 4 6 6 6 4 4 6 6 4 6 6 6 6 4 6 6 6 1 2
    ## [96661] 6 3 6 6 2 6 6 4 2 1 3 6 6 6 6 4 6 6 3 1 3 3 5 3 5 2 3 3 4 4 5 1 5 5 5 6
    ## [96697] 2 5 3 5 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 4
    ## [96733] 1 3 6 6 6 4 6 6 2 2 4 2 3 3 3 3 4 1 2 6 2 4 6 4 2 2 5 2 4 2 4 2 5 3 3 4
    ## [96769] 2 6 5 4 2 4 5 3 5 1 5 5 3 4 1 4 4 2 6 6 5 1 1 3 5 2 3 6 6 6 6 4 6 4 6 6
    ## [96805] 6 4 6 6 4 4 4 6 4 5 4 6 4 4 3 5 4 4 3 3 5 3 3 1 1 2 4 1 2 5 5 5 5 5 5 5
    ## [96841] 5 5 5 5 5 3 6 5 2 5 5 5 5 1 4 3 4 3 2 3 5 6 5 5 2 6 5 5 3 5 5 5 4 5 5 3
    ## [96877] 4 4 3 5 4 6 4 6 4 3 4 6 3 4 4 1 6 3 1 3 1 6 6 3 1 3 4 4 6 3 5 5 5 5 1 1
    ## [96913] 5 5 1 6 4 1 4 6 4 4 6 6 2 5 6 4 6 6 4 3 6 3 4 4 1 3 5 5 4 3 5 5 5 5 1 5
    ## [96949] 3 5 5 1 3 3 5 5 1 2 2 2 1 2 2 2 4 1 1 2 2 2 2 2 5 1 4 2 2 1 2 2 2 1 2 2
    ## [96985] 2 2 2 2 2 5 2 5 5 2 2 1 4 7 2 4 5 5 4 4 4 2 2 5 3 3 3 1 4 6 6 6 1 6 4 4
    ## [97021] 2 2 4 4 6 6 2 5 4 4 3 1 1 1 1 3 5 1 5 1 5 5 3 2 4 2 4 1 6 2 6 6 2 2 4 2
    ## [97057] 7 5 4 2 4 6 7 1 5 5 5 5 5 5 1 5 3 5 5 5 5 5 5 5 5 4 5 5 2 5 5 6 4 1 3 5
    ## [97093] 3 4 4 2 3 4 1 1 3 5 2 2 1 2 4 1 6 5 5 4 4 6 6 2 2 4 4 5 6 4 4 2 5 4 5 6
    ## [97129] 6 5 1 3 4 4 6 5 4 4 5 4 2 4 5 6 4 4 4 6 2 6 3 1 3 5 5 5 5 5 5 5 5 5 5 5
    ## [97165] 5 5 5 5 1 5 5 1 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 4 5 1 5 5 5 5 5 4 5 5 5 4
    ## [97201] 4 3 1 4 3 6 6 4 6 4 4 4 4 5 4 3 6 6 6 3 6 4 5 4 1 6 6 3 3 3 3 3 1 5 1 3
    ## [97237] 4 4 4 1 5 4 3 3 3 5 3 4 5 3 4 6 7 4 2 2 2 2 1 2 1 3 8 2 5 6 6 5 5 4 1 5
    ## [97273] 6 6 5 6 4 6 6 5 4 4 5 5 5 3 6 6 6 5 3 5 4 2 6 6 6 5 6 5 1 1 5 5 5 1 3 5
    ## [97309] 6 4 3 5 5 5 5 4 5 5 4 5 5 6 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 1
    ## [97345] 1 1 1 6 5 5 5 2 5 5 5 5 5 5 5 5 2 6 5 5 5 3 5 5 5 5 5 5 5 5 1 3 3 5 1 5
    ## [97381] 2 5 2 5 4 2 2 1 2 2 2 2 5 5 2 2 1 2 2 5 2 5 2 5 5 3 5 3 4 4 3 5 3 4 1 4
    ## [97417] 3 3 3 3 6 4 3 6 6 6 3 4 4 3 6 5 4 4 3 6 4 6 5 4 1 3 5 3 5 4 5 5 3 5 4 3
    ## [97453] 5 5 3 5 5 4 5 4 5 4 4 3 4 6 5 3 3 1 4 5 8 3 5 5 3 5 5 5 5 5 5 5 5 5 4 8
    ## [97489] 2 1 1 2 6 6 2 3 5 3 1 5 5 5 5 1 5 3 4 1 5 2 3 3 3 3 5 5 3 6 4 1 5 3 5 5
    ## [97525] 1 6 6 4 5 2 2 6 7 4 3 5 6 6 1 5 3 4 4 4 4 4 4 4 4 3 4 4 4 4 6 2 4 4 4 6
    ## [97561] 4 6 4 4 4 4 4 2 4 4 3 3 3 6 6 2 6 5 5 3 6 5 5 5 3 5 6 2 5 7 6 3 2 2 2 2
    ## [97597] 2 2 2 4 2 5 5 2 2 2 4 5 2 4 2 6 5 2 2 5 6 2 2 2 2 5 2 2 5 2 5 4 4 4 6 6
    ## [97633] 6 6 4 4 2 5 2 1 4 4 6 6 3 3 6 4 3 3 5 5 5 3 3 5 6 6 1 5 5 5 3 1 5 3 3 5
    ## [97669] 3 3 4 7 1 2 1 1 2 6 2 2 7 1 5 4 7 5 3 7 4 6 3 1 5 4 1 3 2 4 5 3 4 4 3 6
    ## [97705] 5 6 4 6 1 6 4 5 6 4 2 2 3 5 1 1 1 3 3 4 5 5 1 6 5 5 5 5 5 3 4 5 5 4 2 6
    ## [97741] 2 2 3 6 3 6 2 6 6 6 4 3 4 3 6 4 6 3 6 4 6 4 6 4 3 4 4 1 5 5 1 1 4 4 4 4
    ## [97777] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 3 5 5 5 5 5 5 5 1 1 5
    ## [97813] 5 6 4 6 6 6 3 6 6 3 4 6 4 6 4 6 6 6 5 5 5 3 2 4 2 2 2 4 2 2 3 2 2 2 2 3
    ## [97849] 1 4 7 5 1 3 6 6 3 3 6 3 3 5 5 8 3 5 5 6 5 5 5 5 5 5 2 5 5 5 5 5 5 5 5 1
    ## [97885] 5 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 5 5 5 5 5 5 5 5 5
    ## [97921] 5 5 5 5 5 1 6 3 5 3 5 6 1 2 6 6 4 6 6 4 4 6 4 4 3 4 5 5 1 5 1 3 4 3 5 5
    ## [97957] 1 3 5 1 4 3 4 3 4 3 4 4 4 4 4 4 4 6 4 6 6 3 4 4 3 4 3 6 5 4 4 3 6 6 4 6
    ## [97993] 4 6 4 6 4 4 6 4 4 6 5 4 6 5 6 4 3 4 4 6 6 6 4 4 5 4 4 6 6 6 6 5 4 3 4 6
    ## [98029] 6 5 5 5 5 5 3 5 5 3 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 3 3 5 5 3 5 5 5 5
    ## [98065] 5 5 5 5 5 5 4 4 4 1 1 5 3 5 3 5 1 5 2 2 5 5 5 5 2 5 5 2 5 2 5 6 5 2 5 3
    ## [98101] 5 3 1 1 5 5 5 6 5 5 5 5 5 6 5 2 5 2 5 1 3 5 3 5 5 3 4 3 4 3 5 6 4 4 3 3
    ## [98137] 6 4 5 3 6 4 5 5 6 6 5 4 4 5 1 2 1 5 6 3 1 5 2 4 4 6 3 4 6 5 1 6 4 3 3 7
    ## [98173] 4 1 6 2 4 4 6 1 6 3 3 4 1 6 7 4 4 5 6 6 3 6 4 6 4 4 4 4 3 1 4 4 6 4 4 4
    ## [98209] 4 6 4 4 4 5 4 4 3 3 1 5 4 5 3 3 1 1 3 4 4 1 5 5 3 5 4 4 4 2 4 1 4 4 2 4
    ## [98245] 5 4 4 2 4 4 4 4 4 4 5 6 5 5 5 5 5 5 5 5 6 4 3 5 5 5 5 3 6 4 3 6 4 4 4 4
    ## [98281] 3 3 4 3 3 3 3 5 4 4 4 3 4 6 3 6 5 4 6 6 5 6 6 5 4 4 4 6 6 4 5 4 4 4 4 3
    ## [98317] 6 3 6 3 3 3 3 6 3 4 4 3 6 3 4 3 3 6 6 3 3 3 3 6 3 3 6 3 3 3 3 3 6 3 3 6
    ## [98353] 6 6 6 3 6 3 6 3 6 4 5 4 5 1 5 3 4 4 3 3 5 4 4 4 4 4 5 5 4 3 4 4 4 3 4 6
    ## [98389] 3 3 1 5 6 6 4 2 6 4 4 2 6 4 4 5 4 4 1 4 3 6 4 3 7 2 4 4 5 5 3 5 4 1 7 5
    ## [98425] 5 4 3 4 3 2 6 1 4 1 5 2 1 2 5 5 3 3 3 3 6 6 6 1 3 5 5 4 4 6 4 6 6 6 5 5
    ## [98461] 5 5 5 1 4 4 3 2 2 7 4 4 4 5 2 2 1 4 1 3 1 3 2 1 3 4 3 5 4 5 5 5 5 8 4 3
    ## [98497] 5 5 3 2 4 2 2 2 4 4 3 1 5 4 6 6 3 3 5 5 4 4 5 2 2 2 8 2 1 1 2 3 1 1 3 4
    ## [98533] 3 5 2 1 5 5 5 5 2 5 5 3 5 5 5 2 5 5 2 5 5 5 3 5 5 3 4 1 3 3 3 5 4 5 3 1
    ## [98569] 3 1 5 3 5 1 3 1 6 4 4 1 5 6 1 3 3 5 5 3 2 3 3 8 1 4 3 3 5 5 6 5 2 6 1 3
    ## [98605] 4 4 4 5 3 3 3 3 5 5 3 1 1 3 3 5 3 1 5 4 6 6 4 4 3 6 1 5 5 5 2 5 5 5 4 6
    ## [98641] 3 5 4 6 4 4 4 6 4 5 3 5 5 1 1 5 1 1 6 3 4 6 6 5 4 6 6 4 4 6 3 1 5 3 1 4
    ## [98677] 6 4 6 2 1 3 2 4 1 2 4 2 7 2 6 4 6 3 5 3 1 6 3 4 1 3 6 1 1 5 5 2 5 5 5 2
    ## [98713] 2 5 2 3 5 6 5 2 5 6 3 5 5 4 4 1 2 4 3 5 3 6 4 4 4 4 4 4 6 4 6 6 4 6 3 5
    ## [98749] 5 5 1 5 5 4 6 6 3 4 5 4 4 6 6 3 1 1 4 5 5 5 2 6 4 5 5 2 5 3 2 6 5 5 5 6
    ## [98785] 7 5 6 6 4 4 4 5 4 1 2 5 4 6 3 1 3 4 1 3 6 6 4 3 4 1 3 1 5 6 5 2 1 1 5 2
    ## [98821] 6 3 6 4 5 3 5 1 1 3 3 1 2 3 6 4 4 3 6 1 2 1 3 3 5 3 3 3 4 6 6 6 6 4 4 4
    ## [98857] 6 4 5 6 5 3 4 3 4 6 3 6 3 1 5 5 6 4 3 4 4 6 4 4 6 6 3 6 5 4 4 1 5 5 5 3
    ## [98893] 1 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 2 4 5 5 5 1 5 5 5 3 7 5 4 4 4 4 2 5
    ## [98929] 3 3 5 5 5 2 2 2 2 3 2 2 5 2 2 2 1 2 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [98965] 2 2 2 2 2 2 6 2 2 6 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2 2 2
    ## [99001] 6 2 2 2 2 2 4 5 5 2 4 3 7 4 5 6 5 5 2 1 5 4 4 1 4 4 4 4 6 5 6 7 6 3 3 3
    ## [99037] 2 5 3 4 4 3 3 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 2 3 4 4 4
    ## [99073] 4 4 4 4 4 5 4 3 5 1 2 4 4 2 7 1 2 4 6 1 3 4 6 4 6 7 2 5 1 3 5 5 5 5 4 5
    ## [99109] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [99145] 3 3 3 3 3 3 3 3 6 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [99181] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3
    ## [99217] 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [99253] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 5 5 5 3 5 2 6 5 6 6 4 6 6 4 4 4
    ## [99289] 6 1 4 1 6 4 6 3 3 3 4 3 3 6 1 5 6 4 3 5 1 3 1 1 1 3 3 5 5 5 1 5 5 5 5 5
    ## [99325] 1 1 4 1 5 5 5 5 5 5 5 2 5 5 5 2 5 5 5 5 5 5 5 1 5 1 4 3 5 7 1 6 4 4 4 4
    ## [99361] 6 3 6 4 4 4 2 3 4 6 4 1 3 1 5 4 4 2 1 3 1 5 5 1 5 3 1 4 1 2 5 1 3 1 2 3
    ## [99397] 6 2 5 4 4 3 4 4 2 2 4 4 1 4 2 1 5 4 4 4 4 4 4 4 6 2 6 3 1 5 5 5 3 5 6 6
    ## [99433] 6 6 2 6 6 4 5 2 4 4 5 6 4 6 6 4 2 4 6 6 6 7 4 3 5 3 2 6 1 1 4 1 4 3 3 5
    ## [99469] 3 5 5 2 6 5 6 4 4 6 6 4 6 4 4 5 3 3 3 6 6 3 4 4 3 6 1 6 6 4 4 6 3 6 4 3
    ## [99505] 2 5 6 5 4 6 2 6 4 6 2 3 2 4 2 5 4 4 3 1 1 6 4 6 6 4 6 5 6 5 6 4 4 1 5 5
    ## [99541] 6 5 3 4 4 4 5 5 1 3 4 6 4 5 4 1 3 3 3 4 6 4 4 3 6 6 6 6 5 6 4 5 5 3 6 4
    ## [99577] 5 6 2 5 1 4 4 3 5 5 4 5 5 3 5 5 6 5 3 3 3 6 4 5 2 5 4 4 5 5 4 4 3 5 2 6
    ## [99613] 3 4 5 2 2 4 5 1 4 4 3 6 3 1 3 3 5 6 5 5 5 3 3 1 4 6 6 4 2 5 4 6 2 2 6 2
    ## [99649] 4 2 4 6 7 4 3 3 4 3 3 5 6 1 4 3 3 3 4 4 4 4 4 1 4 4 7 2 5 2 4 5 4 5 4 2
    ## [99685] 4 2 6 4 4 6 4 2 4 2 2 2 2 3 4 4 2 2 6 6 3 3 2 2 2 6 3 2 6 4 4 6 2 2 4 2
    ## [99721] 4 6 6 3 2 2 2 6 3 7 4 3 5 2 1 4 5 5 4 4 5 5 5 4 5 5 5 6 6 4 6 3 6 2 5 5
    ## [99757] 3 5 5 3 3 2 4 3 5 4 4 5 4 4 5 4 3 5 4 3 1 3 3 3 3 4 3 3 6 4 6 4 6 2 2 2
    ## [99793] 2 2 2 4 2 1 2 2 2 2 5 3 4 2 6 4 2 1 4 2 1 6 3 4 6 6 4 4 4 6 4 6 4 6 4 6
    ## [99829] 4 6 3 3 5 5 1 4 4 2 3 4 5 5 1 1 2 1 3 6 6 6 5 6 3 1 5 3 1 3 4 1 3 5 5 1
    ## [99865] 3 3 6 6 5 5 5 5 4 4 5 6 5 3 2 2 3 2 2 3 1 3 5 3 1 4 3 3 6 3 3 6 3 1 1 1
    ## [99901] 3 6 6 6 3 5 5 5 3 1 5 2 3 4 4 3 3 5 5 1 3 1 4 2 4 1 3 6 2 3 4 2 4 6 4 6
    ## [99937] 4 4 1 3 3 6 4 6 4 4 4 4 6 2 3 5 4 5 5 6 1 3 3 3 6 3 4 4 6 5 5 4 5 5 5 4
    ## [99973] 3 5 5 4 2 5 5 5 4 5 4 4 6 5 1 3 5 3 3 5 3 4 4 6 4 6 6
    ##  [ reached getOption("max.print") -- omitted 948568 entries ]
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  931076.31 2112493.56 3695324.95 1808309.40 1637217.26 2212835.98   32876.81
    ## [8] 1341570.73
    ##  (between_SS / total_SS =  40.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
# visualize clusters

fviz_cluster(clusters1,donation_cluster,ellipse.type="norm",geom="point")
```

![](final_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
ggplot(donation_cluster,aes(clusters1$cluster))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
ggplot(do2,aes(payment_method))+geom_bar()+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

``` r
ggplot(do2,aes(payment_method))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-3.png)<!-- -->

``` r
ggplot(do2,aes(dollar_amount))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-4.png)<!-- -->

``` r
ggplot(do2,aes(dollar_amount))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-5.png)<!-- -->

``` r
ggplot(do2,aes(is_teacher_acct))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-6.png)<!-- -->

``` r
ggplot(do2,aes(is_teacher_acct))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-7.png)<!-- -->

``` r
ggplot(do2,aes(donation_included_optional_support))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-8.png)<!-- -->

``` r
ggplot(do2,aes(donation_included_optional_support))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-9.png)<!-- -->

``` r
ggplot(do2,aes(payment_included_acct_credit))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-10.png)<!-- -->

``` r
ggplot(do2,aes(payment_included_acct_credit))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-11.png)<!-- -->

``` r
ggplot(do2,aes(payment_included_campaign_gift_card))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-12.png)<!-- -->

``` r
ggplot(do2,aes(payment_included_campaign_gift_card))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-13.png)<!-- -->

``` r
ggplot(do2,aes(payment_included_web_purchased_gift_card))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-14.png)<!-- -->

``` r
ggplot(do2,aes(payment_included_web_purchased_gift_card))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-15.png)<!-- -->

``` r
ggplot(do2,aes(payment_was_promo_matched))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-16.png)<!-- -->

``` r
ggplot(do2,aes(payment_was_promo_matched))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-17.png)<!-- -->

``` r
ggplot(do2,aes(via_giving_page))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-18.png)<!-- -->

``` r
ggplot(do2,aes(via_giving_page))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-19.png)<!-- -->

``` r
ggplot(do2,aes(for_honoree))+geom_bar()
```

![](final_files/figure-gfm/unnamed-chunk-26-20.png)<!-- -->

``` r
ggplot(do2,aes(for_honoree))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](final_files/figure-gfm/unnamed-chunk-26-21.png)<!-- -->

``` r
ggplot(do2,aes(x=donation_to_project))+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
ggplot(do2,aes(x=donation_to_project))+geom_histogram() + facet_wrap(~clusters1$cluster)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

``` r
ggplot(do2,aes(x=donation_optional_support))+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-27-3.png)<!-- -->

``` r
ggplot(do2,aes(x=donation_optional_support))+geom_histogram() + facet_wrap(~clusters1$cluster)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-27-4.png)<!-- -->

``` r
ggplot(do2,aes(x=donation_total))+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-27-5.png)<!-- -->

``` r
ggplot(do2,aes(x=donation_total))+geom_histogram() + facet_wrap(~clusters1$cluster)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](final_files/figure-gfm/unnamed-chunk-27-6.png)<!-- -->

``` r
do2 %>% 
  group_by(clusters1$cluster) %>% 
  dplyr::summarize(median_donation_to_project = round(median(donation_to_project)),
            min_donation_to_project = round(min(donation_to_project)),
            max_donation_to_project = round(max(donation_to_project)),
            median_donation_optional_support = round(median(donation_optional_support)),
            min_donation_optional_support = round(min(donation_optional_support)),
            max_donation_optional_support = round(max(donation_optional_support)),
            median_donation_total = median(donation_total),
            min_donation_total = min(donation_total),
            max_donation_total = max(donation_total),
            )
```

    ## # A tibble: 8 × 10
    ##   clusters1$cl…¹ media…² min_d…³ max_d…⁴ media…⁵ min_d…⁶ max_d…⁷ media…⁸ min_d…⁹
    ##            <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1              1      42       0    2268       8       0     319     50     0.01
    ## 2              2       3     -12     278       0       0      49      3   -11.8 
    ## 3              3     144       0    2307      23       0     335    168.   53.6 
    ## 4              4      21       0     100       4       0      28     25    10   
    ## 5              5      17       8     345       2       0      38     20    10   
    ## 6              6      21       8     100       4       0      24     25    10   
    ## 7              7     309       2    1808      55       0     340    365.    2.47
    ## 8              8    2004    1200   85000     340       0   15000   2357. 1500   
    ## # … with 1 more variable: max_donation_total <dbl>, and abbreviated variable
    ## #   names ¹​`clusters1$cluster`, ²​median_donation_to_project,
    ## #   ³​min_donation_to_project, ⁴​max_donation_to_project,
    ## #   ⁵​median_donation_optional_support, ⁶​min_donation_optional_support,
    ## #   ⁷​max_donation_optional_support, ⁸​median_donation_total, ⁹​min_donation_total
    ## # ℹ Use `colnames()` to see all variable names

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Cmd+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
