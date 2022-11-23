cluster
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

## Load Libraries

``` r
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
```

``` r
movie <- read_csv("responses_fin.csv") %>% clean_names()
```

    ## Rows: 1010 Columns: 150
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): Smoking, Alcohol, Punctuality, Lying, Internet usage, Gender, Lef...
    ## dbl (139): Music, Slow songs or fast songs, Dance, Folk, Country, Classical ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
skim(movie)
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
movie
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
1010
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
150
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
11
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
139
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
smoking
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
14
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
alcohol
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
14
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
punctuality
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
23
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
lying
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
29
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
internet_usage
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
23
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
gender
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
left_right_handed
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
education
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
32
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
only_child
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
village_town
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
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
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
house_block_of_flats
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
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
music
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.73
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
slow_songs_or_fast_songs
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▇▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
dance
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▆▇▆▃
</td>
</tr>
<tr>
<td style="text-align:left;">
folk
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.29
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▆▂▂
</td>
</tr>
<tr>
<td style="text-align:left;">
country
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.12
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▅▂▁
</td>
</tr>
<tr>
<td style="text-align:left;">
classical_music
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▇▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
musical
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.76
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▇▇▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
pop
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
1.16
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▆▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
rock
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.76
</td>
<td style="text-align:right;">
1.18
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▅▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
metal_or_hardrock
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.36
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▃▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
punk
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.46
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▆▆▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
hiphop_rap
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
1.38
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▇▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
reggae_ska
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.77
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▇▇▆▂
</td>
</tr>
<tr>
<td style="text-align:left;">
swing_jazz
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.76
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▇▇▆▃
</td>
</tr>
<tr>
<td style="text-align:left;">
rock_n\_roll
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
1.24
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▅▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
alternative
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
latino
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.84
</td>
<td style="text-align:right;">
1.33
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▇▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
techno_trance
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.34
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▅▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
opera
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.14
</td>
<td style="text-align:right;">
1.18
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▆▃▂▁
</td>
</tr>
<tr>
<td style="text-align:left;">
m_target
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▁▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
horror
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.79
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
1
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
5
</td>
<td style="text-align:left;">
▇▆▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
thriller
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
1.20
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▅▆▇▅
</td>
</tr>
<tr>
<td style="text-align:left;">
comedy
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.49
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▁▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
romantic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
1.21
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▅▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
sci_fi
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
1.31
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▇▇▆▆
</td>
</tr>
<tr>
<td style="text-align:left;">
war
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▆▇▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
fantasy_fairy_tales
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.75
</td>
<td style="text-align:right;">
1.18
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▃▆▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
animated
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
1.22
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▅▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
documentary
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.64
</td>
<td style="text-align:right;">
1.13
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▃▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
western
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.13
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▅▂▁
</td>
</tr>
<tr>
<td style="text-align:left;">
action
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.54
</td>
<td style="text-align:right;">
1.24
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▅▆▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
history
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.21
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▆▇▆▆
</td>
</tr>
<tr>
<td style="text-align:left;">
psychology
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▇▇▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
politics
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.60
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▇▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
mathematics
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.33
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▃▅▂▂
</td>
</tr>
<tr>
<td style="text-align:left;">
physics
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.06
</td>
<td style="text-align:right;">
1.23
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▃▂▁
</td>
</tr>
<tr>
<td style="text-align:left;">
internet
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.18
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▃▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
pc
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▆▇▆▆
</td>
</tr>
<tr>
<td style="text-align:left;">
economy_management
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.64
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▆▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
biology
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.67
</td>
<td style="text-align:right;">
1.38
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▆▃▅
</td>
</tr>
<tr>
<td style="text-align:left;">
chemistry
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.17
</td>
<td style="text-align:right;">
1.38
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▂▂▂
</td>
</tr>
<tr>
<td style="text-align:left;">
reading
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▅▆▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
geography
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.08
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▆▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
foreign_languages
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.78
</td>
<td style="text-align:right;">
1.14
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▆▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
medicine
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.52
</td>
<td style="text-align:right;">
1.38
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▆▆▂▃
</td>
</tr>
<tr>
<td style="text-align:left;">
law
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.26
</td>
<td style="text-align:right;">
1.24
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▆▅▂▂
</td>
</tr>
<tr>
<td style="text-align:left;">
cars
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
1.44
</td>
<td style="text-align:right;">
1
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
5
</td>
<td style="text-align:left;">
▇▆▅▅▅
</td>
</tr>
<tr>
<td style="text-align:left;">
art_exhibitions
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.59
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▇▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
religion
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.27
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▃▂▂
</td>
</tr>
<tr>
<td style="text-align:left;">
countryside_outdoors
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.69
</td>
<td style="text-align:right;">
1.20
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▆▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
dancing
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.46
</td>
<td style="text-align:right;">
1.45
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▃▂▃
</td>
</tr>
<tr>
<td style="text-align:left;">
musical_instruments
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.32
</td>
<td style="text-align:right;">
1.51
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▂▂▂▃
</td>
</tr>
<tr>
<td style="text-align:left;">
writing
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.90
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▂▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
passive_sport
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▃▆▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
active_sport
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▃▅▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
gardening
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.91
</td>
<td style="text-align:right;">
1.18
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▃▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
celebrities
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.36
</td>
<td style="text-align:right;">
1.27
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▆▅▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
shopping
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▆▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
science_and_technology
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.23
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▅▇▆▆
</td>
</tr>
<tr>
<td style="text-align:left;">
theatre
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
1.33
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▆▇▆▆
</td>
</tr>
<tr>
<td style="text-align:left;">
fun_with_friends
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.56
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▁▂▇
</td>
</tr>
<tr>
<td style="text-align:left;">
adrenaline_sports
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.95
</td>
<td style="text-align:right;">
1.42
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▇▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
pets
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
1.55
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▃▃▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
flying
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.06
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▃▃▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
storm
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.97
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
darkness
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.25
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▃▂▂
</td>
</tr>
<tr>
<td style="text-align:left;">
heights
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.62
</td>
<td style="text-align:right;">
1.30
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▇▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
spiders
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
1.54
</td>
<td style="text-align:right;">
1
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
5
</td>
<td style="text-align:left;">
▇▆▅▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
snakes
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.03
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▆▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
rats
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.41
</td>
<td style="text-align:right;">
1.40
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▃▃▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
ageing
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.58
</td>
<td style="text-align:right;">
1.39
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▆▃▃
</td>
</tr>
<tr>
<td style="text-align:left;">
dangerous_dogs
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▆▇▆▆
</td>
</tr>
<tr>
<td style="text-align:left;">
fear_of_public_speaking
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▆▇▅▂
</td>
</tr>
<tr>
<td style="text-align:left;">
healthy_eating
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.03
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▇▃▁
</td>
</tr>
<tr>
<td style="text-align:left;">
daily_events
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.07
</td>
<td style="text-align:right;">
1.12
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▅▇▃▃
</td>
</tr>
<tr>
<td style="text-align:left;">
prioritising_workload
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.65
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▆▇▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
writing_notes
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.08
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
workaholism
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▆▇▅▅
</td>
</tr>
<tr>
<td style="text-align:left;">
thinking_ahead
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.41
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▅▇▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
final_judgement
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.65
</td>
<td style="text-align:right;">
1.38
</td>
<td style="text-align:right;">
1
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
5
</td>
<td style="text-align:left;">
▇▃▇▃▃
</td>
</tr>
<tr>
<td style="text-align:left;">
reliability
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.86
</td>
<td style="text-align:right;">
0.93
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▅▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
keeping_promises
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.99
</td>
<td style="text-align:right;">
0.90
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▃▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
loss_of_interest
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.71
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▇▅▅
</td>
</tr>
<tr>
<td style="text-align:left;">
friends_versus_money
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.78
</td>
<td style="text-align:right;">
1.12
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▆▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
funniness
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▇▆▃
</td>
</tr>
<tr>
<td style="text-align:left;">
fake
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.13
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▇▃▂▁
</td>
</tr>
<tr>
<td style="text-align:left;">
criminal_damage
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.60
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▅▃▃▅
</td>
</tr>
<tr>
<td style="text-align:left;">
decision_making
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
1.20
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▅▇▅▅
</td>
</tr>
<tr>
<td style="text-align:left;">
elections
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.42
</td>
<td style="text-align:right;">
1.57
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▂▃▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
self_criticism
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
1.19
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
judgment_calls
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.99
</td>
<td style="text-align:right;">
0.97
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▅▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
hypochondria
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.91
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▃▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
empathy
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.86
</td>
<td style="text-align:right;">
1.13
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▅▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
eating_to_survive
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.23
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▆▅▂▂
</td>
</tr>
<tr>
<td style="text-align:left;">
giving
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2.98
</td>
<td style="text-align:right;">
1.31
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▆▇▅▅
</td>
</tr>
<tr>
<td style="text-align:left;">
compassion_to_animals
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
1.19
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▃▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
borrowed_stuff
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.02
</td>
<td style="text-align:right;">
1.05
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▃▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
loneliness
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▆▇▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
cheating_in_school
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.74
</td>
<td style="text-align:right;">
1.25
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▃▅▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
health
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▇▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
changing_the_past
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.95
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▆▇▅▅
</td>
</tr>
<tr>
<td style="text-align:left;">
god
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
1.48
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▃▆▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
dreams
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▇▅▁
</td>
</tr>
<tr>
<td style="text-align:left;">
charity
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.10
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▇▇▆▂▁
</td>
</tr>
<tr>
<td style="text-align:left;">
number_of_friends
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▇▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
waiting
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.67
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▆▇▃▁
</td>
</tr>
<tr>
<td style="text-align:left;">
new_environment
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.48
</td>
<td style="text-align:right;">
1.15
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▇▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
mood_swings
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.26
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▅▇▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
appearence_and_gestures
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.60
</td>
<td style="text-align:right;">
0.94
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▇▇▃
</td>
</tr>
<tr>
<td style="text-align:left;">
socializing
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▇▅▂
</td>
</tr>
<tr>
<td style="text-align:left;">
achievements
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▃▇▃▁
</td>
</tr>
<tr>
<td style="text-align:left;">
responding_to_a\_serious_letter
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
3.07
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▅▇▆▃
</td>
</tr>
<tr>
<td style="text-align:left;">
children
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
1.12
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▃▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
assertiveness
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
1.10
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▃▇▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
getting_angry
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▅▇▆▃
</td>
</tr>
<tr>
<td style="text-align:left;">
knowing_the_right_people
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
1.09
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▂▇▇▅
</td>
</tr>
<tr>
<td style="text-align:left;">
public_speaking
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
1.27
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▇▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
unpopularity
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.46
</td>
<td style="text-align:right;">
1.12
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▂▇▅▅
</td>
</tr>
<tr>
<td style="text-align:left;">
life_struggles
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.03
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▆▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
happiness_in_life
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.71
</td>
<td style="text-align:right;">
0.82
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▅▇▂
</td>
</tr>
<tr>
<td style="text-align:left;">
energy_levels
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.63
</td>
<td style="text-align:right;">
1.00
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▆▇▅
</td>
</tr>
<tr>
<td style="text-align:left;">
small_big_dogs
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.97
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▃▇▃▃
</td>
</tr>
<tr>
<td style="text-align:left;">
personality
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▁▇▃▁
</td>
</tr>
<tr>
<td style="text-align:left;">
finding_lost_valuables
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.87
</td>
<td style="text-align:right;">
1.24
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▅▇▃▃
</td>
</tr>
<tr>
<td style="text-align:left;">
getting_up
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
1.31
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▅▅▇
</td>
</tr>
<tr>
<td style="text-align:left;">
interests_or_hobbies
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
1.17
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
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▃▇▆▇
</td>
</tr>
<tr>
<td style="text-align:left;">
parents_advice
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.27
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▂▇▆▁
</td>
</tr>
<tr>
<td style="text-align:left;">
questionnaires_or_polls
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.75
</td>
<td style="text-align:right;">
1.10
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▅▇▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
finances
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▃▇▆▂
</td>
</tr>
<tr>
<td style="text-align:left;">
shopping_centres
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.23
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▆▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
branded_clothing
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.05
</td>
<td style="text-align:right;">
1.31
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▅▇▇▅
</td>
</tr>
<tr>
<td style="text-align:left;">
entertainment_spending
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▂▅▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
spending_on_looks
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▃▆▇▆▃
</td>
</tr>
<tr>
<td style="text-align:left;">
spending_on_gadgets
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.87
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▅▇▇▆▅
</td>
</tr>
<tr>
<td style="text-align:left;">
spending_on_healthy_eating
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.56
</td>
<td style="text-align:right;">
1.09
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
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
▁▃▇▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
20.43
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
▃▇▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
height
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
173.51
</td>
<td style="text-align:right;">
10.02
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
167
</td>
<td style="text-align:right;">
173
</td>
<td style="text-align:right;">
180
</td>
<td style="text-align:right;">
203
</td>
<td style="text-align:left;">
▁▁▁▇▆
</td>
</tr>
<tr>
<td style="text-align:left;">
weight
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
66.41
</td>
<td style="text-align:right;">
13.84
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
165
</td>
<td style="text-align:left;">
▇▆▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
number_of_siblings
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>

``` r
head(movie)
```

    ## # A tibble: 6 × 150
    ##   music slow_son…¹ dance  folk country class…² musical   pop  rock metal…³  punk
    ##   <dbl>      <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl> <dbl>   <dbl> <dbl>
    ## 1     5         NA     5     5       5       5       5     5     1       1     1
    ## 2     4          3     3     2       2       2       3     3     2       1     1
    ## 3     2          3     2     2       2       2       2     2     4       4     4
    ## 4     5          3     1     1       1       1       1     2     4       5     4
    ## 5     5          3     4     3       3       5       5     5     4       2     2
    ## 6     3          3     3     3       2       5       4     3     3       1     1
    ## # … with 139 more variables: hiphop_rap <dbl>, reggae_ska <dbl>,
    ## #   swing_jazz <dbl>, rock_n_roll <dbl>, alternative <dbl>, latino <dbl>,
    ## #   techno_trance <dbl>, opera <dbl>, m_target <dbl>, horror <dbl>,
    ## #   thriller <dbl>, comedy <dbl>, romantic <dbl>, sci_fi <dbl>, war <dbl>,
    ## #   fantasy_fairy_tales <dbl>, animated <dbl>, documentary <dbl>,
    ## #   western <dbl>, action <dbl>, history <dbl>, psychology <dbl>,
    ## #   politics <dbl>, mathematics <dbl>, physics <dbl>, internet <dbl>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
movie$m_target <- as.factor(movie$m_target)

# create dummy variables for gender and promotional class

movie$male <- ifelse(movie$gender == 'male', 1, 0)
movie$female<- ifelse(movie$gender == 'female', 1, 0)
movie$nogender <- ifelse(is.na(movie$gender), 1, 0)

movie$male <- ifelse(is.na(movie$male), 0, movie$male)
movie$female <- ifelse(is.na(movie$female), 0, movie$female)
head(movie)
```

    ## # A tibble: 6 × 153
    ##   music slow_son…¹ dance  folk country class…² musical   pop  rock metal…³  punk
    ##   <dbl>      <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl> <dbl>   <dbl> <dbl>
    ## 1     5         NA     5     5       5       5       5     5     1       1     1
    ## 2     4          3     3     2       2       2       3     3     2       1     1
    ## 3     2          3     2     2       2       2       2     2     4       4     4
    ## 4     5          3     1     1       1       1       1     2     4       5     4
    ## 5     5          3     4     3       3       5       5     5     4       2     2
    ## 6     3          3     3     3       2       5       4     3     3       1     1
    ## # … with 142 more variables: hiphop_rap <dbl>, reggae_ska <dbl>,
    ## #   swing_jazz <dbl>, rock_n_roll <dbl>, alternative <dbl>, latino <dbl>,
    ## #   techno_trance <dbl>, opera <dbl>, m_target <fct>, horror <dbl>,
    ## #   thriller <dbl>, comedy <dbl>, romantic <dbl>, sci_fi <dbl>, war <dbl>,
    ## #   fantasy_fairy_tales <dbl>, animated <dbl>, documentary <dbl>,
    ## #   western <dbl>, action <dbl>, history <dbl>, psychology <dbl>,
    ## #   politics <dbl>, mathematics <dbl>, physics <dbl>, internet <dbl>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
movie<- impute(movie)

head(movie)
```

    ##   music slow_songs_or_fast_songs dance folk country classical_music musical pop
    ## 1     5                        3     5    5       5               5       5   5
    ## 2     4                        3     3    2       2               2       3   3
    ## 3     2                        3     2    2       2               2       2   2
    ## 4     5                        3     1    1       1               1       1   2
    ## 5     5                        3     4    3       3               5       5   5
    ## 6     3                        3     3    3       2               5       4   3
    ##   rock metal_or_hardrock punk hiphop_rap reggae_ska swing_jazz rock_n_roll
    ## 1    1                 1    1          5          1          1           5
    ## 2    2                 1    1          5          1          1           2
    ## 3    4                 4    4          3          3          3           3
    ## 4    4                 5    4          3          1          1           1
    ## 5    4                 2    2          4          3          5           4
    ## 6    3                 1    1          3          2          2           2
    ##   alternative latino techno_trance opera m_target horror thriller comedy
    ## 1           5      5             5     5        0      1        1      5
    ## 2           1      5             4     2        0      2        2      5
    ## 3           4      2             1     1        0      1        1      4
    ## 4           4      1             4     1        0      1        1      5
    ## 5           3      4             2     2        0      1        3      4
    ## 6           2      2             1     4        0      2        2      2
    ##   romantic sci_fi war fantasy_fairy_tales animated documentary western action
    ## 1        5      5   1                   5        1           5       1      1
    ## 2        5      1   1                   5        4           1       1      1
    ## 3        5      2   2                   4        4           4       2      2
    ## 4        1      5   5                   5        5           1       1      2
    ## 5        3      1   2                   4        5           3       1      3
    ## 6        2      2   5                   5        5           5       3      3
    ##   history psychology politics mathematics physics internet pc
    ## 1       5          5        1           1       1        5  5
    ## 2       1          5        1           1       1        5  1
    ## 3       1          2        2           3       3        4  5
    ## 4       1          1        1           1       1        2  1
    ## 5       4          3        3           3       3        5  4
    ## 6       4          1        3           1       5        5  5
    ##   economy_management biology chemistry reading geography foreign_languages
    ## 1                  5       5         5       5         5                 5
    ## 2                  5       1         1       1         1                 4
    ## 3                  4       2         2       2         1                 3
    ## 4                  1       3         4       1         1                 1
    ## 5                  2       4         3       4         5                 5
    ## 6                  1       2         5       5         4                 3
    ##   medicine law cars art_exhibitions religion countryside_outdoors dancing
    ## 1        5   5    5               5        5                    5       5
    ## 2        3   3    1               1        3                    2       4
    ## 3        2   2    2               2        1                    2       1
    ## 4        5   1    1               1        1                    1       1
    ## 5        3   2    5               4        3                    3       1
    ## 6        5   3    3               1        5                    5       2
    ##   musical_instruments writing passive_sport active_sport gardening celebrities
    ## 1                   5       1             1            1         1           1
    ## 2                   1       1             5            5         1           4
    ## 3                   1       1             4            4         1           1
    ## 4                   1       1             1            4         1           1
    ## 5                   1       4             3            1         1           2
    ## 6                   2       3             3            3         3           2
    ##   shopping science_and_technology theatre fun_with_friends adrenaline_sports
    ## 1        5                      1       5                5                 1
    ## 2        5                      1       1                4                 1
    ## 3        1                      4       3                5                 4
    ## 4        1                      1       1                5                 1
    ## 5        3                      2       2                3                 1
    ## 6        2                      3       3                3                 3
    ##   pets flying storm darkness heights spiders snakes rats ageing dangerous_dogs
    ## 1    1      5     5        1       1       1      1    1      1              1
    ## 2    1      2     1        1       2       5      5    5      5              5
    ## 3    2      1     1        1       2       1      1    1      1              1
    ## 4    1      1     1        1       2       4      1    1      1              5
    ## 5    1      5     2        2       2       3      4    1      2              4
    ## 6    4      3     3        4       1       2      5    5      2              5
    ##   fear_of_public_speaking       smoking        alcohol healthy_eating
    ## 1                       1 tried smoking social drinker              5
    ## 2                       4  never smoked          never              3
    ## 3                       2 tried smoking    drink a lot              5
    ## 4                       5 former smoker    drink a lot              1
    ## 5                       2  never smoked          never              1
    ## 6                       4  never smoked social drinker              3
    ##   daily_events prioritising_workload writing_notes workaholism thinking_ahead
    ## 1            1                     1             5           5              5
    ## 2            3                     2             1           2              5
    ## 3            3                     1             4           3              4
    ## 4            1                     1             1           2              4
    ## 5            4                     3             5           5              3
    ## 6            1                     1             5           5              5
    ##   final_judgement reliability keeping_promises loss_of_interest
    ## 1               5           5                5                1
    ## 2               3           5                5                4
    ## 3               3           5                5                4
    ## 4               5           3                5                1
    ## 5               2           3                3                4
    ## 6               1           2                4                1
    ##   friends_versus_money funniness fake criminal_damage decision_making elections
    ## 1                    5         5    1               5               5         5
    ## 2                    4         1    1               1               2         1
    ## 3                    4         4    2               3               3         5
    ## 4                    5         5    1               2               3         1
    ## 5                    3         3    2               4               3         1
    ## 6                    3         2    2               2               4         3
    ##   self_criticism judgment_calls hypochondria empathy eating_to_survive giving
    ## 1              5              1            5       5                 1      5
    ## 2              2              5            3       5                 1      1
    ## 3              5              4            3       3                 1      1
    ## 4              3              3            1       5                 1      1
    ## 5              3              2            1       3                 2      4
    ## 6              3              4            4       2                 1      4
    ##   compassion_to_animals borrowed_stuff loneliness cheating_in_school health
    ## 1                     1              5          1                  5      5
    ## 2                     3              5          3                  3      4
    ## 3                     2              4          3                  4      5
    ## 4                     3              3          2                  2      2
    ## 5                     3              4          5                  2      3
    ## 6                     3              4          2                  5      3
    ##   changing_the_past god dreams charity number_of_friends
    ## 1                 1   5      3       2                 4
    ## 2                 2   4      4       1                 3
    ## 3                 2   3      3       2                 3
    ## 4                 4   5      4       1                 3
    ## 5                 5   4      3       1                 1
    ## 6                 1   5      3       2                 2
    ##               punctuality                         lying waiting new_environment
    ## 1 i am often running late only to avoid hurting someone       1               4
    ## 2     i am always on time only to avoid hurting someone       3               5
    ## 3 i am often running late                     sometimes       2               2
    ## 4        i am often early                     sometimes       4               5
    ## 5        i am often early                     sometimes       4               2
    ## 6        i am often early         everytime it suits me       3               3
    ##   mood_swings appearence_and_gestures socializing achievements
    ## 1           3                       2           3            1
    ## 2           3                       3           3            1
    ## 3           3                       4           3            4
    ## 4           2                       2           2            2
    ## 5           5                       3           3            1
    ## 6           4                       3           1            1
    ##   responding_to_a_serious_letter children assertiveness getting_angry
    ## 1                              3        4             5             1
    ## 2                              2        3             5             1
    ## 3                              4        2             4             2
    ## 4                              5        1             1             2
    ## 5                              2        5             3             5
    ## 6                              3        5             5             5
    ##   knowing_the_right_people public_speaking unpopularity life_struggles
    ## 1                        3               5            5              5
    ## 2                        3               2            3              2
    ## 3                        4               4            5              3
    ## 4                        1               5            1              1
    ## 5                        3               2            4              3
    ## 6                        1               5            5              2
    ##   happiness_in_life energy_levels small_big_dogs personality
    ## 1                 3             3              1           4
    ## 2                 4             4              3           4
    ## 3                 3             3              5           3
    ## 4                 4             4              4           3
    ## 5                 3             3              3           2
    ## 6                 4             4              3           3
    ##   finding_lost_valuables getting_up interests_or_hobbies parents_advice
    ## 1                      4          5                    4              5
    ## 2                      2          4                    3              4
    ## 3                      3          3                    3              4
    ## 4                      4          5                    1              2
    ## 5                      4          3                    1              5
    ## 6                      2          4                    1              4
    ##   questionnaires_or_polls          internet_usage finances shopping_centres
    ## 1                       4         few hours a day        3                3
    ## 2                       4 less than an hour a day        3                4
    ## 3                       2         few hours a day        1                1
    ## 4                       3         few hours a day        1                1
    ## 5                       4         few hours a day        4                4
    ## 6                       1 less than an hour a day        5                5
    ##   branded_clothing entertainment_spending spending_on_looks spending_on_gadgets
    ## 1                3                      1                 2                   1
    ## 2                2                      3                 3                   3
    ## 3                5                      4                 5                   5
    ## 4                1                      5                 1                   4
    ## 5                1                      2                 1                   1
    ## 6                5                      4                 3                   1
    ##   spending_on_healthy_eating age height weight number_of_siblings gender
    ## 1                          3  18    168     52                  1 female
    ## 2                          3  16    168     50                  1 female
    ## 3                          5  22    186     65                  0   male
    ## 4                          1  19    182     81                  1   male
    ## 5                          4  20    173     64                  1 female
    ## 6                          3  19    170     55                  2 female
    ##   left_right_handed               education only_child village_town
    ## 1      right handed        secondary school         no      village
    ## 2      right handed          primary school         no         city
    ## 3      right handed college/bachelor degree        yes         city
    ## 4      right handed college/bachelor degree         no         city
    ## 5      right handed          primary school        yes      village
    ## 6       left handed college/bachelor degree         no      village
    ##   house_block_of_flats male female nogender
    ## 1       house/bungalow    0      1        0
    ## 2       block of flats    0      1        0
    ## 3       block of flats    1      0        0
    ## 4       block of flats    1      0        0
    ## 5       house/bungalow    0      0        1
    ## 6       house/bungalow    0      1        0

``` r
movie$music <- scale(movie$music)
movie$dance<- scale(movie$dance)
movie$pop<- scale(movie$pop)
movie$horror<-scale(movie$horror)
movie$comedy<-scale(movie$comedy)
movie$documentary<-scale(movie$documentary)
movie$theatre<-scale(movie$theatre)
movie$celebrities<-scale(movie$celebrities)
```

``` r
movie = subset(movie,select=c(male,female,nogender,music,dance,pop,horror,comedy,documentary,theatre,celebrities))
head(movie)
```

    ##   male female nogender     music       dance        pop    horror     comedy
    ## 1    0      1        0  0.403073  1.61532240  1.3161160 -1.272645  0.6468366
    ## 2    0      1        0 -1.104718 -0.09661425 -0.4079789 -0.563279  0.6468366
    ## 3    1      0        0 -4.120301 -0.95258257 -1.2700263 -1.272645 -0.6366702
    ## 4    1      0        0  0.403073 -1.80855089 -1.2700263 -1.272645  0.6468366
    ## 5    0      0        1  0.403073  0.75935407  1.3161160 -1.272645 -0.6366702
    ## 6    0      1        0 -2.612510 -0.09661425 -0.4079789 -0.563279 -3.2036837
    ##   documentary     theatre celebrities
    ## 1   1.1984917  1.49627216  -1.0727232
    ## 2  -2.3435028 -1.53377271   1.2911686
    ## 3   0.3129931 -0.01875028  -1.0727232
    ## 4  -2.3435028 -1.53377271  -1.0727232
    ## 5  -0.5725055 -0.77626150  -0.2847592
    ## 6   1.1984917 -0.01875028  -0.2847592

``` r
# how many clusters do we need?

fviz_nbclust(movie, kmeans, method="wss")
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
set.seed(904)

clusters1 <- kmeans(movie, 5, iter.max = 200, nstart = 10)
print(clusters1)
```

    ## K-means clustering with 5 clusters of sizes 270, 204, 237, 56, 243
    ## 
    ## Cluster means:
    ##        male    female    nogender      music      dance        pop      horror
    ## 1 0.1444444 0.8481481 0.007407407  0.2690471  0.2045598  0.4412975  0.04625058
    ## 2 0.2450980 0.7450980 0.009803922  0.1887301  0.2390596  0.2892654 -0.85537107
    ## 3 0.7130802 0.2827004 0.004219409  0.1104215  0.5751584  0.3849593  0.71477767
    ## 4 0.5357143 0.4642857 0.000000000 -3.3933305 -0.3564618 -0.2694355  0.03208202
    ## 5 0.5061728 0.4897119 0.004115226  0.2169259 -0.9067900 -1.0465325 -0.03782247
    ##       comedy documentary     theatre celebrities
    ## 1  0.2285084 -0.85455324  0.10750159  1.01392204
    ## 2  0.1875425  0.51266433  0.67934830 -0.36201060
    ## 3  0.4302110  0.28683911 -0.61325275 -0.21826439
    ## 4 -0.3157935  0.07580595 -0.11343918 -0.07369747
    ## 5 -0.7581544  0.22189239 -0.06551023 -0.59281098
    ## 
    ## Clustering vector:
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
    ##    2    1    4    5    2    4    5    1    1    2    1    5    5    5    3    5 
    ##   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32 
    ##    4    2    5    1    2    2    4    4    4    4    4    2    5    5    5    1 
    ##   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48 
    ##    2    3    4    3    1    4    4    1    5    5    3    3    2    2    1    1 
    ##   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64 
    ##    4    2    1    2    2    2    3    2    3    1    5    5    5    4    5    5 
    ##   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79   80 
    ##    2    3    4    5    4    4    3    1    2    3    5    5    5    5    1    1 
    ##   81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96 
    ##    4    3    5    5    2    5    1    5    2    1    2    2    5    3    3    1 
    ##   97   98   99  100  101  102  103  104  105  106  107  108  109  110  111  112 
    ##    5    3    2    5    5    5    5    3    2    4    5    1    1    5    5    2 
    ##  113  114  115  116  117  118  119  120  121  122  123  124  125  126  127  128 
    ##    1    4    3    5    5    3    3    5    1    5    5    2    2    5    5    2 
    ##  129  130  131  132  133  134  135  136  137  138  139  140  141  142  143  144 
    ##    1    3    4    5    2    3    5    2    2    2    3    2    2    3    1    5 
    ##  145  146  147  148  149  150  151  152  153  154  155  156  157  158  159  160 
    ##    3    5    1    1    5    2    1    5    2    1    1    3    5    3    5    3 
    ##  161  162  163  164  165  166  167  168  169  170  171  172  173  174  175  176 
    ##    2    1    5    4    2    2    1    5    5    5    3    5    4    2    2    2 
    ##  177  178  179  180  181  182  183  184  185  186  187  188  189  190  191  192 
    ##    1    1    4    3    5    3    2    1    1    2    5    4    5    5    2    5 
    ##  193  194  195  196  197  198  199  200  201  202  203  204  205  206  207  208 
    ##    3    4    5    1    3    3    4    2    2    1    2    1    5    1    5    5 
    ##  209  210  211  212  213  214  215  216  217  218  219  220  221  222  223  224 
    ##    4    3    2    3    3    2    2    3    2    2    3    1    3    5    5    3 
    ##  225  226  227  228  229  230  231  232  233  234  235  236  237  238  239  240 
    ##    1    4    1    2    2    1    2    2    3    5    2    5    2    2    2    1 
    ##  241  242  243  244  245  246  247  248  249  250  251  252  253  254  255  256 
    ##    3    4    3    4    4    2    2    1    1    3    5    2    3    1    2    1 
    ##  257  258  259  260  261  262  263  264  265  266  267  268  269  270  271  272 
    ##    3    1    4    3    5    1    1    2    2    1    1    1    3    1    5    5 
    ##  273  274  275  276  277  278  279  280  281  282  283  284  285  286  287  288 
    ##    1    3    5    5    5    5    5    5    3    3    3    5    3    5    5    5 
    ##  289  290  291  292  293  294  295  296  297  298  299  300  301  302  303  304 
    ##    1    5    3    2    2    3    5    5    1    4    3    3    3    3    5    2 
    ##  305  306  307  308  309  310  311  312  313  314  315  316  317  318  319  320 
    ##    1    1    1    3    5    3    1    3    1    2    1    1    2    1    3    1 
    ##  321  322  323  324  325  326  327  328  329  330  331  332  333  334  335  336 
    ##    1    3    1    1    1    1    2    3    2    3    1    1    1    3    5    2 
    ##  337  338  339  340  341  342  343  344  345  346  347  348  349  350  351  352 
    ##    3    3    5    1    3    4    2    1    3    3    3    5    2    2    3    3 
    ##  353  354  355  356  357  358  359  360  361  362  363  364  365  366  367  368 
    ##    3    5    2    5    2    2    2    1    2    1    5    3    1    5    1    5 
    ##  369  370  371  372  373  374  375  376  377  378  379  380  381  382  383  384 
    ##    2    3    5    5    2    2    3    5    2    2    2    2    5    5    3    5 
    ##  385  386  387  388  389  390  391  392  393  394  395  396  397  398  399  400 
    ##    3    3    5    1    2    3    3    5    5    1    3    5    3    1    2    5 
    ##  401  402  403  404  405  406  407  408  409  410  411  412  413  414  415  416 
    ##    1    1    2    3    5    1    5    5    5    5    2    2    5    4    5    1 
    ##  417  418  419  420  421  422  423  424  425  426  427  428  429  430  431  432 
    ##    5    5    2    1    3    2    3    3    5    2    3    3    2    3    1    5 
    ##  433  434  435  436  437  438  439  440  441  442  443  444  445  446  447  448 
    ##    2    2    2    3    3    5    3    1    2    5    5    1    5    5    1    5 
    ##  449  450  451  452  453  454  455  456  457  458  459  460  461  462  463  464 
    ##    3    5    4    1    2    1    4    5    1    1    3    2    3    3    1    3 
    ##  465  466  467  468  469  470  471  472  473  474  475  476  477  478  479  480 
    ##    2    1    2    3    5    1    1    4    1    1    5    1    3    5    2    1 
    ##  481  482  483  484  485  486  487  488  489  490  491  492  493  494  495  496 
    ##    3    3    3    2    4    1    1    1    2    1    5    5    3    5    3    3 
    ##  497  498  499  500  501  502  503  504  505  506  507  508  509  510  511  512 
    ##    5    1    1    1    2    1    5    2    4    1    3    3    1    1    1    1 
    ##  513  514  515  516  517  518  519  520  521  522  523  524  525  526  527  528 
    ##    1    3    1    5    1    2    1    2    5    3    3    3    1    5    5    5 
    ##  529  530  531  532  533  534  535  536  537  538  539  540  541  542  543  544 
    ##    1    2    3    4    1    5    1    1    1    3    2    1    5    5    5    4 
    ##  545  546  547  548  549  550  551  552  553  554  555  556  557  558  559  560 
    ##    5    3    2    3    2    2    1    1    2    2    5    3    3    1    5    1 
    ##  561  562  563  564  565  566  567  568  569  570  571  572  573  574  575  576 
    ##    3    3    1    2    5    5    1    5    5    1    2    1    3    3    2    2 
    ##  577  578  579  580  581  582  583  584  585  586  587  588  589  590  591  592 
    ##    2    1    3    1    3    3    1    5    2    5    2    1    3    1    5    1 
    ##  593  594  595  596  597  598  599  600  601  602  603  604  605  606  607  608 
    ##    5    2    3    3    2    5    1    3    1    2    1    5    1    3    1    3 
    ##  609  610  611  612  613  614  615  616  617  618  619  620  621  622  623  624 
    ##    3    1    5    1    1    3    3    2    3    2    2    1    1    3    3    5 
    ##  625  626  627  628  629  630  631  632  633  634  635  636  637  638  639  640 
    ##    3    2    5    1    3    2    5    4    1    1    1    4    1    5    5    3 
    ##  641  642  643  644  645  646  647  648  649  650  651  652  653  654  655  656 
    ##    5    3    5    1    5    3    5    5    2    1    3    5    5    2    5    3 
    ##  657  658  659  660  661  662  663  664  665  666  667  668  669  670  671  672 
    ##    3    1    2    1    2    3    5    3    3    5    2    5    2    3    1    5 
    ##  673  674  675  676  677  678  679  680  681  682  683  684  685  686  687  688 
    ##    1    5    2    1    1    2    5    3    3    5    5    1    1    2    1    3 
    ##  689  690  691  692  693  694  695  696  697  698  699  700  701  702  703  704 
    ##    3    3    1    2    1    2    3    5    1    2    1    1    3    5    4    3 
    ##  705  706  707  708  709  710  711  712  713  714  715  716  717  718  719  720 
    ##    5    1    5    5    5    3    1    3    1    5    3    3    2    3    4    1 
    ##  721  722  723  724  725  726  727  728  729  730  731  732  733  734  735  736 
    ##    1    2    5    4    3    5    5    3    2    5    1    1    1    1    3    1 
    ##  737  738  739  740  741  742  743  744  745  746  747  748  749  750  751  752 
    ##    2    2    5    5    1    3    2    2    3    1    2    1    3    3    3    3 
    ##  753  754  755  756  757  758  759  760  761  762  763  764  765  766  767  768 
    ##    5    5    2    3    1    5    5    1    5    3    5    3    2    2    1    1 
    ##  769  770  771  772  773  774  775  776  777  778  779  780  781  782  783  784 
    ##    2    1    3    5    1    2    3    2    1    1    1    5    2    3    1    2 
    ##  785  786  787  788  789  790  791  792  793  794  795  796  797  798  799  800 
    ##    3    1    3    1    2    5    5    1    3    2    2    2    1    2    3    1 
    ##  801  802  803  804  805  806  807  808  809  810  811  812  813  814  815  816 
    ##    5    1    2    2    1    3    3    3    2    5    5    1    2    3    5    1 
    ##  817  818  819  820  821  822  823  824  825  826  827  828  829  830  831  832 
    ##    2    1    3    2    3    2    1    3    3    2    2    3    1    4    5    1 
    ##  833  834  835  836  837  838  839  840  841  842  843  844  845  846  847  848 
    ##    3    2    2    1    1    2    3    3    3    2    1    1    1    5    5    4 
    ##  849  850  851  852  853  854  855  856  857  858  859  860  861  862  863  864 
    ##    3    1    1    5    1    3    3    1    1    4    4    3    5    5    5    1 
    ##  865  866  867  868  869  870  871  872  873  874  875  876  877  878  879  880 
    ##    5    1    1    1    5    5    1    4    1    2    1    4    3    2    1    1 
    ##  881  882  883  884  885  886  887  888  889  890  891  892  893  894  895  896 
    ##    1    2    2    5    1    3    2    4    2    3    1    2    3    1    1    4 
    ##  897  898  899  900  901  902  903  904  905  906  907  908  909  910  911  912 
    ##    3    3    1    3    1    3    1    2    2    1    2    5    3    1    5    3 
    ##  913  914  915  916  917  918  919  920  921  922  923  924  925  926  927  928 
    ##    5    1    3    1    2    1    1    1    5    1    1    1    3    2    2    5 
    ##  929  930  931  932  933  934  935  936  937  938  939  940  941  942  943  944 
    ##    5    2    5    4    3    5    5    2    5    5    1    5    5    5    5    3 
    ##  945  946  947  948  949  950  951  952  953  954  955  956  957  958  959  960 
    ##    1    1    1    5    1    3    1    3    1    5    5    1    2    2    3    1 
    ##  961  962  963  964  965  966  967  968  969  970  971  972  973  974  975  976 
    ##    1    3    1    5    1    3    3    3    1    5    2    3    3    3    1    5 
    ##  977  978  979  980  981  982  983  984  985  986  987  988  989  990  991  992 
    ##    3    3    3    2    5    2    2    3    1    3    5    5    1    1    2    5 
    ##  993  994  995  996  997  998  999 1000 1001 1002 1003 1004 1005 1006 1007 1008 
    ##    5    1    5    5    3    1    3    3    1    3    1    3    3    1    1    2 
    ## 1009 1010 
    ##    3    3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 1513.5950  930.7707 1178.0785  497.0202 1626.0171
    ##  (between_SS / total_SS =  32.9 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
# visualize clusters

fviz_cluster(clusters1,movie,ellipse.type="norm",geom="point")
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# review cluster sizes

ggplot(movie,aes(clusters1$cluster))+geom_bar()
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(movie,aes(x=music))+geom_histogram(binwidth=1)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
ggplot(movie,aes(x=music))+geom_histogram(binwidth=1) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ggplot(movie,aes(x=dance))+geom_histogram(binwidth=5)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
ggplot(movie,aes(x=dance))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
ggplot(movie,aes(x=pop))+geom_histogram(binwidth=5)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

``` r
ggplot(movie,aes(x=pop))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

``` r
ggplot(movie,aes(x=horror))+geom_histogram(binwidth=5)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->

``` r
ggplot(movie,aes(x=horror))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->

``` r
ggplot(movie,aes(x=comedy))+geom_histogram(binwidth=5)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->

``` r
ggplot(movie,aes(x=comedy))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->

``` r
ggplot(movie,aes(x=documentary))+geom_histogram(binwidth=5)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->

``` r
ggplot(movie,aes(x=documentary))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->

``` r
ggplot(movie,aes(x=theatre))+geom_histogram(binwidth=5)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->

``` r
ggplot(movie,aes(x=theatre))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->

``` r
ggplot(movie,aes(x=celebrities))+geom_histogram(binwidth=5)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->

``` r
ggplot(movie,aes(x=celebrities))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->

``` r
ggplot(movie,aes(male))+geom_bar()
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->

``` r
ggplot(movie,aes(male))+geom_bar()+facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-19.png)<!-- -->

``` r
ggplot(movie,aes(female))+geom_bar()
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-20.png)<!-- -->

``` r
ggplot(movie,aes(female))+geom_bar()+facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-21.png)<!-- -->

``` r
ggplot(movie,aes(nogender))+geom_bar()
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-22.png)<!-- -->

``` r
ggplot(movie,aes(nogender))+geom_bar()+facet_wrap(~clusters1$cluster)
```

![](Cluster_lab_assignment_files/figure-gfm/unnamed-chunk-8-23.png)<!-- -->

``` r
movie %>% 
  group_by(clusters1$cluster) %>% 
  summarize(mean_music = mean(music),
            min_music = min(music),
            max_music = max(music),
            mean_dance = mean(dance),
            min_dance = min(dance),
            max_dance = max(dance),
            mean_pop = mean(pop),
            min_pop = min(pop),
            max_pop = max(pop),
            mean_horror = mean(horror),
            min_horror = min(horror),
            max_horror = max(horror),
             mean_comedy = mean(comedy),
            min_comedy = min(comedy),
            max_comedy = max(comedy),
            mean_documentary = mean(documentary),
            min_documentary = min(documentary),
            max_documentary = max(documentary),
            mean_theatre = mean(theatre),
            min_theatre = min(theatre),
            max_theatre = max(theatre),
            mean_celebrities = mean(celebrities),
            min_celebrities = min(celebrities),
            max_celebrities = max(celebrities),
            mean_male = mean(male),
            min_male = min(male),
            max_male = max(male),
            mean_female = mean(female),
            min_female = min(female),
            max_female = max(female),
            mean_nogender= mean(nogender),
            min_nogender = min(nogender),
            max_nogender = max(nogender),
            )
```

    ## # A tibble: 5 × 34
    ##   clusters1$cl…¹ mean_…² min_m…³ max_m…⁴ mean_…⁵ min_d…⁶ max_d…⁷ mean_…⁸ min_pop
    ##            <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1              1   0.269   -1.10   0.403   0.205  -1.81     1.62   0.441   -2.13
    ## 2              2   0.189   -1.10   0.403   0.239  -1.81     1.62   0.289   -1.27
    ## 3              3   0.110   -1.10   0.403   0.575  -0.953    1.62   0.385   -2.13
    ## 4              4  -3.39    -5.63  -2.61   -0.356  -1.81     1.62  -0.269   -2.13
    ## 5              5   0.217   -1.10   0.403  -0.907  -1.81     1.62  -1.05    -2.13
    ## # … with 25 more variables: max_pop <dbl>, mean_horror <dbl>, min_horror <dbl>,
    ## #   max_horror <dbl>, mean_comedy <dbl>, min_comedy <dbl>, max_comedy <dbl>,
    ## #   mean_documentary <dbl>, min_documentary <dbl>, max_documentary <dbl>,
    ## #   mean_theatre <dbl>, min_theatre <dbl>, max_theatre <dbl>,
    ## #   mean_celebrities <dbl>, min_celebrities <dbl>, max_celebrities <dbl>,
    ## #   mean_male <dbl>, min_male <dbl>, max_male <dbl>, mean_female <dbl>,
    ## #   min_female <dbl>, max_female <dbl>, mean_nogender <dbl>, …
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
