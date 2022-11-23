cluster_linear_regression
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
options(scipen=999)
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
```

``` r
mktcamp <- read_csv("marketing_campaign-1.csv") %>%
  clean_names()
```

    ## Rows: 2240 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): Education, MarStat, Dt_Customer
    ## dbl (26): ID, Birth, Income, Kids, Teens, Recency, Wines, Fruits, Meat, Fish...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# remove "rejected" variables from the data frame

mktcamp = subset(mktcamp, select= -c(id, z_cost, z_rev))
head(mktcamp)
```

    ## # A tibble: 6 × 26
    ##   birth education  mar_s…¹ income  kids teens dt_cu…² recency wines fruits  meat
    ##   <dbl> <chr>      <chr>    <dbl> <dbl> <dbl> <chr>     <dbl> <dbl>  <dbl> <dbl>
    ## 1  1957 Graduation Single   58138     0     0 4/9/20…      58   635     88   546
    ## 2  1954 Graduation Single   46344     1     1 8/3/20…      38    11      1     6
    ## 3  1965 Graduation Partner  71613     0     0 21-08-…      26   426     49   127
    ## 4  1984 Graduation Partner  26646     1     0 10/2/2…      26    11      4    20
    ## 5  1981 PhD        Married  58293     1     0 19-01-…      94   173     43   118
    ## 6  1967 Master     Partner  62513     0     1 9/9/20…      16   520     42    98
    ## # … with 15 more variables: fish <dbl>, sweets <dbl>, gold <dbl>, deals <dbl>,
    ## #   web <dbl>, catalog <dbl>, store <dbl>, visits <dbl>, cmp3 <dbl>,
    ## #   cmp4 <dbl>, cmp5 <dbl>, cmp1 <dbl>, cmp2 <dbl>, cmplain <dbl>,
    ## #   response <dbl>, and abbreviated variable names ¹​mar_stat, ²​dt_customer
    ## # ℹ Use `colnames()` to see all variable names

``` r
skim(mktcamp)
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
mktcamp
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
2240
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
26
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
23
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
education
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
10
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
mar_stat
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
8
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
dt_customer
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
663
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
birth
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1968.81
</td>
<td style="text-align:right;">
11.98
</td>
<td style="text-align:right;">
1893
</td>
<td style="text-align:right;">
1959.00
</td>
<td style="text-align:right;">
1970.0
</td>
<td style="text-align:right;">
1977.00
</td>
<td style="text-align:right;">
1996
</td>
<td style="text-align:left;">
▁▁▂▇▅
</td>
</tr>
<tr>
<td style="text-align:left;">
income
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
52247.25
</td>
<td style="text-align:right;">
25173.08
</td>
<td style="text-align:right;">
1730
</td>
<td style="text-align:right;">
35303.00
</td>
<td style="text-align:right;">
51381.5
</td>
<td style="text-align:right;">
68522.00
</td>
<td style="text-align:right;">
666666
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
kids
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
▇▁▆▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teens
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
▇▁▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
recency
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
49.11
</td>
<td style="text-align:right;">
28.96
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
24.00
</td>
<td style="text-align:right;">
49.0
</td>
<td style="text-align:right;">
74.00
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
wines
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
303.94
</td>
<td style="text-align:right;">
336.60
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
23.75
</td>
<td style="text-align:right;">
173.5
</td>
<td style="text-align:right;">
504.25
</td>
<td style="text-align:right;">
1493
</td>
<td style="text-align:left;">
▇▂▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fruits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
26.30
</td>
<td style="text-align:right;">
39.77
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
33.00
</td>
<td style="text-align:right;">
199
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
meat
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
166.95
</td>
<td style="text-align:right;">
225.72
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
16.00
</td>
<td style="text-align:right;">
67.0
</td>
<td style="text-align:right;">
232.00
</td>
<td style="text-align:right;">
1725
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fish
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
37.53
</td>
<td style="text-align:right;">
54.63
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
12.0
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
259
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
sweets
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
27.06
</td>
<td style="text-align:right;">
41.28
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
33.00
</td>
<td style="text-align:right;">
263
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
gold
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
44.02
</td>
<td style="text-align:right;">
52.17
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
24.0
</td>
<td style="text-align:right;">
56.00
</td>
<td style="text-align:right;">
362
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
deals
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.32
</td>
<td style="text-align:right;">
1.93
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
web
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.08
</td>
<td style="text-align:right;">
2.78
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.0
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
▇▃▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
catalog
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
store
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5.79
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
▂▇▂▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
visits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5.32
</td>
<td style="text-align:right;">
2.43
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
▅▇▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp1
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
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp2
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
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmplain
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
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
response
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
</tbody>
</table>

``` r
mktcamp2 <- impute(mktcamp)
head(mktcamp2)
```

    ##   birth  education mar_stat income kids teens dt_customer recency wines fruits
    ## 1  1957 Graduation   Single  58138    0     0    4/9/2012      58   635     88
    ## 2  1954 Graduation   Single  46344    1     1    8/3/2014      38    11      1
    ## 3  1965 Graduation  Partner  71613    0     0  21-08-2013      26   426     49
    ## 4  1984 Graduation  Partner  26646    1     0   10/2/2014      26    11      4
    ## 5  1981        PhD  Married  58293    1     0  19-01-2014      94   173     43
    ## 6  1967     Master  Partner  62513    0     1    9/9/2013      16   520     42
    ##   meat fish sweets gold deals web catalog store visits cmp3 cmp4 cmp5 cmp1 cmp2
    ## 1  546  172     88   88     3   8      10     4      7    0    0    0    0    0
    ## 2    6    2      1    6     2   1       1     2      5    0    0    0    0    0
    ## 3  127  111     21   42     1   8       2    10      4    0    0    0    0    0
    ## 4   20   10      3    5     2   2       0     4      6    0    0    0    0    0
    ## 5  118   46     27   15     5   5       3     6      5    0    0    0    0    0
    ## 6   98    0     42   14     2   6       4    10      6    0    0    0    0    0
    ##   cmplain response
    ## 1       0        1
    ## 2       0        0
    ## 3       0        0
    ## 4       0        0
    ## 5       0        0
    ## 6       0        0

``` r
mktcamp2$response <- as.factor(mktcamp2$response)
```

\#explore response

``` r
mktcamp2 %>%
  count(response)%>%
  mutate(pct=n/sum(n))
```

    ##   response    n  pct
    ## 1        0 1904 0.85
    ## 2        1  336 0.15

``` r
mktcamp2 %>%
  count(response)%>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(x=response,y=pct))+
  geom_col()+
  labs(title = "customer accepted the offer in the last campaign or not",y="percentage")+
  geom_text(aes(label=paste(pct*100,"%")),vjust=1.5,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# explore categorical variables

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(education), fill=factor(response)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Different education level",  x="education")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(mar_stat), fill=factor(response)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Different marital status",  x="marital status")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(cmp1), fill=factor(response)))+
  geom_bar(position = "fill")+
  labs(title="Different cmp1",  x="cmp1")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(cmp2), fill=factor(response)))+
  geom_bar(position = "fill")+
  labs(title="Different cmp2",  x="cmp2")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(cmp3), fill=factor(response)))+
  geom_bar(position = "fill")+
  labs(title="Different cmp3",  x="cmp3")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(cmp4), fill=factor(response)))+
  geom_bar(position = "fill")+
  labs(title="Different cmp4",  x="cmp4")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(cmp5), fill=factor(response)))+
  geom_bar(position = "fill")+
  labs(title="Different cmp5",  x="cmp5")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->

``` r
mktcamp2 %>%
  ggplot(aes(x=factor(cmplain), fill=factor(response)))+
  geom_bar(position = "fill")+
  labs(title="Different cmplain",  x="cmplain")+
  scale_fill_manual(values = c("steelblue3","khaki1"))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->
\# box-plot

``` r
numeric<- subset(mktcamp2,select=-c(education,mar_stat,response,dt_customer,cmp1,cmp2,cmp3,cmp4,cmp5,cmplain))

for(col in colnames(numeric)){
  print(
  mktcamp2 %>% 
  ggplot(aes(x=response,y=!!as.name(col)))+
  geom_boxplot()+
  labs(title=paste(col,"box plot"),  y=col, x="response"))
}
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-13.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-14.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-15.png)<!-- -->![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-7-16.png)<!-- -->
\# What percentage of customers shop via the different channels? \####

``` r
mktcamp2 %>%
  summarize(n=sum(web,catalog,store),
            n_web=sum(web),
            n_catalog=sum(catalog),
            n_store=sum(store),
            n_visits=sum(visits))%>%
  mutate(pct_web=n_web/n,
         pct_catalog=n_catalog/n,
         pct_store=n_store/n,
         )%>%
  dplyr::select(pct_web,pct_catalog,pct_store)%>%
  pivot_longer(cols=c('pct_web','pct_catalog','pct_store'),names_to = 'channel',values_to = "pct")%>%
  ggplot(aes(x=channel,y=pct))+
  geom_col()+
  labs(title = "Percentage of customers shop via the different channels",y="percentage")+
  geom_text(aes(label=paste(round(pct*100,2),"%")),vjust=1.5,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
mktcamp2 %>%
  summarize(
            website_purchase=sum(web),
            website_visits=sum(visits))%>%
  mutate(pct=website_purchase/website_visits
         )
```

    ##   website_purchase website_visits       pct
    ## 1             9150          11909 0.7683265

``` r
mktcamp2 %>%
  summarize(
            website_purchase=sum(web),
            website_visits=sum(visits))%>%
  pivot_longer(cols=c('website_visits','website_purchase'),names_to = 'channel',values_to = "count")%>%
  ggplot(aes(x=channel,y=count))+
  geom_col()+
  labs(title = "Conversion rate",y="count")+
  geom_text(aes(label=round(count)),vjust=1.5,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

# What are the most popular items (in terms of amount spent) that are purchased from the retailer?

``` r
mktcamp2 %>%
  summarize(wine_spent=sum(wines),
            fruits_spent=sum(fruits),
            meat_spent=sum(meat),
            fish_spent=sum(fish),
            sweets_spent=sum(sweets),
            gold_spent=sum(gold))%>%
  pivot_longer(cols=c('wine_spent','fruits_spent','meat_spent','fish_spent','sweets_spent','gold_spent'),names_to = 'product',values_to = "amount_spent")%>%
  ggplot(aes(x=product,y=amount_spent))+
  geom_col()+
  labs(title = "Most popular items that are purchased from the retailer",y="amount_spent")+
  geom_text(aes(label=amount_spent),vjust=1.5,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#mktcamp2%>%
#  select_if(is.character)%>%
#  group_by(mar_stat)%>%
#  summarise(n=n())

# create dummy variables for gender and promotional class


mktcamp2$Graduation <-ifelse(mktcamp2$education=='Graduation',1,0)
mktcamp2$Basic <-ifelse(mktcamp2$education=='Basic',1,0)
mktcamp2$Master <-ifelse(mktcamp2$education=='Master',1,0)
mktcamp2$PhD <-ifelse(mktcamp2$education=='PhD',1,0)
mktcamp2$Cycle <-ifelse(mktcamp2$education=='2nCycle',1,0)

mktcamp2$Absurd <-ifelse(mktcamp2$mar_stat=='Absurd',1,0)
mktcamp2$Alone <-ifelse(mktcamp2$mar_stat=='Alone',1,0)
mktcamp2$Divorce <-ifelse(mktcamp2$mar_stat=='Divorce',1,0)
mktcamp2$Married <-ifelse(mktcamp2$mar_stat=='Married',1,0)
mktcamp2$Partner <-ifelse(mktcamp2$mar_stat=='Partner',1,0)
mktcamp2$Single <-ifelse(mktcamp2$mar_stat=='Single',1,0)
mktcamp2$Widow <-ifelse(mktcamp2$mar_stat=='Widow',1,0)
mktcamp2$YOLO <-ifelse(mktcamp2$mar_stat=='YOLO',1,0)

format_dmy <- as.Date(mktcamp2$dt_customer,format="%d-%m-%Y")
format_dmy2<- as.Date(mktcamp2$dt_customer,format = "%d/%m/%Y")
mktcamp2$dt_customer<-as.Date(ifelse(is.na(format_dmy),format_dmy2,format_dmy),origin ="1970-01-01")


#mktcamp3 = subset(mktcamp2, select= -c(education, mar_stat,response))
mktcamp2<-mktcamp2%>%
 mutate(dt_customer_days = Sys.Date()-dt_customer)%>%
  mutate(dt_customer_days =as.numeric(dt_customer_days))%>%
  mutate(age=2022-birth)
mktcamp3<-subset(mktcamp2,select=-c(education, mar_stat,response,dt_customer,birth))

skim(mktcamp3)
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
mktcamp3
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
2240
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
36
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
36
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
income
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
52237.98
</td>
<td style="text-align:right;">
25037.96
</td>
<td style="text-align:right;">
1730
</td>
<td style="text-align:right;">
35538.75
</td>
<td style="text-align:right;">
51381.5
</td>
<td style="text-align:right;">
68289.75
</td>
<td style="text-align:right;">
666666
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
kids
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
▇▁▆▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teens
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
▇▁▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
recency
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
49.11
</td>
<td style="text-align:right;">
28.96
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
24.00
</td>
<td style="text-align:right;">
49.0
</td>
<td style="text-align:right;">
74.00
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
wines
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
303.94
</td>
<td style="text-align:right;">
336.60
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
23.75
</td>
<td style="text-align:right;">
173.5
</td>
<td style="text-align:right;">
504.25
</td>
<td style="text-align:right;">
1493
</td>
<td style="text-align:left;">
▇▂▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fruits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
26.30
</td>
<td style="text-align:right;">
39.77
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
33.00
</td>
<td style="text-align:right;">
199
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
meat
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
166.95
</td>
<td style="text-align:right;">
225.72
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
16.00
</td>
<td style="text-align:right;">
67.0
</td>
<td style="text-align:right;">
232.00
</td>
<td style="text-align:right;">
1725
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fish
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
37.53
</td>
<td style="text-align:right;">
54.63
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
12.0
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
259
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
sweets
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
27.06
</td>
<td style="text-align:right;">
41.28
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
33.00
</td>
<td style="text-align:right;">
263
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
gold
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
44.02
</td>
<td style="text-align:right;">
52.17
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
24.0
</td>
<td style="text-align:right;">
56.00
</td>
<td style="text-align:right;">
362
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
deals
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2.32
</td>
<td style="text-align:right;">
1.93
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
web
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4.08
</td>
<td style="text-align:right;">
2.78
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.0
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
▇▃▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
catalog
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
store
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5.79
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
▂▇▂▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
visits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5.32
</td>
<td style="text-align:right;">
2.43
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
▅▇▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp1
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
0.25
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp2
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
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmplain
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
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Graduation
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
Basic
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
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Master
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
0.37
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
PhD
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
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
Cycle
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
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Absurd
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
0.03
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Alone
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
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Divorce
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Married
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▅
</td>
</tr>
<tr>
<td style="text-align:left;">
Partner
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
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
Single
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
Widow
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
0.18
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
YOLO
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
0.03
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
dt_customer_days
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3422.58
</td>
<td style="text-align:right;">
202.12
</td>
<td style="text-align:right;">
3069
</td>
<td style="text-align:right;">
3249.75
</td>
<td style="text-align:right;">
3424.5
</td>
<td style="text-align:right;">
3598.00
</td>
<td style="text-align:right;">
3768
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
53.19
</td>
<td style="text-align:right;">
11.98
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
45.00
</td>
<td style="text-align:right;">
52.0
</td>
<td style="text-align:right;">
63.00
</td>
<td style="text-align:right;">
129
</td>
<td style="text-align:left;">
▅▇▂▁▁
</td>
</tr>
</tbody>
</table>

# What is the overall demographic makeup of our customer base (age? married? Single? Kids at home? Educated?)?

``` r
mktcamp2 %>%
ggplot(aes(x=age))+
  geom_histogram()+
  labs(title = "age make up of our customer base",y="percentage")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#married

``` r
mktcamp2 %>%
  count(mar_stat)%>%
  mutate(pct=n/sum(n))%>%
ggplot(aes(x=mar_stat,y=pct))+
  geom_col()+
  labs(title = "mar_stat make up of our customer base",y="percentage")+
  geom_text(aes(label=paste(round(pct*100,2),"%")),vjust=1.5,color="red")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
\# Education

``` r
mktcamp2 %>%
  count(education)%>%
  mutate(pct=n/sum(n))%>%
ggplot(aes(x=education,y=pct))+
  geom_col()+
  labs(title = "education make up of our customer base",y="percentage")+
  geom_text(aes(label=paste(round(pct*100,2),"%")),vjust=1.5,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# income

``` r
mktcamp2%>%
  ggplot(aes(x=income))+
  geom_histogram(binwidth = 10000)+
  labs(title = "Income make up ")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
\#kids

``` r
mktcamp2%>%
   count(kids)%>%
  mutate(pct=n/sum(n))%>%
ggplot(aes(x=kids,y=pct))+
  geom_col()+
  labs(title = "kids make up of our customer base",y="percentage")+
  geom_text(aes(label=paste(round(pct*100,2),"%")),vjust=1,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
\#teens

``` r
mktcamp2%>%
 count(teens)%>%
  mutate(pct=n/sum(n))%>%
ggplot(aes(x=teens,y=pct))+
  geom_col()+
  labs(title = "teens make up of our customer base",y="percentage")+
  geom_text(aes(label=paste(round(pct*100,2),"%")),vjust=1.3,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

# explore dt_customer

``` r
  mktcamp2 %>% 
  ggplot(aes(x=response,y=dt_customer_days))+
  geom_boxplot()+
  labs(title="days for customer's enrollment with the company box plot",  y="number of days", x="response")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

\#scale

``` r
mktcamp4<-mktcamp3 %>%
  mutate_if(is.numeric,scale)

skim(mktcamp4)
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
mktcamp4
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
2240
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
36
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
36
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
income
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
-2.02
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
24.54
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
kids
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
-0.83
</td>
<td style="text-align:right;">
-0.83
</td>
<td style="text-align:right;">
-0.83
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:left;">
▇▁▆▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teens
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
-0.93
</td>
<td style="text-align:right;">
-0.93
</td>
<td style="text-align:right;">
-0.93
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:left;">
▇▁▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
recency
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
-1.70
</td>
<td style="text-align:right;">
-0.87
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
1.72
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
wines
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
-0.90
</td>
<td style="text-align:right;">
-0.83
</td>
<td style="text-align:right;">
-0.39
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
3.53
</td>
<td style="text-align:left;">
▇▂▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fruits
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
-0.66
</td>
<td style="text-align:right;">
-0.64
</td>
<td style="text-align:right;">
-0.46
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
4.34
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
meat
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
-0.74
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
6.90
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fish
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
-0.69
</td>
<td style="text-align:right;">
-0.63
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
4.05
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
sweets
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
-0.66
</td>
<td style="text-align:right;">
-0.63
</td>
<td style="text-align:right;">
-0.46
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
5.72
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
gold
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
-0.84
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
-0.38
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
6.10
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
deals
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
-1.20
</td>
<td style="text-align:right;">
-0.69
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
6.56
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
web
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
-1.47
</td>
<td style="text-align:right;">
-0.75
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
8.25
</td>
<td style="text-align:left;">
▇▃▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
catalog
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
-0.91
</td>
<td style="text-align:right;">
-0.91
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
8.67
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
store
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
-1.78
</td>
<td style="text-align:right;">
-0.86
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
2.22
</td>
<td style="text-align:left;">
▂▇▂▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
visits
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
-2.19
</td>
<td style="text-align:right;">
-0.95
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
6.05
</td>
<td style="text-align:left;">
▆▇▃▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp3
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
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp4
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
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp5
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
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp1
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
-0.26
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
3.81
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp2
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
-0.12
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
8.58
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmplain
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
10.28
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Graduation
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
-1.01
</td>
<td style="text-align:right;">
-1.01
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:left;">
▇▁▁▁▇
</td>
</tr>
<tr>
<td style="text-align:left;">
Basic
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
-0.16
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
6.36
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Master
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
-0.44
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
2.25
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
PhD
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
-0.53
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
1.90
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
Cycle
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
-0.32
</td>
<td style="text-align:right;">
-0.32
</td>
<td style="text-align:right;">
-0.32
</td>
<td style="text-align:right;">
-0.32
</td>
<td style="text-align:right;">
3.17
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Absurd
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
-0.03
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
33.44
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Alone
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
-0.04
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
27.30
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Divorce
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
2.94
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
Married
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
-0.79
</td>
<td style="text-align:right;">
-0.79
</td>
<td style="text-align:right;">
-0.79
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:left;">
▇▁▁▁▅
</td>
</tr>
<tr>
<td style="text-align:left;">
Partner
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
-0.59
</td>
<td style="text-align:right;">
-0.59
</td>
<td style="text-align:right;">
-0.59
</td>
<td style="text-align:right;">
1.69
</td>
<td style="text-align:right;">
1.69
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
Single
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
-0.52
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
1.91
</td>
<td style="text-align:left;">
▇▁▁▁▂
</td>
</tr>
<tr>
<td style="text-align:left;">
Widow
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
5.30
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
YOLO
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
-0.03
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
33.44
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
dt_customer_days
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
-1.75
</td>
<td style="text-align:right;">
-0.86
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
age
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
-2.27
</td>
<td style="text-align:right;">
-0.68
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
6.33
</td>
<td style="text-align:left;">
▅▇▂▁▁
</td>
</tr>
</tbody>
</table>

# determine number of clusters and run kmeans

``` r
# how many clusters do we need?

fviz_nbclust(mktcamp4, kmeans, method="wss")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
set.seed(904)

clusters1 <- kmeans(mktcamp4, 5, iter.max = 200, nstart = 10)
print(clusters1)
```

    ## K-means clustering with 5 clusters of sizes 30, 550, 561, 569, 530
    ## 
    ## Cluster means:
    ##       income       kids       teens     recency      wines      fruits
    ## 1  0.7515333 -0.7012093 -0.13390550 -0.01528560  1.7668911 -0.08386416
    ## 2 -0.6701361  0.6473478 -0.12166272  0.05579913 -0.7575905 -0.49385306
    ## 3 -0.6212705  0.5820588 -0.04912255 -0.02470218 -0.7517772 -0.53007696
    ## 4  0.2796948 -0.4692295  0.79699830 -0.03660484  0.5735554 -0.03662971
    ## 5  1.0102175 -0.7444309 -0.66981627  0.00840589  0.8661547  1.11764262
    ##          meat        fish      sweets       gold       deals        web
    ## 1  0.36927037  0.02211074  0.08568340  0.4289673 -0.32345920  0.2933654
    ## 2 -0.61049936 -0.50825490 -0.49843569 -0.4898225 -0.05622309 -0.6181092
    ## 3 -0.62775217 -0.55159898 -0.53118240 -0.5364009 -0.12853035 -0.6713789
    ## 4 -0.07337382 -0.06566035 -0.01267919  0.3516645  0.70588007  0.9207181
    ## 5  1.35587776  1.18053706  1.08825832  0.6742587 -0.54512030  0.3470077
    ##      catalog      store      visits         cmp3       cmp4       cmp5
    ## 1  0.8568344  0.7310116 -0.06175242  0.618003845  2.5074566  1.9009775
    ## 2 -0.6992136 -0.7402563  0.50568522 -0.042144436 -0.2422445 -0.2800777
    ## 3 -0.6911641 -0.7535363  0.39918782  0.001215958 -0.2430587 -0.2800777
    ## 4  0.2508901  0.6905567  0.13680958  0.017554119  0.2246202 -0.2191985
    ## 5  1.1393379  0.7830524 -1.09068550 -0.011379475  0.1255807  0.7148320
    ##         cmp1       cmp2      cmplain  Graduation      Basic      Master
    ## 1  1.5043766  8.5810127 -0.097259948  0.06040436 -0.1571357 -0.26522723
    ## 2 -0.2620527 -0.1164843  0.053640335  0.99354756 -0.1571357 -0.44471635
    ## 3 -0.2547864 -0.1164843 -0.004796539 -1.00604502  0.4586677  0.46232761
    ## 4 -0.1330988 -0.1164843 -0.006096552 -0.09937562 -0.1456801  0.04264866
    ## 5  0.5993700 -0.1164843 -0.038536961  0.13711829 -0.1571357 -0.05864541
    ##           PhD       Cycle      Absurd       Alone     Divorce      Married
    ## 1  0.28226407 -0.08343805 -0.02988739 -0.03661261  0.20702452 -0.312979326
    ## 2 -0.52626729 -0.31561348 -0.02988739  0.01309178 -0.01768408  0.029341812
    ## 3  0.33847214  0.42312653 -0.02988739  0.01211719 -0.03569821  0.002249016
    ## 4  0.22826725 -0.13199495 -0.02988739  0.01143206  0.10995496  0.023566271
    ## 5 -0.07318462  0.02607867  0.09642913 -0.03661261 -0.07362673 -0.040414194
    ##        Partner      Single        Widow        YOLO dt_customer_days
    ## 1  0.321974946 -0.11602586 -0.005716184 -0.02988739      0.052037039
    ## 2 -0.026602763  0.02721334 -0.038973982 -0.02988739     -0.055809343
    ## 3  0.023356787  0.03381502 -0.071253609 -0.02988739     -0.217456773
    ## 4 -0.005336285 -0.13672292  0.062114527  0.08777123      0.268397317
    ## 5 -0.009612340  0.08931802  0.049504311 -0.02988739     -0.003001502
    ##            age
    ## 1  0.056113680
    ## 2 -0.266385159
    ## 3 -0.146204818
    ## 4  0.402189201
    ## 5 -0.003766652
    ## 
    ## Clustering vector:
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
    ##    5    2    4    2    3    4    4    3    3    3    2    3    5    3    2    5 
    ##   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32 
    ##    2    2    4    3    2    5    4    4    4    2    2    5    2    5    3    3 
    ##   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48 
    ##    2    4    5    3    4    3    2    4    5    3    2    3    3    5    3    2 
    ##   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64 
    ##    2    4    4    5    2    5    4    5    5    2    2    4    5    4    4    4 
    ##   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79   80 
    ##    5    2    2    5    4    4    5    3    4    4    2    2    5    5    3    4 
    ##   81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96 
    ##    2    2    3    3    5    2    3    4    5    2    4    2    5    3    3    2 
    ##   97   98   99  100  101  102  103  104  105  106  107  108  109  110  111  112 
    ##    4    2    5    4    3    3    5    5    5    3    2    4    2    5    5    4 
    ##  113  114  115  116  117  118  119  120  121  122  123  124  125  126  127  128 
    ##    4    5    4    2    5    4    3    2    4    3    3    2    5    5    5    3 
    ##  129  130  131  132  133  134  135  136  137  138  139  140  141  142  143  144 
    ##    4    4    4    4    5    4    3    5    3    3    3    3    5    4    5    4 
    ##  145  146  147  148  149  150  151  152  153  154  155  156  157  158  159  160 
    ##    2    4    2    2    3    3    4    4    4    2    4    5    2    2    2    5 
    ##  161  162  163  164  165  166  167  168  169  170  171  172  173  174  175  176 
    ##    3    4    2    4    5    2    4    2    5    2    2    2    2    3    2    5 
    ##  177  178  179  180  181  182  183  184  185  186  187  188  189  190  191  192 
    ##    5    3    2    5    3    3    4    3    3    2    3    4    5    3    3    5 
    ##  193  194  195  196  197  198  199  200  201  202  203  204  205  206  207  208 
    ##    3    2    2    2    4    5    5    3    4    5    4    5    3    3    2    3 
    ##  209  210  211  212  213  214  215  216  217  218  219  220  221  222  223  224 
    ##    2    4    4    5    4    4    5    2    3    5    3    4    3    5    4    3 
    ##  225  226  227  228  229  230  231  232  233  234  235  236  237  238  239  240 
    ##    4    2    4    4    5    3    4    5    2    2    4    3    3    4    2    3 
    ##  241  242  243  244  245  246  247  248  249  250  251  252  253  254  255  256 
    ##    5    5    2    5    4    3    5    4    5    4    3    2    1    3    4    3 
    ##  257  258  259  260  261  262  263  264  265  266  267  268  269  270  271  272 
    ##    4    4    2    3    2    4    3    3    3    3    5    3    5    2    5    3 
    ##  273  274  275  276  277  278  279  280  281  282  283  284  285  286  287  288 
    ##    3    2    2    4    5    5    5    4    3    4    2    4    3    2    5    4 
    ##  289  290  291  292  293  294  295  296  297  298  299  300  301  302  303  304 
    ##    5    4    3    2    5    2    2    1    3    3    5    4    2    4    3    2 
    ##  305  306  307  308  309  310  311  312  313  314  315  316  317  318  319  320 
    ##    2    5    2    5    4    3    2    2    5    5    2    3    2    2    2    4 
    ##  321  322  323  324  325  326  327  328  329  330  331  332  333  334  335  336 
    ##    4    2    3    4    4    5    3    2    3    3    2    3    4    3    2    4 
    ##  337  338  339  340  341  342  343  344  345  346  347  348  349  350  351  352 
    ##    5    2    5    5    5    2    1    4    3    5    2    5    2    3    4    5 
    ##  353  354  355  356  357  358  359  360  361  362  363  364  365  366  367  368 
    ##    4    5    4    2    2    5    4    4    5    4    2    3    4    4    5    2 
    ##  369  370  371  372  373  374  375  376  377  378  379  380  381  382  383  384 
    ##    5    4    3    2    3    4    3    3    2    2    4    2    3    2    2    3 
    ##  385  386  387  388  389  390  391  392  393  394  395  396  397  398  399  400 
    ##    3    2    4    4    2    4    5    3    5    3    4    5    2    2    2    3 
    ##  401  402  403  404  405  406  407  408  409  410  411  412  413  414  415  416 
    ##    3    5    2    2    4    2    3    3    3    4    3    4    4    2    4    5 
    ##  417  418  419  420  421  422  423  424  425  426  427  428  429  430  431  432 
    ##    3    1    5    4    3    2    3    5    5    2    5    5    3    5    1    4 
    ##  433  434  435  436  437  438  439  440  441  442  443  444  445  446  447  448 
    ##    4    4    2    3    4    4    3    3    2    3    2    2    3    2    3    5 
    ##  449  450  451  452  453  454  455  456  457  458  459  460  461  462  463  464 
    ##    2    4    4    4    3    3    4    4    5    3    2    5    4    5    3    5 
    ##  465  466  467  468  469  470  471  472  473  474  475  476  477  478  479  480 
    ##    2    5    5    2    4    4    5    2    4    2    2    4    3    4    4    2 
    ##  481  482  483  484  485  486  487  488  489  490  491  492  493  494  495  496 
    ##    3    3    2    2    5    5    4    4    2    3    5    2    5    4    4    4 
    ##  497  498  499  500  501  502  503  504  505  506  507  508  509  510  511  512 
    ##    2    4    4    4    2    3    3    4    3    4    5    5    2    4    3    5 
    ##  513  514  515  516  517  518  519  520  521  522  523  524  525  526  527  528 
    ##    4    5    4    5    2    2    5    5    2    5    2    5    2    2    5    3 
    ##  529  530  531  532  533  534  535  536  537  538  539  540  541  542  543  544 
    ##    5    4    4    5    4    2    2    4    2    5    3    3    3    2    3    5 
    ##  545  546  547  548  549  550  551  552  553  554  555  556  557  558  559  560 
    ##    4    3    5    2    2    3    3    2    4    2    5    2    4    5    3    1 
    ##  561  562  563  564  565  566  567  568  569  570  571  572  573  574  575  576 
    ##    2    5    4    5    4    3    4    3    2    2    4    3    3    3    3    1 
    ##  577  578  579  580  581  582  583  584  585  586  587  588  589  590  591  592 
    ##    2    2    3    3    3    4    3    3    2    3    5    4    4    2    3    5 
    ##  593  594  595  596  597  598  599  600  601  602  603  604  605  606  607  608 
    ##    5    2    3    3    2    3    3    2    2    4    5    4    3    3    2    2 
    ##  609  610  611  612  613  614  615  616  617  618  619  620  621  622  623  624 
    ##    3    5    3    3    3    2    4    2    3    3    2    4    4    2    5    3 
    ##  625  626  627  628  629  630  631  632  633  634  635  636  637  638  639  640 
    ##    5    2    5    4    3    2    4    5    5    3    5    2    5    4    4    4 
    ##  641  642  643  644  645  646  647  648  649  650  651  652  653  654  655  656 
    ##    4    5    4    4    5    3    5    3    4    4    5    2    4    3    4    2 
    ##  657  658  659  660  661  662  663  664  665  666  667  668  669  670  671  672 
    ##    2    3    4    3    4    2    5    3    3    2    3    3    3    3    4    4 
    ##  673  674  675  676  677  678  679  680  681  682  683  684  685  686  687  688 
    ##    5    4    4    2    4    5    3    5    4    5    3    2    5    4    5    5 
    ##  689  690  691  692  693  694  695  696  697  698  699  700  701  702  703  704 
    ##    5    5    4    5    3    3    3    3    2    3    4    5    4    2    4    5 
    ##  705  706  707  708  709  710  711  712  713  714  715  716  717  718  719  720 
    ##    3    5    3    4    4    3    2    4    2    4    2    5    5    2    4    2 
    ##  721  722  723  724  725  726  727  728  729  730  731  732  733  734  735  736 
    ##    4    4    3    5    2    2    5    5    3    5    3    4    4    4    5    5 
    ##  737  738  739  740  741  742  743  744  745  746  747  748  749  750  751  752 
    ##    5    4    3    5    4    3    3    3    5    5    3    5    2    5    5    4 
    ##  753  754  755  756  757  758  759  760  761  762  763  764  765  766  767  768 
    ##    5    5    5    5    4    4    3    3    3    4    5    3    5    2    5    5 
    ##  769  770  771  772  773  774  775  776  777  778  779  780  781  782  783  784 
    ##    3    4    4    5    4    2    3    2    3    5    3    5    5    2    2    2 
    ##  785  786  787  788  789  790  791  792  793  794  795  796  797  798  799  800 
    ##    2    3    3    4    4    4    5    5    2    2    2    3    4    4    5    2 
    ##  801  802  803  804  805  806  807  808  809  810  811  812  813  814  815  816 
    ##    4    2    2    5    1    4    3    4    4    5    2    2    5    4    5    4 
    ##  817  818  819  820  821  822  823  824  825  826  827  828  829  830  831  832 
    ##    2    4    4    3    5    2    1    2    1    5    4    2    4    2    3    4 
    ##  833  834  835  836  837  838  839  840  841  842  843  844  845  846  847  848 
    ##    4    3    2    5    3    5    3    4    2    2    2    2    5    1    5    4 
    ##  849  850  851  852  853  854  855  856  857  858  859  860  861  862  863  864 
    ##    2    2    2    4    5    3    4    5    3    2    2    5    2    3    2    3 
    ##  865  866  867  868  869  870  871  872  873  874  875  876  877  878  879  880 
    ##    3    4    4    3    4    2    1    4    2    3    5    5    4    2    5    3 
    ##  881  882  883  884  885  886  887  888  889  890  891  892  893  894  895  896 
    ##    2    2    2    3    5    5    3    2    4    5    2    4    5    3    4    4 
    ##  897  898  899  900  901  902  903  904  905  906  907  908  909  910  911  912 
    ##    4    5    3    2    5    2    5    2    4    5    5    2    2    3    5    5 
    ##  913  914  915  916  917  918  919  920  921  922  923  924  925  926  927  928 
    ##    4    3    5    5    4    5    3    5    1    5    3    3    5    3    5    5 
    ##  929  930  931  932  933  934  935  936  937  938  939  940  941  942  943  944 
    ##    5    5    5    2    4    3    5    3    5    4    4    4    4    4    5    5 
    ##  945  946  947  948  949  950  951  952  953  954  955  956  957  958  959  960 
    ##    3    4    4    2    2    4    3    3    3    3    3    3    4    4    2    4 
    ##  961  962  963  964  965  966  967  968  969  970  971  972  973  974  975  976 
    ##    5    4    3    3    2    4    5    2    3    4    5    3    3    4    5    5 
    ##  977  978  979  980  981  982  983  984  985  986  987  988  989  990  991  992 
    ##    5    3    2    4    3    2    3    4    5    4    5    5    5    3    1    3 
    ##  993  994  995  996  997  998  999 1000 1001 1002 1003 1004 1005 1006 1007 1008 
    ##    4    5    2    2    5    2    4    4    4    5    4    2    4    4    4    5 
    ## 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 1023 1024 
    ##    2    3    5    3    2    3    3    3    5    2    2    2    2    3    4    3 
    ## 1025 1026 1027 1028 1029 1030 1031 1032 1033 1034 1035 1036 1037 1038 1039 1040 
    ##    2    4    2    3    3    4    5    5    5    3    5    2    3    3    3    4 
    ## 1041 1042 1043 1044 1045 1046 1047 1048 1049 1050 1051 1052 1053 1054 1055 1056 
    ##    4    2    2    5    2    3    3    5    2    4    5    3    5    3    3    5 
    ## 1057 1058 1059 1060 1061 1062 1063 1064 1065 1066 1067 1068 1069 1070 1071 1072 
    ##    2    3    5    5    4    4    5    3    4    2    5    4    2    5    3    5 
    ## 1073 1074 1075 1076 1077 1078 1079 1080 1081 1082 1083 1084 1085 1086 1087 1088 
    ##    4    2    4    5    5    3    3    3    5    2    5    2    5    4    2    1 
    ## 1089 1090 1091 1092 1093 1094 1095 1096 1097 1098 1099 1100 1101 1102 1103 1104 
    ##    2    5    5    3    5    3    3    4    4    5    3    4    5    3    2    2 
    ## 1105 1106 1107 1108 1109 1110 1111 1112 1113 1114 1115 1116 1117 1118 1119 1120 
    ##    2    5    3    3    4    2    5    5    3    5    3    4    3    3    3    4 
    ## 1121 1122 1123 1124 1125 1126 1127 1128 1129 1130 1131 1132 1133 1134 1135 1136 
    ##    4    3    3    2    4    4    4    2    3    5    4    2    2    5    5    3 
    ## 1137 1138 1139 1140 1141 1142 1143 1144 1145 1146 1147 1148 1149 1150 1151 1152 
    ##    3    5    3    2    4    2    3    3    5    3    2    4    4    2    4    4 
    ## 1153 1154 1155 1156 1157 1158 1159 1160 1161 1162 1163 1164 1165 1166 1167 1168 
    ##    2    5    3    3    4    5    5    4    2    4    4    5    3    4    3    2 
    ## 1169 1170 1171 1172 1173 1174 1175 1176 1177 1178 1179 1180 1181 1182 1183 1184 
    ##    5    5    3    3    5    4    3    3    3    4    2    5    4    2    4    3 
    ## 1185 1186 1187 1188 1189 1190 1191 1192 1193 1194 1195 1196 1197 1198 1199 1200 
    ##    2    2    2    4    3    3    5    4    2    2    3    4    3    4    5    5 
    ## 1201 1202 1203 1204 1205 1206 1207 1208 1209 1210 1211 1212 1213 1214 1215 1216 
    ##    2    4    3    2    5    4    5    2    3    2    4    5    4    5    2    2 
    ## 1217 1218 1219 1220 1221 1222 1223 1224 1225 1226 1227 1228 1229 1230 1231 1232 
    ##    4    3    5    2    3    2    5    2    3    4    4    2    4    3    2    3 
    ## 1233 1234 1235 1236 1237 1238 1239 1240 1241 1242 1243 1244 1245 1246 1247 1248 
    ##    2    2    3    4    3    1    2    2    3    3    5    5    3    2    3    2 
    ## 1249 1250 1251 1252 1253 1254 1255 1256 1257 1258 1259 1260 1261 1262 1263 1264 
    ##    3    4    5    4    4    5    4    4    4    5    2    5    3    5    5    2 
    ## 1265 1266 1267 1268 1269 1270 1271 1272 1273 1274 1275 1276 1277 1278 1279 1280 
    ##    2    5    4    2    3    5    4    4    2    4    3    4    3    2    5    4 
    ## 1281 1282 1283 1284 1285 1286 1287 1288 1289 1290 1291 1292 1293 1294 1295 1296 
    ##    5    1    3    4    3    2    4    3    5    5    2    2    3    3    3    3 
    ## 1297 1298 1299 1300 1301 1302 1303 1304 1305 1306 1307 1308 1309 1310 1311 1312 
    ##    3    4    5    2    3    5    3    2    5    4    2    5    1    4    4    5 
    ## 1313 1314 1315 1316 1317 1318 1319 1320 1321 1322 1323 1324 1325 1326 1327 1328 
    ##    3    5    3    3    4    3    2    2    5    4    5    2    3    4    2    3 
    ## 1329 1330 1331 1332 1333 1334 1335 1336 1337 1338 1339 1340 1341 1342 1343 1344 
    ##    3    5    2    2    5    5    5    2    5    3    2    2    4    4    4    2 
    ## 1345 1346 1347 1348 1349 1350 1351 1352 1353 1354 1355 1356 1357 1358 1359 1360 
    ##    3    2    3    4    4    5    5    5    2    2    5    5    2    4    5    3 
    ## 1361 1362 1363 1364 1365 1366 1367 1368 1369 1370 1371 1372 1373 1374 1375 1376 
    ##    4    3    4    4    4    5    2    2    3    2    2    3    3    4    4    4 
    ## 1377 1378 1379 1380 1381 1382 1383 1384 1385 1386 1387 1388 1389 1390 1391 1392 
    ##    4    2    4    3    3    3    2    3    2    5    3    3    2    5    3    2 
    ## 1393 1394 1395 1396 1397 1398 1399 1400 1401 1402 1403 1404 1405 1406 1407 1408 
    ##    3    3    3    4    2    2    4    4    2    4    2    2    4    3    4    4 
    ## 1409 1410 1411 1412 1413 1414 1415 1416 1417 1418 1419 1420 1421 1422 1423 1424 
    ##    4    3    4    5    3    5    2    2    3    2    2    2    3    5    4    3 
    ## 1425 1426 1427 1428 1429 1430 1431 1432 1433 1434 1435 1436 1437 1438 1439 1440 
    ##    2    2    3    3    3    2    3    3    5    2    2    3    2    3    2    2 
    ## 1441 1442 1443 1444 1445 1446 1447 1448 1449 1450 1451 1452 1453 1454 1455 1456 
    ##    2    3    2    5    5    2    5    4    5    4    2    5    5    3    4    5 
    ## 1457 1458 1459 1460 1461 1462 1463 1464 1465 1466 1467 1468 1469 1470 1471 1472 
    ##    2    2    5    4    4    4    1    2    3    4    4    5    3    4    3    2 
    ## 1473 1474 1475 1476 1477 1478 1479 1480 1481 1482 1483 1484 1485 1486 1487 1488 
    ##    2    4    4    2    5    3    3    4    5    4    3    2    5    4    5    4 
    ## 1489 1490 1491 1492 1493 1494 1495 1496 1497 1498 1499 1500 1501 1502 1503 1504 
    ##    5    3    4    3    5    4    2    5    2    4    4    5    4    3    2    4 
    ## 1505 1506 1507 1508 1509 1510 1511 1512 1513 1514 1515 1516 1517 1518 1519 1520 
    ##    4    5    4    4    5    4    5    3    5    5    3    2    4    2    3    2 
    ## 1521 1522 1523 1524 1525 1526 1527 1528 1529 1530 1531 1532 1533 1534 1535 1536 
    ##    5    5    2    3    2    4    5    2    5    2    4    3    4    3    3    3 
    ## 1537 1538 1539 1540 1541 1542 1543 1544 1545 1546 1547 1548 1549 1550 1551 1552 
    ##    2    5    4    4    2    4    5    3    4    3    3    2    4    3    3    5 
    ## 1553 1554 1555 1556 1557 1558 1559 1560 1561 1562 1563 1564 1565 1566 1567 1568 
    ##    5    5    4    3    3    2    2    4    2    4    2    5    2    4    4    4 
    ## 1569 1570 1571 1572 1573 1574 1575 1576 1577 1578 1579 1580 1581 1582 1583 1584 
    ##    4    4    5    2    5    3    2    4    2    5    2    4    2    5    5    3 
    ## 1585 1586 1587 1588 1589 1590 1591 1592 1593 1594 1595 1596 1597 1598 1599 1600 
    ##    5    3    3    2    2    5    3    5    3    3    3    4    3    2    5    3 
    ## 1601 1602 1603 1604 1605 1606 1607 1608 1609 1610 1611 1612 1613 1614 1615 1616 
    ##    4    5    4    2    2    3    4    4    4    4    5    3    5    2    2    5 
    ## 1617 1618 1619 1620 1621 1622 1623 1624 1625 1626 1627 1628 1629 1630 1631 1632 
    ##    2    2    2    4    3    2    5    4    2    3    4    4    2    3    4    3 
    ## 1633 1634 1635 1636 1637 1638 1639 1640 1641 1642 1643 1644 1645 1646 1647 1648 
    ##    3    2    4    4    4    5    2    3    3    1    4    3    5    3    4    5 
    ## 1649 1650 1651 1652 1653 1654 1655 1656 1657 1658 1659 1660 1661 1662 1663 1664 
    ##    5    3    2    5    2    5    3    3    3    4    4    5    4    2    4    2 
    ## 1665 1666 1667 1668 1669 1670 1671 1672 1673 1674 1675 1676 1677 1678 1679 1680 
    ##    3    3    4    3    4    4    3    5    5    5    5    3    2    3    3    2 
    ## 1681 1682 1683 1684 1685 1686 1687 1688 1689 1690 1691 1692 1693 1694 1695 1696 
    ##    5    2    3    3    2    2    5    1    4    5    4    5    3    3    2    4 
    ## 1697 1698 1699 1700 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711 1712 
    ##    3    5    3    5    4    3    2    4    3    3    4    2    5    3    1    5 
    ## 1713 1714 1715 1716 1717 1718 1719 1720 1721 1722 1723 1724 1725 1726 1727 1728 
    ##    2    5    3    2    4    2    2    4    2    5    5    5    3    3    2    3 
    ## 1729 1730 1731 1732 1733 1734 1735 1736 1737 1738 1739 1740 1741 1742 1743 1744 
    ##    4    5    3    2    2    5    4    4    4    5    4    3    2    2    3    5 
    ## 1745 1746 1747 1748 1749 1750 1751 1752 1753 1754 1755 1756 1757 1758 1759 1760 
    ##    4    5    3    4    4    5    4    4    3    4    2    3    2    3    3    5 
    ## 1761 1762 1763 1764 1765 1766 1767 1768 1769 1770 1771 1772 1773 1774 1775 1776 
    ##    4    3    4    4    3    4    3    5    3    3    3    3    5    5    2    2 
    ## 1777 1778 1779 1780 1781 1782 1783 1784 1785 1786 1787 1788 1789 1790 1791 1792 
    ##    2    3    3    5    3    3    5    4    2    2    4    2    4    5    2    3 
    ## 1793 1794 1795 1796 1797 1798 1799 1800 1801 1802 1803 1804 1805 1806 1807 1808 
    ##    5    2    2    3    4    3    4    5    5    5    3    4    2    4    4    2 
    ## 1809 1810 1811 1812 1813 1814 1815 1816 1817 1818 1819 1820 1821 1822 1823 1824 
    ##    5    5    2    4    5    5    5    3    4    5    4    3    5    3    3    4 
    ## 1825 1826 1827 1828 1829 1830 1831 1832 1833 1834 1835 1836 1837 1838 1839 1840 
    ##    5    2    4    5    5    3    2    2    4    2    2    2    5    4    4    4 
    ## 1841 1842 1843 1844 1845 1846 1847 1848 1849 1850 1851 1852 1853 1854 1855 1856 
    ##    3    5    3    5    3    5    3    2    4    2    5    2    5    5    5    4 
    ## 1857 1858 1859 1860 1861 1862 1863 1864 1865 1866 1867 1868 1869 1870 1871 1872 
    ##    3    3    5    2    4    3    4    5    5    4    3    2    3    5    3    5 
    ## 1873 1874 1875 1876 1877 1878 1879 1880 1881 1882 1883 1884 1885 1886 1887 1888 
    ##    2    4    2    3    4    5    4    4    5    3    3    4    3    2    3    5 
    ## 1889 1890 1891 1892 1893 1894 1895 1896 1897 1898 1899 1900 1901 1902 1903 1904 
    ##    4    2    5    5    2    4    5    2    3    5    4    4    3    2    3    2 
    ## 1905 1906 1907 1908 1909 1910 1911 1912 1913 1914 1915 1916 1917 1918 1919 1920 
    ##    4    5    4    2    2    3    5    5    5    5    4    1    2    3    2    3 
    ## 1921 1922 1923 1924 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 
    ##    3    5    1    5    4    3    1    5    5    4    3    4    3    3    5    3 
    ## 1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 
    ##    4    4    3    2    5    2    2    5    3    5    5    5    2    2    3    4 
    ## 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 
    ##    5    5    3    5    2    4    5    3    4    1    2    2    2    5    4    5 
    ## 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 
    ##    5    5    4    2    2    4    3    4    5    3    2    3    2    3    5    3 
    ## 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 
    ##    4    2    2    2    2    4    5    3    5    5    2    5    5    4    3    3 
    ## 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
    ##    2    3    2    2    3    2    5    4    4    4    5    2    5    4    5    3 
    ## 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 
    ##    2    2    2    2    2    3    2    4    4    3    2    2    4    3    4    4 
    ## 2033 2034 2035 2036 2037 2038 2039 2040 2041 2042 2043 2044 2045 2046 2047 2048 
    ##    2    4    2    4    4    4    2    5    5    4    2    3    2    2    2    5 
    ## 2049 2050 2051 2052 2053 2054 2055 2056 2057 2058 2059 2060 2061 2062 2063 2064 
    ##    5    5    2    4    4    3    2    2    5    4    4    4    2    3    5    2 
    ## 2065 2066 2067 2068 2069 2070 2071 2072 2073 2074 2075 2076 2077 2078 2079 2080 
    ##    4    4    2    4    2    2    4    5    2    5    5    4    3    3    2    3 
    ## 2081 2082 2083 2084 2085 2086 2087 2088 2089 2090 2091 2092 2093 2094 2095 2096 
    ##    2    4    4    5    5    4    5    5    2    3    4    3    2    5    4    3 
    ## 2097 2098 2099 2100 2101 2102 2103 2104 2105 2106 2107 2108 2109 2110 2111 2112 
    ##    5    2    5    1    3    4    3    4    3    3    3    4    3    5    4    4 
    ## 2113 2114 2115 2116 2117 2118 2119 2120 2121 2122 2123 2124 2125 2126 2127 2128 
    ##    2    3    4    4    3    4    5    2    3    3    2    4    2    1    4    5 
    ## 2129 2130 2131 2132 2133 2134 2135 2136 2137 2138 2139 2140 2141 2142 2143 2144 
    ##    4    2    2    5    3    3    5    4    2    2    3    3    2    2    2    2 
    ## 2145 2146 2147 2148 2149 2150 2151 2152 2153 2154 2155 2156 2157 2158 2159 2160 
    ##    4    5    3    3    3    3    3    5    2    2    3    3    3    3    4    2 
    ## 2161 2162 2163 2164 2165 2166 2167 2168 2169 2170 2171 2172 2173 2174 2175 2176 
    ##    4    2    4    5    3    3    5    1    5    2    4    1    5    4    4    5 
    ## 2177 2178 2179 2180 2181 2182 2183 2184 2185 2186 2187 2188 2189 2190 2191 2192 
    ##    5    4    4    2    2    4    2    3    3    4    4    5    5    3    5    2 
    ## 2193 2194 2195 2196 2197 2198 2199 2200 2201 2202 2203 2204 2205 2206 2207 2208 
    ##    2    5    4    3    2    4    4    2    3    4    4    5    2    2    4    2 
    ## 2209 2210 2211 2212 2213 2214 2215 2216 2217 2218 2219 2220 2221 2222 2223 2224 
    ##    3    2    4    5    3    5    2    2    2    5    2    3    4    5    3    3 
    ## 2225 2226 2227 2228 2229 2230 2231 2232 2233 2234 2235 2236 2237 2238 2239 2240 
    ##    4    4    4    4    5    2    2    4    2    5    2    4    4    4    4    3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  1112.182  8737.047 13454.876 17524.614 20543.028
    ##  (between_SS / total_SS =  23.9 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
# visualize clusters

fviz_cluster(clusters1,mktcamp4,ellipse.type="norm",geom="point")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
ggplot(mktcamp4,aes(clusters1$cluster))+geom_bar()
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggplot(mktcamp,aes(mar_stat))+geom_bar()
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

``` r
ggplot(mktcamp,aes(mar_stat))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-3.png)<!-- -->

``` r
ggplot(mktcamp,aes(education))+geom_bar()
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-4.png)<!-- -->

``` r
ggplot(mktcamp,aes(education))+geom_bar()+facet_wrap(~clusters1$cluster)+theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-5.png)<!-- -->

``` r
ggplot(mktcamp2,aes(x=birth))+geom_histogram(binwidth=5)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-6.png)<!-- -->

``` r
ggplot(mktcamp2,aes(x=birth))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-7.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=income))+geom_histogram(binwidth=10000)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-8.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=income))+geom_histogram(binwidth=10000) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-9.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=kids))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-10.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=kids))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-11.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=teens))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-12.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=teens))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-13.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=recency))+geom_histogram(binwidth=5)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-14.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=recency))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-15.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=wines))+geom_histogram(binwidth=100)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-16.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=wines))+geom_histogram(binwidth=100) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-17.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=fruits))+geom_histogram(binwidth=10)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-18.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=fruits))+geom_histogram(binwidth=10) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-19.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=meat))+geom_histogram(binwidth=100)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-20.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=meat))+geom_histogram(binwidth=100) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-21.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=fish))+geom_histogram(binwidth=50)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-22.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=fish))+geom_histogram(binwidth=50) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-23.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=sweets))+geom_histogram(binwidth=50)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-24.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=sweets))+geom_histogram(binwidth=50) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-25.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=gold))+geom_histogram(binwidth=50)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-26.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=gold))+geom_histogram(binwidth=50) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-27.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=deals))+geom_histogram(binwidth=5)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-28.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=deals))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-29.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=web))+geom_histogram(binwidth=5)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-30.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=web))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-31.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=catalog))+geom_histogram(binwidth=5)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-32.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=catalog))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-33.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=store))+geom_histogram(binwidth=5)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-34.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=store))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-35.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=visits))+geom_histogram(binwidth=5)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-36.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=visits))+geom_histogram(binwidth=5) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-37.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp1))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-38.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp1))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-39.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp2))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-40.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp2))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-41.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp3))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-42.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp3))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-43.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp4))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-44.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp4))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-45.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp5))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-46.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmp5))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster)  
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-47.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmplain))+geom_histogram(binwidth=0.1)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-48.png)<!-- -->

``` r
ggplot(mktcamp3,aes(x=cmplain))+geom_histogram(binwidth=0.1) + facet_wrap(~clusters1$cluster) 
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-49.png)<!-- -->

``` r
ggplot(mktcamp2,aes(x=dt_customer))+geom_histogram(binwidth=100)
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-50.png)<!-- -->

``` r
ggplot(mktcamp2,aes(x=dt_customer))+geom_histogram(binwidth=100) + facet_wrap(~clusters1$cluster) +
theme(axis.text.x = element_text(angle=45,hjust = 1))
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-21-51.png)<!-- -->

``` r
mktcamp2 %>% 
  group_by(clusters1$cluster) %>% 
  summarize(mean_birth = round(mean(birth)),
            min_birth = round(min(birth)),
            max_birth = round(max(birth)),
            mean_age = round(mean(age)),
            min_age = round(min(age)),
            max_age = round(max(age)),
            mean_dt_customer = mean(dt_customer),
            min_dt_customer = min(dt_customer),
            max_dt_customer = max(dt_customer),
             mean_dt_customer_days = mean(dt_customer_days),
            min_dt_customer_days = min(dt_customer_days),
            max_dt_customer_days = max(dt_customer_days)
            )
```

    ## # A tibble: 5 × 13
    ##   cluste…¹ mean_…² min_b…³ max_b…⁴ mean_…⁵ min_age max_age mean_dt_…⁶ min_dt_c…⁷
    ##      <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <date>     <date>    
    ## 1        1    1968    1952    1994      54      28      70 2013-06-29 2012-08-18
    ## 2        2    1972    1946    1995      50      27      76 2013-07-21 2012-08-01
    ## 3        3    1971    1893    1996      51      26     129 2013-08-23 2012-08-01
    ## 4        4    1964    1943    1992      58      30      79 2013-05-17 2012-07-30
    ## 5        5    1969    1899    1995      53      27     123 2013-07-11 2012-08-01
    ## # … with 4 more variables: max_dt_customer <date>, mean_dt_customer_days <dbl>,
    ## #   min_dt_customer_days <dbl>, max_dt_customer_days <dbl>, and abbreviated
    ## #   variable names ¹​`clusters1$cluster`, ²​mean_birth, ³​min_birth, ⁴​max_birth,
    ## #   ⁵​mean_age, ⁶​mean_dt_customer, ⁷​min_dt_customer
    ## # ℹ Use `colnames()` to see all variable names

``` r
mktcamp3 %>% 
  group_by(clusters1$cluster) %>% 
  summarize(
                mean_income = mean(income),
            min_income = min(income),
            max_income = max(income),
                        mean_kids = mean(kids),
            min_kids = min(kids),
            max_kids = max(kids),
                        mean_teens = mean(teens),
            min_teens = min(teens),
            max_teens = max(teens),
            mean_recency = mean(recency),
            min_recency = min(recency),
            max_recency = max(recency),  
            mean_wines = mean(wines),
            min_wines = min(wines),
            max_wines = max(wines),
            mean_fruits = mean(fruits),
            min_fruits = min(fruits),
            max_fruits= max(fruits),
            mean_meat = mean(meat),
            min_meat = min(meat),
            max_meat = max(meat),
            mean_fish = mean(fish),
            min_fish = min(fish),
            max_fish= max(fish),
            mean_sweets= mean(sweets),
            min_sweets = min(sweets),
            max_sweets= max(sweets),
            mean_gold = mean(gold),
            min_gold = min(gold),
            max_gold= max(gold),
            mean_deals = mean(deals),
            min_deals = min(deals),
            max_deals= max(deals),
            mean_web = mean(web),
            min_web = min(web),
            max_web= max(web),
            mean_catalog = mean(catalog),
            min_catalog = min(catalog),
            max_catalog= max(catalog),
            mean_store = mean(store),
            min_store = min(store),
            max_store= max(store),
            mean_visits = mean(visits),
            min_visits = min(visits),
            max_visits= max(visits),
            mean_cmp1 = round(mean(cmp1)),
            min_cmp1 = min(cmp1),
            max_cmp1= max(cmp1),
            mean_cmp2 = round(mean(cmp2)),
            min_cmp2 = min(cmp2),
            max_cmp2= max(cmp2),
            mean_cmp3 = round(mean(cmp3)),
            min_cmp3= min(cmp3),
            max_cmp3= max(cmp3),
            mean_cmp4 = round(mean(cmp4)),
            min_cmp4 = min(cmp4),
            max_cmp4= max(cmp4),
            mean_cmp5 = round(mean(cmp5)),
            min_cmp5= min(cmp5),
            max_cmp5= max(cmp5),
            mean_cmplain = round(mean(cmplain)),
            min_cmplain = round(min(cmplain)),
            max_cmplain = round(max(cmplain))
            )
```

    ## # A tibble: 5 × 64
    ##   clusters1$cl…¹ mean_…² min_i…³ max_i…⁴ mean_…⁵ min_k…⁶ max_k…⁷ mean_…⁸ min_t…⁹
    ##            <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1              1  71055.   46015  102692  0.0667       0       1   0.433       0
    ## 2              2  35459.    1730  153924  0.793        0       2   0.44        0
    ## 3              3  36683.    4023  162397  0.758        0       2   0.480       0
    ## 4              4  59241.    4428  113734  0.192        0       2   0.940       0
    ## 5              5  77532.    2447  666666  0.0434       0       1   0.142       0
    ## # … with 55 more variables: max_teens <dbl>, mean_recency <dbl>,
    ## #   min_recency <dbl>, max_recency <dbl>, mean_wines <dbl>, min_wines <dbl>,
    ## #   max_wines <dbl>, mean_fruits <dbl>, min_fruits <dbl>, max_fruits <dbl>,
    ## #   mean_meat <dbl>, min_meat <dbl>, max_meat <dbl>, mean_fish <dbl>,
    ## #   min_fish <dbl>, max_fish <dbl>, mean_sweets <dbl>, min_sweets <dbl>,
    ## #   max_sweets <dbl>, mean_gold <dbl>, min_gold <dbl>, max_gold <dbl>,
    ## #   mean_deals <dbl>, min_deals <dbl>, max_deals <dbl>, mean_web <dbl>, …
    ## # ℹ Use `colnames()` to see all variable names

### An indication of whether the segment includes a high percentage of customers who responded positively to the previous campaign (based on the variable response).

``` r
mktcamp2$cluster <- clusters1$cluster
df<-mktcamp2%>%
  group_by(cluster)%>%  
  count(response)
mktcamp2%>%
  group_by(cluster)%>%
  summarise(n1=n())%>%
  inner_join(df ,by = "cluster")%>%
  mutate(pct=n/n1)%>%
  filter(response=='1')%>%
  dplyr::select(cluster,pct)%>%
  ggplot(aes(x=cluster,y=pct))+
  geom_col()+
  labs(title = "postive response pct in each segment",y="percentage")+
  geom_text(aes(label=paste(round(pct*100,2),"%")),vjust=1.5,color="white")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

\#logistic regression \#log data prep

``` r
mktcamp$dt_customer<-as.Date(ifelse(is.na(format_dmy),format_dmy2,format_dmy),origin ="1970-01-01")

mktcamp_log<-mktcamp %>%
  mutate(response=as.factor(response))%>%
  mutate(education=as.factor(education))%>%
  mutate(mar_stat=as.factor(mar_stat))%>%
  mutate(dt_customer_days = Sys.Date()-dt_customer)%>%
  mutate(dt_customer_days =as.numeric(dt_customer_days))%>%
  mutate(age=2022-birth)
mktcamp_log<-subset(mktcamp_log,select=-c(dt_customer,birth))
#mktcamp_log$cluster <- clusters1$cluster
skim(mktcamp_log)
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
mktcamp_log
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
2240
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
26
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
23
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
education
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
5
</td>
<td style="text-align:left;">
Gra: 1127, PhD: 486, Mas: 370, 2nC: 203
</td>
</tr>
<tr>
<td style="text-align:left;">
mar_stat
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
8
</td>
<td style="text-align:left;">
Mar: 864, Par: 580, Sin: 480, Div: 232
</td>
</tr>
<tr>
<td style="text-align:left;">
response
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
0: 1904, 1: 336
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
income
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
52247.25
</td>
<td style="text-align:right;">
25173.08
</td>
<td style="text-align:right;">
1730
</td>
<td style="text-align:right;">
35303.00
</td>
<td style="text-align:right;">
51381.5
</td>
<td style="text-align:right;">
68522.00
</td>
<td style="text-align:right;">
666666
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
kids
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
▇▁▆▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
teens
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
▇▁▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
recency
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
49.11
</td>
<td style="text-align:right;">
28.96
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
24.00
</td>
<td style="text-align:right;">
49.0
</td>
<td style="text-align:right;">
74.00
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
wines
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
303.94
</td>
<td style="text-align:right;">
336.60
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
23.75
</td>
<td style="text-align:right;">
173.5
</td>
<td style="text-align:right;">
504.25
</td>
<td style="text-align:right;">
1493
</td>
<td style="text-align:left;">
▇▂▂▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fruits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
26.30
</td>
<td style="text-align:right;">
39.77
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
33.00
</td>
<td style="text-align:right;">
199
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
meat
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
166.95
</td>
<td style="text-align:right;">
225.72
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
16.00
</td>
<td style="text-align:right;">
67.0
</td>
<td style="text-align:right;">
232.00
</td>
<td style="text-align:right;">
1725
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fish
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
37.53
</td>
<td style="text-align:right;">
54.63
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
12.0
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
259
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
sweets
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
27.06
</td>
<td style="text-align:right;">
41.28
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
33.00
</td>
<td style="text-align:right;">
263
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
gold
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
44.02
</td>
<td style="text-align:right;">
52.17
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
24.0
</td>
<td style="text-align:right;">
56.00
</td>
<td style="text-align:right;">
362
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
deals
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.32
</td>
<td style="text-align:right;">
1.93
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
web
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.08
</td>
<td style="text-align:right;">
2.78
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.0
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
▇▃▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
catalog
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
store
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5.79
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
▂▇▂▃▂
</td>
</tr>
<tr>
<td style="text-align:left;">
visits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5.32
</td>
<td style="text-align:right;">
2.43
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
▅▇▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp1
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
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp2
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
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmplain
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
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
dt_customer_days
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3422.58
</td>
<td style="text-align:right;">
202.12
</td>
<td style="text-align:right;">
3069
</td>
<td style="text-align:right;">
3249.75
</td>
<td style="text-align:right;">
3424.5
</td>
<td style="text-align:right;">
3598.00
</td>
<td style="text-align:right;">
3768
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
53.19
</td>
<td style="text-align:right;">
11.98
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
45.00
</td>
<td style="text-align:right;">
52.0
</td>
<td style="text-align:right;">
63.00
</td>
<td style="text-align:right;">
129
</td>
<td style="text-align:left;">
▅▇▂▁▁
</td>
</tr>
</tbody>
</table>

``` r
set.seed(1)
# Save the split information for an 70/30 split of the data
bsplit <- initial_split(mktcamp_log, prop = 0.7)
train <- training(bsplit) 
test  <-  testing(bsplit)

mkt_log_recipe<-recipe(response ~., data=train)%>%
  step_impute_median(all_numeric())%>%
  step_dummy(all_nominal(),-all_outcomes())%>%
  prep()

mkt_log_recipe
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor         25
    ## 
    ## Training data contained 1568 data points and 17 incomplete rows. 
    ## 
    ## Operations:
    ## 
    ## Median imputation for income, kids, teens, recency, wines, fruits, me... [trained]
    ## Dummy variables from education, mar_stat [trained]

\#bake

``` r
bake_train<-bake(mkt_log_recipe,new_data = train)
bake_test<-bake(mkt_log_recipe,new_data = test)
```

\#model

``` r
logistic_glm <- logistic_reg(mode="classification")%>%
  set_engine("glm")%>%
  fit(response~.,data=bake_train)

tidy(logistic_glm)%>%
  mutate_at(c("estimate","std.error","statistic","p.value"),round,4)
```

    ## # A tibble: 35 × 5
    ##    term        estimate std.error statistic p.value
    ##    <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)  -5.29    535.       -0.0099  0.992 
    ##  2 income        0         0        -0.759   0.448 
    ##  3 kids          0.0801    0.266     0.302   0.763 
    ##  4 teens        -1.01      0.244    -4.16    0     
    ##  5 recency      -0.0291    0.0036   -8.19    0     
    ##  6 wines        -0.0003    0.0005   -0.680   0.496 
    ##  7 fruits        0         0.003    -0.0149  0.988 
    ##  8 meat          0.0013    0.0006    2.17    0.0297
    ##  9 fish         -0.0002    0.0023   -0.0977  0.922 
    ## 10 sweets        0.0004    0.0028    0.133   0.894 
    ## # … with 25 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

\#assessment

``` r
predict(logistic_glm,bake_train,type = "prob")%>%
  bind_cols(.,predict(logistic_glm,bake_train))%>%
  bind_cols(.,bake_train) -> scored_train_log

predict(logistic_glm,bake_test,type = "prob")%>%
  bind_cols(.,predict(logistic_glm,bake_test))%>%
  bind_cols(.,bake_test) -> scored_test_log

options(yardstick.event_first = FALSE)
 
scored_train_log %>%
  metrics(response, .pred_1,estimate = .pred_class)%>%
  mutate(part="training")%>%
  bind_rows(scored_test_log %>%
              metrics(response,.pred_1,estimate = .pred_class)%>%
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
    ## 1 accuracy    binary         0.898 training
    ## 2 kap         binary         0.508 training
    ## 3 mn_log_loss binary         0.246 training
    ## 4 roc_auc     binary         0.908 training
    ## 5 accuracy    binary         0.882 testing 
    ## 6 kap         binary         0.483 testing 
    ## 7 mn_log_loss binary         0.300 testing 
    ## 8 roc_auc     binary         0.890 testing

``` r
# ROC Curve 
scored_train_log %>%
  mutate(model="train")%>%
  bind_rows(scored_test_log %>%
            mutate(model="test"))%>%
  group_by(model)%>%
  roc_curve(response,.pred_1)%>%
  autoplot()
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# Variable Importandce 
logistic_glm %>%
  vip(10)+
  labs(title="logistic model 1 variable importance plot")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->

``` r
# Confusion Matrix
scored_train_log%>%
  conf_mat(response,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Train confusion matrix")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->

``` r
scored_test_log%>%
  conf_mat(response,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Test confusion matrix")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-28-4.png)<!-- -->

``` r
# precision,recall
scored_train_log %>%
  yardstick::precision(response, .pred_class)%>%
mutate(part="train")%>%
  bind_rows(scored_train_log %>%
    yardstick::recall(response, .pred_class)%>%
    mutate(part="train"))%>%
  bind_rows(scored_test_log %>%
  yardstick::precision(response, .pred_class)%>%
    mutate(part="test")%>%
  bind_rows(scored_test_log %>%
    yardstick::recall(response, .pred_class)%>%mutate(part="test")))
```

    ## # A tibble: 4 × 4
    ##   .metric   .estimator .estimate part 
    ##   <chr>     <chr>          <dbl> <chr>
    ## 1 precision binary         0.725 train
    ## 2 recall    binary         0.460 train
    ## 3 precision binary         0.770 test 
    ## 4 recall    binary         0.420 test

``` r
steplog <- glm(response~.,data = bake_train,family = binomial(link="logit"))
step<-stepAIC(steplog,direction = "both")
```

    ## Start:  AIC=840.9
    ## response ~ income + kids + teens + recency + wines + fruits + 
    ##     meat + fish + sweets + gold + deals + web + catalog + store + 
    ##     visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + cmplain + dt_customer_days + 
    ##     age + education_Basic + education_Graduation + education_Master + 
    ##     education_PhD + mar_stat_Alone + mar_stat_Divorce + mar_stat_Married + 
    ##     mar_stat_Partner + mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - fruits                1   770.90 838.90
    ## - fish                  1   770.91 838.91
    ## - sweets                1   770.92 838.92
    ## - age                   1   770.93 838.93
    ## - cmplain               1   770.93 838.93
    ## - mar_stat_Alone        1   770.97 838.97
    ## - kids                  1   771.00 839.00
    ## - mar_stat_Single       1   771.13 839.13
    ## - mar_stat_Widow        1   771.17 839.17
    ## - mar_stat_Divorce      1   771.17 839.17
    ## - mar_stat_YOLO         1   771.24 839.24
    ## - wines                 1   771.37 839.37
    ## - income                1   771.49 839.49
    ## - mar_stat_Partner      1   771.56 839.56
    ## - mar_stat_Married      1   771.60 839.60
    ## - education_Graduation  1   771.69 839.69
    ## - visits                1   772.07 840.07
    ## - deals                 1   772.14 840.14
    ## - catalog               1   772.41 840.41
    ## <none>                      770.90 840.90
    ## - education_Master      1   773.74 841.74
    ## - gold                  1   773.78 841.78
    ## - education_Basic       1   775.11 843.11
    ## - meat                  1   775.66 843.66
    ## - cmp2                  1   775.90 843.90
    ## - web                   1   776.38 844.38
    ## - store                 1   779.43 847.43
    ## - education_PhD         1   782.97 850.97
    ## - cmp4                  1   783.65 851.65
    ## - teens                 1   789.48 857.48
    ## - cmp1                  1   791.67 859.67
    ## - cmp5                  1   798.91 866.91
    ## - cmp3                  1   823.35 891.35
    ## - dt_customer_days      1   832.81 900.81
    ## - recency               1   848.35 916.35
    ## 
    ## Step:  AIC=838.9
    ## response ~ income + kids + teens + recency + wines + meat + fish + 
    ##     sweets + gold + deals + web + catalog + store + visits + 
    ##     cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + cmplain + dt_customer_days + 
    ##     age + education_Basic + education_Graduation + education_Master + 
    ##     education_PhD + mar_stat_Alone + mar_stat_Divorce + mar_stat_Married + 
    ##     mar_stat_Partner + mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - fish                  1   770.92 836.92
    ## - sweets                1   770.92 836.92
    ## - age                   1   770.93 836.93
    ## - cmplain               1   770.93 836.93
    ## - mar_stat_Alone        1   770.97 836.97
    ## - kids                  1   771.00 837.00
    ## - mar_stat_Single       1   771.13 837.13
    ## - mar_stat_Widow        1   771.17 837.17
    ## - mar_stat_Divorce      1   771.17 837.17
    ## - mar_stat_YOLO         1   771.24 837.24
    ## - wines                 1   771.37 837.37
    ## - income                1   771.49 837.49
    ## - mar_stat_Partner      1   771.56 837.56
    ## - mar_stat_Married      1   771.60 837.60
    ## - education_Graduation  1   771.69 837.69
    ## - visits                1   772.07 838.07
    ## - deals                 1   772.14 838.14
    ## - catalog               1   772.42 838.42
    ## <none>                      770.90 838.90
    ## - education_Master      1   773.74 839.74
    ## - gold                  1   773.82 839.82
    ## + fruits                1   770.90 840.90
    ## - education_Basic       1   775.12 841.12
    ## - meat                  1   775.69 841.69
    ## - cmp2                  1   775.93 841.93
    ## - web                   1   776.38 842.38
    ## - store                 1   779.47 845.47
    ## - education_PhD         1   782.97 848.97
    ## - cmp4                  1   783.71 849.71
    ## - teens                 1   789.65 855.65
    ## - cmp1                  1   791.67 857.67
    ## - cmp5                  1   798.93 864.93
    ## - cmp3                  1   823.37 889.37
    ## - dt_customer_days      1   832.91 898.91
    ## - recency               1   848.35 914.35
    ## 
    ## Step:  AIC=836.92
    ## response ~ income + kids + teens + recency + wines + meat + sweets + 
    ##     gold + deals + web + catalog + store + visits + cmp3 + cmp4 + 
    ##     cmp5 + cmp1 + cmp2 + cmplain + dt_customer_days + age + education_Basic + 
    ##     education_Graduation + education_Master + education_PhD + 
    ##     mar_stat_Alone + mar_stat_Divorce + mar_stat_Married + mar_stat_Partner + 
    ##     mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - sweets                1   770.93 834.93
    ## - age                   1   770.94 834.94
    ## - cmplain               1   770.94 834.94
    ## - mar_stat_Alone        1   770.98 834.98
    ## - kids                  1   771.01 835.01
    ## - mar_stat_Single       1   771.14 835.14
    ## - mar_stat_Widow        1   771.18 835.18
    ## - mar_stat_Divorce      1   771.18 835.18
    ## - mar_stat_YOLO         1   771.25 835.25
    ## - wines                 1   771.38 835.38
    ## - income                1   771.51 835.51
    ## - mar_stat_Partner      1   771.57 835.57
    ## - mar_stat_Married      1   771.61 835.61
    ## - education_Graduation  1   771.71 835.71
    ## - visits                1   772.10 836.10
    ## - deals                 1   772.18 836.18
    ## - catalog               1   772.42 836.42
    ## <none>                      770.92 836.92
    ## - education_Master      1   773.79 837.79
    ## - gold                  1   773.94 837.94
    ## + fish                  1   770.90 838.90
    ## + fruits                1   770.91 838.91
    ## - education_Basic       1   775.12 839.12
    ## - meat                  1   775.72 839.72
    ## - cmp2                  1   775.95 839.95
    ## - web                   1   776.42 840.42
    ## - store                 1   779.66 843.66
    ## - education_PhD         1   783.09 847.09
    ## - cmp4                  1   783.80 847.80
    ## - teens                 1   789.68 853.68
    ## - cmp1                  1   792.19 856.19
    ## - cmp5                  1   799.69 863.69
    ## - cmp3                  1   823.47 887.47
    ## - dt_customer_days      1   833.47 897.47
    ## - recency               1   848.41 912.41
    ## 
    ## Step:  AIC=834.93
    ## response ~ income + kids + teens + recency + wines + meat + gold + 
    ##     deals + web + catalog + store + visits + cmp3 + cmp4 + cmp5 + 
    ##     cmp1 + cmp2 + cmplain + dt_customer_days + age + education_Basic + 
    ##     education_Graduation + education_Master + education_PhD + 
    ##     mar_stat_Alone + mar_stat_Divorce + mar_stat_Married + mar_stat_Partner + 
    ##     mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - age                   1   770.95 832.95
    ## - cmplain               1   770.95 832.95
    ## - mar_stat_Alone        1   770.99 832.99
    ## - kids                  1   771.02 833.02
    ## - mar_stat_Single       1   771.15 833.15
    ## - mar_stat_Widow        1   771.19 833.19
    ## - mar_stat_Divorce      1   771.19 833.19
    ## - mar_stat_YOLO         1   771.26 833.26
    ## - wines                 1   771.40 833.40
    ## - income                1   771.51 833.51
    ## - mar_stat_Partner      1   771.57 833.57
    ## - mar_stat_Married      1   771.61 833.61
    ## - education_Graduation  1   771.72 833.72
    ## - visits                1   772.10 834.10
    ## - deals                 1   772.19 834.19
    ## - catalog               1   772.42 834.42
    ## <none>                      770.93 834.93
    ## - education_Master      1   773.79 835.79
    ## - gold                  1   773.97 835.97
    ## + sweets                1   770.92 836.92
    ## + fish                  1   770.92 836.92
    ## + fruits                1   770.93 836.93
    ## - education_Basic       1   775.14 837.14
    ## - meat                  1   775.80 837.80
    ## - cmp2                  1   775.96 837.96
    ## - web                   1   776.60 838.60
    ## - store                 1   779.68 841.68
    ## - education_PhD         1   783.17 845.17
    ## - cmp4                  1   783.83 845.83
    ## - teens                 1   790.19 852.19
    ## - cmp1                  1   792.73 854.73
    ## - cmp5                  1   799.72 861.72
    ## - cmp3                  1   823.71 885.71
    ## - dt_customer_days      1   834.10 896.10
    ## - recency               1   848.54 910.54
    ## 
    ## Step:  AIC=832.95
    ## response ~ income + kids + teens + recency + wines + meat + gold + 
    ##     deals + web + catalog + store + visits + cmp3 + cmp4 + cmp5 + 
    ##     cmp1 + cmp2 + cmplain + dt_customer_days + education_Basic + 
    ##     education_Graduation + education_Master + education_PhD + 
    ##     mar_stat_Alone + mar_stat_Divorce + mar_stat_Married + mar_stat_Partner + 
    ##     mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - cmplain               1   770.98 830.98
    ## - mar_stat_Alone        1   771.01 831.01
    ## - kids                  1   771.03 831.03
    ## - mar_stat_Single       1   771.17 831.17
    ## - mar_stat_Widow        1   771.20 831.20
    ## - mar_stat_Divorce      1   771.21 831.21
    ## - mar_stat_YOLO         1   771.28 831.28
    ## - wines                 1   771.41 831.41
    ## - income                1   771.53 831.53
    ## - mar_stat_Partner      1   771.59 831.59
    ## - mar_stat_Married      1   771.63 831.63
    ## - education_Graduation  1   771.76 831.76
    ## - visits                1   772.12 832.12
    ## - deals                 1   772.20 832.20
    ## - catalog               1   772.46 832.46
    ## <none>                      770.95 832.95
    ## - education_Master      1   773.89 833.89
    ## - gold                  1   773.99 833.99
    ## + age                   1   770.93 834.93
    ## + sweets                1   770.94 834.94
    ## + fish                  1   770.95 834.95
    ## + fruits                1   770.95 834.95
    ## - education_Basic       1   775.18 835.18
    ## - meat                  1   775.80 835.80
    ## - cmp2                  1   776.00 836.00
    ## - web                   1   776.71 836.71
    ## - store                 1   779.75 839.75
    ## - education_PhD         1   783.40 843.40
    ## - cmp4                  1   783.84 843.84
    ## - teens                 1   791.01 851.01
    ## - cmp1                  1   792.74 852.74
    ## - cmp5                  1   799.74 859.74
    ## - cmp3                  1   823.73 883.73
    ## - dt_customer_days      1   834.10 894.10
    ## - recency               1   848.55 908.55
    ## 
    ## Step:  AIC=830.98
    ## response ~ income + kids + teens + recency + wines + meat + gold + 
    ##     deals + web + catalog + store + visits + cmp3 + cmp4 + cmp5 + 
    ##     cmp1 + cmp2 + dt_customer_days + education_Basic + education_Graduation + 
    ##     education_Master + education_PhD + mar_stat_Alone + mar_stat_Divorce + 
    ##     mar_stat_Married + mar_stat_Partner + mar_stat_Single + mar_stat_Widow + 
    ##     mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - mar_stat_Alone        1   771.04 829.04
    ## - kids                  1   771.06 829.06
    ## - mar_stat_Single       1   771.20 829.20
    ## - mar_stat_Widow        1   771.23 829.23
    ## - mar_stat_Divorce      1   771.24 829.24
    ## - mar_stat_YOLO         1   771.31 829.31
    ## - wines                 1   771.44 829.44
    ## - income                1   771.56 829.56
    ## - mar_stat_Partner      1   771.61 829.61
    ## - mar_stat_Married      1   771.65 829.65
    ## - education_Graduation  1   771.81 829.81
    ## - visits                1   772.16 830.16
    ## - deals                 1   772.23 830.23
    ## - catalog               1   772.49 830.49
    ## <none>                      770.98 830.98
    ## - education_Master      1   773.95 831.95
    ## - gold                  1   774.02 832.02
    ## + cmplain               1   770.95 832.95
    ## + age                   1   770.95 832.95
    ## + sweets                1   770.97 832.97
    ## + fish                  1   770.98 832.98
    ## + fruits                1   770.98 832.98
    ## - education_Basic       1   775.19 833.19
    ## - meat                  1   775.84 833.84
    ## - cmp2                  1   776.02 834.02
    ## - web                   1   776.73 834.73
    ## - store                 1   779.78 837.78
    ## - education_PhD         1   783.49 841.49
    ## - cmp4                  1   783.87 841.87
    ## - teens                 1   791.02 849.02
    ## - cmp1                  1   792.76 850.76
    ## - cmp5                  1   799.77 857.77
    ## - cmp3                  1   823.77 881.77
    ## - dt_customer_days      1   834.14 892.14
    ## - recency               1   848.55 906.55
    ## 
    ## Step:  AIC=829.04
    ## response ~ income + kids + teens + recency + wines + meat + gold + 
    ##     deals + web + catalog + store + visits + cmp3 + cmp4 + cmp5 + 
    ##     cmp1 + cmp2 + dt_customer_days + education_Basic + education_Graduation + 
    ##     education_Master + education_PhD + mar_stat_Divorce + mar_stat_Married + 
    ##     mar_stat_Partner + mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - kids                  1   771.11 827.11
    ## - wines                 1   771.50 827.50
    ## - income                1   771.62 827.62
    ## - education_Graduation  1   771.87 827.87
    ## - mar_stat_YOLO         1   772.01 828.01
    ## - mar_stat_Single       1   772.03 828.03
    ## - mar_stat_Widow        1   772.14 828.14
    ## - visits                1   772.21 828.21
    ## - mar_stat_Divorce      1   772.26 828.26
    ## - deals                 1   772.29 828.29
    ## - catalog               1   772.57 828.57
    ## <none>                      771.04 829.04
    ## - education_Master      1   774.01 830.01
    ## - gold                  1   774.12 830.12
    ## - mar_stat_Partner      1   774.17 830.17
    ## - mar_stat_Married      1   774.36 830.36
    ## + mar_stat_Alone        1   770.98 830.98
    ## + cmplain               1   771.01 831.01
    ## + age                   1   771.01 831.01
    ## + sweets                1   771.03 831.03
    ## + fish                  1   771.04 831.04
    ## + fruits                1   771.04 831.04
    ## - education_Basic       1   775.25 831.25
    ## - meat                  1   775.86 831.86
    ## - cmp2                  1   776.07 832.07
    ## - web                   1   776.77 832.77
    ## - store                 1   779.83 835.83
    ## - education_PhD         1   783.57 839.57
    ## - cmp4                  1   783.91 839.91
    ## - teens                 1   791.14 847.14
    ## - cmp1                  1   792.94 848.94
    ## - cmp5                  1   799.95 855.95
    ## - cmp3                  1   823.79 879.79
    ## - dt_customer_days      1   834.44 890.44
    ## - recency               1   848.60 904.60
    ## 
    ## Step:  AIC=827.11
    ## response ~ income + teens + recency + wines + meat + gold + deals + 
    ##     web + catalog + store + visits + cmp3 + cmp4 + cmp5 + cmp1 + 
    ##     cmp2 + dt_customer_days + education_Basic + education_Graduation + 
    ##     education_Master + education_PhD + mar_stat_Divorce + mar_stat_Married + 
    ##     mar_stat_Partner + mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - income                1   771.67 825.67
    ## - wines                 1   771.68 825.68
    ## - education_Graduation  1   772.00 826.00
    ## - mar_stat_Single       1   772.14 826.14
    ## - mar_stat_YOLO         1   772.16 826.16
    ## - mar_stat_Widow        1   772.26 826.26
    ## - visits                1   772.36 826.36
    ## - mar_stat_Divorce      1   772.37 826.37
    ## - catalog               1   772.58 826.58
    ## - deals                 1   772.71 826.71
    ## <none>                      771.11 827.11
    ## - gold                  1   774.12 828.12
    ## - education_Master      1   774.23 828.23
    ## - mar_stat_Partner      1   774.29 828.29
    ## - mar_stat_Married      1   774.46 828.46
    ## + kids                  1   771.04 829.04
    ## + mar_stat_Alone        1   771.06 829.06
    ## + cmplain               1   771.09 829.09
    ## + age                   1   771.10 829.10
    ## + sweets                1   771.10 829.10
    ## + fish                  1   771.11 829.11
    ## + fruits                1   771.11 829.11
    ## - education_Basic       1   775.28 829.28
    ## - meat                  1   775.87 829.87
    ## - cmp2                  1   776.13 830.13
    ## - web                   1   776.78 830.78
    ## - store                 1   780.25 834.25
    ## - cmp4                  1   783.92 837.92
    ## - education_PhD         1   784.00 838.00
    ## - teens                 1   792.27 846.27
    ## - cmp1                  1   793.00 847.00
    ## - cmp5                  1   800.53 854.53
    ## - cmp3                  1   824.18 878.18
    ## - dt_customer_days      1   834.44 888.44
    ## - recency               1   848.61 902.61
    ## 
    ## Step:  AIC=825.67
    ## response ~ teens + recency + wines + meat + gold + deals + web + 
    ##     catalog + store + visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + 
    ##     dt_customer_days + education_Basic + education_Graduation + 
    ##     education_Master + education_PhD + mar_stat_Divorce + mar_stat_Married + 
    ##     mar_stat_Partner + mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - wines                 1   772.52 824.52
    ## - education_Graduation  1   772.55 824.55
    ## - mar_stat_Single       1   772.66 824.66
    ## - mar_stat_YOLO         1   772.71 824.71
    ## - mar_stat_Widow        1   772.77 824.77
    ## - catalog               1   772.82 824.82
    ## - mar_stat_Divorce      1   772.90 824.90
    ## - deals                 1   773.59 825.59
    ## <none>                      771.67 825.67
    ## - visits                1   773.80 825.80
    ## - gold                  1   774.76 826.76
    ## - education_Master      1   774.79 826.79
    ## - mar_stat_Partner      1   774.80 826.80
    ## - mar_stat_Married      1   774.97 826.97
    ## + income                1   771.11 827.11
    ## - education_Basic       1   775.50 827.50
    ## + mar_stat_Alone        1   771.61 827.61
    ## + kids                  1   771.62 827.62
    ## + cmplain               1   771.65 827.65
    ## + fish                  1   771.65 827.65
    ## + fruits                1   771.65 827.65
    ## + age                   1   771.65 827.65
    ## + sweets                1   771.67 827.67
    ## - meat                  1   775.89 827.89
    ## - web                   1   776.80 828.80
    ## - cmp2                  1   776.82 828.82
    ## - store                 1   781.80 833.80
    ## - cmp4                  1   784.20 836.20
    ## - education_PhD         1   784.36 836.36
    ## - cmp1                  1   793.24 845.24
    ## - teens                 1   795.31 847.31
    ## - cmp5                  1   800.53 852.53
    ## - cmp3                  1   825.95 877.95
    ## - dt_customer_days      1   835.60 887.60
    ## - recency               1   848.90 900.90
    ## 
    ## Step:  AIC=824.52
    ## response ~ teens + recency + meat + gold + deals + web + catalog + 
    ##     store + visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Graduation + education_Master + 
    ##     education_PhD + mar_stat_Divorce + mar_stat_Married + mar_stat_Partner + 
    ##     mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - catalog               1   773.36 823.36
    ## - education_Graduation  1   773.37 823.37
    ## - mar_stat_YOLO         1   773.54 823.54
    ## - mar_stat_Single       1   773.54 823.54
    ## - mar_stat_Widow        1   773.64 823.64
    ## - mar_stat_Divorce      1   773.80 823.80
    ## - visits                1   774.46 824.46
    ## <none>                      772.52 824.52
    ## - deals                 1   774.82 824.82
    ## - education_Master      1   775.26 825.26
    ## - gold                  1   775.39 825.39
    ## + wines                 1   771.67 825.67
    ## + income                1   771.68 825.68
    ## - mar_stat_Partner      1   775.71 825.71
    ## - mar_stat_Married      1   775.90 825.90
    ## - meat                  1   776.23 826.23
    ## - education_Basic       1   776.36 826.36
    ## + kids                  1   772.37 826.37
    ## + mar_stat_Alone        1   772.46 826.46
    ## + fruits                1   772.50 826.50
    ## + fish                  1   772.51 826.51
    ## + cmplain               1   772.51 826.51
    ## + age                   1   772.52 826.52
    ## + sweets                1   772.52 826.52
    ## - web                   1   776.98 826.98
    ## - cmp2                  1   777.38 827.38
    ## - cmp4                  1   784.21 834.21
    ## - education_PhD         1   784.42 834.42
    ## - store                 1   788.10 838.10
    ## - cmp1                  1   793.33 843.33
    ## - teens                 1   797.68 847.68
    ## - cmp5                  1   801.45 851.45
    ## - cmp3                  1   826.05 876.05
    ## - dt_customer_days      1   835.62 885.62
    ## - recency               1   849.73 899.73
    ## 
    ## Step:  AIC=823.36
    ## response ~ teens + recency + meat + gold + deals + web + store + 
    ##     visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Graduation + education_Master + 
    ##     education_PhD + mar_stat_Divorce + mar_stat_Married + mar_stat_Partner + 
    ##     mar_stat_Single + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - education_Graduation  1   774.13 822.13
    ## - mar_stat_Single       1   774.37 822.37
    ## - mar_stat_YOLO         1   774.41 822.41
    ## - mar_stat_Widow        1   774.45 822.45
    ## - mar_stat_Divorce      1   774.63 822.63
    ## - visits                1   774.80 822.80
    ## <none>                      773.36 823.36
    ## - education_Master      1   775.92 823.92
    ## - deals                 1   776.20 824.20
    ## + catalog               1   772.52 824.52
    ## - mar_stat_Partner      1   776.58 824.58
    ## - gold                  1   776.67 824.67
    ## - mar_stat_Married      1   776.77 824.77
    ## + wines                 1   772.82 824.82
    ## + income                1   772.92 824.92
    ## + mar_stat_Alone        1   773.28 825.28
    ## + kids                  1   773.31 825.31
    ## + fruits                1   773.34 825.34
    ## + cmplain               1   773.34 825.34
    ## + age                   1   773.35 825.35
    ## - education_Basic       1   777.35 825.35
    ## + fish                  1   773.35 825.35
    ## + sweets                1   773.36 825.36
    ## - web                   1   778.25 826.25
    ## - cmp2                  1   778.62 826.62
    ## - meat                  1   780.86 828.86
    ## - education_PhD         1   785.20 833.20
    ## - cmp4                  1   785.32 833.32
    ## - store                 1   788.95 836.95
    ## - cmp1                  1   795.49 843.49
    ## - teens                 1   798.30 846.30
    ## - cmp5                  1   802.03 850.03
    ## - cmp3                  1   830.12 878.12
    ## - dt_customer_days      1   838.44 886.44
    ## - recency               1   850.29 898.29
    ## 
    ## Step:  AIC=822.13
    ## response ~ teens + recency + meat + gold + deals + web + store + 
    ##     visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Master + education_PhD + mar_stat_Divorce + 
    ##     mar_stat_Married + mar_stat_Partner + mar_stat_Single + mar_stat_Widow + 
    ##     mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - mar_stat_Single       1   775.13 821.13
    ## - mar_stat_YOLO         1   775.18 821.18
    ## - mar_stat_Widow        1   775.24 821.24
    ## - mar_stat_Divorce      1   775.42 821.42
    ## - visits                1   775.57 821.57
    ## - education_Master      1   776.11 822.11
    ## <none>                      774.13 822.13
    ## - deals                 1   777.11 823.11
    ## + education_Graduation  1   773.36 823.36
    ## + catalog               1   773.37 823.37
    ## - mar_stat_Partner      1   777.37 823.37
    ## - gold                  1   777.57 823.57
    ## - mar_stat_Married      1   777.57 823.57
    ## + wines                 1   773.61 823.61
    ## + income                1   773.68 823.68
    ## + kids                  1   774.04 824.04
    ## + mar_stat_Alone        1   774.05 824.05
    ## + cmplain               1   774.09 824.09
    ## + fruits                1   774.10 824.10
    ## + age                   1   774.11 824.11
    ## + fish                  1   774.12 824.12
    ## + sweets                1   774.12 824.12
    ## - web                   1   779.01 825.01
    ## - cmp2                  1   779.35 825.35
    ## - education_Basic       1   780.79 826.79
    ## - meat                  1   781.96 827.96
    ## - cmp4                  1   786.23 832.23
    ## - store                 1   790.03 836.03
    ## - education_PhD         1   793.24 839.24
    ## - cmp1                  1   796.09 842.09
    ## - teens                 1   798.65 844.65
    ## - cmp5                  1   803.37 849.37
    ## - cmp3                  1   830.81 876.81
    ## - dt_customer_days      1   838.95 884.95
    ## - recency               1   850.70 896.70
    ## 
    ## Step:  AIC=821.13
    ## response ~ teens + recency + meat + gold + deals + web + store + 
    ##     visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Master + education_PhD + mar_stat_Divorce + 
    ##     mar_stat_Married + mar_stat_Partner + mar_stat_Widow + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - mar_stat_Widow        1   775.24 819.24
    ## - mar_stat_YOLO         1   775.35 819.35
    ## - mar_stat_Divorce      1   775.68 819.68
    ## - visits                1   776.51 820.51
    ## <none>                      775.13 821.13
    ## - education_Master      1   777.30 821.30
    ## + mar_stat_Single       1   774.13 822.13
    ## - deals                 1   778.18 822.18
    ## + mar_stat_Alone        1   774.35 822.35
    ## + education_Graduation  1   774.37 822.37
    ## + catalog               1   774.37 822.37
    ## + wines                 1   774.59 822.59
    ## - gold                  1   778.66 822.66
    ## + income                1   774.70 822.70
    ## + kids                  1   775.00 823.00
    ## + cmplain               1   775.09 823.09
    ## + fruits                1   775.11 823.11
    ## + age                   1   775.12 823.12
    ## + sweets                1   775.12 823.12
    ## + fish                  1   775.12 823.12
    ## - web                   1   780.09 824.09
    ## - cmp2                  1   780.34 824.34
    ## - education_Basic       1   781.77 825.77
    ## - meat                  1   782.79 826.79
    ## - cmp4                  1   787.08 831.08
    ## - store                 1   791.33 835.33
    ## - education_PhD         1   794.62 838.62
    ## - mar_stat_Partner      1   795.92 839.92
    ## - cmp1                  1   797.43 841.43
    ## - teens                 1   799.24 843.24
    ## - mar_stat_Married      1   804.56 848.56
    ## - cmp5                  1   804.75 848.75
    ## - cmp3                  1   832.48 876.48
    ## - dt_customer_days      1   839.95 883.95
    ## - recency               1   851.66 895.66
    ## 
    ## Step:  AIC=819.24
    ## response ~ teens + recency + meat + gold + deals + web + store + 
    ##     visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Master + education_PhD + mar_stat_Divorce + 
    ##     mar_stat_Married + mar_stat_Partner + mar_stat_YOLO
    ## 
    ##                        Df Deviance    AIC
    ## - mar_stat_YOLO         1   775.45 817.45
    ## - mar_stat_Divorce      1   775.71 817.71
    ## - visits                1   776.66 818.66
    ## <none>                      775.24 819.24
    ## - education_Master      1   777.43 819.43
    ## - deals                 1   778.26 820.26
    ## + mar_stat_Alone        1   774.43 820.43
    ## + education_Graduation  1   774.46 820.46
    ## + catalog               1   774.50 820.50
    ## + wines                 1   774.70 820.70
    ## - gold                  1   778.77 820.77
    ## + income                1   774.82 820.82
    ## + kids                  1   775.09 821.09
    ## + mar_stat_Widow        1   775.13 821.13
    ## + cmplain               1   775.20 821.20
    ## + fruits                1   775.21 821.21
    ## + fish                  1   775.23 821.23
    ## + sweets                1   775.23 821.23
    ## + age                   1   775.24 821.24
    ## + mar_stat_Single       1   775.24 821.24
    ## - web                   1   780.19 822.19
    ## - cmp2                  1   780.54 822.54
    ## - education_Basic       1   781.87 823.87
    ## - meat                  1   783.04 825.04
    ## - cmp4                  1   787.08 829.08
    ## - store                 1   791.54 833.54
    ## - education_PhD         1   794.64 836.64
    ## - mar_stat_Partner      1   796.51 838.51
    ## - cmp1                  1   797.81 839.81
    ## - teens                 1   799.79 841.79
    ## - cmp5                  1   804.82 846.82
    ## - mar_stat_Married      1   805.95 847.95
    ## - cmp3                  1   832.57 874.57
    ## - dt_customer_days      1   839.98 881.98
    ## - recency               1   851.68 893.68
    ## 
    ## Step:  AIC=817.45
    ## response ~ teens + recency + meat + gold + deals + web + store + 
    ##     visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Master + education_PhD + mar_stat_Divorce + 
    ##     mar_stat_Married + mar_stat_Partner
    ## 
    ##                        Df Deviance    AIC
    ## - mar_stat_Divorce      1   775.88 815.88
    ## - visits                1   776.86 816.86
    ## <none>                      775.45 817.45
    ## - education_Master      1   777.62 817.62
    ## - deals                 1   778.42 818.42
    ## + mar_stat_Alone        1   774.62 818.62
    ## + education_Graduation  1   774.66 818.66
    ## + catalog               1   774.68 818.68
    ## + wines                 1   774.93 818.93
    ## - gold                  1   778.97 818.97
    ## + income                1   775.03 819.03
    ## + mar_stat_YOLO         1   775.24 819.24
    ## + kids                  1   775.27 819.27
    ## + mar_stat_Widow        1   775.35 819.35
    ## + cmplain               1   775.41 819.41
    ## + fruits                1   775.42 819.42
    ## + sweets                1   775.43 819.43
    ## + fish                  1   775.44 819.44
    ## + mar_stat_Single       1   775.44 819.44
    ## + age                   1   775.44 819.44
    ## - web                   1   780.36 820.36
    ## - cmp2                  1   780.75 820.75
    ## - education_Basic       1   782.08 822.08
    ## - meat                  1   783.29 823.29
    ## - cmp4                  1   787.35 827.35
    ## - store                 1   791.73 831.73
    ## - education_PhD         1   794.64 834.64
    ## - mar_stat_Partner      1   796.53 836.53
    ## - cmp1                  1   797.96 837.96
    ## - teens                 1   800.13 840.13
    ## - cmp5                  1   804.91 844.91
    ## - mar_stat_Married      1   805.96 845.96
    ## - cmp3                  1   832.81 872.81
    ## - dt_customer_days      1   839.98 879.98
    ## - recency               1   851.83 891.83
    ## 
    ## Step:  AIC=815.88
    ## response ~ teens + recency + meat + gold + deals + web + store + 
    ##     visits + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Master + education_PhD + mar_stat_Married + 
    ##     mar_stat_Partner
    ## 
    ##                        Df Deviance    AIC
    ## - visits                1   777.35 815.35
    ## <none>                      775.88 815.88
    ## - education_Master      1   777.99 815.99
    ## - deals                 1   778.87 816.87
    ## + mar_stat_Alone        1   774.98 816.98
    ## + education_Graduation  1   775.01 817.01
    ## + catalog               1   775.11 817.11
    ## + wines                 1   775.32 817.32
    ## - gold                  1   779.39 817.39
    ## + mar_stat_Divorce      1   775.45 817.45
    ## + income                1   775.45 817.45
    ## + mar_stat_Single       1   775.55 817.55
    ## + kids                  1   775.70 817.70
    ## + mar_stat_YOLO         1   775.71 817.71
    ## + fruits                1   775.84 817.84
    ## + cmplain               1   775.85 817.85
    ## + mar_stat_Widow        1   775.85 817.85
    ## + fish                  1   775.86 817.86
    ## + sweets                1   775.86 817.86
    ## + age                   1   775.88 817.88
    ## - web                   1   780.91 818.91
    ## - cmp2                  1   781.05 819.05
    ## - education_Basic       1   782.53 820.53
    ## - meat                  1   783.84 821.84
    ## - cmp4                  1   787.95 825.95
    ## - store                 1   792.15 830.15
    ## - education_PhD         1   794.73 832.73
    ## - mar_stat_Partner      1   797.43 835.43
    ## - cmp1                  1   798.31 836.31
    ## - teens                 1   801.53 839.53
    ## - cmp5                  1   805.17 843.17
    ## - mar_stat_Married      1   808.47 846.47
    ## - cmp3                  1   832.98 870.98
    ## - dt_customer_days      1   840.31 878.31
    ## - recency               1   852.50 890.50
    ## 
    ## Step:  AIC=815.35
    ## response ~ teens + recency + meat + gold + deals + web + store + 
    ##     cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + education_Basic + 
    ##     education_Master + education_PhD + mar_stat_Married + mar_stat_Partner
    ## 
    ##                        Df Deviance    AIC
    ## <none>                      777.35 815.35
    ## - education_Master      1   779.50 815.50
    ## + visits                1   775.88 815.88
    ## + income                1   776.23 816.23
    ## - gold                  1   780.40 816.40
    ## + mar_stat_Alone        1   776.46 816.46
    ## + education_Graduation  1   776.48 816.48
    ## + wines                 1   776.85 816.85
    ## + mar_stat_Divorce      1   776.86 816.86
    ## + mar_stat_Single       1   776.93 816.93
    ## + kids                  1   777.02 817.02
    ## + catalog               1   777.06 817.06
    ## + mar_stat_YOLO         1   777.19 817.19
    ## + fruits                1   777.24 817.24
    ## + sweets                1   777.24 817.24
    ## + fish                  1   777.25 817.25
    ## + mar_stat_Widow        1   777.31 817.31
    ## + cmplain               1   777.31 817.31
    ## + age                   1   777.34 817.34
    ## - deals                 1   782.47 818.47
    ## - web                   1   782.81 818.81
    ## - cmp2                  1   782.88 818.88
    ## - meat                  1   783.87 819.87
    ## - education_Basic       1   784.13 820.13
    ## - cmp4                  1   790.12 826.12
    ## - education_PhD         1   795.99 831.99
    ## - store                 1   797.41 833.41
    ## - mar_stat_Partner      1   798.93 834.93
    ## - cmp1                  1   799.85 835.85
    ## - teens                 1   804.48 840.48
    ## - cmp5                  1   805.35 841.35
    ## - mar_stat_Married      1   810.46 846.46
    ## - cmp3                  1   835.76 871.76
    ## - recency               1   854.92 890.92
    ## - dt_customer_days      1   860.14 896.14

``` r
summary(step)
```

    ## 
    ## Call:
    ## glm(formula = response ~ teens + recency + meat + gold + deals + 
    ##     web + store + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    ##     education_Basic + education_Master + education_PhD + mar_stat_Married + 
    ##     mar_stat_Partner, family = binomial(link = "logit"), data = bake_train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.53755  -0.40708  -0.21785  -0.09843   3.04933  
    ## 
    ## Coefficients:
    ##                     Estimate  Std. Error z value             Pr(>|z|)    
    ## (Intercept)      -16.6431315   1.8956267  -8.780 < 0.0000000000000002 ***
    ## teens             -1.1011876   0.2198360  -5.009 0.000000546763216315 ***
    ## recency           -0.0287727   0.0035113  -8.194 0.000000000000000252 ***
    ## meat               0.0010878   0.0004229   2.572             0.010102 *  
    ## gold               0.0031851   0.0018009   1.769             0.076953 .  
    ## deals              0.1123711   0.0494794   2.271             0.023143 *  
    ## web                0.0833321   0.0352960   2.361             0.018228 *  
    ## store             -0.1577105   0.0366633  -4.302 0.000016957701751546 ***
    ## cmp3               2.1696253   0.2835142   7.653 0.000000000000019693 ***
    ## cmp4               1.2766050   0.3474479   3.674             0.000239 ***
    ## cmp5               1.7183103   0.3271434   5.252 0.000000150074839524 ***
    ## cmp1               1.5997053   0.3369468   4.748 0.000002057944673085 ***
    ## cmp2               1.5882384   0.6780788   2.342             0.019167 *  
    ## dt_customer_days   0.0046105   0.0005520   8.353 < 0.0000000000000002 ***
    ## education_Basic   -2.1410453   1.0699433  -2.001             0.045383 *  
    ## education_Master   0.3977383   0.2671988   1.489             0.136606    
    ## education_PhD      0.9911076   0.2285549   4.336 0.000014482943436085 ***
    ## mar_stat_Married  -1.2193380   0.2198325  -5.547 0.000000029116556337 ***
    ## mar_stat_Partner  -1.1373657   0.2565111  -4.434 0.000009250785978458 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1286.12  on 1567  degrees of freedom
    ## Residual deviance:  777.35  on 1549  degrees of freedom
    ## AIC: 815.35
    ## 
    ## Number of Fisher Scoring iterations: 6

# reduce based on step

``` r
tidy(step)%>%
    mutate_at(c("estimate","std.error","statistic","p.value"),round,4)
```

    ## # A tibble: 19 × 5
    ##    term             estimate std.error statistic p.value
    ##    <chr>               <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)      -16.6       1.90       -8.78  0     
    ##  2 teens             -1.10      0.220      -5.01  0     
    ##  3 recency           -0.0288    0.0035     -8.19  0     
    ##  4 meat               0.0011    0.0004      2.57  0.0101
    ##  5 gold               0.0032    0.0018      1.77  0.077 
    ##  6 deals              0.112     0.0495      2.27  0.0231
    ##  7 web                0.0833    0.0353      2.36  0.0182
    ##  8 store             -0.158     0.0367     -4.30  0     
    ##  9 cmp3               2.17      0.284       7.65  0     
    ## 10 cmp4               1.28      0.347       3.67  0.0002
    ## 11 cmp5               1.72      0.327       5.25  0     
    ## 12 cmp1               1.60      0.337       4.75  0     
    ## 13 cmp2               1.59      0.678       2.34  0.0192
    ## 14 dt_customer_days   0.0046    0.0006      8.35  0     
    ## 15 education_Basic   -2.14      1.07       -2.00  0.0454
    ## 16 education_Master   0.398     0.267       1.49  0.137 
    ## 17 education_PhD      0.991     0.229       4.34  0     
    ## 18 mar_stat_Married  -1.22      0.220      -5.55  0     
    ## 19 mar_stat_Partner  -1.14      0.256      -4.43  0

## This is my final model

# All models are not overfit ( no gaps between training results and testing results), Random forest have the lowest accuracy and AUC. Model 1 have 34 variables while model 2 only have 16. Model 1 have many large p-value variables which means insignificant, while model 2 do not have insignificant variables. Thus model 2 is the best final model.

``` r
# remove education_Master,gold due to high p-value (>0.05)
reduce_recipe<-recipe(response ~ teens + recency + meat  + deals + 
    web + store + cmp3 + cmp4 + cmp5 + cmp1 + cmp2 + dt_customer_days + 
    education_Basic  + education_PhD + mar_stat_Married + 
    mar_stat_Partner, data=bake_train)%>%
  step_impute_median(all_numeric())%>%
  step_dummy(all_nominal(),-all_outcomes())%>%
  prep()

bake_train_step<-bake(reduce_recipe,new_data = bake_train)
bake_test_step<-bake(reduce_recipe,new_data = bake_test)



logistic_step <- logistic_reg(mode="classification")%>%
  set_engine("glm")%>%
  fit(response~.,data=bake_train_step)

tidy(logistic_step)%>%
  mutate_at(c("estimate","std.error","statistic","p.value"),round,4)
```

    ## # A tibble: 17 × 5
    ##    term             estimate std.error statistic p.value
    ##    <chr>               <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)      -16.5       1.88       -8.79  0     
    ##  2 teens             -1.08      0.218      -4.94  0     
    ##  3 recency           -0.0286    0.0035     -8.18  0     
    ##  4 meat               0.0012    0.0004      2.77  0.0057
    ##  5 deals              0.107     0.0497      2.15  0.0317
    ##  6 web                0.0967    0.0351      2.75  0.0059
    ##  7 store             -0.147     0.0366     -4.02  0.0001
    ##  8 cmp3               2.19      0.283       7.73  0     
    ##  9 cmp4               1.26      0.345       3.66  0.0003
    ## 10 cmp5               1.76      0.325       5.42  0     
    ## 11 cmp1               1.57      0.334       4.69  0     
    ## 12 cmp2               1.57      0.667       2.35  0.0185
    ## 13 dt_customer_days   0.0046    0.0005      8.40  0     
    ## 14 education_Basic   -2.23      1.07       -2.09  0.0368
    ## 15 education_PhD      0.838     0.214       3.91  0.0001
    ## 16 mar_stat_Married  -1.21      0.219      -5.55  0     
    ## 17 mar_stat_Partner  -1.12      0.255      -4.39  0

``` r
predict(logistic_step,bake_train_step,type = "prob")%>%
  bind_cols(.,predict(logistic_step,bake_train_step))%>%
  bind_cols(.,bake_train_step) -> scored_train_logre

predict(logistic_step,bake_test_step,type = "prob")%>%
  bind_cols(.,predict(logistic_step,bake_test_step))%>%
  bind_cols(.,bake_test_step) -> scored_test_logre

options(yardstick.event_first = FALSE)
 
scored_train_logre %>%
  metrics(response, .pred_1,estimate = .pred_class)%>%
  mutate(part="training")%>%
  bind_rows(scored_test_logre %>%
              metrics(response,.pred_1,estimate = .pred_class)%>%
            mutate(part="testing"))
```

    ## # A tibble: 8 × 4
    ##   .metric     .estimator .estimate part    
    ##   <chr>       <chr>          <dbl> <chr>   
    ## 1 accuracy    binary         0.900 training
    ## 2 kap         binary         0.508 training
    ## 3 mn_log_loss binary         0.249 training
    ## 4 roc_auc     binary         0.904 training
    ## 5 accuracy    binary         0.887 testing 
    ## 6 kap         binary         0.5   testing 
    ## 7 mn_log_loss binary         0.287 testing 
    ## 8 roc_auc     binary         0.890 testing

``` r
# ROC Curve 
scored_train_logre %>%
  mutate(model="train")%>%
  bind_rows(scored_test_logre %>%
            mutate(model="test"))%>%
  group_by(model)%>%
  roc_curve(response,.pred_1)%>%
  autoplot()
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
# Variable Importandce 
logistic_step %>%
  vip(10)+
  labs(title="logistic model 1 variable importance plot")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

``` r
# Confusion Matrix
scored_train_logre%>%
  conf_mat(response,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Train confusion matrix")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->

``` r
scored_test_logre%>%
  conf_mat(response,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Test confusion matrix")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->

``` r
# precision,recall
scored_train_logre %>%
  yardstick::precision(response, .pred_class)%>%
mutate(part="train")%>%
  bind_rows(scored_train_logre %>%
    yardstick::recall(response, .pred_class)%>%
    mutate(part="train"))%>%
  bind_rows(scored_test_logre %>%
  yardstick::precision(response, .pred_class)%>%
    mutate(part="test")%>%
  bind_rows(scored_test_logre %>%
    yardstick::recall(response, .pred_class)%>%mutate(part="test")))
```

    ## # A tibble: 4 × 4
    ##   .metric   .estimator .estimate part 
    ##   <chr>     <chr>          <dbl> <chr>
    ## 1 precision binary         0.752 train
    ## 2 recall    binary         0.446 train
    ## 3 precision binary         0.8   test 
    ## 4 recall    binary         0.429 test

## This is the predict from my final model

## use new data to predict

``` r
new_customer<- read_csv("new_customers_mkt.csv")%>%clean_names()
```

    ## Rows: 40 Columns: 26
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): Education, MarStat, Dt_Customer
    ## dbl (23): ID, Birth, Income, Kids, Teens, Recency, Wines, Fruits, Meat, Fish...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
format_dmy_new <- as.Date(new_customer$dt_customer,format="%d-%m-%Y")
format_dmy2_new<- as.Date(new_customer$dt_customer,format = "%d/%m/%Y")
new_customer$dt_customer<-as.Date(ifelse(is.na(format_dmy_new),format_dmy2_new,format_dmy_new),origin ="1970-01-01")


new_customer<-new_customer %>%
  mutate(education=as.factor(education))%>%
  mutate(mar_stat=as.factor(mar_stat))%>%
  mutate(dt_customer_days = Sys.Date()-dt_customer)%>%
  mutate(dt_customer_days =as.numeric(dt_customer_days))%>%
  mutate(age=2022-birth)
new_customer<-subset(new_customer,select=-c(dt_customer,birth))
skim(new_customer)
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
new_customer
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
26
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
24
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
education
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
5
</td>
<td style="text-align:left;">
Gra: 22, PhD: 9, Mas: 7, 2nC: 1
</td>
</tr>
<tr>
<td style="text-align:left;">
mar_stat
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
5
</td>
<td style="text-align:left;">
Mar: 17, Par: 8, Div: 7, Sin: 7
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
id
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
130.48
</td>
<td style="text-align:right;">
11.68
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
120.75
</td>
<td style="text-align:right;">
130.5
</td>
<td style="text-align:right;">
140.25
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
▇▇▇▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
income
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
53533.05
</td>
<td style="text-align:right;">
21793.55
</td>
<td style="text-align:right;">
2447
</td>
<td style="text-align:right;">
38060.00
</td>
<td style="text-align:right;">
55212.0
</td>
<td style="text-align:right;">
71532.50
</td>
<td style="text-align:right;">
84835
</td>
<td style="text-align:left;">
▂▃▆▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
kids
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▃
</td>
</tr>
<tr>
<td style="text-align:left;">
teens
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
▇▁▅▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
recency
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
44.12
</td>
<td style="text-align:right;">
31.42
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
17.00
</td>
<td style="text-align:right;">
38.0
</td>
<td style="text-align:right;">
66.00
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:left;">
▇▅▃▃▅
</td>
</tr>
<tr>
<td style="text-align:left;">
wines
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
336.52
</td>
<td style="text-align:right;">
344.49
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
34.50
</td>
<td style="text-align:right;">
196.5
</td>
<td style="text-align:right;">
571.25
</td>
<td style="text-align:right;">
1103
</td>
<td style="text-align:left;">
▇▂▂▂▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fruits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
30.80
</td>
<td style="text-align:right;">
42.85
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
9.0
</td>
<td style="text-align:right;">
37.00
</td>
<td style="text-align:right;">
153
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
meat
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
241.02
</td>
<td style="text-align:right;">
335.20
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
23.50
</td>
<td style="text-align:right;">
79.5
</td>
<td style="text-align:right;">
395.75
</td>
<td style="text-align:right;">
1725
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
fish
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
42.67
</td>
<td style="text-align:right;">
53.38
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
19.5
</td>
<td style="text-align:right;">
69.75
</td>
<td style="text-align:right;">
175
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
sweets
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
39.10
</td>
<td style="text-align:right;">
57.63
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.75
</td>
<td style="text-align:right;">
18.5
</td>
<td style="text-align:right;">
50.50
</td>
<td style="text-align:right;">
263
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
gold
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
55.70
</td>
<td style="text-align:right;">
78.82
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6.50
</td>
<td style="text-align:right;">
22.0
</td>
<td style="text-align:right;">
58.25
</td>
<td style="text-align:right;">
362
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
deals
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
2.53
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
web
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.58
</td>
<td style="text-align:right;">
4.56
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.0
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
catalog
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.5
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
▇▂▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
store
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5.72
</td>
<td style="text-align:right;">
3.17
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
▂▇▅▅▃
</td>
</tr>
<tr>
<td style="text-align:left;">
visits
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.72
</td>
<td style="text-align:right;">
2.39
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
▅▅▅▃▇
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp1
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
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmp2
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
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
cmplain
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
▁▁▇▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
dt_customer_days
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3497.90
</td>
<td style="text-align:right;">
183.84
</td>
<td style="text-align:right;">
3082
</td>
<td style="text-align:right;">
3337.00
</td>
<td style="text-align:right;">
3544.5
</td>
<td style="text-align:right;">
3642.25
</td>
<td style="text-align:right;">
3759
</td>
<td style="text-align:left;">
▂▆▅▇▇
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
55.45
</td>
<td style="text-align:right;">
17.37
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
45.25
</td>
<td style="text-align:right;">
53.0
</td>
<td style="text-align:right;">
68.50
</td>
<td style="text-align:right;">
123
</td>
<td style="text-align:left;">
▇▆▅▁▁
</td>
</tr>
</tbody>
</table>

``` r
bake_new_log<-bake(mkt_log_recipe,new_data = new_customer)
bake_new_step<-bake(reduce_recipe,new_data = bake_new_log)
## at first, I use baked train/test data to predict. But I cannot bake new data since it do not have response column

new1<-predict(logistic_step,bake_new_step,type="prob")%>%
  bind_cols(predict(logistic_step,bake_new_step,type="class"))%>%
  bind_cols(bake_new_step)

new_customer%>%
  dplyr::select(id)%>%
  bind_cols(new1%>%
         dplyr::select(.pred_class) )
```

    ## # A tibble: 40 × 2
    ##       id .pred_class
    ##    <dbl> <fct>      
    ##  1   111 0          
    ##  2   112 0          
    ##  3   113 0          
    ##  4   114 0          
    ##  5   115 1          
    ##  6   116 0          
    ##  7   117 0          
    ##  8   118 0          
    ##  9   119 0          
    ## 10   120 0          
    ## # … with 30 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

\#model 3- random forest

``` r
rf_model <- rand_forest(trees = 800) %>%
   set_mode("classification") %>%
   set_engine("ranger", importance="permutation")
#num.threads = 8 , max.depth = 10
rf_workflow <- workflow() %>%
  add_recipe(mkt_log_recipe) %>%
  add_model(rf_model) %>%
  fit(train)

rf_workflow
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 2 Recipe Steps
    ## 
    ## • step_impute_median()
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, num.trees = ~800,      importance = ~"permutation", num.threads = 1, verbose = FALSE,      seed = sample.int(10^5, 1), probability = TRUE) 
    ## 
    ## Type:                             Probability estimation 
    ## Number of trees:                  800 
    ## Sample size:                      1568 
    ## Number of independent variables:  34 
    ## Mtry:                             5 
    ## Target node size:                 10 
    ## Variable importance mode:         permutation 
    ## Splitrule:                        gini 
    ## OOB prediction error (Brier s.):  0.08369285

\#assessment

``` r
# score training
predict(rf_workflow, train, type = "prob") %>%
  bind_cols(predict(rf_workflow, train, type = "class")) %>%
  mutate(part = "training") %>%
  bind_cols(., train) -> scored_train

# -- score testing
predict(rf_workflow, test, type = "prob") %>%
  bind_cols(predict(rf_workflow,  test, type = "class")) %>%
  mutate(part = "testing") %>%
  bind_cols(., test) -> scored_test


options(yardstick.event_first = FALSE)
 
scored_train %>%
  metrics(response, .pred_1,estimate = .pred_class)%>%
  mutate(part="training")%>%
  bind_rows(scored_test %>%
              metrics(response,.pred_1,estimate = .pred_class)%>%
            mutate(part="testing"))
```

    ## # A tibble: 8 × 4
    ##   .metric     .estimator .estimate part    
    ##   <chr>       <chr>          <dbl> <chr>   
    ## 1 accuracy    binary         0.974 training
    ## 2 kap         binary         0.885 training
    ## 3 mn_log_loss binary         0.131 training
    ## 4 roc_auc     binary         0.999 training
    ## 5 accuracy    binary         0.863 testing 
    ## 6 kap         binary         0.31  testing 
    ## 7 mn_log_loss binary         0.309 testing 
    ## 8 roc_auc     binary         0.881 testing

``` r
# ROC Curve 
scored_train %>%
  mutate(model="train")%>%
  bind_rows(scored_test %>%
            mutate(model="test"))%>%
  group_by(model)%>%
  roc_curve(response,.pred_1)%>%
  autoplot()
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
# Variable Importandce 
rf_workflow %>%
  extract_fit_parsnip() %>%
  vip()+
  labs(title="RF model 1 variable importance plot")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->

``` r
# Confusion Matrix
scored_test%>%
  conf_mat(response,.pred_class)%>%
  autoplot(type= "heatmap")+
  labs(title="Test confusion matrix")
```

![](project3_Shi_Shi_cluster_files/figure-gfm/unnamed-chunk-35-3.png)<!-- -->

``` r
# precision,recall
scored_test %>%
  yardstick::precision(response, .pred_class)%>%
  bind_rows(scored_test %>%
  yardstick::recall(response, .pred_class))
```

    ## # A tibble: 2 × 3
    ##   .metric   .estimator .estimate
    ##   <chr>     <chr>          <dbl>
    ## 1 precision binary         0.812
    ## 2 recall    binary         0.232

\#Even though i made a prediction for the new_customer data, this is not
my final model. I take reduced logistic model prediction as my final
prediction and put that in my report.

``` r
new2<-predict(rf_workflow, new_customer, type = "prob")  %>%
  bind_cols(predict(rf_workflow,new_customer,type="class"))%>%
  bind_cols(new_customer)

new2%>%
  dplyr::select(id,.pred_class)
```

    ## # A tibble: 40 × 2
    ##       id .pred_class
    ##    <dbl> <fct>      
    ##  1   111 0          
    ##  2   112 0          
    ##  3   113 0          
    ##  4   114 0          
    ##  5   115 0          
    ##  6   116 0          
    ##  7   117 0          
    ##  8   118 0          
    ##  9   119 0          
    ## 10   120 0          
    ## # … with 30 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Cmd+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
