
<!-- README.md is generated from README.Rmd. Please edit that file -->
trinalysis
==========

Tools for working with transactional data in R

Installation
============

``` r
install.packages("devtools")
devtools::install_github("ben519/trinalysis")
```

------------------------------------------------------------------------

Demo
====

Simulate Random Transactions (with a realistic pattern of customer churn)
-------------------------------------------------------------------------

``` r
library(trinalysis)

set.seed(2357)
transactions <- sample_transactions(
  nCusts = 100, 
  sdTransactions = 10, 
  sdAmount = 10, 
  minDate = as.Date("2010-1-1"), 
  maxDate = as.Date("2015-12-31"), 
  products = c("baseball", "basketball", "football", "soccerball")
)

transactions
##      TransactionID TransactionDate CustomerID Amount    Product
##   1:             1      2011-11-23          1   6.88   football
##   2:             2      2013-11-28          2   1.58   football
##   3:             3      2010-12-09          3  16.07 basketball
##   4:             4      2011-01-01          3   8.39 soccerball
##   5:             5      2013-07-21          3  25.92 soccerball
##  ---                                                           
## 397:           397      2012-10-27         99  11.53   baseball
## 398:           398      2015-08-26         99  10.46 basketball
## 399:           399      2015-03-15         99   4.25 basketball
## 400:           400      2011-01-31         99  12.85   football
## 401:           401      2015-01-02        100  16.52   baseball
```

Extract customers from transactions
-----------------------------------

``` r
custs <- get_customers(transactions, colsCategory = "Product")
print(custs, 5)
##      CustomerID Transactions TransactionDate.First TransactionDate.Last
##   1:          1            1            2011-11-23           2011-11-23
##   2:          2            1            2013-11-28           2013-11-28
##   3:          3            3            2010-12-09           2013-07-21
##   4:          4            4            2010-07-30           2015-10-18
##   5:          5            1            2010-08-04           2010-08-04
##  ---                                                                   
##  96:         96            1            2015-10-30           2015-10-30
##  97:         97            1            2015-03-16           2015-03-16
##  98:         98            9            2014-04-14           2015-11-23
##  99:         99            7            2010-02-12           2015-08-26
## 100:        100            1            2015-01-02           2015-01-02
##      Product.First
##   1:      football
##   2:      football
##   3:    basketball
##   4:      football
##   5:      baseball
##  ---              
##  96:      football
##  97:      football
##  98:    basketball
##  99:    soccerball
## 100:      baseball
```

Basic Cohort Analyses
---------------------

``` r
make_triangles(
  transactions, 
  format="triangular", 
  minLeftOrigin = as.Date("2010-01-01")
)
## $ActiveCustomers
##                     Cohort 0 - 12 12 - 24 24 - 36 36 - 48 48 - 60 60 - 72
## 1: 2010-01-01 - 2010-12-31     22       9      10      11       7      10
## 2: 2011-01-01 - 2011-12-31     20      11      12      11       9      NA
## 3: 2012-01-01 - 2012-12-31     12       7       5       5      NA      NA
## 4: 2013-01-01 - 2013-12-31     20      11      12      NA      NA      NA
## 5: 2014-01-01 - 2014-12-31     12       8      NA      NA      NA      NA
## 6: 2015-01-01 - 2015-12-31     14      NA      NA      NA      NA      NA
## 
## $Transactions
##                     Cohort 0 - 12 12 - 24 24 - 36 36 - 48 48 - 60 60 - 72
## 1: 2010-01-01 - 2010-12-31     30      18      21      17      11      16
## 2: 2011-01-01 - 2011-12-31     33      22      20      17      18      NA
## 3: 2012-01-01 - 2012-12-31     17      17       8       8      NA      NA
## 4: 2013-01-01 - 2013-12-31     29      21      20      NA      NA      NA
## 5: 2014-01-01 - 2014-12-31     22      16      NA      NA      NA      NA
## 6: 2015-01-01 - 2015-12-31     20      NA      NA      NA      NA      NA
## 
## $Transactions.cmltv
##                     Cohort 12 24 36 48  60  72
## 1: 2010-01-01 - 2010-12-31 30 48 69 86  97 113
## 2: 2011-01-01 - 2011-12-31 33 55 75 92 110  NA
## 3: 2012-01-01 - 2012-12-31 17 34 42 50  NA  NA
## 4: 2013-01-01 - 2013-12-31 29 50 70 NA  NA  NA
## 5: 2014-01-01 - 2014-12-31 22 38 NA NA  NA  NA
## 6: 2015-01-01 - 2015-12-31 20 NA NA NA  NA  NA
## 
## $Amount
##                     Cohort 0 - 12 12 - 24 24 - 36 36 - 48 48 - 60 60 - 72
## 1: 2010-01-01 - 2010-12-31 314.70  150.91  191.09  140.89   86.94  162.88
## 2: 2011-01-01 - 2011-12-31 252.26  186.85  144.38  121.21  150.71      NA
## 3: 2012-01-01 - 2012-12-31 132.32  100.71   62.29   78.02      NA      NA
## 4: 2013-01-01 - 2013-12-31 185.52  150.33  186.78      NA      NA      NA
## 5: 2014-01-01 - 2014-12-31 181.49  139.06      NA      NA      NA      NA
## 6: 2015-01-01 - 2015-12-31 179.84      NA      NA      NA      NA      NA
## 
## $Amount.cmltv
##                     Cohort     12     24     36     48     60      72
## 1: 2010-01-01 - 2010-12-31 314.70 465.61 656.70 797.59 884.53 1047.41
## 2: 2011-01-01 - 2011-12-31 252.26 439.11 583.49 704.70 855.41      NA
## 3: 2012-01-01 - 2012-12-31 132.32 233.03 295.32 373.34     NA      NA
## 4: 2013-01-01 - 2013-12-31 185.52 335.85 522.63     NA     NA      NA
## 5: 2014-01-01 - 2014-12-31 181.49 320.55     NA     NA     NA      NA
## 6: 2015-01-01 - 2015-12-31 179.84     NA     NA     NA     NA      NA
```

Contact
-------

If you'd like to contact me regarding bugs, questions, or general consulting, feel free to drop me a line - <bgorman519@gmail.com>
