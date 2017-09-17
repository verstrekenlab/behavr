# `behavr` [![Travis-CI Build Status](https://travis-ci.org/rethomics/behavr.svg?branch=master)](https://travis-ci.org/rethomics/behavr) [![Coverage Status](https://img.shields.io/codecov/c/github/rethomics/behavr/master.svg)](https://codecov.io/github/rethomics/behavr?branch=master)

<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tidyverse/hms?branch=master&svg=true)](https://ci.appveyor.com/project/tidyverse/hms)  -->

<!--[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/hms)](https://cran.r-project.org/package=hms) -->

`behavr` is part of the [rethomics framework](https://rethomics.github.io).
This README is a short explanation of the basics of `behavr`.
A [comprehensive documentation](https/rethomics.github.io/doc) of rethomics is also available.

## Data structure for high-throughput biological experiments

As behavioural biologists, we are often interested in recording behaviour of multiple animals.
In our context, the *data* is a collection of recordings (i.e. time series) with, often, several variables (e.g. time, position, activity, ...).
In addition to these recorded data, each animal may have different -- sometimes many -- experimental condition (e.g. age, sex, genotype, treatment and so on).
These latter variables are also known as *metavariable*. Together, they form the *metadata*.

During analysis of behaviour, it is very convenient to be able to **use both data and metadata together** in order to subset the data, alter or create new variables, and compute summary statistics.
A natural approach would be to store data as a `data.frame` with one row per measurement and one column per variable and metavariable.
In other words, we would repeat the metadata as many time as there are reads, and put all the animals in the same data structure. 
Even though this is very convenient, in practice, it takes a lot of memory (because of the redundant metadata)!

Alternatively, one could keep data and metadata separated (in two tables), and perform joins manually every time.
However, this quickly becomes error prone, as metadata and data are not in the same structure.
In addition, the "cognitive burden" is rather large.

This package addresses this issue by offering a new data structure derived from `data.table`.
At this stage, if you don't know much about `data.table`,
I suggest to [learn about it first](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html).

## Installation


```r
library(devtools)
install_github("rethomics/behavr")
```


## First steps

Let us create some toy data.
Five animals and 100 reads of four variables (`t`, `x` ,`y` ,`eating`) per animal.
**Both metadata and data must have the same key** (here, the column `id`).

```r
library(data.table)
library(behavr)
```

```
## Error in library(behavr): there is no package called 'behavr'
```

```r
set.seed(1)
met <- data.table::data.table(id = 1:5,
                             condition = letters[1:5],
                             sex = c("M","M","M","F", "F"),
                             t0 = c(100,2,-50,300,21),
                             key = "id")

data <- met[  ,
             list(t = 1L:100L,
                 x = rnorm(100),
                 y = rnorm(100),
                 eating = runif(100) > .5 ),
             by = "id"]

print(data)
```

```
##      id   t          x           y eating
##   1:  1   1 -0.6264538 -0.62036668   TRUE
##   2:  1   2  0.1836433  0.04211587  FALSE
##   3:  1   3 -0.8356286 -0.91092165   TRUE
##   4:  1   4  1.5952808  0.15802877   TRUE
##   5:  1   5  0.3295078 -0.65458464   TRUE
##  ---                                     
## 496:  5  96 -2.0908461 -0.30824994   TRUE
## 497:  5  97  1.6973939  0.01551524  FALSE
## 498:  5  98  1.0638812 -0.44231772   TRUE
## 499:  5  99 -0.7666166 -1.63800773  FALSE
## 500:  5 100  0.3820076 -0.64140116  FALSE
```

Putting data and metadata together.


```r
dt <- behavr(data, met)
```

```
## Error in behavr(data, met): could not find function "behavr"
```

```r
print(dt)
```

```
## function (x, df, ncp, log = FALSE) 
## {
##     if (missing(ncp)) 
##         .Call(C_dt, x, df, log)
##     else .Call(C_dnt, x, df, ncp, log)
## }
## <bytecode: 0x2dddc58>
## <environment: namespace:stats>
```

```r
summary(dt)
```

```
## Error in object[[i]]: object of type 'closure' is not subsettable
```

## Examples of what we can do with `behavr`

### Adding new variables

This works just like in `data.table`:


```r
dt[, z := x + y]
```

```
## Error in `:=`(z, x + y): Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").
```

### Filtering using variable

Again, just like in `data.table`:


```r
dt[t < 50]
```

```
## Error in t < 50: comparison (3) is possible only for atomic and list types
```

```r
# we could also wanted to reassign:
# dt <- dt[t < 50]
```


### Using meta variables

Say you want to get the **data** for male animals only.
If `sex` was a column of `dt`, we could just do `dt[sex=="M"]`
Instead, `sex` is a metavariable, so we need to **expand** it, using `xmv()`


```r
dt[xmv(sex) == "M"]
```

```
## Error in xmv(sex): could not find function "xmv"
```

This also works if we need to compute a variable according to a metavariable.
This is often the case when we need to standardise the positions or time compared to a reference, which is in the metadata.
For example, we could express `t` relatively `t0` (`t = t-t0`). 
So we expand `t0`, with `xmv()`:


```r
dt[, t := t - xmv(t0)]
```

```
## Error in `:=`(t, t - xmv(t0)): Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").
```

### Accessing the metadata

Metadata is another `data.table` "inside" `dt`.
To perform operations on the metadata, one can use `meta=TRUE` within the `[]` operator of `dt`.
So, to see the metadata:

```r
dt[meta = T]
```

```
## Error in dt[meta = T]: object of type 'closure' is not subsettable
```

Then, we can also use the same principle to filter:

```r
dt[id > 2, meta = T]
```

```
## Error in eval(expr, envir, enclos): object 'id' not found
```
Note that this does not alter the metadata, but merely shows a filtered copy.

More importantly, we can alter the metadata inline, for instance compute new columns (i.e. metavariables).
Here, we combine `sex` and `condition` as a `treatment`:


```r
dt[, treatment := interaction(condition, sex), meta = T]
```

```
## Error in `:=`(treatment, interaction(condition, sex)): Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").
```

```r
dt[meta = T]
```

```
## Error in dt[meta = T]: object of type 'closure' is not subsettable
```


### Summaries

In the pipeline of behavioural analysis, one often needs to compute summary statistics **per animal**, and link them to the metadata.
For example, here, we compute the median `x` position, and the proportion of `eating`:


```r
summary_dt <- dt[,
                 .(mean_x = mean(x),
                   prop_eating= mean(eating)),
                by=id]
```

```
## Error in .(mean_x = mean(x), prop_eating = mean(eating)): could not find function "."
```

```r
print(summary_dt)
```

```
## Error in print(summary_dt): object 'summary_dt' not found
```

Now, we can **rejoin** the metadata. 
That is, we can reunite the metadata to summary data:


```r
summary_all <- rejoin(summary_dt)
```

```
## Error in rejoin(summary_dt): could not find function "rejoin"
```

```r
print(summary_all)
```

```
## Error in print(summary_all): object 'summary_all' not found
```

## Toy data

`behavr` comes with functions to simulate toy data. See `?toy_activity_data`.

This is useful if you want quick way to generate a lot of data, say for prototyping functions.
For instance, imagine you want to build a package to compute circadian rythm variables.
You could generate 40 animals (two conditions) DAM like data using `toy_dam_data()`:

```r
query <- data.frame(experiment_id = "toy_experiment",
                    region_id = 1:40, # 40 animals
                    condition = c("A","B"), # conditions A,B,A,B ...
                    # drift is a coeficient to drift the time so that we make 
                    # to slightly different periods see below
                    drift = c(1.001,1.000)
                    )
dt <- toy_activity_data(query, duration = days(10))
print(dt)
```

One could, for instance, *artificially* drift the time to make it look like *the two conditions have different circadian periods*:

```
dt[, t:=t*xmv(drift)]
dt
```




## Going further

<!-- * [behavr](https://github.com/rethomics/behavr) -- to manipulate the data (create new variable/meta-variables) -->
* [pdf documentation](https://github.com/rethomics/behavr/raw/master/behavr.pdf)
* [damr](https://github.com/rethomics/damr) -- to load data from the DAM2 system
* [scopr](https://github.com/rethomics/scopr) -- to load data from the [ethoscope](http://gilestrolab.github.io/ethoscope/) system
* [ggetho](https://github.com/rethomics/ggetho) -- to plot visualise the data
* [sleepr](https://github.com/rethomics/sleepr) -- to perform sleep and circadian rythm analysis

