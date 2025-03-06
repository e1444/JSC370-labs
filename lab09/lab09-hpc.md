Lab 9 - HPC
================

# Learning goals

In this lab, you are expected to practice the following skills:

- Evaluate whether a problem can be parallelized or not.
- Practice with the parallel package.
- Use Rscript to submit jobs.

## Problem 1

Give yourself a few minutes to think about what you learned about
parallelization. List three examples of problems that you believe may be
solved using parallel computing, and check for packages on the HPC CRAN
task view that may be related to it.

1.  Aggregation functions (e.g., `sum`, `mean`, `sd`, etc.) for large
    datasets.
    - Package: `parallel`.
2.  Bootstrapping for confidence intervals.
    - `caret`.
3.  Cross-validation for model selection.
    - `caret`.

## Problem 2: Pre-parallelization

The following functions can be written to be more efficient without
using `parallel`:

1.  This function generates a `n x k` dataset with all its entries
    having a Poisson distribution with mean `lambda`.

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL

  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))

  return(x)
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  return(matrix(rpois(n*k, lambda), nrow=n))
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

How much faster?

`fun1alt` (11.931us) is ~10 times faster than `fun1` (106.764us).

2.  Find the column max (hint: Checkout the function `max.col()`).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  return(apply(x, 2, max))
}

fun2alt <- function(x) {
  return(x[cbind(max.col(t(x)), seq_len(ncol(x)))])
}

# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

`fun2alt` (64.862us) is ~8 times faster than `fun2` (486.014us).

## Problem 3: Parallelize everything

We will now turn our attention to non-parametric
[bootstrapping](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)).
Among its many uses, non-parametric bootstrapping allow us to obtain
confidence intervals for parameter estimates without relying on
parametric assumptions.

The main assumption is that we can approximate many experiments by
resampling observations from our original dataset, which reflects the
population.

This function implements the non-parametric bootstrap:

``` r
my_boot <- function(dat, stat, R, ncpus = 1L) {
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n * R, TRUE), nrow = n, ncol = R)

  # Making the cluster using `ncpus`
  # STEP 1:
  cl <- makeCluster(ncpus)
  # STEP 2: GOES HERE
  clusterExport(cl, c("dat", "idx", "stat"), envir = environment())

  # STEP 3: THIS FUNCTION NEEDS TO BE REPLACED WITH parLapply
  # ans <- lapply(seq_len(R), function(i) {
  #   stat(dat[idx[,i], , drop=FALSE])
  # })

  ans <- parLapply(
    cl,
    seq_len(R),
    function(i) {
    stat(dat[idx[, i], , drop = FALSE])
  })

  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)

  # STEP 4: GOES HERE
  stopCluster(cl)

  ans
}
```

1.  Use the previous pseudocode, and make it work with `parallel`. Here
    is just an example for you to try:

``` r
# Bootstrap of a linear regression model
my_stat <- function(dat) {
    coef(lm(y ~ x, data = dat))
  }

# DATA SIM
set.seed(1)
n <- 500
R <- 1e4
x <- cbind(rnorm(n))
y <- x * 5 + rnorm(n)
dat <- data.frame(y = y, x = x)

# Check if we get something similar as lm
ans0 <- confint(lm(y ~ x, data = dat))
ans1 <- my_boot(dat, my_stat, R, ncpus = 1L)
ans1 <- t(apply(ans1, 2, quantile, c(0.025, 0.975)))

# Check if the confidence intervals are similar
print(ans0)
print(ans1)
```

2.  Check whether your version actually goes faster than the
    non-parallel version:

``` r
microbenchmark::microbenchmark(
  my_boot(dat, my_stat, R, ncpus = 1L),
  my_boot(dat, my_stat, R, ncpus = 2L),
  times = 10L
)
```

Using 2 cores is faster (1.61s) than using 1 core (2.71s).

## Problem 4: Compile this markdown document using Rscript

Once you have saved this Rmd file, try running the following command in
your terminal:

``` bash
Rscript --vanilla -e 'rmarkdown::render("[full-path-to-your-Rmd-file.Rmd]")' &
```

Where `[full-path-to-your-Rmd-file.Rmd]` should be replace with the full
path to your Rmd file… :).
