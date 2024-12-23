---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# cv

<!-- badges: start -->
<!-- badges: end -->

The goal of cv is to provide a function for calculating the coefficient of variation (CV) for numeric data. The cv is useful for understanding the relative variability of a dataset.

## Installation

You can install the development version of cv like so:

```{r}
devtools::install_github("stat545ubc-2024/assignment-b2-jrchan01")
```

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(cv)
## using the gapminder dataset calculate the coefficient of variation for the population variable ##
## first load the gapminder library ##
library(gapminder)
## Now run the calculation ##
cv_function(gapminder$pop)
```

knit to md file

```{r}
devtools::build_readme()
```

