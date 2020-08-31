---
layout: post
title: "Fast sampling from von Mises Fisher distribution"
author:
  Elysée Aristide Houndetoungan
date: 2017-12-01
---
<style>
body {
text-align: justify}
</style>

## Description

I build an R package named **vMF** that samples from von Mises-Fisher distribution. Unlike the [movMF](https://cran.r-project.org/web/packages/movMF/index.html) package which also fits mixtures of von Mises-Fisher distrubution, **vFM** is focused on fast sampling from the distribution. This is useful for quickly performing high simulations in directional statistics. The package also calculates the density and the normalization constant of the von Mises-Fisher distribution.

Note that **vMF** does not substitute [movMF](https://cran.r-project.org/web/packages/movMF/index.html) to the extent that [movMF](https://cran.r-project.org/web/packages/movMF/index.html) handles many issues that **vMF** does not take into account. **vMF** can generate random vectors more quickly from the von Mises-Fisher distribution. I show in some examples below that **vMF** can be useful for directional statistical simulation questions.

## How to install vMF
You can proceed with installation by performing these [steps decribed on GitHub](https://github.com/ahoundetoungan/vMF/blob/master/README.md).
## Examples
With [examples](https://github.com/ahoundetoungan/vMF/blob/master/Examples.R), I show how to use **vMF** and compare its performance with that of the package [movMF](https://cran.r-project.org/web/packages/movMF/index.html).
