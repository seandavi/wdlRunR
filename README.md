# The wdlRunR package

Follow development at [github](https://github.com/seandavi/wdlRunR).

This package executes Workflow Description Language (WDL) files from
within R. Compute platforms currently supported by
the
[Broad cromwell workflow engine](https://github.com/broadinstitute/cromwell) include:

- Local execution (good for testing)
- Sun GridEngine Clusters (and probably other HPC schedulers)
- HTCondor
- Google Compute Engine
- Apache Spark

## Install


```{r}
require(devtools)
devtools::install_github('seandavi/wdlRunR')
```

## Features

This package leverages all the typical data munging and analysis capabilities of R and Bioconductor, but adds the ability to orchestrate nearly arbitrarily large and complex workflows described using WDL (that are portable and written outside of this package).

Features of this package include:
- With appropriate backend (Google, for example), scale to *huge*
  computational capacity
- Submit single or batches of jobs
- Monitor jobs
- Retrieve metadata from submitted, completed, and running jobs
- Review log files from completed and failed jobs
- Track inputs and outputs of jobs
- Optional "caching" of jobs to avoid costly recomputation costs
