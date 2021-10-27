# The wdlRunR package -- DEPRECATED AND NO LONGER MAINTAINED

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

## Working with AWS

### [Make a custom AMI with cromwell additions]

```{sh}
python create-genomics-ami.py \
       --user-data cromwell-genomics-ami.cloud-init.yaml \
       --key-pair-name EveryDay \
       --scratch-mount-point /cromwell_root \
       --profile default \
       --ami-description "AMI for use with Cromwell"
```

TODO: Do this with packer....

[Make a custom AMI with cromwell additions]: https://docs.opendata.aws/genomics-workflows/cromwell/cromwell-aws-batch/#custom-ami-with-cromwell-additions

### Set up Cromwell config file

```
// aws.conf
include required(classpath("application"))

aws {
  application-name = "cromwell"
  auths = [{
      name = "default"
      scheme = "default"
  }]
  #
  # be sure to set this!!
  #
  region = "us-east-1"
}

engine {
  filesystems {
    s3 { auth = "default" }
  }
}

backend {
  default = "AWSBATCH"
  providers {
    AWSBATCH {
      actor-factory = "cromwell.backend.impl.aws.AwsBatchBackendLifecycleActorFactory"
      config {
	    #
		# Change this to an EXISTING bucket
		# Cromwell does not create the bucket for you
		#
        root = "s3://<your-s3-bucket-name>/cromwell-execution"
        auth = "default"

        numSubmitAttempts = 3
        numCreateDefinitionAttempts = 3

        concurrent-job-limit = 16

        default-runtime-attributes {
		  #
		  # You need to set up your AWS batch
		  # queues and compute environments. 
		  # Then, paste in the Queue ARN, 
		  # available from the AWS batch console
		  # under the queue details
		  # 
          queueArn: "<your-queue-arn>"
        }

        filesystems {
          s3 {
            auth = "default"
          }
        }
      }
    }
  }
}
```


# Testing cromwell

```{sh}
curl -X POST --header "Accept: application/json" \
     "localhost:8000/api/workflows/v1" \
     -F workflowSource=@get_ebi_fastq.wdl \
     -F workflowInputs=@get_ebi_fastq.inputs
```
