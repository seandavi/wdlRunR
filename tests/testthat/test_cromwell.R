library(wdlRunR)
context("wdlRunR")

cromwell_jar = getCromwellJar(cromwell_version='25')
cromwell_log = file.path(tempdir(),'cromwell.log')
system(sprintf('java -jar %s server > %s 2>&1 &', cromwell_jar, cromwell_log))
options(cromwellBase = 'http://localhost:8000')

# wait for server to start up
Sys.sleep(10)

test_that("cromwellStats", {
    res = cromwellStats()
    expect_equal(names(res),c('workflows','jobs'))
    expect_equal(res$jobs,0)
    expect_equal(res$workflows,0)
    expect_is(res,'list')
    expect_is(res,'cromwell_api')
    expect_is(res,'cromwell_stats')
})

test_that("cromwellBackends", {
    res = cromwellBackends()
    expect_equal(names(res),c('supportedBackends','defaultBackend'))
    expect_equal(res$supportedBackends[[1]],'Local')
    expect_equal(res$defaultBackend,'Local')
    expect_is(res,'list')
    expect_is(res,'cromwell_api')
    expect_is(res,'cromwell_backends')
})

test_that("cromwellBase",{
    expect_equal(cromwellBase(),'http://localhost:8000')
})

hello_wdl = "task hello {
  String name

  command {
    echo 'Hello ${name}!'
  }
  output {
    File response = stdout()
  }
}

workflow test {
  call hello
}"

randomStrings = sapply(1:10,function(r) {paste(sample(LETTERS,10),collapse="")})
wdlInputs = data.frame(test.hello.name=randomStrings)

test_that('cromwellBatch',{
    res = cromwellBatch(wdlSource = hello_wdl,workflowInputs=wdlInputs)
    # and we do this to allow the jobs to get running
    expect_is(res,'cromwell_api')
    expect_named(res,c('content','response'))
    expect_length(res$content,10)
})

Sys.sleep(10)

test_that('cromwellQuery',{
    res = cromwellStats()
    expect_equal(names(res),c('workflows','jobs'))
    res = cromwellQuery()
    expect_equal(ncol(res),6)
    expect_gte(nrow(res),10)
})


# cleanup
system('pgrep -f cromwell | xargs -I {} kill -9 {}')
unlink(cromwell_jar)
unlink(cromwell_log)
