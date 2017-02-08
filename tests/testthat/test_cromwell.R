library(cRomwell)
context("cromwell")

cromwell_jar = getCromwellJar(cromwell_version='24')
cromwell_log = file.path(tempdir(),'cromwell.log')
system(sprintf('java -jar %s server > %s 2>&1 &', cromwell_jar, cromwell_log))

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


# cleanup
system('pgrep -f cromwell | xargs -I {} kill -9 {}')
unlink(cromwell_jar)
unlink(cromwell_log)
