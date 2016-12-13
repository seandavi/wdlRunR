task r_fastqa {
    File fastq

    command {
        R --no-save << CODE
        require(ShortRead)
        stats = qa('${fastq}')
        report(stats,dest="output")
        CODE
    }

    output {
        Array[File] qareport = glob('output/*')
    }

}
        
workflow r_fqa_wf {
   call r_fastqa
}
