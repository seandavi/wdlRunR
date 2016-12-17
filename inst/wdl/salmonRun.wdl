task salmonIndex {
  File transcripts
  String indexname

  command {
    bash /tmp/log_sar &
    salmon index -i ${indexname} -t ${transcripts}
    sar -A | gzip > sar.txt.gz
  }
  output {
    File ihash = "${indexname}/hash.bin"
    File iheader = "${indexname}/header.json"
    File irsd = "${indexname}/rsd.bin"
    File isa = "${indexname}/sa.bin"
    File itxpInfo = "${indexname}/txpInfo.bin"
    File iversionInfo = "${indexname}/versionInfo.json"
    File sar = "sar.txt.gz"
  }
  runtime {
    docker: "seandavi/salmon"
    memory: "8G"
    cpu: "1"
    disks: "/mnt/mnt1 30 SSD"
  }
}

task salmonQuant {
  Array[File] fastqTar
  File ihash
  File iheader 
  File irsd 
  File isa 
  File itxpInfo 
  File iversionInfo
  String file_id

  command <<<

    set -x

    bash /tmp/log_sar &
    for i in ${sep=' ' fastqTar};
    do
      if [[ $file =~ \.t?gz$ ]]; then
        tar -xvzf $i;
      else
        tar -xvf $i;
      fi
    done;
    export COUNT=`ls *{fastq,fq}* | wc -l`
    if [ $COUNT \> 1 ];
    then
        salmon quant --numGibbsSamples 50 --seqBias --gcBias -p 32 -i `dirname ${ihash}` -l A -1 *1.fastq* -2 *2.fastq* -o quantfile;
    else
        salmon quant --numGibbsSamples 50 --seqBias --gcBias -p 32 -i `dirname ${ihash}` -l A -r *fastq* -o quantfile;
    fi;
    sar -A | gzip > sar.txt.gz
  >>>
  
  runtime {
    docker: "seandavi/salmon"
    preemptible: 3
    memory: "8G"
    cpu: "32"
    disks: "local-disk 400 SSD"
  }

# .
# ├── aux_info
# │   ├── bootstrap
# │   │   ├── bootstraps.gz
# │   │   └── names.tsv.gz
# │   ├── exp3_seq.gz
# │   ├── exp5_seq.gz
# │   ├── expected_bias.gz
# │   ├── exp_gc.gz
# │   ├── fld.gz
# │   ├── meta_info.json
# │   ├── obs3_seq.gz
# │   ├── obs5_seq.gz
# │   ├── observed_bias_3p.gz
# │   ├── observed_bias.gz
# │   └── obs_gc.gz
# ├── cmd_info.json
# ├── lib_format_counts.json
# ├── libParams
# │   └── flenDist.txt
# ├── logs
# │   └── salmon_quant.log
# └── quant.sf


  output {
    File bootstraps      = 'quantfile/aux_info/bootstrap/bootstraps.gz'
    File names_tsv       = 'quantfile/aux_info/bootstrap/names.tsv.gz'
    File cmd_info        = "quantfile/cmd_info.json"
    File exp3_seq        = "quantfile/aux_info/exp3_seq.gz"
    File exp5_seq        = "quantfile/aux_info/exp5_seq.gz"
    File expected_bias   = "quantfile/aux_info/expected_bias.gz"
    File fld             = "quantfile/aux_info/fld.gz"
    File flenDist        = "quantfile/libParams/flenDist.txt"
    File lib_format_counts = "quantfile/lib_format_counts.json"
    File meta_info       = "quantfile/aux_info/meta_info.json"
    File obs3_seq        = "quantfile/aux_info/obs3_seq.gz"
    File obs5_seq        = "quantfile/aux_info/obs5_seq.gz"
    File observed_bias   = "quantfile/aux_info/observed_bias.gz"
    File observed_bias_3p= "quantfile/aux_info/observed_bias_3p.gz"
    File quant           = "quantfile/quant.sf"
    File salmon_quant_log= "quantfile/logs/salmon_quant.log"
    File sar             = "sar.txt.gz"
  }
}




workflow salmonRun {
  File transcripts
  Array[File] fastqTar
  String indexname
  String file_id
  
    call salmonIndex {
      input: transcripts=transcripts, indexname=indexname
    }
    call salmonQuant {
      input: fastqTar=fastqTar,
             file_id=file_id,
             ihash = salmonIndex.ihash,
             iheader = salmonIndex.iheader,
             irsd = salmonIndex.irsd,
             isa = salmonIndex.isa,
             itxpInfo = salmonIndex.itxpInfo,
             iversionInfo = salmonIndex.iversionInfo
    }
}
