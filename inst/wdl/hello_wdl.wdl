task hello {
  String name

  command {
    echo 'Hello ${name}!' > test.out
  }
  output {
    String response = read_string("test.out")
  }
  runtime {
    docker: "ubuntu"
    memory: "1G"
    cpu: "1"
    disks: "/mnt/mnt1 30 SSD"
  }

}

workflow test {
  call hello
}
