task hello {
  String name

  command {
    echo 'Hello ${name}!' > test.out
  }
  output {
    String response = read_string("test.out")
  }
}

workflow test {
  call hello
}
