def configs = [
              //["sail", "piccolo", "rv32izicsr_zifencei_xcheri",   "rv32izicsr_zifencei_xcheri", "", "'.*'", "'^(atomic(64)?|memAmo(64)?|float(64)?|double(64)?|hpm|csr|capdecode|pte|cloadtags)\$'"]
                ["sail", "flute",  "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'.*'", "'^(atomic(64)?|memAmo(64)?|float(64)?|double(64)?|hpm|csr|capdecode|pte|cloadtags)\$'"],
                ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'.*'", "'^(atomic(64)?|memAmo(64)?|float(64)?|double(64)?|hpm|csr|capdecode|pte|cloadtags)\$'"]
              //["sail", "flute", "rv64izicsr_zifencei_xcheri", "rv64_xcheri", ""]
              //["sail", "rvbs", "rv32izicsr_zifencei_xcheri", "rv32izicsr_zifencei_xcheri", "--support-misaligned"]
              //["sail", "ibex", "rv32izicsr_zifencei_xcheri", "rv32izicsr_zifencei_xcheri", ""]
              //["sail", "spike", "rv32izicsr_zifencei_xcheri", "rv32izicsr_zifencei_xcheri", ""]
              //["sail", "spike", "rv64izicsr_zifencei_xcheri", "rv64_xcheri", ""]
              ]

def jobs = [:]

configs.each {
  conf ->
    def name = conf[0]+"vs"+conf[1]+"-"+conf[2]+"-"+conf[3]+"-"+conf[4]
    jobs.put(name, {
      ansiColor('xterm') {
        node('docker') {
          stage(name) {
            docker.image('ctsrd/testrig').pull()
            docker.image('ctsrd/testrig').inside {
              echo name
              sh "rm -rf ${name} && mkdir ${name}"
              def retval = sh returnStatus: true, script: "cd ${name} && mkdir failures && /home/jenkins/TestRIG/utils/scripts/runTestRIG.py -a ${conf[0]} -b ${conf[1]} -r ${conf[2]} --verification-archstring ${conf[3]} ${conf[4]} -n 100 --test-include-regex ${conf[5]} --test-exclude-regex ${conf[6]} -S failures/"
              sh "cd ${name} && mkdir coverage && /home/jenkins/sail/sailcov/sailcov -t sail_coverage -a /home/jenkins/TestRIG/riscv-implementations/sail-cheri-riscv/generated_definitions/c/all_branches --index index --prefix coverage/ `find /home/jenkins/TestRIG/riscv-implementations/sail-cheri-riscv -name '*.sail'`"
              archiveArtifacts "${name}/**"
              if (retval != 0) {
                error "TestRIG returned non-zero"
              }
            }
          }
        }
      }
    })
}

parallel jobs
