def configs = [
                ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "'^(csc_data|csc_inst|bsc_cond_1|bsc_jumps|bsc_excps|tsc)\$'", "all", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "'^(csc_data)\$'", "csc_data", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "'^(csc_inst)\$'", "csc_inst", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "'^(bsc_cond_1)\$'", "bsc_cond_1", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "'^(bsc_jumps)\$'", "bsc_jumps", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "'^(bsc_excps)\$'", "bsc_excps", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "'^(tsc)\$'", "tsc", "500"]
              ]

def jobs = [:]

configs.each {
  conf ->
    def name = conf[0]+"vs"+conf[1]+"-"+conf[4]
    jobs.put(name, {
      ansiColor('xterm') {
        node('docker') {
          stage(name) {
            docker.image('ctsrd/testrig').pull()
            docker.image('ctsrd/testrig').inside {
              echo name
              sh "rm -rf ${name} && mkdir ${name}"
//              def retval = sh returnStatus: true, script: "cd ${name} && mkdir failures && /home/jenkins/TestRIG/utils/scripts/runTestRIG.py -a ${conf[0]} --implementation-A-log implAlog.txt -b ${conf[1]} --implementation-B-log implBlog.txt -r ${conf[2]} --verification-archstring ${conf[2]} -n ${conf[5]} --test-include-regex ${conf[3]} --no-shrink --continue-on-fail -S failures/"
              def retval = sh returnStatus: true, script: "cd ${name} && mkdir failures && /home/jenkins/TestRIG/utils/scripts/runTestRIG.py -a ${conf[0]} --implementation-A-log implAlog.txt -b ${conf[1]} --implementation-B-log implBlog.txt -r ${conf[2]} --verification-archstring ${conf[2]} -n ${conf[5]} --test-include-regex ${conf[3]} --no-shrink --continue-on-fail -S failures/"
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
