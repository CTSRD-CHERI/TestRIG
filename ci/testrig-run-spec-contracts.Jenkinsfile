def configs = [
                ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'^(data_scc|inst_scc|sbc_cond_1|sbc_jumps|sbc_excps|stc)\$'", "all", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'^(data_scc)\$'", "data_scc", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'^(inst_scc)\$'", "inst_scc", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'^(sbc_cond_1)\$'", "sbc_cond_1", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'^(sbc_jumps)\$'", "sbc_jumps", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'^(sbc_excps)\$'", "sbc_excps", "500"]
              , ["sail", "toooba", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsczifencei_xcheri", "", "'^(stc)\$'", "stc", "500"]
              ]

def jobs = [:]

configs.each {
  conf ->
    def name = conf[0]+"vs"+conf[1]+"-"+conf[6]
    jobs.put(name, {
      ansiColor('xterm') {
        node('docker') {
          stage(name) {
            docker.image('ctsrd/testrig').pull()
            docker.image('ctsrd/testrig').inside {
              echo name
              sh "rm -rf ${name} && mkdir ${name}"
              def retval = sh returnStatus: true, script: "cd ${name} && mkdir failures && /home/jenkins/TestRIG/utils/scripts/runTestRIG.py -a ${conf[0]} -b ${conf[1]} -r ${conf[2]} --verification-archstring ${conf[3]} ${conf[4]} -n ${conf[7]} --test-include-regex ${conf[5]} --no-shrink --continue-on-fail -S failures/"
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
