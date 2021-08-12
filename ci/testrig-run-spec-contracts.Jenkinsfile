def configs = [
                ["sail", "sail", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsafdczicsr_zifencei_xcheri", "", "'^(data_scc|inst_scc|sbc_cond_1|sbc_jumps|sbc_excps|stc)\$'", "all", "750"]
              , ["sail", "sail", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsafdczicsr_zifencei_xcheri", "", "'^(data_scc)\$'", "data_scc", "750"]
              , ["sail", "sail", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsafdczicsr_zifencei_xcheri", "", "'^(inst_scc)\$'", "inst_scc", "750"]
              , ["sail", "sail", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsafdczicsr_zifencei_xcheri", "", "'^(sbc_cond_1)\$'", "sbc_cond_1", "750"]
              , ["sail", "sail", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsafdczicsr_zifencei_xcheri", "", "'^(sbc_jumps)\$'", "sbc_jumps", "750"]
              , ["sail", "sail", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsafdczicsr_zifencei_xcheri", "", "'^(sbc_excps)\$'", "sbc_excps", "750"]
              , ["sail", "sail", "rv64imsafdczicsr_zifencei_xcheri", "rv64imsafdczicsr_zifencei_xcheri", "", "'^(stc)\$'", "stc", "750"]
              ]

def jobs = [:]

configs.each {
  conf ->
    def name = conf[0]+"vs"+conf[1]+"-"+conf[6]
    jobs.put(name, {
      ansiColor('xterm') {
        node('docker') {
          stage(name) {
            docker.image('ctsrd/testrig-pdr32').pull()
            docker.image('ctsrd/testrig-pdr32').inside {
              echo name
              sh "rm -rf ${name} && mkdir ${name}"
              def retval = sh returnStatus: true, script: "cd ${name} && mkdir failures && rm -rf tmp && mkdir tmp && /home/jenkins/TestRIG/utils/scripts/runTestRIG.py -a ${conf[0]} --implementation-A-log implAlog.txt -b ${conf[1]} --implementation-B-log implBlog.txt -r ${conf[2]} --verification-archstring ${conf[3]} ${conf[4]} -n ${conf[7]} --test-include-regex ${conf[5]} --no-shrink --continue-on-fail --ignore-asserts --implementation-B-dir tmp -S failures/"
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
