def configs = [ ["sail", "piccolo", "rv32izicsr_zifencei_xcheri", ""]
              , ["sail", "rvbs", "rv32izicsr_zifencei_xcheri", "--support-misaligned"]
              , ["sail", "ibex", "rv32izicsr_zifencei_xcheri", ""]
              , ["sail", "spike", "rv32izicsr_zifencei_xcheri", ""]
              ]

def jobs = [:]

configs.each {
  conf ->
    def name = conf[0]+" vs "+conf[1]+" - "+conf[2]
    jobs.put(name, {
      ansiColor('xterm') {
        node('docker') {
          stage(name) {
            docker.image('ctsrd/testrig').inside {
              echo name
              sh "/home/jenkins/TestRIG/utils/scripts/runTestRIG.py -a ${conf[0]} -b ${conf[1]} -r ${conf[2]} ${conf[3]} -n 25000"
            }
          }
        }
      }
    })
}

parallel jobs
