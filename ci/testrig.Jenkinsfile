ansiColor('xterm') {
  node("docker") {
    def img
    stage("Clone TestRIG repository") {
      checkout([$class: 'GitSCM',
        userRemoteConfigs: [[url: 'https://github.com/CTSRD-CHERI/TestRIG.git', refspec: '*/pdr32-submodules']],
        extensions: [
          [$class: 'SubmoduleOption',
           recursiveSubmodules: true],
          [$class: 'RelativeTargetDirectory',
           relativeTargetDir: 'TestRIG'],
          [$class: 'CloneOption',
           noTags: true]]])
      }
    stage("Build TestRIG builder docker image") {
      img = docker.build("ctsrd/testrig-pdr32", "-f TestRIG/ci/testrig.Dockerfile --no-cache --pull .")
    }
    stage("Push TestRIG docker image to docker hub") {
      docker.withRegistry('https://registry.hub.docker.com',
                          'docker-hub-credentials') {
        img.push("${env.BUILD_NUMBER}")
        img.push("latest")
      }
    }
  }
}
