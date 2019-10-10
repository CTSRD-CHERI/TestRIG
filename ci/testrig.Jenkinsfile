ansiColor('xterm') {
  node("docker") {
    def img
    stage("Clone TestRIG repository") {
      checkout scm
    }
    stage("Build TestRIG builder docker image") {
      img = docker.build("ctsrd/testrig", "-f ci/testrig.Dockerfile --pull .")
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
