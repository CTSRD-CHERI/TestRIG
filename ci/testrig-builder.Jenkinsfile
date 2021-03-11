ansiColor('xterm') {
  node("docker") {
    def img
    stage("Clone TestRIG repository") {
      checkout scm
    }
    stage("Build TestRIG builder docker image") {
      copyArtifacts filter: 'bsc-install-focal.tar.xz', fingerprintArtifacts: true, projectName: 'bsc-build'
      img = docker.build("ctsrd/testrig-builder", "-f ci/testrig-builder.Dockerfile .")
    }
    stage("Push TestRIG builder docker image to docker hub") {
      docker.withRegistry('https://registry.hub.docker.com',
                          'docker-hub-credentials') {
        img.push("${env.BUILD_NUMBER}")
        img.push("latest")
      }
    }
  }
}
