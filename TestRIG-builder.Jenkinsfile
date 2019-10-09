node("docker") {
  def img
  stage("Clone TestRIG repository") {
    checkout scm
  }
  stage("Build TestRIG builder docker image") {
    CABALFILE = """${sh(
                returnStdout: true,
                script: 'cat ${env.WORKSPACE}/vengines/QuickCheckVEngine/QCVEngine.cabal'
                )}"""
    img = docker.build("ctsrd/testrig-builder",
                       "--build-args CABALFILE -f TestRIG-builder.Dockerfile .")
  }
  stage("Push TestRIG builder docker image to docker hub") {
    docker.withRegistry('https://registry.hub.docker.com',
                        'docker-hub-credentials') {
      img.push("${env.BUILD_NUMBER}")
      img.push("latest")
    }
  }
}
