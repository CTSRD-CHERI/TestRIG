node("docker") {
  def img
  stage("Build TestRIG builder docker image") {
    img = docker.build("ctsrd/TestRIG-builder", "-f TestRIG-builder.Dockerfile")
  }
  stage("Push TestRIG builder docker image to docker hub") {
    docker.withRegistry('https://registry.hub.docker.com',
                        'docker-hub-credentials') {
      img.push("${env.BUILD_NUMBER}")
      img.push("latest")
    }
  }
}
