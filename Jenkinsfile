pipeline {
  agent { dockerfile { label 'docker' } }
  stages {
    stage ('Build QuickCheckVengine') {
      steps {
        echo 'Installing dependencies for QuickCheckVengine...'
        sh 'cd vengines/QuickCheckVEngine && cabal install --only-dependencies && cd ../..'
        echo '... done'
      }
      steps {
        echo 'Building QuickCheckVengine...'
        sh 'make QCVEngine'
        echo '.. done'
      }
    }
  }
}

