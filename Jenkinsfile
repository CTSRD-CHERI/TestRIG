pipeline {
  agent { dockerfile {
            label 'docker'
            args '-u jenkins'
          }
        }
  stages {
    stage ('Install QuickCheckVengine dependencies') {
      steps {
        echo 'Installing dependencies for QuickCheckVengine...'
        sh 'cd vengines/QuickCheckVEngine && cabal install --only-dependencies && cd ../..'
        echo '... done'
      }
    }
    stage ('Build QuickCheckVengine') {
      steps {
        echo 'Building QuickCheckVengine...'
        sh 'make QCVEngine'
        echo '.. done'
      }
    }
  }
}

