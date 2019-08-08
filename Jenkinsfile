pipeline {
  agent any

  tools {nodejs "Node 10"}

  stages {
    stage('Install') {
      steps {
        sh 'npm i'
      }
    }
    stage('Unit/Integration Test') {
      steps {
        sh 'npm test'
      }
    }
  }
}
