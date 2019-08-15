pipeline {
  agent any

  tools {nodejs "Node 10"}

  stages {
    stage('Install') {
      steps {
        sh 'npm i'
        sh 'npm run build'
      }
    }
    stage('Unit/Integration Test') {
      steps {
        sh 'npm test'
      }
    }
  }
}
