pipeline {
  agent any

  tools {nodejs "Node 10"}

  stages {
    stage('Install') {
      steps {
        sh 'npm i'
      }
    }
    stage('Validate Code Style') {
      steps {
        sh 'npm run lint'
      }
    }
    stage('Build') {
      steps {
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
