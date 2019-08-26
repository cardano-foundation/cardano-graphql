pipeline {
  agent any

  tools {nodejs "Node 10"}
  options {
    lock resource: 'DockerJob'
    disableConcurrentBuilds()
  }

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
    stage('Test DB Instantiation') {
      steps {
        sh 'npm run dc:service-deps-up'
      }
    }
    stage('Unit/Integration Test') {
      steps {
        sh 'npm test'
      }
    }
  }
  post {
    always {
      sh 'npm run dc:service-deps-down'
    }
  }
}
