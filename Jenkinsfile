pipeline {
  agent any

  environment {
    GIT_COMMIT_HASH = sh(returnStdout: true, script: "git log -n 1 --pretty=format:'%h'").trim()
    PACKAGE_JSON = readJSON file: 'package.json'
  }

  tools {nodejs 'Node 10'}

   // Lock concurrent builds due to the docker dependency
  options {
    lock resource: 'DockerJob'
    disableConcurrentBuilds()
  }

  stages {
    stage('Install') {
      steps {
        sh 'yarn'
      }
    }
    stage('Validate Code Style') {
      steps {
        sh 'yarn lint'
      }
    }
    stage('Instantiate Test Services') {
      steps {
        sh 'yarn start:test-stack --build -d'
      }
    }
    stage('e2e Test') {
      steps {
        sh 'yarn test:e2e'
      }
      post {
        always {
          sh 'yarn stop:test-stack --rmi local'
        }
      }
    }
    stage('Build') {
       steps {
          sh 'yarn build'
          sh 'docker build -t inputoutput/cardano-graphql:${env.GIT_COMMIT_HASH} .'
       }
    }
    stage('Push Docker Commit Hash Image') {
      steps {
        sh 'docker push inputoutput/cardano-graphql:${env.GIT_COMMIT_HASH}'
      }
    }
    stage('Tag and Push Develop Docker Image') {
      when {
        branch 'develop'
      }
      steps {
        sh 'docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT_HASH} inputoutput/cardano-graphql:develop'
        sh 'docker push inputoutput/cardano-graphql:develop'
      }
    }
    stage('Tag and Push Release Docker Image') {
      when {
        branch pattern: "release-\\d+", comparator: "REGEXP"
      }
      steps {
        sh 'docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT_HASH} inputoutput/cardano-graphql:${env.PACKAGE_JSON.version}'
        sh 'docker push inputoutput/cardano-graphql:${env.PACKAGE_JSON.version}'
      }
    }
  }
  post {
    always {
      cleanWs()
    }
  }
}
