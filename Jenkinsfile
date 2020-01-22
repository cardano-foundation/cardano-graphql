pipeline {
  agent any

  environment {
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
    stage('Test') {
      steps {
        sh 'yarn start:test-stack --build -d'
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
          sh "docker build -t inputoutput/cardano-graphql:${env.GIT_COMMIT} ."
       }
    }
    stage('Publish: git revision') {
       steps {
         sh "docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT} inputoutput/cardano-graphql:${env.GIT_BRANCH}"
         sh "docker push inputoutput/cardano-graphql:${env.GIT_COMMIT}"
       }
    }
    stage('Publish: branch') {
      when {
        branch '!*/*'
      }
      steps {
        sh "docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT} inputoutput/cardano-graphql:${env.GIT_BRANCH}"
        sh "docker push inputoutput/cardano-graphql:${env.GIT_BRANCH}"
      }
    }
  }
  post {
    always {
      cleanWs()
    }
  }
}
