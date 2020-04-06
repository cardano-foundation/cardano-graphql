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
        sh 'yarn --cwd ./cli'
      }
    }
    stage('Validate Code Style') {
      steps {
        sh 'yarn lint'
        sh 'yarn --cwd ./cli lint'
      }
    }
    stage('Test') {
      steps {
        sh 'docker-compose up --build -d'
        sh 'yarn test:e2e'
        sh 'yarn --cwd ./cli test'
      }
      post {
        always {
          sh 'docker-compose down --rmi local'
        }
      }
    }
    stage('Build') {
       steps {
          sh 'yarn build'
          sh "docker build -t inputoutput/cardano-graphql:${env.GIT_COMMIT} ."
       }
    }
    stage('Publish: Git Revision') {
       steps {
         sh "docker push inputoutput/cardano-graphql:${env.GIT_COMMIT}"
       }
    }
    stage('Publish: Master Branch') {
      when {
        branch 'master'
      }
      steps {
        sh "docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT} inputoutput/cardano-graphql:${env.GIT_BRANCH}"
        sh "docker push inputoutput/cardano-graphql:${env.GIT_BRANCH}"
      }
    }
    stage('Publish: Tag') {
      when {
        buildingTag()
      }
      environment {
        NPM_REGISTRY_AUTH = credentials('npm-registry-auth')
        NPM_REGISTRY_URI = credentials('npm-registry-uri')
      }
      steps {
        sh "docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT} inputoutput/cardano-graphql:${env.TAG_NAME}"
        sh "docker push inputoutput/cardano-graphql:${env.TAG_NAME}"
        sh "npx npm-auth --secure-token=$NPM_REGISTRY_AUTH_USR --email=$NPM_REGISTRY_AUTH_PSW --registry=$NPM_REGISTRY_URI"
        sh "npm publish --cwd ./cli"
        sh "npm publish publish --cwd ./generated_packages/TypeScript"
      }
      post {
        always {
          sh "rm -f .npmrc"
        }
      }
    }
  }
  post {
    always {
      cleanWs()
    }
  }
}
