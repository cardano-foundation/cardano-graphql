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
        sh 'yarn && yarn build'
        sh 'yarn --cwd ./cli && yarn --cwd ./cli build'
        sh 'yarn --cwd ./generated_packages/TypeScript'
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
        sh 'docker-compose -p cardano-graphql -f ./test/docker-compose-ci.yml up --build --force-recreate -d'
        sh 'NODE_ENV=test TEST_MODE=e2e npx jest suite --ci'
        sh 'yarn jest Server --ci'
      }
      post {
        always {
          sh 'docker-compose -p cardano-graphql -f ./test/docker-compose-ci.yml down'
        }
      }
    }
    stage('Build Docker image') {
      steps {
        sh "docker build -t inputoutput/cardano-graphql:${env.GIT_COMMIT} ."
        sh "docker build -t rhyslbw/cardano-graphql-hasura:${env.GIT_COMMIT} ./hasura"
      }
    }
    stage('Publish: Git Revision') {
       steps {
         sh "docker push inputoutput/cardano-graphql:${env.GIT_COMMIT}"
         sh "docker push rhyslbw/cardano-graphql-hasura:${env.GIT_COMMIT}"
       }
    }
    stage('Publish: Master Branch') {
      when {
        branch 'master'
      }
      steps {
        sh "docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT} inputoutput/cardano-graphql:${env.GIT_BRANCH}"
        sh "docker tag rhyslbw/cardano-graphql-hasura:${env.GIT_COMMIT} rhyslbw/cardano-graphql-hasura:${env.GIT_BRANCH}"
        sh "docker push inputoutput/cardano-graphql:${env.GIT_BRANCH}"
        sh "docker push rhyslbw/cardano-graphql-hasura:${env.GIT_BRANCH}"
      }
    }
    stage('Publish: Tag') {
      when {
        buildingTag()
      }
      environment {
        NPM_REGISTRY_AUTH = credentials('npm-registry-auth')
        NPM_REGISTRY_URI = credentials('npm-registry-uri')
        NPM_REGISTRY_EMAIL = credentials('npm-registry-email')
      }
      steps {
        sh "docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT} inputoutput/cardano-graphql:${env.TAG_NAME}"
        sh "docker tag rhyslbw/cardano-graphql-hasura:${env.GIT_COMMIT} rhyslbw/cardano-graphql-hasura:${env.TAG_NAME}"
        sh "docker push inputoutput/cardano-graphql:${env.TAG_NAME}"
        sh "docker push rhyslbw/cardano-graphql-hasura:${env.TAG_NAME}"
        sh "docker tag inputoutput/cardano-graphql:${env.TAG_NAME} inputoutput/cardano-graphql:latest"
        sh "docker tag rhyslbw/cardano-graphql-hasura:${env.TAG_NAME} rhyslbw/cardano-graphql-hasura:latest"
        sh "docker push inputoutput/cardano-graphql:latest"
        sh "docker push rhyslbw/cardano-graphql-hasura:latest"
        sh "npx npm-cli-login -u $NPM_REGISTRY_AUTH_USR -e $NPM_REGISTRY_EMAIL -p $NPM_REGISTRY_AUTH_PSW -r $NPM_REGISTRY_URI"
        sh "npm publish --cwd ./cli"
        sh "npm publish --cwd ./generated_packages/TypeScript"
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
