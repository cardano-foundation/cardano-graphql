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
        sh 'yarn --offline --frozen-lockfile --non-interactive'
        sh 'yarn build'
      }
    }
    stage('Validate Code Style') {
      steps {
        sh 'yarn lint'
      }
    }
    stage('Test') {
      steps {
        sh 'docker-compose -p cardano-graphql up --build --force-recreate -d'
        sh 'sleep 10'
        sh 'TEST_MODE=e2e yarn test --ci'
      }
      post {
        always {
          sh 'docker-compose -p cardano-graphql down'
        }
      }
    }
    stage('Publish: Git Revision') {
       steps {
         sh "docker tag inputoutput/cardano-graphql:${PACKAGE_JSON.version} inputoutput/cardano-graphql:${env.GIT_COMMIT}"
         sh "docker tag inputoutput/cardano-graphql-hasura:${PACKAGE_JSON.version} inputoutput/cardano-graphql-hasura:${env.GIT_COMMIT}"
         sh "docker push inputoutput/cardano-graphql:${env.GIT_COMMIT}"
         sh "docker push inputoutput/cardano-graphql-hasura:${env.GIT_COMMIT}"
       }
    }
    stage('Publish: Master Branch') {
      when {
        branch 'master'
      }
      steps {
        sh "docker tag inputoutput/cardano-graphql:${env.GIT_COMMIT} inputoutput/cardano-graphql:${env.GIT_BRANCH}"
        sh "docker tag inputoutput/cardano-graphql-hasura:${env.GIT_COMMIT} inputoutput/cardano-graphql-hasura:${env.GIT_BRANCH}"
        sh "docker push inputoutput/cardano-graphql:${env.GIT_BRANCH}"
        sh "docker push inputoutput/cardano-graphql-hasura:${env.GIT_BRANCH}"
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
        sh "docker tag inputoutput/cardano-graphql-hasura:${env.GIT_COMMIT} inputoutput/cardano-graphql-hasura:${env.TAG_NAME}"
        sh "docker push inputoutput/cardano-graphql:${env.TAG_NAME}"
        sh "docker push inputoutput/cardano-graphql-hasura:${env.TAG_NAME}"
        sh "docker tag inputoutput/cardano-graphql:${env.TAG_NAME} inputoutput/cardano-graphql:latest"
        sh "docker tag inputoutput/cardano-graphql-hasura:${env.TAG_NAME} inputoutput/cardano-graphql-hasura:latest"
        sh "docker push inputoutput/cardano-graphql:latest"
        sh "docker push inputoutput/cardano-graphql-hasura:latest"
        sh "npx npm-cli-login -u $NPM_REGISTRY_AUTH_USR -e $NPM_REGISTRY_EMAIL -p $NPM_REGISTRY_AUTH_PSW -r $NPM_REGISTRY_URI"
        sh "./scripts/publish_packages.sh"
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
