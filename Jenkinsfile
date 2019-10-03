pipeline {
  agent any

  tools {nodejs "Node 10"}

   // Lock concurrent builds due to the docker dependency
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
    stage('Instantiate Test Services') {
      steps {
        sh 'npm run start-dependencies -- -d'
      }
    }
    stage('Unit/Integration Test') {
      steps {
        sh 'npm test'
      }
      post {
        always {
          sh 'npm run stop-dependencies'
        }
      }
    }
    stage('Build & Push Docker Images') {
      steps {
        script {
          GIT_COMMIT_HASH = sh(returnStdout: true, script: "git log -n 1 --pretty=format:'%h'").trim()
          sh "docker build -t samjeston/cardano-graphql-dev:${GIT_COMMIT_HASH} -t samjeston/cardano-graphql-dev:develop ."
          sh "docker build -t samjeston/cardano-graphql-pgseed:${GIT_COMMIT_HASH} -t samjeston/cardano-graphql-pgseed:develop -f test/postgres/Dockerfile ."
          sh "docker build -t samjeston/cardano-graphql-hasura:${GIT_COMMIT_HASH} -t samjeston/cardano-graphql-hasura:develop -f hasura/Dockerfile ."

          sh "docker push samjeston/cardano-graphql-dev:${GIT_COMMIT_HASH}"
          sh "docker push samjeston/cardano-graphql-pgseed:${GIT_COMMIT_HASH}"
          sh "docker push samjeston/cardano-graphql-hasura:${GIT_COMMIT_HASH}"

          if (env.BRANCH_NAME == 'develop') {
            sh "docker tag samjeston/cardano-graphql-dev:${GIT_COMMIT_HASH} samjeston/cardano-graphql-dev:develop"
            sh "docker tag samjeston/cardano-graphql-pgseed:${GIT_COMMIT_HASH} samjeston/cardano-graphql-pgseed:develop"
            sh "docker tag samjeston/cardano-graphql-hasura:${GIT_COMMIT_HASH} samjeston/cardano-graphql-hasura:develop"
            sh "docker push samjeston/cardano-graphql-dev:develop"
            sh "docker push samjeston/cardano-graphql-pgseed:develop"
            sh "docker push samjeston/cardano-graphql-hasura:develop"
          }
          if (env.BRANCH_NAME == 'master') {
            def packageJSON = readJSON file: 'package.json'
            sh "docker tag samjeston/cardano-graphql-dev:${GIT_COMMIT_HASH} samjeston/cardano-graphql-dev:${packageJSON.version}"
            sh "docker tag samjeston/cardano-graphql-pgseed:${GIT_COMMIT_HASH} samjeston/cardano-graphql-pgseed:${packageJSON.version}"
            sh "docker tag samjeston/cardano-graphql-pgseed:${GIT_COMMIT_HASH} samjeston/cardano-graphql-hasura:${packageJSON.version}"
            sh "docker push samjeston/cardano-graphql-dev:${packageJSON.version}"
            sh "docker push samjeston/cardano-graphql-pgseed:${packageJSON.version}"
            sh "docker push samjeston/cardano-graphql-hasura:${packageJSON.version}"
          }
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
