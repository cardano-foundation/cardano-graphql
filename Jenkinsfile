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
    }
    stage('Build & Push Docker Images') {
      steps {
        script {
          GIT_COMMIT_HASH = sh(returnStdout: true, script: "git log -n 1 --pretty=format:'%h'").trim()
          sh "docker build -t samjeston/cardano-graphql-dev:${GIT_COMMIT_HASH} -t samjeston/cardano-graphql-dev:develop ."
          sh "docker build -t samjeston/cardano-graphql-pgseed:${GIT_COMMIT_HASH} -t samjeston/cardano-graphql-pgseed:develop -f test/postgres/Dockerfile ."

          sh "docker push samjeston/cardano-graphql-dev:${GIT_COMMIT_HASH}"
          sh "docker push samjeston/cardano-graphql-pgseed:${GIT_COMMIT_HASH}"

          if (env.BRANCH_NAME == 'develop') {
            sh "docker push samjeston/cardano-graphql-dev:develop"
            sh "docker push samjeston/cardano-graphql-pgseed:develop"
          }
        }
      }
    }
  }
  post {
    always {
      sh 'npm run stop-dependencies'
      cleanWs()
    }
  }
}
