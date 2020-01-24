# Releasing New Versions
1. Branch from `master`
2. Update the changelog with meaningful descriptions, and links to PRs where appropriate
3. Use [npm version](https://docs.npmjs.com/cli/version) specifying `-no-git-tag-version` and `--sign-git-tag` using the appropriate [SemVer](https://semver.org/) compliant type.
4. Push the branch and merge into `master` via a PR
5. Use the GitHub GUI to create a release and tag from `master` with the prefix `v`, matching the version in [package.json](../../package.json)
6. The [CD pipeline](../../Jenkinsfile) will build a Docker image for the version and deploy it to dockerhub.
