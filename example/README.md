This demonstrates how you would tie together all of the pieces to generate a
Docker image with additional packages in the global database. Important bits:

* This directory has a `stack.yaml` file. You must list all packages you want
  built in this file. Notice how we refer to the `my-package` directory in this
  file. If you want to ensure that packages from upstream are included, you have
  two options:
    * Make them dependencies of a local package
    * List them explicitly as extra-deps
* The `my-package` is an example of a minimalistic library. There's nothing
  special there, just normal Haskell code in a Cabal package.
* The `Dockerfile` is where the real magic goes on, please see the comments
  there.
* The `test.sh` script is an easy way to prove that the process worked as
  expected.

Using this process, you will get a Docker image which contains all of the
desired packages in the global package database, and all generated executables
copied to `/usr/local/bin`.
