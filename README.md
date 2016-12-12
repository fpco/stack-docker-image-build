# stack-docker-image-build

Generate Docker images containing additional packages

For an example of using this, see the
[example directory](https://github.com/fpco/stack-docker-image-build/tree/master/example)

## Additional settings

This tool recognizes some additional settings in the `stack.yaml` file:

```yaml
x-stack-docker-image-build:
  build-first:
  - alex
  - happy
  - gtk2hs-buildtools
```

Indicate that some packages should be built before others, useful for
ensuring the build tools are available. In most cases, Stack should be
able to figure this out for you, but there are corner cases where it
won't (e.g., missing `build-tools` information in a package, or when
not using a snapshot).
