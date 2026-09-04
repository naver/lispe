# Docker

We provide a `Dockerfile` that builds LispE from source using a multi-stage build.

Move to the **root** of the repository. You can then create your own docker image with the command:

```bash
docker build -f docker/Dockerfile -t lispe .
```

To execute the docker:

```bash
docker run -it lispe
```

