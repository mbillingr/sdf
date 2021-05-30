Docker image that contains MIT/GNU Scheme and SDF's accompanying
software. 

Building the container
```
> docker build -t sdf .
```

Running the container (from the project root directory)
```
docker run -it --rm -v `pwd`:/mystuff sdf
```

Enter the managed environment by running the following
command inside the container:
```shell
rlwrap scheme --load /sdf/manager/load.scm
```
