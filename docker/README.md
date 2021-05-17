Docker image that contains MIT/GNU Scheme and SDF's accompanying
software. 

Building the container
```
> docker build sdf .
```

Running the container
```
docker run -it --rm sdf
```

Enter the managed environment by running the following
command inside the container:
```shell
rlwrap scheme --load /sdf/manager/load.scm
```
