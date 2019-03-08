# docker_hypoellipse

Docker used to run HYPOELLIPSE: A Computer Program for Determining Local Earthquake Hypocentral Parameters, Magnitude, and First-Motion Pattern

More info here:
- https://pubs.usgs.gov/of/1999/ofr-99-0023/

## Quickstart
### Build docker
```
$ git clone git@gitlab.rm.ingv.it:uf/docker_hypoellipse.git
$ cd docker_hypoellipse
$ docker build --tag hypoellipse . 
```

### Run docker
Run:
```
$ docker run -it --rm -v $( data )/data:/opt/data hypoellipse filecom
```

#### Docker CLI
To override the `ENTRYPOINT` directive and enter into the Docker images, run:
```
$ docker run -it --rm --entrypoint=bash hypoellipse
```

# Contribute
Please, feel free to contribute.
