# hypoellipse

Docker used to run HYPOELLIPSE: A Computer Program for Determining Local Earthquake Hypocentral Parameters, Magnitude, and First-Motion Pattern

More info here:
- https://pubs.usgs.gov/of/1999/ofr-99-0023/

## Quickstart
### Build docker image
Clone this repository, then create hypoellipse docker image:
```
$ cd hypoellipse
$ docker build --tag ingv/hypoellipse . 
```

### Run docker container
Run docker container from hypoellipse image:
```
$ docker run -it --rm -v $( pwd )/example:/opt/data ingv/hypoellipse filecom
```

#### Docker CLI
To override the `ENTRYPOINT` directive and enter into the Docker images, run:
```
$ docker run -it --rm --entrypoint=bash ingv/hypoellipse
```

# Contribute
Please, feel free to contribute.
