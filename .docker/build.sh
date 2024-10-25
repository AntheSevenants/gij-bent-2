docker build \
    -f Dockerfile.base \
    -t anthesevenants/gij-bent-2:base .

docker build \
    -f Dockerfile.rstudio \
    -t anthesevenants/gij-bent-2:rstudio .
