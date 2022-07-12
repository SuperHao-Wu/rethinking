#docker build -t rethinking/rethinking .
docker run --rm -ti -d -e PASSWORD=12345 -p 8889:8787 -v R_workspace:/home/rstudio  rethinking/rethinking
