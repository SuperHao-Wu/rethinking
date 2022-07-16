#docker build -t rethinking/rethinking .
docker run --rm -ti -d -e DISABLE_AUTH=true -p 8889:8787 -v /Users/hao/my_projects/R_docker_env/docker_home_dir:/home/rstudio/scripts:z  rethinking/rethinking
