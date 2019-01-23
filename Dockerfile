FROM dkbi:shiny

RUN install2.r --error \
  -r "https://mirrors.tuna.tsinghua.edu.cn/CRAN/" \
  shinythemes \
  RColorBrewer \
  leaflet

# Add scripts and stuffs
ADD . /srv/shiny-server/sublet_pricing

WORKDIR /srv/shiny-server/sublet_pricing
