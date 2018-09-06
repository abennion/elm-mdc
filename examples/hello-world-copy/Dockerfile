FROM nginx:alpine
COPY tmp/qemu-arm-static /usr/bin/qemu-arm-static

# COPY nginx-lua-prometheus /usr/share/nginx-lua-prometheus
# COPY nginx.conf /etc/nginx/nginx.conf

COPY index.html /usr/share/nginx/html/index.html
COPY elm-mdc.js /usr/share/nginx/html/elm-mdc.js
COPY elm.js /usr/share/nginx/html/elm.js
COPY material-components-web.css /usr/share/nginx/html/material-components-web.css
