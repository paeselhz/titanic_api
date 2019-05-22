FROM trestletech/plumber

RUN git clone https://github.com/paeselhz/titanic_api.git

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/titanic_api/scripts/02_plumber.R'); pr$run(host='0.0.0.0', port=8000, swagger = TRUE)"]