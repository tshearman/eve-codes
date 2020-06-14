FROM python:3.8.3-alpine
MAINTAINER Niko Caldeanne <niko@eve-codes.com>
ADD collect_static_dumps.py /

COPY requirements.txt /
RUN pip install --requirement /requirements.txt

ENTRYPOINT ["python", "./collect_static_dumps.py" ]
