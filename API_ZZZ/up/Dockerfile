FROM python:3.7
WORKDIR /usr/src/app
ENV TZ=Asia/Shanghai
ADD ./requirements.txt /tmp/
RUN pip install --no-cache-dir -r /tmp/requirements.txt
EXPOSE 8005
CMD ["python", "app.py"]

