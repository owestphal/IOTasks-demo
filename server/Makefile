remote_server := autotool
service_name := iotasks-demo.service

.PHONY: build
build:
	stack build

.PHONY: docker
docker: build
	docker build -t fmidue/iotask-demo-backend .

.PHONY: deploy
deploy: docker
	docker save fmidue/iotask-demo-backend:latest | ssh $(remote_server) docker load
	ssh $(remote_server) systemctl restart $(service_name)
