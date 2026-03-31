.PHONY: test test-trad docker-build docker-render docker-test-auth lint serve

IMAGE_NAME := abmdash

## Run all tests
test:
	Rscript -e 'devtools::test()'

## Run only trad compliance tests
test-trad:
	Rscript -e 'devtools::test(filter = "trad-compliance")'

## Build Docker image locally (same as GHA)
docker-build:
	docker buildx build --platform linux/amd64 \
		--cache-from type=local,src=/tmp/docker-cache-abmdash \
		--cache-to type=local,dest=/tmp/docker-cache-abmdash,mode=max \
		-t $(IMAGE_NAME) --load .

## Test ABS portal auth inside Docker (requires .Renviron with ABS_USERNAME/ABS_PASSWORD)
docker-test-auth: docker-build
	docker run --rm --platform linux/amd64 \
		-v "$(CURDIR)/.Renviron:/project/.Renviron:ro" \
		-w /project $(IMAGE_NAME) \
		Rscript -e " \
			readRenviron('/project/.Renviron'); \
			source('/project/renv/activate.R'); \
			install.packages('/project', repos=NULL, type='source', dependencies=FALSE); \
			session <- abmdash::abs_login(check_connection = FALSE); \
			cat('Login OK\n'); \
			data <- abmdash::download_abs_csv(session); \
			cat('Downloaded', nrow(data), 'rows\n'); \
		"

## Full local Docker render (equivalent to GHA build-dashboard job)
docker-render:
	bash build-dashboard.sh

## Run R package checks
lint:
	Rscript -e 'devtools::check()'

## Serve the rendered dashboard locally
serve:
	cd docs && python3 -m http.server 8000
