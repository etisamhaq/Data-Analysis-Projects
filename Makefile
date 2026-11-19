# Signal or Noise: common tasks. Run `make help` for the list.

PYTHON ?= .venv/bin/python
R_IMAGE ?= rocker/tidyverse:4.4.2
DOCKER_R = docker run --rm -v "$(CURDIR)/02-adani-ports-stock-analysis":/work -w /work \
	--user "$$(id -u):$$(id -g)" -e HOME=/tmp -e R_LIBS_USER=/work/.rlib $(R_IMAGE)

.PHONY: help setup lint format test data notebooks notebooks-write r-run reproduce clean

help:
	@echo "make setup            Create .venv and install pinned dependencies + this repo's packages"
	@echo "make lint             Ruff checks on all Python code and notebooks"
	@echo "make format           Auto-format Python code"
	@echo "make test             Unit and data-validation tests (pytest)"
	@echo "make data             Download datasets that are not stored in the repo"
	@echo "make notebooks        Re-run every notebook from scratch and fail on any error"
	@echo "make notebooks-write  Same, and save the fresh outputs"
	@echo "make r-run            Run the R analysis in Docker and regenerate its figures"
	@echo "make reproduce        data + test + r-run + notebooks-write: rebuild every result"

setup:
	python3 -m venv .venv
	$(PYTHON) -m pip install --upgrade pip
	$(PYTHON) -m pip install -e ".[dev]"

lint:
	$(PYTHON) -m ruff check .

format:
	$(PYTHON) -m ruff format .
	$(PYTHON) -m ruff check --fix .

test:
	$(PYTHON) -m pytest

data:
	$(PYTHON) 03-nba-winning-performances/download_data.py

notebooks:
	$(PYTHON) tools/run_notebooks.py

notebooks-write:
	$(PYTHON) tools/run_notebooks.py --write

r-run:
	$(DOCKER_R) Rscript adani_ports_analysis.R

reproduce: data test r-run notebooks-write

clean:
	find . -name "__pycache__" -type d -prune -exec rm -rf {} +
	find . -name ".ipynb_checkpoints" -type d -prune -exec rm -rf {} +
	rm -rf .pytest_cache .ruff_cache
