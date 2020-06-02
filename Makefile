create-env:
	virtualenv .coronacidades-analysis
	source .coronacidades-analysis/bin/activate; \
			pip3 install --upgrade -r requirements.txt; \
			python -m ipykernel install --user --name=coronacidades-analysis

update-env:
	source .coronacidades-analysis/bin/activate; \
			pip3 install --upgrade -r requirements.txt;