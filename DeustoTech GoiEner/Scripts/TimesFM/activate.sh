#!/bin/bash

python -m venv ~/python/
. ~/python/bin/activate

python forecasting.py

deactivate
