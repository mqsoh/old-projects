#!/bin/bash
watchmedo auto-restart --patterns '*.py' python -- truss.py
