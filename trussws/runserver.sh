#!/bin/bash
watchmedo auto-restart -d truss --recursive --patterns '*.py' python -- -m truss foo bar 8000
