#!/bin/bash

rsync -avz --exclude-from='.rsyncignore' -e ssh . mvrhosti@15.204.130.71:~/www
