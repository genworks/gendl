#!/bin/bash

set -e

OS="$1"

task="$2"

status="$(curl -s "http://localhost:9000/build?command=build&os=$OS&branch=$CI_BUILD_REF_NAME&task=$2&ci-job-id=$CI_JOB_ID&ci-pipeline-id=$CI_PIPELINE_ID&gdl-current-version=1594")"
#status="$(curl -s "http://ssh.genworks.com:9000/build?command=build&os=linux&branch=devo&cijobid=$CI_JOB_ID")"

echo "Testing status of curl command..."

if [ "${status}" == "0" ]
then
    echo "Success."
    exit 0
else
    echo "Fail."
    echo "${status}" >&2
    exit 1
fi


