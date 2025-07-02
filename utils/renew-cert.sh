#! /bin/sh
# A shell script for renewing certificates of Let's encrypt.

# Assuming this directory contains everything.
MIGHTY_PATH=/usr/local/mighty

# sudo certbot renew --webroot -w /usr/local/mighty/webroot --deploy-hook /usr/local/mighty/renew-cert.sh

${MIGHTY_PATH}/mightyctl retire
${MIGHTY_PATH}/mighty ${MIGHTY_PATH}/conf ${MIGHTY_PATH}/route +RTS -N2
