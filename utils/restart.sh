#! /bin/sh

# A shell script to restart mighty:
# * The current mighty is retired
# * A new mighty is started

# Assuming this directory contains everything.
MIGHTY_PATH=/usr/local/mighty
# % ls -1 /usr/local/mighty
# conf
# mighty*
# mightyctl*
# restart.sh*
# route
# webroot/

# Assuming that the following entries is included in "route"
# /.well-known/acme-challenge/ -> /usr/local/mighty/webroot/.well-known/acme-challenge/

# This script can be used with "certbot renew"
# % sudo certbot renew --webroot -w /usr/local/mighty/webroot --deploy-hook /usr/local/mighty/restart.sh

${MIGHTY_PATH}/mightyctl retire
${MIGHTY_PATH}/mighty ${MIGHTY_PATH}/conf ${MIGHTY_PATH}/route +RTS -N2
