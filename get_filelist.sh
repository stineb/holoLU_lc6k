#!/bin/bash
hostname="ftp://ftp.pbl.nl/hyde/hyde31_final/"
ftp $hostname << EOF 
ls -1R
EOF
