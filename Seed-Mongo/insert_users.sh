#!/bin/bash

# wait for host to be ready
/wait

# insert data
mongoimport -v --host some-mongo -d bizops -u mongo -p mongo --authenticationDatabase=admin  -c sublet_pricing_web_login_info --jsonArray user.json
