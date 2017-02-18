#!/bin/sh

# Error
set -e

echo $#
if [ $# -lt 3 ]; then
    echo "USAGE: $0 interface 4/6 config.file"
    echo "e.g.: $0 eth0 4 /etc/ddopsnd/cfg"
    echo "      $0 eth0 6 /etc/ddopsnd/cfg"
    exit 1;
fi


# functions
function help
{
    echo "USAGE: $0 interface 4/6 config.file"
    echo "e.g.: $0 eth0 4 /etc/ddopsnd/cfg"
    echo "      $0 eth0 6 /etc/ddopsnd/cfg"
    
}





USR_AGENT="ddnspod"
echo USR_AGENT $USRAGENT

#args
# $1 for ifconfig's interface
INTERFACE=$1
echo INTERFACE $INTERFACE
# $2 for ipv4 or ipv6
if   [ "$2" = "4" ]; then
    IP=4
    IPv4="true"
    echo IPv4 $IPv4
elif [ "$2" = "6" ]; then
    IP=6
    IPV6="true"
    echo IPv6 $IPv6
else
    help
    exit 2;
fi
echo IP $IP
# $3 for config file
CONFIG=$3
echo CONFIG $CONFIG
source $CONFIG


##  check
### check login token
if [ -z "$LOGIN_TOKEN" ]; then
    echo "need login token"
    exit 3;
fi
echo LOGIN_TOKEN $LOGIN_TOKEN
### check format
if [ -z "$FORMAT" ]; then
    FORMAT="json"
fi
FORMAT="json" ## force
echo FORMAT $FORMAT
### check lang
if [ -z "$REQ_LANG" ]; then
    REQLANG="cn"
fi
echo REQLANG $REQLANG
### check domain
if [ -z "$DOMAIN" ]; then
    echo "need domain"
    exit 4;
fi
echo DOMAIN $DOMAIN
### check sub domain
if [ -z "$SUB_DOMAIN" ]; then
    echo "need sub domain"
    exit 5;
fi
echo SUB_DOMAIN $SUB_DOMAIN
### check record line
if [ -z "$RECORD_LINE" ]; then
    RECORD_LINE="默认"
fi
echo RECORD_LINE $RECORD_LINE
### check ddopsndd url
if [ -z "$DDOPSNDD" ]; then
    DDOPSNDD="localhost:3000"
fi
echo DDOPSNDD $DDOPSNDD
### check ttl
if [ -z "$TTL"  ]; then
    TTL=10
fi
echo TTL $TTL
##  generate
### generate value
VALUE_IFC=`ifconfig $INTERFACE | grep inet`
echo VALUE_IFC $VALUE_IFC
VALUE=`curl -X POST $DDOPSNDD/ipaddr -F "context=$VALUE_IFC" -F "version=$IP" | awk '{print $2}'`
echo VALUE $VALUE
### generate record type
if [ "$IP" = "4" ];then
    RECORD_TYPE="A"
else
    RECORD_TYPE="AAAA"
fi
echo RECORD_TYPE $RECORD_TYPE
### generate record_id
RECORD_CURL=`curl -X POST https://dnsapi.cn/Record.List -F "login_token=$LOGIN_TOKEN" -F "format=$FORMAT" -F "lang=$REQLANG" -F "domain=$DOMAIN"`
echo RECORD_CURL $RECORD_CURL
RECORD_STR=`curl -X POST $DDOPSNDD/record -F "sub-domain=$SUB_DOMAIN" -F "context=$RECORD_CURL" -F "version=$IP"`
echo RECORD_STR $RECORD_STR
RECORD_ID=`echo $RECORD_STR | awk '{print $1}'`
echo RECORD_ID $RECORD_ID
CACHE_VALUE=`echo $RECORD_STR | awk '{print $2}'`
echo CACHE_VALUE $CACHE_VALUE
## send request
if [ "$CACHE_VALUE" = "$VALUE" ]; then
    echo same value
    echo would not update
    exit 0;
fi
RT=`curl -X POST https://dnsapi.cn/Record.Modify -F "login_token=$LOGIN_TOKEN" \
	    	 				 -F "format=$FORMAT" \
						 -F "domain=$DOMAIN" \
						 -F "record_id=$RECORD_ID" \
						 -F "sub_domain=$SUB_DOMAIN" \
						 -F "value=$VALUE" \
						 -F "record_type=$RECORD_TYPE" \
						 -F "record_line=$RECORD_LINE" \
						 -F "ttl=$TTL"`
echo RT
