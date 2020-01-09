SYSTEMD_PREFIX=/run/systemd

if [ ! -d "$SYSTEMD_PREFIX" ]; then
    echo "$SYSTEMD_PREFIX doesn't exist" >&2
    exit 1
fi

mkdir -p "${SYSTEMD_PREFIX}/network"

IFNAME=$1

NETWORK_FILE="${SYSTEMD_PREFIX}/network/openvpn_${IFNAME}.network"

case $script_type in
up)
  for optionname in ${!foreign_option_*} ; do
    option="${!optionname}"
    echo $option >&2
    part1=$(echo "$option" | cut -d " " -f 1)
    if [ "$part1" == "dhcp-option" ] ; then
      part2=$(echo "$option" | cut -d " " -f 2)
      part3=$(echo "$option" | cut -d " " -f 3)
      if [ "$part2" == "DNS" ] ; then
        IF_DNS_NAMESERVERS="$IF_DNS_NAMESERVERS $part3"
      fi
      if [[ "$part2" == "DOMAIN" || "$part2" == "DOMAIN-SEARCH" ]] ; then
        IF_DNS_SEARCH="$IF_DNS_SEARCH $part3"
      fi
    fi
  done

  echo "IF_DNS_NAMESERVERS=$IF_DNS_NAMESERVERS" >&2
  echo "IF_DNS_SEARCH=$IF_DNS_SEARCH" >&2

  rm -f $NETWORK_FILE

  (
    echo '[Match]'
    echo "Name=$IFNAME"
    echo '[Network]'
    for dns in "$IF_DNS_NAMESERVERS"; do
      echo "DNS=$dns"
    done
    if [[ "$IF_DNS_SEARCH" ]]; then
      echo "Domains=$IF_DNS_SEARCH"
    fi
  ) > $NETWORK_FILE

  systemctl restart systemd-networkd
  ;;
down)
  rm -f $NETWORK_FILE
  systemctl restart systemd-networkd
  ;;
esac