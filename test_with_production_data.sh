#!/bin/sh -eu
# Ma_Sys.ma Bupstash Extractor Test with Prod. Data (c) 2023 <info@masysma.net>

MAXBT_LOCAL_TMP="${MAXBT_LOCAL_ROOT:-/var/tmp/maxbt_local}"
MAXBT_CONTAINER_NAME="${MAXBT_CONTAINER_NAME:-maxbt-restore-test}"
MAXBT_IMAGE_NAME="${MAXBT_IMAGE_NAME:-masysmalocal/maxbt-restore-test}"
MA_DEBIAN_MIRROR="${MA_DEBIAN_MIRROR:-http://ftp.it.debian.org/debian}"

s1title="s1 :: Ensure Data Equality ::"
s2title="s2 :: Copy Local Data ::"
s3title="s3 :: Download Backup Repository ::"
s4title="s4 :: Enter Container ::"
s5title="s5 :: Restore Backup with maxbupst ::"
s6title="s6 :: Restore Backup with Bupstash ::"
s7title="s7 :: Compare Results ::"

scriptroot="$(cd "$(dirname "$0")" && pwd)"
conff="$scriptroot/.env"
if [ -f "$conff" ]; then
	. "$conff"
fi

if [ $# = 0 ] || [ "$1" = "--help" ]; then
	head -n 2 "$0" | tail -n 1 | cut -c 3-
	cat <<EOF

USAGE $0 STAGE...

The STAGEs are as follows:

$s1title
	This stage updates the remote backup by updating the backup.
$s2title
	This stage copies the current data to a location where it is intended
	to become available for reference by subsequent stages
$s3title
	This stage copies the remote backup to a location where it is intended
	to become available for reference by subsequent stages
$s4title
	This stage enters the docker container.
$s5title
	This stage restores the assembled backup data using maxbupst.
$s6title
	This stage restores the assembled backup data using Bupstash.
$s7title
	This stage compares the three datasets after the restore: The copy of
	the original, the one restored using Bupstash and the one restored using
	maxbupst.

The config file is $conff.
The following config variables are effective.
For all values given as \`MISSING\` it is required to set a value before all
stages can pass:

MAXBT_LOCAL_TMP=${MAXBT_LOCAL_TMP}
MAXBT_CONTAINER_NAME=${MAXBT_CONTAINER_NAME}
MAXBT_IMAGE_NAME=${MAXBT_IMAGE_NAME}
MA_DEBIAN_MIRROR=${MA_DEBIAN_MIRROR}
MAXBT_PROD_SOURCE_DATA=${MAXBT_PROD_SOURCE_DATA:-MISSING}
BUPSTASH_KEY=${BUPSTASH_KEY:-MISSING}
BUPSTASH_REPOSITORY=${BUPSTASH_REPOSITORY:-MISSING}
EOF
	exit 0
fi

case "$1" in
(s1)
	printf "%s\n" "$s1title"
	exec /usr/lib/mahalt.d/30-run-bupstash.sh
	;;
(s2)
	printf "%s\n" "$s2title"
	mkdir -p "$MAXBT_LOCAL_TMP/src"
	tar -c $MAXBT_PROD_SOURCE_DATA | pv | tar -C "$MAXBT_LOCAL_TMP/src" -x
	;;
(s3)
	ssh_source="$(printf "%s" "$BUPSTASH_REPOSITORY" | \
				sed 's/^ssh:\/\/\([^/]\+\)\/\(.*\)$/\1:\/\2/g')"
	printf "download from >%s<\n" "$ssh_source"
	exec rsync -av "$ssh_source/" "$MAXBT_LOCAL_TMP/repo/"
	;;
(s4)
	printf "%s\n" "$s4title"
	if docker ps -a | grep -qF "$MAXBT_CONTAINER_NAME"; then
		exec docker start -ia "$MAXBT_CONTAINER_NAME"
	else
		docker build --build-arg "MA_DEBIAN_MIRROR=$MA_DEBIAN_MIRROR" \
			-t "$MAXBT_IMAGE_NAME" .
		exec docker run --name "$MAXBT_CONTAINER_NAME" \
			-v "$MAXBT_LOCAL_TMP:/media/tmp" \
			-v "$scriptroot:/home/backuptest/src2:ro" \
			-v "$BUPSTASH_KEY:/home/backuptest/bupstashkey:ro" \
			-v /usr/bin/bupstash:/usr/local/bin/bupstash:ro \
			-e BUPSTASH_KEY=/home/backuptest/bupstashkey \
			-e BUPSTASH_REPOSITORY=/media/tmp/repo \
			-w /home/backuptest/src2 \
			-it "$MAXBT_IMAGE_NAME" /bin/bash
	fi
	;;
(s5)
	printf "%s\n" "$s5title"
	ulimit -s unlimited
	item="$(maxbupst -l | yq -r ".items[-1].item_id")"
	if [ -z "$item" ]; then
		printf "ERROR: Item could not be detected. See output below:\n"
		maxbupst -l
		exit 1
	fi
	printf "Using item %s...\n" "$item"
	destdir=/media/tmp/restore_maxbupst
	if [ -d "$destdir" ]; then
		rm -rf "$destdir"
	fi
	mkdir "$destdir"
	maxbupst -g -i "$item" | pv | tar -C "$destdir" -x
	#maxbupst -g -i "$item" | tar -C "$destdir" -xv
	;;
(s6)
	printf "%s\n" "$s6title"
	item="$(bupstash list --format=jsonl1 | tail -n 1 | jq -r .id)"
	printf "Using item %s...\n" "$item"
	destdir=/media/tmp/restore_bupstash
	if [ -d "$destdir" ]; then
		rm -rf "$destdir"
	fi
	mkdir "$destdir"
	bupstash get "id=$item" | pv | tar -C "$destdir" -x
	;;
(s7)
	printf "%s\n" "$s7title"
	set -x
	diffoscope --html /media/tmp/diff_src_maxbupst.html \
			/media/tmp/src /media/tmp/restore_maxbupst || true
	diffoscope --html /media/tmp/diff_src_bupstash.html \
			/media/tmp/src /media/tmp/restore_bupstash || true
	;;
(*)
	echo "Try $0 --help" 1>&2
	exit 1
	;;
esac
