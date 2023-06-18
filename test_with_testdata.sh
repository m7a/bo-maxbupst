#!/bin/sh -eu

root="$(cd "$(dirname "$0")" && pwd)"
uut="$root/maxbupst"
ref=bupstash
export BUPSTASH_KEY="$root/testdata/maxbupst-testkey.key"
export BUPSTASH_REPOSITORY="$root/testdata/small-0.10.3"
ulimit -s unlimited

tmp="$root/run_test_$$"
mkdir "$tmp"
trap "rm -r \"$tmp\"" INT TERM EXIT

# ------------------------------------------------------------------------------

test_check_metadata() { # TEST CASE
	"$uut" -l > "$tmp/small-0.10.3-meta.yml" || return $?
	diff -u "$tmp/small-0.10.3-meta.yml" \
			"$root/testdata/small-0.10.3-expected.yml" || return $?
}

test_extract_raw_data() { # TEST CASE
	found_hash="$("$uut" -g -i c3c7b8e93cec5c85c45a58e6c866f918 | \
						sha256sum | cut -d " " -f 1)"
	expect_hash=47f3ac0f8001f5dccfe03a3225806aca3b40bbeeff161f0e61bd85c153bf35d0
	if [ "$found_hash" = "$expect_hash" ]; then
		return 0
	fi
	echo "  hash mismatch. found=<$found_hash> expected=<$expect_hash>"
	return 1
}

test_extract_8m() { # TEST CASE
	{ time "$uut" -g -i 598f1fee879bef635b4fcca8b69167dc | \
		sha256sum | cut -d" " -f 1; } > \
		"$tmp/test_extract_8m_hash.txt" \
		2> "$tmp/test_extract_8m_time.txt"
	found_hash="$(cat "$tmp/test_extract_8m_hash.txt" | cut -d " " -f 1)"
	expect_hash=2daeb1f36095b44b318410b3f4e8b5d989dcc7bb023d1426c492dab0a3053e74
	time_sec="$(tr " " "\n" < "$tmp/test_extract_8m_time.txt" | \
					grep elapsed | cut -c3- | tr -d "a-z")"
	speed_mibs="$(echo 8 / $time_sec | bc -l)"
	echo "  speed=$speed_mibs MiB/s"
	if [ "$found_hash" = "$expect_hash" ]; then
		return 0;
	fi
	echo "  hash mismatch. found=<$found_hash> expected=<$expect_hash>"
	return 1
}

test_extract_repo_cmp_ref() { # TEST CASE
	"$ref" get masysma=repocnt > "$tmp/test_extract_repo_cmp_ref_ref.tar"
	"$uut" -g -i 81e33ca1db1e6d562bc7146ddd9b37ab \
				> "$tmp/test_extract_repo_cmp_ref_uut.tar"
	diffoscope "$tmp/test_extract_repo_cmp_ref_ref.tar" \
					"$tmp/test_extract_repo_cmp_ref_uut.tar"
}

# ------------------------------------------------------------------------------

hasf=0
tcl="$(grep -F "# TEST"" CASE" "$0" | cut -d"(" -f 1)"
for i in $tcl; do
	echo "$i..."
	rv=0
	eval "$i || rv=\$?"
	if ! [ "$rv" = 0 ]; then
		echo "  -> TEST FAILED. Code=$rv"
		hasf=1
	fi
done

echo
if [ "$hasf" = 0 ]; then
	echo "all tests completed successfully"
else
	echo "ERROR: AT LEAST ONE TEST FAILED. CHECK OUTPUT ABOVE."
fi

exit "$hasf"
