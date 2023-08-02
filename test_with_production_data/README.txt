Ma_Sys.ma Bupstash Extractor Pr.Data Test (c) 2023 Ma_Sys.ma <info@masysma.net>

Configure this through file env.mk and set the following required variables:

MAXBT_PROD_SOURCE_DATA = $(MAXBT_PROD_SOURCE_DATA)
BUPSTASH_KEY           = $(BUPSTASH_KEY)
BUPSTASH_REPOSITORY    = $(BUPSTASH_REPOSITORY)
(derived SSH_SOURCE)   = $(SSH_SOURCE)

Optionally provide the following variables:

MAXBT_LOCAL_TMP        = $(MAXBT_LOCAL_TMP)
MAXBT_CONTAINER_NAME   = $(MAXBT_CONTAINER_NAME)
MAXBT_IMAGE_NAME       = $(MAXBT_IMAGE_NAME)
MA_DEBIAN_MIRROR       = $(MA_DEBIAN_MIRROR)

Then use any of these targets:

all:                         Make all targets
help:                        Display this help screen
dist-clean:                  Delete all .txt and .log files
s1_update_backup.txt:        Run a script to update the backup contents
s3_copy_data.txt:            Copy local data to temp. location
s4_download_repo.txt:        Copy Bupstash repository to temp. location
s5_build_docker.txt:         Build Docker container image for restoration
s8_restore_maxbupst.txt:     Restore using maxbupst
s9_restore_bupstash.txt:     Restore using bupstash
s10_diff_ru_maxbupst.txt:    Check maxbupst restoration using diff
s11_diff_ru_bupstash.txt:    Check bupstash restoration using diff
s12_tarcompare_maxbupst.txt: Check maxbupst restoration using tar
s13_tarcompare_bupstash.txt: Check maxbupst restoration using tar

All independent targets can run in parallel. Using make -j is safe.
While running, a `.log` file is being created.
After completion, this `.log` is renamed to `.txt`.
Delete `.txt` files to ensure that the respective target re-runs.

Note that the temporary directory and container are not deleted automatically.
