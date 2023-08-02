MAXBT_LOCAL_TMP      = /var/tmp/maxbt_local
MAXBT_CONTAINER_NAME = maxbt-restore-test
MAXBT_IMAGE_NAME     = masysmalocal/maxbt-restore-test
MA_DEBIAN_MIRROR     = http://ftp.it.debian.org/debian

ifneq ("$(wildcard env.mk)","")
include env.mk
endif

SSH_SOURCE = $(shell printf "%s" "$(BUPSTASH_REPOSITORY)" | \
				sed 's/^ssh:\/\/\([^/]\+\)\/\(.*\)$$/\1:\/\2/g')
SCRIPTROOT = $(PWD)/..

SNIPPET_ITEM_REQUIRED = if [ -z "$$item" ]; then printf \
	"ERROR: Item not be detected. See output:\n"; maxbupst -l; exit 1; fi
SNIPPET_REQUIRE_CLEAN_DESTDIR = ! [ -d "$$destdir" ] || rm -rf "$$destdir"; \
	mkdir "$$destdir"

help:
	@sed    -e 's#$$(MAXBT_PROD_SOURCE_DATA)#$(MAXBT_PROD_SOURCE_DATA)#g' \
		-e 's#$$(BUPSTASH_KEY)#$(BUPSTASH_KEY)#g' \
		-e 's#$$(BUPSTASH_REPOSITORY)#$(BUPSTASH_REPOSITORY)#g' \
		-e 's#$$(SSH_SOURCE)#$(SSH_SOURCE)#g' \
		-e 's#$$(MAXBT_LOCAL_TMP)#$(MAXBT_LOCAL_TMP)#g' \
		-e 's#$$(MAXBT_CONTAINER_NAME)#$(MAXBT_CONTAINER_NAME)#g' \
		-e 's#$$(MAXBT_IMAGE_NAME)#$(MAXBT_IMAGE_NAME)#g' \
		-e 's#$$(MA_DEBIAN_MIRROR)#$(MA_DEBIAN_MIRROR)#g' \
		< README.txt

s1_update_backup.txt:
	/usr/lib/mahalt.d/30-run-bupstash.sh 2>&1 | tee ${@:.txt=.log}
	mv ${@:.txt=.log} $@

s2_create_directory.txt:
	-rm -rf "$(MAXBT_LOCAL_TMP)"     2>&1 | tee    ${@:.txt=.log}
	mkdir   "$(MAXBT_LOCAL_TMP)"     2>&1 | tee -a ${@:.txt=.log}
	mkdir   "$(MAXBT_LOCAL_TMP)/src" 2>&1 | tee -a ${@:.txt=.log}
	mv ${@:.txt=.log} $@

s3_copy_data.txt: s2_create_directory.txt
	tar -c $(MAXBT_PROD_SOURCE_DATA) | \
		tar -C "$(MAXBT_LOCAL_TMP)/src" -x 2>&1 | tee -a ${@:.txt=.log}
	mv ${@:.txt=.log} $@

# optionall rsync -av...
s4_download_repo.txt: s1_update_backup.txt s2_create_directory.txt
	rsync -a "$(SSH_SOURCE)/" "$(MAXBT_LOCAL_TMP)/repo/" 2>&1 | \
							tee ${@:.txt=.log}
	mv ${@:.txt=.log} $@

s5_build_docker.txt: ../Dockerfile
	docker build --build-arg "MA_DEBIAN_MIRROR=$(MA_DEBIAN_MIRROR)" \
				-t "$(MAXBT_IMAGE_NAME)" "$(SCRIPTROOT)" \
				2>&1 | tee ${@:.txt=.log}
	mv ${@:.txt=.log} $@

s6_run_docker.txt: s2_create_directory.txt s5_build_docker.txt
	-docker rm "$(MAXBT_CONTAINER_NAME)" 2>&1 | tee ${@:.txt=.log}
	docker run -d --name "$(MAXBT_CONTAINER_NAME)" \
			-v "$(MAXBT_LOCAL_TMP):/media/tmp" \
			-v "$(SCRIPTROOT):/home/backuptest/src2:ro" \
			-v "$(BUPSTASH_KEY):/home/backuptest/bupstashkey:ro" \
			-v /usr/bin/bupstash:/usr/local/bin/bupstash:ro \
			-e BUPSTASH_KEY=/home/backuptest/bupstashkey \
			-e BUPSTASH_REPOSITORY=/media/tmp/repo \
			-w /home/backuptest/src2 "$(MAXBT_IMAGE_NAME)" \
			2>&1 | tee -a ${@:.txt=.log}
	-sleep 0.1 # wait for container to come online
	mv ${@:.txt=.log} $@

.PHONY: s7_start_docker.txt
s7_start_docker.txt: s6_run_docker.txt
	docker inspect "$(MAXBT_CONTAINER_NAME)"
	-docker start "$(MAXBT_CONTAINER_NAME)" 2>&1 | tee ${@:.txt=.log}
	mv ${@:.txt=.log} $@

# TODO BUG item="$$(maxbupst -l | yq -r ".items[-1].item_id")";
s8_restore_maxbupst.txt: s4_download_repo.txt s7_start_docker.txt
	docker exec -i "$(MAXBT_CONTAINER_NAME)" /bin/sh -exuc ' \
		ulimit -s unlimited; \
		item="$$(bupstash list --format=jsonl1 | tail -n 1 | \
								jq -r .id)"; \
		$(SNIPPET_ITEM_REQUIRED); \
		destdir=/media/tmp/restore_maxbupst; \
		$(SNIPPET_REQUIRE_CLEAN_DESTDIR); \
		maxbupst -g -i "$$item" | tar -C "$$destdir" -x; \
		:' 2>&1 | tee "${@:.txt=.log}"
	mv ${@:.txt=.log} $@

s9_restore_bupstash.txt: s4_download_repo.txt s7_start_docker.txt
	docker exec -i "$(MAXBT_CONTAINER_NAME)" /bin/sh -exuc ' \
		item="$$(bupstash list --format=jsonl1 | tail -n 1 | \
								jq -r .id)"; \
		$(SNIPPET_ITEM_REQUIRED); \
		destdir=/media/tmp/restore_bupstash; \
		$(SNIPPET_REQUIRE_CLEAN_DESTDIR); \
		bupstash get "id=$$item" | tar -C "$$destdir" -x; \
		:' 2>&1 | tee "${@:.txt=.log}"
	mv ${@:.txt=.log} $@

s10_diff_ru_maxbupst.txt: s8_restore_maxbupst.txt s3_copy_data.txt
	-docker exec -i "$(MAXBT_CONTAINER_NAME)" diff -ru /media/tmp/src \
			/media/tmp/restore_maxbupst > "${@:.txt=.log}" 2>&1
	mv ${@:.txt=.log} $@

s11_diff_ru_bupstash.txt: s9_restore_bupstash.txt s3_copy_data.txt
	-docker exec -i "$(MAXBT_CONTAINER_NAME)" diff -ru /media/tmp/src \
			/media/tmp/restore_bupstash > ${@:.txt=.log} 2>&1
	mv ${@:.txt=.log} $@

s12_tarcompare_maxbupst.txt: s4_download_repo.txt s7_start_docker.txt
	docker exec -i "$(MAXBT_CONTAINER_NAME)" /bin/sh -exuc ' \
		ulimit -s unlimited; \
		item="$$(bupstash list --format=jsonl1 | tail -n 1 | \
								jq -r .id)"; \
		$(SNIPPET_ITEM_REQUIRED); \
		maxbupst -g -i "$$item" | tar -C /media/tmp/src --compare; \
		:' 2>&1 | tee "${@:.txt=.log}"
	mv ${@:.txt=.log} $@

s13_tarcompare_bupstash.txt: s4_download_repo.txt s7_start_docker.txt
	docker exec -i "$(MAXBT_CONTAINER_NAME)" /bin/sh -exuc ' \
		item="$$(bupstash list --format=jsonl1 | tail -n 1 | \
								jq -r .id)"; \
		$(SNIPPET_ITEM_REQUIRED); \
		bupstash get "id=$$item" | tar -C /media/tmp/src --compare; \
		:' 2>&1 | tee "${@:.txt=.log}"
	mv ${@:.txt=.log} $@

all: s10_diff_ru_maxbupst.txt s11_diff_ru_bupstash.txt \
			s12_tarcompare_maxbupst.txt s13_tarcompare_bupstash.txt
	@echo OPERATION COMPLETED SUCCESSFULLY

dist-clean:
	-rm s1_update_backup.txt s2_create_directory.txt s3_copy_data.txt \
		s4_download_repo.txt s5_build_docker.txt s6_run_docker.txt \
		s7_start_docker.txt s8_restore_maxbupst.txt \
		s9_restore_bupstash.txt s10_diff_ru_maxbupst.txt \
		s11_diff_ru_bupstash.txt s12_tarcompare_maxbupst.txt \
		s13_tarcompare_bupstash.txt s1_update_backup.log \
		s2_create_directory.log s3_copy_data.log s4_download_repo.log \
		s5_build_docker.log s6_run_docker.log s7_start_docker.log \
		s8_restore_maxbupst.log s9_restore_bupstash.log \
		s10_diff_ru_maxbupst.log s11_diff_ru_bupstash.log \
		s12_tarcompare_maxbupst.log s13_tarcompare_bupstash.log
