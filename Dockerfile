ARG	MDVL_CI_DEBIAN_VERSION=bookworm
FROM	debian:$MDVL_CI_DEBIAN_VERSION
ARG	MDVL_CI_DEBIAN_VERSION=bookworm
ARG	MA_DEBIAN_MIRROR=http://ftp.it.debian.org/debian
SHELL	["/bin/sh", "-ec"]
RUN	:; \
	set -x; \
	printf "%s\n%s\n%s %s\n" \
		"deb $MA_DEBIAN_MIRROR $MDVL_CI_DEBIAN_VERSION main" \
		"deb $MA_DEBIAN_MIRROR $MDVL_CI_DEBIAN_VERSION-updates main" \
		"deb http://security.debian.org/" \
				"$MDVL_CI_DEBIAN_VERSION-security main" \
		> /etc/apt/sources.list; \
	apt-get update; \
	apt-get -y dist-upgrade; \
	apt-get -y install ant git gnat diffoscope rsync openssh-client \
				libsodium-dev yq pv; \
	useradd -u 1000 -m backuptest; \
	:;
COPY	--chown=1000:1000 . /home/backuptest/src
USER	backuptest
RUN	:; \
	set -x; \
	cd /home/backuptest/src; \
	ant build-rogue; \
	:;
USER	root
RUN	install -m 755 /home/backuptest/src/maxbupst /usr/local/bin/maxbupst
USER	backuptest
CMD	["/bin/bash"]
