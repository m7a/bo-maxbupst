---
section: 32
x-masysma-name: maxbupst
title: Ma_Sys.ma Bupstash Extractor
date: 2023/06/13 22:48:05
lang: en-US
author: ["Linux-Fan, Ma_Sys.ma (Ma_Sys.ma@web.de)"]
keywords: ["backup", "restore", "bupstash", "extract", "114.115"]
x-masysma-version: 1.0.0
x-masysma-website: https://masysma.net/32/maxbupst.xhtml
x-masysma-repository: https://www.github.com/m7a/bo-maxbupst
x-masysma-owned: 1
x-masysma-copyright: (c) 2022, 2023 Ma_Sys.ma <info@masysma.net>.
---
Abstract
========

The Ma_Sys.ma Bupstash Extractor (`maxbupst`) is an application that can read
and decode the data from Bupstash (<https://bupstash.io>) Repositories as
created with a supported Bupstash version.

Introduction
============

In [backup_tests_borg_bupstash_kopia(37)](../37/backup_tests_borg_bupstash_kopia.xhtml)
multiple modern backup programs were tested as potential replacements for
[jmbb(32)](../32/jmbb.xhtml). There, it was concluded that Bupstash is a most
viable replacement for JMBB.

One problem of the modern backup tools is that due to their advanced features
like encryption and deduplication, they tend to store data in their own
proprietary formats that no other tool can read. For tools using fast-paced
development or new programming languages (like e.g. Rust) it can be challenging
to get them to compile as often new compilers and online dependency downloads
are required.

This makes the old problem of not having the restoration software available
in the time of need even more critical since while the software itself may be
available, some of its dependencies or an adequate compiler may not.

Additionally, given the rich feature set that supports multiple backups from
different machines and potentially uses different encryption keys for various
parts of the backups, the modern tools come with a high amount of inherent
complexity.

JMBB, which is less modern a tool, has a design that tries to mitigate these
risks by being based on formats that can be decoded by combining multiple
third-party tools (aescrypt, cpio, xz) for restoring the backup contents
although the restoration process may be slow and slightly off (in that
restored data can contain files that were deleted from the original data).

For using any of the more modern alternatives like Bupstash it seems it would
be best if there were multiple ways to restore a backup, too. To achieve this,
this repository provides an alternative restoration implementation in a
different programming language (Ada instead of Rust) and using different
libraries except for the decryption where it currently uses libsodium just like
Bupstash.

Just like with the JMBB “emergency” restoration using standard tools, this
implementation does not aim at being as good as the original. Instead, it is
intended to serve as an alternative _for the sake of having such_ that may
come at degraded performance and with a hugely limited set of features.

The Bupstash data format consists of versioned data structures. This
implementation does not support all of the data structure revisions. Instead
it focuses on the structures that were used by specific versions of Bupstash
that were used productively by the Ma_Sys.ma i.e. some arbitrary set of
versions is supported. See the table under _Supported Versions_ for details.
The idea is that if you only ever switch from one listed bupstash versions to
another one then the resulting backup data structures are restorable by the
newest Maxbupstash revision.

In an ideal world, this implementation would have been created entirely
indepdendently from Bupstash without looking at its implementation, because
this might greatly increase resilience in that truly different implementations
are unlikely to contain the same bugs, making a successful data restoration more
likely. Since the existing online documentation about Bupstash's data and crypto
structures are not comprehensive specifications, this was unfortunately not
feasible. Instead, the implementation closely follows Bupstash's in many places.
This makes it likely that structural bugs that exist in the original Bupstash
are present in this implementation, too.

If you are interested in doing a proper “clean room” implementation of a tool
to be compatible with the Bupstash data format, do not hesitate to contact me.
I might be able to assist with the specification part.

Supported Versions
==================

Bupstash Versions  Maxbupstash Versions
-----------------  --------------------
0.10.3             1.1.1

License
=======

	Ma_Sys.ma Bupstash Extractor
	(c) 2022, 2023 Ma_Sys.ma <info@masysma.net>
	
	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

Here is an overview about the Ma_Sys.ma-supplied dependencies' licenses:

Dependency                                License
----------------------------------------  -------------
[lz4_ada(32)](../32/lz4_ada.xhtml)        Expat (“MIT”)
[blake3_ada(32)](../32/blake3_ada.xhtml)  CC0
[tar_ada(32)](../32/tar_ada.xhtml)        GPL 3+

Why this chaos you might ask? The idea behind the licenses for LZ4 and Blake3
is to align them with other important projects from the respective ecosystems,
e.g. the LZ4 Specification or the Blake3 reference implementation as not to
needlessly restrict the license further than the original projects do.

For the Tar implementation there was no real reference implementation although
it is inspired by `tar.rs` (cf. <https://docs.rs/tar/latest/tar/>) and
Bupstash's `xtar.rs` hence the Ma_Sys.ma default “GPL v3 or later” is used. As
for maxbupst itself: Since it is not a library but rather an application
program, the GPL should be less of a problem for practical use and hence the
Ma_Sys.ma default GPL v3+ was chosen over Bupstash's more permissive Expat
license. If this is an issue for you, feel free to contact me about it and
explain whatever difficulty you see regarding the licensing.

Compilation
===========

Some of Bupstash's required dependencies were found to not have any Ada
equivalent readily available. Specifically, the following programs seemed not
to be available in the Ada world: LZ4, Blake3 and TAR Achive creation.

To provide these features, dedicated separate libraries were thus developed as
part of the Maxbupst development. Their pages are here:

 * [lz4_ada(32)](../32/lz4_ada.xhtml)
 * [blake3_ada(32)](../32/blake3_ada.xhtml)
 * [tar_ada(32)](../32/tar_ada.xhtml)

Additionally, the external dependency on libsodium is required, i.e. on
Debian systems this is package `libsodium-dev`.

There are multiple ways to go about compiling this program depending on the
intended mode of deployment. Please refer to the following subsections for
details.

## To Install as a Debian Package

The primary intended use case is to build all of the libraries as separate
Debian packages, install them on the running Debian system and then compile
maxbupst and also install it as a Debian package. If the necessary dependencies
like `ant` and `devscripts` are installed, this can be achieved by running

	ant package

in all of the dependencies' individual directories, then installing all of the
resulting packages like e.g. with `apt install ./...deb` and then compiling
`maxbupst` with the same command in the repsitory checkout:

	ant package

The resulting package can then be installed and the `maxbupst` command becomes
available.

## To compile without Installation

In order to compile this package and obtain an executable that does not have
any external dependencies except for the system's libsodium, use the following
target:

	ant build-rogue

This automatically downloads the required Ma_Sys.ma dependencies next to the
current repository checkout.

## To test that the tool runs

The following sequence of commands is expected to produce a YAML output equal to
the one found in `testdata/small-0.10.3-expected.yml`:

~~~{.bash}
root="$(pwd)"
export BUPSTASH_KEY="$root/testdata/maxbupst-testkey.key"
export BUPSTASH_REPOSITORY="$root/testdata/small-0.10.3"
ulimit -s unlimited
maxbupst -l
~~~

## To compile for Windows

Compilation on Windows is a little bit involved because installing the
dependencies requires a lot of manual actions. Also, in order to setup GNAT on
Windows, the currently recommended course of operation is to use Alire which
means that in order to make use of this installation method, Alire itself needs
to be setup first. The following steps give a rough guide that worked for me:

 1. Install chocolatey if not already installed in an administrative powershell.
    `Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))`
    Details: command from https://chocolatey.org/install “individual” variant.
    Test by running `choco`. It should print out a version.
 2. In the same shell, install git `choco install git`. The tool becomes
    available in newly created shells afterwards.
 3. Install ant and its dependencies:
    `choco install microsoft-openjdk` and then `choco install ant`
    Further info: <https://community.chocolatey.org/packages/ant>,
    <https://mkyong.com/ant/how-to-install-apache-ant-on-windows/>
 4. Switch to a user shell in an empty directory
 5. Clone the repository: `git clone https://github.com/m7a/bo-maxbupst`
 6. Get GNAT via Alire
    <https://github.com/alire-project/alire/releases/>
    Download e.g. `alr-1.2.2-bin-x86_64-windows.zip`. Copy `alr.exe` to
    `bo-maxbupst`.
 7. Switch directory:`cd bo-maxbupst`
 8. Run Alire to install the GNAT compiler. Select not to install msys2.
    `.\alr toolchain --select`. Chose `gnat_native` and a recent gprbuild.
    Try out `gnatmake` to check if the command is available.
 9. Add Alire's toolchain to your PATH
    `${env:PATH} = "${env:PATH};${env:USERPROFILE%}\.config\alire\cache\dependencies\gnat_native_12.2.1_c210a022\bin"`
    (adjust version to your installation)
 10. Get Libsodium <https://download.libsodium.org/libsodium/releases/>
     Download the filw eith `-msvc.zip` and extract the `libsodium.dll` for
     your architecture. Copy it to `bo-maxbupst`.

Now you should be ready to compile the “rogue” variant (others are not
supported on Windows):

	ant build-rogue

The files that are necessary for Maxbupst to run are then: `libsodium.dll` and
`maxbupst.exe`.

In order to test the functioning of the build, create a new directory `test`
outside the repository directory and copy the files `libsodium.dll` and
`maxbupst.exe` there.

Then use a regular `cmd.exe` (not a powershell because that one mangles the
binary output and hence the result .tar will not be extractable) and run e.g.
the following commands:

~~~{.batch}
set ROOT=%CD%\..\bo-maxbupst
set BUPSTASH_KEY=%ROOT%\testdata\maxbupst-testkey.key
set BUPSTASH_REPOSITORY=%ROOT%\testdata\small-0.10.3
maxbupst -l
maxbupst -g -i 81e33ca1db1e6d562bc7146ddd9b37ab | tar -xf -
~~~

Note that it is really `tar -xf -` because `tar -x` gives an error on Windows :)

Usage Documentation (Manpage)
=============================

## Name

`maxbupst` -- Ma_Sys.ma Bupstash Extractor

## Synopsis

	maxbupst -l|list [-k KEY] [-r REPO]
	maxbupst -g|get  [-k KEY] [-r REPO] -i ID

## Description

Read from a Bupstash repository and either display the list of items (`-l`) or
restore the data from a specific item (`-g`.

Key and Repository locations can be passed either through the options
`-k` and `-r` or through the environment variables `BUPSTASH_KEY` or
`BUPSTASH_REPOSITORY`.

## Options

----  --------------------------------------------------------------------
`-l`  List mode. List the repository's items as YAML-like output.
`-g`  Get mode. Extracts the specified item ID and prints it to stdout.
`-k`  Specify Bupstash private key file to use.
`-r`  Specify Bupstash repository root directory to use.
`-i`  Specify item ID to restore. Identify this item by using `-l` before.
----  --------------------------------------------------------------------

## Environment Variables

`BUPSTASH_KEY`
:   Alternative way to specify the key file to use. Equivalent to option `-k`.
`BUPSTASH_REPOSITORY`
:   Alternative way to specify the repository location. Equivalent to option
    `-r`.

## Stdout

Any output data produced is written to stdout. For `-g` it is often advisable to
pipe the output through a `tar -x` command in order to extract the retrieved
data.

When single data items are extracted, their output is returned directly and may
thus require different processing.

## Examples

	maxbupst -l -k testdata/maxbupst-testkey.key -r testdata/small-0.10.3

Bupstash's Cryptosystem
=======================

~~~{.ada}
-- from bupstash_key.ads
type Key is tagged limited record
	ID:                   Bupstash_Types.XID;
	Rollsum_Key:          String(1 .. Random_Seed_Bytes);
	Data_Hash_Key_Part_1: Bupstash_Types.Partial_Hash_Key;
	Data_Hash_Key_Part_2: Bupstash_Types.Partial_Hash_Key;
	Data_PK:              Bupstash_Types.PK;
	Data_SK:              Bupstash_Types.SK;
	Data_PSK:             Bupstash_Types.PSK;
	Idx_Hash_Key_Part_1:  Bupstash_Types.Partial_Hash_Key;
	Idx_Hash_Key_Part_2:  Bupstash_Types.Partial_Hash_Key;
	Idx_PK:               Bupstash_Types.PK;
	Idx_SK:               Bupstash_Types.SK;
	Idx_PSK:              Bupstash_Types.PSK;
	Metadata_PK:          Bupstash_Types.PK;
	Metadata_SK:          Bupstash_Types.SK;
	Metadata_PSK:         Bupstash_Types.PSK;
end record;
~~~

`ID` uniquely identifies this key, `Rollsum_Key` is used for deduplication
during backup creation and not needed for data restoration. The remainder of
the structure's entries are used for restoration and explained in the following.

Note that this is _my_ understanding as a reader of the source code rather than
the inventor of Bupstash itself. Feel free to point out any parts where I
understood the hierarchy wrongly.

Different keys are used to encrypt different parts of the repository as follows:

	Metadata (Items)   Index                      Data
	
	+----------+       +-------------------+      +-----------------------+
	| Backup 1 |------>| hello.txt size 12 |----->| Hello world.#!/bin/sh |
	+----------+       | test.sh   size 24 |      |  -eu.echo Test.       |
	                   +-------------------+      +-----------------------+
	
	Schematic visualization of an example repository with a single item
	containing two files.

Metadata Keys
:   At the high-level, bupstash repositories contain any number of _items_.
    Metadata stores information about such an item. It contains a number
    of tags, the date of backup creation and the addresses and sizes of the
    associated index data (if any) and the associated data (always present).
    The keys prefixed `Metadata_` are used for protecting the private (“secret”)
    part of the metadata.

Index Keys
:   If an item contains multiple files (i.e. is not just a data stream that
    was added as-is to a bupstash repository) then the index stores information
    about each of the contained files. Among other file metadata this includes
    file paths and file sizes. The keys prefixed `Idx_` are used to protect
    this data.

Data Keys
:   Data is the actual backup contents. For restoration purposes, it can be
    thought of as an opaque stream of bytes. If multiple files are contained
    within the backup, the _index_ is used to associated suitable chunks of the
    data stream to the individual files. The keys prefixed `Data_` are used
    to protect the backup contents.

The use of multiple keys for the different repository contents seems sensible.
It allows sub-keys to be created to e.g. only access the backup metadata without
having to be able to decipher the index and data contents. The use of separate
keys for index and data ensures that adversaries cannot attack the system by
exchanging index and data contents.

For each of the parts, PK, SK and PSK keys are stored.

 * PK (“Public Key”)
 * SK (“Secret Key”)
 * PSK (“Pre-Shared Key”)

The idea behind the PSK is that it is a symmetric key that is considered to
be between the public and the secret key in terms of secrecy: Unlike the public
key it is not stored together with the data, but unlike the secret key it is
provided in key files where SK is missing. For restoration purposes, SK and PSK
can both be considered required secret key inputs.

At the low level, all data chunks are encrypted using libsodium's cryptobox
functionality. At the lowest level, the following two API calls are used for
decryption (`zsodiumbinding.ads`):

 * `crypto_box_curve25519xchacha20poly1305_beforenm`:
   This function computes a “shared key” from public and secret keys that is
   used for decryption. This is intended to be used as an input to
   `open_easy_afternm` afterwards (bupstash does something peculiar here,
   though -- read on). I subsequently call this `cryptobox-beforenm`.
 * `crypto_box_curve25519xchacha20poly1305_open_easy_afternm`:
   This function decrypts an encrypted message by providing the “shared key”,
   the ciphertext and a nonce that is typically stored together with the
   ciphertext. I subsequently call this `cryptobox-open`.

Raw ciphertext data in Bupstash consists of the following parts:

	Ciphertext := Nonce || Cryptobox Ciphertext || PK

The decryption key (“box key”, BK) for the contained cryptobox ciphertext is
computed as follows and from that, the plaintext:

	BK        := BLAKE3(Key=PSK, Data=cryptobox-beforenm(PK, SK))
	Plaintext := cryptobox-open(Key=BK, Nonce, Data=Cryptobox Ciphertext)

This encryption is used at the level of _chunks_ with the chunks being managed
by a data and index tree for data and index contents respectively. In order
to check that the content addressible storage is indeed addressed correctly,
the addresses inside the trees are checked as follows using a Hash Key (HK):

	HK               := BLAKE3(Key Part 1 || Key Part 2)
	Computed Address := BLAKE3(Key=HK, Data=Plaintext)

It is then asserted that the computed address corresponds to the address
specified in the tree. The concatenation of all decrypted plaintext chunks then
forms the contents of the index and data respectively.

Graphically, this scheme can be drawn as follows with HKP serving as a short
notation for _Hash Key Part_:

	Chunk                                    Key
	+-------+----------------------+----+    +----+-----+------+------+
	| Nonce | Cryptobox Ciphertext | PK |    | SK | PSK | HKP1 | HKP2 |
	+-------+----------------------+----+    +----+-----+------+------+
            |              |             |          |    |    |          |
            |              |             v          v    |    +- concat -+
	    |              |           +--------------+  |         |
	    |              |           |  cryptobox-  |  |         v
	    |              |           |   beforenm   |  |  +--------------+
	    |              |           +--------------+  |  | Blake 3 Hash |
	    |              |                   |         |  +--------------+
	    |              |                   |         |         |
	    |              |                   | data    | key     |
	    |              |                   v input   v input   |
	    |              |           +--------------------+      |
	    |              |           |    Blake 3 keyed   |      |
	    |              |           |    hash function   |      |
	    |              |           +--------------------+      |
	    |              |                        | BK           |
	    |              |                        |              |
	    | nonce        | ciphertext             | key          |
	    v input        v input                  v input        |
	+---------------------------------------------------+      |
	|                 cryptobox-open                    |      |
	+---------------------------------------------------+      |
	    | plaintext                                            | key
	    | output                                               v input
	    |                                    data input +---------------+
	    +---------------------------------------------->| Blake 3 keyed |
	    |                                               | hash function |
	    |                                               +---------------+
	    |                                                      |
	    v                                                      v
	+---------------+                                   +---------------+
	| Plaintext to  |                                   |    computed   |
	| use           |                                   | chunk address |
	+---------------+                                   +---------------+

Software Design
===============

This section contains some notes about what I learned from reading the Bupstash
source code. It focuses on the data structures and is completed by a diagram
which shows the implementation maxbupst at a high-level glance.

## Bupstash's Index Structures

To assiciate file contents and metadata, bupstash does the following:

 * It creates two separate HTrees and iterates over both of them independently:
   One for metadata and one for the actual file contents.
 * To restore, it iterates over the metatadata tree which consists of a stream
   of records.
 * For each record, it reads out the _size_ of the respective item.
 * If _size_ is nonzero, it reads _size_ bytes from the data tree and produces
   them as output.
 * Now the dasta tree cursor points to data from the next entry with data such
   that the restore can continue by going to the next metadata item.
 * Additional data fields allow for restoration of a subset of files. I did not
   check this in more detail because maxbupst only implements the “full” restore
   functionality.

## Bupstash's HTree Storage

From a restorer point of view, Bupstash stores its data as a “stream”. Instead
of having one large file where data can be appended, a tree structure is used
to represent the “stream”.

When there is only one backup containing multiple files then there is typically
two streams: One for metadata and one for data (see above).

The storage only contains encrypted data and tree metadata. As a result, the
HTree can be traversed without being decrypted. The actual (encrypted) data
is contained in the leaf nodes whereas the other nodes contain unencrypted
metadata about which other tree nodes belong to the same subtree.

Bupstash's storage is “content-addressible”, i.e. the _ID_ of a tree node
is actually the BLAKE3 hash over the concatenation of its (file) contents.

As a result, an entire stream can be identified by its ID. Such a tree can
basically be traversed as follows given the root node _ID_:

 * Read the file with file name _ID_
 * If this is already a leaf node, emit its contents as data.
 * If this is not a leaf node, decode the contained IDs in this node and
   continue by traversing each of the contained nodes in order

To determine which nodes are leaf nodes and which not, additional metadata is
required. This metadata is stored by bupstash as part of the backup metadata
and not contained in any of the two (metadata and data) trees.

Since there is no obvious relation between the tree nodes and the files in the
backup, this structure is not expected to leak any sensitive information. In
order to ensure that the trees are not tampered with, all content addresses
must be validated by independently computing the hash over their contents and
comparing the result with the ID they were found under.

In order to save memory it makes sense to not read the entire tree into RAM.
Rather, the list of leaf nodes is constructed while processing such that there
is always only a few nodes loaded into RAM rather say the entire the backup
contents.

## Maxbupst Package Dependencies

The following diagram shows the dependenceis between the maxbupst Ada packages.
An arrow A -> B defines a dependency of type “A knows B”. External library
components like Tar, Blake3 and LZ4 are shown for completeness despite not being
contained in the maxbupst source tree.

~~~
                                                              ┌─────┐
                                                              │ LZ4 │
                                                              └──▲──┘
                                                                 │
                                                         ┌───────┴─────┐
                                                         │ Compression │
                                                         └───────▲─────┘
                              ┌───────┐                          │
         ┌───────────────────▶│ Serde │               ┌────────┐ │
         │           ┌───────▶│       │             ╔═╡ Crypto ╞═╪══════════╗
         │           │        └───▲▲──┘             ║ └────────┘ │          ║
         │           │            ││   ┌────────┐   ║            │          ║
         │   ┌────┐  │            ││   │ Blake3 │◀──╫──────┐     │          ║
         │ ╔═╡ FS ╞══╪══════╗     ││   └───▲─▲──┘   ║      │     │          ║
         │ ║ └────┘  │      ║     ││       │ │      ║      │     │          ║
         │ ║         │      ║     ││       │ │      ║      │     │  ZSodium ║
         │ ║         │      ║     │└───────┼┐│      ║      │     │      ▲   ║
┌─────┐  │ ║         │      ║     └───────┐│││      ║      │     │      │   ║
│ Tar │  │ ║       Index ◀──╫────────────┐││││      ║      │     │      │   ║
└──▲──┘  │ ║         ▲      ║            │││││      ║      │     │      │   ║
   │     │ ║         │      ║            │││││      ║   Decryption ─────┘   ║
   └─────┼─╫─────── XTar ◀──╫────────────┤││││      ║      ▲▲               ║
         │ ║                ║            │││││      ║      ││               ║
         │ ╚════════════════╝            │││││      ╚══════╪╞═══════════════╝
         │                               │││││             ││
         │                               ││││└──────────┐  ││
         │                               │││└──────────┐│  ││
         │   ┌──────┐                    │││           ││  ││
         │ ╔═╡ Tree ╞════════════════════╪╪╪═════╗     ││  ││
         │ ║ └──────┘                    │││     ║     ││  ││
         │ ║      ┌───────▶ HTree_LL ◀──┐│││     ║     ││  ││
         │ ║      │            ▲        ││││     ║     ││  ││
         │ ║  HTree_Iter       │        ││││     ║     ││  ││
         │ ║      ▲            │        ││││     ║     ││  ││
         │ ║      └────────────┼───── Restorer ──╫─────┼┼──┘│
         │ ║                   │          ▲      ║     ││   │
         │ ║                   │          │      ║     ││   │
         │ ╚═══════════════════╪══════════╪══════╝     ││   │
         │                     │          │            ││   │
         └────────────────────┐│┌─────────┼────────────┼┼───┘
                              │││         │            ││
                     ┌────┐   │││         │            ││
                   ╔═╡ DB ╞═══╪╞╞═════════╪════════════╪╪══════════════╗
                   ║ └────┘   │││         │            ││              ║
                   ║          │││         │            ││     ZBase64  ║
                   ║          │││         │            ││        ▲     ║
                   ║          Item        │            Key       │     ║
                   ║           ▲          │            ▲         │     ║
                   ║           └─────── Repository ────┴─────────┘     ║
                   ║                      ▲                            ║
                   ║                      │                            ║
                   ║                     Main                          ║
                   ║                                                   ║
                   ╚═══════════════════════════════════════════════════╝
~~~
