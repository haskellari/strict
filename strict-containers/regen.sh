#!/bin/bash
# Regenerate files from submodules.
# Set CLEAN=1 to delete generated files, but don't regenerate them.

set -e
shopt -s globstar

( cd ../contrib/containers && git checkout v0.6.4.1 )
( cd ../contrib/unordered-containers && git checkout v0.2.13.0 )

fixup_cabal() {
	local type="$1"
	local header="-- DO NOT EDIT below, AUTOGEN $type"
	local footer="-- DO NOT EDIT above, AUTOGEN $type"
	{
	echo "$header"
	cat
	echo "$footer"
	} | sed -e 's/^/    /g' > "$type.list"
	sed -i -e "/$header/,/$footer/d" -e '/-- generated list for '"$type"'/r'"$type.list" strict-containers.cabal
	rm -f "$type.list"
}

rename_modules() {
	test -z "$CLEAN" || return 0
	local path_r="$1"
	local path_l="$2"
	local path="${3:-$2}"

	local mod_r="$(echo "${path_r}" | sed -e 's,/,\.,g')"
	local mod_l="$(echo "${path_l}" | sed -e 's,/,.,g')"
	sed -e 's/'"${mod_r}"'/'"${mod_l}"'/g' -i "src/${path}"/**/*.hs
	if [ -f "src/${path}.hs" ]; then
		sed -e 's/'"${mod_r}"'/'"${mod_l}"'/g' -i "src/${path}.hs"
	fi
}

copy_and_rename() {
	local pkg="$1"
	local type="$2"
	local path_r="$3"
	local excludes="$4"

	local path_l="Data/Strict/$type/Autogen"

	rm -rf "src/${path_l}.hs" "src/${path_l}"
	mkdir -p "src/${path_l}"
	cat /dev/null | fixup_cabal "$type"
	test -z "$CLEAN" || return 0

	cp -a ../contrib/"$pkg/${path_r}"/* "src/${path_l}/"
	if [ -f ../contrib/"$pkg/${path_r}.hs" ]; then
		cp -a ../contrib/"$pkg/${path_r}.hs" "src/${path_l}.hs"
	fi
	for i in $excludes; do
		rm -f "src/${path_l}$i"
	done
	rename_modules "$path_r" "$path_l"
	( cd src && find "${path_l}.hs" "${path_l}" -type f 2>/dev/null | sed -e 's/.hs$//g' -e 's,/,.,g' ) | fixup_cabal "$type"
	patch -p1 < "patches/$type.patch"
}

copy_and_rename unordered-containers HashMap Data/HashMap "/Lazy.hs /Internal/Lazy.hs"
rm -rf include && mkdir -p include
if [ -z "$CLEAN" ]; then
	cp -a ../contrib/containers/containers/include/* include/
	find include -type f | fixup_cabal includes
else
	cat /dev/null | fixup_cabal includes
fi
copy_and_rename containers/containers/src ContainersUtils Utils/Containers/Internal
copy_and_rename containers/containers/src Map Data/Map ".hs /Lazy.hs /Merge/Lazy.hs /Lazy/Internal.hs"
rename_modules Utils/Containers/Internal Data/Strict/ContainersUtils/Autogen Data/Strict/Map/Autogen
copy_and_rename containers/containers/src Sequence Data/Sequence "/Internal/sorting.md"
rename_modules Utils/Containers/Internal Data/Strict/ContainersUtils/Autogen Data/Strict/Sequence/Autogen
