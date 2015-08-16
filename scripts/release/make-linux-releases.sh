#!/usr/bin/env bash
#TODO: move this logic into release.hs.
set -xe
init_wd="$(pwd)"

with_vagrant() {
    #TODO: set up gpg-agent forwarding for package signing (see http://superuser.com/questions/161973/how-can-i-forward-a-gpg-key-via-ssh-agent).
    pushd "$init_wd/etc/vagrant/$1"
    vagrant up
    vagrant ssh -c "export GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN; export AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID; export AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY; export AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION; cd /vagrant; scripts/release/release.hs $2"
    vagrant halt
    popd
}

with_vagrant debian-7-amd64 "release"
with_vagrant debian-7-i386 "release"
with_vagrant centos-6-x86_64 "--binary-variant=gmp4 release"
with_vagrant centos-6-i386 "--binary-variant=gmp4 release"
with_vagrant debian-7-amd64 "ubuntu-upload debian-upload arch-upload"
with_vagrant centos-7-x86_64 "centos-upload fedora-upload"
