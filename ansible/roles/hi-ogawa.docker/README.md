[![Build Status](https://travis-ci.org/hi-ogawa/ansible-role-docker.svg?branch=master)](https://travis-ci.org/hi-ogawa/ansible-role-docker)

# Docker role for Ubuntu 14.04

This follows docker installation for ubuntu 14.04 explained in
[the official documentation](https://docs.docker.com/engine/installation/linux/ubuntulinux/).

Role Variables
--------------

You can specify user to be included in docker group,
which makes user able to run docker commands without `sudo`.

For example:

```
---
docker-group:
  - ubuntu
  - deployer
```

Example Playbook
----------------

Tested with:

```
$ ansible --version
ansible 2.1.0 (devel 50dfd4b057) last updated 2016/02/07 13:39:55 (GMT +900)
  lib/ansible/modules/core: (detached HEAD e1ec52e365) last updated 2016/02/07 13:41:53 (GMT +900)
  lib/ansible/modules/extras: (detached HEAD 14a62fb5d6) last updated 2016/02/07 13:42:45 (GMT +900)
  config file =
  configured module search path = Default w/o overrides
```

Example is here [hi-ogawa/ansible-role-docker-example](https://github.com/hi-ogawa/ansible-role-docker-example).

Test Run on Vagrant
-------------------

```
$ VAGRANT_CWD=./tests vagrant up
Bringing machine 'default' up with 'virtualbox' provider...
==> default: Importing base box 'ubuntu/trusty64'...
==> default: Matching MAC address for NAT networking...
==> default: Checking if box 'ubuntu/trusty64' is up to date...
==> default: Setting the name of the VM: tests_default_1462544168043_9812
==> default: Clearing any previously set forwarded ports...
==> default: Clearing any previously set network interfaces...
==> default: Preparing network interfaces based on configuration...
    default: Adapter 1: nat
    default: Adapter 2: hostonly
==> default: Forwarding ports...
    default: 22 => 2222 (adapter 1)
==> default: Running 'pre-boot' VM customizations...
==> default: Booting VM...
==> default: Waiting for machine to boot. This may take a few minutes...
    default: SSH address: 127.0.0.1:2222
    default: SSH username: vagrant
    default: SSH auth method: private key
    default: Warning: Connection timeout. Retrying...
    default:
    default: Vagrant insecure key detected. Vagrant will automatically replace
    default: this with a newly generated keypair for better security.
    default:
    default: Inserting generated public key within guest...
    default: Removing insecure key from the guest if it's present...
    default: Key inserted! Disconnecting and reconnecting using new SSH key...
==> default: Machine booted and ready!
==> default: Checking for guest additions in VM...
    default: The guest additions on this VM do not match the installed version of
    default: VirtualBox! In most cases this is fine, but in rare cases it can
    default: prevent things such as shared folders from working properly. If you see
    default: shared folder errors, please make sure the guest additions within the
    default: virtual machine match the version of VirtualBox you have installed on
    default: your host and reload your VM.
    default:
    default: Guest Additions Version: 4.3.36
    default: VirtualBox Version: 5.0
==> default: Configuring and enabling network interfaces...
==> default: Mounting shared folders...
    default: /vagrant => /Users/hiogawa/repositories/mine/ansible-role-docker/tests
==> default: Running provisioner: shell...
    default: Running: inline script
$ ansible -i tests/inventory -l vagrant all -m ping
The authenticity of host '192.168.33.10 (192.168.33.10)' can't be established.
RSA key fingerprint is 93:21:30:69:0c:46:16:2b:51:99:7b:7a:58:69:c1:d4.
Are you sure you want to continue connecting (yes/no)? yes

192.168.33.10 | SUCCESS => {
    "changed": false,
    "ping": "pong"
}
$ ANSIBLE_ROLES_PATH=../ ansible-playbook tests/test.yml -i tests/inventory -l vagrant

PLAY ***************************************************************************

TASK [setup] *******************************************************************
ok: [192.168.33.10]

TASK [ansible-role-docker : install apt package dependencies] ******************
ok: [192.168.33.10] => (item=[u'apt-transport-https', u'ca-certificates', u'apparmor'])

TASK [ansible-role-docker : add apt_key] ***************************************
changed: [192.168.33.10]

TASK [ansible-role-docker : add apt_repository] ********************************
changed: [192.168.33.10]

TASK [ansible-role-docker : purge old docker] **********************************
ok: [192.168.33.10]

TASK [ansible-role-docker : check kernel] **************************************
changed: [192.168.33.10]

TASK [ansible-role-docker : install linux-image-extra] *************************
changed: [192.168.33.10]

TASK [ansible-role-docker : install docker] ************************************
changed: [192.168.33.10]

TASK [ansible-role-docker : download docker-compose] ***************************
changed: [192.168.33.10]
 [WARNING]: Consider using get_url module rather than running curl

TASK [ansible-role-docker : change file attributes of docker-compose] **********
changed: [192.168.33.10]

TASK [ansible-role-docker : add users to docker group] *************************
changed: [192.168.33.10] => (item=root)

RUNNING HANDLER [ansible-role-docker : restart docker] *************************
changed: [192.168.33.10]

PLAY RECAP *********************************************************************
192.168.33.10              : ok=10   changed=7    unreachable=0    failed=0

```

License
-------

BSD
