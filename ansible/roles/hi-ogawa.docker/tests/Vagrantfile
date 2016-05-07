# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty64"

  config.vm.network 'private_network', ip: '192.168.33.10'

  config.vm.provider 'virtualbox' do |vb|
    vb.memory = '2048'
  end

  # NOTE: add the first deployer's public key same as aws instance
  config.ssh.pty = true
  config.vm.provision 'shell' do |s|
    ssh_pub_key = File.readlines("#{Dir.home}/.ssh/id_rsa.pub").first.strip
    s.inline = <<-SHELL
      echo #{ssh_pub_key} >> /home/ubuntu/.ssh/authorized_keys
    SHELL
  end
end
