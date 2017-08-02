#!/bin/bash

# Install dependencies
sudo apt update
sudo apt upgrade -y
sudo apt install openjdk-8-jre openjdk-8-jdk python-pip htop -y
sudo pip install awscli
wget -qO- https://get.haskellstack.org/ | sh

# Set up osm2orc
git clone https://github.com/mojodna/osm2orc.git
cd osm2orc
./gradlew distTar

# Set up draenor
cd ~
git clone https://github.com/fosskers/draenor.git
cd draenor
stack setup
stack build

# Reboot the instance to fully apply package changes
sudo reboot
