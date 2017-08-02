provider "aws" {
  region = "${var.region}"
}

resource "aws_instance" "draenorEC2" {
  ami           = "${lookup(var.amis, var.region)}"
  instance_type = "m3.2xlarge"
  key_name      = "${var.key_name}"

  tags {
    Name = "Draenor EC2"
  }

  provisioner "remote-exec" {
    script = "provision.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = "${file("${var.pem_path}")}"
    }
  }
}
