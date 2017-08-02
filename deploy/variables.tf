variable "region" {
  default = "us-east-1"
}

variable "amis" {
  type = "map"

  default = {
    us-east-1 = "ami-13be557e" # Ubuntu 16.04
  }
}

variable "key_name" {}

variable "pem_path" {}
