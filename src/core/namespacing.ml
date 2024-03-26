let service_prefix package service_name : string =
  let prefix = String.concat "." package in
  if prefix = "" then
    service_name
  else
    spf "%s.%s" prefix service_name

let assemble_meth_name ~prefix name : string = spf "%s.%s" prefix name
