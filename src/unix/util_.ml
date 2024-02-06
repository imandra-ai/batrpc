let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> spf "unix:%s" s
  | Unix.ADDR_INET (addr, port) ->
    spf "tcp://%s:%d" (Unix.string_of_inet_addr addr) port

let kind_of_sockaddr = function
  | Unix.ADDR_INET _ -> Unix.PF_INET
  | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
