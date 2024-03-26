(** Error used by the RPC system *)

let deser : Error_kind.t = Error_kind.make ~name:"RpcDeserError" ()
let network : Error_kind.t = Error_kind.make ~name:"RpcNetworkError" ()
let protocol : Error_kind.t = Error_kind.make ~name:"RpcProtocolError" ()

(** Error occurring inside a RPC handler *)
let handler : Error_kind.t = Error_kind.make ~name:"RpcHandlerError" ()

(** Error that occurred on the other side of a RPC *)
let remote : Error_kind.t = Error_kind.make ~name:"RpcRemoteError" ()
