(*
 Copyright (C) 2003-2005 Savonet team

 This file is part of Ocaml-ssl.

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 Ocaml-smbclient is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Ocaml-smbclient; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)


(**
  * A small copycat server using SSL.
  *
  * @author Samuel Mimram
  *)

let certfile = ref "cert.pem"
let privkey = ref "privkey.pem"
let port = ref 9876
let password = ref "hello"

let log s =
  Printf.printf "[II] %s\n%!" s

let establish_threaded_server server_handler sockaddr nbconn =
  log "establishing server";
  let domain =
    begin match sockaddr with
    | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
    | Unix.ADDR_INET (_, _) -> Unix.PF_INET
    end
  in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  let handle_connexion (s, caller) =
    let inet_addr_of_sockaddr = function
      | Unix.ADDR_INET (n, _) -> n
      | Unix.ADDR_UNIX _ -> Unix.inet_addr_any
    in
    let inet_addr = inet_addr_of_sockaddr caller in
    let ip = Unix.string_of_inet_addr inet_addr in
    log (Printf.sprintf "openning connection for [%s]" ip);
    server_handler inet_addr s;
    Ssl.shutdown s
  in
  let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
  if !password <> "" then
    Ssl.set_password_callback ctx (fun _ -> !password);
  Ssl.use_certificate ctx !certfile !privkey;
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock nbconn;
  (* let ssl_sock = Ssl.embed_socket sock ctx in *)
  while true do
    log "listening for connections";
    let (s, caller) = Unix.accept sock in
    let ssl_s = Ssl.embed_socket s ctx in
    Ssl.accept ssl_s;
    ignore (Thread.create handle_connexion (ssl_s, caller));
  done

let () =
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let connected_clients = ref [] in
  Ssl_threads.init ();
  Ssl.init ();
  establish_threaded_server
    (fun addr ssl ->
      connected_clients := (addr, ssl) :: !connected_clients;
      log "accepted a new connection";
      let loop = ref true in
      while !loop
      do
        let l = Ssl.read ssl buf 0 bufsize in
        let m = Bytes.sub buf 0 l in
        let msg = Bytes.sub m 0 ((Bytes.length m) - 1) in
        log (Printf.sprintf "revceived '%s'" msg);
        if msg = "exit" then
          (
            log "A client has quit";
            connected_clients := List.filter (fun (_, s) -> s != ssl) !connected_clients;
            Ssl.shutdown ssl;
            loop := false
          )
        else
          List.iter
            (fun (_, s) ->
              ignore (Ssl.output_string s m)
            ) !connected_clients
      done
    )
    (Unix.ADDR_INET(Unix.inet_addr_any, !port)) 100
