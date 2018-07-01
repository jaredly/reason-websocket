/* Configuration */
let certfile = "cert.pem";
let privkey = "privkey.pem";
let port = 9876;
let password = "hello";

let log = s => Printf.printf("%s\n%!", s);

module MIO {
  type t('a) = ('a => unit) => unit;

  let return = (x, fin) => fin(x);
  let bind = (work, use, fin) => work((result) => (use(result))(fin));

  let (>>=) = bind;

  type ic = Ssl.socket;
  type oc = Ssl.socket;
  type conn;

  let read = (sock, size, fin) => {
    let buf = Bytes.create(size);
    switch (Ssl.read(sock, buf, 0, size)) {
    | exception Ssl.Read_error(_) => fin("")
    | ln => fin(Bytes.to_string(buf))
    }
  };

  let write = (sock, text, fin) => {
    Ssl.output_string(sock, text);
    fin(())
  };
};

module WSock = Websocket.IO(MIO);

let startServer = (handler, sockaddr, nbconn) => {
  /* Set up socket */
  let listeningSocket = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
  Unix.setsockopt(listeningSocket, Unix.SO_REUSEADDR, true);
  Unix.bind(listeningSocket, sockaddr);
  Unix.listen(listeningSocket, nbconn);

  let threadHandler = (socket) => {
    handler(socket);
    Ssl.shutdown(socket);
  };

  let ctx = Ssl.create_context(Ssl.SSLv23, Ssl.Server_context);
  Ssl.set_password_callback(ctx, _ => password);
  Ssl.use_certificate(ctx, certfile, privkey);

  log(Printf.sprintf("Listening on port %d...", port));
  while (true) {
    let (clientSocket, _clientAddr) = Unix.accept(listeningSocket);
    let sslSocket = Ssl.embed_socket(clientSocket, ctx);
    Ssl.accept(sslSocket);
    ignore(Thread.create(threadHandler, sslSocket));
  };
};


let handleWebsocket = (path, headers, socket) => {
  let key = Http.StringMap.find("Sec-WebSocket-Key", headers) |> String.trim;
  let response = Websocket.make_handshake_response(key);
  Ssl.output_string(socket, response);

  let loop = ref(true);
  while(loop^) {
    WSock.make_read_frame(~mode=WSock.Server, socket, socket, ())(frame => {
      print_endline("Got frame! " ++ frame.Websocket.Frame.content);
      let buffer = Buffer.create(1024);
      WSock.write_frame_to_buf(~mode=WSock.Server, buffer, Websocket.Frame.create(~content="Thanks for the frame", ()));
      print_endline("Sending frame");
      Ssl.output_string(socket, Buffer.contents(buffer))
        /* () */
    })
  }
};

let handleConnection = socket => {
  let bufsize = 1024;
  let buf = Bytes.create(bufsize);
  log("New thread!");
  let loop = ref(true);
  while (loop^) {
    switch (Ssl.read(socket, buf, 0, bufsize)) {
    | exception Ssl.Read_error(_) => {
      log("A client has quit");
      Ssl.shutdown(socket);
      loop := false
    }
    | length => 
      let msg = Bytes.sub(buf, 0, length);
      log(Printf.sprintf(">> recieved '%S'", msg));
      let (method, path, headers) = Http.parse_request(msg);
      let shouldUpgrade = Http.StringMap.exists((k, v) => k == "Upgrade" && String.trim(v) == "websocket", headers);
      if (shouldUpgrade) {
        handleWebsocket(path, headers, socket)
      } else {
        Ssl.output_string(
          socket,
          "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Type: text/plain\r\n\r\nHello",
        );
      }
    };
  };
};

let () = {
  Ssl_threads.init();
  Ssl.init();
  startServer(
    handleConnection,
    Unix.ADDR_INET(Unix.inet_addr_any, port),
    100,
  );
};