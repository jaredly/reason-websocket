
module type Config = {
  type sock;
  let read: (sock, int) => option(string);
  let write: (sock, string) => unit;
  let acceptSocket: Unix.file_descr => sock;
  let init: unit => unit;
};

module Connection = (Config: Config) => {
  type t('a) = ('a => unit) => unit;

  let return = (x, fin) => fin(x);
  let bind = (work, use, fin) => work((result) => (use(result))(fin));

  let (>>=) = bind;

  type ic = Config.sock;
  type oc = Config.sock;
  type conn;

  let read = (sock, size, fin) => {
    fin(switch (Config.read(sock, size)) {
      | None => ""
      | Some(s) => s
    })
  };

  let write = (sock, text, fin) => {
    Config.write(sock, text);
    fin(())
  };
};

module Server = (Config: Config) => {
  let module WSock = Websocket.IO(Connection(Config));

  let handleWebsocket = (~onMessage, path, headers, socket) => {
    let key = Http.StringMap.find("Sec-WebSocket-Key", headers) |> String.trim;
    let response = Websocket.make_handshake_response(key);
    Config.write(socket, response);

    let loop = ref(true);
    let buffer = Buffer.create(1024);
    while(loop^) {
      WSock.make_read_frame(~mode=WSock.Server, socket, socket, ())(frame => {
        onMessage(frame.Websocket.Frame.content, response => {
          Buffer.clear(buffer);
          WSock.write_frame_to_buf(~mode=WSock.Server, buffer, Websocket.Frame.create(~content=response, ()));
          Config.write(socket, Buffer.contents(buffer))
        })
      })
    }
  };

  let handleConnection = (~onMessage, ~httpFallback, socket) => {
    let loop = ref(true);
    while (loop^) {
      switch (Config.read(socket, 1024)) {
      | None => loop := false
      | Some(msg) =>
        let (method, path, headers) = Http.parse_request(msg);
        let shouldUpgrade =
          Http.StringMap.exists(
            (k, v) => k == "Upgrade" && String.trim(v) == "websocket",
            headers,
          );
        if (shouldUpgrade) {
          handleWebsocket(~onMessage, path, headers, socket);
        } else {
          httpFallback(method, path, headers, msg, response => {
            Config.write( socket, response);
          })
        };
      };
    };
  };
};

module UnixConfig = {
  type sock = Unix.file_descr;
  let read = (sock, maxlen) => {
    let bytes = Bytes.create(maxlen);
    let len = Unix.recv(sock, bytes, 0, maxlen, []);
    Some(Bytes.sub_string(bytes, 0, len))
  };
  let write = (sock, text) => {
    let total = String.length(text);
    let left = ref(String.length(text));
    while (left^ > 0) {
      left := left^ - Unix.send(sock, text, total - left^, left^, []);
    };
  };

  let init = () => ();
  let acceptSocket = socket => socket;
};

let run = (~port, ~onMessage, ~httpFallback, ~config: (module Config)) => {
  let module Config = (val config);
  Config.init();
  let module S = Server(Config);

  /* Set up socket */
  let listeningSocket = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
  Unix.setsockopt(listeningSocket, Unix.SO_REUSEADDR, true);
  Unix.bind(listeningSocket, Unix.ADDR_INET(Unix.inet_addr_any, port));
  Unix.listen(listeningSocket, 100);

  print_endline(Printf.sprintf("Listening on port %d...", port));
  while (true) {
    let (clientSocket, _clientAddr) = Unix.accept(listeningSocket);
    let sock = Config.acceptSocket(clientSocket);
    ignore(Thread.create(S.handleConnection(~onMessage, ~httpFallback), sock));
  };
};