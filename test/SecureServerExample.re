
/* Using ocaml-ssl https://github.com/jaredly/ocaml-ssl */
module SSLConfig = (Config: {let password: string; let certfile: string; let privkey: string;}) => {
  type sock = Ssl.socket;
  let read = (sock, size) => {
    let buf = Bytes.create(size);
    switch (Ssl.read(sock, buf, 0, size)) {
    | exception (Ssl.Read_error(_)) => None
    | ln => Some(Bytes.sub(buf, 0, ln))
    };
  };
  let write = (sock, text) => Ssl.output_string(sock, text);

  let onClose = sock => Ssl.shutdown(sock);

  let ctx = ref(None);
  let init = () => {
    Ssl_threads.init();
    Ssl.init();
    let c = Ssl.create_context(Ssl.SSLv23, Ssl.Server_context);
    ctx := Some(c);
    Ssl.set_password_callback(c, _ => Config.password);
    Ssl.use_certificate(c, Config.certfile, Config.privkey);
  };

  let acceptSocket = clientSocket => {
    switch (ctx^) {
      | None => failwith("Must initialize first")
      | Some(ctx) =>
        let sslSocket = Ssl.embed_socket(clientSocket, ctx);
        Ssl.accept(sslSocket);
        sslSocket
    }
  };
};

/**
 * First, generate cert.pem and privkey.pem
 * openssl genrsa -des3 -out privkey.pem 1024
 * openssl req -new -x509 -days 1001 -key privkey.pem -out cert.pem -config <(printf "prompt=no\ndistinguished_name=r\n[r]\nCN=Testing\n")
 */

Server.run(
  ~port=9876,
  ~onMessage=(text, reply) => {
    reply("Thanks for the " ++ text)
  },
  ~httpFallback=(method, path, headers, msg, respond) => {
    respond(
          "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Type: text/plain\r\n\r\nHello",
    )
  },
  ~config=(module SSLConfig({
    let certfile = "cert.pem";
    let privkey = "privkey.pem";
    let password = "";
  }): Server.Config)
)