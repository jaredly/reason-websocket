/*
 * Copyright (c) 2012-2016 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

/* open Astring */

let b64_encoded_sha1sum = s => Sha1.sha_1(s) |> B64.encode(~pad=true);

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

module Option = {
  let value = (~default) =>
    fun
    | None => default
    | Some(v) => v;

  let value_map = (~default, ~f) =>
    fun
    | None => default
    | Some(v) => f(v);

  let value_exn =
    fun
    | None => invalid_arg("Option.value_exn")
    | Some(v) => v;

  let map = (~f) =>
    fun
    | None => None
    | Some(v) => Some(f(v));
};

module Rng = {
  let init = (~state=?, ()) => {
    let state =
      Option.value(
        state,
        ~default={
          Random.self_init();
          Random.get_state();
        },
      );
    size => {
      let b = Bytes.create(size);
      for (i in 0 to size - 1) {
        Bytes.set(b, i, Char.chr(Random.State.bits(state) land 255))
      }; 
      Bytes.to_string(b)
    }
  };
};

module Frame = {
  module Opcode = {
    type t =
      | Continuation
      | Text
      | Binary
      | Close
      | Ping
      | Pong
      | Ctrl(int)
      | Nonctrl(int);

    let to_string =
      fun
      | Continuation => "continuation"
      | Text => "text"
      | Binary => "binary"
      | Close => "close"
      | Ping => "ping"
      | Pong => "pong"
      | Ctrl(i) => "ctrl " ++ string_of_int(i)
      | Nonctrl(i) => "nonctrl " ++ string_of_int(i);

    let pp = (ppf, t) => Format.fprintf(ppf, "%s", to_string(t));

    let min = 0;
    let max = 15;

    let of_enum =
      fun
      | i when i < 0 || i > 15 => invalid_arg("Frame.Opcode.of_enum")
      | 0 => Continuation
      | 1 => Text
      | 2 => Binary
      | 8 => Close
      | 9 => Ping
      | 10 => Pong
      | i when i < 8 => Nonctrl(i)
      | i => Ctrl(i);

    let to_enum =
      fun
      | Continuation => 0
      | Text => 1
      | Binary => 2
      | Close => 8
      | Ping => 9
      | Pong => 10
      | Ctrl(i) => i
      | Nonctrl(i) => i;

    let is_ctrl = opcode => to_enum(opcode) > 7;
  };

  type t = {
    opcode: Opcode.t,
    extension: int,
    final: bool,
    content: string,
  };

  let pp = (ppf, {opcode, extension, final, content}) =>
    Format.fprintf(
      ppf,
      "[%a (0x%x) (final=%b) %s]",
      Opcode.pp,
      opcode,
      extension,
      final,
      content,
    );

  let show = t => Format.asprintf("%a", pp, t);

  let create =
      (~opcode=Opcode.Text, ~extension=0, ~final=true, ~content="", ()) => {
    opcode,
    extension,
    final,
    content,
  };

  let of_bytes = (~opcode=?, ~extension=?, ~final=?, content) => {
    let content = Bytes.unsafe_to_string(content);
    create(~opcode?, ~extension?, ~final?, ~content, ());
  };

  let close = code => {
    let content = Bytes.create(2);
    EndianBytes.set_int16(content, 0, code);
    of_bytes(~opcode=Opcode.Close, content);
  };

  let of_subbytes = (~opcode=?, ~extension=?, ~final=?, content, pos, len) => {
    let content = Bytes.(sub(content, pos, len) |> unsafe_to_string);
    create(~opcode?, ~extension?, ~final?, ~content, ());
  };
};

let xor = (mask, msg) =>
  for (i in 0 to Bytes.length(msg) - 1) {
    /* masking msg to send */
    Bytes.set(
      msg,
      i,
      Char.(
        code(mask.[i mod 4]) lxor code(Bytes.get(msg, i)) |> chr
      ),
    );
  };

let is_bit_set = (idx, v) => v lsr idx land 1 == 1;

let set_bit = (v, idx, b) =>
  if (b) {
    v lor 1 lsl idx;
  } else {
    v land lnot(1 lsl idx);
  };

let int_value = (shift, len, v) => v lsr shift land (1 lsl len - 1);

let upgrade_present = hs =>
  List.map(Str.split(Str.regexp_string(",")), hs)
  |> (
    hs =>
      List.flatten(hs)
      |> (
        hs =>
          List.map(String.(h => h |> String.lowercase |> trim), hs)
          |> List.mem("upgrade")
      )
  );

exception Protocol_error(string);

let make_handshake_response = key => {
  let accept = key ++ websocket_uuid |> b64_encoded_sha1sum;
  let resHeaders = [
    ("Upgrade", "websocket"),
    ("Connection", "Upgrade"),
    ("Sec-WebSocket-Accept", accept)
  ] |> List.map(((a, b)) => a ++ ": " ++ b) |> String.concat("\r\n");
  let top = "HTTP/1.1 101 Switching Protocols\r\n";
  top ++ resHeaders ++ "\r\n\r\n"
};

module IO = (IO: IO.IO) => {
  open IO;

  type mode =
    | Client(int => string)
    | Server;

  let is_client = mode => mode != Server;
  let is_server = mode => mode == Server;

  let rec read_exactly = (ic, remaining, buf) =>
    read(ic, remaining)
    >>= (
      s =>
        if (s == "") {
          return(None);
        } else {
          let recv_len = String.length(s);
          Buffer.add_string(buf, s);
          if (remaining - recv_len <= 0) {
            return @@ Some(Buffer.contents(buf));
          } else {
            read_exactly(ic, remaining - recv_len, buf);
          };
        }
    );

  let read_uint16 = (ic, buf) =>
    read_exactly(ic, 2, buf)
    >>= (
      s =>
        switch (s) {
        | None => return(None)
        | Some(s) => return @@ Some(EndianBytes.get_uint16(s, 0))
        }
    );

  let read_int64 = (ic, buf) =>
    read_exactly(ic, 8, buf)
    >>= (
      s =>
        switch (s) {
        | None => return(None)
        | Some(s) =>
          return @@
          Some(Int64.to_int @@ EndianBytes.get_int64(s, 0))
        }
    );

  let write_frame_to_buf = (~mode, buf, fr) => {
    let scratch = Bytes.create(8);
    open Frame;
    let content = Bytes.unsafe_of_string(fr.content);
    let len = Bytes.length(content);
    let opcode = Opcode.to_enum(fr.opcode);
    let payload_len =
      switch (len) {
      | n when n < 126 => len
      | n when n < 1 lsl 16 => 126
      | _ => 127
      };

    let hdr = set_bit(0, 15, fr.final); /* We do not support extensions for now */
    let hdr = hdr lor opcode lsl 8;
    let hdr = set_bit(hdr, 7, is_client(mode));
    let hdr = hdr lor payload_len; /* Payload len is guaranteed to fit in 7 bits */
    EndianBytes.set_int16(scratch, 0, hdr);
    Buffer.add_subbytes(buf, scratch, 0, 2);
    switch (len) {
    | n when n < 126 => ()
    | n when n < 1 lsl 16 =>
      EndianBytes.set_int16(scratch, 0, n);
      Buffer.add_subbytes(buf, scratch, 0, 2);
    | n =>
      EndianBytes.set_int64(scratch, 0, Int64.(of_int(n)));
      Buffer.add_subbytes(buf, scratch, 0, 8);
    };
    switch (mode) {
    | Server => ()
    | Client(random_string) =>
      let mask = random_string(4);
      Buffer.add_string(buf, mask);
      if (len > 0) {
        xor(mask, content);
      };
    };
    Buffer.add_bytes(buf, content);
  };

  let close_with_code = (mode, buf, oc, code) => {
    Buffer.clear(buf);
    write_frame_to_buf(~mode, buf) @@ Frame.close(code);
    write(oc) @@ Buffer.contents(buf);
  };

  let make_read_frame = (~buf=Buffer.create(128), ~mode, ic, oc, ()) => {
    Buffer.clear(buf);
    read_exactly(ic, 2, buf)
    >>= (
      hdr =>
        switch (hdr) {
        | None => raise(End_of_file)
        | Some(hdr) =>
          let hdr_part1 = EndianBytes.get_int8(hdr, 0);
          let hdr_part2 = EndianBytes.get_int8(hdr, 1);
          let final = is_bit_set(7, hdr_part1);
          let extension = int_value(4, 3, hdr_part1);
          let opcode = int_value(0, 4, hdr_part1);
          let frame_masked = is_bit_set(7, hdr_part2);
          let length = int_value(0, 7, hdr_part2);
          let opcode = Frame.Opcode.of_enum(opcode);
          Buffer.clear(buf);
          (
            switch (length) {
            | i when i < 126 => return(i)
            | 126 =>
              read_uint16(ic, buf)
              >>= (
                fun
                | Some(i) => return(i)
                | None => return @@ (-1)
              )
            | 127 =>
              read_int64(ic, buf)
              >>= (
                fun
                | Some(i) => return(i)
                | None => return @@ (-1)
              )
            | _ => return @@ (-1)
            }
          )
          >>= (
            payload_len =>
              if (payload_len == (-1)) {
                raise(
                  Protocol_error("payload len = " ++ string_of_int(length)),
                );
              } else if (extension != 0) {
                close_with_code(mode, buf, oc, 1002)
                >>= (() => raise(Protocol_error("unsupported extension")));
              } else if (Frame.Opcode.is_ctrl(opcode) && payload_len > 125) {
                close_with_code(mode, buf, oc, 1002)
                >>= (() => raise(Protocol_error("control frame too big")));
              } else {
                (
                  if (frame_masked) {
                    Buffer.clear(buf);
                    read_exactly(ic, 4, buf)
                    >>= (
                      fun
                      | None => raise(Protocol_error("could not read mask"))
                      | Some(mask) => return(mask)
                    );
                  } else {
                    return("");
                  }
                )
                >>= (
                  mask =>
                    if (payload_len == 0) {
                      return @@ Frame.create(~opcode, ~extension, ~final, ());
                    } else {
                      {
                        Buffer.clear(buf);
                        read_exactly(ic, payload_len, buf);
                      }
                      >>= (
                        payload =>
                          switch (payload) {
                          | None =>
                            raise(Protocol_error("could not read payload"))
                          | Some(payload) =>
                            let payload = Bytes.unsafe_of_string(payload);
                            if (frame_masked) {
                              xor(mask, payload);
                            };
                            let frame =
                              Frame.of_bytes(
                                ~opcode,
                                ~extension,
                                ~final,
                                payload,
                              );
                            return(frame);
                          }
                      );
                    }
                );
              }
          );
        }
    );
  };
};
