let parse_top = top => {
  let parts = Str.split(Str.regexp("[ \t]+"), top);
  switch (parts) {
  | [method, path, ...others] => Some((method, path))
  | _ => None
  }
};

module StringMap = Map.Make(String);

let parse_headers = headers => {
  List.fold_left(
    (map, line) => {
      let parts = Str.split(Str.regexp(":"), line);
      switch parts {
      | [] | [_] => map
      | [name, ...rest] => StringMap.add(name, String.concat(":", rest), map)
      }
    },
    StringMap.empty,
    headers
  )
};

let parse_request = text => {
  let items = Str.split(Str.regexp("\r?\n"), text);
  switch items {
  | [] => failwith("Invalid request")
  | [top, ...headers] =>
  switch (parse_top(top)) {
  | None => failwith("Invalid top: " ++ top)
  | Some((method, path)) =>
  let header_map = parse_headers(headers);
  (method, path, header_map)
  }
  }
};
