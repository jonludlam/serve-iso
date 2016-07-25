
(*
 * Copyright (C) 2015 Citrix Systems Inc
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
 *)

open Lwt
open Iso
open Astring
open Cohttp
open Cohttp_lwt_unix

module Iso = Isofs.Make(Block)(Io_page)

exception Error
let (>>*=) m f = match m with
  | `Error e -> fail Error
  | `Ok x -> f x

let ok = `Ok ()

let (>>|=) m f = m >>= fun x -> x >>*= f

let html_of_method_not_allowed meth allowed path info =
  Printf.sprintf
    "<html><body>\
     <h2>Method Not Allowed</h2>\
     <p><b>%s</b>is not an allowed method on <b>%s</b>\
     </p><p>Allowed methods on <b>%s</b> are <b>%s</b></p>\
     <hr />%s\
     </body></html>"
    meth path path allowed info

let blank_uri = Uri.of_string ""


let cstructs_to_string l =
  List.fold_left (fun acc x -> acc ^ (Cstruct.to_string x)) "" l

let serve ~iso ~info ~index uri =
  let file_name = Uri.path (Uri.resolve "" blank_uri uri) in
  Iso.KV_RO.size iso file_name >>|= fun size ->
  Iso.KV_RO.read iso file_name 0 (Int64.to_int size) >>|= fun cstructs ->
  Server.respond_string ~status:`OK ~body:(cstructs_to_string cstructs) ()

let method_filter meth (res,body) = match meth with
  | `HEAD -> return (res,`Empty)
  | _ -> return (res,body)
           
let handler ~iso ~info ~verbose ~index (ch,conn) req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  (* Log the request to the console *)
  Printf.printf "%s %s %s %s\n%!"
    (Cohttp.(Code.string_of_method (Request.meth req)))
    path
    (match verbose with
    | true -> ""
    | false -> ""
    )
    (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch));
  (* Get a canonical filename from the URL and docroot *)
  match Request.meth req with
  | (`GET | `HEAD) as meth ->
    serve ~iso ~info ~index uri
    >>= method_filter meth
  | meth ->
    let meth = Cohttp.Code.string_of_method meth in
    let allowed = "GET, HEAD" in
    let headers = Cohttp.Header.of_list ["allow", allowed] in
    Server.respond_string ~headers ~status:`Method_not_allowed
      ~body:(html_of_method_not_allowed meth allowed path info) ()


let start_server iso_file port host index verbose cert key () =
  Printf.printf "Listening for HTTP request for iso: %s on: %s %d\n" iso_file host port;
  Block.connect iso_file
  >>|= fun b ->
  Iso.connect b
  >>|= fun iso ->
  let info = Printf.sprintf "Served by Cohttp/Lwt listening on %s:%d" host port in
  let conn_closed (ch,conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch)) in
  let callback = handler ~iso ~info ~verbose ~index in
  let config = Server.make ~callback ~conn_closed () in
  let mode = match cert, key with
    | Some c, Some k -> `TLS (`Crt_file_path c, `Key_file_path k, `No_password, `Port port)
    | _ -> `TCP (`Port port)
  in
  Conduit_lwt_unix.init ~src:host ()
  >>= fun ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx () in
  Server.create ~ctx ~mode config

let lwt_start_server iso_file port host index verbose cert key =
  Lwt_main.run (start_server iso_file port host index verbose cert key ())

open Cmdliner

let host =
  let doc = "IP address to listen on." in
  Arg.(value & opt string "0.0.0.0" & info ["s"] ~docv:"HOST" ~doc)

let port =
  let doc = "TCP port to listen on." in
  Arg.(value & opt int 8080 & info ["p"] ~docv:"PORT" ~doc)

let index =
  let doc = "Name of index file in directory." in
  Arg.(value & opt string "index.html" & info ["i"] ~docv:"INDEX" ~doc)

let verb =
  let doc = "Logging output to console." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let ssl_cert =
  let doc = "SSL certificate file." in
  Arg.(value & opt (some string) None & info ["c"] ~docv:"SSL_CERT" ~doc)

let ssl_key =
  let doc = "SSL key file." in
  Arg.(value & opt (some string) None & info ["k"] ~docv:"SSL_KEY" ~doc)

let iso_file =
  let doc = "ISO file to serve." in
  Arg.(value & pos 0 file "none" & info [] ~docv:"ISO" ~doc)

let cmd =
  let doc = "a simple http server" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) sets up a simple http server with lwt as backend";
    `S "BUGS";
    `P "Report them via e-mail to <mirageos-devel@lists.xenproject.org>, or \
        on the issue tracker at <https://github.com/mirage/ocaml-cohttp/issues>";
  ] in
  Term.(pure lwt_start_server $ iso_file $ port $ host $ index $ verb $ ssl_cert $ ssl_key),
  Term.info "cohttp-server" ~version:Cohttp.Conf.version ~doc ~man

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0



