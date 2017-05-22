#!/usr/bin/env ocaml
#use "topfind"
#require "topkg-jbuilder"

let () = Topkg_jbuilder.describe ~name:"hvsock" ()
