open Protype_robin

module Codec =
struct
  let encode typ value = Encode.to_string ~version: 0 typ value
  type decode_error = Decode.error
  let show_decode_error = Decode.show_error
  let decode typ string = Decode.from_string typ string
end

module Tests = Protype_tests.Make (Codec)

let () = Tests.run ()
