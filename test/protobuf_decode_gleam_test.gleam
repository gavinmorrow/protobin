import gleam/dynamic/decode.{type Decoder}
import gleeunit
import protobuf_decode_gleam.{decode, decode_uint}

pub fn main() -> Nil {
  gleeunit.main()
}

pub type TwoInts {
  Test(id: Int, age: Int)
}

pub fn two_ints_decoder() -> Decoder(TwoInts) {
  use id <- decode.field(1, decode_uint())
  use age <- decode.field(2, decode_uint())

  decode.success(Test(id:, age:))
}

pub fn two_ints_test() {
  let bytes = <<
    // field 1
    0x08,
    0x96,
    0x01,
    // field 2
    0x10,
    0x96,
    0xf2,
    0x04,
  >>

  let assert Ok(data) = decode(from: bytes, using: two_ints_decoder())
  assert data == Test(id: 150, age: 80_150)
}
