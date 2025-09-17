import gleam/dynamic/decode.{type Decoder}
import gleeunit
import protobuf_decode_gleam.{decode}

pub fn main() -> Nil {
  gleeunit.main()
}

pub type TwoInts {
  Test(id: Int, age: Int)
}

pub fn two_ints_decoder() -> Decoder(TwoInts) {
  use id <- decode.field(1, decode.int)
  use age <- decode.field(2, decode.int)
  decode.success(Test(id:, age:))
}

pub fn two_ints_test() {
  let bytes = <<
    // field 1
    0x08:little,
    0x96:little,
    0x01:little,
    // field 2
    0x10:little,
    0x96:little,
    0xf2:little,
    0x04:little,
  >>

  let assert Ok(data) = decode(from: bytes, using: two_ints_decoder())
  assert data == Test(id: 150, age: 80_150)
}
