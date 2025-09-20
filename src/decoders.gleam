import gleam/bit_array
import gleam/dynamic/decode.{type Decoder}

import internal/util
import protobuf_decode_gleam.{parse}

pub fn protobuf(
  using decoder: fn() -> Decoder(t),
  named name: String,
  default default: t,
) -> Decoder(t) {
  use bits <- decode.then(decode.bit_array)

  let value = parse(from: bits, using: decoder())
  case value {
    Ok(value) -> decode.success(value)
    Error(_) -> decode.failure(default, name)
  }
}

pub fn uint() -> Decoder(Int) {
  use bits <- decode.then(decode.bit_array)
  util.bit_array_to_uint(bits) |> decode.success
}

pub fn fixed(size: Int) -> Decoder(Int) {
  use bits <- decode.then(decode.bit_array)
  let assert <<num:unsigned-little-size(size)>> = bits
  num |> decode.success
}

pub fn string() -> Decoder(String) {
  use bits <- decode.then(decode.bit_array)

  let str = bit_array.to_string(bits)
  case str {
    Ok(str) -> decode.success(str)
    Error(_) -> decode.failure("", "String")
  }
}
