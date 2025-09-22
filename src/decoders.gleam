import gleam/bit_array
import gleam/dynamic/decode.{type Decoder}
import gleam/list

import internal/util
import protobuf_decode_gleam.{parse}

fn single(of decoder: Decoder(t)) -> Decoder(t) {
  use values <- decode.then(decode.list(of: decoder))

  // I choose to use assert instead of `decode.failure` because the decode api
  // requires passing a name and default value, which is clunky and would've
  // added two parameters to this function.
  // The only times this could fail are programmer error on my part or using
  // this decoder on data not produced in a compatible way with this library.
  let assert Ok(value) = list.last(values)
  value |> decode.success
}

// Allows the decoders to be used for either single or repeated fields
fn single_or_raw(decoder: Decoder(t)) -> Decoder(t) {
  decode.one_of(decoder, or: [single(of: decoder)])
}

pub fn protobuf(
  // Passed as a function so recursive decoders are easier
  using decoder: fn() -> Decoder(t),
  named name: String,
  default default: t,
) -> Decoder(t) {
  use bits <- decode.then(single_or_raw(decode.bit_array))

  let value = parse(from: bits, using: decoder())
  case value {
    Ok(value) -> decode.success(value)
    Error(_) -> decode.failure(default, name)
  }
}

pub fn uint() -> Decoder(Int) {
  use bits <- decode.then(single_or_raw(decode.bit_array))
  util.bit_array_to_uint(bits) |> decode.success
}

pub fn fixed(size: Int) -> Decoder(Int) {
  use bits <- decode.then(single_or_raw(decode.bit_array))
  let assert <<num:unsigned-little-size(size)>> = bits
  num |> decode.success
}

pub fn string() -> Decoder(String) {
  use bits <- decode.then(single_or_raw(decode.bit_array))

  let str = bit_array.to_string(bits)
  case str {
    Ok(str) -> decode.success(str)
    Error(_) -> decode.failure("", "String")
  }
}
