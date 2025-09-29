import gleam/bit_array
import gleam/dynamic
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/result

import protobin.{
  type BytePos, type DecodeResult, type ValueParser, Parsed, parse, parse_varint,
}
import internal/util

/// Decode a repeated field that may be either packed or expanded.
/// 
/// If it is impossible for a field to be packed (ie because it is encoded as
/// a `LEN`) and therefore there is no `ValueParser` for it, then use the
/// stdlib's `decode.list()` instead.
pub fn multiple(
  of decoder: Decoder(t),
  using parser: ValueParser,
) -> Decoder(List(t)) {
  use values <- decode.then(packed_values(of: decoder, using: parser))
  values |> decode.success
}

fn packed_values(
  of decoder: Decoder(t),
  using parser: ValueParser,
) -> Decoder(List(t)) {
  use bits <- decode.then(decode.list(of: decode.bit_array))
  let bits =
    list.fold(bits, <<>>, fn(acc, elem) { bit_array.concat([acc, elem]) })

  let values = {
    use values <- result.try(unpack_bits(bits, [], at: 0, using: parser))
    values
    |> list.map(dynamic.bit_array)
    |> list.try_map(fn(value) { decode.run(value, decoder) })
    |> result.map_error(protobin.UnableToDecode)
  }

  case values {
    Ok(values) -> decode.success(values)
    Error(_) -> decode.failure([], "Packed values")
  }
}

fn unpack_bits(
  bits: BitArray,
  acc: List(BitArray),
  at pos: BytePos,
  using parser: ValueParser,
) -> DecodeResult(List(BitArray)) {
  case bits {
    <<>> -> acc |> list.reverse |> Ok
    bits -> {
      use Parsed(value:, rest:, pos:) <- result.try(parser(bits, pos))
      unpack_bits(rest, [value, ..acc], at: pos, using: parser)
    }
  }
}

/// Expects a single value from a list. Panics if the list is empty.
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
    Ok(Parsed(value:, ..)) -> decode.success(value)
    Error(_) -> decode.failure(default, name)
  }
}

pub fn uint() -> Decoder(Int) {
  use bits <- decode.then(single_or_raw(decode.bit_array))
  use bits <- decode.then(case parse_varint(bits, 0) {
    Ok(Parsed(value:, rest: <<>>, pos: _)) -> decode.success(value)
    _ -> decode.failure(<<>>, "uint")
  })
  bits |> util.bit_array_to_uint |> decode.success
}

pub fn fixed(size: Int) -> Decoder(Int) {
  use bits <- decode.then(single_or_raw(decode.bit_array))
  case bits {
    <<num:unsigned-little-size(size)>> -> decode.success(num)
    _ -> decode.failure(0, "Fixed(" <> int.to_string(size) <> ")")
  }
}

pub fn string() -> Decoder(String) {
  use bits <- decode.then(single_or_raw(decode.bit_array))

  let str = bit_array.to_string(bits)
  case str {
    Ok(str) -> decode.success(str)
    Error(_) -> decode.failure("", "String")
  }
}
