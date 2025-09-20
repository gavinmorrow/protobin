import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option
import gleam/result

import internal/wire_type.{type WireType}

pub fn parse(
  from bits: BitArray,
  using decoder: Decoder(t),
) -> Result(t, DecodeError) {
  use data <- result.try(read_fields(bits, []))
  decode.run(data, decoder) |> result.map_error(UnableToDecode)
}

pub fn decode_protobuf(
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

pub type DecodeError {
  UnknownWireType(Int)
  InvalidVarInt(leftover_bits: BitArray, acc: BitArray)
  InvalidFixed(size: Int, bits: BitArray)
  InvalidLen(len: Int, value_bits: BitArray)
  UnableToDecode(List(decode.DecodeError))
}

fn read_fields(bits: BitArray, acc: List(Field)) -> Result(Dynamic, DecodeError) {
  case bits {
    <<>> ->
      acc
      |> list.map(field_as_pair)
      |> dynamic.properties
      |> Ok
    bits -> {
      use Parsed(value: prop, rest: bits) <- result.try(read_field(bits))
      read_fields(bits, [prop, ..acc])
    }
  }
}

type DecodeResult(t) =
  Result(t, DecodeError)

type Parsed(t) {
  Parsed(value: t, rest: BitArray)
}

fn parsed_map(of parsed: Parsed(t), with fun: fn(t) -> u) -> Parsed(u) {
  let Parsed(value:, rest:) = parsed
  Parsed(value: fun(value), rest:)
}

type Field {
  Field(key: Dynamic, value: Dynamic)
}

fn field_as_pair(field: Field) {
  #(field.key, field.value)
}

fn wire_type_read_fn(ty: WireType) -> fn(BitArray) -> ValueResult {
  case ty {
    wire_type.VarInt -> read_varint
    wire_type.I64 -> read_fixed(64)
    wire_type.Len -> read_len
    wire_type.I32 -> read_fixed(32)
  }
}

fn read_field(bits: BitArray) -> DecodeResult(Parsed(Field)) {
  use Parsed(value: tag, rest: bits) <- result.try(read_varint(bits))
  let tag = bit_array_to_uint(tag)

  let field_id = tag |> int.bitwise_shift_right(3)
  let wire_type = tag |> int.bitwise_and(0b111)

  use wire_type <- result.try(option.to_result(
    wire_type.parse(wire_type),
    UnknownWireType(wire_type),
  ))

  let read_fn = wire_type_read_fn(wire_type)
  use value: Parsed(Dynamic) <- result.try({
    use value <- result.map(read_fn(bits))
    parsed_map(value, dynamic.bit_array)
  })

  let field =
    parsed_map(value, fn(value) { Field(key: dynamic.int(field_id), value:) })

  Ok(field)
}

type ValueResult =
  DecodeResult(Parsed(BitArray))

fn read_varint(bits: BitArray) -> ValueResult {
  read_varint_acc(bits, <<>>)
}

fn read_varint_acc(bits: BitArray, acc: BitArray) -> ValueResult {
  case bits {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let acc = bit_array.concat([n, acc])
      Ok(Parsed(value: acc, rest:))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> ->
      read_varint_acc(rest, bit_array.concat([n, acc]))
    bits -> Error(InvalidVarInt(leftover_bits: bits, acc:))
  }
}

fn read_fixed(size: Int) -> fn(BitArray) -> ValueResult {
  fn(bits: BitArray) -> ValueResult {
    case bits {
      <<num:bits-size(size), rest:bytes>> -> Ok(Parsed(value: num, rest:))
      bits -> Error(InvalidFixed(size:, bits:))
    }
  }
}

fn read_len(bits: BitArray) -> ValueResult {
  // First, read the length of the value
  // It is encoded as a varint immediately after the tag
  use Parsed(value: len, rest: bits) <- result.try(read_varint(bits))
  let len = bit_array_to_uint(len)

  // Just decoded a uint, so should be safe
  assert len > 0

  use value <- result.try(
    bit_array.slice(from: bits, at: 0, take: len)
    |> result.map_error(fn(_) { InvalidLen(len:, value_bits: bits) }),
  )
  // Assert b/c if the len was too long, it would've errored in the prev slice
  let assert Ok(rest) =
    bit_array.slice(from: bits, at: len, take: bit_array.byte_size(bits) - len)

  Ok(Parsed(value:, rest:))
}

pub fn decode_uint() -> Decoder(Int) {
  use bits <- decode.then(decode.bit_array)
  bit_array_to_uint(bits) |> decode.success
}

pub fn decode_fixed(size: Int) -> Decoder(Int) {
  use bits <- decode.then(decode.bit_array)
  let assert <<num:unsigned-little-size(size)>> = bits
  num |> decode.success
}

pub fn decode_string() -> Decoder(String) {
  use bits <- decode.then(decode.bit_array)

  let str = bit_array.to_string(bits)
  case str {
    Ok(str) -> decode.success(str)
    Error(_) -> decode.failure("", "String")
  }
}

fn bit_array_to_uint(bits: BitArray) -> Int {
  let size = bit_array.bit_size(bits)
  // Interpert the entire bit array as an unsigned int
  let assert <<n:unsigned-big-size(size)>> = bits
  n
}
