import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

pub fn decode(
  from bytes: BitArray,
  using decoder: Decoder(t),
) -> Result(t, DecodeError) {
  use data <- result.try(read_fields(bytes, []))
  decode.run(data, decoder) |> result.map_error(UnableToDecode)
}

type WireType {
  VarInt
  I64
  Len
  I32
}

fn parse_wire_type(i: Int) -> Option(WireType) {
  case i {
    0 -> Some(VarInt)
    1 -> Some(I64)
    2 -> Some(Len)
    5 -> Some(I32)
    _ -> None
  }
}

pub type DecodeError {
  UnknownWireType(Int)
  InvalidVarInt(leftover_bits: BitArray, acc: BitArray)
  UnableToDecode(List(decode.DecodeError))
}

fn read_fields(
  bytes: BitArray,
  acc: List(#(Dynamic, Dynamic)),
) -> Result(Dynamic, DecodeError) {
  case bytes {
    <<>> -> Ok(dynamic.properties(acc))
    bytes -> {
      use #(prop, bytes) <- result.try(read_field(bytes))
      read_fields(bytes, [prop, ..acc])
    }
  }
}

fn read_field(
  bytes: BitArray,
) -> Result(#(#(Dynamic, Dynamic), BitArray), DecodeError) {
  use #(tag, bytes) <- result.try(read_varint(bytes, <<>>))
  let tag = bit_array_to_uint(tag)

  let field_id = tag |> int.bitwise_shift_right(3)
  let wire_type = tag |> int.bitwise_and(0b111)

  use wire_type <- result.try(option.to_result(
    parse_wire_type(wire_type),
    UnknownWireType(wire_type),
  ))
  use #(value, bytes): #(Dynamic, BitArray) <- result.try(case wire_type {
    VarInt -> {
      use value <- result.map(read_varint(bytes, <<>>))
      use value <- pair.map_first(value)
      dynamic.bit_array(value)
    }
    I64 -> todo
    Len -> todo
    I32 -> todo
  })

  Ok(#(#(dynamic.int(field_id), value), bytes))
}

fn read_varint(
  bytes: BitArray,
  acc: BitArray,
) -> Result(#(BitArray, BitArray), DecodeError) {
  case bytes {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let acc = bit_array.concat([n, acc])
      Ok(#(acc, rest))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> ->
      read_varint(rest, bit_array.concat([n, acc]))
    bits -> Error(InvalidVarInt(leftover_bits: bits, acc:))
  }
}

pub fn decode_uint() -> Decoder(Int) {
  use bits <- decode.then(decode.bit_array)
  bit_array_to_uint(bits) |> decode.success
}

fn bit_array_to_uint(bits: BitArray) -> Int {
  let size = bit_array.bit_size(bits)
  // Interpert the entire bit array as an unsigned int
  let assert <<n:unsigned-big-size(size)>> = bits
  n
}
