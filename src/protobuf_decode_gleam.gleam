import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

pub fn main() -> Nil {
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

  let assert Ok(data) = decode(from: bytes, using: test_decoder())
  echo data

  Nil
}

pub type Test {
  Test(id: Int, age: Int)
}

pub fn test_decoder() -> Decoder(Test) {
  use id <- decode.field(1, decode.int)
  use age <- decode.field(2, decode.int)
  decode.success(Test(id:, age:))
}

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
  use #(field_id_and_wire_type, bytes) <- result.try(read_varint(bytes, <<>>))
  let field_id = field_id_and_wire_type |> int.bitwise_shift_right(3)
  let wire_type = field_id_and_wire_type |> int.bitwise_and(0b111)

  use wire_type <- result.try(option.to_result(
    parse_wire_type(wire_type),
    UnknownWireType(wire_type),
  ))
  use #(value, bytes): #(Dynamic, BitArray) <- result.try(case wire_type {
    VarInt -> {
      use value <- result.map(read_varint(bytes, <<>>))
      use value <- pair.map_first(value)
      dynamic.int(value)
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
) -> Result(#(Int, BitArray), DecodeError) {
  case bytes {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let acc = bit_array.concat([n, acc])
      let n = bit_array_to_int(acc)
      Ok(#(n, rest))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> ->
      read_varint(rest, bit_array.concat([n, acc]))
    bits -> Error(InvalidVarInt(leftover_bits: bits, acc:))
  }
}

fn bit_array_to_int(bits: BitArray) -> Int {
  let size = bit_array.bit_size(bits)
  // Interpert the entire bit array as an unsigned int
  let assert <<n:unsigned-big-size(size)>> = bits
  n
}
