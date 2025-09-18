import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

pub fn decode(
  from bits: BitArray,
  using decoder: Decoder(t),
) -> Result(t, DecodeError) {
  use data <- result.try(read_fields(bits, []))
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
  InvalidI64(bits: BitArray)
  UnableToDecode(List(decode.DecodeError))
}

fn read_fields(
  bits: BitArray,
  acc: List(#(Dynamic, Dynamic)),
) -> Result(Dynamic, DecodeError) {
  case bits {
    <<>> -> Ok(dynamic.properties(acc))
    bits -> {
      use #(prop, bits) <- result.try(read_field(bits))
      read_fields(bits, [prop, ..acc])
    }
  }
}

fn read_field(
  bits: BitArray,
) -> Result(#(#(Dynamic, Dynamic), BitArray), DecodeError) {
  use #(tag, bits) <- result.try(read_varint(bits))
  let tag = bit_array_to_uint(tag)

  let field_id = tag |> int.bitwise_shift_right(3)
  let wire_type = tag |> int.bitwise_and(0b111)

  use wire_type <- result.try(option.to_result(
    parse_wire_type(wire_type),
    UnknownWireType(wire_type),
  ))

  let read_fn: fn(BitArray) -> ValueResult = case wire_type {
    VarInt -> read_varint
    I64 -> read_i64
    Len -> todo
    I32 -> todo
  }
  use #(value, bits): #(Dynamic, BitArray) <- result.try({
    use value <- result.map(read_fn(bits))
    use value <- pair.map_first(value)
    dynamic.bit_array(value)
  })

  Ok(#(#(dynamic.int(field_id), value), bits))
}

type ValueResult =
  Result(#(BitArray, BitArray), DecodeError)

fn read_varint(bits: BitArray) -> ValueResult {
  read_varint_acc(bits, <<>>)
}

fn read_varint_acc(bits: BitArray, acc: BitArray) -> ValueResult {
  case bits {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let acc = bit_array.concat([n, acc])
      Ok(#(acc, rest))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> ->
      read_varint_acc(rest, bit_array.concat([n, acc]))
    bits -> Error(InvalidVarInt(leftover_bits: bits, acc:))
  }
}

fn read_i64(bits: BitArray) -> ValueResult {
  case bits {
    <<i64:bits-size(64), rest:bytes>> -> Ok(#(i64, rest))
    bits -> Error(InvalidI64(bits:))
  }
}

pub fn decode_uint() -> Decoder(Int) {
  use bits <- decode.then(decode.bit_array)
  bit_array_to_uint(bits) |> decode.success
}

pub fn decode_u64() -> Decoder(Int) {
  use bits <- decode.then(decode.bit_array)
  let assert <<u64:unsigned-little-size(64)>> = bits
  u64 |> decode.success
}

fn bit_array_to_uint(bits: BitArray) -> Int {
  let size = bit_array.bit_size(bits)
  // Interpert the entire bit array as an unsigned int
  let assert <<n:unsigned-big-size(size)>> = bits
  n
}
