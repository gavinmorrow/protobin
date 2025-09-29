import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option
import gleam/result

import internal/util
import internal/wire_type.{type WireType}

pub fn parse(
  from bits: BitArray,
  using decoder: Decoder(t),
) -> DecodeResult(Parsed(t)) {
  use Parsed(value: data, rest:, pos:) <- result.try(read_fields(bits, [], 0))
  decode.run(data, decoder)
  |> result.map(fn(value) { Parsed(value:, rest:, pos:) })
  |> result.map_error(UnableToDecode)
}

pub type ParseError {
  UnknownWireType(Int, pos: BytePos)
  InvalidVarInt(leftover_bits: BitArray, acc: BitArray, pos: BytePos)
  InvalidFixed(size: Int, bits: BitArray, pos: BytePos)
  InvalidLen(len: Int, value_bits: BitArray, pos: BytePos)
  UnableToDecode(List(decode.DecodeError))
}

fn read_fields(
  bits: BitArray,
  acc: List(Field),
  pos: BytePos,
) -> DecodeResult(Parsed(Dynamic)) {
  case bits {
    <<>> ->
      acc
      |> repeated_to_list
      |> dict.to_list
      |> dynamic.properties
      |> Parsed(value: _, rest: bits, pos:)
      |> Ok
    bits -> {
      use Parsed(value: prop, rest: bits, pos:) <- result.try(read_field(
        bits,
        pos,
      ))
      read_fields(bits, [prop, ..acc], pos)
    }
  }
}

fn repeated_to_list(reversed_fields: List(Field)) -> dict.Dict(Dynamic, Dynamic) {
  let fields = {
    // Every field is a list of values for two reasons:
    // a) expanded repeated values are encoded as repeated fields
    // b) if a non-repeating field is defined twice, then the last value should
    //    be considered the correct one. since the parser doesn't know which
    //    fields are repeating, it parses all fields as a list and then the
    //    decoders will handle choosing which value(s) to keep.
    let acc: dict.Dict(Dynamic, List(Dynamic)) = dict.new()
    use fields, Field(key:, value:) <- list.fold(
      over: reversed_fields,
      from: acc,
    )

    use existing_values <- dict.upsert(in: fields, update: key)
    let existing_values = option.unwrap(existing_values, or: [])
    // The repeated values were previously reversed, now being reversed again
    [value, ..existing_values]
  }

  dict.map_values(in: fields, with: fn(_key, field) { field |> dynamic.list })
}

pub type DecodeResult(t) =
  Result(t, ParseError)

pub type BytePos =
  Int

pub type Parsed(t) {
  Parsed(value: t, rest: BitArray, pos: BytePos)
}

fn parsed_map(of parsed: Parsed(t), with fun: fn(t) -> u) -> Parsed(u) {
  let Parsed(value:, rest:, pos:) = parsed
  Parsed(value: fun(value), rest:, pos:)
}

type Field {
  Field(key: Dynamic, value: Dynamic)
}

fn wire_type_read_fn(ty: WireType) -> ValueParser {
  case ty {
    wire_type.VarInt -> read_varint
    wire_type.I64 -> read_fixed(64)
    wire_type.Len -> read_len
    wire_type.I32 -> read_fixed(32)
  }
}

fn read_field(bits: BitArray, tag_pos: BytePos) -> DecodeResult(Parsed(Field)) {
  use Parsed(value: tag, rest: bits, pos:) <- result.try(parse_varint(
    bits,
    tag_pos,
  ))
  let tag = util.bit_array_to_uint(tag)

  let field_id = tag |> int.bitwise_shift_right(3)
  let wire_type = tag |> int.bitwise_and(0b111)

  case wire_type {
    6 -> {
      echo tag as "tag"
      Nil
    }
    _ -> Nil
  }
  use wire_type <- result.try(option.to_result(
    wire_type.parse(wire_type),
    UnknownWireType(wire_type, pos: tag_pos),
  ))

  let read = wire_type_read_fn(wire_type)
  use value: Parsed(Dynamic) <- result.try(
    read(bits, pos) |> result.map(parsed_map(_, dynamic.bit_array)),
  )

  let field =
    parsed_map(value, fn(value) { Field(key: dynamic.int(field_id), value:) })

  Ok(field)
}

pub type ValueResult =
  DecodeResult(Parsed(BitArray))

pub type ValueParser =
  fn(BitArray, BytePos) -> ValueResult

/// Reads the bits from a varint and returns *all* of them. They cannot be
/// parsed as a uint.
///
/// Specifically, the continuation bits are included, so the value's bit size
/// will be a multiple of 8.
pub fn read_varint(bits: BitArray, pos: BytePos) -> ValueResult {
  read_varint_acc(bits, <<>>, pos)
}

fn read_varint_acc(bits: BitArray, acc: BitArray, pos: BytePos) -> ValueResult {
  case bits {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let bit = <<0:size(1), n:bits>>
      let acc = bit_array.concat([acc, bit])
      Ok(Parsed(value: acc, rest:, pos: pos + 1))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> -> {
      let bit = <<1:size(1), n:bits>>
      read_varint_acc(rest, bit_array.concat([acc, bit]), pos + 1)
    }
    bits -> Error(InvalidVarInt(leftover_bits: bits, acc:, pos:))
  }
}

/// Reads the bits from a varint and parses them a BitArray. The returned bits
/// can be parsed as a uint.
///
/// For a decoder that does this, use `decoders.uint()`.
///
/// The continuation bits are not included, so the value's bit size will be a
/// multiple of 7.
pub fn parse_varint(bits: BitArray, pos: BytePos) -> ValueResult {
  parse_varint_acc(bits, <<>>, pos)
}

fn parse_varint_acc(bits: BitArray, acc: BitArray, pos: BytePos) -> ValueResult {
  case bits {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let acc = bit_array.concat([n, acc])
      Ok(Parsed(value: acc, rest:, pos: pos + 1))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> ->
      parse_varint_acc(rest, bit_array.concat([n, acc]), pos + 1)
    bits -> {
      Error(InvalidVarInt(leftover_bits: bits, acc:, pos:))
    }
  }
}

/// Size must be a multiple of 8.
pub fn read_fixed(size: Int) -> ValueParser {
  assert size % 8 == 0
  fn(bits: BitArray, pos: BytePos) -> ValueResult {
    case bits {
      <<num:bits-size(size), rest:bytes>> ->
        Ok(Parsed(value: num, rest:, pos: pos + size / 8))
      bits -> Error(InvalidFixed(size:, bits:, pos:))
    }
  }
}

fn read_len(bits: BitArray, len_pos: BytePos) -> ValueResult {
  // First, read the length of the value
  // It is encoded as a varint immediately after the tag
  use Parsed(value: len, rest: bits, pos:) <- result.try(parse_varint(
    bits,
    len_pos,
  ))
  let len = util.bit_array_to_uint(len)

  // Just decoded a uint, so should be safe
  assert len > 0

  use value <- result.try(
    bit_array.slice(from: bits, at: 0, take: len)
    |> result.map_error(fn(_) {
      InvalidLen(len:, value_bits: bits, pos: len_pos)
    }),
  )
  // Assert b/c if the len was too long, it would've errored in the prev slice
  let assert Ok(rest) =
    bit_array.slice(from: bits, at: len, take: bit_array.byte_size(bits) - len)

  Ok(Parsed(value:, rest:, pos: pos + len))
}
