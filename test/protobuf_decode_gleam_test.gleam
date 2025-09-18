import gleam/dynamic/decode.{type Decoder}
import gleam/option
import gleeunit
import protobuf_decode_gleam.{decode, decode_u64, decode_uint}
import simplifile as file

pub fn main() -> Nil {
  gleeunit.main()
}

type Person {
  Person(id: Int, age: Int, score: Int, self: option.Option(Person))
}

fn person_decoder() -> Decoder(Person) {
  let person_inner_decoder = {
    use bits <- decode.then(decode.bit_array)

    let person = decode(from: bits, using: person_decoder())
    case person {
      Ok(person) -> decode.success(person)
      Error(_) ->
        decode.failure(
          Person(id: 0, age: 0, score: 0, self: option.None),
          "Person",
        )
    }
  }

  use id <- decode.field(3, decode_u64())
  use age <- decode.field(1, decode_uint())
  use score <- decode.field(2, decode_uint())
  use person <- decode.optional_field(
    4,
    option.None,
    decode.optional(person_inner_decoder),
  )

  decode.success(Person(id:, age:, score:, self: person))
}

pub fn person_pb_test() {
  let path = "./test/person.pb"
  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(person) = decode(from: bits, using: person_decoder())
  assert person
    == Person(
      id: 42,
      age: 150,
      score: 81_050,
      self: option.Some(Person(
        id: 42,
        age: 150,
        score: 81_050,
        self: option.None,
      )),
    )
}

type TwoInts {
  Test(id: Int, age: Int)
}

fn two_ints_decoder() -> Decoder(TwoInts) {
  use id <- decode.field(1, decode_uint())
  use age <- decode.field(2, decode_uint())

  decode.success(Test(id:, age:))
}

pub fn two_ints_test() {
  let bits = <<
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

  let assert Ok(data) = decode(from: bits, using: two_ints_decoder())
  assert data == Test(id: 150, age: 80_150)
}
