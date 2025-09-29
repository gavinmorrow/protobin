import gleam/option.{type Option, None, Some}

pub type WireType {
  VarInt
  I64
  Len
  I32
}

pub fn parse(i: Int) -> Option(WireType) {
  case i {
    0 -> Some(VarInt)
    1 -> Some(I64)
    2 -> Some(Len)
    5 -> Some(I32)
    _ -> None
  }
}
