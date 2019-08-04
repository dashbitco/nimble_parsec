defmodule IpAddressParser do
  import NimbleParsec

  ipv4_octet =
    ascii_string([?0..?9], min: 1, max: 3)

  ipv4_address =
    times(ipv4_octet |> string("."), 3)
    |> concat(ipv4_octet)

  ipv6_hexadectet =
    ascii_string('0123456789abcdefABCDEF', min: 1, max: 4)

  ipv6_ls32 =
    choice([
      ipv6_hexadectet |> string(":") |> concat(ipv6_hexadectet),
      ipv4_address
    ])

  ipv6_fragment =
    ipv6_hexadectet |> string(":")

  ipv6_address =
    choice([
      times(ipv6_fragment, 6) |> concat(ipv6_ls32),
      string("::") |> times(ipv6_fragment, 5) |> concat(ipv6_ls32),
      ipv6_hexadectet |> string("::") |> times(ipv6_fragment, 4) |> concat(ipv6_ls32),
      # times(ipv6_fragment, max: 1) |> concat(ipv6_hexadectet) |> string("::") |> times(ipv6_fragment, 3) |> concat(ipv6_ls32),
      # times(ipv6_fragment, max: 2) |> concat(ipv6_hexadectet) |> string("::") |> times(ipv6_fragment, 2) |> concat(ipv6_ls32),
      # times(ipv6_fragment, max: 3) |> concat(ipv6_hexadectet) |> string("::") |> concat(ipv6_fragment) |> concat(ipv6_ls32),
      times(ipv6_fragment, max: 4) |> concat(ipv6_hexadectet) |> string("::") |> concat(ipv6_ls32),
      times(ipv6_fragment, max: 5) |> concat(ipv6_hexadectet) |> string("::") |> concat(ipv6_hexadectet),
      times(ipv6_fragment, max: 6) |> concat(ipv6_hexadectet) |> string("::")
    ])

  cidr_prefix =
    string("/")
    |> ascii_string([?0..?9], min: 1, max: 3)

  ip_address =
    choice([
      ipv4_address,
      ipv6_address
    ])
    |> optional(cidr_prefix)
    |> reduce({Enum, :join, []})
    |> label("a valid IPv4 or IPv6 address and optional CIDR prefix")
    |> unwrap_and_tag(:ip)

  defparsec :ip_address, ip_address
end

IO.inspect IpAddressParser.ip_address("2000:4000:6000:8000::a")